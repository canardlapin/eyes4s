/*
 * Copyright 2026 canardlapin
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package eyes4s.codec

import cats.syntax.all.*
import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.plan.*
import io.circe.Json

/** Versioned codecs for normalized recordings and the recording input payload.
  *
  * Samples are encoded as parallel columns of equal declared length: exact
  * microsecond timestamps as decimal strings, a support category per sample,
  * finite positions where the category has one, the pupil value where one was
  * measured, and the ordered derivation lineage of every sample. Decoding
  * rebuilds the recording through `Recording.of`, so every core invariant
  * (monotonic time, in-frame positions, declared pupil unit, fixed-rate
  * tolerance) is re-proven, and then compares `Recording.contentHash` with the
  * declared digest. A recording plan therefore accepts a decoded recording by
  * the same artifact reference as the original.
  *
  * Payloads are in-memory JSON. Encoding and decoding refuse more than
  * [[RecordingInputCodecs.maximumSamples]] samples per recording with a named
  * `CodecError.Unsupported`; larger recordings must be split per block or
  * carried as separately referenced typed payloads.
  */
object RecordingInputCodecs:
  val recordingSchema: DefinitionId = DefinitionId.recording
  val binocularSchema: DefinitionId = DefinitionId.binocularRecording
  val inputSchema: DefinitionId     = DefinitionId.recordingInput

  /** Inline sample bound per recording: 2^22 samples, roughly seventy minutes
    * at 1 kHz, and about 300 MB of pretty-printed JSON at the upper end.
    */
  val maximumSamples: Int = 1 << 22

  /** A standalone monocular recording with its own identity table. */
  def recording[U <: Unit2D: UnitLabel]: VersionedCodec[Recording[U]] =
    VersionedCodec.checked[Recording[U]](recordingSchema)(value =>
      for
        table <- DocumentIdentities.empty.addFrame(value.frame).map(_.addClock(value.clock))
        body  <- RecordingInputWire.writeRecording(value)
      yield Json.obj("identities" -> table.json, "recording" -> body)
    )(json =>
      for
        table <- Wire.field[Json](json, "identities").flatMap(DocumentIdentities.read)
        body  <- Wire.field[Json](json, "recording")
        value <- RecordingInputWire.readRecording[U](body, table).left.map(Wire.at("recording"))
      yield value
    )

  /** A standalone paired recording with its own identity table. */
  def binocular[U <: Unit2D: UnitLabel]: VersionedCodec[BinocularRecording[U]] =
    VersionedCodec.checked[BinocularRecording[U]](binocularSchema)(value =>
      for
        table <- DocumentIdentities.empty.addFrame(value.frame).map(_.addClock(value.clock))
        body  <- RecordingInputWire.writeBinocular(value)
      yield Json.obj("identities" -> table.json, "recording" -> body)
    )(json =>
      for
        table <- Wire.field[Json](json, "identities").flatMap(DocumentIdentities.read)
        body  <- Wire.field[Json](json, "recording")
        value <- RecordingInputWire.readBinocular[U](body, table).left.map(Wire.at("recording"))
      yield value
    )

  /** The recording input payload: channels, viewing geometry and observed
    * synchronization marks, with the input digest checked on decode.
    */
  def input[U <: Unit2D](using unit: UnitLabel[U]): VersionedCodec[RecordingInput[U]] =
    VersionedCodec.checked[RecordingInput[U]](inputSchema)(writeInput[U])(readInput[U])

  private def writeInput[U <: Unit2D: UnitLabel](
      value: RecordingInput[U]
  ): Either[CodecError, Json] =
    for
      base <- DocumentIdentities.empty.addFrame(value.frame).map(_.addClock(value.clock))
      table = value.synchronization.fold(base)(sync => base.addClock(sync.target))
      channels <- value.channels match
        case RecordingChannels.Monocular(r) =>
          RecordingInputWire
            .writeRecording(r)
            .map(body => Json.obj("kind" -> Json.fromString("monocular"), "recording" -> body))
        case RecordingChannels.Binocular(b) =>
          RecordingInputWire
            .writeBinocular(b)
            .map(body => Json.obj("kind" -> Json.fromString("binocular"), "recording" -> body))
      synchronization <- value.synchronization.traverse(sync =>
        sync
          .fit(value.clock)
          .left
          .map(CodecError.Synchronization("synchronization", _))
          .map(evidence =>
            Json.obj(
              "target"              -> Json.fromString(sync.target.name),
              "mode"                -> Json.fromString(sync.mode.render),
              "marks"               -> Json.arr(sync.marks.map(DomainWire.mark)*),
              "residualLimitMicros" -> sync.residualLimit
                .fold(Json.Null)(limit => DomainWire.time(limit.span.toMicros)),
              "fitted" -> Json.obj(
                "offsetMicros" -> DomainWire.time(evidence.offset.toMicros),
                "drift"        -> Json.fromDoubleOrNull(evidence.sync.drift)
              )
            )
          )
      )
    yield Json.obj(
      "unit"       -> Json.fromString(summon[UnitLabel[U]].symbol),
      "source"     -> Json.fromString(value.source.value),
      "input"      -> Json.fromString(value.reference.digest),
      "identities" -> table.json,
      "channels"   -> channels,
      "viewing"    -> value.viewing.fold(Json.Null)(v => DomainWire.perspective(v.perspective)),
      "synchronization" -> synchronization.getOrElse(Json.Null)
    )

  private def readInput[U <: Unit2D](
      json: Json
  )(using unit: UnitLabel[U]): Either[CodecError, RecordingInput[U]] =
    for
      symbol <- Wire.field[String](json, "unit")
      _      <- Either.cond(
        symbol == unit.symbol,
        (),
        CodecError.Field("unit", json, s"expected ${unit.symbol}, got $symbol")
      )
      source   <- Wire.field[String](json, "source")
      declared <- Wire.field[String](json, "input")
      _ <- ArtifactRef.parse[RecordingInput[U]](declared).left.map(CodecError.Definition.apply)
      table       <- Wire.field[Json](json, "identities").flatMap(DocumentIdentities.read)
      channels    <- Wire.field[Json](json, "channels").flatMap(readChannels[U](_, table))
      viewingJson <- Wire.field[Option[Json]](json, "viewing")
      viewing     <- viewingJson.traverse(value =>
        DomainWire.readPerspective(value).map(Viewing.apply).left.map(Wire.at("viewing"))
      )
      syncJson <- Wire.field[Option[Json]](json, "synchronization")
      sync     <- syncJson.traverse(readSynchronization(_, table))
      fitted   <- syncJson.traverse(Wire.field[Json](_, "fitted"))
      value    <- RecordingInput
        .of(RecordingRef(source), channels, viewing, sync.map(_._1))
        .left
        .map(CodecError.Input.apply)
      _ <- sync.traverse { case (observed, offset, drift) =>
        observed
          .fit(channels.clock)
          .left
          .map(CodecError.Synchronization("synchronization", _))
          .flatMap(evidence =>
            Either.cond(
              evidence.offset.toMicros == offset && evidence.sync.drift == drift,
              (),
              CodecError.Field(
                "synchronization.fitted",
                fitted.getOrElse(Json.Null),
                s"declared offsetMicros=$offset drift=$drift but the observed marks refit to " +
                  s"offsetMicros=${evidence.offset.toMicros} drift=${evidence.sync.drift}"
              )
            )
          )
      }
      _ <- Either.cond(
        value.reference.digest == declared,
        (),
        CodecError.InputIdentity(declared, value.reference.digest)
      )
    yield value

  private def readChannels[U <: Unit2D: UnitLabel](
      json: Json,
      table: DocumentIdentities
  ): Either[CodecError, RecordingChannels[U]] =
    Wire.field[String](json, "kind").flatMap {
      case "monocular" =>
        Wire
          .field[Json](json, "recording")
          .flatMap(RecordingInputWire.readRecording[U](_, table))
          .map(RecordingChannels.Monocular.apply)
          .left
          .map(Wire.at("channels.recording"))
      case "binocular" =>
        Wire
          .field[Json](json, "recording")
          .flatMap(RecordingInputWire.readBinocular[U](_, table))
          .map(RecordingChannels.Binocular.apply)
          .left
          .map(Wire.at("channels.recording"))
      case other => Left(CodecError.Field("channels.kind", json, s"unknown channels $other"))
    }

  private def readSynchronization(
      json: Json,
      table: DocumentIdentities
  ): Either[CodecError, (ObservedSynchronization, Long, Double)] =
    (for
      targetName <- Wire.field[String](json, "target")
      target     <- table.clock(ClockId(targetName))
      modeName   <- Wire.field[String](json, "mode")
      mode       <- SyncFitMode.values
        .find(_.render == modeName)
        .toRight(CodecError.Field("mode", json, s"unknown synchronization mode '$modeName'"))
      rawMarks <- Wire.field[Vector[Json]](json, "marks")
      marks    <- rawMarks.zipWithIndex.traverse { case (raw, index) =>
        DomainWire.readMark(raw).left.map(Wire.at(s"marks[$index]"))
      }
      rawLimit <- Wire.field[Option[Json]](json, "residualLimitMicros")
      limit    <- rawLimit.traverse(raw =>
        DomainWire
          .readTime(raw, "residualLimitMicros")
          .flatMap(micros =>
            SyncResidualLimit
              .of(Span.micros(micros))
              .left
              .map(CodecError.Synchronization("residualLimitMicros", _))
          )
      )
      fitted <- Wire.field[Json](json, "fitted")
      offset <- DomainWire.micros(fitted, "offsetMicros")
      drift  <- DomainWire.finite(fitted, "drift")
    yield (ObservedSynchronization(target, mode, marks, limit), offset, drift)).left
      .map(Wire.at("synchronization"))

/** Inner wire forms shared by the standalone codecs and containing payloads
  * (a source-supported scanpath embeds its recording with the same shape).
  * Frames and clocks are referenced by nominal ID against a document table.
  */
private[codec] object RecordingInputWire:
  private val eyes: Vector[(Eye, String)] =
    Vector(Eye.Left -> "left", Eye.Right -> "right", Eye.Cyclopean -> "cyclopean")

  private val pupilUnits: Vector[(PupilUnit, String)] = Vector(
    PupilUnit.Area      -> "area",
    PupilUnit.Diameter  -> "diameter",
    PupilUnit.Arbitrary -> "arbitrary"
  )

  private val origins: Vector[(SampleOrigin, String)] = Vector(
    SampleOrigin.Measured     -> "measured",
    SampleOrigin.Interpolated -> "interpolated",
    SampleOrigin.Smoothed     -> "smoothed",
    SampleOrigin.Projected    -> "projected"
  )

  private def name[A](table: Vector[(A, String)], value: A): Json =
    Json.fromString(table.collectFirst { case (v, n) if v == value => n }.get)

  private def lookup[A](
      table: Vector[(A, String)],
      json: Json,
      field: String,
      kind: String
  ): Either[CodecError, A] =
    Wire
      .field[String](json, field)
      .flatMap(text =>
        table
          .collectFirst { case (v, n) if n == text => v }
          .toRight(CodecError.Field(field, json, s"unknown $kind '$text'"))
      )

  private def rate(value: Rate): Json = value match
    case Rate.Fixed(hz) =>
      Json.obj("kind" -> Json.fromString("fixed"), "hz" -> Json.fromDoubleOrNull(hz.value))
    case Rate.Irregular => Json.obj("kind" -> Json.fromString("irregular"))

  private def readRate(json: Json): Either[CodecError, Rate] =
    for
      raw  <- Wire.field[Json](json, "rate")
      kind <- Wire.field[String](raw, "kind")
      rate <- kind match
        case "fixed" =>
          DomainWire
            .finite(raw, "hz")
            .flatMap(hz =>
              Hz(hz).left
                .map(e => CodecError.Field("rate.hz", raw, e.message))
                .map(Rate.Fixed(_))
            )
        case "irregular" => Right(Rate.Irregular)
        case other => Left(CodecError.Field("rate.kind", raw, s"unknown sampling rate $other"))
    yield rate

  private def readTolerance(json: Json): Either[CodecError, SamplingTolerance] =
    DomainWire
      .micros(json, "samplingToleranceMicros")
      .flatMap(micros =>
        SamplingTolerance
          .of(Span.micros(micros))
          .left
          .map(CodecError.Recording("samplingToleranceMicros", _))
      )

  private def checkLength(json: Json, length: Int): Either[CodecError, Unit] =
    if length < 1 then Left(CodecError.Field("samples.length", json, "expected at least one"))
    else if length > RecordingInputCodecs.maximumSamples then
      Left(
        CodecError.Unsupported(
          "samples",
          s"$length samples exceed the inline payload bound ${RecordingInputCodecs.maximumSamples}"
        )
      )
    else Right(())

  private def column(
      json: Json,
      field: String,
      length: Int
  ): Either[CodecError, Vector[Json]] =
    Wire
      .field[Vector[Json]](json, field)
      .flatMap(values =>
        Either.cond(
          values.length == length,
          values,
          CodecError.Field(
            s"samples.$field",
            json,
            s"column has ${values.length} entries, expected length $length"
          )
        )
      )

  private def gazeColumns[U <: Unit2D](gazes: IArray[Gaze[U]]): Vector[(String, Json)] =
    val n     = gazes.length
    val state = Vector.tabulate(n) { i =>
      gazes(i) match
        case Gaze.Tracked(_, _) => Json.fromString("tracked")
        case Gaze.Blink()       => Json.fromString("blink")
        case Gaze.Lost()        => Json.fromString("lost")
        case Gaze.OffScreen(_)  => Json.fromString("offScreen")
    }
    val x     = Vector.tabulate(n)(i => gazes(i).position.fold(Json.Null)(p => number(p.x)))
    val y     = Vector.tabulate(n)(i => gazes(i).position.fold(Json.Null)(p => number(p.y)))
    val pupil = Vector.tabulate(n)(i => gazes(i).pupil.fold(Json.Null)(number))
    Vector(
      "state" -> Json.arr(state*),
      "x"     -> Json.arr(x*),
      "y"     -> Json.arr(y*),
      "pupil" -> Json.arr(pupil*)
    )

  /** Positions and pupils are validated finite by the core constructors. */
  private def number(value: Double): Json = Json.fromDoubleOrNull(value)

  private def readGazeColumns[U <: Unit2D](
      json: Json,
      length: Int,
      path: String
  ): Either[CodecError, IArray[Gaze[U]]] =
    for
      state <- column(json, "state", length)
      xs    <- column(json, "x", length)
      ys    <- column(json, "y", length)
      pupil <- column(json, "pupil", length)
      gazes <- (0 until length).toVector.traverse { i =>
        readGaze[U](state(i), xs(i), ys(i), pupil(i)).left.map(Wire.at(s"$path[$i]"))
      }
    yield IArray.from(gazes)

  private def readGaze[U <: Unit2D](
      state: Json,
      x: Json,
      y: Json,
      pupil: Json
  ): Either[CodecError, Gaze[U]] =
    def finite(value: Json, field: String): Either[CodecError, Double] =
      value.asNumber
        .map(_.toDouble)
        .filter(_.isFinite)
        .toRight(CodecError.Field(field, value, "expected a finite number"))
    def absent(value: Json, field: String): Either[CodecError, Unit] =
      Either.cond(value.isNull, (), CodecError.Field(field, value, "expected null"))
    state.asString match
      case Some("tracked") =>
        for
          px <- finite(x, "x")
          py <- finite(y, "y")
          ps <- if pupil.isNull then Right(None) else finite(pupil, "pupil").map(Some(_))
        yield Gaze.Tracked(Pt[U](px, py), ps)
      case Some("offScreen") =>
        for
          px <- finite(x, "x")
          py <- finite(y, "y")
          _  <- absent(pupil, "pupil")
        yield Gaze.OffScreen(Pt[U](px, py))
      case Some("blink") =>
        for
          _ <- absent(x, "x")
          _ <- absent(y, "y")
          _ <- absent(pupil, "pupil")
        yield Gaze.Blink[U]()
      case Some("lost") =>
        for
          _ <- absent(x, "x")
          _ <- absent(y, "y")
          _ <- absent(pupil, "pupil")
        yield Gaze.Lost[U]()
      case _ => Left(CodecError.Field("state", state, "unknown sample support category"))

  private def lineage(value: SampleLineage): Json =
    Json.fromString(value.toVector.map(step => name(origins, step).asString.get).mkString(">"))

  private def readLineage(json: Json): Either[CodecError, SampleLineage] =
    json.asString
      .toRight(CodecError.Field("lineage", json, "expected a lineage string"))
      .flatMap { text =>
        val steps = text
          .split(">", -1)
          .toVector
          .traverse(step =>
            origins
              .collectFirst { case (o, n) if n == step => o }
              .toRight(CodecError.Field("lineage", json, s"unknown derivation step '$step'"))
          )
        steps.flatMap { parsed =>
          val basis = parsed.headOption match
            case Some(SampleOrigin.Measured)     => Right(SampleLineage.measured)
            case Some(SampleOrigin.Interpolated) => Right(SampleLineage.interpolated)
            case _                               =>
              Left(
                CodecError.Field(
                  "lineage",
                  json,
                  "a lineage begins with measured or interpolated"
                )
              )
          parsed.drop(1).foldLeft(basis) { (acc, step) =>
            acc.flatMap(current =>
              step match
                case SampleOrigin.Smoothed  => Right(current.smoothed)
                case SampleOrigin.Projected => Right(current.projected)
                case other                  =>
                  Left(
                    CodecError.Field(
                      "lineage",
                      json,
                      s"derivation step '${name(origins, other).asString.get}' cannot follow a basis"
                    )
                  )
            )
          }
        }
      }

  def writeRecording[U <: Unit2D](value: Recording[U]): Either[CodecError, Json] =
    checkLength(Json.Null, value.size).map { _ =>
      val n = value.size
      Json.obj(
        "frame"                   -> Json.fromString(value.frame.id.name),
        "clock"                   -> Json.fromString(value.clock.name),
        "eye"                     -> name(eyes, value.eye),
        "pupilUnit"               -> value.pupilUnit.fold(Json.Null)(name(pupilUnits, _)),
        "rate"                    -> rate(value.rate),
        "samplingToleranceMicros" -> DomainWire.time(value.samplingTolerance.toSpan.toMicros),
        "recording"               -> Json.fromString(value.contentHash.render),
        "samples"                 -> Json.fromFields(
          Vector(
            "length"  -> Json.fromInt(n),
            "tMicros" -> Json.arr(
              Vector.tabulate(n)(i => DomainWire.time(value.samples(i).t.toMicros))*
            )
          ) ++ gazeColumns(value.samples.map(_.gaze)) :+
            ("lineage" -> Json.arr(Vector.tabulate(n)(i => lineage(value.samples(i).lineage))*))
        )
      )
    }

  def readRecording[U <: Unit2D: UnitLabel](
      json: Json,
      table: DocumentIdentities
  ): Either[CodecError, Recording[U]] =
    for
      frameName <- Wire.field[String](json, "frame")
      frame     <- table.frame[U](FrameId(frameName))
      clockName <- Wire.field[String](json, "clock")
      clock     <- table.clock(ClockId(clockName))
      eye       <- lookup(eyes, json, "eye", "eye")
      pupilJson <- Wire.field[Option[Json]](json, "pupilUnit")
      pupilUnit <- pupilJson.traverse(_ => lookup(pupilUnits, json, "pupilUnit", "pupil unit"))
      rate      <- readRate(json)
      tolerance <- readTolerance(json)
      declared  <- Wire.field[String](json, "recording")
      samples   <- Wire.field[Json](json, "samples")
      length    <- Wire.field[Int](samples, "length")
      _         <- checkLength(samples, length)
      times     <- readTimes(samples, length)
      gazes     <- readGazeColumns[U](samples, length, "samples")
      lineages  <- column(samples, "lineage", length).flatMap(_.zipWithIndex.traverse {
        case (raw, i) => readLineage(raw).left.map(Wire.at(s"samples[$i]"))
      })
      value <- Recording
        .of(
          frame,
          clock,
          rate,
          eye,
          pupilUnit,
          IArray.tabulate(length)(i => Sample(times(i), gazes(i), lineages(i))),
          tolerance
        )
        .left
        .map(CodecError.Recording("samples", _))
      _ <- Either.cond(
        value.contentHash.render == declared,
        (),
        CodecError.InputIdentity(declared, value.contentHash.render)
      )
    yield value

  private def readTimes(samples: Json, length: Int): Either[CodecError, Vector[Instant]] =
    column(samples, "tMicros", length).flatMap(_.zipWithIndex.traverse { case (raw, i) =>
      DomainWire.readTime(raw, s"samples.tMicros[$i]").map(Instant.micros)
    })

  def writeBinocular[U <: Unit2D](value: BinocularRecording[U]): Either[CodecError, Json] =
    checkLength(Json.Null, value.size).map { _ =>
      val n = value.size
      Json.obj(
        "frame"                   -> Json.fromString(value.frame.id.name),
        "clock"                   -> Json.fromString(value.clock.name),
        "pupilUnit"               -> value.pupilUnit.fold(Json.Null)(name(pupilUnits, _)),
        "rate"                    -> rate(value.rate),
        "samplingToleranceMicros" -> DomainWire.time(value.samplingTolerance.toSpan.toMicros),
        "recording"               -> Json.fromString(value.contentHash.render),
        "samples"                 -> Json.obj(
          "length"  -> Json.fromInt(n),
          "tMicros" -> Json.arr(
            Vector.tabulate(n)(i => DomainWire.time(value.timestamps(i).toMicros))*
          ),
          "left"  -> Json.fromFields(gazeColumns(value.leftGaze)),
          "right" -> Json.fromFields(gazeColumns(value.rightGaze))
        )
      )
    }

  def readBinocular[U <: Unit2D: UnitLabel](
      json: Json,
      table: DocumentIdentities
  ): Either[CodecError, BinocularRecording[U]] =
    for
      frameName <- Wire.field[String](json, "frame")
      frame     <- table.frame[U](FrameId(frameName))
      clockName <- Wire.field[String](json, "clock")
      clock     <- table.clock(ClockId(clockName))
      pupilJson <- Wire.field[Option[Json]](json, "pupilUnit")
      pupilUnit <- pupilJson.traverse(_ => lookup(pupilUnits, json, "pupilUnit", "pupil unit"))
      rate      <- readRate(json)
      tolerance <- readTolerance(json)
      declared  <- Wire.field[String](json, "recording")
      samples   <- Wire.field[Json](json, "samples")
      length    <- Wire.field[Int](samples, "length")
      _         <- checkLength(samples, length)
      times     <- readTimes(samples, length)
      left      <- Wire
        .field[Json](samples, "left")
        .flatMap(readGazeColumns[U](_, length, "samples.left"))
      right <- Wire
        .field[Json](samples, "right")
        .flatMap(readGazeColumns[U](_, length, "samples.right"))
      value <- BinocularRecording
        .of(frame, clock, rate, pupilUnit, IArray.from(times), left, right, tolerance)
        .left
        .map(CodecError.Recording("samples", _))
      _ <- Either.cond(
        value.contentHash.render == declared,
        (),
        CodecError.InputIdentity(declared, value.contentHash.render)
      )
    yield value
