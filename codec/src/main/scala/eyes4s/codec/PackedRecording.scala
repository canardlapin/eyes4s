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

/** An encoded packed recording: the JSON document and the payloads it
  * references by digest. Store the payloads as separate artifacts.
  */
final class PackedRecording private[codec] (
    val document: Json,
    val payloads: Vector[VerifiedPayload]
)

object PackedRecordingCodecs:
  val schema: DefinitionId = DefinitionId.packedRecording

  /** Support codes of the `support` column: the category in bits 0-1 and a
    * measured pupil in bit 2, which only a tracked sample may carry.
    */
  val Tracked: Int   = 0
  val Blink: Int     = 1
  val Lost: Int      = 2
  val OffScreen: Int = 3
  val PupilBit: Int  = 4

  /** The largest distinct-lineage dictionary a `uint8` index addresses. */
  val maximumLineages: Int = 256

  def recording[U <: Unit2D: UnitLabel]: PackedRecordingCodec[U] =
    new PackedRecordingCodec[U](schema)

  /** The payload references a packed-recording document declares, in the
    * order `tMicros`, `support`, `lineage`, `values`; nothing is resolved.
    */
  def references(document: Json): Either[CodecError, Vector[PayloadRef]] = for
    found   <- Wire.definition(document, "schema")
    _       <- Either.cond(found == schema, (), CodecError.Schema(schema, found))
    value   <- Wire.field[Json](document, "value")
    body    <- Wire.field[Json](value, "recording")
    samples <- Wire.field[Json](body, "samples")
    refs    <- Vector("tMicros", "support", "lineage", "values").traverse(field =>
      Wire
        .field[Json](samples, field)
        .flatMap(PayloadRef.read)
        .left
        .map(Wire.at(s"recording.samples.$field"))
    )
  yield refs

/** A monocular recording as JSON metadata plus four typed payloads: the
  * escape hatch for recordings above the inline bound
  * [[RecordingInputCodecs.maximumSamples]].
  *
  * The metadata is the inline `eyes4s.recording@1` shape (frame and clock by
  * nominal ID, eye, pupil unit, rate, sampling tolerance and the declared
  * `Recording.contentHash`); the samples are four payload references:
  *
  *   - `tMicros`: `int64[n]`, exact microsecond timestamps;
  *   - `support`: `uint8[n]`, the support code (see `PackedRecordingCodecs`);
  *   - `lineage`: `uint8[n]`, an index into the `lineages` dictionary, which
  *     lists each distinct derivation in order of first appearance;
  *   - `values`: `float64[n, 3]` in column-major order, so the `x`, `y` and
  *     `pupil` columns follow one another; an absent value is `+0.0` exactly.
  *
  * Every multi-byte value is little-endian. Decoding requires the declared
  * layouts, reads each payload through a caller-supplied lookup of verified
  * payloads (it never loads anything itself), rebuilds the recording through
  * `Recording.of` and compares its `contentHash` with the declared one. The
  * only size bound is the payload bound, [[PayloadLayout.maximumBytes]].
  */
final class PackedRecordingCodec[U <: Unit2D] private[codec] (val schema: DefinitionId)(using
    unit: UnitLabel[U]
):
  import PackedRecordingCodecs.{Blink, Lost, OffScreen, PupilBit, Tracked, maximumLineages}

  def encode(value: Recording[U]): Either[CodecError, PackedRecording] =
    val n        = value.size
    val lineages = value.samples.toVector.map(_.lineage).distinct
    for
      table <- DocumentIdentities.empty.addFrame(value.frame).map(_.addClock(value.clock))
      _     <- Either.cond(
        lineages.size <= maximumLineages,
        (),
        CodecError.Unsupported(
          "samples.lineages",
          s"${lineages.size} distinct lineages exceed the $maximumLineages a uint8 index addresses"
        )
      )
      // Every layout is validated against the payload bound before any
      // column is allocated.
      layouts <- (for
        timeLayout  <- PayloadLayout.column(ElementKind.Int64, n)
        codeLayout  <- PayloadLayout.column(ElementKind.UInt8, n)
        valueLayout <- PayloadLayout.of(
          ElementKind.Float64,
          Vector(n, 3),
          ArrayOrder.ColumnMajor
        )
      yield (timeLayout, codeLayout, valueLayout)).left.map(CodecError.Payload("samples", _))
      (timeLayout, codeLayout, valueLayout) = layouts
      index                                 = lineages.zipWithIndex.toMap
      times   <- pack(timeLayout, IArray.tabulate(n)(i => value.samples(i).t.toMicros))
      support <- pack(
        codeLayout,
        IArray.tabulate(n)(i => code(value.samples(i).gaze).toByte)
      )
      lineage <- pack(
        codeLayout,
        IArray.tabulate(n)(i => index(value.samples(i).lineage).toByte)
      )
      values <- pack(
        valueLayout,
        IArray.tabulate(3 * n) { k =>
          val gaze = value.samples(k % n).gaze
          (k / n) match
            case 0 => gaze.position.fold(0.0)(_.x)
            case 1 => gaze.position.fold(0.0)(_.y)
            case _ => gaze.pupil.getOrElse(0.0)
        }
      )
    yield
      val body = Json.obj(
        "frame"     -> Json.fromString(value.frame.id.name),
        "clock"     -> Json.fromString(value.clock.name),
        "eye"       -> Json.fromString(RecordingInputWire.eyeName(value.eye)),
        "pupilUnit" -> value.pupilUnit.fold(Json.Null)(u =>
          Json.fromString(RecordingInputWire.pupilUnitName(u))
        ),
        "rate"                    -> RecordingInputWire.rate(value.rate),
        "samplingToleranceMicros" -> DomainWire.time(value.samplingTolerance.toSpan.toMicros),
        "recording"               -> Json.fromString(value.contentHash.render),
        "samples"                 -> Json.obj(
          "length"   -> Json.fromInt(n),
          "lineages" -> Json.arr(lineages.map(RecordingInputWire.lineage)*),
          "tMicros"  -> PayloadRef.json(times.ref),
          "support"  -> PayloadRef.json(support.ref),
          "lineage"  -> PayloadRef.json(lineage.ref),
          "values"   -> PayloadRef.json(values.ref)
        )
      )
      new PackedRecording(
        Json.obj(
          "schema" -> Wire.id(schema),
          "value"  -> Json.obj("identities" -> table.json, "recording" -> body)
        ),
        Vector(times, support, lineage, values)
      )

  /** Rebuild the recording from its document and already verified payloads.
    * `payloads` is consulted only for the four declared references.
    */
  def decode(
      document: Json,
      payloads: PayloadRef => Option[VerifiedPayload]
  ): Either[CodecError, Recording[U]] = for
    found <- Wire.definition(document, "schema")
    _     <- Either.cond(found == schema, (), CodecError.Schema(schema, found))
    value <- Wire.field[Json](document, "value")
    table <- Wire.field[Json](value, "identities").flatMap(DocumentIdentities.read)
    body  <- Wire.field[Json](value, "recording")
    rec   <- readRecording(body, table, payloads).left.map(Wire.at("recording"))
  yield rec

  private def pack[A: PackedElement](
      layout: PayloadLayout,
      values: IArray[A]
  ): Either[CodecError, VerifiedPayload] =
    PackedArrays.pack(layout, values).left.map(CodecError.Payload("samples", _))

  private def code(gaze: Gaze[U]): Int = gaze match
    case Gaze.Tracked(_, pupil) => Tracked | pupil.fold(0)(_ => PupilBit)
    case Gaze.Blink()           => Blink
    case Gaze.Lost()            => Lost
    case Gaze.OffScreen(_)      => OffScreen

  private def column[A: PackedElement](
      samples: Json,
      field: String,
      expected: Either[PayloadError, PayloadLayout],
      payloads: PayloadRef => Option[VerifiedPayload]
  ): Either[CodecError, IArray[A]] =
    val path = s"samples.$field"
    for
      layout <- expected.left.map(CodecError.Payload(path, _))
      raw    <- Wire.field[Json](samples, field)
      ref    <- PayloadRef.read(raw).left.map(Wire.at(path))
      _      <- Either.cond(
        ref.layout == layout,
        (),
        CodecError.Field(
          path,
          raw,
          s"expected ${layout.element.wire}${layout.shape.mkString("[", ",", "]")} " +
            s"${layout.order.wire}"
        )
      )
      payload <- payloads(ref)
        .filter(_.ref == ref)
        .toRight(CodecError.MissingPayload(path, ref))
      values <- PackedArrays.unpack[A](payload).left.map(CodecError.Payload(path, _))
    yield values

  private def readRecording(
      json: Json,
      table: DocumentIdentities,
      payloads: PayloadRef => Option[VerifiedPayload]
  ): Either[CodecError, Recording[U]] = for
    frameName <- Wire.field[String](json, "frame")
    frame     <- table.frame[U](FrameId(frameName))
    clockName <- Wire.field[String](json, "clock")
    clock     <- table.clock(ClockId(clockName))
    eye       <- RecordingInputWire.readEye(json)
    pupilUnit <- RecordingInputWire.readPupilUnit(json)
    rate      <- RecordingInputWire.readRate(json)
    tolerance <- RecordingInputWire.readTolerance(json)
    declared  <- Wire.field[String](json, "recording")
    samples   <- Wire.field[Json](json, "samples")
    n         <- Wire.field[Int](samples, "length")
    _         <- Either.cond(
      n >= 1,
      (),
      CodecError.Field("samples.length", samples, "expected at least one")
    )
    names      <- Wire.field[Vector[Json]](samples, "lineages")
    dictionary <- names.zipWithIndex.traverse { case (name, i) =>
      RecordingInputWire.readLineage(name).left.map(Wire.at(s"samples.lineages[$i]"))
    }
    _ <- Either.cond(
      dictionary.distinct.size == dictionary.size && dictionary.size <= maximumLineages,
      (),
      CodecError.Field(
        "samples.lineages",
        samples,
        s"expected at most $maximumLineages distinct lineages"
      )
    )
    times <- column[Long](
      samples,
      "tMicros",
      PayloadLayout.column(ElementKind.Int64, n),
      payloads
    )
    support <- column[Byte](
      samples,
      "support",
      PayloadLayout.column(ElementKind.UInt8, n),
      payloads
    )
    codes <- column[Byte](
      samples,
      "lineage",
      PayloadLayout.column(ElementKind.UInt8, n),
      payloads
    )
    values <- column[Double](
      samples,
      "values",
      PayloadLayout.of(ElementKind.Float64, Vector(n, 3), ArrayOrder.ColumnMajor),
      payloads
    )
    lineage <- lineages(codes, dictionary)
    gazes   <- (0 until n).toVector.traverse(i =>
      gaze(support(i) & 0xff, values(i), values(n + i), values(2 * n + i)).left
        .map(Wire.at(s"samples[$i]"))
    )
    value <- Recording
      .of(
        frame,
        clock,
        rate,
        eye,
        pupilUnit,
        IArray.tabulate(n)(i => Sample(Instant.micros(times(i)), gazes(i), lineage(i))),
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

  /** Indices must address the dictionary in order of first appearance and use
    * every entry, so one recording has exactly one packed form.
    */
  private def lineages(
      codes: IArray[Byte],
      dictionary: Vector[SampleLineage]
  ): Either[CodecError, Vector[SampleLineage]] =
    var next  = 0
    var error = Option.empty[CodecError]
    var i     = 0
    while i < codes.length && error.isEmpty do
      val code = codes(i) & 0xff
      if code >= dictionary.size then
        error = Some(
          CodecError.Field(
            s"samples.lineage[$i]",
            Json.fromInt(code),
            s"lineage index $code is outside a dictionary of ${dictionary.size}"
          )
        )
      else if code == next then next += 1
      else if code > next then
        error = Some(
          CodecError.Field(
            s"samples.lineage[$i]",
            Json.fromInt(code),
            s"lineage index $code precedes the first use of index $next"
          )
        )
      i += 1
    error match
      case Some(e)                        => Left(e)
      case None if next < dictionary.size =>
        Left(
          CodecError.Field(
            "samples.lineages",
            Json.fromInt(dictionary.size),
            s"lineage dictionary entries from index $next are never used"
          )
        )
      case None => Right(codes.toVector.map(c => dictionary(c & 0xff)))

  private def gaze(
      code: Int,
      x: Double,
      y: Double,
      pupil: Double
  ): Either[CodecError, Gaze[U]] =
    def finite(value: Double, field: String): Either[CodecError, Double] =
      Either.cond(
        value.isFinite,
        value,
        CodecError.Field(field, Json.fromDoubleOrString(value), "expected a finite number")
      )
    def absent(value: Double, field: String): Either[CodecError, Unit] =
      Either.cond(
        java.lang.Double.doubleToLongBits(value) == 0L,
        (),
        CodecError.Field(
          field,
          Json.fromDoubleOrString(value),
          "an absent value is +0.0 exactly"
        )
      )
    val category = code & 3
    val measured = (code & PupilBit) != 0
    if (code & ~7) != 0 || (measured && category != Tracked) then
      Left(CodecError.Field("support", Json.fromInt(code), "unknown support code"))
    else
      category match
        case PackedRecordingCodecs.Tracked =>
          for
            px <- finite(x, "x")
            py <- finite(y, "y")
            ps <-
              if measured then finite(pupil, "pupil").map(Some(_))
              else absent(pupil, "pupil").as(Option.empty[Double])
          yield Gaze.Tracked(Pt[U](px, py), ps)
        case PackedRecordingCodecs.OffScreen =>
          for
            px <- finite(x, "x")
            py <- finite(y, "y")
            _  <- absent(pupil, "pupil")
          yield Gaze.OffScreen(Pt[U](px, py))
        case other =>
          for
            _ <- absent(x, "x")
            _ <- absent(y, "y")
            _ <- absent(pupil, "pupil")
          yield if other == PackedRecordingCodecs.Blink then Gaze.Blink[U]() else Gaze.Lost[U]()
