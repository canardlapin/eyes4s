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
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.plan.*
import io.circe.Json

/** How a temporal payload carries its base fixation-study input. */
enum StudyEmbedding derives CanEqual:
  /** The complete study-input payload is nested in the temporal payload. */
  case Inline

  /** Only the study-input digest is written; decoding resolves it through a
    * caller-supplied lookup and fails with `PlanError.MissingArtifact` when
    * the referenced input is not available. Decoding never loads artifacts.
    */
  case ByReference

/** Built-in temporal input codecs and the default lookup for by-reference payloads. */
object TemporalInputCodecs:
  val input: DefinitionId = DefinitionId.temporalStudyInput

  /** A lookup that resolves nothing; by-reference payloads then fail with
    * `PlanError.MissingArtifact` on decode.
    */
  def unresolved[K, U <: Unit2D]: ArtifactRef[StudyInput[K, U]] => Option[StudyInput[K, U]] =
    _ => Option.empty[StudyInput[K, U]]

  /** The ordinary participant/stimulus/phase route over `StudyInputCodecs.study`. */
  def study[U <: Unit2D: UnitLabel](
      embedding: StudyEmbedding = StudyEmbedding.Inline,
      resolve: ArtifactRef[StudyInput[StudyKey, U]] => Option[StudyInput[StudyKey, U]] =
        unresolved[StudyKey, U]
  ): TemporalInputCodec[StudyKey, U] =
    new TemporalInputCodec(input, StudyInputCodecs.study[U], embedding, resolve)

/** Versioned codec for `TemporalStudyInput[K, U]`: the base study input inline
  * or by reference, and one epoch per trial that has one, carrying the
  * measured anchor and observed coverage on a typed clock. Trials without an
  * epoch stay absent, which the temporal plan reports as `MissingEpoch` at run
  * time rather than the codec inventing coverage. Decoding rebuilds the input
  * through `TemporalStudyInput.of`, so duplicate or foreign epochs are the
  * constructor's own refusals, and then compares the reconstructed digest
  * with the declared one.
  */
final class TemporalInputCodec[K, U <: Unit2D](
    val schema: DefinitionId,
    val study: StudyInputCodec[K, U],
    val embedding: StudyEmbedding,
    val resolve: ArtifactRef[StudyInput[K, U]] => Option[StudyInput[K, U]]
)(using unit: UnitLabel[U]):
  private given keyDigest: KeyDigest[K]  = study.layout.digest
  private given keyOrdering: Ordering[K] = study.layout.ordering

  val input: VersionedCodec[TemporalStudyInput[K, U]] =
    VersionedCodec.checked(schema)(writeInput)(readInput)

  private def writeInput(value: TemporalStudyInput[K, U]): Either[CodecError, Json] =
    val epochs = value.epochs.toVector.sortBy(_._1)
    val table  = epochs.foldLeft(DocumentIdentities.empty) { case (t, (_, epoch)) =>
      t.addClock(epoch.coverage.clock)
    }
    for
      base <- embedding match
        case StudyEmbedding.Inline =>
          study.input
            .encode(value.study)
            .map(payload => Json.obj("kind" -> Json.fromString("inline"), "payload" -> payload))
            .left
            .map(Wire.at("study"))
        case StudyEmbedding.ByReference =>
          Right(
            Json.obj(
              "kind"   -> Json.fromString("reference"),
              "digest" -> Json.fromString(value.study.reference.digest)
            )
          )
      rows <- epochs.zipWithIndex.traverse { case ((key, epoch), index) =>
        study.keys
          .encode(key)
          .map(k =>
            Json.obj(
              "key"          -> k,
              "anchorMicros" -> DomainWire.time(epoch.anchor.toMicros),
              "coverage"     -> Json.obj(
                "clock"     -> Json.fromString(epoch.coverage.clock.name),
                "intervals" -> Json.arr(epoch.coverage.intervals.map(DomainWire.interval)*)
              )
            )
          )
          .left
          .map(Wire.at(s"epochs[$index]"))
      }
    yield Json.obj(
      "layout"     -> Wire.id(study.layout.id),
      "keySchema"  -> Wire.id(study.keys.schema),
      "unit"       -> Json.fromString(unit.symbol),
      "input"      -> Json.fromString(value.reference.digest),
      "study"      -> base,
      "identities" -> table.json,
      "epochs"     -> Json.arr(rows*)
    )

  private def readInput(json: Json): Either[CodecError, TemporalStudyInput[K, U]] = for
    _      <- Wire.requireId(json, "layout", study.layout.id)
    _      <- Wire.requireId(json, "keySchema", study.keys.schema)
    symbol <- Wire.field[String](json, "unit")
    _      <- Either.cond(
      symbol == unit.symbol,
      (),
      CodecError.Field("unit", json, s"expected ${unit.symbol}, got $symbol")
    )
    declared <- Wire.field[String](json, "input")
    _        <- ArtifactRef
      .parse[TemporalStudyInput[K, U]](declared)
      .left
      .map(CodecError.Definition.apply)
    base   <- Wire.field[Json](json, "study").flatMap(readStudy).left.map(Wire.at("study"))
    table  <- Wire.field[Json](json, "identities").flatMap(DocumentIdentities.read)
    rows   <- Wire.field[Vector[Json]](json, "epochs")
    epochs <- rows.zipWithIndex.traverse { case (row, index) =>
      readEpoch(row, table, base).left.map(Wire.at(s"epochs[$index]"))
    }
    value <- TemporalStudyInput.of(base, epochs).left.map(CodecError.Temporal.apply)
    _     <- Wire.ascending("epochs", epochs.map(_._1).zip(rows))
    _     <- Either.cond(
      value.reference.digest == declared,
      (),
      CodecError.InputIdentity(declared, value.reference.digest)
    )
  yield value

  private def readStudy(json: Json): Either[CodecError, StudyInput[K, U]] =
    Wire.field[String](json, "kind").flatMap {
      case "inline"    => Wire.field[Json](json, "payload").flatMap(study.input.decode)
      case "reference" =>
        for
          digest <- Wire.field[String](json, "digest")
          ref    <- ArtifactRef
            .parse[StudyInput[K, U]](digest)
            .left
            .map(CodecError.Definition.apply)
          value <- resolve(ref).toRight(
            CodecError.Definition(PlanError.MissingArtifact(digest))
          )
          _ <- Either.cond(
            value.reference == ref,
            (),
            CodecError.InputIdentity(digest, value.reference.digest)
          )
        yield value
      case other => Left(CodecError.Field("kind", json, s"unknown study embedding $other"))
    }

  /** An epoch's coverage must be observed on its trial's own clock; the
    * temporal plan would otherwise refuse the window at run time, so the
    * codec names the disagreement before execution.
    */
  private def readEpoch(
      json: Json,
      table: DocumentIdentities,
      base: StudyInput[K, U]
  ): Either[CodecError, (K, TrialEpoch)] = for
    key       <- Wire.field[Json](json, "key").flatMap(study.keys.decode)
    anchor    <- DomainWire.micros(json, "anchorMicros")
    coverage  <- Wire.field[Json](json, "coverage")
    clockName <- Wire.field[String](coverage, "clock")
    clock     <- table.clock(ClockId(clockName))
    _         <- base.trials.rows.find(_.key == key).map(_.value.clock) match
      case Some(trialClock) if trialClock != clock =>
        Left(
          CodecError.Field(
            "coverage",
            coverage,
            s"coverage is observed on clock '${clock.name}' but the trial scanpath is on " +
              s"clock '${trialClock.name}'"
          )
        )
      case _ => Right(())
    raw       <- Wire.field[Vector[Json]](coverage, "intervals")
    intervals <- raw.zipWithIndex.traverse { case (interval, index) =>
      DomainWire.readInterval(interval).left.map(Wire.at(s"coverage.intervals[$index]"))
    }
    _        <- Wire.ascending("coverage.intervals", intervals.map(_.onset.toMicros).zip(raw))
    observed <- ObservedCoverage
      .of(clock, intervals)
      .left
      .map(e => CodecError.Field("coverage", coverage, e.message))
  yield key -> TrialEpoch(Instant.micros(anchor), observed)

/** Conditional codecs for neutral clock-bound timelines: given a codec for
  * the mark values, a timeline is its clock and its ordered marks with exact
  * microsecond instants. Equal instants keep their order.
  */
object TimelineCodecs:
  def timeline[A](schema: DefinitionId, value: VersionedCodec[A]): VersionedCodec[Timeline[A]] =
    VersionedCodec.checked[Timeline[A]](schema)(line =>
      line.marks.zipWithIndex
        .traverse { case (mark, index) =>
          value
            .encode(mark.value)
            .map(v => Json.obj("atMicros" -> DomainWire.time(mark.at.toMicros), "value" -> v))
            .left
            .map(Wire.at(s"marks[$index]"))
        }
        .map(marks =>
          Json.obj("clock" -> Json.fromString(line.clock.name), "marks" -> Json.arr(marks*))
        )
    ) { json =>
      for
        timing <- Wire.omittable[String](json, "timing")
        _      <- Either.cond(
          timing.isEmpty,
          (),
          CodecError.Field(
            "timing",
            json,
            s"a ${timing.getOrElse("")} timeline cannot be read as a neutral timeline; use the matching codec"
          )
        )
        clock <- Wire.field[String](json, "clock")
        raw   <- Wire.field[Vector[Json]](json, "marks")
        marks <- raw.zipWithIndex.traverse { case (mark, index) =>
          (for
            at <- DomainWire.micros(mark, "atMicros")
            v  <- Wire.field[Json](mark, "value").flatMap(value.decode)
          yield Mark(Instant.micros(at), v)).left.map(Wire.at(s"marks[$index]"))
        }
        line <- Timeline
          .of(ClockId(clock), marks)
          .left
          .map(e => CodecError.Field("timeline", json, e.message))
      yield line
    }

  def planned[A](
      schema: DefinitionId,
      value: VersionedCodec[A]
  ): VersionedCodec[PlannedTimeline[A]] =
    val inner = timeline(schema, value)
    VersionedCodec.checked[PlannedTimeline[A]](schema)(line =>
      inner
        .encode(line.timeline)
        .flatMap(payload =>
          Wire
            .field[Json](payload, "value")
            .map(_.mapObject(_.add("timing", Json.fromString("planned"))))
        )
    ) { json =>
      for
        timing <- Wire.field[String](json, "timing")
        _      <- Either.cond(
          timing == "planned",
          (),
          CodecError.Field("timing", json, s"expected planned timing, got $timing")
        )
        line <- inner.decode(
          Json.obj("schema" -> Wire.id(schema), "value" -> json.mapObject(_.remove("timing")))
        )
      yield PlannedTimeline.from(line)
    }

  def observed[A](
      schema: DefinitionId,
      value: VersionedCodec[A]
  ): VersionedCodec[ObservedTimeline[A]] =
    val inner = timeline(schema, value)
    VersionedCodec.checked[ObservedTimeline[A]](schema)(line =>
      inner
        .encode(line.timeline)
        .flatMap(payload =>
          Wire
            .field[Json](payload, "value")
            .map(_.mapObject(_.add("timing", Json.fromString("observed"))))
        )
    ) { json =>
      for
        timing <- Wire.field[String](json, "timing")
        _      <- Either.cond(
          timing == "observed",
          (),
          CodecError.Field("timing", json, s"expected observed timing, got $timing")
        )
        line <- inner.decode(
          Json.obj("schema" -> Wire.id(schema), "value" -> json.mapObject(_.remove("timing")))
        )
      yield ObservedTimeline.from(line)
    }
