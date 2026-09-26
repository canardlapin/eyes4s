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
import eyes4s.aoi.*
import eyes4s.compare.ComparisonWorkError
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.plan.*
import io.circe.Json

/** Derived members of an archive: written from the value, and on decode
  * compared with the value re-derived from the archive's own evidence.
  */
private[codec] object Derived:
  /** The declared JSON must carry every member of the derived JSON with the
    * same value; members the derivation does not write are ignored, as the
    * schema compatibility policy ignores unknown members. The first
    * difference is reported at its located path with both values.
    */
  def check(path: String, derived: Json, declared: Json): Either[CodecError, Unit] =
    (derived.asObject, derived.asArray) match
      case (Some(fields), _) =>
        declared.asObject match
          case None       => Left(CodecError.Derived(path, declared, derived))
          case Some(held) =>
            fields.toVector.traverse_ { (name, value) =>
              held(name) match
                case None =>
                  Left(CodecError.Field(s"$path.$name", declared, "missing derived member"))
                case Some(found) => check(s"$path.$name", value, found)
            }
      case (_, Some(values)) =>
        declared.asArray match
          case Some(held) if held.size == values.size =>
            values.zip(held).zipWithIndex.traverse_ { case ((value, found), index) =>
              check(s"$path[$index]", value, found)
            }
          case Some(held) =>
            Left(
              CodecError.Derived(
                s"$path.length",
                Json.fromInt(held.size),
                Json.fromInt(values.size)
              )
            )
          case None => Left(CodecError.Derived(path, declared, derived))
      case _ =>
        // Numbers compare as the doubles they denote, never through a decimal
        // rendering, which differs between JDK releases and Scala.js.
        val same = (derived.asNumber, declared.asNumber) match
          case (Some(a), Some(b)) => java.lang.Double.compare(a.toDouble, b.toDouble) == 0
          case _                  => derived == declared
        Either.cond(same, (), CodecError.Derived(path, declared, derived))

/** Wire forms of the temporal failure family: every case of
  * `TemporalStudyError` and the plan, occupancy and scheduling errors it
  * wraps, each with an explicit `kind` tag and its operands.
  */
private[codec] object TemporalWire:
  import ResultWire.{kind, long, readLong, strings, tagged, unknown}

  private def text(value: String): Json = Json.fromString(value)
  private def int(value: Int): Json     = Json.fromInt(value)
  private def big(value: BigInt): Json  = Json.fromString(value.toString)

  private def readBig(json: Json, field: String): Either[CodecError, BigInt] =
    Wire
      .field[String](json, field)
      .flatMap(raw =>
        Either
          .catchOnly[NumberFormatException](BigInt(raw))
          .filterOrElse(_.toString == raw, ())
          .leftMap(_ =>
            CodecError.Field(field, json, "expected a canonical integer as a decimal string")
          )
      )

  def evaluationSpecError(error: EvaluationSpecError): Json = error match
    case EvaluationSpecError.EmptyField(f, v) =>
      tagged("emptyField", "field" -> text(f), "value" -> text(v))
    case EvaluationSpecError.InvalidComponents(v) =>
      tagged("invalidComponents", "values" -> strings(v))
    case EvaluationSpecError.InvalidParameters(v) =>
      tagged("invalidParameters", "values" -> ResultWire.params(v))

  def readEvaluationSpecError(json: Json): Either[CodecError, EvaluationSpecError] =
    kind(json).flatMap {
      case "emptyField" =>
        (Wire.field[String](json, "field"), Wire.field[String](json, "value"))
          .mapN(EvaluationSpecError.EmptyField.apply)
      case "invalidComponents" =>
        Wire
          .field[Vector[String]](json, "values")
          .map(EvaluationSpecError.InvalidComponents.apply)
      case "invalidParameters" =>
        ResultWire.readParams(json, "values").map(EvaluationSpecError.InvalidParameters.apply)
      case other => Left(unknown(json, "evaluation specification error", other))
    }

  def pairScheduleError(error: PairScheduleError): Json =
    import PairScheduleError.*
    error match
      case InvalidBudget(rows, candidates, selected) =>
        tagged(
          "invalidBudget",
          "sourceRows"     -> int(rows),
          "candidatePairs" -> long(candidates),
          "selectedPairs"  -> int(selected)
        )
      case InvalidCounts(l, r) => tagged("invalidCounts", "left" -> long(l), "right" -> long(r))
      case InvalidQuantum(v)   => tagged("invalidQuantum", "value" -> int(v))
      case SourceBudget(l, r, m) =>
        tagged("sourceBudget", "left" -> long(l), "right" -> long(r), "maximum" -> int(m))
      case CandidateBudget(l, r, m) =>
        tagged("candidateBudget", "left" -> long(l), "right" -> long(r), "maximum" -> long(m))
      case SelectedBudget(relation, attempted, m) =>
        tagged(
          "selectedBudget",
          "relation"  -> text(relation),
          "attempted" -> long(attempted),
          "maximum"   -> int(m)
        )

  def readPairScheduleError(json: Json): Either[CodecError, PairScheduleError] =
    import PairScheduleError.*
    def pair = (readLong(json, "left"), readLong(json, "right")).tupled
    kind(json).flatMap {
      case "invalidBudget" =>
        (
          Wire.field[Int](json, "sourceRows"),
          readLong(json, "candidatePairs"),
          Wire.field[Int](json, "selectedPairs")
        ).mapN(InvalidBudget.apply)
      case "invalidCounts"  => pair.map(InvalidCounts.apply)
      case "invalidQuantum" => Wire.field[Int](json, "value").map(InvalidQuantum.apply)
      case "sourceBudget"   =>
        (pair, Wire.field[Int](json, "maximum")).mapN { case ((l, r), m) =>
          SourceBudget(l, r, m)
        }
      case "candidateBudget" =>
        (pair, readLong(json, "maximum")).mapN { case ((l, r), m) => CandidateBudget(l, r, m) }
      case "selectedBudget" =>
        (
          Wire.field[String](json, "relation"),
          readLong(json, "attempted"),
          Wire.field[Int](json, "maximum")
        ).mapN(SelectedBudget.apply)
      case other => Left(unknown(json, "pair schedule error", other))
    }

  def comparisonWorkError(error: ComparisonWorkError): Json = error match
    case ComparisonWorkError.InvalidQuantum(v) => tagged("invalidQuantum", "value" -> int(v))
    case ComparisonWorkError.InvalidBudget(m)  =>
      tagged("invalidBudget", "maxWorkUnits" -> long(m))
    case ComparisonWorkError.WorkBudget(measure, units, maximum) =>
      tagged(
        "workBudget",
        "measure"   -> text(measure),
        "workUnits" -> long(units),
        "maximum"   -> long(maximum)
      )

  def readComparisonWorkError(json: Json): Either[CodecError, ComparisonWorkError] =
    kind(json).flatMap {
      case "invalidQuantum" =>
        Wire.field[Int](json, "value").map(ComparisonWorkError.InvalidQuantum.apply)
      case "invalidBudget" =>
        readLong(json, "maxWorkUnits").map(ComparisonWorkError.InvalidBudget.apply)
      case "workBudget" =>
        (
          Wire.field[String](json, "measure"),
          readLong(json, "workUnits"),
          readLong(json, "maximum")
        ).mapN(ComparisonWorkError.WorkBudget.apply)
      case other => Left(unknown(json, "comparison work error", other))
    }

  def planError(error: PlanError): Json =
    import PlanError.*
    error match
      case InvalidDefinition(n, v) =>
        tagged("invalidDefinition", "name" -> text(n), "version" -> int(v))
      case InvalidArtifact(d)     => tagged("invalidArtifact", "digest" -> text(d))
      case MissingArtifact(d)     => tagged("missingArtifact", "digest" -> text(d))
      case ArtifactMismatch(e, a) =>
        tagged("artifactMismatch", "expected" -> text(e), "actual" -> text(a))
      case InvalidPhases(f, r) =>
        tagged("invalidPhases", "focal" -> text(f), "reference" -> text(r))
      case EmptyScales(n)         => tagged("emptyScales", "count" -> int(n))
      case DuplicateScales(names) => tagged("duplicateScales", "names" -> strings(names))
      case Specification(e)       => tagged("specification", "error" -> evaluationSpecError(e))
      case Schedule(e)            => tagged("schedule", "error" -> pairScheduleError(e))
      case StudyWorkBudget(focal, reference, scales, maximum) =>
        tagged(
          "studyWorkBudget",
          "focalTrials"            -> int(focal),
          "referenceTrials"        -> int(reference),
          "scales"                 -> int(scales),
          "maximumCandidateVisits" -> long(maximum)
        )
      case ChangedPreparedPlan(m, l) =>
        tagged("changedPreparedPlan", "method" -> Wire.id(m), "layout" -> Wire.id(l))
      case ComparisonWork(e) => tagged("comparisonWork", "error" -> comparisonWorkError(e))
      case UnsupportedExecution(m, c) =>
        tagged("unsupportedExecution", "method" -> Wire.id(m), "capability" -> text(c.toString))
      case MissingAngularScale(scale) => tagged("missingAngularScale", "scale" -> int(scale))
      case Geometry(e) => tagged("geometry", "error" -> ResultWire.geometryError(e))
      case InvalidWindowTally(screen, window, total, screenMicros, windowMicros, totalMicros) =>
        tagged(
          "invalidWindowTally",
          "outsideScreen"       -> int(screen),
          "outsideWindow"       -> int(window),
          "total"               -> int(total),
          "outsideScreenMicros" -> long(screenMicros),
          "outsideWindowMicros" -> long(windowMicros),
          "totalMicros"         -> long(totalMicros)
        )

  def readPlanError(json: Json): Either[CodecError, PlanError] =
    import PlanError.*
    def str(field: String) = Wire.field[String](json, field)
    def error              = Wire.field[Json](json, "error")
    kind(json).flatMap {
      case "invalidDefinition" =>
        (str("name"), Wire.field[Int](json, "version")).mapN(InvalidDefinition.apply)
      case "invalidArtifact"  => str("digest").map(InvalidArtifact.apply)
      case "missingArtifact"  => str("digest").map(MissingArtifact.apply)
      case "artifactMismatch" => (str("expected"), str("actual")).mapN(ArtifactMismatch.apply)
      case "invalidPhases"    => (str("focal"), str("reference")).mapN(InvalidPhases.apply)
      case "emptyScales"      => Wire.field[Int](json, "count").map(EmptyScales.apply)
      case "duplicateScales"  =>
        Wire.field[Vector[String]](json, "names").map(DuplicateScales.apply)
      case "specification" =>
        error.flatMap(readEvaluationSpecError).map(Specification.apply)
      case "schedule"        => error.flatMap(readPairScheduleError).map(Schedule.apply)
      case "studyWorkBudget" =>
        (
          Wire.field[Int](json, "focalTrials"),
          Wire.field[Int](json, "referenceTrials"),
          Wire.field[Int](json, "scales"),
          readLong(json, "maximumCandidateVisits")
        ).mapN(StudyWorkBudget.apply)
      case "changedPreparedPlan" =>
        (Wire.definition(json, "method"), Wire.definition(json, "layout"))
          .mapN(ChangedPreparedPlan.apply)
      case "comparisonWork" =>
        error.flatMap(readComparisonWorkError).map(ComparisonWork.apply)
      case "unsupportedExecution" =>
        for
          method <- Wire.definition(json, "method")
          name   <- str("capability")
          value  <- ExecutionCapability.values
            .find(_.toString == name)
            .toRight(
              CodecError.Field("capability", json, s"unknown execution capability $name")
            )
        yield UnsupportedExecution(method, value)
      case "missingAngularScale" =>
        Wire.field[Int](json, "scale").map(MissingAngularScale.apply)
      case "geometry" => error.flatMap(ResultWire.readGeometryError).map(Geometry.apply)
      case "invalidWindowTally" =>
        (
          Wire.field[Int](json, "outsideScreen"),
          Wire.field[Int](json, "outsideWindow"),
          Wire.field[Int](json, "total"),
          readLong(json, "outsideScreenMicros"),
          readLong(json, "outsideWindowMicros"),
          readLong(json, "totalMicros")
        ).mapN(InvalidWindowTally.apply)
      case other => Left(unknown(json, "plan error", other))
    }

  def windowOccupancyError(error: WindowOccupancyError): Json =
    import WindowOccupancyError.*
    error match
      case Time(e)                 => tagged("time", "error" -> ResultWire.timeError(e))
      case Measure(e)              => tagged("measure", "error" -> ResultWire.surfaceError(e))
      case InvalidWidth(i, micros) =>
        tagged("invalidWidth", "interval" -> DomainWire.interval(i), "micros" -> big(micros))
      case EmptyCoverageInterval(c, xs) =>
        tagged(
          "emptyCoverageInterval",
          "clock"     -> ResultWire.clockId(c),
          "intervals" -> Json.arr(xs.map(DomainWire.interval)*)
        )
      case OverlappingCoverage(c, xs) =>
        tagged(
          "overlappingCoverage",
          "clock"     -> ResultWire.clockId(c),
          "intervals" -> Json.arr(xs.map(DomainWire.interval)*)
        )
      case ObservedTime(i, observed, missing) =>
        tagged(
          "observedTime",
          "interval"       -> DomainWire.interval(i),
          "observedMicros" -> long(observed),
          "missingMicros"  -> long(missing)
        )
      case Ledger(position, index, original, retained, boundary) =>
        tagged(
          "ledger",
          "position"       -> int(position),
          "index"          -> int(index),
          "originalMicros" -> big(original),
          "retainedMicros" -> long(retained),
          "boundary"       -> text(boundary.toString)
        )
      case MeasureSupport(retained, positions) =>
        tagged("measureSupport", "retained" -> int(retained), "positions" -> int(positions))

  def readBoundary(json: Json, field: String): Either[CodecError, FixationBoundary] =
    Wire
      .field[String](json, field)
      .flatMap(name =>
        FixationBoundary.values
          .find(_.toString == name)
          .toRight(CodecError.Field(field, json, s"unknown fixation boundary $name"))
      )

  def readWindowOccupancyError(json: Json): Either[CodecError, WindowOccupancyError] =
    import WindowOccupancyError.*
    def interval = Wire.field[Json](json, "interval").flatMap(DomainWire.readInterval)
    def coverage(build: (ClockId, Vector[Interval]) => WindowOccupancyError) =
      for
        c  <- ResultWire.readClockId(json, "clock")
        xs <- Wire
          .field[Vector[Json]](json, "intervals")
          .flatMap(_.traverse(DomainWire.readInterval))
      yield build(c, xs)
    kind(json).flatMap {
      case "time" =>
        Wire.field[Json](json, "error").flatMap(ResultWire.readTimeError).map(Time.apply)
      case "measure" =>
        Wire.field[Json](json, "error").flatMap(ResultWire.readSurfaceError).map(Measure.apply)
      case "invalidWidth" => (interval, readBig(json, "micros")).mapN(InvalidWidth.apply)
      case "emptyCoverageInterval" => coverage(EmptyCoverageInterval.apply)
      case "overlappingCoverage"   => coverage(OverlappingCoverage.apply)
      case "observedTime"          =>
        (interval, readLong(json, "observedMicros"), readLong(json, "missingMicros"))
          .mapN(ObservedTime.apply)
      case "ledger" =>
        (
          Wire.field[Int](json, "position"),
          Wire.field[Int](json, "index"),
          readBig(json, "originalMicros"),
          readLong(json, "retainedMicros"),
          readBoundary(json, "boundary")
        ).mapN(Ledger.apply)
      case "measureSupport" =>
        (Wire.field[Int](json, "retained"), Wire.field[Int](json, "positions"))
          .mapN(MeasureSupport.apply)
      case other => Left(unknown(json, "window occupancy error", other))
    }

  def temporalStudyError(error: TemporalStudyError): Json =
    import TemporalStudyError.*
    error match
      case Input(e)               => tagged("input", "error" -> planError(e))
      case Time(e)                => tagged("time", "error" -> ResultWire.timeError(e))
      case Occupancy(e)           => tagged("occupancy", "error" -> windowOccupancyError(e))
      case InvalidWindow(n, f, u) =>
        tagged(
          "invalidWindow",
          "name"        -> text(n),
          "fromMicros"  -> long(f),
          "untilMicros" -> long(u)
        )
      case AnchorOverflow(w, a, f, u) =>
        tagged(
          "anchorOverflow",
          "window"       -> text(w),
          "anchorMicros" -> long(a),
          "fromMicros"   -> long(f),
          "untilMicros"  -> long(u)
        )
      case InvalidRepetition(n, f, r) =>
        tagged(
          "invalidRepetition",
          "name"      -> text(n),
          "focal"     -> text(f),
          "reference" -> text(r)
        )
      case WindowNames(ns)     => tagged("windowNames", "names" -> strings(ns))
      case RepetitionNames(ns) => tagged("repetitionNames", "names" -> strings(ns))
      case DuplicateEpochs(ks) => tagged("duplicateEpochs", "keyDigests" -> strings(ks))
      case DuplicateTrials(ks) => tagged("duplicateTrials", "keyDigests" -> strings(ks))
      case UnknownEpochs(ks)   => tagged("unknownEpochs", "keyDigests" -> strings(ks))
      case MissingEpoch(k)     => tagged("missingEpoch", "keyDigest" -> text(k))
      case Weighting(w)        => tagged("weighting", "weight" -> text(w.toString))

  def readTemporalStudyError(json: Json): Either[CodecError, TemporalStudyError] =
    import TemporalStudyError.*
    def str(field: String)   = Wire.field[String](json, field)
    def names(field: String) = Wire.field[Vector[String]](json, field)
    def error                = Wire.field[Json](json, "error")
    kind(json).flatMap {
      case "input"         => error.flatMap(readPlanError).map(Input.apply)
      case "time"          => error.flatMap(ResultWire.readTimeError).map(Time.apply)
      case "occupancy"     => error.flatMap(readWindowOccupancyError).map(Occupancy.apply)
      case "invalidWindow" =>
        (str("name"), readLong(json, "fromMicros"), readLong(json, "untilMicros"))
          .mapN(InvalidWindow.apply)
      case "anchorOverflow" =>
        (
          str("window"),
          readLong(json, "anchorMicros"),
          readLong(json, "fromMicros"),
          readLong(json, "untilMicros")
        ).mapN(AnchorOverflow.apply)
      case "invalidRepetition" =>
        (str("name"), str("focal"), str("reference")).mapN(InvalidRepetition.apply)
      case "windowNames"     => names("names").map(WindowNames.apply)
      case "repetitionNames" => names("names").map(RepetitionNames.apply)
      case "duplicateEpochs" => names("keyDigests").map(DuplicateEpochs.apply)
      case "duplicateTrials" => names("keyDigests").map(DuplicateTrials.apply)
      case "unknownEpochs"   => names("keyDigests").map(UnknownEpochs.apply)
      case "missingEpoch"    => str("keyDigest").map(MissingEpoch.apply)
      case "weighting"       =>
        str("weight").flatMap(name =>
          Weight.values
            .find(_.toString == name)
            .toRight(CodecError.Field("weight", json, s"unknown weight $name"))
            .map(Weighting.apply)
        )
      case other => Left(unknown(json, "temporal study error", other))
    }

  /** One trial's occupancy in a temporal cell: the resolved window, the
    * boundary, observed and missing time, the complete fixation ledger and
    * the retained fixations' positions, with the measure frame by identity.
    * Weights are not written: they are the ledger's retained times.
    */
  def occupancy[U <: Unit2D](value: WindowOccupancy[U]): Json =
    tagged(
      "occupancy",
      "interval"       -> DomainWire.interval(value.interval),
      "boundary"       -> text(value.boundary.toString),
      "observedMicros" -> long(value.observedMicros),
      "missingMicros"  -> long(value.missingMicros),
      "fixations"      -> Json.arr(value.fixationTimes.map { row =>
        Json.obj(
          "index"          -> int(row.index),
          "originalMicros" -> big(row.originalMicros),
          "retainedMicros" -> long(row.retainedMicros)
        )
      }*),
      "frame"     -> ResultWire.frameId(value.measure.frame.id),
      "positions" -> Json.arr(value.measure.positions.toVector.map(RecordingResultWire.point)*)
    )

  def readOccupancy[U <: Unit2D: UnitLabel](
      json: Json,
      table: DocumentIdentities
  ): Either[CodecError, WindowOccupancy[U]] =
    for
      interval <- Wire.field[Json](json, "interval").flatMap(DomainWire.readInterval)
      boundary <- readBoundary(json, "boundary")
      observed <- readLong(json, "observedMicros")
      missing  <- readLong(json, "missingMicros")
      rows     <- Wire.field[Vector[Json]](json, "fixations")
      ledger   <- rows.zipWithIndex.traverse { case (row, index) =>
        (
          Wire.field[Int](row, "index"),
          readBig(row, "originalMicros"),
          readLong(row, "retainedMicros")
        ).mapN(FixationWindowTime.apply).left.map(Wire.at(s"fixations[$index]"))
      }
      frame     <- ResultWire.readFrameId(json, "frame").flatMap(table.frame[U])
      raw       <- Wire.field[Vector[Json]](json, "positions")
      positions <- raw.zipWithIndex.traverse { case (point, index) =>
        RecordingResultWire.readPoint[U](point).left.map(Wire.at(s"positions[$index]"))
      }
      value <- WindowOccupancy
        .reconstruct(
          interval,
          boundary,
          frame,
          IArray.from(positions),
          observed,
          missing,
          ledger
        )
        .left
        .map(e => CodecError.Temporal(TemporalStudyError.Occupancy(e)))
    yield value

/** Wire forms of a recording analysis: events with their sample support,
  * areas and their regions, and the derived synchronization, detection and
  * assignment members.
  */
private[codec] object RecordingResultWire:
  import ResultWire.{double, long, tagged, unknown}

  def point[U <: Unit2D](p: Pt[U]): Json =
    Json.obj("x" -> Json.fromDoubleOrNull(p.x), "y" -> Json.fromDoubleOrNull(p.y))

  def readPoint[U <: Unit2D](json: Json): Either[CodecError, Pt[U]] =
    (DomainWire.finite(json, "x"), DomainWire.finite(json, "y")).mapN(Pt[U](_, _))

  private def span(value: Interval): Vector[(String, Json)] = Vector(
    "onsetMicros"  -> long(value.onset.toMicros),
    "offsetMicros" -> long(value.offset.toMicros)
  )

  private def range(value: SampleRange): Json =
    Json.obj("from" -> Json.fromInt(value.from), "until" -> Json.fromInt(value.until))

  // ---- events ------------------------------------------------------------

  /** An event and the half-open sample range that supports it. A fixation's
    * dispersion is carried by method only: its value is re-derived from the
    * supporting samples, as a source-supported scanpath's is.
    */
  def event[U <: Unit2D](value: Event[U], support: SampleRange): Either[CodecError, Json] =
    val common = span(value.span) :+ ("support" -> range(support))
    value match
      case f: Event.Fixation[U] =>
        val dispersion: Either[CodecError, Vector[(String, Json)]] = f.dispersionStatus match
          case DispersionStatus.Unavailable(DispersionUnavailable.NotReported) =>
            Right(Vector.empty)
          case DispersionStatus.Available(spread, SummaryEvidence.SourceSupported(_, _)) =>
            Right(
              Vector(
                "dispersion" -> Json.obj(
                  "method" -> Json.fromString(StudyInputCodec.dispersionMethods(spread.method))
                )
              )
            )
          case other =>
            Left(
              CodecError.Unsupported(
                "dispersion",
                s"status $other is not the source-supported spread a detection derives"
              )
            )
        dispersion.map(extra =>
          Json.fromFields(
            (("kind" -> Json.fromString("fixation")) +: common) ++ Vector(
              "x"           -> Json.fromDoubleOrNull(f.centre.x),
              "y"           -> Json.fromDoubleOrNull(f.centre.y),
              "sampleCount" -> Json.fromInt(f.sampleCount)
            ) ++ extra
          )
        )
      case s: Event.Saccade[U] =>
        val peak: Either[CodecError, Json] = s.peakVelocityStatus match
          case PeakVelocityStatus.Available(v) => Right(Json.fromDoubleOrNull(v.value))
          case PeakVelocityStatus.Unavailable(PeakVelocityUnavailable.NotMeasured) =>
            Right(Json.Null)
          case other =>
            Left(
              CodecError.Unsupported(
                "peakVelocity",
                s"status $other belongs to a transformed saccade, not to a detection"
              )
            )
        peak.map(p =>
          Json.fromFields(
            (("kind" -> Json.fromString("saccade")) +: common) ++ Vector(
              "from"         -> point(s.from),
              "to"           -> point(s.to),
              "peakVelocity" -> p
            )
          )
        )
      case p: Event.Pursuit[U] =>
        Right(
          Json.fromFields(
            (("kind" -> Json.fromString("pursuit")) +: common) :+
              ("path" -> Json.arr(p.path.toVector.map(point)*))
          )
        )
      case _: Event.Blink[U] =>
        Right(Json.fromFields(("kind" -> Json.fromString("blink")) +: common))

  def readEvent[U <: Unit2D](
      json: Json,
      clock: ClockId
  ): Either[CodecError, (Event[U], SampleRange)] =
    def built[A](value: Either[CoreError, A]): Either[CodecError, A] =
      value.left.map(e => CodecError.Field("event", json, e.message))
    for
      onset    <- DomainWire.micros(json, "onsetMicros")
      offset   <- DomainWire.micros(json, "offsetMicros")
      interval <- Interval
        .of(clock, Instant.micros(onset), Instant.micros(offset))
        .left
        .map(e => CodecError.Field("span", json, e.message))
      rawRange <- Wire.field[Json](json, "support")
      support  <- (Wire.field[Int](rawRange, "from"), Wire.field[Int](rawRange, "until")).tupled
        .flatMap((from, until) =>
          SampleRange.of(from, until).left.map(CodecError.Support("support", _))
        )
      name  <- ResultWire.kind(json)
      event <- name match
        case "fixation" =>
          for
            centre <- readPoint[U](json)
            count  <- Wire.field[Int](json, "sampleCount")
            value  <- json.asObject.flatMap(_("dispersion")) match
              case None => built(Event.Fixation.withoutDispersion(interval, centre, count))
              case Some(spread) =>
                for
                  // A detected spread is re-derived from its supporting
                  // samples; a declared value would be ignored, so it is
                  // refused, as a source-supported scanpath refuses it.
                  _ <- Either.cond(
                    spread.asObject.exists(!_.contains("value")),
                    (),
                    CodecError.Field(
                      "value",
                      spread,
                      "a source-supported dispersion carries its method only; its value is derived"
                    )
                  )
                  method <- Wire.field[String](spread, "method")
                  parsed <- StudyInputCodec.dispersionMethods
                    .collectFirst { case (m, n) if n == method => m }
                    .toRight(
                      CodecError.Field("method", spread, s"unknown dispersion method $method")
                    )
                  // A placeholder the source samples replace when the series is rebuilt.
                  fixation <- built(Event.Fixation.of(interval, centre, 0.0, parsed, count))
                yield fixation
          yield value: Event[U]
        case "saccade" =>
          for
            from <- Wire.field[Json](json, "from").flatMap(readPoint[U])
            to   <- Wire.field[Json](json, "to").flatMap(readPoint[U])
            raw  <- Wire.field[Option[Double]](json, "peakVelocity")
            peak <- raw.traverse(v =>
              Velocity
                .perSecond[U](v)
                .left
                .map(e => CodecError.Field("peakVelocity", json, e.message))
            )
            saccade <- built(Event.Saccade.of(interval, from, to, peak))
          yield saccade: Event[U]
        case "pursuit" =>
          for
            points <- Wire.field[Vector[Json]](json, "path")
            path   <- points.zipWithIndex.traverse { case (p, i) =>
              readPoint[U](p).left.map(Wire.at(s"path[$i]"))
            }
            pursuit <- built(Event.Pursuit.of(interval, IArray.from(path)))
          yield pursuit: Event[U]
        case "blink" => built(Event.Blink.of[U](interval)).map(e => e: Event[U])
        case other   => Left(unknown(json, "event", other))
    yield event -> support

  // ---- areas ---------------------------------------------------------------

  def region[U <: Unit2D](value: Region[U]): Json = value match
    case r: Region.Rect[U] =>
      tagged(
        "rect",
        "xMin" -> Json.fromDoubleOrNull(r.lo.x),
        "yMin" -> Json.fromDoubleOrNull(r.lo.y),
        "xMax" -> Json.fromDoubleOrNull(r.hi.x),
        "yMax" -> Json.fromDoubleOrNull(r.hi.y)
      )
    case e: Region.Ellipse[U] =>
      tagged(
        "ellipse",
        "centre" -> point(e.centre),
        "rx"     -> Json.fromDoubleOrNull(e.rx),
        "ry"     -> Json.fromDoubleOrNull(e.ry)
      )
    case p: Region.Polygon[U] =>
      tagged("polygon", "vertices" -> Json.arr(p.vertices.map(point)*))
    case u: Region.Union[U]      => tagged("union", "a" -> region(u.a), "b" -> region(u.b))
    case i: Region.Intersect[U]  => tagged("intersect", "a" -> region(i.a), "b" -> region(i.b))
    case c: Region.Complement[U] => tagged("complement", "a" -> region(c.a))
    case _: Region.Empty[U]      => tagged("empty")
    case _: Region.Everything[U] => tagged("everything")

  def readRegion[U <: Unit2D](json: Json): Either[CodecError, Region[U]] =
    def geometry[A](value: Either[GeometryError, A]): Either[CodecError, A] =
      value.left.map(e => CodecError.Field("region", json, e.message))
    def operand(field: String) = Wire.field[Json](json, field).flatMap(readRegion[U])
    ResultWire.kind(json).flatMap {
      case "rect" =>
        for
          a <- DomainWire.finite(json, "xMin")
          b <- DomainWire.finite(json, "yMin")
          c <- DomainWire.finite(json, "xMax")
          d <- DomainWire.finite(json, "yMax")
          r <- geometry(Region.rect(Pt[U](a, b), Pt[U](c, d)))
        yield r
      case "ellipse" =>
        for
          centre <- Wire.field[Json](json, "centre").flatMap(readPoint[U])
          rx     <- DomainWire.finite(json, "rx")
          ry     <- DomainWire.finite(json, "ry")
          r      <- geometry(Region.ellipse(centre, rx, ry))
        yield r
      case "polygon" =>
        for
          raw      <- Wire.field[Vector[Json]](json, "vertices")
          vertices <- raw.traverse(readPoint[U])
          r        <- geometry(Region.polygon(vertices))
        yield r
      case "union"      => (operand("a"), operand("b")).mapN(_ || _)
      case "intersect"  => (operand("a"), operand("b")).mapN(_ && _)
      case "complement" => operand("a").map(a => !a)
      case "empty"      => Right(Region.empty[U])
      case "everything" => Right(Region.everything[U])
      case other        => Left(unknown(json, "region", other))
    }

  def area[U <: Unit2D](value: Aoi[U]): Json = Json.obj(
    "id"         -> Json.fromString(value.id.value),
    "label"      -> Json.fromString(value.label),
    "frame"      -> ResultWire.frameId(value.frame.id),
    "region"     -> region(value.region),
    "attributes" -> Json.arr(value.attributes.toVector.sortBy(_._1).map { (k, v) =>
      Json.obj("key" -> Json.fromString(k), "value" -> Json.fromString(v))
    }*)
  )

  def readArea[U <: Unit2D: UnitLabel](
      json: Json,
      table: DocumentIdentities
  ): Either[CodecError, Aoi[U]] =
    for
      id         <- Wire.field[String](json, "id")
      label      <- Wire.field[String](json, "label")
      frame      <- ResultWire.readFrameId(json, "frame").flatMap(table.frame[U])
      region     <- Wire.field[Json](json, "region").flatMap(readRegion[U])
      raw        <- Wire.field[Vector[Json]](json, "attributes")
      attributes <- raw.traverse(entry =>
        (Wire.field[String](entry, "key"), Wire.field[String](entry, "value")).tupled
      )
      keys = attributes.map(_._1)
      _ <- Either.cond(
        keys.distinct.size == keys.size,
        (),
        CodecError.Field("attributes", json, "attribute keys must be unique")
      )
      aoi <- Aoi
        .of(id, label, frame, region, attributes.toMap)
        .left
        .map(e => CodecError.Field("area", json, e.message))
    yield aoi

  // ---- derived members ------------------------------------------------------

  /** The synchronization evidence, refitted on decode from the plan's marks. */
  def synchronization(value: SyncEvidence): Json = Json.obj(
    "source"       -> ResultWire.clockId(value.source),
    "target"       -> ResultWire.clockId(value.target),
    "mode"         -> Json.fromString(value.mode.render),
    "offsetMicros" -> long(value.offset.toMicros),
    "drift"        -> double(value.sync.drift),
    "used"         -> Json.arr(value.usedMarks.map(m => Json.fromString(m.id))*),
    "residuals"    -> Json.arr(value.residuals.map { r =>
      Json.obj(
        "mark"            -> Json.fromString(r.mark.id),
        "predictedMicros" -> long(r.predictedTarget.toMicros),
        "errorMicros"     -> long(r.error.toMicros)
      )
    }*),
    "rejected" -> Json.arr(value.rejectedMarks.map { r =>
      Json.obj(
        "mark"           -> Json.fromString(r.mark.id),
        "residualMicros" -> long(r.residual.toMicros),
        "limitMicros"    -> long(r.limit.span.toMicros)
      )
    }*),
    "rmsMicros"         -> long(value.rootMeanSquareResidual.toMicros),
    "maximumMicros"     -> long(value.maximumAbsoluteResidual.toMicros),
    "uncertaintyMicros" -> long(value.uncertainty.toMicros)
  )

  private def detector(value: DetectorRef): Json =
    Json.obj("name" -> Json.fromString(value.name), "version" -> Json.fromString(value.version))

  def gapPolicy(value: GapPolicy): Json = value match
    case GapPolicy.Break           => tagged("break")
    case GapPolicy.Bridge(maximum) =>
      tagged("bridge", "maxMicros" -> long(maximum.span.toMicros))
    case GapPolicy.UseInterpolatedOnly => tagged("useInterpolatedOnly")

  private def maxGap(value: MaximumSupportGap): Json = value match
    case MaximumSupportGap.Unlimited => tagged("unlimited")
    case g: MaximumSupportGap.AtMost => tagged("atMost", "micros" -> long(g.span.toMicros))

  private def edge(value: EdgeSupport): Json = value match
    case EdgeSupport.Censored         => tagged("censored")
    case EdgeSupport.PreviousInterval => tagged("previousInterval")
    case EdgeSupport.MedianInterval   => tagged("medianInterval")
    case e: EdgeSupport.Fixed         => tagged("fixed", "micros" -> long(e.span.toMicros))

  def temporalSupport(value: TemporalSupport): Json = value match
    case f: TemporalSupport.Fixed => tagged("fixed", "periodMicros" -> long(f.period.toMicros))
    case TemporalSupport.Voronoi(g, e) =>
      tagged("voronoi", "maxGap" -> maxGap(g), "edge" -> edge(e))
    case TemporalSupport.ForwardHold(g, e) =>
      tagged("forwardHold", "maxGap" -> maxGap(g), "edge" -> edge(e))

  /** Wire names as an exhaustive match: a new class fails to compile here. */
  private def sampleClass(value: SampleClass): String = value match
    case SampleClass.Fixation     => "fixation"
    case SampleClass.Saccade      => "saccade"
    case SampleClass.Pursuit      => "pursuit"
    case SampleClass.Blink        => "blink"
    case SampleClass.Missing      => "missing"
    case SampleClass.OffSurface   => "offSurface"
    case SampleClass.Unclassified => "unclassified"

  private def report(value: DetectionReport): Json = Json.obj(
    "recording"            -> Json.fromString(value.recording.value),
    "detector"             -> detector(value.detector),
    "gapPolicy"            -> gapPolicy(value.gapPolicy),
    "temporalSupport"      -> temporalSupport(value.temporalSupport),
    "policyCensoredMicros" -> long(value.policyCensoredTime.toMicros),
    "totalSamples"         -> Json.fromInt(value.totalSamples),
    "classDurations"       -> Json.arr(value.classDurations.map { (c, d) =>
      Json.obj("class" -> Json.fromString(sampleClass(c)), "micros" -> long(d.toMicros))
    }*),
    "unclassifiedRanges" -> Json.arr(value.unclassifiedRanges.map(range)*),
    "bridgedGaps"        -> Json.arr(value.bridgedGaps.map(range)*),
    "warnings"           -> Json.arr(value.warnings.map {
      case DetectionWarning.UnclassifiedSupport(recording, ref, r, duration) =>
        tagged(
          "unclassifiedSupport",
          "recording"      -> Json.fromString(recording.value),
          "detector"       -> detector(ref),
          "range"          -> range(r),
          "durationMicros" -> long(duration.toMicros)
        )
    }*)
  )

  /** The detection: its identity, policies, events with support, and the
    * labels, report and provenance the events and samples derive.
    */
  def detection[U <: Unit2D](value: DetectionResult[U]): Either[CodecError, Json] =
    value.eventSeries.events
      .zip(value.eventSeries.support)
      .zipWithIndex
      .traverse { case ((e, r), index) => event(e, r).left.map(Wire.at(s"events[$index]")) }
      .map(events =>
        Json.obj(
          "detector"        -> detector(value.identity.detectorRef),
          "gapPolicy"       -> gapPolicy(value.report.gapPolicy),
          "temporalSupport" -> temporalSupport(value.report.temporalSupport),
          "events"          -> Json.arr(events*),
          "labels"          -> Json.arr(
            value.labels.toVector.map(c => Json.fromString(sampleClass(c)))*
          ),
          "report"     -> report(value.report),
          "provenance" -> ResultWire.provenance(value.provenance)
        )
      )

  private def membership(value: SampleMembership): Json = value match
    case SampleMembership.Areas(ids) =>
      tagged("areas", "ids" -> Json.arr(ids.map(id => Json.fromString(id.value))*))
    case SampleMembership.Background       => tagged("background")
    case SampleMembership.Excluded(reason) =>
      tagged(
        "excluded",
        "reason" -> Json.fromString(reason match
          case ExclusionReason.Blink      => "blink"
          case ExclusionReason.SignalLoss => "signalLoss"
          case ExclusionReason.OffSurface => "offSurface")
      )

  private def policy(value: MembershipPolicy): Json = value match
    case MembershipPolicy.Multiple              => tagged("multiple")
    case MembershipPolicy.ExclusiveByPriority   => tagged("exclusiveByPriority")
    case MembershipPolicy.RejectOverlap         => tagged("rejectOverlap")
    case MembershipPolicy.SmallestContaining(r) =>
      tagged("smallestContaining", "nx" -> Json.fromInt(r.nx), "ny" -> Json.fromInt(r.ny))

  /** The assignment: the areas, the membership policy and temporal support,
    * every sample's membership and the exact time ledger they derive.
    */
  def assignment[U <: Unit2D](value: AoiAssignment[U]): Json = Json.obj(
    "areas"           -> Json.arr(value.aoiSet.areas.map(area)*),
    "policy"          -> policy(value.policy),
    "temporalSupport" -> temporalSupport(value.support.policy),
    "memberships"     -> Json.arr(value.toVector.map(membership)*),
    "report"          -> Json.obj(
      "aoiUnionMicros"       -> long(value.report.aoiUnionTime.toMicros),
      "backgroundMicros"     -> long(value.report.backgroundTime.toMicros),
      "excludedMicros"       -> long(value.report.excludedTime.toMicros),
      "policyCensoredMicros" -> long(value.report.policyCensoredTime.toMicros),
      "duplicatedAoiMicros"  -> long(value.report.duplicatedAoiTime.toMicros)
    )
  )
