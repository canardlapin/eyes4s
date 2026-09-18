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

package eyes4s.io

import eyes4s.aoi.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.plan.*
import io.circe.Json

/** An exact, canonical rendering of a scientific value for comparison
  * across processes, independent of any codec: every double becomes the
  * sixteen hexadecimal digits of its raw IEEE 754 bits, every 64-bit
  * integer (including instants, spans and content hashes) a decimal string,
  * and every structure is written field by field in declaration order. Two
  * values have equal fingerprints exactly when every field and every
  * double's bits agree, so `-0.0` and `+0.0` differ and nothing is rounded.
  *
  * Result classes without structural equality are listed explicitly; any
  * other class that is not a case class, an enum case or a collection is
  * refused, so a new result field of an unknown class cannot be skipped.
  */
object ScientificFingerprint:
  /** The fingerprint and the number of doubles whose bits it carries. */
  final case class Rendered(json: Json, doubles: Int):
    def bytes: Array[Byte] = json.noSpaces.getBytes("UTF-8")

  def of(value: Any): Rendered =
    val walker = new Walker
    val json   = walker(value)
    Rendered(json, walker.doubles)

  private final class Walker:
    var doubles = 0

    private def fields(kind: String, values: (String, Any)*): Json =
      Json.fromFields(("type" -> Json.fromString(kind)) +: values.map((k, v) => k -> apply(v)))

    def apply(value: Any): Json = value match
      case d: Double =>
        doubles += 1
        Json.fromString(f"f64:${java.lang.Double.doubleToRawLongBits(d)}%016x")
      case f: Float   => Json.fromString(f"f32:${java.lang.Float.floatToRawIntBits(f)}%08x")
      case l: Long    => Json.fromString(s"i64:$l")
      case i: Int     => Json.fromInt(i)
      case s: Short   => Json.fromInt(s.toInt)
      case b: Byte    => Json.fromInt(b.toInt)
      case b: Boolean => Json.fromBoolean(b)
      case c: Char    => Json.fromString(s"char:$c")
      case s: String  => Json.fromString(s"str:$s")
      case b: BigInt  => Json.fromString(s"bigint:$b")
      case ()         => Json.fromString("unit")
      case None       => Json.Null
      case Some(x)    => Json.obj("some" -> apply(x))
      case Left(x)    => Json.obj("left" -> apply(x))
      case Right(x)   => Json.obj("right" -> apply(x))
      case m: collection.Map[?, ?] =>
        Json.arr(
          m.toVector
            .map((k, v) => (apply(k), apply(v)))
            .sortBy(_._1.noSpaces)
            .map((k, v) => Json.obj("key" -> k, "value" -> v))*
        )
      case s: collection.Set[?] => Json.arr(s.toVector.map(apply).sortBy(_.noSpaces)*)
      case i: Iterable[?]       => Json.arr(i.iterator.map(apply).toVector*)
      case a: Array[?]          => Json.arr(a.iterator.map(apply).toVector*)

      // Study results.
      case r: StudyResult[?, ?, ?, ?] =>
        fields(
          "StudyResult",
          "input"       -> r.input,
          "description" -> r.description,
          "scales"      -> r.scales
        )
      case s: StudyScaleResult[?, ?, ?, ?] =>
        fields(
          "StudyScaleResult",
          "estimate"       -> s.estimate,
          "estimation"     -> s.estimation,
          "excludedPhases" -> s.excludedPhases,
          "analyses"       -> s.analyses,
          "contrast"       -> s.contrast
        )
      case a: StudyAnalyses[?, ?] =>
        fields(
          "StudyAnalyses",
          "matchedSource" -> a.matchedSource,
          "matched"       -> a.matched,
          "controlSource" -> a.controlSource,
          "control"       -> a.control
        )
      case a: Analysis[?, ?] =>
        fields(
          "Analysis",
          "entries"     -> a.entries,
          "diagnostics" -> a.diagnostics,
          "provenance"  -> a.provenance,
          "evaluation"  -> a.evaluation
        )
      case c: Contrast[?, ?, ?] =>
        fields("Contrast", "matched" -> c.matched, "control" -> c.control, "rows" -> c.rows)
      case r: ContrastRow[?, ?, ?] =>
        fields(
          "ContrastRow",
          "key"        -> r.key,
          "matched"    -> r.matched,
          "control"    -> r.control,
          "difference" -> r.difference
        )
      case s: EvaluationSpec =>
        fields(
          "EvaluationSpec",
          "method"     -> s.method,
          "revision"   -> s.revision,
          "parameters" -> s.parameters,
          "components" -> s.components,
          "geometry"   -> s.geometry,
          "time"       -> s.time
        )
      case g: EvaluationGeometry =>
        fields("EvaluationGeometry", "unit" -> g.unit, "frame" -> g.frame, "grid" -> g.grid)
      case m: Mass[?] =>
        fields("Mass", "grid" -> m.grid, "values" -> m.values, "provenance" -> m.provenance)
      case c: ObservedCoverage =>
        fields("ObservedCoverage", "clock" -> c.clock, "intervals" -> c.intervals)
      case c: AlgorithmCard =>
        // Static metadata of the shipped detector: its identity and version.
        fields("AlgorithmCard", "detector" -> c.detectorRef, "name" -> c.name)
      case p: StudyPlan[?, ?, ?, ?, ?] =>
        fields("StudyPlan", "input" -> p.input, "description" -> p.description)

      // Temporal results.
      case r: TemporalStudyResult[?, ?, ?, ?, ?] =>
        fields("TemporalStudyResult", "description" -> r.description, "cells" -> r.cells)
      case c: TemporalCell[?, ?, ?, ?, ?] =>
        fields(
          "TemporalCell",
          "repetition" -> c.repetition,
          "window"     -> c.window,
          "study"      -> c.study,
          "occupancy"  -> c.occupancy,
          "result"     -> c.result
        )
      case r: RepetitionContrast =>
        fields(
          "RepetitionContrast",
          "name"      -> r.name,
          "focal"     -> r.focalPhase,
          "reference" -> r.referencePhase
        )
      case w: StudyWindow => fields("StudyWindow", "name" -> w.name, "window" -> w.window)
      case o: WindowOccupancy[?] =>
        fields(
          "WindowOccupancy",
          "interval"       -> o.interval,
          "boundary"       -> o.boundary,
          "measure"        -> o.measure,
          "observedMicros" -> o.observedMicros,
          "missingMicros"  -> o.missingMicros,
          "fixationTimes"  -> o.fixationTimes
        )
      case p: PointMeasure[?] =>
        fields(
          "PointMeasure",
          "frame"      -> p.frame,
          "positions"  -> p.positions,
          "weights"    -> p.weights,
          "provenance" -> p.provenance
        )

      // Recording results.
      case a: RecordingAnalysis[?] =>
        fields(
          "RecordingAnalysis",
          "description"     -> a.description,
          "synchronization" -> a.synchronization,
          "angular"         -> a.angular,
          "prepared"        -> a.prepared,
          "detection"       -> a.detection,
          "assignment"      -> a.assignment
        )
      case e: SyncEvidence =>
        fields(
          "SyncEvidence",
          "mode"          -> e.mode,
          "sync"          -> e.sync,
          "usedMarks"     -> e.usedMarks,
          "residuals"     -> e.residuals,
          "rejectedMarks" -> e.rejectedMarks,
          "rms"           -> e.rootMeanSquareResidual,
          "maximum"       -> e.maximumAbsoluteResidual,
          "uncertainty"   -> e.uncertainty
        )
      case m: SyncMark =>
        fields("SyncMark", "id" -> m.id, "source" -> m.onSource, "target" -> m.onTarget)
      case r: Recording[?] =>
        fields(
          "Recording",
          "frame"             -> r.frame,
          "clock"             -> r.clock,
          "rate"              -> r.rate,
          "eye"               -> r.eye,
          "pupilUnit"         -> r.pupilUnit,
          "samplingTolerance" -> r.samplingTolerance,
          "samplingEvidence"  -> r.samplingEvidence,
          "samples"           -> r.samples,
          "contentHash"       -> r.contentHash
        )
      case l: SampleLineage      => fields("SampleLineage", "steps" -> l.render)
      case d: DetectionResult[?] =>
        fields(
          "DetectionResult",
          "identity"   -> d.identity,
          "labels"     -> d.labels,
          "events"     -> d.eventSeries,
          "report"     -> d.report,
          "provenance" -> d.provenance
        )
      case l: SampleLabels   => fields("SampleLabels", "labels" -> l.toVector)
      case s: EventSeries[?] =>
        fields(
          "EventSeries",
          "recording" -> s.recording,
          "source"    -> s.source,
          "events"    -> s.events,
          "support"   -> s.support,
          "lineage"   -> s.lineage
        )
      case r: SampleRange      => fields("SampleRange", "from" -> r.from, "until" -> r.until)
      case d: Dispersion[?]    => fields("Dispersion", "value" -> d.value, "method" -> d.method)
      case a: AoiAssignment[?] =>
        fields(
          "AoiAssignment",
          "areas"       -> a.aoiSet,
          "recording"   -> a.recording,
          "policy"      -> a.policy,
          "support"     -> a.support,
          "memberships" -> a.toVector,
          "report"      -> a.report
        )
      case s: AoiSet[?] => fields("AoiSet", "frame" -> s.frame, "areas" -> s.areas)
      case a: Aoi[?]    =>
        fields(
          "Aoi",
          "id"         -> a.id,
          "label"      -> a.label,
          "frame"      -> a.frame,
          "region"     -> a.region,
          "attributes" -> a.attributes
        )
      case s: SampleSupportLedger =>
        fields(
          "SampleSupportLedger",
          "policy"    -> s.policy,
          "durations" -> s.toVector,
          "censored"  -> s.censoredTime
        )

      case p: Product =>
        Json.fromFields(
          ("type" -> Json.fromString(p.productPrefix)) +:
            p.productElementNames.zip(p.productIterator).map((k, v) => k -> apply(v)).toVector
        )
      case other =>
        throw new IllegalArgumentException(
          s"no exact fingerprint for ${other.getClass.getName}; list it in ScientificFingerprint"
        )
