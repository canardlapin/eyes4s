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

package eyes4s.laws

import eyes4s.aoi.*
import eyes4s.core.*
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.plan.*

/** Complete structural identity of two recording analyses, for recording
  * result codec laws: the same plan description, synchronization evidence,
  * angular and prepared recordings (every sample), detection (identity,
  * labels, every event with its sample support, report and provenance) and
  * assignment (areas, policy, per-sample support, every membership and the
  * time ledger). Result classes keep reference equality, so a law needs this
  * explicit notion of "the same analysis".
  */
object RecordingResultEquivalence:
  def same[P](a: RecordingAnalysis[P], b: RecordingAnalysis[P]): Boolean =
    a.description == b.description && a.input == b.input &&
      sameSynchronization(a.synchronization, b.synchronization) &&
      sameRecording(a.angular, b.angular) && sameRecording(a.prepared, b.prepared) &&
      sameDetection(a.detection, b.detection) && sameAssignment(a.assignment, b.assignment)

  def sameSynchronization(a: SyncEvidence, b: SyncEvidence): Boolean =
    a.mode == b.mode && a.sync == b.sync && a.usedMarks == b.usedMarks &&
      a.residuals == b.residuals && a.rejectedMarks == b.rejectedMarks &&
      a.rootMeanSquareResidual.toMicros == b.rootMeanSquareResidual.toMicros &&
      a.maximumAbsoluteResidual.toMicros == b.maximumAbsoluteResidual.toMicros &&
      a.uncertainty.toMicros == b.uncertainty.toMicros

  def sameRecording[U <: Unit2D](a: Recording[U], b: Recording[U]): Boolean =
    a.frame == b.frame && a.clock == b.clock && a.rate == b.rate && a.eye == b.eye &&
      a.pupilUnit == b.pupilUnit &&
      a.samplingTolerance.toSpan.toMicros == b.samplingTolerance.toSpan.toMicros &&
      a.samples.toVector == b.samples.toVector && a.contentHash == b.contentHash

  /** Events compare structurally; a pursuit's path is compared point by point. */
  def sameEvent[U <: Unit2D](a: Event[U], b: Event[U]): Boolean = (a, b) match
    case (x: Event.Pursuit[U], y: Event.Pursuit[U]) =>
      x.span == y.span && x.path.toVector == y.path.toVector
    case _ => a == b

  def sameDetection[U <: Unit2D](a: DetectionResult[U], b: DetectionResult[U]): Boolean =
    a.identity == b.identity && a.labels.toVector == b.labels.toVector &&
      a.eventSeries.events.size == b.eventSeries.events.size &&
      a.eventSeries.events.zip(b.eventSeries.events).forall(sameEvent) &&
      a.eventSeries.support == b.eventSeries.support &&
      a.eventSeries.source == b.eventSeries.source &&
      sameRecording(a.eventSeries.recording, b.eventSeries.recording) &&
      a.report == b.report && a.provenance == b.provenance

  def sameAssignment[U <: Unit2D](a: AoiAssignment[U], b: AoiAssignment[U]): Boolean =
    a.aoiSet.frame == b.aoiSet.frame && a.aoiSet.areas == b.aoiSet.areas &&
      a.policy == b.policy && a.support.policy == b.support.policy &&
      a.support.toVector == b.support.toVector &&
      a.support.censoredTime == b.support.censoredTime && a.toVector == b.toVector &&
      a.report == b.report && sameRecording(a.recording, b.recording)

/** Complete structural identity of two temporal study results, for temporal
  * result codec laws: the same plan description and input, and per cell the
  * same repetition, window and repetition plan, every trial's occupancy (its
  * interval, boundary, observed and missing time, ledger and measure) or
  * typed failure, and the same study result under
  * [[StudyResultEquivalence]].
  */
object TemporalResultEquivalence:
  def same[K, U <: Unit2D, P, S, D](
      a: TemporalStudyResult[K, U, P, S, D],
      b: TemporalStudyResult[K, U, P, S, D]
  )(scores: (S, S) => Boolean, differences: (D, D) => Boolean): Boolean =
    a.description == b.description && a.input == b.input && a.cells.size == b.cells.size &&
      a.cells.zip(b.cells).forall { (x, y) =>
        x.repetition.name == y.repetition.name &&
        x.repetition.focalPhase == y.repetition.focalPhase &&
        x.repetition.referencePhase == y.repetition.referencePhase &&
        x.window.name == y.window.name && x.window.window == y.window.window &&
        x.study.description == y.study.description &&
        x.occupancy.size == y.occupancy.size &&
        x.occupancy.zip(y.occupancy).forall {
          case ((k, Right(p)), (l, Right(q))) => k == l && sameOccupancy(p, q)
          case ((k, Left(p)), (l, Left(q)))   => k == l && p == q
          case _                              => false
        } &&
        StudyResultEquivalence.same(x.result, y.result)(scores, differences)
      }

  def sameOccupancy[U <: Unit2D](a: WindowOccupancy[U], b: WindowOccupancy[U]): Boolean =
    a.interval == b.interval && a.boundary == b.boundary &&
      a.observedMicros == b.observedMicros && a.missingMicros == b.missingMicros &&
      a.fixationTimes == b.fixationTimes && a.measure.frame == b.measure.frame &&
      a.measure.positions.toVector == b.measure.positions.toVector &&
      a.measure.weights.toVector == b.measure.weights.toVector &&
      a.measure.provenance == b.measure.provenance
