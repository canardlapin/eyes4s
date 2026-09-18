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

package eyes4s.plan

import cats.syntax.all.*
import eyes4s.aoi.*
import eyes4s.core.*
import eyes4s.design.WorkQuanta
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Deg, Px}

/** The scientific stage a recording step advanced. The sample index names the
  * first sample of a preprocessing or detection chunk.
  */
enum RecordingStage derives CanEqual:
  case Synchronizing
  case Warping
  case Interpolating(sample: Int)
  case Detecting(sample: Int)
  case Assigning

private[plan] sealed trait RecordingPhase
private[plan] object RecordingPhase:
  case object Synchronizing extends RecordingPhase
  final case class Warping(synchronization: SyncEvidence, synchronized: Recording[Px])
      extends RecordingPhase
  final case class Interpolating(
      synchronization: SyncEvidence,
      warp: Warp[Px, Deg],
      angularFrame: Frame[Deg],
      angular: Recording[Deg],
      filter: MachineCursor[Sample[Deg], Sample[Deg]]
  ) extends RecordingPhase
  final case class Detecting(
      synchronization: SyncEvidence,
      warp: Warp[Px, Deg],
      angularFrame: Frame[Deg],
      angular: Recording[Deg],
      prepared: Recording[Deg],
      detection: DetectionCursor[Deg]
  ) extends RecordingPhase
  final case class Assigning(
      synchronization: SyncEvidence,
      warp: Warp[Px, Deg],
      angularFrame: Frame[Deg],
      angular: Recording[Deg],
      prepared: Recording[Deg],
      detection: DetectionResult[Deg]
  ) extends RecordingPhase

/** An immutable position inside recording analysis.
  *
  * In order: synchronization (one whole step), the angular warp (one whole
  * step), gap interpolation in chunks of `quanta.samples` samples, event
  * detection in chunks of `quanta.samples` samples through
  * [[eyes4s.detect.DetectionCursor]], and area assignment (one whole step).
  * The interpolation and detection machines are stepped, never re-run, so
  * their flush happens exactly once, inside the step that feeds the last
  * sample; a cursor abandoned earlier has manufactured no event. Every
  * intermediate value is retained, so re-advancing the same cursor is
  * deterministic, and driving the cursor to completion is `RecordingPlan.run`.
  */
final class RecordingCursor[P] private[plan] (
    private val plan: RecordingPlan[P],
    private val recording: Recording[Px],
    private val phase: RecordingPhase
):
  import RecordingPhase.*

  /** The stage the next `advance` will work on. */
  def stage: RecordingStage = phase match
    case Synchronizing                => RecordingStage.Synchronizing
    case _: Warping                   => RecordingStage.Warping
    case Interpolating(_, _, _, _, f) => RecordingStage.Interpolating(f.consumed)
    case Detecting(_, _, _, _, _, d)  => RecordingStage.Detecting(d.consumed)
    case _: Assigning                 => RecordingStage.Assigning

  /** The recording's sample count: the exact total of each chunked stage. */
  def samples: Int = recording.size

  def advance(
      quanta: WorkQuanta
  ): Either[RecordingPlanError, WorkStep[RecordingStage, RecordingCursor[P], RecordingAnalysis[
    P
  ]]] =
    phase match
      case Synchronizing =>
        for
          _ <- plan.viewing.toRight(RecordingPlanError.MissingViewing(plan.source))
          synchronization <- SyncEvidence
            .fromCommonMarks(
              plan.trackerClock,
              plan.analysisClock,
              plan.synchronizationModel,
              plan.marks,
              plan.residualLimit
            )
            .left
            .map(RecordingPlanError.Synchronization.apply)
          synchronized <- Recording
            .of(
              recording.frame,
              plan.analysisClock,
              recording.rate,
              recording.eye,
              recording.pupilUnit,
              recording.samples.map(sample => sample.copy(t = synchronization(sample.t)))
            )
            .left
            .map(RecordingPlanError.Recording("synchronize", _))
        yield more(1, Warping(synchronization, synchronized))

      case Warping(synchronization, synchronized) =>
        for
          geometry     <- plan.viewing.toRight(RecordingPlanError.MissingViewing(plan.source))
          angularFrame <- Frame
            .angular(
              plan.angularFrameId.name,
              geometry.horizontalExtent.toDegrees,
              geometry.verticalExtent.toDegrees
            )
            .left
            .map(RecordingPlanError.Geometry.apply)
          warp = Viewing.angularWarp(geometry, plan.display, angularFrame)
          angular <- synchronized.warp(warp).left.map(RecordingPlanError.Core("warp", _))
        yield more(
          1,
          Interpolating(
            synchronization,
            warp,
            angularFrame,
            angular,
            MachineCursor.of(
              Filter.interpolateGaps[Deg](plan.interpolationGap),
              angular.samples
            )
          )
        )

      case Interpolating(synchronization, warp, angularFrame, angular, filter) =>
        filter.advance(quanta.samples.value) match
          case MachinePage.More(units, next) =>
            Right(
              more(units, Interpolating(synchronization, warp, angularFrame, angular, next))
            )
          case MachinePage.Done(units, samples) =>
            for
              _ <- Either.cond(
                samples.length == angular.size,
                (),
                RecordingPlanError.Cardinality(plan.source, angular.size, samples.length)
              )
              prepared <- Recording
                .of(
                  angular.frame,
                  angular.clock,
                  angular.rate,
                  angular.eye,
                  angular.pupilUnit,
                  IArray.from(samples)
                )
                .left
                .map(RecordingPlanError.Recording("preprocess", _))
              detector <- plan.method
                .detector(plan.parameters, plan.analysisClock)
                .left
                .map(RecordingPlanError.DetectorDefinition.apply)
            yield more(
              units,
              Detecting(
                synchronization,
                warp,
                angularFrame,
                angular,
                prepared,
                Detection.stepped(
                  plan.source,
                  prepared,
                  detector,
                  GapPolicy.Break,
                  prepared.representedSupport.policy
                )
              )
            )

      case Detecting(synchronization, warp, angularFrame, angular, prepared, detection) =>
        detection.advance(quanta.samples.value) match
          case DetectionPage.More(units, next) =>
            Right(
              more(
                units,
                Detecting(synchronization, warp, angularFrame, angular, prepared, next)
              )
            )
          case DetectionPage.Done(_, Left(error)) => Left(RecordingPlanError.Detection(error))
          case DetectionPage.Done(units, Right(result)) =>
            Right(
              more(
                units,
                Assigning(synchronization, warp, angularFrame, angular, prepared, result)
              )
            )

      case Assigning(synchronization, warp, angularFrame, angular, prepared, detection) =>
        for
          built <- plan.areas.traverse { area =>
            for
              a <- warp(Pt[Px](area.bounds.xMin, area.bounds.yMin))
                .toRight(RecordingPlanError.AreaWarp(area.id, "minimum"))
              b <- warp(Pt[Px](area.bounds.xMax, area.bounds.yMax))
                .toRight(RecordingPlanError.AreaWarp(area.id, "maximum"))
              region <- Region
                .rect(
                  Pt[Deg](math.min(a.x, b.x), math.min(a.y, b.y)),
                  Pt[Deg](math.max(a.x, b.x), math.max(a.y, b.y))
                )
                .left
                .map(RecordingPlanError.Geometry.apply)
              result <- Aoi
                .of(
                  area.id,
                  area.label,
                  angularFrame,
                  region,
                  RecordingArea.attributes(plan.display, area)
                )
                .left
                .map(RecordingPlanError.Areas.apply)
            yield result
          }
          aoiSet     <- AoiSet.of(built).left.map(RecordingPlanError.Areas.apply)
          assignment <- aoiSet
            .assign(
              prepared,
              MembershipPolicy.ExclusiveByPriority,
              prepared.representedSupport.policy
            )
            .left
            .map(RecordingPlanError.Areas.apply)
        yield WorkStep.Done(
          1,
          new RecordingAnalysis(
            plan,
            synchronization,
            angular,
            prepared,
            detection,
            assignment
          )
        )

  private def more(
      units: Int,
      next: RecordingPhase
  ): WorkStep[RecordingStage, RecordingCursor[P], RecordingAnalysis[P]] =
    WorkStep.More(stage, units, new RecordingCursor(plan, recording, next))

object RecordingWork:

  /** Drive a recording cursor to completion with fixed quanta. */
  def complete[P](
      cursor: RecordingCursor[P],
      quanta: WorkQuanta = WorkQuanta.default
  ): Either[RecordingPlanError, RecordingAnalysis[P]] =
    Stepwise.complete(cursor, quanta)

  private[plan] def begin[P](
      plan: RecordingPlan[P],
      recording: Recording[Px]
  ): Either[RecordingPlanError, RecordingCursor[P]] =
    plan.prerequisites(Some(recording)).headOption match
      case Some(error) => Left(error)
      case None => Right(new RecordingCursor(plan, recording, RecordingPhase.Synchronizing))
