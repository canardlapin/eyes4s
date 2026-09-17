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
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Px, Deg}

final case class IvtParameters(threshold: IvtThreshold, minimumDuration: MinimumEventDuration)

/** Typed detector factory, reusable by shipped and downstream implementations. */
final class RecordingMethod[P](
    val id: DefinitionId,
    val parameters: P => Vector[(String, Provenance.Param)],
    val detector: (P, ClockId) => Either[DetectorDefinitionError, EventDetector[Deg]],
    val descriptor: Option[RecordingMethodDescriptor[P]] = None
)
object RecordingMethod:
  def ivt(id: DefinitionId): RecordingMethod[IvtParameters] = new RecordingMethod(
    id,
    p =>
      Vector(
        "thresholdDegPerSecond" -> Provenance.Param.Num(p.threshold.velocity.value),
        "minimumDurationMicros" -> Provenance.Param.Text(
          p.minimumDuration.span.toMicros.toString
        )
      ),
    (p, c) => Right(Detectors.ivt(p.threshold, p.minimumDuration, c)),
    Some(RecordingMethodDescriptor.ivt(id))
  )

final class RecordingArea private (val id: String, val label: String, val bounds: Bounds[Px])
object RecordingArea:
  def of(
      id: String,
      label: String,
      bounds: Bounds[Px]
  ): Either[RecordingPlanError, RecordingArea] =
    Either.cond(
      id.trim.nonEmpty && label.trim.nonEmpty,
      new RecordingArea(id, label, bounds),
      RecordingPlanError.InvalidArea(id, label)
    )

final class RecordingAnalysis[P] private[plan] (
    val description: Vector[(String, Vector[Provenance.Param])],
    val synchronization: SyncEvidence,
    val angular: Recording[Deg],
    val prepared: Recording[Deg],
    val detection: DetectionResult[Deg],
    val assignment: AoiAssignment[Deg]
)

/** Pure recording analysis. Import and export adapters retain their original source diagnostics. */
final class RecordingPlan[P] private (
    val input: ArtifactRef[Recording[Px]],
    val source: RecordingRef,
    val display: Frame[Px],
    val trackerClock: ClockId,
    val analysisClock: ClockId,
    val angularFrameId: FrameId,
    val viewing: Option[Viewing],
    val synchronizationModel: SyncFitMode,
    val marks: Vector[SyncMark],
    val residualLimit: Option[SyncResidualLimit],
    val interpolationGap: InterpolationGap,
    val areas: Vector[RecordingArea],
    val method: RecordingMethod[P],
    val parameters: P
):
  def inspect: Either[DescriptorError, RecipeInspection] = RecipeDescriptors.recording(this)

  /** Typed availability report; see [[Preflight.recording]]. */
  def preflight(available: Option[Recording[Px]]): RecordingReport =
    Preflight.recording(this, available)

  def description: Vector[(String, Vector[Provenance.Param])] =
    import Provenance.Param.*
    Vector(
      "input"  -> Vector(Text(input.digest)),
      "source" -> Vector(Text(source.value)),
      "frame"  -> Vector(
        Text(display.id.name),
        Num(display.bounds.xMin),
        Num(display.bounds.yMin),
        Num(display.bounds.xMax),
        Num(display.bounds.yMax),
        Text(display.yAxis.toString)
      ),
      "clocks"       -> Vector(Text(trackerClock.name), Text(analysisClock.name)),
      "angularFrame" -> Vector(Text(angularFrameId.name)),
      "viewing"      -> viewing.toVector.flatMap(v =>
        Vector(
          Num(v.perspective.distance.toMm),
          Num(v.perspective.surfaceWidth.toMm),
          Num(v.perspective.surfaceHeight.toMm)
        )
      ),
      "syncModel"           -> Vector(Text(synchronizationModel.toString)),
      "residualLimitMicros" -> residualLimit.toVector.map(l => Text(l.span.toMicros.toString)),
      "interpolationGapMicros" -> Vector(Text(interpolationGap.span.toMicros.toString)),
      "detector"               -> Vector(Text(method.id.name), Num(method.id.version.toDouble))
    ) ++
      method.parameters(parameters).sortBy(_._1).map { case (n, v) =>
        s"detector.$n" -> Vector(v)
      } ++
      marks.zipWithIndex.map { case (m, i) =>
        s"sync.$i" -> Vector(
          Text(m.id),
          Text(m.onSource.toMicros.toString),
          Text(m.onTarget.toMicros.toString)
        )
      } ++
      areas.zipWithIndex.map { case (a, i) =>
        s"area.$i" -> Vector(
          Text(a.id),
          Text(a.label),
          Num(a.bounds.xMin),
          Num(a.bounds.yMin),
          Num(a.bounds.xMax),
          Num(a.bounds.yMax)
        )
      }
  def diff(other: RecordingPlan[P]): Vector[PlanChange] =
    PlanChange.between(description, other.description)
  def prerequisites(available: Option[Recording[Px]]): Vector[RecordingPlanError] =
    val artifact = available match
      case None => Vector(RecordingPlanError.Input(PlanError.MissingArtifact(input.digest)))
      case Some(recording) if ArtifactRef.of[Recording[Px]](recording.contentHash) != input =>
        Vector(
          RecordingPlanError.Input(
            PlanError.ArtifactMismatch(input.digest, recording.contentHash.render)
          )
        )
      case Some(recording) =>
        Agreement
          .frames(display, recording.frame)
          .left
          .toOption
          .map(RecordingPlanError.Geometry.apply)
          .toVector ++
          Agreement
            .clocks(trackerClock, recording.clock)
            .left
            .toOption
            .map(RecordingPlanError.Time.apply)
            .toVector
    artifact ++ (if viewing.isEmpty then Vector(RecordingPlanError.MissingViewing(source))
                 else Vector.empty) ++
      (if marks.isEmpty then
         Vector(RecordingPlanError.MissingSynchronization(trackerClock, analysisClock))
       else Vector.empty)

  def run(recording: Recording[Px]): Either[RecordingPlanError, RecordingAnalysis[P]] =
    prerequisites(Some(recording)).headOption match
      case Some(error) => Left(error)
      case None        =>
        for
          geometry        <- viewing.toRight(RecordingPlanError.MissingViewing(source))
          synchronization <- SyncEvidence
            .fromCommonMarks(
              trackerClock,
              analysisClock,
              synchronizationModel,
              marks,
              residualLimit
            )
            .left
            .map(RecordingPlanError.Synchronization.apply)
          synchronized <- Recording
            .of(
              recording.frame,
              analysisClock,
              recording.rate,
              recording.eye,
              recording.pupilUnit,
              recording.samples.map(sample => sample.copy(t = synchronization(sample.t)))
            )
            .left
            .map(RecordingPlanError.Recording("synchronize", _))
          angularFrame <- Frame
            .angular(
              angularFrameId.name,
              geometry.horizontalExtent.toDegrees,
              geometry.verticalExtent.toDegrees
            )
            .left
            .map(RecordingPlanError.Geometry.apply)
          warp = Viewing.angularWarp(geometry, display, angularFrame)
          angular <- synchronized.warp(warp).left.map(RecordingPlanError.Core("warp", _))
          samples = Filter.interpolateGaps[Deg](interpolationGap).runAll(angular.samples)
          _ <- Either.cond(
            samples.length == angular.size,
            (),
            RecordingPlanError.Cardinality(source, angular.size, samples.length)
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
          detector <- method
            .detector(parameters, analysisClock)
            .left
            .map(RecordingPlanError.DetectorDefinition.apply)
          support = prepared.representedSupport.policy
          detection <- Detection
            .run(source, prepared, detector, GapPolicy.Break, support)
            .left
            .map(RecordingPlanError.Detection.apply)
          built <- areas.traverse { area =>
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
                  Map(
                    "nativeFrame" -> display.id.name,
                    "nativeBoundsPixels" -> s"${area.bounds.xMin},${area.bounds.yMin},${area.bounds.xMax},${area.bounds.yMax}"
                  )
                )
                .left
                .map(RecordingPlanError.Areas.apply)
            yield result
          }
          aoiSet     <- AoiSet.of(built).left.map(RecordingPlanError.Areas.apply)
          assignment <- aoiSet
            .assign(prepared, MembershipPolicy.ExclusiveByPriority, support)
            .left
            .map(RecordingPlanError.Areas.apply)
        yield new RecordingAnalysis(
          description,
          synchronization,
          angular,
          prepared,
          detection,
          assignment
        )

object RecordingPlan:
  def of[P](
      input: ArtifactRef[Recording[Px]],
      source: RecordingRef,
      display: Frame[Px],
      trackerClock: ClockId,
      analysisClock: ClockId,
      angularFrameId: FrameId,
      viewing: Option[Viewing],
      synchronizationModel: SyncFitMode,
      marks: Vector[SyncMark],
      residualLimit: Option[SyncResidualLimit],
      interpolationGap: InterpolationGap,
      areas: Vector[RecordingArea],
      method: RecordingMethod[P],
      parameters: P
  ): Either[RecordingPlanError, RecordingPlan[P]] =
    val names  = areas.map(_.id)
    val params = method.parameters(parameters)
    if source.value.trim.isEmpty then Left(RecordingPlanError.InvalidSource(source))
    else if names.isEmpty || names.distinct.size != names.size then
      Left(RecordingPlanError.AreaNames(names))
    else if params.map(_._1).distinct.size != params.size || params.exists {
        case (n, Provenance.Param.Num(v)) => n.trim.isEmpty || !v.isFinite
        case (n, _)                       => n.trim.isEmpty
      }
    then Left(RecordingPlanError.Parameters(method.id, params))
    else
      Right(
        new RecordingPlan(
          input,
          source,
          display,
          trackerClock,
          analysisClock,
          angularFrameId,
          viewing,
          synchronizationModel,
          marks,
          residualLimit,
          interpolationGap,
          areas,
          method,
          parameters
        )
      )

enum RecordingPlanError derives CanEqual:
  case Input(underlying: PlanError)
  case Geometry(underlying: GeometryError)
  case Time(underlying: TimeError)
  case Synchronization(underlying: SyncEvidenceError)
  case Core(stage: String, underlying: CoreError)
  case Recording(stage: String, underlying: RecordingError)
  case DetectorDefinition(underlying: DetectorDefinitionError)
  case Detection(underlying: DetectionResultError)
  case Areas(underlying: AoiError)
  case MissingViewing(source: RecordingRef)
  case MissingSynchronization(source: ClockId, target: ClockId)
  case InvalidSource(source: RecordingRef)
  case InvalidArea(id: String, label: String)
  case AreaNames(ids: Vector[String])
  case AreaWarp(id: String, corner: String)
  case Cardinality(source: RecordingRef, before: Int, after: Int)
  case Parameters(method: DefinitionId, values: Vector[(String, Provenance.Param)])
  def message: String = this match
    case Input(e)                     => e.message
    case Geometry(e)                  => e.message
    case Time(e)                      => e.message
    case Synchronization(e)           => e.message
    case Recording(stage, e)          => s"Recording $stage failed: ${e.message}"
    case Core(stage, e)               => s"Recording $stage failed: ${e.message}"
    case DetectorDefinition(e)        => e.message
    case Detection(e)                 => e.message
    case Areas(e)                     => e.message
    case MissingViewing(s)            => s"Recording $s requires viewing geometry."
    case MissingSynchronization(s, t) =>
      s"Recording synchronization from $s to $t requires observed common marks."
    case InvalidSource(s)       => s"Recording source must be named, got $s."
    case InvalidArea(id, label) => s"Area needs non-empty id and label, got '$id' and '$label'."
    case AreaNames(ids)         => s"Recording areas need non-empty unique ids, got $ids."
    case AreaWarp(id, c) => s"Area '$id' has an undefined $c corner under viewing geometry."
    case Cardinality(s, b, a) =>
      s"Preprocessing recording $s changed sample count from $b to $a."
    case Parameters(m, p) => s"Detector method $m needs unique named finite parameters, got $p."
