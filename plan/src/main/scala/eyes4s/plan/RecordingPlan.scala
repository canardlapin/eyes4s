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

import eyes4s.aoi.*
import eyes4s.core.*
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Px, Deg}

final case class IvtParameters(threshold: IvtThreshold, minimumDuration: MinimumEventDuration)

/** Per-axis angular extent, not a radial dispersion threshold. */
final case class IdtParameters(extent: Extent[Deg], minimumDuration: MinimumEventDuration)

/** Supplied or previously estimated thresholds. Estimation remains a separate
  * whole-trial/calibration operation via EkThresholds.estimate, never a hidden
  * change of parameters when replaying a recording plan.
  */
final case class EkParameters(thresholds: EkThresholds, minimumSamples: EkMinimumSamples)

/** Typed detector factory, reusable by shipped and downstream implementations. */
final class RecordingMethod[P](
    val id: DefinitionId,
    val parameters: P => Vector[(String, Provenance.Param)],
    val detector: (P, ClockId) => Either[DetectorDefinitionError, EventDetector[Deg]],
    val descriptor: Option[RecordingMethodDescriptor[P]] = None
)
object RecordingMethod:
  def idt(id: DefinitionId): RecordingMethod[IdtParameters] =
    val descriptor = RecordingMethodDescriptor.idt(id)
    new RecordingMethod(
      id,
      descriptor.parameters.values,
      (p, c) => Right(Detectors.idt(p.extent, p.minimumDuration, c)),
      Some(descriptor)
    )

  /** Canonical five-point Engbert-Kliegl detection with fixed per-axis thresholds.
    * Irregular sampling remains a typed detection failure; no resampling occurs.
    */
  def engbertKliegl(id: DefinitionId): RecordingMethod[EkParameters] =
    val descriptor = RecordingMethodDescriptor.engbertKliegl(id)
    new RecordingMethod(
      id,
      descriptor.parameters.values,
      (p, c) => Right(Detectors.engbertKliegl(p.thresholds, p.minimumSamples, c)),
      Some(descriptor)
    )

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
  /** The attributes a run records on the area's angular AOI: the native
    * display frame and the pixel bounds, rendered canonically so the same
    * area has the same attributes on the JVM and Scala.js.
    */
  def attributes(display: Frame[Px], area: RecordingArea): Map[String, String] =
    Map(
      "nativeFrame"        -> display.id.name,
      "nativeBoundsPixels" -> Vector(
        area.bounds.xMin,
        area.bounds.yMin,
        area.bounds.xMax,
        area.bounds.yMax
      ).map(v => Provenance.Param.Num(v).render).mkString(",")
    )

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

/** A completed recording analysis: the plan it ran, the synchronization
  * evidence, the angular and prepared recordings, the detection and the area
  * assignment. Only a completed run or [[RecordingAnalysis.reconstruct]]
  * builds one.
  */
final class RecordingAnalysis[P] private[plan] (
    val plan: RecordingPlan[P],
    val synchronization: SyncEvidence,
    val angular: Recording[Deg],
    val prepared: Recording[Deg],
    val detection: DetectionResult[Deg],
    val assignment: AoiAssignment[Deg]
):
  def input: ArtifactRef[Recording[Px]]                       = plan.input
  def description: Vector[(String, Vector[Provenance.Param])] = plan.description

object RecordingAnalysis:
  /** Checked reconstruction of a completed recording analysis from its plan
    * and its archived recordings, events and areas, without re-running
    * synchronization, the angular warp, interpolation or detection.
    *
    * Re-derived from the plan: the description and input identity, the
    * synchronization evidence (refitted from the plan's marks exactly as the
    * run fits them), the detector's identity and configuration, the gap
    * policy and the temporal support. Checked: the angular recording is on
    * the plan's angular frame and analysis clock; the prepared recording is a
    * re-derived preparation of it (below); the detection is `Detection.reconstruct` of the
    * archived events and support over the prepared recording, which
    * re-derives fixation summaries, labels, the report and the provenance;
    * the areas are the plan's, in order, with the attributes the run records
    * (native frame and pixel bounds); and the assignment is re-derived by
    * assigning the prepared recording to them under exclusive priority, an
    * exact point-in-region test. The prepared recording itself is re-derived
    * by re-running the plan's gap interpolation over the angular samples, a
    * linear fill in basic IEEE arithmetic, and must equal the archived one
    * sample for sample. Not re-derived: the angular samples and the angular
    * frame (a trigonometric warp of the input) and each area's warped region.
    */
  def reconstruct[P](
      plan: RecordingPlan[P],
      angular: Recording[Deg],
      prepared: Recording[Deg],
      events: Vector[Event[Deg]],
      support: Vector[SampleRange],
      areas: Vector[Aoi[Deg]]
  ): Either[RecordingResultError, RecordingAnalysis[P]] =
    import RecordingResultError.*
    def stage(name: String, field: String, expected: String, found: String) =
      Option.when(expected != found)(Stage(name, field, expected, found))
    def fromPlan[A](value: Either[RecordingPlanError, A]): Either[RecordingResultError, A] =
      value.left.map(Plan(_))
    val angularChecks = stage(
      "angular",
      "frame",
      plan.angularFrameId.name,
      angular.frame.id.name
    ).orElse(stage("angular", "clock", plan.analysisClock.name, angular.clock.name))
    // The prepared recording is re-derived: gap interpolation is a linear
    // fill in basic IEEE arithmetic, exact and identical on every platform.
    def preparation: Either[RecordingResultError, Recording[Deg]] =
      val filled =
        Filter.interpolateGaps[Deg](plan.interpolationGap).runAll(angular.samples.toVector)
      for
        _ <- Either.cond(
          filled.length == angular.size,
          (),
          Plan(RecordingPlanError.Cardinality(plan.source, angular.size, filled.length))
        )
        derived <- fromPlan(
          Recording
            .of(
              angular.frame,
              angular.clock,
              angular.rate,
              angular.eye,
              angular.pupilUnit,
              IArray.from(filled)
            )
            .left
            .map(RecordingPlanError.Recording("preprocess", _))
        )
        _ <- stage(
          "prepared",
          "frame",
          RecordingResultError.renderFrame(derived.frame),
          RecordingResultError.renderFrame(prepared.frame)
        )
          .orElse(stage("prepared", "clock", derived.clock.name, prepared.clock.name))
          .orElse(
            stage(
              "prepared",
              "rate",
              RecordingResultError.renderRate(derived.rate),
              RecordingResultError.renderRate(prepared.rate)
            )
          )
          .orElse(stage("prepared", "eye", derived.eye.toString, prepared.eye.toString))
          .orElse(
            stage("prepared", "pupilUnit", s"${derived.pupilUnit}", s"${prepared.pupilUnit}")
          )
          .orElse(
            stage(
              "prepared",
              "samplingToleranceMicros",
              derived.samplingTolerance.toSpan.toMicros.toString,
              prepared.samplingTolerance.toSpan.toMicros.toString
            )
          )
          .orElse(stage("prepared", "samples", derived.size.toString, prepared.size.toString))
          .orElse((0 until derived.size).view.flatMap { index =>
            val (d, p) = (derived.samples(index), prepared.samples(index))
            Option.when(d != p)(
              Stage(
                "prepared",
                s"samples[$index]",
                RecordingResultError.renderSample(d),
                RecordingResultError.renderSample(p)
              )
            )
          }.headOption)
          .toLeft(())
      yield derived
    val expectedAreas = plan.areas.map(a => (a.id.trim, a.label.trim))
    val foundAreas    = areas.map(a => (a.id.value, a.label))
    val areaChecks    = Option
      .when(expectedAreas != foundAreas)(
        Stage(
          "assignment",
          "areas",
          RecordingResultError.renderAreas(expectedAreas),
          RecordingResultError.renderAreas(foundAreas)
        )
      )
      .orElse(
        plan.areas
          .zip(areas)
          .zipWithIndex
          .view
          .flatMap { case ((declared, area), index) =>
            val expected = RecordingArea.attributes(plan.display, declared)
            Option.when(area.attributes != expected)(
              Stage(
                "assignment",
                s"areas[$index].attributes",
                RecordingResultError.renderAttributes(expected),
                RecordingResultError.renderAttributes(area.attributes)
              )
            )
          }
          .headOption
      )
    for
      _ <- fromPlan(plan.viewing.toRight(RecordingPlanError.MissingViewing(plan.source)))
      synchronization <- fromPlan(
        SyncEvidence
          .fromCommonMarks(
            plan.trackerClock,
            plan.analysisClock,
            plan.synchronizationModel,
            plan.marks,
            plan.residualLimit
          )
          .left
          .map(RecordingPlanError.Synchronization.apply)
      )
      _        <- angularChecks.toLeft(())
      _        <- preparation
      detector <- fromPlan(
        plan.method
          .detector(plan.parameters, plan.analysisClock)
          .left
          .map(RecordingPlanError.DetectorDefinition.apply)
      )
      detection <- fromPlan(
        Detection
          .reconstruct(
            plan.source,
            prepared,
            DetectorIdentity.Algorithm(detector.card),
            GapPolicy.Break,
            prepared.representedSupport.policy,
            events,
            support,
            detector.configuration
          )
          .left
          .map(RecordingPlanError.Detection.apply)
      )
      _          <- areaChecks.toLeft(())
      aoiSet     <- fromPlan(AoiSet.of(areas).left.map(RecordingPlanError.Areas.apply))
      assignment <- fromPlan(
        aoiSet
          .assign(
            prepared,
            MembershipPolicy.ExclusiveByPriority,
            prepared.representedSupport.policy
          )
          .left
          .map(RecordingPlanError.Areas.apply)
      )
    yield new RecordingAnalysis(
      plan,
      synchronization,
      angular,
      prepared,
      detection,
      assignment
    )

/** Refusals while rebuilding a completed recording analysis from its plan and
  * archived parts: a stage of the plan that cannot be re-derived, or a stage
  * whose archived value is not the one the plan and the earlier stages give.
  */
enum RecordingResultError derives CanEqual:
  case Plan(underlying: RecordingPlanError)
  case Stage(stage: String, field: String, expected: String, found: String)

  def message: String = this match
    case Plan(underlying)                     => underlying.message
    case Stage(stage, field, expected, found) =>
      s"The archived $stage stage has $field $found where the plan and earlier stages give $expected."

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

  /** Execute this plan on the supplied recording; the bounded cursor driven to completion. */
  def run(recording: Recording[Px]): Either[RecordingPlanError, RecordingAnalysis[P]] =
    work(recording).flatMap(RecordingWork.complete(_))

  /** Resumable execution: preprocessing and detection step their machines in
    * chunks of `quanta.samples`; see [[RecordingCursor]]. Prerequisites are
    * checked here, before any step.
    */
  def work(recording: Recording[Px]): Either[RecordingPlanError, RecordingCursor[P]] =
    RecordingWork.begin(this, recording)

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

object RecordingResultError:
  /** Canonical renderings for `Stage` operands: numbers through
    * `Provenance.Param.Num.render`, so a refusal reads the same on the JVM
    * and Scala.js.
    */
  private def number(value: Double): String = Provenance.Param.Num(value).render

  private[plan] def renderFrame[U <: Unit2D](value: Frame[U]): String =
    val spec = value.spec
    s"${value.id.name}[${Vector(spec.xMin, spec.yMin, spec.xMax, spec.yMax).map(number).mkString(",")}]" +
      s"${value.yAxis}"

  private[plan] def renderSample[U <: Unit2D](value: Sample[U]): String =
    val gaze = value.gaze match
      case Gaze.Tracked(p, pupil) =>
        s"tracked(${number(p.x)},${number(p.y)}${pupil.fold("")(v => s",pupil=${number(v)}")})"
      case Gaze.OffScreen(p) => s"offScreen(${number(p.x)},${number(p.y)})"
      case Gaze.Blink()      => "blink"
      case Gaze.Lost()       => "lost"
    s"${value.t.toMicros}:$gaze:${value.lineage.render}"

  private[plan] def renderRate(value: Rate): String = value match
    case Rate.Fixed(hz) => s"fixed:${number(hz.value)}Hz"
    case Rate.Irregular => "irregular"

  private[plan] def renderAreas(values: Vector[(String, String)]): String =
    values.map((id, label) => s"[$id|$label]").mkString

  private[plan] def renderAttributes(values: Map[String, String]): String =
    values.toVector.sorted.map((k, v) => s"$k=$v").mkString(";")
