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

import eyes4s.core.*
import eyes4s.design.*
import eyes4s.detect.DetectorDefinitionError
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import scala.annotation.tailrec

/** The shipped recipe families a consumer can list and preflight. */
enum RecipeFamily derives CanEqual:
  case FixationStudy, EventRecording, TemporalStudy

/** A blocker means the recipe will be refused as a whole; a warning means it
  * will execute but the named object has a deterministic failure or a reduced
  * explanation. Neither certifies numerical success.
  */
enum Severity derives CanEqual:
  case Blocker, Warning

/** Why a finding arises: the input is absent, present but incompatible with the
  * plan, the plan's own settings are unsound, or the observed data limit the
  * design without any setting being wrong.
  */
enum FindingClass derives CanEqual:
  case UnavailableInput, IncompatibleInput, InvalidSetting, DataDependent

/** A scientifically meaningful next step. Wording belongs to the consumer. */
enum Remedy derives CanEqual:
  case SupplyReferencedArtifact, RetargetPlanToAvailableInput, RegisterMethodDescriptor,
    ReconcileMethodDescriptor, RaiseBudgetOrReduceStudy, AlignFrame, AlignClock,
    ResolveDuplicateTrials, SupplyMatchedReference, SupplyControlReference,
    SupplyViewingGeometry, SupplyCommonMarks, ReviseSynchronizationMarks, ReviseViewingOrArea,
    ReviseDetectorParameters, SupplyEpoch, ReviseWindow, AcceptMissingObservation

/** Outcomes that only execution decides. Preflight lists them instead of guessing. */
enum UncheckedAspect derives CanEqual:
  case OccupancyEstimation, PairComparison, FailurePolicyReduction, WindowOccupancy,
    SampleSynchronization, GapInterpolation, EventDetection, AreaAssignment

enum Availability derives CanEqual:
  case Ready, Unavailable

enum PairingSide derives CanEqual:
  case Focal, Reference

enum AreaCorner derives CanEqual:
  case Minimum, Maximum

sealed trait PreflightFinding:
  def severity: Severity
  def category: FindingClass
  def remedy: Remedy
  def message: String

/** Findings for the within-participant matched/control fixation study. Trial
  * keys stay typed; duplicate positions index the focal or reference operand.
  */
enum StudyFinding[K, U <: Unit2D] extends PreflightFinding derives CanEqual:
  case UndescribedMethod(method: DefinitionId)
  case InconsistentDescriptor(method: DefinitionId, underlying: DescriptorError)
  case MissingArtifact(expected: ArtifactRef[StudyInput[K, U]])
  case ArtifactMismatch(
      expected: ArtifactRef[StudyInput[K, U]],
      actual: ArtifactRef[StudyInput[K, U]]
  )
  case OverBudget(underlying: PlanError)
  case FrameMismatch(key: K, underlying: GeometryError)
  case DuplicateTrial(key: K, side: PairingSide, positions: Vector[Int])
  case UnmatchedFocal(key: K)
  case UncontrolledFocal(key: K)

  def keys: Vector[K] = this match
    case FrameMismatch(k, _)     => Vector(k)
    case DuplicateTrial(k, _, _) => Vector(k)
    case UnmatchedFocal(k)       => Vector(k)
    case UncontrolledFocal(k)    => Vector(k)
    case _                       => Vector.empty

  def severity: Severity = this match
    case InconsistentDescriptor(_, _) | MissingArtifact(_) | ArtifactMismatch(_, _) |
        OverBudget(_) =>
      Severity.Blocker
    case _ => Severity.Warning

  def category: FindingClass = this match
    case UndescribedMethod(_) | InconsistentDescriptor(_, _) | OverBudget(_) =>
      FindingClass.InvalidSetting
    case MissingArtifact(_) | ArtifactMismatch(_, _) => FindingClass.UnavailableInput
    case FrameMismatch(_, _)                         => FindingClass.IncompatibleInput
    case DuplicateTrial(_, _, _) | UnmatchedFocal(_) | UncontrolledFocal(_) =>
      FindingClass.DataDependent

  def remedy: Remedy = this match
    case UndescribedMethod(_)         => Remedy.RegisterMethodDescriptor
    case InconsistentDescriptor(_, _) => Remedy.ReconcileMethodDescriptor
    case MissingArtifact(_)           => Remedy.SupplyReferencedArtifact
    case ArtifactMismatch(_, _)       => Remedy.RetargetPlanToAvailableInput
    case OverBudget(_)                => Remedy.RaiseBudgetOrReduceStudy
    case FrameMismatch(_, _)          => Remedy.AlignFrame
    case DuplicateTrial(_, _, _)      => Remedy.ResolveDuplicateTrials
    case UnmatchedFocal(_)            => Remedy.SupplyMatchedReference
    case UncontrolledFocal(_)         => Remedy.SupplyControlReference

  def message: String = this match
    case UndescribedMethod(m) =>
      s"Method ${m.name}@${m.version} has no descriptor; it can run but its parameters cannot be explained."
    case InconsistentDescriptor(m, e) =>
      s"Method ${m.name}@${m.version} disagrees with its descriptor: ${e.message}"
    case MissingArtifact(e)     => s"Study requires fixation artifact ${e.digest}."
    case ArtifactMismatch(e, a) => s"Study requires artifact ${e.digest}, supplied ${a.digest}."
    case OverBudget(e)          => e.message
    case FrameMismatch(k, e)    => s"Trial $k will fail at every scale: ${e.message}"
    case DuplicateTrial(k, side, positions) =>
      s"Trial key $k occurs at $side positions $positions and is excluded from pairing."
    case UnmatchedFocal(k) =>
      s"Focal trial $k has no same-stimulus reference within participant."
    case UncontrolledFocal(k) =>
      s"Focal trial $k has no different-stimulus control within participant."

/** Findings for the synchronized, angular, detected and AOI-assigned recording recipe. */
enum RecordingFinding extends PreflightFinding derives CanEqual:
  case UndescribedMethod(method: DefinitionId)
  case InconsistentDescriptor(method: DefinitionId, underlying: DescriptorError)
  case MissingArtifact(expected: ArtifactRef[Recording[Px]])
  case ArtifactMismatch(
      expected: ArtifactRef[Recording[Px]],
      actual: ArtifactRef[Recording[Px]]
  )
  case FrameMismatch(source: RecordingRef, underlying: GeometryError)
  case ClockMismatch(source: RecordingRef, underlying: TimeError)
  case MissingViewing(source: RecordingRef)
  case MissingSynchronization(source: ClockId, target: ClockId)
  case Synchronization(underlying: SyncEvidenceError)
  case AngularFrame(frame: FrameId, underlying: GeometryError)
  case AreaWarp(area: String, corner: AreaCorner)
  case DetectorDefinition(method: DefinitionId, underlying: DetectorDefinitionError)

  def severity: Severity = this match
    case UndescribedMethod(_) => Severity.Warning
    case _                    => Severity.Blocker

  def category: FindingClass = this match
    case MissingArtifact(_) | ArtifactMismatch(_, _) | MissingViewing(_) |
        MissingSynchronization(_, _) =>
      FindingClass.UnavailableInput
    case FrameMismatch(_, _) | ClockMismatch(_, _) => FindingClass.IncompatibleInput
    case _                                         => FindingClass.InvalidSetting

  def remedy: Remedy = this match
    case UndescribedMethod(_)         => Remedy.RegisterMethodDescriptor
    case InconsistentDescriptor(_, _) => Remedy.ReconcileMethodDescriptor
    case MissingArtifact(_)           => Remedy.SupplyReferencedArtifact
    case ArtifactMismatch(_, _)       => Remedy.RetargetPlanToAvailableInput
    case FrameMismatch(_, _)          => Remedy.AlignFrame
    case ClockMismatch(_, _)          => Remedy.AlignClock
    case MissingViewing(_)            => Remedy.SupplyViewingGeometry
    case MissingSynchronization(_, _) => Remedy.SupplyCommonMarks
    case Synchronization(_)           => Remedy.ReviseSynchronizationMarks
    case AngularFrame(_, _)           => Remedy.ReviseViewingOrArea
    case AreaWarp(_, _)               => Remedy.ReviseViewingOrArea
    case DetectorDefinition(_, _)     => Remedy.ReviseDetectorParameters

  def message: String = this match
    case UndescribedMethod(m) =>
      s"Detector ${m.name}@${m.version} has no descriptor; it can run but its parameters cannot be explained."
    case InconsistentDescriptor(m, e) =>
      s"Detector ${m.name}@${m.version} disagrees with its descriptor: ${e.message}"
    case MissingArtifact(e)     => s"Recording plan requires artifact ${e.digest}."
    case ArtifactMismatch(e, a) =>
      s"Recording plan requires artifact ${e.digest}, supplied ${a.digest}."
    case FrameMismatch(s, e)          => s"Recording $s: ${e.message}"
    case ClockMismatch(s, e)          => s"Recording $s: ${e.message}"
    case MissingViewing(s)            => s"Recording $s requires viewing geometry."
    case MissingSynchronization(s, t) =>
      s"Recording synchronization from $s to $t requires observed common marks."
    case Synchronization(e) => e.message
    case AngularFrame(f, e) => s"Angular frame ${f.name}: ${e.message}"
    case AreaWarp(id, c)    => s"Area '$id' has an undefined $c corner under viewing geometry."
    case DetectorDefinition(m, e) => s"Detector ${m.name}@${m.version}: ${e.message}"

/** Findings for the windowed temporal study. Repetition-level findings name the
  * repetition whose focal/reference phases produced them; base findings hold
  * for every repetition.
  */
enum TemporalFinding[K, U <: Unit2D] extends PreflightFinding derives CanEqual:
  case MissingArtifact(expected: ArtifactRef[TemporalStudyInput[K, U]])
  case ArtifactMismatch(
      expected: ArtifactRef[TemporalStudyInput[K, U]],
      actual: ArtifactRef[TemporalStudyInput[K, U]]
  )
  case Study(underlying: StudyFinding[K, U])
  case RepetitionPlan(repetition: String, underlying: PlanError)
  case Repetition(repetition: String, underlying: StudyFinding[K, U])
  case MissingEpoch(key: K)
  case CoverageClock(key: K, underlying: TimeError)
  case WindowResolution(key: K, window: String, underlying: TemporalStudyError)
  case NoObservedCoverage(key: K, window: String)

  def keys: Vector[K] = this match
    case Study(f)                  => f.keys
    case Repetition(_, f)          => f.keys
    case MissingEpoch(k)           => Vector(k)
    case CoverageClock(k, _)       => Vector(k)
    case WindowResolution(k, _, _) => Vector(k)
    case NoObservedCoverage(k, _)  => Vector(k)
    case MissingArtifact(_)        => Vector.empty
    case ArtifactMismatch(_, _)    => Vector.empty
    case RepetitionPlan(_, _)      => Vector.empty

  def severity: Severity = this match
    case MissingArtifact(_) | ArtifactMismatch(_, _) | RepetitionPlan(_, _) => Severity.Blocker
    case Study(f)                                                           => f.severity
    case Repetition(_, f)                                                   => f.severity
    case _                                                                  => Severity.Warning

  def category: FindingClass = this match
    case MissingArtifact(_) | ArtifactMismatch(_, _) | MissingEpoch(_) =>
      FindingClass.UnavailableInput
    case Study(f)                  => f.category
    case Repetition(_, f)          => f.category
    case RepetitionPlan(_, _)      => FindingClass.InvalidSetting
    case CoverageClock(_, _)       => FindingClass.IncompatibleInput
    case WindowResolution(_, _, _) => FindingClass.InvalidSetting
    case NoObservedCoverage(_, _)  => FindingClass.DataDependent

  def remedy: Remedy = this match
    case MissingArtifact(_)        => Remedy.SupplyReferencedArtifact
    case ArtifactMismatch(_, _)    => Remedy.RetargetPlanToAvailableInput
    case Study(f)                  => f.remedy
    case Repetition(_, f)          => f.remedy
    case RepetitionPlan(_, _)      => Remedy.ReviseWindow
    case MissingEpoch(_)           => Remedy.SupplyEpoch
    case CoverageClock(_, _)       => Remedy.AlignClock
    case WindowResolution(_, _, _) => Remedy.ReviseWindow
    case NoObservedCoverage(_, _)  => Remedy.AcceptMissingObservation

  def message: String = this match
    case MissingArtifact(e)     => s"Temporal study requires artifact ${e.digest}."
    case ArtifactMismatch(e, a) =>
      s"Temporal study requires artifact ${e.digest}, supplied ${a.digest}."
    case Study(f)                  => f.message
    case RepetitionPlan(r, e)      => s"Repetition '$r': ${e.message}"
    case Repetition(r, f)          => s"Repetition '$r': ${f.message}"
    case MissingEpoch(k)           => s"Trial $k has no measured anchor and observed coverage."
    case CoverageClock(k, e)       => s"Trial $k: ${e.message}"
    case WindowResolution(k, w, e) => s"Trial $k, window '$w': ${e.message}"
    case NoObservedCoverage(k, w)  =>
      s"Trial $k has no observed coverage inside window '$w'; its window mass is undefined."

/** Why a preflight report cannot be acted on now. */
enum PreflightError derives CanEqual:
  case ChangedPlan(family: RecipeFamily, changes: Vector[PlanChange])
  case ChangedInput(family: RecipeFamily, reported: ArtifactRef[?], actual: ArtifactRef[?])
  case NotReady(family: RecipeFamily, blockers: Vector[PreflightFinding])
  case Refused(underlying: PlanError)

  def message: String = this match
    case ChangedPlan(f, changes) =>
      s"$f plan changed since preflight in fields ${changes.map(_.field)}; preflight again."
    case ChangedInput(f, r, a) =>
      s"$f input changed since preflight: reported ${r.digest}, supplied ${a.digest}."
    case NotReady(f, blockers) => s"$f is unavailable: ${blockers.map(_.message)}"
    case Refused(e)            => e.message

/** A preflight report binds findings to the plan description and input identity
  * observed at preflight time. Readiness is the absence of blockers; it is not
  * a guarantee that estimation, comparison or detection succeeds on the data.
  */
sealed abstract class PreflightReport[F <: PreflightFinding]:
  def family: RecipeFamily
  def description: Vector[(String, Vector[Provenance.Param])]
  def findings: Vector[F]
  def notChecked: Vector[UncheckedAspect]
  final def blockers: Vector[F]        = findings.filter(_.severity == Severity.Blocker)
  final def warnings: Vector[F]        = findings.filter(_.severity == Severity.Warning)
  final def availability: Availability =
    if blockers.isEmpty then Availability.Ready else Availability.Unavailable
  final def ready: Boolean = availability == Availability.Ready

final class StudyReport[K, U <: Unit2D] private[plan] (
    val description: Vector[(String, Vector[Provenance.Param])],
    val expected: ArtifactRef[StudyInput[K, U]],
    val available: Option[ArtifactRef[StudyInput[K, U]]],
    val findings: Vector[StudyFinding[K, U]],
    val notChecked: Vector[UncheckedAspect]
)(using ordering: Ordering[K])
    extends PreflightReport[StudyFinding[K, U]]:
  val family: RecipeFamily = RecipeFamily.FixationStudy

  /** Distinct affected trial keys in the layout's canonical order. */
  def affectedTrials: Vector[K] = findings.flatMap(_.keys).distinct.sorted

  /** Prepare the same plan against the same input; anything changed since
    * preflight, or any blocker, is refused. Preparation itself revalidates
    * the artifact identity again.
    */
  def prepare[P, S, D](
      plan: StudyPlan[K, U, P, S, D],
      input: StudyInput[K, U],
      budget: PairScheduleBudget = PairScheduleBudget.default
  ): Either[PreflightError, PreparedStudy[K, U, P, S, D]] =
    for
      _ <- Preflight.confirm(
        family,
        description,
        plan.description,
        available,
        input.reference,
        blockers
      )
      work <- plan.prepare(input, budget).left.map(PreflightError.Refused.apply)
    yield work

final class RecordingReport private[plan] (
    val description: Vector[(String, Vector[Provenance.Param])],
    val expected: ArtifactRef[Recording[Px]],
    val available: Option[ArtifactRef[Recording[Px]]],
    val findings: Vector[RecordingFinding],
    val notChecked: Vector[UncheckedAspect]
) extends PreflightReport[RecordingFinding]:
  val family: RecipeFamily = RecipeFamily.EventRecording

  def confirm[P](
      plan: RecordingPlan[P],
      recording: Recording[Px]
  ): Either[PreflightError, Unit] =
    Preflight.confirm(
      family,
      description,
      plan.description,
      available,
      ArtifactRef.of[Recording[Px]](recording.contentHash),
      blockers
    )

final class TemporalReport[K, U <: Unit2D] private[plan] (
    val description: Vector[(String, Vector[Provenance.Param])],
    val expected: ArtifactRef[TemporalStudyInput[K, U]],
    val available: Option[ArtifactRef[TemporalStudyInput[K, U]]],
    val findings: Vector[TemporalFinding[K, U]],
    val notChecked: Vector[UncheckedAspect]
)(using ordering: Ordering[K])
    extends PreflightReport[TemporalFinding[K, U]]:
  val family: RecipeFamily = RecipeFamily.TemporalStudy

  def affectedTrials: Vector[K] = findings.flatMap(_.keys).distinct.sorted

  def confirm[P, S, D](
      plan: TemporalStudyPlan[K, U, P, S, D],
      input: TemporalStudyInput[K, U]
  ): Either[PreflightError, Unit] =
    Preflight.confirm(
      family,
      description,
      plan.description,
      available,
      input.reference,
      blockers
    )

/** Pure, bounded availability checks for the shipped recipes.
  *
  * Preflight reuses the plan constructors, `Agreement`, the synchronization
  * evidence constructor, the detector factory and the prepared pair schedule.
  * It never estimates densities, compares maps, warps samples or detects events.
  * Schedule traversal is bounded by the supplied `PairScheduleBudget`; recording
  * identity is O(samples) exactly as `RecordingPlan.prerequisites` is.
  */
object Preflight:
  val families: Vector[RecipeFamily] = RecipeFamily.values.toVector

  val studyUnchecked: Vector[UncheckedAspect] = Vector(
    UncheckedAspect.OccupancyEstimation,
    UncheckedAspect.PairComparison,
    UncheckedAspect.FailurePolicyReduction
  )
  val recordingUnchecked: Vector[UncheckedAspect] = Vector(
    UncheckedAspect.SampleSynchronization,
    UncheckedAspect.GapInterpolation,
    UncheckedAspect.EventDetection,
    UncheckedAspect.AreaAssignment
  )
  val temporalUnchecked: Vector[UncheckedAspect] =
    UncheckedAspect.WindowOccupancy +: studyUnchecked

  def study[K, U <: Unit2D, P, S, D](
      plan: StudyPlan[K, U, P, S, D],
      available: Option[StudyInput[K, U]],
      budget: PairScheduleBudget = PairScheduleBudget.default
  ): StudyReport[K, U] =
    given Ordering[K] = plan.layout.ordering
    val findings      = describe(plan) ++ studyArtifact(plan, available).fold(
      identity,
      input =>
        prepared(plan, input, budget).fold(
          e => Vector(e),
          work => frames(work) ++ schedule(work).fold(e => Vector(e), identity)
        )
    )
    new StudyReport(
      plan.description,
      plan.input,
      available.map(_.reference),
      findings,
      studyUnchecked
    )

  def recording[P](plan: RecordingPlan[P], available: Option[Recording[Px]]): RecordingReport =
    val described = plan.inspect match
      case Right(_)                                => Vector.empty
      case Left(DescriptorError.MissingMethod(id)) =>
        Vector(RecordingFinding.UndescribedMethod(id))
      case Left(e) => Vector(RecordingFinding.InconsistentDescriptor(plan.method.id, e))
    val artifact = available match
      case None    => Vector(RecordingFinding.MissingArtifact(plan.input))
      case Some(r) =>
        val ref = ArtifactRef.of[Recording[Px]](r.contentHash)
        if ref != plan.input then Vector(RecordingFinding.ArtifactMismatch(plan.input, ref))
        else
          Agreement
            .frames(plan.display, r.frame)
            .left
            .toOption
            .map(RecordingFinding.FrameMismatch(plan.source, _))
            .toVector ++
            Agreement
              .clocks(plan.trackerClock, r.clock)
              .left
              .toOption
              .map(RecordingFinding.ClockMismatch(plan.source, _))
              .toVector
    val viewing =
      if plan.viewing.isEmpty then Vector(RecordingFinding.MissingViewing(plan.source))
      else Vector.empty
    val synchronization =
      if plan.marks.isEmpty then
        Vector(RecordingFinding.MissingSynchronization(plan.trackerClock, plan.analysisClock))
      else
        SyncEvidence
          .fromCommonMarks(
            plan.trackerClock,
            plan.analysisClock,
            plan.synchronizationModel,
            plan.marks,
            plan.residualLimit
          )
          .left
          .toOption
          .map(RecordingFinding.Synchronization(_))
          .toVector
    val geometry = plan.viewing.toVector.flatMap { g =>
      Frame.angular(
        plan.angularFrameId.name,
        g.horizontalExtent.toDegrees,
        g.verticalExtent.toDegrees
      ) match
        case Left(e)        => Vector(RecordingFinding.AngularFrame(plan.angularFrameId, e))
        case Right(angular) =>
          val warp = Viewing.angularWarp(g, plan.display, angular)
          plan.areas.flatMap { area =>
            val lo = warp(Pt[Px](area.bounds.xMin, area.bounds.yMin))
            val hi = warp(Pt[Px](area.bounds.xMax, area.bounds.yMax))
            (if lo.isEmpty then Vector(RecordingFinding.AreaWarp(area.id, AreaCorner.Minimum))
             else Vector.empty) ++
              (if hi.isEmpty then Vector(RecordingFinding.AreaWarp(area.id, AreaCorner.Maximum))
               else Vector.empty)
          }
    }
    val detector = plan.method
      .detector(plan.parameters, plan.analysisClock)
      .left
      .toOption
      .map(RecordingFinding.DetectorDefinition(plan.method.id, _))
      .toVector
    new RecordingReport(
      plan.description,
      plan.input,
      available.map(r => ArtifactRef.of[Recording[Px]](r.contentHash)),
      described ++ artifact ++ viewing ++ synchronization ++ geometry ++ detector,
      recordingUnchecked
    )

  def temporal[K, U <: Unit2D: UnitLabel, P, S, D](
      plan: TemporalStudyPlan[K, U, P, S, D],
      available: Option[TemporalStudyInput[K, U]],
      budget: PairScheduleBudget = PairScheduleBudget.default
  ): TemporalReport[K, U] =
    given Ordering[K] = plan.base.layout.ordering
    val described     = describe(plan.base).map(TemporalFinding.Study(_))
    val artifact: Either[Vector[TemporalFinding[K, U]], TemporalStudyInput[K, U]] =
      available match
        case None => Left(Vector(TemporalFinding.MissingArtifact(plan.input)))
        case Some(a) if a.reference != plan.input =>
          Left(Vector(TemporalFinding.ArtifactMismatch(plan.input, a.reference)))
        case Some(a) => Right(a)
    val rest: Vector[TemporalFinding[K, U]] = artifact.fold(
      identity,
      a =>
        studyArtifact(plan.base, Some(a.study)) match
          case Left(fs)     => fs.map(TemporalFinding.Study(_))
          case Right(input) =>
            val repetitions = plan.repetitions.map { r =>
              r.name -> StudyPlan
                .of(
                  plan.base.input,
                  plan.base.layout,
                  plan.base.grid,
                  r.focalPhase,
                  r.referencePhase,
                  plan.base.weight,
                  plan.base.estimates,
                  plan.base.policy,
                  plan.base.method,
                  plan.base.parameters
                )
                .left
                .map(TemporalFinding.RepetitionPlan(r.name, _))
                .flatMap(p =>
                  prepared(p, input, budget).left.map(TemporalFinding.Repetition(r.name, _))
                )
            }
            val frameFindings = repetitions
              .collectFirst { case (_, Right(work)) =>
                frames(work).map(TemporalFinding.Study(_))
              }
              .getOrElse(Vector.empty)
            val scheduleFindings = repetitions.flatMap {
              case (_, Left(e))        => Vector(e)
              case (name, Right(work)) =>
                schedule(work).fold(
                  e => Vector(TemporalFinding.Repetition(name, e)),
                  _.map(TemporalFinding.Repetition(name, _))
                )
            }
            frameFindings ++ scheduleFindings ++ epochs(plan, a)
    )
    new TemporalReport(
      plan.description,
      plan.input,
      available.map(_.reference),
      described ++ rest,
      temporalUnchecked
    )

  private def describe[K, U <: Unit2D, P, S, D](
      plan: StudyPlan[K, U, P, S, D]
  ): Vector[StudyFinding[K, U]] =
    plan.inspect match
      case Right(_)                                => Vector.empty
      case Left(DescriptorError.MissingMethod(id)) => Vector(StudyFinding.UndescribedMethod(id))
      case Left(e) => Vector(StudyFinding.InconsistentDescriptor(plan.method.id, e))

  private def studyArtifact[K, U <: Unit2D, P, S, D](
      plan: StudyPlan[K, U, P, S, D],
      available: Option[StudyInput[K, U]]
  ): Either[Vector[StudyFinding[K, U]], StudyInput[K, U]] =
    available match
      case None => Left(Vector(StudyFinding.MissingArtifact(plan.input)))
      case Some(input) if input.reference != plan.input =>
        Left(Vector(StudyFinding.ArtifactMismatch(plan.input, input.reference)))
      case Some(input) => Right(input)

  private def prepared[K, U <: Unit2D, P, S, D](
      plan: StudyPlan[K, U, P, S, D],
      input: StudyInput[K, U],
      budget: PairScheduleBudget
  ): Either[StudyFinding[K, U], PreparedStudy[K, U, P, S, D]] =
    plan.prepare(input, budget).left.map(StudyFinding.OverBudget(_))

  private def frames[K, U <: Unit2D, P, S, D](
      work: PreparedStudy[K, U, P, S, D]
  ): Vector[StudyFinding[K, U]] =
    work.frameChecks.collect { case Left(StudyFailure.Frame(key, e)) =>
      StudyFinding.FrameMismatch(key, e)
    }

  private def schedule[K, U <: Unit2D, P, S, D](
      work: PreparedStudy[K, U, P, S, D]
  ): Either[StudyFinding[K, U], Vector[StudyFinding[K, U]]] =
    for
      matched <- complete(work.matched).left.map(e =>
        StudyFinding.OverBudget(PlanError.Schedule(e))
      )
      controls <- complete(work.controls).left.map(e =>
        StudyFinding.OverBudget(PlanError.Schedule(e))
      )
    yield work.matched.ambiguities.map {
      case PairingAmbiguity.DuplicateLeft(key, positions) =>
        StudyFinding.DuplicateTrial(key, PairingSide.Focal, positions)
      case PairingAmbiguity.DuplicateRight(key, positions) =>
        StudyFinding.DuplicateTrial(key, PairingSide.Reference, positions)
    } ++ matched.unmatchedLeft.map(StudyFinding.UnmatchedFocal(_)) ++
      controls.unmatchedLeft.map(StudyFinding.UncontrolledFocal(_))

  private def complete[KL, KR](
      schedule: DirectedPairSchedule[KL, KR]
  ): Either[PairScheduleError, PairingReport[KL, KR]] =
    @tailrec
    def loop(cursor: PairCursor[KL, KR]): Either[PairScheduleError, PairingReport[KL, KR]] =
      cursor.advance(PairQuantum.default) match
        case Left(e)                            => Left(e)
        case Right(PairPage.Done(_, _, report)) => Right(report)
        case Right(PairPage.More(_, _, next))   => loop(next)
    loop(schedule.start)

  private def epochs[K, U <: Unit2D, P, S, D](
      plan: TemporalStudyPlan[K, U, P, S, D],
      available: TemporalStudyInput[K, U]
  ): Vector[TemporalFinding[K, U]] =
    available.study.trials.rows.flatMap { trial =>
      available.epochs.get(trial.key) match
        case None        => Vector(TemporalFinding.MissingEpoch(trial.key))
        case Some(epoch) =>
          Agreement.clocks(trial.value.clock, epoch.coverage.clock) match
            case Left(e)  => Vector(TemporalFinding.CoverageClock(trial.key, e))
            case Right(_) =>
              plan.windows.flatMap { window =>
                window.resolve(epoch) match
                  case Left(e) =>
                    Vector(TemporalFinding.WindowResolution(trial.key, window.name, e))
                  case Right(interval) =>
                    val observed = epoch.coverage.intervals.exists { i =>
                      i.onset.toMicros < interval.offset.toMicros &&
                      i.offset.toMicros > interval.onset.toMicros
                    }
                    if observed then Vector.empty
                    else Vector(TemporalFinding.NoObservedCoverage(trial.key, window.name))
              }
    }

  private[plan] def changes(
      before: Vector[(String, Vector[Provenance.Param])],
      after: Vector[(String, Vector[Provenance.Param])]
  ): Vector[PlanChange] =
    val left  = before.toMap
    val right = after.toMap
    (left.keySet ++ right.keySet).toVector.sorted.collect {
      case key if left.get(key) != right.get(key) =>
        PlanChange(key, left.getOrElse(key, Vector.empty), right.getOrElse(key, Vector.empty))
    }

  private[plan] def confirm(
      family: RecipeFamily,
      reported: Vector[(String, Vector[Provenance.Param])],
      current: Vector[(String, Vector[Provenance.Param])],
      available: Option[ArtifactRef[?]],
      actual: ArtifactRef[?],
      blockers: Vector[PreflightFinding]
  ): Either[PreflightError, Unit] =
    val changed = changes(reported, current)
    if changed.nonEmpty then Left(PreflightError.ChangedPlan(family, changed))
    else
      available match
        case Some(ref) if ref != actual =>
          Left(PreflightError.ChangedInput(family, ref, actual))
        case _ if blockers.nonEmpty => Left(PreflightError.NotReady(family, blockers))
        case _                      => Right(())
