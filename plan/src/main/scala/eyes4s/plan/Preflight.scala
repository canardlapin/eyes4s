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

import cats.data.NonEmptyVector
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.detect.DetectorDefinitionError
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import scala.annotation.tailrec

/** The shipped recipe families a consumer can list and preflight. */
enum RecipeFamily derives CanEqual:
  case FixationStudy, EventRecording, TemporalStudy

/** A blocker means the plan's own constructors or prerequisites refuse the
  * recipe as a whole, so `prepare`/`run` cannot proceed. A warning means
  * execution proceeds, but the named trial has a deterministic failure or the
  * method's explanation is missing or inconsistent. Neither certifies
  * numerical success.
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
    ReviseDetectorParameters, SupplyEpoch, ReviseWindow, AcceptMissingObservation,
    ReviewAnalysisWindow, ReviseScaleDeclaration, ChooseMatchedReference,
    ResolveMatchItemConflict, ReviseInitialFixationPolicy

/** Outcomes that only execution decides. Preflight lists them instead of guessing. */
enum UncheckedAspect derives CanEqual:
  case OccupancyEstimation, PairComparison, FailurePolicyReduction, WindowOccupancy,
    SampleSynchronization, GapInterpolation, EventDetection, AreaAssignment

/** A report's verdict: `Ready` when it has no blocker, else `Unavailable`. */
enum Availability derives CanEqual:
  case Ready, Unavailable

/** Which side of a pairing a finding concerns: the focal (query) trials or the
  * reference trials.
  */
enum PairingSide derives CanEqual:
  case Focal, Reference

/** The corner of an area's bounding rectangle, minimum or maximum, that a finding
  * concerns.
  */
enum AreaCorner derives CanEqual:
  case Minimum, Maximum

/** The two ways a study exceeds its declared pair budget. `plan` is the exact
  * `PlanError` that `StudyPlan.prepare` or schedule paging returns for it.
  */
enum BudgetError derives CanEqual:
  case CandidateVisits(
      focalTrials: Int,
      referenceTrials: Int,
      scales: Int,
      maximumCandidateVisits: Long
  )
  case Schedule(underlying: PairScheduleError)

  def plan: PlanError = this match
    case CandidateVisits(f, r, s, m) => PlanError.StudyWorkBudget(f, r, s, m)
    case Schedule(e)                 => PlanError.Schedule(e)

  def message: String = plan.message

/** A finding of any shipped recipe family. `K` is the family's trial key;
  * recording findings name no trial and are `PreflightFinding[Nothing]`.
  */
sealed trait PreflightFinding[+K]:
  def severity: Severity
  def category: FindingClass
  def remedy: Remedy
  def message: String

  /** The trials this finding concerns, as typed keys. */
  def keys: Vector[K]

/** Findings for the within-participant matched/control fixation study. Trial
  * keys stay typed; duplicate positions index the focal or reference operand.
  * `Refused` carries any other constructor refusal so a new prerequisite in
  * `StudyPlan` surfaces as a blocker rather than being dropped.
  */
enum StudyFinding[K, U <: Unit2D] extends PreflightFinding[K] derives CanEqual:
  case UndescribedMethod(method: DefinitionId)
  case InconsistentDescriptor(method: DefinitionId, underlying: DescriptorError)
  case MissingArtifact(expected: ArtifactRef[StudyInput[K, U]])
  case ArtifactMismatch(
      expected: ArtifactRef[StudyInput[K, U]],
      actual: ArtifactRef[StudyInput[K, U]]
  )
  case OverBudget(underlying: BudgetError)
  case Refused(underlying: PlanError)
  case FrameMismatch(key: K, underlying: GeometryError)
  case DuplicateTrial(key: K, side: PairingSide, positions: Vector[Int])
  case UnmatchedFocal(key: K)
  case UncontrolledFocal(key: K)

  /** Some of the trial's fixations lie outside the analysis window or the
    * screen: left out of its map, or failing the trial under
    * [[OffWindowPolicy.FailTrial]] when any lies outside the window.
    */
  case OffWindowFixations(key: K, tally: WindowTally, policy: OffWindowPolicy)

  /** None of the trial's fixations lies inside the analysis window; it fails
    * at every scale.
    */
  case NoFixationInWindow(key: K, tally: WindowTally)

  /** The focal trial has more than one matched reference: a blocker under a
    * rule that requires one, a warning under the explicitly averaging
    * `MeanOfAll`.
    */
  case MatchedCardinality(key: K, references: Vector[K], matched: MatchedReferences)

  /** References of one participant and item that the control pool should
    * reduce to one under the pairing rule, but cannot.
    */
  case AmbiguousReferences(references: Vector[K], matched: MatchedReferences)

  /** The focal trial has no matched reference and the plan refuses such trials. */
  case UnmatchedFocalRefused(key: K)

  /** Keys the layout identifies as one trial name different match items. */
  case MatchItemConflict(trials: Vector[K])

  /** The plan's initial-fixation policy drops every fixation of the trial,
    * so its map would be empty and it fails at every scale.
    */
  case NoFixationKept(key: K, tally: InitialFixationTally)

  def keys: Vector[K] = this match
    case FrameMismatch(k, _)          => Vector(k)
    case DuplicateTrial(k, _, _)      => Vector(k)
    case UnmatchedFocal(k)            => Vector(k)
    case UncontrolledFocal(k)         => Vector(k)
    case OffWindowFixations(k, _, _)  => Vector(k)
    case NoFixationInWindow(k, _)     => Vector(k)
    case MatchedCardinality(k, rs, _) => k +: rs
    case AmbiguousReferences(rs, _)   => rs
    case UnmatchedFocalRefused(k)     => Vector(k)
    case MatchItemConflict(ks)        => ks
    case NoFixationKept(k, _)         => Vector(k)
    case UndescribedMethod(_)         => Vector.empty
    case InconsistentDescriptor(_, _) => Vector.empty
    case MissingArtifact(_)           => Vector.empty
    case ArtifactMismatch(_, _)       => Vector.empty
    case OverBudget(_)                => Vector.empty
    case Refused(_)                   => Vector.empty

  def severity: Severity = this match
    case MissingArtifact(_) | ArtifactMismatch(_, _) | OverBudget(_) | Refused(_) |
        AmbiguousReferences(_, _) | UnmatchedFocalRefused(_) | MatchItemConflict(_) =>
      Severity.Blocker
    case MatchedCardinality(_, _, matched) =>
      if matched == MatchedReferences.MeanOfAll then Severity.Warning else Severity.Blocker
    case UndescribedMethod(_) | InconsistentDescriptor(_, _) | FrameMismatch(_, _) |
        DuplicateTrial(_, _, _) | UnmatchedFocal(_) | UncontrolledFocal(_) |
        OffWindowFixations(_, _, _) | NoFixationInWindow(_, _) | NoFixationKept(_, _) =>
      Severity.Warning

  def category: FindingClass = this match
    case UndescribedMethod(_) | InconsistentDescriptor(_, _) | OverBudget(_) | Refused(_) =>
      FindingClass.InvalidSetting
    case MissingArtifact(_) | ArtifactMismatch(_, _) => FindingClass.UnavailableInput
    case FrameMismatch(_, _)                         => FindingClass.IncompatibleInput
    case DuplicateTrial(_, _, _) | UnmatchedFocal(_) | UncontrolledFocal(_) |
        OffWindowFixations(_, _, _) | NoFixationInWindow(_, _) | MatchedCardinality(_, _, _) |
        AmbiguousReferences(_, _) | UnmatchedFocalRefused(_) | MatchItemConflict(_) |
        NoFixationKept(_, _) =>
      FindingClass.DataDependent

  def remedy: Remedy = this match
    case UndescribedMethod(_)         => Remedy.RegisterMethodDescriptor
    case InconsistentDescriptor(_, _) => Remedy.ReconcileMethodDescriptor
    case MissingArtifact(_)           => Remedy.SupplyReferencedArtifact
    case ArtifactMismatch(_, _)       => Remedy.RetargetPlanToAvailableInput
    case OverBudget(_)                => Remedy.RaiseBudgetOrReduceStudy
    case Refused(e)                   => Preflight.remedyFor(e)
    case FrameMismatch(_, _)          => Remedy.AlignFrame
    case DuplicateTrial(_, _, _)      => Remedy.ResolveDuplicateTrials
    case UnmatchedFocal(_)            => Remedy.SupplyMatchedReference
    case UncontrolledFocal(_)         => Remedy.SupplyControlReference
    case OffWindowFixations(_, _, _)  => Remedy.ReviewAnalysisWindow
    case NoFixationInWindow(_, _)     => Remedy.ReviewAnalysisWindow
    case MatchedCardinality(_, _, _)  => Remedy.ChooseMatchedReference
    case AmbiguousReferences(_, _)    => Remedy.ChooseMatchedReference
    case UnmatchedFocalRefused(_)     => Remedy.SupplyMatchedReference
    case MatchItemConflict(_)         => Remedy.ResolveMatchItemConflict
    case NoFixationKept(_, _)         => Remedy.ReviseInitialFixationPolicy

  def message: String = this match
    case UndescribedMethod(m) =>
      s"Method ${m.name}@${m.version} has no descriptor; it can run but its parameters cannot be explained."
    case InconsistentDescriptor(m, e) =>
      s"Method ${m.name}@${m.version} runs but disagrees with its descriptor: ${e.message}"
    case MissingArtifact(e)     => s"Study requires fixation artifact ${e.digest}."
    case ArtifactMismatch(e, a) => s"Study requires artifact ${e.digest}, supplied ${a.digest}."
    case OverBudget(e)          => e.message
    case Refused(e)             => e.message
    case FrameMismatch(k, e)    => s"Trial $k will fail at every scale: ${e.message}"
    case DuplicateTrial(k, side, positions) =>
      s"Trial key $k occurs at $side positions $positions and is excluded from pairing."
    case UnmatchedFocal(k) =>
      s"Focal trial $k has no same-stimulus reference within participant."
    case UncontrolledFocal(k) =>
      s"Focal trial $k has no different-stimulus control within participant."
    case OffWindowFixations(k, t, policy) =>
      val fate =
        if policy == OffWindowPolicy.FailTrial && t.outsideWindow > 0 then
          "the trial fails at every scale"
        else "they are left out of its map"
      s"Trial $k has ${t.outsideWindow} of ${t.total} fixations outside the analysis window " +
        s"and ${t.outsideScreen} outside the screen; $fate."
    case NoFixationInWindow(k, t) =>
      s"Trial $k has no fixation inside the analysis window (${t.outsideWindow} outside the " +
        s"window, ${t.outsideScreen} outside the screen, of ${t.total}); it fails at every scale."
    case MatchedCardinality(k, rs, matched) =>
      if matched == MatchedReferences.MeanOfAll then
        s"Focal trial $k has ${rs.size} matched references $rs; MeanOfAll averages them."
      else
        s"Focal trial $k has ${rs.size} matched references $rs; ${matched.render} requires " +
          "one, so choose an occurrence."
    case AmbiguousReferences(rs, matched) =>
      s"References $rs share a participant and item; under ${matched.render} the control pool " +
        "needs one of them, so choose an occurrence."
    case UnmatchedFocalRefused(k) =>
      s"Focal trial $k has no matched reference, and the plan refuses unmatched focal trials."
    case MatchItemConflict(ks) =>
      s"Trials $ks are one trial by identity but name different match items."
    case NoFixationKept(k, t) =>
      s"Trial $k: the initial-fixation policy ${t.render}, so its map would be empty; " +
        "it fails at every scale."

/** Findings for the synchronized, angular, detected and AOI-assigned recording recipe. */
enum RecordingFinding extends PreflightFinding[Nothing] derives CanEqual:
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
  case Refused(underlying: RecordingPlanError)
  case Synchronization(underlying: SyncEvidenceError)
  case AngularFrame(frame: FrameId, underlying: GeometryError)
  case AreaWarp(area: String, corner: AreaCorner)
  case DetectorDefinition(method: DefinitionId, underlying: DetectorDefinitionError)

  /** A recording finding names no trial. */
  def keys: Vector[Nothing] = Vector.empty

  def severity: Severity = this match
    case UndescribedMethod(_) | InconsistentDescriptor(_, _) => Severity.Warning
    case MissingArtifact(_) | ArtifactMismatch(_, _) | FrameMismatch(_, _) |
        ClockMismatch(_, _) | MissingViewing(_) | MissingSynchronization(_, _) | Refused(_) |
        Synchronization(_) | AngularFrame(_, _) | AreaWarp(_, _) | DetectorDefinition(_, _) =>
      Severity.Blocker

  def category: FindingClass = this match
    case MissingArtifact(_) | ArtifactMismatch(_, _) | MissingViewing(_) |
        MissingSynchronization(_, _) =>
      FindingClass.UnavailableInput
    case FrameMismatch(_, _) | ClockMismatch(_, _) => FindingClass.IncompatibleInput
    case UndescribedMethod(_) | InconsistentDescriptor(_, _) | Refused(_) | Synchronization(_) |
        AngularFrame(_, _) | AreaWarp(_, _) | DetectorDefinition(_, _) =>
      FindingClass.InvalidSetting

  def remedy: Remedy = this match
    case UndescribedMethod(_)         => Remedy.RegisterMethodDescriptor
    case InconsistentDescriptor(_, _) => Remedy.ReconcileMethodDescriptor
    case MissingArtifact(_)           => Remedy.SupplyReferencedArtifact
    case ArtifactMismatch(_, _)       => Remedy.RetargetPlanToAvailableInput
    case FrameMismatch(_, _)          => Remedy.AlignFrame
    case ClockMismatch(_, _)          => Remedy.AlignClock
    case MissingViewing(_)            => Remedy.SupplyViewingGeometry
    case MissingSynchronization(_, _) => Remedy.SupplyCommonMarks
    case Refused(_)                   => Remedy.ReviseDetectorParameters
    case Synchronization(_)           => Remedy.ReviseSynchronizationMarks
    case AngularFrame(_, _)           => Remedy.ReviseViewingOrArea
    case AreaWarp(_, _)               => Remedy.ReviseViewingOrArea
    case DetectorDefinition(_, _)     => Remedy.ReviseDetectorParameters

  def message: String = this match
    case UndescribedMethod(m) =>
      s"Detector ${m.name}@${m.version} has no descriptor; it can run but its parameters cannot be explained."
    case InconsistentDescriptor(m, e) =>
      s"Detector ${m.name}@${m.version} runs but disagrees with its descriptor: ${e.message}"
    case MissingArtifact(e)     => s"Recording plan requires artifact ${e.digest}."
    case ArtifactMismatch(e, a) =>
      s"Recording plan requires artifact ${e.digest}, supplied ${a.digest}."
    case FrameMismatch(s, e)          => s"Recording $s: ${e.message}"
    case ClockMismatch(s, e)          => s"Recording $s: ${e.message}"
    case MissingViewing(s)            => s"Recording $s requires viewing geometry."
    case MissingSynchronization(s, t) =>
      s"Recording synchronization from $s to $t requires observed common marks."
    case Refused(e)         => e.message
    case Synchronization(e) => e.message
    case AngularFrame(f, e) => s"Angular frame ${f.name}: ${e.message}"
    case AreaWarp(id, c)    => s"Area '$id' has an undefined $c corner under viewing geometry."
    case DetectorDefinition(m, e) => s"Detector ${m.name}@${m.version}: ${e.message}"

/** Findings for the windowed temporal study. Repetition-level findings name the
  * repetition whose focal/reference phases produced them; base findings hold
  * for every repetition.
  */
enum TemporalFinding[K, U <: Unit2D] extends PreflightFinding[K] derives CanEqual:
  case MissingArtifact(expected: ArtifactRef[TemporalStudyInput[K, U]])
  case ArtifactMismatch(
      expected: ArtifactRef[TemporalStudyInput[K, U]],
      actual: ArtifactRef[TemporalStudyInput[K, U]]
  )
  case Refused(underlying: TemporalStudyError)
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
    case Refused(_)                => Vector.empty
    case RepetitionPlan(_, _)      => Vector.empty

  def severity: Severity = this match
    case MissingArtifact(_) | ArtifactMismatch(_, _) | Refused(_) | RepetitionPlan(_, _) =>
      Severity.Blocker
    case Study(f)         => f.severity
    case Repetition(_, f) => f.severity
    case MissingEpoch(_) | CoverageClock(_, _) | WindowResolution(_, _, _) |
        NoObservedCoverage(_, _) =>
      Severity.Warning

  def category: FindingClass = this match
    case MissingArtifact(_) | ArtifactMismatch(_, _) | MissingEpoch(_) =>
      FindingClass.UnavailableInput
    case Study(f)                          => f.category
    case Repetition(_, f)                  => f.category
    case Refused(_) | RepetitionPlan(_, _) => FindingClass.InvalidSetting
    case CoverageClock(_, _)               => FindingClass.IncompatibleInput
    case WindowResolution(_, _, _)         => FindingClass.InvalidSetting
    case NoObservedCoverage(_, _)          => FindingClass.DataDependent

  def remedy: Remedy = this match
    case MissingArtifact(_)        => Remedy.SupplyReferencedArtifact
    case ArtifactMismatch(_, _)    => Remedy.RetargetPlanToAvailableInput
    case Refused(e)                => Preflight.remedyFor(e)
    case Study(f)                  => f.remedy
    case RepetitionPlan(_, e)      => Preflight.remedyFor(e)
    case Repetition(_, f)          => f.remedy
    case MissingEpoch(_)           => Remedy.SupplyEpoch
    case CoverageClock(_, _)       => Remedy.AlignClock
    case WindowResolution(_, _, _) => Remedy.ReviseWindow
    case NoObservedCoverage(_, _)  => Remedy.AcceptMissingObservation

  def message: String = this match
    case MissingArtifact(e)     => s"Temporal study requires artifact ${e.digest}."
    case ArtifactMismatch(e, a) =>
      s"Temporal study requires artifact ${e.digest}, supplied ${a.digest}."
    case Refused(e)                => e.message
    case Study(f)                  => f.message
    case RepetitionPlan(r, e)      => s"Repetition '$r': ${e.message}"
    case Repetition(r, f)          => s"Repetition '$r': ${f.message}"
    case MissingEpoch(k)           => s"Trial $k has no measured anchor and observed coverage."
    case CoverageClock(k, e)       => s"Trial $k: ${e.message}"
    case WindowResolution(k, w, e) => s"Trial $k, window '$w': ${e.message}"
    case NoObservedCoverage(k, w)  =>
      s"Trial $k has no observed coverage inside window '$w'; its window mass is undefined."

/** Why a preflight report cannot be acted on now. `K` is the recipe family's
  * trial key, so the blockers of `NotReady` keep their typed keys.
  */
enum PreflightError[+K] derives CanEqual:
  case ChangedPlan(family: RecipeFamily, changes: Vector[PlanChange])
  case ChangedInput(family: RecipeFamily, reported: ArtifactRef[?], actual: ArtifactRef[?])
  case NotReady(family: RecipeFamily, blockers: Vector[PreflightFinding[K]])
  case Refused(underlying: PlanError)

  /** The trials the remaining blockers concern, each once in blocker order. */
  def affectedTrials: Vector[K] = this match
    case NotReady(_, blockers) => blockers.flatMap(_.keys).distinct
    case _                     => Vector.empty

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
sealed abstract class PreflightReport[F <: PreflightFinding[?]]:
  def family: RecipeFamily
  def description: Vector[(String, Vector[Provenance.Param])]
  def findings: Vector[F]
  def notChecked: Vector[UncheckedAspect]
  final def blockers: Vector[F]        = findings.filter(_.severity == Severity.Blocker)
  final def warnings: Vector[F]        = findings.filter(_.severity == Severity.Warning)
  final def availability: Availability =
    if blockers.isEmpty then Availability.Ready else Availability.Unavailable
  final def ready: Boolean = availability == Availability.Ready

/** A fixation study's preflight: the plan description, the input it expects and
  * the input it saw, its findings and the aspects only execution decides.
  * `prepare` refuses a plan or input that changed since preflight, or any blocker.
  */
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

  /** The findings as diagnostics, in finding order, keys typed. */
  def diagnostics: Vector[Diagnostic[K]] = findings.map(Diagnostic.of(_))

  /** Prepare the same plan against the same input; anything changed since
    * preflight, or any blocker, is refused. Preparation itself revalidates
    * the artifact identity again.
    */
  def prepare[P, S, D](
      plan: StudyPlan[K, U, P, S, D],
      input: StudyInput[K, U],
      budget: PairScheduleBudget = PairScheduleBudget.default
  ): Either[PreflightError[K], PreparedStudy[K, U, P, S, D]] =
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

/** An event recording's preflight, as [[StudyReport]] for a single pixel
  * `Recording`; its findings name no trial. `confirm` refuses
  * a changed plan or recording, or any blocker.
  */
final class RecordingReport private[plan] (
    val description: Vector[(String, Vector[Provenance.Param])],
    val expected: ArtifactRef[Recording[Px]],
    val available: Option[ArtifactRef[Recording[Px]]],
    val findings: Vector[RecordingFinding],
    val notChecked: Vector[UncheckedAspect]
) extends PreflightReport[RecordingFinding]:
  val family: RecipeFamily = RecipeFamily.EventRecording

  /** The findings as diagnostics, in finding order; they name no trial. */
  def diagnostics: Vector[Diagnostic[Nothing]] = findings.map(Diagnostic.of(_))

  def confirm[P](
      plan: RecordingPlan[P],
      recording: Recording[Px]
  ): Either[PreflightError[Nothing], Unit] =
    Preflight.confirm(
      family,
      description,
      plan.description,
      available,
      ArtifactRef.of[Recording[Px]](recording.contentHash),
      blockers
    )

/** A temporal study's preflight, as [[StudyReport]] over a `TemporalStudyInput`.
  * `confirm` refuses a changed plan or input, or any blocker.
  */
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

  /** The findings as diagnostics, in finding order, keys typed. */
  def diagnostics: Vector[Diagnostic[K]] = findings.map(Diagnostic.of(_))

  def confirm[P, S, D](
      plan: TemporalStudyPlan[K, U, P, S, D],
      input: TemporalStudyInput[K, U]
  ): Either[PreflightError[K], Unit] =
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
  * Artifact, frame, clock, viewing and mark findings are derived from each
  * plan's own `prerequisites`, so preflight and execution refuse on one rule.
  * Preflight additionally reuses `inspect`, the synchronization evidence
  * constructor, the detector factory and the prepared pair schedule. It never
  * estimates densities, compares maps, warps samples or detects events.
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
    val prerequisites = plan.prerequisites(available).map(studyRefusal(plan, available))
    val rest          =
      if prerequisites.nonEmpty then prerequisites
      else
        available.toVector.flatMap { input =>
          prepared(plan, input, budget).fold(
            e => Vector(e),
            work =>
              frames(work) ++ emptied(work) ++ windows(work) ++
                schedule(work).fold(e => Vector(e), identity)
          )
        }
    new StudyReport(
      plan.description,
      plan.input,
      available.map(_.reference),
      describe(plan) ++ rest,
      studyUnchecked
    )

  def recording[P](plan: RecordingPlan[P], available: Option[Recording[Px]]): RecordingReport =
    val actual    = available.map(r => ArtifactRef.of[Recording[Px]](r.contentHash))
    val described = plan.inspect match
      case Right(_)                                => Vector.empty
      case Left(DescriptorError.MissingMethod(id)) =>
        Vector(RecordingFinding.UndescribedMethod(id))
      case Left(e) => Vector(RecordingFinding.InconsistentDescriptor(plan.method.id, e))
    val prerequisites   = plan.prerequisites(available).map(recordingRefusal(plan, actual))
    val synchronization =
      if plan.marks.isEmpty then Vector.empty
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
      actual,
      described ++ prerequisites ++ synchronization ++ geometry ++ detector,
      recordingUnchecked
    )

  def temporal[K, U <: Unit2D, P, S, D](
      plan: TemporalStudyPlan[K, U, P, S, D],
      available: Option[TemporalStudyInput[K, U]],
      budget: PairScheduleBudget = PairScheduleBudget.default
  ): TemporalReport[K, U] =
    given Ordering[K] = plan.base.layout.ordering
    val described     = describe(plan.base).map(TemporalFinding.Study(_))
    val prerequisites = plan.prerequisites(available).map(temporalRefusal(plan, available))
    val rest: Vector[TemporalFinding[K, U]] =
      if prerequisites.nonEmpty then prerequisites
      else
        available.toVector.flatMap { a =>
          val input       = a.study
          val repetitions = plan.repetitions.map { r =>
            r.name -> plan.base
              .withPhases(r.focalPhase, r.referencePhase, plan.base.weight)
              .left
              .map(TemporalFinding.RepetitionPlan(r.name, _))
              .flatMap(p =>
                prepared(p, input, budget).left.map(TemporalFinding.Repetition(r.name, _))
              )
          }
          val frameFindings = repetitions
            .collectFirst { case (_, Right(work)) =>
              (frames(work) ++ emptied(work) ++ windows(work)).map(TemporalFinding.Study(_))
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
        }
    new TemporalReport(
      plan.description,
      plan.input,
      available.map(_.reference),
      described ++ rest,
      temporalUnchecked
    )

  /** The remedy for a plan-level refusal that is not one of the named findings. */
  private[plan] def remedyFor(error: PlanError): Remedy = error match
    case PlanError.MissingArtifact(_)     => Remedy.SupplyReferencedArtifact
    case PlanError.ArtifactMismatch(_, _) => Remedy.RetargetPlanToAvailableInput
    case PlanError.Schedule(_) | PlanError.StudyWorkBudget(_, _, _, _) |
        PlanError.ComparisonWork(_) =>
      Remedy.RaiseBudgetOrReduceStudy
    case PlanError.InvalidDefinition(_, _) | PlanError.InvalidArtifact(_) |
        PlanError.InvalidPhases(_, _) | PlanError.EmptyScales(_) |
        PlanError.DuplicateScales(_) | PlanError.Specification(_) |
        PlanError.ChangedPreparedPlan(_, _) | PlanError.UnsupportedExecution(_, _) =>
      Remedy.ReconcileMethodDescriptor
    case PlanError.MissingAngularScale(_) | PlanError.Geometry(_) |
        PlanError.InvalidWindowTally(_, _, _, _, _, _) =>
      Remedy.ReviseScaleDeclaration
    case PlanError.InvalidOccurrence(_) | PlanError.BlankKeyField(_) |
        PlanError.OccurrenceUnavailable(_, _) | PlanError.MatchedCardinality(_, _, _) =>
      Remedy.ChooseMatchedReference
    case PlanError.MatchItemConflict(_)     => Remedy.ResolveMatchItemConflict
    case PlanError.UnmatchedFocalRefused(_) => Remedy.SupplyMatchedReference
    case PlanError.InitialFixations(InitialFixationError.MissingAngularScale(_)) =>
      Remedy.ReviseScaleDeclaration
    case PlanError.InitialFixations(_) => Remedy.ReviseInitialFixationPolicy

  private[plan] def remedyFor(error: TemporalStudyError): Remedy = error match
    case TemporalStudyError.Input(e)     => remedyFor(e)
    case TemporalStudyError.Time(_)      => Remedy.AlignClock
    case TemporalStudyError.Occupancy(_) => Remedy.AlignClock
    case TemporalStudyError.InvalidWindow(_, _, _) |
        TemporalStudyError.AnchorOverflow(_, _, _, _) | TemporalStudyError.WindowNames(_) =>
      Remedy.ReviseWindow
    case TemporalStudyError.InvalidRepetition(_, _, _) | TemporalStudyError.RepetitionNames(_) |
        TemporalStudyError.Weighting(_) =>
      Remedy.ReconcileMethodDescriptor
    case TemporalStudyError.DuplicateEpochs(_) | TemporalStudyError.DuplicateTrials(_) |
        TemporalStudyError.UnknownEpochs(_) | TemporalStudyError.MissingEpoch(_) =>
      Remedy.SupplyEpoch

  private def describe[K, U <: Unit2D, P, S, D](
      plan: StudyPlan[K, U, P, S, D]
  ): Vector[StudyFinding[K, U]] =
    plan.inspect match
      case Right(_)                                => Vector.empty
      case Left(DescriptorError.MissingMethod(id)) => Vector(StudyFinding.UndescribedMethod(id))
      case Left(e) => Vector(StudyFinding.InconsistentDescriptor(plan.method.id, e))

  /** Map a `StudyPlan` refusal to its finding; total over `PlanError`. */
  private def studyRefusal[K, U <: Unit2D, P, S, D](
      plan: StudyPlan[K, U, P, S, D],
      available: Option[StudyInput[K, U]]
  )(error: PlanError): StudyFinding[K, U] =
    (error, available) match
      case (PlanError.MissingArtifact(_), _) => StudyFinding.MissingArtifact(plan.input)
      case (PlanError.ArtifactMismatch(_, _), Some(input)) =>
        StudyFinding.ArtifactMismatch(plan.input, input.reference)
      case (PlanError.StudyWorkBudget(f, r, s, m), _) =>
        StudyFinding.OverBudget(BudgetError.CandidateVisits(f, r, s, m))
      case (PlanError.Schedule(e), _) => StudyFinding.OverBudget(BudgetError.Schedule(e))
      case (other, _)                 => StudyFinding.Refused(other)

  /** Map a `RecordingPlan` prerequisite refusal to its finding; total. */
  private def recordingRefusal[P](
      plan: RecordingPlan[P],
      actual: Option[ArtifactRef[Recording[Px]]]
  )(error: RecordingPlanError): RecordingFinding =
    (error, actual) match
      case (RecordingPlanError.Input(PlanError.MissingArtifact(_)), _) =>
        RecordingFinding.MissingArtifact(plan.input)
      case (RecordingPlanError.Input(PlanError.ArtifactMismatch(_, _)), Some(ref)) =>
        RecordingFinding.ArtifactMismatch(plan.input, ref)
      case (RecordingPlanError.Geometry(e), _) => RecordingFinding.FrameMismatch(plan.source, e)
      case (RecordingPlanError.Time(e), _)     => RecordingFinding.ClockMismatch(plan.source, e)
      case (RecordingPlanError.MissingViewing(s), _) => RecordingFinding.MissingViewing(s)
      case (RecordingPlanError.MissingSynchronization(s, t), _) =>
        RecordingFinding.MissingSynchronization(s, t)
      case (other, _) => RecordingFinding.Refused(other)

  /** Map a `TemporalStudyPlan` prerequisite refusal to its finding; total. The
    * temporal and base study artifacts are both reported as `Input`, so the
    * expected digest decides which one a refusal names.
    */
  private def temporalRefusal[K, U <: Unit2D, P, S, D](
      plan: TemporalStudyPlan[K, U, P, S, D],
      available: Option[TemporalStudyInput[K, U]]
  )(error: TemporalStudyError): TemporalFinding[K, U] =
    (error, available) match
      case (TemporalStudyError.Input(PlanError.MissingArtifact(d)), _)
          if d == plan.input.digest =>
        TemporalFinding.MissingArtifact(plan.input)
      case (TemporalStudyError.Input(PlanError.ArtifactMismatch(d, _)), Some(a))
          if d == plan.input.digest =>
        TemporalFinding.ArtifactMismatch(plan.input, a.reference)
      case (TemporalStudyError.Input(e), _) =>
        TemporalFinding.Study(studyRefusal(plan.base, available.map(_.study))(e))
      case (other, _) => TemporalFinding.Refused(other)

  private def prepared[K, U <: Unit2D, P, S, D](
      plan: StudyPlan[K, U, P, S, D],
      input: StudyInput[K, U],
      budget: PairScheduleBudget
  ): Either[StudyFinding[K, U], PreparedStudy[K, U, P, S, D]] =
    plan.prepare(input, budget).left.map(studyRefusal(plan, Some(input)))

  private def frames[K, U <: Unit2D, P, S, D](
      work: PreparedStudy[K, U, P, S, D]
  ): Vector[StudyFinding[K, U]] =
    work.frameChecks.collect { case Left(StudyFailure.Frame(key, e)) =>
      StudyFinding.FrameMismatch(key, e)
    }

  /** Trials the initial-fixation policy leaves without fixations, in input
    * order: execution fails them with `InitialFixationError.NoFixationKept`.
    */
  private def emptied[K, U <: Unit2D, P, S, D](
      work: PreparedStudy[K, U, P, S, D]
  ): Vector[StudyFinding[K, U]] =
    work.initialFixationTallies.collect {
      case (key, Right(tally)) if tally.total > 0 && tally.kept == 0 =>
        StudyFinding.NoFixationKept(key, tally)
    }

  /** Trials with fixations outside the window or the screen, in input order.
    * A trial the initial-fixation policy leaves without fixations has an
    * empty tally and fails for that reason, not the window's.
    */
  private def windows[K, U <: Unit2D, P, S, D](
      work: PreparedStudy[K, U, P, S, D]
  ): Vector[StudyFinding[K, U]] =
    val policy = work.geometry match
      case StudyGeometry.Windowed(_, _, p) => p
      case StudyGeometry.WholeFrame(_)     => OffWindowPolicy.Exclude
    work.windowTallies.collect {
      case (key, Right(tally)) if tally.total > 0 && tally.allOutside =>
        StudyFinding.NoFixationInWindow(key, tally)
      case (key, Right(tally)) if tally.anyOutside =>
        StudyFinding.OffWindowFixations(key, tally, policy)
    }

  private def schedule[K, U <: Unit2D, P, S, D](
      work: PreparedStudy[K, U, P, S, D]
  ): Either[StudyFinding[K, U], Vector[StudyFinding[K, U]]] =
    for
      matched <- complete(work.matched).left.map(e =>
        StudyFinding.OverBudget(BudgetError.Schedule(e))
      )
      controls <- complete(work.controls).left.map(e =>
        StudyFinding.OverBudget(BudgetError.Schedule(e))
      )
      cardinality <- work.matchedCardinality.left.map {
        case PlanError.Schedule(e) => StudyFinding.OverBudget(BudgetError.Schedule(e))
        case other                 => StudyFinding.Refused(other)
      }
    yield
      val refuse = work.pairing.unmatched == UnmatchedFocalPolicy.Refuse
      val policy = work.pairing.matched
      work.matched.ambiguities.map {
        case PairingAmbiguity.DuplicateLeft(key, positions) =>
          StudyFinding.DuplicateTrial(key, PairingSide.Focal, positions)
        case PairingAmbiguity.DuplicateRight(key, positions) =>
          StudyFinding.DuplicateTrial(key, PairingSide.Reference, positions)
      } ++ cardinality.itemConflicts.map(StudyFinding.MatchItemConflict(_)) ++
        matched.unmatchedLeft.map(k =>
          if refuse then StudyFinding.UnmatchedFocalRefused(k)
          else StudyFinding.UnmatchedFocal(k)
        ) ++
        cardinality.multiple.map((k, rs) => StudyFinding.MatchedCardinality(k, rs, policy)) ++
        cardinality.blockingReferences.map(StudyFinding.AmbiguousReferences(_, policy)) ++
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

  private[plan] def confirm[K](
      family: RecipeFamily,
      reported: Vector[(String, Vector[Provenance.Param])],
      current: Vector[(String, Vector[Provenance.Param])],
      available: Option[ArtifactRef[?]],
      actual: ArtifactRef[?],
      blockers: Vector[PreflightFinding[K]]
  ): Either[PreflightError[K], Unit] =
    val changed = PlanChange.between(reported, current)
    if changed.nonEmpty then Left(PreflightError.ChangedPlan(family, changed))
    else
      available match
        case Some(ref) if ref != actual =>
          Left(PreflightError.ChangedInput(family, ref, actual))
        case _ if blockers.nonEmpty => Left(PreflightError.NotReady(family, blockers))
        case _                      => Right(())

/** The finding shape shared by every recipe family added after the fixation,
  * recording and temporal studies. A family states its own checks as typed
  * errors of its own catalogued families and carries them here as
  * diagnostics, so no family adds a finding type to this sealed hierarchy.
  *
  * Severity and class follow from the case: a missing or mismatched artifact
  * and a refusal are blockers, a data-dependent failure is a warning that
  * names the trials execution will fail. The remedy of an artifact finding
  * follows from its case too; a refusal carries the remedy its cause implies
  * (build one with [[AnalysisFinding.refused]]).
  */
enum AnalysisFinding[K] extends PreflightFinding[K] derives CanEqual:
  /** The plan's input artifact was not supplied. */
  case MissingArtifact(expected: ArtifactRef[?])

  /** An input artifact other than the one the plan names was supplied. */
  case ArtifactMismatch(expected: ArtifactRef[?], actual: ArtifactRef[?])

  /** The plan's constructors or prerequisites refuse the recipe as a whole;
    * `underlying` is the refusal projected through its own family and
    * `suggested` the remedy its cause implies.
    */
  case Refused(underlying: Diagnostic[K], suggested: Remedy)

  /** Execution proceeds, but the named trials (at least one) fail
    * deterministically for the reason `underlying` gives.
    */
  case DataDependent(underlying: Diagnostic[K], trials: NonEmptyVector[K])

  def keys: Vector[K] = this match
    case MissingArtifact(_)            => Vector.empty
    case ArtifactMismatch(_, _)        => Vector.empty
    case Refused(underlying, _)        => underlying.affectedTrials
    case DataDependent(underlying, ks) => (ks.toVector ++ underlying.affectedTrials).distinct

  def severity: Severity = this match
    case MissingArtifact(_) | ArtifactMismatch(_, _) | Refused(_, _) => Severity.Blocker
    case DataDependent(_, _)                                         => Severity.Warning

  def category: FindingClass = this match
    case MissingArtifact(_) | ArtifactMismatch(_, _) => FindingClass.UnavailableInput
    case Refused(_, _)                               => FindingClass.InvalidSetting
    case DataDependent(_, _)                         => FindingClass.DataDependent

  def remedy: Remedy = this match
    case MissingArtifact(_)     => Remedy.SupplyReferencedArtifact
    case ArtifactMismatch(_, _) => Remedy.RetargetPlanToAvailableInput
    case Refused(_, suggested)  => suggested
    case DataDependent(_, _)    => Remedy.AcceptMissingObservation

  def message: String = this match
    case MissingArtifact(e)     => s"Analysis requires artifact ${e.digest}."
    case ArtifactMismatch(e, a) =>
      s"Analysis requires artifact ${e.digest}, supplied ${a.digest}."
    case Refused(d, _)        => s"Analysis refused (${d.code}): ${d.message}"
    case DataDependent(d, ks) => s"Trials ${ks.toVector} will fail (${d.code}): ${d.message}"

object AnalysisFinding:
  /** A refusal with the remedy its cause implies. The cause's type must have
    * a [[Remedial]] instance, so a family whose error type states no remedy
    * does not compile; a family that wants the generic remedy says so with
    * [[Remedial.fixed]].
    */
  def refused[E, K](error: E)(using
      diagnose: Diagnose[E, K],
      remedial: Remedial[E]
  ): AnalysisFinding[K] =
    Refused(diagnose(error), remedial.remedy(error))

/** The remedy a refusal's cause implies. Instances follow the rule the
  * dedicated finding types use; there is deliberately no catch-all instance.
  */
trait Remedial[-E]:
  def remedy(error: E): Remedy

object Remedial:
  /** An explicit, constant remedy for every error of the type. */
  def fixed[E](value: Remedy): Remedial[E] = new Fixed(value)

  /** As `StudyFinding.Refused` derives it. */
  given plan: Remedial[PlanError] = new Remedial[PlanError]:
    def remedy(error: PlanError): Remedy = Preflight.remedyFor(error)

  /** As `TemporalFinding.Refused` derives it. */
  given temporal: Remedial[TemporalStudyError] = new Remedial[TemporalStudyError]:
    def remedy(error: TemporalStudyError): Remedy = Preflight.remedyFor(error)

  /** As `RecordingFinding.Refused` gives it. */
  given recording: Remedial[RecordingPlanError] = fixed(Remedy.ReviseDetectorParameters)

  /** A finding keeps its own remedy. */
  given finding: Remedial[PreflightFinding[?]] = new Remedial[PreflightFinding[?]]:
    def remedy(error: PreflightFinding[?]): Remedy = error.remedy

  private final class Fixed[E](value: Remedy) extends Remedial[E]:
    def remedy(error: E): Remedy = value

/** The preflight report of a recipe family that uses [[AnalysisFinding]]:
  * findings bound to the plan description and the identity of the input `A`
  * observed at preflight time, confirmed exactly as the other reports are.
  */
final class AnalysisReport[K, A] private (
    val family: RecipeFamily,
    val description: Vector[(String, Vector[Provenance.Param])],
    val expected: ArtifactRef[A],
    val available: Option[ArtifactRef[A]],
    val findings: Vector[AnalysisFinding[K]],
    val notChecked: Vector[UncheckedAspect]
) extends PreflightReport[AnalysisFinding[K]]:

  /** Distinct affected trial keys, in finding order. */
  def affectedTrials: Vector[K] = findings.flatMap(_.keys).distinct

  /** The findings as diagnostics, in finding order, keys typed. */
  def diagnostics: Vector[Diagnostic[K]] = findings.map(Diagnostic.of(_))

  /** Refuse a plan whose description changed since preflight, an input
    * other than the one preflight saw, or any remaining blocker.
    */
  def confirm(
      current: Vector[(String, Vector[Provenance.Param])],
      actual: ArtifactRef[A]
  ): Either[PreflightError[K], Unit] =
    Preflight.confirm(family, description, current, available, actual, blockers)

object AnalysisReport:
  /** A report whose artifact findings are derived here, before the family's
    * own: `MissingArtifact` when nothing was supplied, `ArtifactMismatch`
    * when another artifact was.
    */
  private[plan] def of[K, A](
      family: RecipeFamily,
      description: Vector[(String, Vector[Provenance.Param])],
      expected: ArtifactRef[A],
      available: Option[ArtifactRef[A]],
      findings: Vector[AnalysisFinding[K]],
      notChecked: Vector[UncheckedAspect]
  ): AnalysisReport[K, A] =
    val artifact: Vector[AnalysisFinding[K]] = available match
      case None                         => Vector(AnalysisFinding.MissingArtifact(expected))
      case Some(ref) if ref != expected =>
        Vector(AnalysisFinding.ArtifactMismatch(expected, ref))
      case Some(_) => Vector.empty
    new AnalysisReport(
      family,
      description,
      expected,
      available,
      artifact ++ findings,
      notChecked
    )
