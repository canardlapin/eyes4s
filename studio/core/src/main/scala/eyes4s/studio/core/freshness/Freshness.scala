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

package eyes4s.studio.core.freshness

import cats.data.NonEmptyVector
import eyes4s.studio.core.backend.{
  AnalysisRevision,
  DatasetRevision,
  DiagnosticLevel,
  JobOutcome,
  JobProgress,
  ProgressTotal,
  RunId,
  StageKind,
  StudioDiagnostic
}
import eyes4s.studio.core.document.*

// ---------------------------------------------------------------------------
// Standing of one run
// ---------------------------------------------------------------------------

/** Why a completed run no longer reflects the latest saved science: a newer
  * analysis revision was saved, or a newer dataset revision was admitted.
  */
enum StaleReason derives CanEqual:
  case AnalysisMoved(run: AnalysisRevision, latest: AnalysisRevision)
  case DatasetMoved(run: DatasetRevision, latest: DatasetRevision)

/** A run's standing against the latest saved analysis revision and the
  * latest admitted dataset revision. Only a completed run can be current or
  * stale; the other lifecycles are reported as they are.
  */
enum RunStanding derives CanEqual:
  case Current

  /** Analysis reason first, then dataset. */
  case Stale(reasons: NonEmptyVector[StaleReason])
  case Running
  case Cancelled(at: Option[StageKind])
  case Failed

/** One run and its derived standing. */
final case class RunFreshness(run: RunRef, standing: RunStanding) derives CanEqual

// ---------------------------------------------------------------------------
// Session facts: what the document does not store
// ---------------------------------------------------------------------------

/** The result of checking a draft (preflight and studio checks), for exactly
  * that draft value: a check of another draft, or of an earlier edit of this
  * one, does not apply.
  */
enum DraftCheck derives CanEqual:
  case Unchecked
  case Checked(draft: Draft, diagnostics: Vector[StudioDiagnostic])

/** Session state that the freshness grammar reads besides the document: the
  * progress reports and outcomes of this session's jobs, and the latest draft
  * check. None of it is science, and a reopened document starts with
  * [[SessionFacts.empty]].
  */
final case class SessionFacts(
    progress: Vector[JobProgress],
    outcomes: Vector[JobOutcome],
    draftCheck: DraftCheck
) derives CanEqual

object SessionFacts:
  val empty: SessionFacts = SessionFacts(Vector.empty, Vector.empty, DraftCheck.Unchecked)

// ---------------------------------------------------------------------------
// The derived values
// ---------------------------------------------------------------------------

/** How far a running run has got, as the jobs chip reads it (UI-D run
  * totals): the current stage and pairs compared over every scale.
  */
final case class RunMeter(stage: StageKind, completedPairs: Long, totalPairs: ProgressTotal)
    derives CanEqual

/** A run at least as new as the shown one that has not completed. */
enum RunActivity derives CanEqual:
  /** `meter` is `None` until this session holds a progress report from the
    * run's own job.
    */
  case Running(run: RunId, revision: AnalysisRevision, meter: Option[RunMeter])

  /** `diagnostics` is `None` when this session did not see the outcome. */
  case Failed(run: RunId, revision: AnalysisRevision, diagnostics: Option[Int])
  case Cancelled(run: RunId, revision: AnalysisRevision, at: Option[StageKind])

  def run: RunId
  def revision: AnalysisRevision

/** The one-word state of the badge. */
enum BadgeTone derives CanEqual:
  case Current, Stale, Running, Failed, Cancelled, NoRun

  def word: String = this match
    case Current   => "current"
    case Stale     => "stale"
    case Running   => "running"
    case Failed    => "failed"
    case Cancelled => "cancelled"
    case NoRun     => "no run yet"

/** The freshness badge of the context strip (DESIGN_SPEC section 3). */
enum Badge derives CanEqual:
  /** No run to show: the latest saved revision and admitted data, if any. */
  case NoRun(analysis: Option[AnalysisRevision], data: Option[DatasetRevision])

  /** The shown run, its standing, and a newer run's activity when there is
    * one; the activity outranks the standing in the tone.
    */
  case Shown(run: RunRef, standing: RunStanding, activity: Option[RunActivity])

  def tone: BadgeTone = this match
    case NoRun(_, _)                                 => BadgeTone.NoRun
    case Shown(_, _, Some(_: RunActivity.Running))   => BadgeTone.Running
    case Shown(_, _, Some(_: RunActivity.Failed))    => BadgeTone.Failed
    case Shown(_, _, Some(_: RunActivity.Cancelled)) => BadgeTone.Cancelled
    case Shown(_, RunStanding.Current, None)         => BadgeTone.Current
    case Shown(_, RunStanding.Stale(_), None)        => BadgeTone.Stale
    case Shown(_, RunStanding.Running, None)         => BadgeTone.Running
    case Shown(_, RunStanding.Failed, None)          => BadgeTone.Failed
    case Shown(_, RunStanding.Cancelled(_), None)    => BadgeTone.Cancelled

/** Whether Save & run is possible for the draft. */
enum DraftReadiness derives CanEqual:
  /** No check of this exact draft has been seen. */
  case Unchecked
  case Ready

  /** `blockers` error-level diagnostics; warnings do not block. */
  case Blocked(blockers: Int)

/** The dashed draft chip: "Draft rev 5 · 1 change · ready". */
final case class DraftChip(draft: Draft, readiness: DraftReadiness) derives CanEqual:
  def changes: Int = draft.changeCount

/** The dock-wide banner, shown only in Compare and Figures. */
enum Banner derives CanEqual:
  /** Nothing has completed yet ("Compare · No run yet"). */
  case NoRun(latest: Option[AnalysisRevision])

  /** A newer run is producing results; the view stays on `shown`. `changes`
    * turn the shown run's recipe into the running one's.
    */
  case NewerRunning(shown: RunRef, running: RunActivity.Running, changes: Vector[RecipeChange])

  /** A newer run has completed but is not shown. */
  case NewerCompleted(shown: RunRef, newer: RunRef, changes: Vector[RecipeChange])

  /** The draft differs from the shown run's revision and has not been run. */
  case DraftNotRun(
      shown: RunRef,
      draft: Draft,
      changes: Vector[RecipeChange],
      dataset: Option[DatasetRevision]
  )

  /** The shown run is stale and nothing newer is pending. */
  case ShownStale(shown: RunRef, reasons: NonEmptyVector[StaleReason])

/** A figure and the standing of the one run it binds. A figure never
  * follows a newer run, so it goes stale with its run.
  */
final case class FigureFreshness(figure: FigureId, run: RunRef, standing: RunStanding)
    derives CanEqual:
  def isStale: Boolean = standing match
    case RunStanding.Stale(_) => true
    case _                    => false

/** A dataset revision being verified (not admitted), newer than the latest
  * admitted one: admitting it makes `wouldStale` stale.
  */
final case class PendingDataset(dataset: DatasetRevision, wouldStale: Vector[RunId])
    derives CanEqual

/** Everything the UI shows about freshness, derived from one document and
  * the session's facts (ticket S2.7).
  */
final case class Freshness(
    badge: Badge,
    draft: Option[DraftChip],
    banner: Option[Banner],
    runs: Vector[RunFreshness],
    figures: Vector[FigureFreshness],
    activity: Option[RunActivity],
    pending: Vector[PendingDataset]
) derives CanEqual:
  /** The banner as a perspective shows it: only Compare and Figures do. */
  def bannerFor(perspective: Perspective): Option[Banner] = perspective match
    case Perspective.Compare | Perspective.Figures => banner
    case _                                         => None

  def standing(run: RunId): Option[RunStanding] = runs.find(_.run.id == run).map(_.standing)

  def figure(id: FigureId): Option[FigureFreshness] = figures.find(_.figure == id)

// ---------------------------------------------------------------------------
// Derivation
// ---------------------------------------------------------------------------

/** The pure freshness derivation (ticket S2.7).
  *
  * A completed run is current exactly when it names the latest saved analysis
  * revision and the latest admitted dataset revision; otherwise it is stale,
  * with each reason that applies. A draft is not saved science and never
  * makes a run stale; a dataset revision still being verified does not
  * either, but is reported in [[Freshness.pending]].
  *
  * The shown run is the presentation's, or else the latest completed run, or
  * else the latest run of any lifecycle. The newest run is the badge's
  * activity when it is at least as new as the shown run and has not
  * completed.
  */
object Freshness:

  def of(document: StudioDocument, session: SessionFacts): Freshness =
    val latestAnalysis = document.latestAnalysis.map(_.id)
    val latestData     = document.latestAdmitted.map(_.id)

    def standingOf(run: RunRef): RunStanding = run.state match
      case RunLifecycle.Running       => RunStanding.Running
      case RunLifecycle.Failed        => RunStanding.Failed
      case RunLifecycle.Cancelled(at) => RunStanding.Cancelled(at)
      case RunLifecycle.Completed     =>
        val reasons =
          latestAnalysis
            .filter(_ != run.analysis)
            .map(StaleReason.AnalysisMoved(run.analysis, _)) ++
            latestData.filter(_ != run.dataset).map(StaleReason.DatasetMoved(run.dataset, _))
        NonEmptyVector
          .fromVector(reasons.toVector)
          .fold(RunStanding.Current)(RunStanding.Stale(_))

    val runs = document.runs.map(r => RunFreshness(r, standingOf(r)))

    val shown: Option[RunRef] =
      document.presentation.shownRun
        .flatMap(document.run)
        .orElse(document.runs.findLast(_.state == RunLifecycle.Completed))
        .orElse(document.runs.lastOption)

    val newest   = document.runs.lastOption
    val activity = newest
      .filter(n => shown.forall(_.id.number <= n.id.number))
      .flatMap(activityOf(document, session, _))

    val badge = shown.fold(Badge.NoRun(latestAnalysis, latestData)) { s =>
      Badge.Shown(s, standingOf(s), activity)
    }

    val chip = document.draft.map(d => DraftChip(d, readiness(d, session.draftCheck)))

    val banner = shown.filter(_.state == RunLifecycle.Completed) match
      case None    => Some(Banner.NoRun(latestAnalysis))
      case Some(s) =>
        val newer = newest.filter(_.id.number > s.id.number)
        activity
          .collect { case r: RunActivity.Running if r.run.number > s.id.number => r }
          .map(r => Banner.NewerRunning(s, r, changes(document, s.analysis, r.revision)))
          .orElse(
            newer
              .filter(_.state == RunLifecycle.Completed)
              .map(n => Banner.NewerCompleted(s, n, changes(document, s.analysis, n.analysis)))
          )
          .orElse(document.draft.flatMap(draftBanner(document, s, _)))
          .orElse(standingOf(s) match
            case RunStanding.Stale(reasons) => Some(Banner.ShownStale(s, reasons))
            case _                          => None)

    val figures = document.figures.flatMap { f =>
      document.run(f.run).map(r => FigureFreshness(f.id, r, standingOf(r)))
    }

    val pending = latestData match
      case None         => Vector.empty
      case Some(latest) =>
        val current = runs.collect { case RunFreshness(r, RunStanding.Current) => r.id }
        document.datasets
          .filter(d => !d.decision.isAdmitted && d.id.number > latest.number)
          .map(d => PendingDataset(d.id, current))

    Freshness(badge, chip, banner, runs, figures, activity, pending)

  /** A run's activity, if it has not completed. */
  private def activityOf(
      document: StudioDocument,
      session: SessionFacts,
      run: RunRef
  ): Option[RunActivity] = run.state match
    case RunLifecycle.Completed     => None
    case RunLifecycle.Cancelled(at) => Some(RunActivity.Cancelled(run.id, run.analysis, at))
    case RunLifecycle.Failed        =>
      val diagnostics = session.outcomes.collect {
        case JobOutcome.Failed(_, r, ds, _) if r == run.id => ds.size
      }
      Some(RunActivity.Failed(run.id, run.analysis, diagnostics.lastOption))
    case RunLifecycle.Running =>
      // Only a report from the run's own job counts: a report of a lost or
      // earlier job is never shown as this run's progress.
      val meter = document.job(run.id).flatMap { job =>
        session.progress
          .filter(p => p.run == run.id && p.job == job)
          .maxByOption(_.step)
          .map(p => RunMeter(p.segment.kind, p.totals.completedPairs, p.totals.totalPairs))
      }
      Some(RunActivity.Running(run.id, run.analysis, meter))

  private def readiness(draft: Draft, check: DraftCheck): DraftReadiness = check match
    case DraftCheck.Checked(checked, diagnostics) if checked == draft =>
      val blockers = diagnostics.count(_.level == DiagnosticLevel.Error)
      if blockers == 0 then DraftReadiness.Ready else DraftReadiness.Blocked(blockers)
    case _ => DraftReadiness.Unchecked

  /** The recipe changes from one saved revision to another. */
  private def changes(
      document: StudioDocument,
      from: AnalysisRevision,
      to: AnalysisRevision
  ): Vector[RecipeChange] =
    (document.analysis(from), document.analysis(to)) match
      case (Some(a), Some(b)) => RecipeChange.between(a.recipe, b.recipe)
      case _                  => Vector.empty

  /** The draft banner, when the draft differs from the shown run's revision
    * in a recipe field or in the dataset it would run on.
    */
  private def draftBanner(
      document: StudioDocument,
      shown: RunRef,
      draft: Draft
  ): Option[Banner] =
    for
      shownSpec <- document.analysis(shown.analysis)
      base      <- document.analysis(draft.base)
      recipe = draft.recipe(base.recipe)
      target = draft.dataset.getOrElse(base.dataset)
      diff   = RecipeChange.between(shownSpec.recipe, recipe)
      rebase = Option.when(target != shown.dataset)(target)
      if diff.nonEmpty || rebase.nonEmpty
    yield Banner.DraftNotRun(shown, draft, diff, rebase)
