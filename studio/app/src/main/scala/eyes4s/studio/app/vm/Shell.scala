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

package eyes4s.studio.app.vm

import eyes4s.studio.app.{AppModel, Confirmation, Intent}
import eyes4s.studio.app.jobs.JobHeadline
import eyes4s.studio.core.backend.{ProgressTotal, RunId}
import eyes4s.studio.core.execution.{ExecutionJob, JobPhase, Meter, MeterTotal}
import eyes4s.studio.app.keys.CommandRegistry
import eyes4s.studio.app.nav.{Location, Place}
import eyes4s.studio.app.text.{Format, MessageId, Messages}
import eyes4s.studio.core.document.{
  AdmissionDecision,
  DatasetRevisionSpec,
  Perspective,
  RecipeChange
}
import eyes4s.studio.core.freshness.{
  Badge,
  BadgeTone,
  Banner,
  DraftReadiness,
  RunActivity,
  RunStanding,
  StaleReason
}
import eyes4s.studio.core.selection.StudioRef

/** The shell view-models of one model (ticket S1.0): pure projections. */
object Shell:
  import MessageId.*

  def project(model: AppModel, messages: Messages = Messages.english): ShellVM =
    val p = Projection(model, messages)
    ShellVM(
      p.window,
      p.appBar,
      p.context,
      p.banner,
      p.status,
      model.notice.map(n => NoticeVM(n.message(messages), Intent.Dismiss)),
      p.confirmation
    )

  def appBar(model: AppModel, messages: Messages = Messages.english): AppBarVM =
    Projection(model, messages).appBar

  def context(model: AppModel, messages: Messages = Messages.english): ContextStripVM =
    Projection(model, messages).context

  def banner(model: AppModel, messages: Messages = Messages.english): Option[DraftBannerVM] =
    Projection(model, messages).banner

  def status(model: AppModel, messages: Messages = Messages.english): StatusBarVM =
    Projection(model, messages).status

  def confirmation(
      model: AppModel,
      messages: Messages = Messages.english
  ): Option[ConfirmationVM] =
    Projection(model, messages).confirmation

  /** Where a crumb of the current trail lands: the perspective its prefix
    * belongs to, with that prefix (the same rule as [[Intent.OpenCrumb]]).
    */
  def crumbTarget(model: AppModel, index: Int): Option[Location] =
    val trail = model.location.trail
    Option.when(index >= 0 && index < trail.size) {
      val prefix = trail.take(index + 1)
      Location(Place.home(prefix).getOrElse(model.perspective), prefix)
    }

  private final case class Projection(m: AppModel, messages: Messages):
    private val labels = Labels(m, messages)
    private def doc    = m.document

    // --- Window and app bar ----------------------------------------------------

    def window: WindowVM =
      WindowVM(
        m.project.fold(messages(UntitledProject))(p => messages(WindowTitle, p.value)),
        m.save.edited
      )

    def appBar: AppBarVM =
      val project = m.project.fold(messages(UntitledProject))(_.value)
      AppBarVM(
        messages(AppName),
        project,
        messages(ProjectAccessible, project),
        Perspective.values.toVector.map { p =>
          val shortcut = CommandRegistry.shortcutText(CommandRegistry.forPerspective(p))
          PerspectiveButtonVM(
            p,
            labels.perspective(p),
            shortcut,
            m.perspective == p,
            Intent.SwitchPerspective(p),
            messages(PerspectiveAccessible, labels.perspective(p), shortcut)
          )
        },
        jobsChip
      )

    private def total(t: MeterTotal): String = t match
      case MeterTotal.Exact(n)  => Format.count(n)
      case MeterTotal.AtMost(n) => messages(JobAtMostTotal, Format.count(n))
      case MeterTotal.Counting  => messages(JobCountingTotal)

    /** done / total, only when the total is exact and positive. */
    private def fraction(meter: Meter): Option[Double] = meter.total match
      case MeterTotal.Exact(t) if t > 0 => Some(meter.done.toDouble / t.toDouble)
      case _                            => None

    private def cancel(job: ExecutionJob): ActionVM =
      ActionVM(
        messages(Cancel),
        CommandRegistry.cancelRun.intent(m).contains(Intent.CancelJob(job.id)),
        Intent.CancelJob(job.id),
        Some(messages(JobCancelAccessible, job.run.number.toString, job.id.number.toString))
      )

    private def failedTitle(job: ExecutionJob, diagnostics: Int): String =
      if diagnostics <= 0 then messages(JobFailed, job.run.number.toString)
      else
        messages(
          JobFailedWith,
          job.run.number.toString,
          labels.plural(diagnostics, DiagnosticsOne, DiagnosticsMany)
        )

    def jobsChip: JobsChipVM = m.jobs.headline match
      case JobHeadline.Idle =>
        JobsChipVM(
          JobsChipState.Idle,
          messages(JobsIdle),
          None,
          None,
          None,
          None,
          None,
          messages(JobsIdleAccessible)
        )
      case JobHeadline.Active(job) =>
        val title = labels.run(job.run)
        job.phase match
          case JobPhase.Queued =>
            val chip = JobsChipVM(
              JobsChipState.Queued,
              title,
              Some(messages(JobQueued)),
              None,
              None,
              Some(cancel(job)),
              None,
              ""
            )
            chip.copy(accessible = chip.text)
          case JobPhase.Running(progress) =>
            val stage = labels.stage(progress.stage)
            val pairs = progress.pairs
            val done  = Format.count(pairs.done)
            JobsChipVM(
              JobsChipState.Running,
              title,
              Some(stage),
              Some(messages(JobPairs, done, total(pairs.total))),
              fraction(pairs),
              Some(cancel(job)),
              None,
              messages(
                JobRunningAccessible,
                job.run.number.toString,
                stage.toLowerCase,
                done,
                total(pairs.total)
              )
            )
          case _ =>
            // Cancelling; a settled job is never the active one.
            val chip = JobsChipVM(
              JobsChipState.Cancelling,
              title,
              Some(messages(JobCancelling)),
              None,
              None,
              None,
              None,
              ""
            )
            chip.copy(accessible = chip.text)
      case JobHeadline.Failed(job, diagnostics) =>
        val title = failedTitle(job, diagnostics.size)
        JobsChipVM(
          JobsChipState.Failed,
          title,
          None,
          None,
          None,
          None,
          Some(Intent.OpenDiagnostics),
          title
        )
      case JobHeadline.Ready(notice) =>
        val run   = notice.run
        val title = messages(JobReady, run.number.toString)
        JobsChipVM(
          JobsChipState.Ready,
          title,
          None,
          None,
          None,
          None,
          Some(Intent.ShowRun(run)),
          title
        )

    // --- Context strip ---------------------------------------------------------

    /** In Data, the newest dataset revision not yet admitted, when it is newer
      * than every admitted one.
      */
    private def pendingDataset: Option[DatasetRevisionSpec] =
      val latest = doc.latestAdmitted.map(_.id.number).getOrElse(0)
      doc.datasets.lastOption.filter(d => !d.decision.isAdmitted && d.id.number > latest)

    private def badgeState(standing: RunStanding): String = messages(standing match
      case RunStanding.Current      => ToneCurrent
      case RunStanding.Stale(_)     => ToneStale
      case RunStanding.Running      => ToneRunning
      case RunStanding.Failed       => ToneFailed
      case RunStanding.Cancelled(_) => ToneCancelled)

    private def tone(t: BadgeTone): FreshnessTone = t match
      case BadgeTone.Current   => FreshnessTone.Current
      case BadgeTone.Stale     => FreshnessTone.Stale
      case BadgeTone.Running   => FreshnessTone.Running
      case BadgeTone.Failed    => FreshnessTone.Failed
      case BadgeTone.Cancelled => FreshnessTone.Cancelled
      case BadgeTone.NoRun     => FreshnessTone.NoRun

    /** The badge's words: the same grammar as S2.7's reference rendering, in
      * the catalogue, plus the empty project ("No dataset · no analysis").
      */
    def badge(b: Badge): FreshnessVM = b match
      case Badge.NoRun(None, None)     => FreshnessVM(messages(BadgeEmpty), FreshnessTone.NoRun)
      case Badge.NoRun(analysis, data) =>
        val parts = analysis.map(a => messages(BadgeAnalysisPart, a.label)).toVector ++
          Vector(messages(ToneNoRun)) ++ data.map(d => messages(BadgeDataPart, d.label))
        FreshnessVM(parts.mkString(messages(BadgePartSeparator)), FreshnessTone.NoRun)
      case Badge.Shown(run, standing, newer) =>
        val word = badgeState(standing)
        val hue  = (standing, newer) match
          case (RunStanding.Current, Some(_)) => FreshnessTone.Showing
          case _                              => tone(b.tone)
        val parts = (run.analysis.label, run.id.label, run.dataset.label)
        val text  = (standing, newer) match
          case (_, None) => messages(BadgeShown, parts._1, parts._2, parts._3, word)
          case (RunStanding.Current, Some(_)) =>
            messages(BadgeShowing, parts._1, parts._2, parts._3)
          case (_, Some(_)) => messages(BadgeShowingState, parts._1, parts._2, parts._3, word)
        FreshnessVM(text, hue)

    /** The chip beside the badge while a newer run runs (Results board):
      * "Rev 5 · run 8 running · 48%". The percentage is the pairs meter's
      * fraction, from the execution service's job, else from the session's
      * progress report; none while the total is not exact.
      */
    def newer(b: Badge): Option[FreshnessVM] = b match
      case Badge.Shown(_, _, Some(r)) =>
        val fromJob = m.jobs.jobs
          .filter(_.run == r.run)
          .flatMap(_.phase.lastReport)
          .lastOption
          .flatMap(p => fraction(p.pairs))
        val fromSession = r.meter.collect {
          case eyes4s.studio.core.freshness.RunMeter(_, done, ProgressTotal.Exact(t))
              if t > 0 =>
            done.toDouble / t.toDouble
        }
        val head = (r.revision.label.capitalize, r.run.label)
        val text = fromJob
          .orElse(fromSession)
          .fold(messages(BadgeNewer, head._1, head._2))(f =>
            messages(BadgeNewerPercent, head._1, head._2, Format.percent(f))
          )
        Some(FreshnessVM(text, FreshnessTone.Running))
      case _ => None

    /** A newer run that failed or was cancelled while an older one is shown
      * (S2.7: the badge stays current): "Run 8 failed · 2 diagnostics".
      */
    def ended: Option[FreshnessVM] = m.freshness.banner.collect {
      case Banner.NewerEnded(_, a @ RunActivity.Failed(_, _, _)) =>
        FreshnessVM(activity(a), FreshnessTone.Failed)
      case Banner.NewerEnded(_, a @ RunActivity.Cancelled(_, _, _)) =>
        FreshnessVM(activity(a), FreshnessTone.Cancelled)
    }

    private def pendingBadge(d: DatasetRevisionSpec): (FreshnessVM, Vector[String]) =
      val state = d.decision match
        case AdmissionDecision.Verifying(_) => messages(DatasetVerifying)
        case _                              => messages(DatasetPending)
      val notes = m.freshness.pending.filter(_.dataset == d.id).flatMap { p =>
        p.wouldStale.flatMap(doc.run).map { r =>
          messages(
            PendingNote,
            r.id.number.toString,
            r.analysis.label,
            r.dataset.label,
            p.dataset.label
          )
        }
      }
      (
        FreshnessVM(
          messages(BadgeDataset, d.id.label, state, messages(NoRunOnDataset, d.id.label)),
          FreshnessTone.PendingData
        ),
        notes
      )

    def draftChip: Option[DraftChipVM] = m.freshness.draft.map { chip =>
      val (readiness, blocked) = chip.readiness match
        case DraftReadiness.Unchecked   => (messages(DraftUnchecked), false)
        case DraftReadiness.Ready       => (messages(DraftReady), false)
        case DraftReadiness.Blocked(bs) =>
          (labels.plural(bs.length, BlockersOne, BlockersMany), true)
      val text = messages(
        DraftChip,
        chip.draft.id.label,
        labels.plural(chip.changes, ChangesOne, ChangesMany),
        readiness
      )
      DraftChipVM(text, blocked, Intent.ReviewDraft, messages(DraftChipAccessible, text))
    }

    def context: ContextStripVM =
      val trail              = m.location.trail
      val last               = trail.size - 1
      val (freshness, notes) =
        pendingDataset
          .filter(_ => m.perspective == Perspective.Data)
          .fold((badge(m.freshness.badge), Vector.empty[String]))(pendingBadge)
      ContextStripVM(
        ActionVM(
          messages(Back, CommandRegistry.shortcutText(CommandRegistry.back)),
          m.navigation.canGoBack,
          Intent.Back
        ),
        ActionVM(
          messages(Forward, CommandRegistry.shortcutText(CommandRegistry.forward)),
          m.navigation.canGoForward,
          Intent.Forward
        ),
        trail.zipWithIndex.map { (place, i) =>
          val label      = labels.place(place, current = i == last)
          val opens      = Shell.crumbTarget(m, i).map(_.perspective).filter(_ != m.perspective)
          val accessible =
            if i == last then messages(CrumbCurrentAccessible, label)
            else
              opens.fold(label)(p =>
                messages(CrumbOpensAccessible, label, labels.perspective(p))
              )
          CrumbVM(label, i == last, Intent.OpenCrumb(i), opens, accessible)
        },
        freshness,
        if pendingDataset.exists(_ => m.perspective == Perspective.Data) then None
        else newer(m.freshness.badge).orElse(ended),
        notes,
        // Explore is view-only: the boards show no draft chip there.
        draftChip.filter(_ => m.perspective != Perspective.Explore)
      )

    // --- Banner ------------------------------------------------------------------

    /** "adds σ 8°" when scales were only added; the changes otherwise. */
    private def describe(changes: Vector[RecipeChange]): String = changes match
      case Vector(RecipeChange.Scales(before, after)) if before.values.forall(after.contains) =>
        messages(
          ChangeAdds,
          after.values.filterNot(before.contains).map(_.render).mkString(", ")
        )
      case _ => changes.map(_.render).mkString(messages(ClauseSeparator))

    private def activity(a: RunActivity): String = a match
      case RunActivity.Running(run, _, _)          => labels.run(run)
      case RunActivity.Failed(run, _, diagnostics) =>
        diagnostics
          .filter(_ > 0)
          .fold(messages(JobFailed, run.number.toString))(n =>
            messages(
              JobFailedWith,
              run.number.toString,
              labels.plural(n, DiagnosticsOne, DiagnosticsMany)
            )
          )
      case RunActivity.Cancelled(run, _, at) =>
        at.fold(messages(JobCancelled, run.number.toString))(k =>
          messages(JobCancelledAt, run.number.toString, labels.stage(k))
        )

    /** The figure the Figures trail is on, if any. */
    private def openFigure = m.location.trail.reverseIterator.collectFirst {
      case Place.Figure(f)                       => f
      case Place.At(StudioRef.FigurePanel(f, _)) => f
    }

    /** Show is enabled exactly when the shelf would show `run`. */
    private def showAction(run: RunId): ActionVM =
      val finished = m.jobs.show(run).isRight
      ActionVM(
        messages(if finished then ShowRun else ShowRunWhenFinished, run.number.toString),
        finished,
        Intent.ShowRun(run)
      )

    def banner: Option[DraftBannerVM] = m.freshness.bannerFor(m.perspective).map {
      case Banner.NoRun(_) =>
        DraftBannerVM(
          messages(BannerNoRun),
          "",
          Vector(
            ActionVM(
              messages(OpenAnalysis),
              true,
              Intent.SwitchPerspective(Perspective.Analysis)
            )
          )
        )
      case Banner.DraftNotRun(shown, draft, diff, data) =>
        val figure = openFigure
          .filter(_ => m.perspective == Perspective.Figures)
          .flatMap(f => doc.figures.find(_.id == f))
          .flatMap(f => doc.run(f.run).map(r => (f.id, r)))
        val lead = figure.fold(messages(BannerShowing, shown.id.label, shown.analysis.label)) {
          (f, r) =>
            messages(BannerFigureShows, f.label, r.id.label, r.analysis.label)
        }
        val what = (data.map(d => messages(ChangeMovesData, d.label)).toVector ++
          Option.when(diff.nonEmpty)(describe(diff))).mkString(messages(ClauseSeparator))
        val clause = messages(BannerDraftNotRun, draft.id.label, what)
        val detail =
          if m.perspective == Perspective.Figures then
            s"$clause ${messages(BannerFiguresNeverFollow)}"
          else clause
        DraftBannerVM(
          lead,
          detail,
          Vector(
            ActionVM(messages(ReviewInAnalysis), true, Intent.ReviewDraft),
            ActionVM(messages(DiscardDraft), true, Intent.RequestDiscardDraft)
          )
        )
      case Banner.NewerRunning(shown, running, diff) =>
        val detail =
          if diff.isEmpty then
            messages(BannerNewerRunning, running.run.number.toString, running.revision.label)
          else
            messages(
              BannerNewerRunningChanges,
              running.run.number.toString,
              running.revision.label,
              describe(diff)
            )
        DraftBannerVM(
          messages(BannerShowingBrief, shown.id.label, shown.analysis.label),
          detail,
          Vector(showAction(running.run))
        )
      case Banner.NewerEnded(shown, ended) =>
        DraftBannerVM(
          messages(BannerShowing, shown.id.label, shown.analysis.label),
          activity(ended),
          Vector.empty
        )
      case Banner.NewerCompleted(shown, newer, _) =>
        DraftBannerVM(
          messages(BannerShowing, shown.id.label, shown.analysis.label),
          messages(BannerNewerCompleted, newer.id.number.toString, newer.analysis.label),
          Vector(showAction(newer.id))
        )
      case Banner.ShownStale(shown, reasons) =>
        val detail = reasons.head match
          case StaleReason.Superseded(_, _) =>
            messages(BannerStaleSuperseded, shown.id.label, shown.analysis.label)
          case StaleReason.DatasetMoved(data, latest) =>
            messages(BannerStaleDataset, shown.id.label, data.label, latest.label)
        DraftBannerVM(
          messages(
            BannerShowingStale,
            shown.id.label,
            shown.analysis.label,
            shown.dataset.label
          ),
          detail,
          Vector.empty
        )
    }

    // --- Confirmation ------------------------------------------------------------

    /** Discard draft's question, while it is pending: "Discard draft rev 5
      * and its 1 change? Runs are not affected."
      */
    def confirmation: Option[ConfirmationVM] = m.pending.map {
      case Confirmation.DiscardDraft(draft) =>
        val changes = m.freshness.draft.filter(_.draft.id == draft).fold(0)(_.changes)
        ConfirmationVM(
          messages(
            ConfirmDiscardDraft,
            draft.label,
            labels.plural(changes, ChangesOne, ChangesMany)
          ),
          ActionVM(messages(DiscardDraft), true, Intent.Confirm),
          ActionVM(messages(KeepDraft), true, Intent.Dismiss)
        )
    }

    // --- Status bar --------------------------------------------------------------

    def statusJob: StatusJobVM =
      val chip = jobsChip
      m.jobs.headline match
        case JobHeadline.Active(ExecutionJob(_, _, _, JobPhase.Running(progress))) =>
          val pairs = progress.pairs
          val count = messages(JobCount, Format.count(pairs.done), total(pairs.total))
          StatusJobVM(
            (Vector(chip.title) ++ chip.stage).mkString(" · "),
            Some(count),
            chip.action
          )
        case JobHeadline.Active(_)     => StatusJobVM(chip.text, None, chip.action)
        case JobHeadline.Ready(notice) =>
          StatusJobVM(chip.text, None, Some(showAction(notice.run)))
        case _ => StatusJobVM(chip.text, None, None)

    /** The selected slot shows the focused pane's own subject when it has
      * reported one (a form field: "Draft rev 5 › Scales"), else the shared
      * selection, else "No selection".
      */
    def status: StatusBarVM =
      val refs     = m.selection.selected
      val selected = refs.lastOption.map { ref =>
        val path = labels.selected(ref)
        if refs.size > 1 then messages(SelectedMore, path, (refs.size - 1).toString) else path
      }
      val subject = m.panes.subjects.get(m.focusedPane).filter(_.nonEmpty).map(labels.path)
      val layout  = m.layout
      val hint    = pendingDataset match
        case Some(d) if layout.hint == HintDataVerify => messages(HintDataPending, d.id.label)
        case _                                        => messages(layout.hint)
      StatusBarVM(
        messages(SelectedLabel),
        subject.orElse(selected),
        messages(NoSelection),
        hint,
        statusJob,
        m.save.last.fold(messages(NotSaved))(t => messages(Saved, t.render))
      )
