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

import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.app.jobs.{JobHeadline, JobPhase, JobSummary, MeterTotal}
import eyes4s.studio.app.keys.CommandRegistry
import eyes4s.studio.app.nav.Place
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
    ShellVM(p.window, p.appBar, p.context, p.banner, p.status)

  def appBar(model: AppModel, messages: Messages = Messages.english): AppBarVM =
    Projection(model, messages).appBar

  def context(model: AppModel, messages: Messages = Messages.english): ContextStripVM =
    Projection(model, messages).context

  def banner(model: AppModel, messages: Messages = Messages.english): Option[DraftBannerVM] =
    Projection(model, messages).banner

  def status(model: AppModel, messages: Messages = Messages.english): StatusBarVM =
    Projection(model, messages).status

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
      AppBarVM(
        messages(AppName),
        m.project.fold(messages(UntitledProject))(_.value),
        Perspective.values.toVector.map { p =>
          PerspectiveButtonVM(
            p,
            labels.perspective(p),
            CommandRegistry.shortcutText(CommandRegistry.forPerspective(p)),
            m.perspective == p,
            Intent.SwitchPerspective(p)
          )
        },
        jobsChip
      )

    private def total(t: MeterTotal): String = t match
      case MeterTotal.Exact(n)  => Format.count(n)
      case MeterTotal.AtMost(n) => messages(JobAtMostTotal, Format.count(n))
      case MeterTotal.Counting  => messages(JobCountingTotal)

    private def cancel(job: JobSummary): ActionVM =
      ActionVM(
        messages(Cancel),
        CommandRegistry.cancelRun.intent(m).contains(Intent.CancelJob(job.run)),
        Intent.CancelJob(job.run)
      )

    private def failedTitle(job: JobSummary, diagnostics: Int): String =
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
          case JobPhase.Cancelling =>
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
          case _ =>
            job.meter match
              case None =>
                val chip = JobsChipVM(
                  JobsChipState.Running,
                  title,
                  Some(messages(JobRunningUnmetered)),
                  None,
                  None,
                  Some(cancel(job)),
                  None,
                  ""
                )
                chip.copy(accessible = chip.text)
              case Some(meter) =>
                val stage = labels.stage(meter.stage)
                val done  = Format.count(meter.done)
                JobsChipVM(
                  JobsChipState.Running,
                  title,
                  Some(stage),
                  Some(messages(JobPairs, done, total(meter.total))),
                  meter.fraction,
                  Some(cancel(job)),
                  None,
                  messages(
                    JobRunningAccessible,
                    job.run.number.toString,
                    stage.toLowerCase,
                    done,
                    total(meter.total)
                  )
                )
      case JobHeadline.Failed(job, diagnostics) =>
        val title = failedTitle(job, diagnostics)
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
      case JobHeadline.Ready(run) =>
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
        val word    = badgeState(standing)
        val running = newer.map(r => messages(BadgeRunningNewer, r.revision.label))
        val state   = (standing, running) match
          case (RunStanding.Current, Some(r)) => r
          case (_, Some(r))                   => messages(BadgeStateWithNewer, word, r)
          case (_, None)                      => word
        FreshnessVM(
          messages(BadgeShown, run.analysis.label, run.id.label, run.dataset.label, state),
          tone(b.tone)
        )

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
      DraftChipVM(
        messages(
          DraftChip,
          chip.draft.id.label,
          labels.plural(chip.changes, ChangesOne, ChangesMany),
          readiness
        ),
        blocked,
        Intent.ReviewDraft
      )
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
          CrumbVM(labels.place(place, current = i == last), i == last, Intent.OpenCrumb(i))
        },
        freshness,
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

    private def showAction(run: eyes4s.studio.core.backend.RunId, finished: Boolean): ActionVM =
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
          Vector(showAction(running.run, m.jobs.isReady(running.run)))
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
          Vector(showAction(newer.id, finished = true))
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

    // --- Status bar --------------------------------------------------------------

    def statusJob: StatusJobVM =
      val chip = jobsChip
      m.jobs.headline match
        case JobHeadline.Active(job) if job.phase != JobPhase.Cancelling =>
          val count = job.meter.map(meter =>
            messages(JobCount, Format.count(meter.done), total(meter.total))
          )
          val text = if count.isDefined then (Vector(chip.title) ++ chip.stage).mkString(" · ")
          else chip.text
          StatusJobVM(text, count, chip.action)
        case JobHeadline.Ready(run) =>
          StatusJobVM(chip.text, None, Some(showAction(run, finished = true)))
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
