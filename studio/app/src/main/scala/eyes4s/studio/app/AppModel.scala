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

package eyes4s.studio.app

import eyes4s.codec.CanonicalDigest
import eyes4s.studio.app.jobs.JobBoard
import eyes4s.studio.app.keys.{CommandId, CommandRegistry, KeyChord}
import eyes4s.studio.app.layout.{LayoutId, PaneId, PerspectiveLayout, StudioLayouts}
import eyes4s.studio.app.nav.{Location, Navigation, Place}
import eyes4s.studio.app.text.{Format, MessageId, Messages}
import eyes4s.studio.core.backend.{AnalysisRevision, DatasetRevision, JobId, RunId, TrialKey}
import eyes4s.studio.core.execution.{
  ExecutionEffect,
  ExecutionError,
  ExecutionEvent,
  ExecutionJob,
  RunStamp
}
import eyes4s.studio.core.command.{
  Command,
  CommandError,
  Effect,
  History,
  HistoryStack,
  JournalEntry,
  Step
}
import eyes4s.studio.core.document.{
  CoreBinding,
  DatasetRevisionSpec,
  DocumentError,
  LayoutBlob,
  Perspective,
  PresentationState,
  StudioDocument
}
import eyes4s.studio.core.freshness.{Freshness, SessionFacts}
import eyes4s.studio.core.selection.{
  SelectionError,
  SelectionInput,
  SelectionState,
  StudioRef,
  ViewId
}

// ---------------------------------------------------------------------------
// Small values
// ---------------------------------------------------------------------------

/** Why an app value was refused; each case names its operands. */
enum AppError derives CanEqual:
  case BlankProjectName
  case BadClock(hour: Int, minute: Int)

  def message: String = this match
    case BlankProjectName => "A project name is blank."
    case BadClock(h, m)   => s"$h:$m is not a time of day (hours 0–23, minutes 0–59)."

/** The project's name as the app bar and window title show it ("memory-study"). */
final case class ProjectName private (value: String) derives CanEqual

object ProjectName:
  def of(value: String): Either[AppError, ProjectName] =
    Either.cond(value.trim.nonEmpty, new ProjectName(value.trim), AppError.BlankProjectName)

/** A local wall-clock time, supplied by the platform clock ("Saved 10:24"). */
final case class ClockTime private (hour: Int, minute: Int) derives CanEqual:
  def render: String = Format.clock(hour, minute)

object ClockTime:
  def of(hour: Int, minute: Int): Either[AppError, ClockTime] =
    Either.cond(
      hour >= 0 && hour < 24 && minute >= 0 && minute < 60,
      new ClockTime(hour, minute),
      AppError.BadClock(hour, minute)
    )

/** The last completed save, and whether the document changed since. */
final case class SaveState(last: Option[ClockTime], edited: Boolean) derives CanEqual:
  def edit: SaveState = copy(edited = true)

object SaveState:
  val never: SaveState = SaveState(None, edited = false)

/** Each trial's match item ("beach-042"), as the backend's ledger reports it.
  * Labels only: a trial with no item is shown by its trial id.
  */
final case class TrialItems(byTrial: Map[TrialKey, String]) derives CanEqual:
  def item(key: TrialKey): Option[String] = byTrial.get(key)

object TrialItems:
  val empty: TrialItems = TrialItems(Map.empty)

/** One view's hover. Hover is local (S3.3): it never reaches the selection. */
final case class HoverAt(view: ViewId, target: StudioRef) derives CanEqual

/** A destructive intent awaiting the user's confirmation. */
enum Confirmation derives CanEqual:
  case DiscardDraft(draft: AnalysisRevision)

/** Something the shell should tell the user once; Dismiss clears it. */
enum Notice derives CanEqual:
  /** A document command, undo or redo was refused; nothing changed. */
  case Refused(entry: JournalEntry, error: CommandError)
  case SelectionRefused(error: SelectionError)

  /** A command was invoked while disabled (or is not registered);
    * `reason` is the history's own refusal when it has one (undo at a
    * barrier: S2.2's UndoBlocked).
    */
  case Unavailable(command: CommandId, reason: Option[CommandError])

  /** The execution service's rules refused a run action (Show before the
    * run is ready: NotReady).
    */
  case ExecutionRefused(error: ExecutionError)

  /** A confirmation no longer applies (the draft it named has gone). */
  case Outdated(confirmation: Confirmation)

  /** A value the user typed was refused (a blank project name). */
  case Invalid(error: AppError)

  /** The saved layouts of `perspectives` could not be read; they show their
    * default arrangement instead (S1.5a).
    */
  case LayoutsReset(perspectives: Vector[Perspective], reason: String)

  def message: String = message(Messages.english)

  /** The notice's words; a command is named by its label ("Undo"). */
  def message(messages: Messages): String = this match
    case Refused(_, error)   => error.message
    case SelectionRefused(e) => e.message
    case ExecutionRefused(e) => e.message
    case Unavailable(c, why) =>
      val label = CommandRegistry
        .find(c)
        .fold(c.value)(cmd => messages(cmd.label, CommandRegistry.shortcutText(cmd)))
      why.fold(messages(MessageId.NoticeUnavailable, label))(e =>
        messages(MessageId.NoticeBlocked, label, e.message)
      )
    case Invalid(e)               => e.message
    case LayoutsReset(ps, reason) =>
      messages(MessageId.NoticeLayoutsReset, ps.map(_.label).mkString(", "), reason)
    case Outdated(Confirmation.DiscardDraft(d)) =>
      s"Draft ${d.label} is no longer the draft; nothing was discarded."

/** Which pane has focus in each layout, which layouts are maximized, and the
  * sub-selection each pane reports for the status bar ("Draft rev 5 ›
  * Scales"): a pane-local focus that is not a shared selection.
  */
final case class PaneState(
    focus: Map[LayoutId, PaneId],
    maximized: Set[LayoutId],
    subjects: Map[PaneId, Vector[Place]]
) derives CanEqual

object PaneState:
  val empty: PaneState = PaneState(Map.empty, Set.empty, Map.empty)

// ---------------------------------------------------------------------------
// Intents and effects
// ---------------------------------------------------------------------------

/** A dialog only the platform can show. */
/** A docking gesture only the dock can carry out: it moves focus, and the
  * dock reports the focus back as [[Intent.FocusPane]].
  */
enum DockCommand derives CanEqual:
  case NextTab, PreviousTab

enum PlatformDialog derives CanEqual:
  case ImportSources, OpenProject

  /** Ask for the project's new name; the answer is [[Intent.RenameProject]]. */
  case RenameProject

  /** The project's location, size and format version (S1.4). */
  case ProjectInfo

/** What the application must do after an update: data, performed by the
  * shell and services, never by [[AppModel.update]] (DESIGN_SPEC section 13).
  */
enum AppEffect derives CanEqual:
  /** Submit, cancel or require a run on the execution service (S3.1):
    * Save & run submits its stamp; a changed requirement without a
    * submission is `Require`.
    */
  case Execution(effect: ExecutionEffect)

  /** Verify `dataset`; `content` is the digest a later Admit must carry. */
  case RequestAdmission(dataset: DatasetRevision, content: CanonicalDigest[DatasetRevisionSpec])

  /** The document changed; schedule a save (S2.4a/b). */
  case Persist

  /** Append one entry to the autosave journal (S2.4b). */
  case Journal(entry: JournalEntry)

  case OpenDialog(dialog: PlatformDialog)

  /** Show the project bundle in the platform's file browser (S1.4). */
  case RevealProject

  /** Return every layout of `perspective` to its default arrangement
    * (View › Reset perspective, S1.5a); the document's saved layout is
    * cleared by the same update.
    */
  case ResetLayouts(perspective: Perspective)

  /** Ctrl+Tab, Ctrl+Shift+Tab: the dock's own tab cycling. */
  case Dock(command: DockCommand)

object AppEffect:
  /** A command effect of studio-core as an app effect, against the document
    * the command produced.
    */
  def of(effect: Effect, document: StudioDocument): AppEffect = effect match
    case Effect.RequestRun(_, analysis, dataset) =>
      Execution(ExecutionEffect.Submit(AppModel.stampOf(document, analysis, dataset)))
    case Effect.RequestAdmission(dataset, content) => RequestAdmission(dataset, content)
    case Effect.CancelJob(_, job)                  => Execution(ExecutionEffect.Cancel(job))
    case Effect.Persist                            => Persist

/** A user action or a service fact the shell dispatches (DESIGN_SPEC
  * section 13). Hover and selection are intents, never document commands.
  * Slider drags are coalesced by the view, which dispatches on release.
  */
enum Intent derives CanEqual:
  // --- Navigation -------------------------------------------------------------
  /** ⌘1–⌘5: the perspective at the trail it last showed. */
  case SwitchPerspective(perspective: Perspective)
  case Navigate(to: Location)

  /** A crumb of the current trail, by position; it may cross perspectives. */
  case OpenCrumb(index: Int)
  case Back
  case Forward

  // --- Selection and hover (S3.3) ------------------------------------------------
  case Select(input: SelectionInput)
  case HoverOver(view: ViewId, target: Option[StudioRef])

  // --- The document (S2.2) ----------------------------------------------------------
  case Dispatch(command: Command)
  case Undo(stack: HistoryStack)
  case Redo(stack: HistoryStack)

  // --- Jobs and runs --------------------------------------------------------------------
  case CancelJob(job: JobId)

  /** Show a finished run through the shelf (`RunShelf.show`): Compare and
    * the banner move to it. Refused (NotReady) unless it is the pending run.
    */
  case ShowRun(run: RunId)

  /** Put the ready notice away without showing its run. */
  case DismissReady(run: RunId)
  case ReviewDraft
  case RequestDiscardDraft

  /** The failed jobs chip: Analysis, focused on the Diagnostics pane. */
  case OpenDiagnostics
  case RequestImport

  // --- Confirmations and notices ------------------------------------------------------------
  case Confirm
  case Dismiss

  // --- Commands, keys and panes ---------------------------------------------------------------
  case Invoke(command: CommandId)
  case KeyPressed(chord: KeyChord)
  case FocusPane(pane: PaneId)

  /** F6. */
  case FocusNextPane

  /** ⇧F6. */
  case FocusPreviousPane

  /** ⌃⇥ and ⌃⇧⇥: the next or previous tab of the focused group. */
  case NextTab
  case PreviousTab

  /** ⌘⇧↩. */
  case ToggleMaximize

  /** The dock itself (a group's header button) maximized the group showing
    * `pane`, which takes focus, or, with `None`, restored the layout; the
    * model follows in one step.
    */
  case SetMaximized(pane: Option[PaneId])

  /** A pane reports its local focus for the status bar, and takes focus. */
  case PaneSubject(pane: PaneId, subject: Vector[Place])

  // --- Service facts ---------------------------------------------------------------------------
  /** One event of the execution service (S3.1), in publication order. */
  case Execution(event: ExecutionEvent)

  /** A snapshot of the execution service's jobs (on attach). */
  case JobsChanged(jobs: Vector[ExecutionJob])

  /** Progress, outcomes and the latest draft check (freshness inputs). */
  case SessionChanged(facts: SessionFacts)
  case ItemsLoaded(items: TrialItems)
  case Saved(at: ClockTime)

  // --- Project and layouts (S1.4, S1.5a) -------------------------------------------------------
  /** The project chip's Rename…: the platform asks for the name. */
  case RequestRename
  case RenameProject(name: ProjectName)

  /** The name typed for Rename… was refused. */
  case RenameRefused(error: AppError)

  /** The document's saved layouts of `perspectives` could not be read. */
  case LayoutsUnreadable(perspectives: Vector[Perspective], reason: String)
  case RevealProject
  case ShowProjectInfo

  /** View › Reset perspective: the current perspective's layouts. */
  case ResetPerspective

  /** The shell's current arrangement of each perspective, `None` where it
    * is the default. Only those that differ from the document's are saved
    * (a view-only [[Command.SaveLayout]] each).
    */
  case LayoutsCaptured(layouts: Vector[(Perspective, Option[LayoutBlob])])

// ---------------------------------------------------------------------------
// The model
// ---------------------------------------------------------------------------

/** The UI-neutral application model every view-model is derived from
  * (ticket S1.0).
  *
  * The active perspective is the document's (a view-only command), so it
  * saves and undoes with the presentation; [[navigation]] keeps the trail
  * of each perspective and the back/forward history.
  */
final case class AppModel private (
    project: Option[ProjectName],
    history: History,
    session: SessionFacts,
    jobs: JobBoard,
    navigation: Navigation,
    selection: SelectionState,
    hover: Option[HoverAt],
    items: TrialItems,
    panes: PaneState,
    pending: Option[Confirmation],
    notice: Option[Notice],
    save: SaveState
) derives CanEqual:

  def document: StudioDocument = history.document
  def perspective: Perspective = document.presentation.perspective
  def location: Location       = navigation.at(perspective)

  /** The derived freshness (S2.7). */
  lazy val freshness: Freshness = Freshness.of(document, session)

  /** The layout the current perspective shows. */
  def layout: PerspectiveLayout =
    StudioLayouts.layoutFor(perspective, location.trail, document.datasets.nonEmpty)

  /** The focused pane of the current layout: the one last focused, else the
    * first non-navigator group's first tab.
    */
  def focusedPane: PaneId =
    val l = layout
    panes.focus
      .get(l.id)
      .filter(p => l.pane(p).isDefined)
      .getOrElse(
        l.groups
          .find(!_.navigator)
          .orElse(l.groups.headOption)
          .map(l.selectedPane)
          .fold(l.panes.head.id)(_.id)
      )

  def isMaximized: Boolean = panes.maximized.contains(layout.id)

object AppModel:

  private val none: Vector[AppEffect] = Vector.empty

  /** The trail each perspective starts at. */
  def rootTrails(document: StudioDocument): Map[Perspective, Vector[Place]] =
    Map(
      Perspective.Data -> document.datasets.lastOption.fold(Vector(Place.NewProject))(d =>
        Vector(Place.Dataset(d.id))
      ),
      Perspective.Explore  -> Vector.empty,
      Perspective.Analysis -> Vector(Place.Analyses),
      Perspective.Compare  -> document.reporting.headOption
        .map(r => Place.Summary(r.id))
        .toVector,
      Perspective.Figures -> Vector(Place.Figures)
    )

  /** A new, untitled project with no dataset and no analysis (the DataEmpty
    * board).
    */
  def newProject: Either[DocumentError, AppModel] =
    StudioDocument
      .of(
        Vector.empty,
        Vector.empty,
        None,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        PresentationState.default,
        Vector.empty
      )
      .map(open(_, None))

  /** A freshly opened project: empty histories, no session facts, nothing
    * selected, never saved in this session.
    */
  def open(document: StudioDocument, project: Option[ProjectName]): AppModel =
    AppModel(
      project,
      History.start(document),
      SessionFacts.empty,
      requestedStamp(document).foldLeft(JobBoard.empty(document.presentation.shownRun))(
        _.require(_)
      ),
      Navigation.start(rootTrails(document)),
      SelectionState.empty,
      None,
      TrialItems.empty,
      PaneState.empty,
      None,
      None,
      SaveState.never
    )

  /** The Analysis trail of the current draft, or of the latest revision. */
  def draftTrail(document: StudioDocument): Vector[Place] =
    val revision = document.draft.map(_.id).orElse(document.latestAnalysis.map(_.id))
    val base     =
      document.draft.flatMap(d => document.analysis(d.base)).orElse(document.latestAnalysis)
    Vector(Place.Analyses) ++ base.map(b => Place.Lineage(b.studio.preset)) ++
      revision.map(Place.Revision(_))

  /** Apply intents in order, collecting their effects. */
  def run(model: AppModel, intents: Iterable[Intent]): (AppModel, Vector[AppEffect]) =
    intents.foldLeft((model, none)) { case ((m, effects), i) =>
      val (next, more) = update(m, i)
      (next, effects ++ more)
    }

  /** The Elm-style update: pure and total. A refused command, undo or
    * selection changes nothing but the notice and emits no effect.
    */
  def update(m: AppModel, intent: Intent): (AppModel, Vector[AppEffect]) = intent match
    case Intent.SwitchPerspective(p) => navigate(m, m.navigation.at(p))
    case Intent.Navigate(to)         => navigate(m, to)
    case Intent.OpenCrumb(index)     =>
      val trail = m.location.trail
      if index < 0 || index >= trail.size then (m, none)
      else
        val prefix = trail.take(index + 1)
        navigate(m, Location(Place.home(prefix).getOrElse(m.perspective), prefix))
    case Intent.Back =>
      m.navigation
        .goBack(m.location)
        .fold((m, none))((to, nav) => arrive(m, m.copy(navigation = nav), to))
    case Intent.Forward =>
      m.navigation
        .goForward(m.location)
        .fold((m, none))((to, nav) => arrive(m, m.copy(navigation = nav), to))

    case Intent.Select(input) =>
      m.selection
        .submit(input)
        .fold(
          e => (m.copy(notice = Some(Notice.SelectionRefused(e))), none),
          s => (m.copy(selection = s), none)
        )
    case Intent.HoverOver(view, target) =>
      val next = target match
        case Some(ref) => Some(HoverAt(view, ref))
        case None      => m.hover.filterNot(_.view == view)
      (m.copy(hover = next), none)

    case Intent.Dispatch(command) =>
      applyHistory(m, JournalEntry.Apply(command), m.history.apply(command))
    case Intent.Undo(stack) => applyHistory(m, undoEntry(stack), m.history.undoOn(stack))
    case Intent.Redo(stack) => applyHistory(m, redoEntry(stack), m.history.redoOn(stack))

    case Intent.CancelJob(job) =>
      m.jobs.job(job) match
        case Some(j) => update(m, Intent.Dispatch(Command.CancelRun(j.run)))
        case None    =>
          val refused = ExecutionError.UnknownJob(job, m.jobs.jobs.map(_.id))
          (m.copy(notice = Some(Notice.ExecutionRefused(refused))), none)
    case Intent.ShowRun(run) =>
      m.jobs.show(run) match
        case Left(error)  => (m.copy(notice = Some(Notice.ExecutionRefused(error))), none)
        case Right(board) =>
          if m.document.presentation.shownRun.contains(run) then (m.copy(jobs = board), none)
          else
            val (next, effects) = update(m, Intent.Dispatch(Command.ShowRun(Some(run))))
            if next.document.presentation.shownRun.contains(run) then
              (next.copy(jobs = board), effects)
            else (next, effects)
    case Intent.DismissReady(run) => (m.copy(jobs = m.jobs.dismiss(run)), none)
    case Intent.ReviewDraft       =>
      navigate(m, Location(Perspective.Analysis, draftTrail(m.document)))
    case Intent.RequestDiscardDraft =>
      m.document.draft.fold((m, none))(d =>
        (m.copy(pending = Some(Confirmation.DiscardDraft(d.id))), none)
      )
    case Intent.OpenDiagnostics =>
      val (next, effects) = navigate(m, Location(Perspective.Analysis, draftTrail(m.document)))
      (focus(next, StudioLayouts.diagnostics), effects)
    case Intent.RequestImport => (m, Vector(AppEffect.OpenDialog(PlatformDialog.ImportSources)))

    case Intent.Confirm =>
      m.pending match
        case None                                       => (m, none)
        case Some(c @ Confirmation.DiscardDraft(draft)) =>
          val cleared = m.copy(pending = None)
          if m.document.draft.exists(_.id == draft) then
            update(cleared, Intent.Dispatch(Command.DiscardDraft))
          else (cleared.copy(notice = Some(Notice.Outdated(c))), none)
    case Intent.Dismiss => (m.copy(pending = None, notice = None), none)

    case Intent.Invoke(id) =>
      CommandRegistry
        .find(id)
        .flatMap(_.intent(m))
        .fold {
          val why = CommandRegistry.find(id).flatMap(_.reason(m))
          (m.copy(notice = Some(Notice.Unavailable(id, why))), none)
        }(update(m, _))
    case Intent.KeyPressed(chord) =>
      CommandRegistry.keymap.get(chord).fold((m, none))(id => update(m, Intent.Invoke(id)))
    case Intent.FocusPane(pane) =>
      if m.layout.pane(pane).isDefined then (focus(m, pane), none) else (m, none)
    case Intent.FocusNextPane      => (cycleGroup(m, +1), none)
    case Intent.FocusPreviousPane  => (cycleGroup(m, -1), none)
    case Intent.NextTab            => (m, Vector(AppEffect.Dock(DockCommand.NextTab)))
    case Intent.PreviousTab        => (m, Vector(AppEffect.Dock(DockCommand.PreviousTab)))
    case Intent.SetMaximized(pane) =>
      val id = m.layout.id
      pane match
        case None =>
          (m.copy(panes = m.panes.copy(maximized = m.panes.maximized - id)), none)
        case Some(p) if m.layout.pane(p).isDefined =>
          val focused = focus(m, p)
          (
            focused.copy(panes = focused.panes.copy(maximized = focused.panes.maximized + id)),
            none
          )
        case Some(_) => (m, none)
    case Intent.ToggleMaximize =>
      val id  = m.layout.id
      val max = m.panes.maximized
      (m.copy(panes = m.panes.copy(maximized = if max(id) then max - id else max + id)), none)
    case Intent.PaneSubject(pane, subject) =>
      if m.layout.pane(pane).isEmpty then (m, none)
      else
        val focused = focus(m, pane)
        (
          focused.copy(panes =
            focused.panes.copy(subjects = focused.panes.subjects.updated(pane, subject))
          ),
          none
        )

    case Intent.Execution(event)   => (m.copy(jobs = m.jobs.receive(event)), none)
    case Intent.JobsChanged(jobs)  => (m.copy(jobs = m.jobs.withJobs(jobs)), none)
    case Intent.SessionChanged(f)  => (m.copy(session = f), none)
    case Intent.ItemsLoaded(items) => (m.copy(items = items), none)
    case Intent.Saved(at)          => (m.copy(save = SaveState(Some(at), edited = false)), none)

    case Intent.RequestRename =>
      (m, Vector(AppEffect.OpenDialog(PlatformDialog.RenameProject)))
    case Intent.RenameProject(name) =>
      if m.project.contains(name) then (m, none)
      else (m.copy(project = Some(name), save = m.save.edit), Vector(AppEffect.Persist))
    case Intent.RenameRefused(e) => (m.copy(notice = Some(Notice.Invalid(e))), none)
    case Intent.LayoutsUnreadable(ps, reason) =>
      (m.copy(notice = Some(Notice.LayoutsReset(ps, reason))), none)
    case Intent.RevealProject =>
      (m, m.project.fold(none)(_ => Vector(AppEffect.RevealProject)))
    case Intent.ShowProjectInfo => (m, Vector(AppEffect.OpenDialog(PlatformDialog.ProjectInfo)))
    case Intent.ResetPerspective =>
      // The default arrangement: its default focus, nothing maximized.
      val p       = m.perspective
      val reset   = AppEffect.ResetLayouts(p)
      val layouts = StudioLayouts.spec.layouts(p).map(_.id).toSet
      val cleared = m.copy(panes =
        m.panes.copy(
          focus = m.panes.focus.filterNot((l, _) => layouts(l)),
          maximized = m.panes.maximized -- layouts
        )
      )
      if savedLayout(m, p).isEmpty then (cleared, Vector(reset))
      else
        val (next, effects) = update(cleared, Intent.Dispatch(Command.SaveLayout(p, None)))
        (next, effects :+ reset)
    case Intent.LayoutsCaptured(layouts) =>
      layouts
        .filter((p, blob) => savedLayout(m, p) != blob)
        .foldLeft((m, none)) { case ((acc, effects), (p, blob)) =>
          val (next, more) = update(acc, Intent.Dispatch(Command.SaveLayout(p, blob)))
          (next, effects ++ more)
        }

  // -------------------------------------------------------------------------

  /** Focus the first pane of the group `step` groups on (F6, ⇧F6), leaving
    * any maximize.
    */
  private def cycleGroup(m: AppModel, step: Int): AppModel =
    val l      = m.layout
    val groups = l.groups
    val n      = groups.size.max(1)
    val at     = groups.indexWhere(_.panes.exists(_.id == m.focusedPane))
    val next   = groups.lift(Math.floorMod(at + step, n)).map(l.selectedPane(_).id)
    val moved  = next.fold(m)(focus(m, _))
    moved.copy(panes = moved.panes.copy(maximized = moved.panes.maximized - l.id))

  /** The document's saved layout of `perspective`, if any. */
  def savedLayout(m: AppModel, perspective: Perspective): Option[LayoutBlob] =
    m.document.presentation.layouts.find(_.perspective == perspective).map(_.layout)

  private def undoEntry(stack: HistoryStack): JournalEntry = stack match
    case HistoryStack.Science      => JournalEntry.Undo
    case HistoryStack.Presentation => JournalEntry.UndoView

  private def redoEntry(stack: HistoryStack): JournalEntry = stack match
    case HistoryStack.Science      => JournalEntry.Redo
    case HistoryStack.Presentation => JournalEntry.RedoView

  private def focus(m: AppModel, pane: PaneId): AppModel =
    m.copy(panes = m.panes.copy(focus = m.panes.focus.updated(m.layout.id, pane)))

  /** One history step: the journal entry, then the command's effects. */
  private def applyHistory(
      m: AppModel,
      entry: JournalEntry,
      result: Either[CommandError, Step]
  ): (AppModel, Vector[AppEffect]) = result match
    case Left(error) => (m.copy(notice = Some(Notice.Refused(entry, error))), none)
    case Right(step) =>
      val edited  = step.effects.contains(Effect.Persist)
      val doc     = step.history.document
      val effects = step.effects.map(AppEffect.of(_, doc))
      val submits = effects.collect { case AppEffect.Execution(ExecutionEffect.Submit(s)) => s }
      // A requirement that changed without a submission (a plan bound to the
      // running revision) is told to the service as Require.
      val required = requestedStamp(doc)
      val require  = required
        .filter(s => submits.isEmpty && !requestedStamp(m.document).contains(s))
        .map(s => AppEffect.Execution(ExecutionEffect.Require(s)))
      val jobs = (submits ++ require.flatMap(_ => required)).foldLeft(m.jobs)(_.require(_))
      val next = m.copy(
        history = step.history,
        jobs = jobs,
        notice = None,
        save = if edited then m.save.edit else m.save
      )
      (rebased(m, next), (AppEffect.Journal(entry) +: effects) ++ require)

  /** The stamp of `analysis` on `dataset` as the document saves it. The
    * study input's digest is the backend's to report, so it stays unbound
    * here; no digest is invented.
    */
  def stampOf(
      document: StudioDocument,
      analysis: AnalysisRevision,
      dataset: DatasetRevision
  ): RunStamp =
    RunStamp(
      analysis,
      dataset,
      document.analysis(analysis).fold(CoreBinding.unbound)(_.plan),
      CoreBinding.unbound
    )

  /** What the document currently wants results for: the stamp of its newest
    * running run, if any.
    */
  def requestedStamp(document: StudioDocument): Option[RunStamp] =
    document.running.lastOption.map(r => stampOf(document, r.analysis, r.dataset))

  /** When the shown run changes, the selection moves to a new context and
    * keeps only refs that do not belong to another run; hover likewise.
    */
  private def rebased(before: AppModel, after: AppModel): AppModel =
    val shown = after.document.presentation.shownRun
    if before.document.presentation.shownRun == shown then after
    else
      def keep(ref: StudioRef) = runOf(ref).forall(r => shown.contains(r))
      after.copy(
        selection = after.selection.rebase(keep),
        hover = after.hover.filter(h => keep(h.target))
      )

  /** The run a ref belongs to, if it is a run result. */
  def runOf(ref: StudioRef): Option[RunId] = ref match
    case StudioRef.Result(run, _)                      => Some(run)
    case StudioRef.ParticipantSummary(run, _, _, _, _) => Some(run)
    case StudioRef.GroupCell(run, _, _, _)             => Some(run)
    case _                                             => None

  /** Record the move in the history, then arrive. */
  private def navigate(m: AppModel, to: Location): (AppModel, Vector[AppEffect]) =
    arrive(m, m.copy(navigation = m.navigation.go(m.location, to)), to)

  /** Switch to `to`'s perspective (a view-only command); if that is refused,
    * the model stays as it was before the move.
    */
  private def arrive(
      before: AppModel,
      moved: AppModel,
      to: Location
  ): (AppModel, Vector[AppEffect]) =
    if moved.perspective == to.perspective then (moved, none)
    else
      val command = Command.SetPerspective(to.perspective)
      moved.history.apply(command) match
        case Left(error) =>
          (before.copy(notice = Some(Notice.Refused(JournalEntry.Apply(command), error))), none)
        case Right(step) => applyHistory(moved, JournalEntry.Apply(command), Right(step))
