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
import eyes4s.studio.app.analysis.PresetPicker
import eyes4s.studio.app.appearance.{Appearance, AppearanceState}
import eyes4s.studio.app.jobs.JobBoard
import eyes4s.studio.app.keys.{CommandId, CommandRegistry, KeyChord}
import eyes4s.studio.app.layout.{LayoutId, PaneId, PerspectiveLayout, StudioLayouts}
import eyes4s.studio.app.nav.{Location, Navigation, Place, Provenance}
import eyes4s.studio.app.text.{Format, MessageId, Messages, SourcesText}
import eyes4s.studio.core.assets.{InputCheck, SourceBlock, SourceCheck}
import eyes4s.studio.core.bundle.InputStatus
import eyes4s.studio.core.backend.{
  AnalysisRevision,
  DatasetRevision,
  JobId,
  RunId,
  StudioDiagnostic,
  TrialKey
}
import eyes4s.studio.core.artifacts.NativeArtifactError
import eyes4s.studio.core.execution.{
  ExecutionEffect,
  ExecutionError,
  ExecutionEvent,
  ExecutionJob,
  JobPhase,
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
  Preset,
  Recipe,
  RunLifecycle,
  SourceRole,
  StudioDocument,
  Theme
}
import eyes4s.studio.core.freshness.{Freshness, SessionFacts}
import eyes4s.studio.core.preview.PreviewReady
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

/** Which edit a save covers: edits are numbered from 1 in the order the
  * model makes them, and each `Persist` carries the number of the edit that
  * asked for it.
  */
final case class EditMark(value: Long) derives CanEqual:
  def next: EditMark = EditMark(value + 1)

object EditMark:
  val none: EditMark = EditMark(0)

/** The last completed save, the newest edit, and the newest edit a completed
  * save covers. The project is edited while an edit is newer than every
  * save: a save that finishes after a later edit leaves it edited.
  */
final case class SaveState(last: Option[ClockTime], edits: EditMark, covered: EditMark)
    derives CanEqual:
  def edited: Boolean = edits.value > covered.value
  def edit: SaveState = copy(edits = edits.next)

  /** A save of everything up to `upTo` finished at `at`. A save older than
    * one already recorded (delivered late) changes nothing.
    */
  def saved(at: ClockTime, upTo: EditMark): SaveState =
    if upTo.value < covered.value then this else copy(last = Some(at), covered = upTo)

object SaveState:
  val never: SaveState = SaveState(None, EditMark.none, EditMark.none)

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

  /** Computation completed, but verified files or their binding were refused. */
  case ArtifactsRefused(job: JobId, run: RunId, diagnostic: StudioDiagnostic)

  /** A confirmation no longer applies (the draft it named has gone). */
  case Outdated(confirmation: Confirmation)

  /** A value the user typed was refused (a blank project name). */
  case Invalid(error: AppError)

  /** The saved layouts of `perspectives` could not be read; they show their
    * default arrangement instead (S1.5a).
    */
  case LayoutsReset(perspectives: Vector[Perspective], reason: String)

  /** The platform's save of the project failed; the last save stands. */
  case SaveFailed(reason: String)

  /** A Save & run was asked while another waits for its check (S2.5). */
  case RunWaiting

  /** Save & run was refused: the sources of `dataset` are not known to be
    * held as recorded (S2.5), and why.
    */
  case SourcesBlocked(dataset: DatasetRevision, block: SourceBlock)

  def message: String = message(Messages.english)

  /** The notice's words; a command is named by its label ("Undo"). */
  def message(messages: Messages): String = this match
    case Refused(_, error)                    => error.message
    case SelectionRefused(e)                  => e.message
    case ExecutionRefused(e)                  => e.message
    case ArtifactsRefused(_, run, diagnostic) =>
      s"${run.label.capitalize} completed, but its verified files could not be saved: ${diagnostic.message}"
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
    case SaveFailed(reason)             => messages(MessageId.NoticeSaveFailed, reason)
    case SourcesBlocked(dataset, block) => SourcesText.blocked(dataset, block)
    case RunWaiting => SourcesText(eyes4s.studio.app.text.SourcesTextId.RunWaiting)
    case Outdated(Confirmation.DiscardDraft(d)) =>
      s"Draft ${d.label} is no longer the draft; nothing was discarded."

/** The checks of the project's stored inputs (S2.5): each asked check has
  * a round, and its answer names it. `runAfter` is the Save & run waiting
  * for round `n` (or a later one) to be answered.
  */
final case class InputChecks(
    asked: Long,
    answered: Long,
    runAfter: Option[(Command.SaveAndRun, Long)]
) derives CanEqual:
  /** Whether a check newer than the last answer is outstanding. */
  def outstanding: Boolean = asked > answered

  /** Ask the next round. */
  def ask: (InputChecks, Long) = (copy(asked = asked + 1), asked + 1)

object InputChecks:
  val none: InputChecks = InputChecks(0L, 0L, None)

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

  /** The About box: the build and its components (S1.14). */
  case About

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

  /** The document changed with edit `edit`; save it (S2.4a/b). The save's
    * [[Intent.Saved]] names the same mark.
    */
  case Persist(edit: EditMark)

  /** Append one entry to the autosave journal (S2.4b). */
  case Journal(entry: JournalEntry)

  case OpenDialog(dialog: PlatformDialog)

  /** Show the project bundle in the platform's file browser (S1.4). */
  case RevealProject

  /** Check the project's stored inputs against their digests (S2.5): check
    * `round`; the answer is [[Intent.InputsChecked]] or
    * [[Intent.InputsCheckFailed]] naming the same round.
    */
  case CheckInputs(round: Long)

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
  def of(
      effect: Effect,
      document: StudioDocument,
      edit: EditMark,
      prepared: Option[PreparedDesign] = None
  ): AppEffect = effect match
    // A run of the design the resolved-design pane prepared submits that
    // prepared design itself (E2E-05); any other run submits its stamp.
    case Effect.RequestRun(run, analysis, dataset) =>
      val stamp = AppModel.stampOf(document, analysis, dataset)
      Execution(
        prepared
          .filter(_.prepares(document, stamp))
          .fold(ExecutionEffect.Submit(stamp))(p =>
            ExecutionEffect.SubmitPreview(p.ready, Some(run))
          )
      )
    case Effect.RequestAdmission(dataset, content) => RequestAdmission(dataset, content)
    case Effect.CancelJob(_, job)                  => Execution(ExecutionEffect.Cancel(job))
    case Effect.Persist                            => Persist(edit)

/** A backend preview counted to its ready receipt, with the recipe it was
  * prepared from (S7.5). A draft edit keeps the draft's revision, and so its
  * stamp, so the recipe is what tells a changed draft from the one previewed.
  */
final case class PreparedDesign(ready: PreviewReady, recipe: Recipe) derives CanEqual:
  /** Whether a run of `stamp` in `document` runs this design. */
  def prepares(document: StudioDocument, stamp: RunStamp): Boolean =
    val identity = ready.stamp == stamp ||
      (ready.recipe.contains(recipe) && ready.stamp.agreesWithDeclarations(stamp))
    identity && ready.recipe.forall(_ == recipe) &&
    document.analysis(stamp.revision).exists(_.recipe == recipe)

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

  /** Explain a number: land on `target` with the provenance trail that leads
    * to it, keeping the part of the current trail it descends from (S3.4).
    * The perspective follows the target (Place.home).
    */
  case Explain(target: Place)

  /** The trail follows the selection to `target` (S6.6): as [[Explain]],
    * but it replaces the current trail without a Back step, and only within
    * the current perspective (a follow never switches perspective).
    */
  case Follow(target: Place)

  // --- Selection and hover (S3.3) ------------------------------------------------
  case Select(input: SelectionInput)
  case HoverOver(view: ViewId, target: Option[StudioRef])

  // --- The document (S2.2) ----------------------------------------------------------
  case Dispatch(command: Command)

  /** Choose a recipe preset (S7.1): its declared fields only, as a draft. */
  case NewAnalysis
  case ChoosePreset(preset: Preset)
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

  /** Check the project's stored inputs against their digests (S2.5): a
    * window with a project asks this when it opens.
    */
  case CheckInputs

  /** Stop waiting: the Save & run waiting for its check is not run (S2.5). */
  case CancelWaitingRun

  /** Check `round` of the project's stored inputs found `statuses` (S2.5). */
  case InputsChecked(round: Long, statuses: Vector[InputStatus])

  /** Check `round` of the project's stored inputs failed; runs stay blocked. */
  case InputsCheckFailed(round: Long, reason: String)
  case ItemsLoaded(items: TrialItems)

  /** The resolved-design pane counted a backend preview to the end (S7.5): a
    * run of its stamp and recipe submits this receipt.
    */
  case DesignPrepared(design: PreparedDesign)

  /** The resolved-design pane left the design it prepared (another target, a
    * changed draft): no run submits that receipt any longer.
    */
  case DesignWithdrawn

  /** The execution service refused to submit a prepared design (the backend
    * no longer retains it, or it is stale). The exact run Save & run recorded
    * is marked failed; submitting again requires a current prepared design.
    */
  case PreparedRefused(
      ready: PreviewReady,
      error: ExecutionError,
      requestedRun: Option[RunId] = None
  )

  /** The project session finished an atomic save at `at` of every edit up
    * to `upTo` (the mark of the `Persist` it performed, S2.4a).
    */
  case Saved(at: ClockTime, upTo: EditMark)

  /** The project session's save failed; `reason` names what failed. */
  case SaveFailed(reason: String)

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

  /** Help › About Eyes Studio (S1.14). */
  case ShowAbout

  /** View › Reset perspective: the current perspective's layouts. */
  case ResetPerspective

  /** View › Appearance (S1.10): Light and Dark set the document's theme;
    * System follows the platform's.
    */
  case SetAppearance(appearance: Appearance)

  /** The platform's theme, as it reports it (at start and on each change). */
  case SystemTheme(theme: Theme)

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
    save: SaveState,
    prepared: Option[PreparedDesign],
    appearance: AppearanceState,
    inputs: InputCheck,
    checks: InputChecks
) derives CanEqual:

  def document: StudioDocument = history.document

  /** The theme the studio shows (S1.10): the platform's while View ›
    * Appearance › System is chosen, else the document's.
    */
  def theme: Theme             = appearance.effective(document.presentation.theme)
  def perspective: Perspective = document.presentation.perspective
  def location: Location       = navigation.at(perspective)

  /** The stored state of every dataset source, as last checked (S2.5). */
  lazy val sources: SourceCheck = SourceCheck.of(document, inputs, checks.outstanding)

  /** The dataset revision Save & run would run, and why it may not: the
    * draft's dataset, else its base's (the reducer's rule).
    */
  def runBlock: Option[(DatasetRevision, SourceBlock)] =
    for
      context <- document.draftContext
      target = context.dataset
      block <- sources.block(target)
    yield (target, block)

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
      SaveState.never,
      None,
      AppearanceState.initial,
      InputCheck.NoProject,
      InputChecks.none
    )

  /** The Analysis trail of the current draft, or of the latest revision. */
  def draftTrail(document: StudioDocument): Vector[Place] =
    val revision = document.draft.map(_.id).orElse(document.latestAnalysis.map(_.id))
    val studio   =
      document.draftContext.map(_.studio).orElse(document.latestAnalysis.map(_.studio))
    Vector(Place.Analyses) ++ studio.map(fields => Place.Lineage(fields.preset)) ++
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
  // The document's theme set to `theme` (a view-only SetTheme), unless it
  // already is: switching theme changes nothing else.
  private def followTheme(m: AppModel, theme: Theme): (AppModel, Vector[AppEffect]) =
    if m.document.presentation.theme == theme then (m, Vector.empty)
    else update(m, Intent.Dispatch(Command.SetTheme(theme)))

  def update(m: AppModel, intent: Intent): (AppModel, Vector[AppEffect]) =
    val (next, effects) = step(m, intent)
    // A change that adds sources (an import, a re-import, an undone discard)
    // checks the stored inputs again: the new files are not in the last check.
    val added  = sourcesOf(next.document) -- sourcesOf(m.document)
    val asking = effects.exists {
      case AppEffect.CheckInputs(_) => true
      case _                        => false
    }
    if added.isEmpty || asking || next.inputs == InputCheck.NoProject then (next, effects)
    else
      val (checks, round) = next.checks.ask
      (next.copy(checks = checks), effects :+ AppEffect.CheckInputs(round))

  /** Each source as SourceCheck matches it to an input: role, digest and
    * file name. The same bytes imported under another name are a new input.
    */
  private def sourcesOf(d: StudioDocument): Set[(SourceRole, eyes4s.codec.ByteDigest, String)] =
    d.datasets
      .flatMap(_.sources.entries.map(s => (s.role, s.bytes, s.path.value.split('/').last)))
      .toSet

  private def step(m: AppModel, intent: Intent): (AppModel, Vector[AppEffect]) = intent match
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
    case Intent.Explain(target) =>
      val trail = Provenance.explain(m.location.trail, target)
      navigate(m, Location(Place.home(trail).getOrElse(m.perspective), trail))
    case Intent.Follow(target) =>
      val trail = Provenance.explain(m.location.trail, target)
      if Place.home(trail).getOrElse(m.perspective) != m.perspective then (m, none)
      else (m.copy(navigation = m.navigation.replace(Location(m.perspective, trail))), none)

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

    case Intent.Dispatch(run: Command.SaveAndRun) if m.inputs != InputCheck.NoProject =>
      // Every Save & run of a project checks its stored inputs first, then
      // runs only if they are present as recorded (S2.5; fail closed).
      // A second one while the first waits is told so, not dropped silently.
      if m.checks.runAfter.isDefined then (m.copy(notice = Some(Notice.RunWaiting)), none)
      else
        val (checks, round) = m.checks.ask
        (
          m.copy(checks = checks.copy(runAfter = Some((run, round)))),
          Vector(AppEffect.CheckInputs(round))
        )
    case Intent.Dispatch(command) =>
      applyHistory(m, JournalEntry.Apply(command), m.history.apply(command))
    case Intent.NewAnalysis =>
      eyes4s.studio.app.analysis.AnalysesNavigator.create(m.document).fold((m, none)) {
        command =>
          val (next, effects) = update(m, Intent.Dispatch(command))
          if next.document == m.document then (next, effects)
          else
            val (shown, navigationEffects) =
              navigate(next, Location(Perspective.Analysis, draftTrail(next.document)))
            (shown, effects ++ navigationEffects)
      }
    case Intent.ChoosePreset(preset) =>
      PresetPicker
        .command(m.document, preset)
        .fold((m, none))(c => update(m, Intent.Dispatch(c)))
    case Intent.Undo(stack) => applyHistory(m, undoEntry(stack), m.history.undoOn(stack))
    case Intent.Redo(stack) => applyHistory(m, redoEntry(stack), m.history.redoOn(stack))

    case Intent.CancelJob(job) =>
      m.jobs.job(job) match
        case Some(j) =>
          m.document.run(j.run) match
            case Some(run)
                if run.analysis != j.stamp.revision || run.dataset != j.stamp.dataset =>
              val refused = ExecutionError.StampMismatch(
                stampOf(m.document, run.analysis, run.dataset),
                job,
                j.stamp.revision,
                j.stamp.dataset
              )
              (m.copy(notice = Some(Notice.ExecutionRefused(refused))), none)
            case Some(run) if run.state == RunLifecycle.Running && !j.phase.isTerminal =>
              (m, Vector(AppEffect.Execution(ExecutionEffect.Cancel(job))))
            case _ => update(m, Intent.Dispatch(Command.CancelRun(j.run)))
        case None =>
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

    case Intent.Execution(ExecutionEvent.ArtifactsStored(job, facts)) =>
      val observed = m.jobs.job(job)
      val valid    = observed.exists(j =>
        j.phase.isInstanceOf[JobPhase.Succeeded] &&
          j.run == facts.run && facts.stamp.agreesWithDeclarations(j.stamp)
      )
      if valid then
        val command = Command.BindCompletedArtifacts(facts)
        m.history.apply(command) match
          case Right(step) if step.history.document == m.document && step.effects.isEmpty =>
            (m, none)
          case success @ Right(_) =>
            val (bound, effects) = applyHistory(m, JournalEntry.Apply(command), success)
            // A verified fact describes the completed request; enriching
            // its previously unbound declarations requests no new run and
            // withdraws no existing Ready notice (bead q-native-stored-completion).
            (
              bound.copy(jobs = m.jobs),
              effects.filter {
                case AppEffect.Execution(_: ExecutionEffect.Require) => false
                case _                                               => true
              }
            )
          case failure => applyHistory(m, JournalEntry.Apply(command), failure)
      else
        val found =
          observed.fold("not observed")(j => s"${j.run.label}, ${j.stamp.label}, ${j.phase}")
        val error = NativeArtifactError.persistence(
          facts.run,
          "accept stored binding",
          s"Job ${job.number} must be observed succeeded for ${facts.run.label}/${facts.stamp.label}; found $found."
        )
        (m.copy(notice = Some(Notice.ArtifactsRefused(job, facts.run, error.diagnostic))), none)
    case Intent.Execution(ExecutionEvent.ArtifactsRefused(job, run, diagnostic)) =>
      m.jobs.job(job) match
        case Some(observed)
            if observed.run == run && observed.phase.isInstanceOf[JobPhase.Succeeded] =>
          (m.copy(notice = Some(Notice.ArtifactsRefused(job, run, diagnostic))), none)
        case _ => (m, none)
    case Intent.Execution(event) =>
      val received = m.copy(jobs = m.jobs.receive(event))
      outcomeOf(received.document, event).fold((received, none)) { command =>
        applyHistory(received, JournalEntry.Apply(command), received.history.apply(command))
      }
    case Intent.JobsChanged(jobs) => (m.copy(jobs = m.jobs.withJobs(jobs)), none)
    case Intent.SessionChanged(f) => (m.copy(session = f), none)
    case Intent.CheckInputs       =>
      val asked = if m.inputs == InputCheck.NoProject then InputCheck.Unchecked else m.inputs
      val (checks, round) = m.checks.ask
      (m.copy(inputs = asked, checks = checks), Vector(AppEffect.CheckInputs(round)))
    case Intent.CancelWaitingRun =>
      (m.copy(checks = m.checks.copy(runAfter = None)), none)
    case Intent.InputsChecked(round, statuses) =>
      answered(m, round, InputCheck.Checked(statuses))
    case Intent.InputsCheckFailed(round, reason) =>
      answered(m, round, InputCheck.CheckFailed(reason))
    case Intent.ItemsLoaded(items) => (m.copy(items = items), none)
    case Intent.DesignPrepared(r)  => (m.copy(prepared = Some(r)), none)
    case Intent.DesignWithdrawn    => (m.copy(prepared = None), none)
    case Intent.PreparedRefused(ready, error, requestedRun) =>
      val refused = m.copy(
        prepared =
          m.prepared.filterNot(p => p.ready.id == ready.id && p.ready.stamp == ready.stamp),
        notice = Some(Notice.ExecutionRefused(error))
      )
      requestedRun
        .flatMap(refused.document.run)
        .filter(r =>
          r.state == RunLifecycle.Running && r.analysis == ready.stamp.revision && r.dataset == ready.stamp.dataset
        ) match
        case None      => (refused, none)
        case Some(run) =>
          val command =
            Command.RecordRunOutcome(run.id, RunLifecycle.Failed, CoreBinding.unbound)
          val (settled, effects) =
            applyHistory(refused, JournalEntry.Apply(command), refused.history.apply(command))
          (settled.copy(notice = Some(Notice.ExecutionRefused(error))), effects)
    case Intent.Saved(at, upTo)    => (m.copy(save = m.save.saved(at, upTo)), none)
    case Intent.SaveFailed(reason) => (m.copy(notice = Some(Notice.SaveFailed(reason))), none)

    case Intent.RequestRename =>
      (m, Vector(AppEffect.OpenDialog(PlatformDialog.RenameProject)))
    case Intent.RenameProject(name) =>
      if m.project.contains(name) then (m, none)
      else
        val save = m.save.edit
        (m.copy(project = Some(name), save = save), Vector(AppEffect.Persist(save.edits)))
    case Intent.RenameRefused(e) => (m.copy(notice = Some(Notice.Invalid(e))), none)
    case Intent.LayoutsUnreadable(ps, reason) =>
      (m.copy(notice = Some(Notice.LayoutsReset(ps, reason))), none)
    case Intent.RevealProject =>
      (m, m.project.fold(none)(_ => Vector(AppEffect.RevealProject)))
    case Intent.ShowProjectInfo => (m, Vector(AppEffect.OpenDialog(PlatformDialog.ProjectInfo)))
    case Intent.ShowAbout       => (m, Vector(AppEffect.OpenDialog(PlatformDialog.About)))
    // Light and Dark are the user's choice, recorded in the document (a
    // view-only SetTheme); System records nothing: the platform's theme is
    // shown while it is chosen, and a platform change is no edit.
    case Intent.SetAppearance(Appearance.System) =>
      (m.copy(appearance = m.appearance.copy(followSystem = true)), none)
    case Intent.SetAppearance(a) =>
      val next = m.copy(appearance = m.appearance.copy(followSystem = false))
      followTheme(next, next.appearance.themeFor(a))
    case Intent.SystemTheme(t) =>
      (m.copy(appearance = m.appearance.copy(system = t)), none)

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
      val save    = if edited then m.save.edit else m.save
      val doc     = step.history.document
      val effects = step.effects.map(AppEffect.of(_, doc, save.edits, m.prepared))
      val submits = effects
        .collect { case AppEffect.Execution(e) => e }
        .flatMap(
          ExecutionEffect.submitted
        )
      // Verified declarations can enrich the same native request without
      // discarding its backend-owned canonical input identity. Later view
      // and reporting edits retain that request when its declaration has
      // not changed; only a new declaration needs a fresh agreement check.
      val required = requestedStamp(doc).map { declared =>
        val unchanged = requestedStamp(m.document).contains(declared)
        m.jobs.shelf.required
          .filter { existing =>
            existing.agreesWithDeclarations(declared) ||
            (unchanged && existing.revision == declared.revision && existing.dataset == declared.dataset)
          }
          .getOrElse(declared)
      }
      val require = required
        .filter(s => submits.isEmpty && !m.jobs.shelf.required.contains(s))
        .map(s => AppEffect.Execution(ExecutionEffect.Require(s)))
      val jobs = (submits ++ require.flatMap(_ => required))
        .foldLeft(m.jobs)(_.require(_))
        .withShown(doc.presentation.shownRun)
      // A prepared design is submitted once: a later run prepares again.
      val submitted = effects.exists {
        case AppEffect.Execution(ExecutionEffect.SubmitPreview(_, _)) => true
        case _                                                        => false
      }
      val next = m.copy(
        history = step.history,
        jobs = jobs,
        notice = None,
        save = save,
        prepared = if submitted then None else m.prepared
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

  /** What the document last requested results for: its newest declared run.
    * Settling that request must not restore an older run's requirement.
    */
  def requestedStamp(document: StudioDocument): Option[RunStamp] =
    document.runs.lastOption.map(r => stampOf(document, r.analysis, r.dataset))

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

  /** The document command that records a settled job's run, while the
    * document still has that run running: a job's end is a backend fact the
    * document keeps (S3.1). A superseded job that had reported progress
    * completed on the backend; one with none never ran.
    */
  def outcomeOf(document: StudioDocument, event: ExecutionEvent): Option[Command] =
    event match
      case ExecutionEvent.Changed(job)
          if document.run(job.run).exists(_.state == RunLifecycle.Running) =>
        val lifecycle = job.phase match
          case JobPhase.Succeeded(_)    => Some(RunLifecycle.Completed)
          case JobPhase.Failed(_, _)    => Some(RunLifecycle.Failed)
          case JobPhase.Cancelled(last) => Some(RunLifecycle.Cancelled(last.map(_.stage)))
          case JobPhase.Superseded(_, Some(_)) => Some(RunLifecycle.Completed)
          case JobPhase.Superseded(_, None)    => Some(RunLifecycle.Cancelled(None))
          case JobPhase.Queued | JobPhase.Running(_) | JobPhase.Cancelling(_) => None
        // The result archive binds when the real backend reports it (S3.7).
        lifecycle.map(Command.RecordRunOutcome(job.run, _, CoreBinding.unbound))
      case _ => None

  /** The run a ref belongs to, if it is a run result. */
  def runOf(ref: StudioRef): Option[RunId] = ref match
    case StudioRef.Result(run, _)                        => Some(run)
    case StudioRef.ParticipantSummary(run, _, _, _, _)   => Some(run)
    case StudioRef.GroupCell(run, _, _, _)               => Some(run)
    case StudioRef.ReportCell(run, _, _, _, _)           => Some(run)
    case StudioRef.ReportParticipant(run, _, _, _, _, _) => Some(run)
    case StudioRef.ReportContrast(run, _, _, _, _, _)    => Some(run)
    case StudioRef.ReportQueryRange(run, _, _, _)        => Some(run)
    case StudioRef.QueryTally(run, _)                    => Some(run)
    case _                                               => None

  /** Record the move in the history, then arrive. */
  /** The answer to check `round`: an answer older than one already taken is
    * ignored. A Save & run waiting for a round runs once that round (or a
    * later one) is answered, or is refused with why its sources block it;
    * an earlier answer, from a check asked before the click, is not enough.
    */
  private def answered(
      m: AppModel,
      round: Long,
      found: InputCheck
  ): (AppModel, Vector[AppEffect]) =
    // Only the newest requested verification may refresh the UI or release
    // a waiting run. Older focus/open-time checks can complete out of order.
    if round <= m.checks.answered || round != m.checks.asked then (m, Vector.empty)
    else
      val next = m.copy(inputs = found, checks = m.checks.copy(answered = round))
      next.checks.runAfter match
        case Some((run, asked)) if asked <= round =>
          checkedThenRun(next.copy(checks = next.checks.copy(runAfter = None)), run)
        case _ => (next, Vector.empty)

  /** The Save & run `run`, after its own check: it runs, or is refused with
    * why its sources block it.
    */
  private def checkedThenRun(
      next: AppModel,
      run: Command.SaveAndRun
  ): (AppModel, Vector[AppEffect]) =
    next.runBlock match
      case Some((dataset, block)) =>
        (next.copy(notice = Some(Notice.SourcesBlocked(dataset, block))), Vector.empty)
      case None =>
        applyHistory(next, JournalEntry.Apply(run), next.history.apply(run))

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
