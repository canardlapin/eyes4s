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

package eyes4s.studio.app.analysis

import eyes4s.studio.app.text.SourcesText
import eyes4s.studio.core.assets.SourceBlock
import eyes4s.studio.app.nav.Place
import eyes4s.studio.app.{AppModel, Intent, PreparedDesign}
import eyes4s.studio.core.backend.{
  AnalysisRevision,
  DatasetRevision,
  Eligibility,
  PageRequest,
  PreviewPage,
  PreviewRow,
  TrialKey
}
import eyes4s.studio.core.document.{Perspective, Recipe}
import eyes4s.studio.core.execution.RunStamp
import eyes4s.studio.core.preview.*
import eyes4s.studio.core.selection.StudioRef

/** The analysis revision the pane prepares: the revision the Analysis trail
  * names (else the draft, else the latest revision), the dataset it runs on,
  * the stamp a run of it carries, and the recipe the preview must reflect. A
  * draft edit changes the recipe under the same revision, so it prepares
  * the design again.
  */
final case class DesignTarget(
    revision: AnalysisRevision,
    dataset: DatasetRevision,
    stamp: RunStamp,
    recipe: Recipe
) derives CanEqual

/** Which queries the table shows: the chips of the board. */
enum DesignFilter derives CanEqual:
  case All, Eligible, NoMatch, NotAdmitted, ByDesign

  /** Whether a row of `eligibility` passes. No backend row is by design yet:
    * a recipe without that category has none, and one with it (S7.1) adds
    * its own eligibility.
    */
  def admits(eligibility: Eligibility): Boolean = (this, eligibility) match
    case (All, _)                                       => true
    case (Eligible, Eligibility.Eligible)               => true
    case (NoMatch, Eligibility.NoMatch(_))              => true
    case (NotAdmitted, Eligibility.QueryNotAdmitted(_)) => true
    case _                                              => false

/** The backend preview of the target, from creation to its ready receipt. */
enum DesignPreview derives CanEqual:
  /** Nothing to prepare: no analysis revision. */
  case Idle

  /** Asked for; the backend has not created it yet. */
  case Preparing

  /** Created; eligible pairs are being counted, participant by participant. */
  case Counting(
      id: PreviewId,
      stamp: RunStamp,
      candidates: PreviewCandidates,
      progress: Option[PreviewProgress]
  )

  /** Counted: the receipt a run submits. */
  case Ready(ready: PreviewReady)

  /** The backend refused, or prepared another stamp; `reason` names it. */
  case Refused(reason: String)

  def knownCandidates: Option[PreviewCandidates] = this match
    case Counting(_, _, c, _) => Some(c)
    case Ready(r)             => Some(r.candidates)
    case _                    => None

  def receipt: Option[PreviewReady] = this match
    case Ready(r) => Some(r)
    case _        => None

/** How far the rows of the target have been read. */
enum DesignRows derives CanEqual:
  case Waiting
  case Complete
  case Failed(reason: String)

/** A row-cursor key of the table. */
enum RowMove derives CanEqual:
  case Up, Down, PageUp, PageDown, First, Last

/** A user action or a backend answer the pane's view dispatches. Answers
  * carry the generation they were asked for; an older one is ignored.
  */
enum DesignIntent derives CanEqual:
  case ChooseFilter(filter: DesignFilter)
  case Move(move: RowMove)

  /** Put the row cursor on `query`'s row. */
  case FocusRow(query: TrialKey)

  /** Enter: open the trial of the row under the cursor. */
  case OpenFocused

  /** A click on `query`'s row: the cursor moves there and the trial opens. */
  case OpenRow(query: TrialKey)

  case Previewed(generation: Long, event: PreviewEvent)
  case PreviewRefused(generation: Long, reason: String)

  /** A bounded page of the preview ended; counting may need another. */
  case PageEnded(generation: Long)
  case RowsRead(generation: Long, result: Either[String, PreviewPage])

/** What the platform or the app must do after an update. */
enum DesignEffect derives CanEqual:
  case App(intent: Intent)

  /** Create the backend preview of `revision`, counting at most `budget`
    * participants; its events are [[DesignIntent.Previewed]], then
    * [[DesignIntent.PageEnded]].
    */
  case StartPreview(generation: Long, revision: AnalysisRevision, budget: PreviewBudget)

  /** Count the next `budget` participants of preview `id`. */
  case ContinuePreview(generation: Long, id: PreviewId, budget: PreviewBudget)

  /** Read a page of `revision`'s rows; the answer is [[DesignIntent.RowsRead]]. */
  case ReadRows(generation: Long, revision: AnalysisRevision, page: PageRequest)

/** The Analysis perspective's resolved-design table (ticket S7.5;
  * Analysis.dc.html, resolved design).
  *
  * It shows the backend's preview of the target revision: the focal trials
  * with their matched reference, control count and status, filtered by
  * status; the candidate pairs before paging and the exact eligible pairs
  * after; and the stamp that identifies the prepared design. Every count is
  * the backend's: the pane computes none. The receipt of a counted preview
  * goes to the app ([[Intent.DesignPrepared]]), so a run of the same stamp
  * submits that prepared design (E2E-05). The table is one focus stop with a
  * row cursor, and every row opens its trial.
  */
final case class ResolvedDesign(
    target: Option[DesignTarget],
    generation: Long,
    preview: DesignPreview,
    rows: Vector[PreviewRow],
    rowState: DesignRows,
    filter: DesignFilter,
    cursor: Option[TrialKey],
    started: Boolean,
    blocked: Option[SourceBlock],
    workProgress: Option[PreviewEvent.CountingWork] = None
) derives CanEqual:

  /** The rows the filter passes, in source order. */
  def visible: Vector[PreviewRow] = rows.filter(r => filter.admits(r.eligibility))

  /** The cursor's position among the visible rows. */
  def cursorIndex: Option[Int] =
    cursor.flatMap(q => Option(visible.indexWhere(_.query == q)).filter(_ >= 0))

object ResolvedDesign:

  val empty: ResolvedDesign =
    ResolvedDesign(
      None,
      0L,
      DesignPreview.Idle,
      Vector.empty,
      DesignRows.Complete,
      DesignFilter.All,
      None,
      started = false,
      blocked = None
    )

  /** Bounded cursor pages per backend request, bounded by [[PreviewBudget.MaximumPages]].
    */
  val ParticipantsPerPage: Int = 6

  /** Rows read per backend request. */
  val RowsPerPage: Int = 120

  /** Rows a Page Up or Page Down moves the cursor. */
  val CursorPage: Int = 10

  private val none: Vector[DesignEffect] = Vector.empty

  /** The revision the pane prepares for `model` (see [[DesignTarget]]). */
  def target(model: AppModel): Option[DesignTarget] =
    val d      = model.document
    val chosen = model.navigation
      .trail(Perspective.Analysis)
      .reverseIterator
      .collectFirst { case Place.Revision(r) => r }
    def draft(r: AnalysisRevision): Option[(DatasetRevision, Recipe)] =
      for
        dr     <- d.draft.filter(_.id == r)
        base   <- d.analysis(dr.base)
        recipe <- d.draftRecipe
      yield (dr.dataset.getOrElse(base.dataset), recipe)
    def saved(r: AnalysisRevision): Option[(DatasetRevision, Recipe)] =
      d.analysis(r).map(a => (a.dataset, a.recipe))
    def of(r: AnalysisRevision): Option[DesignTarget] =
      draft(r).orElse(saved(r)).map { (dataset, recipe) =>
        DesignTarget(r, dataset, AppModel.stampOf(d, r, dataset), recipe)
      }
    chosen
      .flatMap(of)
      .orElse(d.draft.flatMap(dr => of(dr.id)))
      .orElse(d.latestAnalysis.flatMap(a => of(a.id)))

  /** Follow the model: a new target prepares its design again and reads its
    * rows; the filter is kept.
    */
  def sync(panel: ResolvedDesign, model: AppModel): (ResolvedDesign, Vector[DesignEffect]) =
    // Nothing is asked of the backend until the Analysis perspective has
    // been shown; from then on the pane follows its target.
    val started = panel.started || model.perspective == Perspective.Analysis
    val now     = target(model)
    // A revision whose sources are not known to be held as recorded is not
    // previewed (S2.5); it is prepared once they are.
    val blocked = now.flatMap(t => model.sources.block(t.dataset))
    if !started || (now == panel.target && blocked == panel.blocked) then (panel, none)
    else
      val generation = panel.generation + 1
      // The design prepared for the old target is no longer the one shown.
      val withdraw =
        if panel.target.isDefined then Vector(DesignEffect.App(Intent.DesignWithdrawn))
        else none
      now match
        case None =>
          (empty.copy(generation = generation, filter = panel.filter, started = true), withdraw)
        case Some(t) if blocked.isDefined =>
          val reason = blocked.fold("")(SourcesText.blocked(t.dataset, _))
          (
            empty.copy(
              target = Some(t),
              generation = generation,
              preview = DesignPreview.Refused(reason),
              rowState = DesignRows.Failed(reason),
              filter = panel.filter,
              started = true,
              blocked = blocked
            ),
            withdraw
          )
        case Some(t) =>
          val start = PreviewBudget.of(ParticipantsPerPage) match
            case Left(e)       => (DesignPreview.Refused(e.message), none)
            case Right(budget) =>
              (
                DesignPreview.Preparing,
                Vector(DesignEffect.StartPreview(generation, t.revision, budget))
              )
          (
            ResolvedDesign(
              Some(t),
              generation,
              start._1,
              Vector.empty,
              DesignRows.Waiting,
              panel.filter,
              None,
              started = true,
              blocked = None
            ),
            withdraw ++ start._2
          )

  /** The Elm-style update: pure; effects are data. */
  def update(
      panel: ResolvedDesign,
      intent: DesignIntent
  ): (ResolvedDesign, Vector[DesignEffect]) =
    import DesignIntent.*
    intent match
      // A recipe without a by-design category has nothing to filter by.
      case ChooseFilter(DesignFilter.ByDesign)
          if !panel.preview.knownCandidates.exists(_.byDesignQueries.isDefined) =>
        (panel, none)
      case ChooseFilter(f) =>
        val next = panel.copy(filter = f)
        (next.copy(cursor = next.cursorIndex.flatMap(_ => panel.cursor)), none)
      case Move(move)  => (moved(panel, move), none)
      case FocusRow(q) => (focusOn(panel, q), none)
      case OpenFocused =>
        (panel, panel.cursorIndex.fold(none)(i => open(panel.visible(i).query)))
      case OpenRow(q) =>
        val next = focusOn(panel, q)
        (next, if next.cursor.contains(q) then open(q) else none)
      case Previewed(g, event) if g == panel.generation       => previewed(panel, event)
      case PreviewRefused(g, reason) if g == panel.generation =>
        (panel.copy(preview = DesignPreview.Refused(reason)), none)
      case PageEnded(g) if g == panel.generation =>
        panel.preview match
          case DesignPreview.Counting(id, _, _, _) =>
            PreviewBudget.of(ParticipantsPerPage) match
              case Left(e) => (panel.copy(preview = DesignPreview.Refused(e.message)), none)
              case Right(budget) => (panel, Vector(DesignEffect.ContinuePreview(g, id, budget)))
          case _ => (panel, none)
      case RowsRead(g, result) if g == panel.generation => rowsRead(panel, result)
      case _                                            => (panel, none)

  private def open(query: TrialKey): Vector[DesignEffect] =
    Vector(DesignEffect.App(Intent.Explain(Place.At(StudioRef.Trial(query)))))

  private def focusOn(panel: ResolvedDesign, query: TrialKey): ResolvedDesign =
    if panel.visible.exists(_.query == query) then panel.copy(cursor = Some(query)) else panel

  private def moved(panel: ResolvedDesign, move: RowMove): ResolvedDesign =
    val rows = panel.visible
    if rows.isEmpty then panel
    else
      val last = rows.size - 1
      val next = panel.cursorIndex match
        // With no cursor, every move starts at the first row (End at the last).
        case None =>
          move match
            case RowMove.Last => last
            case _            => 0
        case Some(i) =>
          move match
            case RowMove.Up       => (i - 1).max(0)
            case RowMove.Down     => (i + 1).min(last)
            case RowMove.PageUp   => (i - CursorPage).max(0)
            case RowMove.PageDown => (i + CursorPage).min(last)
            case RowMove.First    => 0
            case RowMove.Last     => last
      panel.copy(cursor = Some(rows(next).query))

  private def previewed(
      panel: ResolvedDesign,
      event: PreviewEvent
  ): (ResolvedDesign, Vector[DesignEffect]) =
    def refuse(stamp: RunStamp): Option[String] =
      panel.target.filter(t => !stamp.agreesWithDeclarations(t.stamp)).map { t =>
        s"the backend prepared ${stamp.label}, not ${t.stamp.label}"
      }
    event match
      case PreviewEvent.Initial(id, stamp, candidates) =>
        refuse(stamp) match
          case Some(reason) => (panel.copy(preview = DesignPreview.Refused(reason)), none)
          case None         =>
            (
              panel.copy(
                preview = DesignPreview.Counting(id, stamp, candidates, None),
                workProgress = None
              ),
              none
            )
      case PreviewEvent.Counting(id, progress) =>
        panel.preview match
          case c @ DesignPreview.Counting(current, _, _, _) if current == id =>
            (panel.copy(preview = c.copy(progress = Some(progress)), workProgress = None), none)
          case _ => (panel, none)
      case progress: PreviewEvent.CountingWork =>
        panel.preview match
          case DesignPreview.Counting(current, _, _, _) if current == progress.id =>
            (panel.copy(workProgress = Some(progress)), none)
          case _ => (panel, none)
      case PreviewEvent.Ready(ready) =>
        val recipeMismatch = panel.target
          .filter { t =>
            ready.recipe.exists(_ != t.recipe) ||
            (ready.stamp != t.stamp && ready.recipe.isEmpty)
          }
          .map(_ => "the backend preview does not carry the current recipe snapshot")
        val ownership = panel.preview match
          case DesignPreview.Counting(id, initial, _, _)
              if id == ready.id && initial == ready.stamp =>
            None
          case DesignPreview.Ready(previous) if previous == ready => None
          case _ => Some("the ready receipt does not belong to the active counting preview")
        refuse(ready.stamp).orElse(recipeMismatch).orElse(ownership) match
          case Some(reason) => (panel.copy(preview = DesignPreview.Refused(reason)), none)
          case None if panel.preview.receipt.contains(ready) => (panel, none)
          case None                                          =>
            val (rowState, read) = PageRequest.first(RowsPerPage) match
              case Left(error) => (DesignRows.Failed(error.message), none)
              case Right(page) =>
                (
                  DesignRows.Waiting,
                  panel.target.toVector.map(t =>
                    DesignEffect.ReadRows(panel.generation, t.revision, page)
                  )
                )
            (
              panel.copy(
                preview = DesignPreview.Ready(ready),
                workProgress = None,
                rowState = rowState
              ),
              panel.target.toVector.map(t =>
                DesignEffect.App(Intent.DesignPrepared(PreparedDesign(ready, t.recipe)))
              ) ++ read
            )

  private def rowsRead(
      panel: ResolvedDesign,
      result: Either[String, PreviewPage]
  ): (ResolvedDesign, Vector[DesignEffect]) =
    result match
      case Left(reason) => (panel.copy(rowState = DesignRows.Failed(reason)), none)
      case Right(page)  =>
        val rows = panel.rows ++ page.rows
        page.page.next match
          case None       => (panel.copy(rows = rows, rowState = DesignRows.Complete), none)
          case Some(next) =>
            (panel.target, PageRequest.of(next, RowsPerPage)) match
              case (Some(t), Right(request)) =>
                (
                  panel.copy(rows = rows),
                  Vector(DesignEffect.ReadRows(panel.generation, t.revision, request))
                )
              case (_, Left(e)) =>
                (panel.copy(rows = rows, rowState = DesignRows.Failed(e.message)), none)
              case (None, _) => (panel.copy(rows = rows, rowState = DesignRows.Complete), none)
