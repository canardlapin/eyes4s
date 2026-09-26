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

package eyes4s.studio.core.command

import cats.syntax.all.*
import eyes4s.studio.core.backend.{AnalysisRevision, DatasetRevision, RunId}
import eyes4s.studio.core.document.*

/** A command's result: the next document, the effects to perform, and how
  * the command enters the history.
  */
final case class Outcome(
    document: StudioDocument,
    effects: Vector[Effect],
    recording: Recording
) derives CanEqual

/** The pure, total reducer (ticket S2.2). Every result is rebuilt through
  * `StudioDocument.of`, so a command can never produce a document that the
  * codec would refuse; a refusal is a [[CommandError]].
  *
  * A reversible command's inverse is a [[Command]] computed from the value
  * it replaces, not a snapshot of the document: undo then rolls back that
  * edit alone, and a backend fact recorded in between (a run completing) is
  * not undone with it.
  */
object Reducer:
  import Command.*
  import CommandError.*

  /** `document` after `command`, and the effects it requests. */
  def step(
      document: StudioDocument,
      command: Command
  ): Either[CommandError, (StudioDocument, Vector[Effect])] =
    run(document, command).map(o => (o.document, o.effects))

  def run(d: StudioDocument, c: Command): Either[CommandError, Outcome] = c match
    // --- Dataset · re-admit --------------------------------------------------
    case ImportSources(parent, sources, mapping, units, geometry) =>
      val id = DatasetRevision(d.datasets.lastOption.fold(1)(_.id.number + 1))
      for
        from <- parent.traverse(p => d.dataset(p).toRight(UnknownDataset(p)))
        spec = DatasetRevisionSpec(
          id,
          parent,
          sources,
          mapping,
          units,
          geometry,
          from.fold(AdmissionChoice.default)(_.admission),
          AdmissionDecision.Pending
        )
        next <- rebuild(d, c)(datasets = d.datasets :+ spec)
      yield reversible(next, DiscardDataset(id))

    case RestoreDataset(spec) =>
      for
        _    <- Either.cond(d.dataset(spec.id).isEmpty, (), DatasetExists(spec.id))
        _    <- Either.cond(!spec.decision.isAdmitted, (), DatasetNotPending(spec.id))
        next <- rebuild(d, c)(datasets = (d.datasets :+ spec).sortBy(_.id.number))
      yield reversible(next, DiscardDataset(spec.id))

    case DiscardDataset(id) =>
      for
        spec <- pending(d, id)
        next <- rebuild(d, c)(datasets = d.datasets.filterNot(_.id == id))
      yield reversible(next, RestoreDataset(spec))

    case SetMapping(id, mapping) =>
      editDataset(d, c, id)(_.mapping, (s, v) => s.copy(mapping = v), mapping)(
        SetMapping(id, _)
      )

    case SetUnits(id, units) =>
      editDataset(d, c, id)(_.units, (s, v) => s.copy(units = v), units)(SetUnits(id, _))

    case SetGeometry(id, geometry) =>
      editDataset(d, c, id)(_.geometry, (s, v) => s.copy(geometry = v), geometry)(
        SetGeometry(id, _)
      )

    case SetOffScreenPolicy(id, policy) =>
      editDataset(d, c, id)(
        _.admission.offScreen,
        (s, v) => s.copy(admission = s.admission.copy(offScreen = v)),
        policy
      )(SetOffScreenPolicy(id, _))

    case AddCorrection(id, index, rule) =>
      for
        spec <- pending(d, id)
        rules = spec.admission.corrections
        _ <- Either.cond(
          index >= 0 && index <= rules.size,
          (),
          CorrectionIndex(id, index, rules.size)
        )
        next <- replaceDataset(d, c)(
          spec.copy(admission =
            spec.admission.copy(corrections = rules.patch(index, Vector(rule), 0))
          )
        )
      yield reversible(next, RemoveCorrection(id, index))

    case RemoveCorrection(id, index) =>
      for
        spec <- pending(d, id)
        rules = spec.admission.corrections
        rule <- rules.lift(index).toRight(CorrectionIndex(id, index, rules.size))
        next <- replaceDataset(d, c)(
          spec.copy(admission =
            spec.admission.copy(corrections = rules.patch(index, Vector.empty, 1))
          )
        )
      yield reversible(next, AddCorrection(id, index, rule))

    case VerifyDataset(id) =>
      pending(d, id).as(Outcome(d, Vector(Effect.RequestAdmission(id)), Recording.Unrecorded))

    case Admit(id, ledger, inventory) =>
      for
        spec <- pending(d, id)
        next <- replaceDataset(d, c)(
          spec.copy(decision = AdmissionDecision.Admitted(ledger, inventory))
        )
      yield Outcome(
        next,
        Vector(Effect.Persist),
        Recording.Barrier(HistoryBarrier.DatasetAdmitted(id))
      )

    // --- Analysis · rerun ----------------------------------------------------
    case StartDraft(base, dataset, changes) =>
      for
        _    <- d.draft.map(existing => DraftExists(existing.id)).toLeft(())
        spec <- d.analysis(base).toRight(UnknownAnalysis(base))
        id = AnalysisRevision(d.latestAnalysis.getOrElse(spec).id.number + 1)
        draft <- Draft.against(id, spec, dataset, changes).left.map(Refused(c.name, _))
        next  <- rebuild(d, c)(draft = Some(draft))
      yield reversible(next, DiscardDraft)

    case RestoreDraft(draft) =>
      for
        _    <- d.draft.map(existing => DraftExists(existing.id)).toLeft(())
        next <- rebuild(d, c)(draft = Some(draft))
      yield reversible(next, DiscardDraft)

    case DiscardDraft =>
      for
        draft <- d.draft.toRight(NoDraft(c.name))
        next  <- rebuild(d, c)(draft = None)
      yield reversible(next, RestoreDraft(draft))

    case ChangeRecipe(change) =>
      for
        (base, id, prior) <- drafting(d, c)
        current = prior.fold(base.recipe)(_.recipe(base.recipe))
        _ <- change
          .mismatch(current)
          .map(StaleChange(change.field, change.renderedValues._1, _))
          .toLeft(())
        _     <- Either.cond(!change.isIdentity, (), NoChange(c.name))
        draft <- redraft(c, id, base, prior.flatMap(_.dataset), change.applyTo(current))
        next  <- rebuild(d, c)(draft = draft)
      yield reversible(next, draftInverse(prior, draft, ChangeRecipe(change.inverse)))

    case RebaseDraft(target) =>
      for
        (base, id, prior) <- drafting(d, c)
        current = prior.flatMap(_.dataset).getOrElse(base.dataset)
        _ <- Either.cond(target != current, (), NoChange(c.name))
        recipe = prior.fold(base.recipe)(_.recipe(base.recipe))
        draft <- redraft(c, id, base, Option.when(target != base.dataset)(target), recipe)
        next  <- rebuild(d, c)(draft = draft)
      yield reversible(next, draftInverse(prior, draft, RebaseDraft(current)))

    case SaveAndRun(studio) =>
      for
        draft <- d.draft.toRight(NoDraft(c.name))
        base  <- d.analysis(draft.base).toRight(UnknownAnalysis(draft.base))
        target = draft.dataset.getOrElse(base.dataset)
        data <- d.dataset(target).toRight(UnknownDataset(target))
        _    <- Either.cond(data.decision.isAdmitted, (), DatasetNotAdmitted(target))
        revision = AnalysisRevisionSpec(
          draft.id,
          target,
          CoreBinding.unbound,
          draft.recipe(base.recipe),
          studio.getOrElse(base.studio)
        )
        run     = RunId(d.runs.lastOption.fold(1)(_.id.number + 1))
        started = RunRef(run, draft.id, target, RunLifecycle.Running, CoreBinding.unbound)
        next <- rebuild(d, c)(
          analyses = d.analyses :+ revision,
          draft = None,
          runs = d.runs :+ started
        )
      yield Outcome(
        next,
        Vector(Effect.RequestRun(run, draft.id, target), Effect.Persist),
        Recording.Barrier(HistoryBarrier.RunRequested(draft.id, run))
      )

    case RecordRunOutcome(id, state, archive) =>
      for
        run  <- d.run(id).toRight(UnknownRun(id))
        _    <- Either.cond(run.state == RunLifecycle.Running, (), RunNotRunning(id, run.state))
        _    <- Either.cond(state != RunLifecycle.Running, (), OutcomeStillRunning(id))
        next <- rebuild(d, c)(
          runs =
            d.runs.map(r => if r.id == id then r.copy(state = state, archive = archive) else r),
          jobs = d.jobs.filterNot(_.run == id)
        )
      yield Outcome(next, Vector(Effect.Persist), Recording.Unrecorded)

    // --- Reporting · no rerun ------------------------------------------------
    case PutReporting(spec) =>
      d.reporting.find(_.id == spec.id) match
        case Some(old) if old == spec => Left(NoChange(c.name))
        case Some(old)                =>
          rebuild(d, c)(reporting = d.reporting.map(r => if r.id == spec.id then spec else r))
            .map(reversible(_, PutReporting(old)))
        case None =>
          rebuild(d, c)(reporting = (d.reporting :+ spec).sortWith(_.id.value < _.id.value))
            .map(reversible(_, RemoveReporting(spec.id)))

    case RemoveReporting(id) =>
      for
        old <- d.reporting.find(_.id == id).toRight(UnknownReporting(id))
        users = d.figures.filter(_.reporting == id).map(_.id)
        _    <- Either.cond(users.isEmpty, (), ReportingInUse(id, users))
        next <- rebuild(d, c)(reporting = d.reporting.filterNot(_.id == id))
      yield reversible(next, PutReporting(old))

    case CreateFigure(run, reporting, panels) =>
      for
        id <- FigureId
          .of(d.figures.lastOption.fold(1)(_.id.number + 1))
          .left
          .map(Refused(c.name, _))
        spec <- FigureSpec.of(id, run, reporting, panels).left.map(Refused(c.name, _))
        next <- rebuild(d, c)(figures = d.figures :+ spec)
      yield reversible(next, DeleteFigure(id))

    case RestoreFigure(spec) =>
      for
        _    <- Either.cond(!d.figures.exists(_.id == spec.id), (), FigureExists(spec.id))
        next <- rebuild(d, c)(figures = (d.figures :+ spec).sortBy(_.id.number))
      yield reversible(next, DeleteFigure(spec.id))

    case DeleteFigure(id) =>
      for
        old  <- figure(d, id)
        next <- rebuild(d, c)(figures = d.figures.filterNot(_.id == id))
      yield reversible(next, RestoreFigure(old))

    case BindFigure(id, run, reporting) =>
      for
        old  <- figure(d, id)
        _    <- Either.cond(old.run != run || old.reporting != reporting, (), NoChange(c.name))
        spec <- FigureSpec.of(id, run, reporting, old.panels).left.map(Refused(c.name, _))
        next <- replaceFigure(d, c)(spec)
      yield reversible(next, BindFigure(id, old.run, old.reporting))

    case SetPanelScale(id, letter, scale) =>
      editPanel(d, c, id, letter)(_.scale, (p, v) => p.copy(scale = v), scale)(
        SetPanelScale(id, letter, _)
      )

    case SetPanelSelection(id, letter, selection) =>
      editPanel(d, c, id, letter)(_.selection, (p, v) => p.copy(selection = v), selection)(
        SetPanelSelection(id, letter, _)
      )

    // --- View only -----------------------------------------------------------
    case SetPerspective(perspective) =>
      view(d, c)(p => (p.perspective, rebuildView(p)(perspective = perspective)))(
        SetPerspective(_)
      )
    case SetTheme(theme) =>
      view(d, c)(p => (p.theme, rebuildView(p)(theme = theme)))(SetTheme(_))
    case SetStage(stage) =>
      view(d, c)(p => (p.stage, rebuildView(p)(stage = stage)))(SetStage(_))
    case SetMapOpacity(opacity) =>
      view(d, c)(p => (p.mapOpacity, rebuildView(p)(mapOpacity = opacity)))(SetMapOpacity(_))
    case SetUnderlay(shown) =>
      view(d, c)(p => (p.underlay, rebuildView(p)(underlay = shown)))(SetUnderlay(_))
    case ShowRun(run) =>
      view(d, c)(p => (p.shownRun, rebuildView(p)(shownRun = run)))(ShowRun(_))
    case SaveLayout(perspective, layout) =>
      view(d, c) { p =>
        val kept = p.layouts.filterNot(_.perspective == perspective)
        (
          p.layouts.find(_.perspective == perspective).map(_.layout),
          rebuildView(p)(layouts = kept ++ layout.map(SavedLayout(perspective, _)))
        )
      }(SaveLayout(perspective, _))

  // --- Helpers ---------------------------------------------------------------

  private def reversible(next: StudioDocument, inverse: Command): Outcome =
    Outcome(next, Vector(Effect.Persist), Recording.Reversible(inverse))

  /** The document with some science replaced, checked as a whole. */
  private def rebuild(d: StudioDocument, c: Command)(
      datasets: Vector[DatasetRevisionSpec] = d.datasets,
      analyses: Vector[AnalysisRevisionSpec] = d.analyses,
      draft: Option[Draft] = d.draft,
      runs: Vector[RunRef] = d.runs,
      reporting: Vector[ReportingSpec] = d.reporting,
      figures: Vector[FigureSpec] = d.figures,
      jobs: Vector[JobHandle] = d.jobs
  ): Either[CommandError, StudioDocument] =
    StudioDocument
      .of(datasets, analyses, draft, runs, reporting, figures, d.presentation, jobs)
      .left
      .map(Refused(c.name, _))

  private def pending(d: StudioDocument, id: DatasetRevision) =
    d.dataset(id)
      .toRight(UnknownDataset(id))
      .flatMap(s => Either.cond(!s.decision.isAdmitted, s, DatasetNotPending(id)))

  private def replaceDataset(d: StudioDocument, c: Command)(spec: DatasetRevisionSpec) =
    rebuild(d, c)(datasets = d.datasets.map(s => if s.id == spec.id then spec else s))

  private def editDataset[A](d: StudioDocument, c: Command, id: DatasetRevision)(
      get: DatasetRevisionSpec => A,
      set: (DatasetRevisionSpec, A) => DatasetRevisionSpec,
      value: A
  )(inverse: A => Command)(using CanEqual[A, A]): Either[CommandError, Outcome] =
    for
      spec <- pending(d, id)
      old = get(spec)
      _    <- Either.cond(old != value, (), NoChange(c.name))
      next <- replaceDataset(d, c)(set(spec, value))
    yield reversible(next, inverse(old))

  /** The draft's base, its id and the draft, or the latest revision and the
    * id a new draft of it takes.
    */
  private def drafting(
      d: StudioDocument,
      c: Command
  ): Either[CommandError, (AnalysisRevisionSpec, AnalysisRevision, Option[Draft])] =
    d.draft match
      case Some(draft) =>
        d.analysis(draft.base)
          .toRight(UnknownAnalysis(draft.base))
          .map((_, draft.id, Some(draft)))
      case None =>
        d.latestAnalysis
          .toRight(NoAnalysis(c.name))
          .map(b => (b, AnalysisRevision(b.id.number + 1), None))

  /** The draft of `base` that describes `target` on `dataset`, or none when
    * that is exactly the base.
    */
  private def redraft(
      c: Command,
      id: AnalysisRevision,
      base: AnalysisRevisionSpec,
      dataset: Option[DatasetRevision],
      target: Recipe
  ): Either[CommandError, Option[Draft]] =
    val changes = RecipeChange.between(base.recipe, target)
    if changes.isEmpty && dataset.isEmpty then Right(None)
    else Draft.against(id, base, dataset, changes).bimap(Refused(c.name, _), Some(_))

  /** A draft edit's inverse: discard a draft it created, restore one it
    * removed, and otherwise the edit in the other direction.
    */
  private def draftInverse(prior: Option[Draft], next: Option[Draft], edit: Command): Command =
    (prior, next) match
      case (None, _)          => DiscardDraft
      case (Some(p), None)    => RestoreDraft(p)
      case (Some(_), Some(_)) => edit

  private def figure(d: StudioDocument, id: FigureId) =
    d.figures.find(_.id == id).toRight(UnknownFigure(id))

  private def replaceFigure(d: StudioDocument, c: Command)(spec: FigureSpec) =
    rebuild(d, c)(figures = d.figures.map(f => if f.id == spec.id then spec else f))

  private def editPanel[A](d: StudioDocument, c: Command, id: FigureId, letter: PanelLetter)(
      get: PanelSpec => A,
      set: (PanelSpec, A) => PanelSpec,
      value: A
  )(inverse: A => Command)(using CanEqual[A, A]): Either[CommandError, Outcome] =
    for
      old   <- figure(d, id)
      panel <- old.panels.find(_.letter == letter).toRight(UnknownPanel(id, letter))
      _     <- Either.cond(get(panel) != value, (), NoChange(c.name))
      panels = old.panels.map(p => if p.letter == letter then set(p, value) else p)
      spec <- FigureSpec.of(id, old.run, old.reporting, panels).left.map(Refused(c.name, _))
      next <- replaceFigure(d, c)(spec)
    yield reversible(next, inverse(get(panel)))

  private def rebuildView(p: PresentationState)(
      perspective: Perspective = p.perspective,
      theme: Theme = p.theme,
      stage: StageAppearance = p.stage,
      mapOpacity: MapOpacity = p.mapOpacity,
      underlay: Boolean = p.underlay,
      shownRun: Option[RunId] = p.shownRun,
      layouts: Vector[SavedLayout] = p.layouts
  ): Either[DocumentError, PresentationState] =
    PresentationState.of(perspective, theme, stage, mapOpacity, underlay, shownRun, layouts)

  /** A view-only edit: `edit` gives the field's current value and the next
    * presentation; the inverse sets the current value back.
    */
  private def view[A](d: StudioDocument, c: Command)(
      edit: PresentationState => (A, Either[DocumentError, PresentationState])
  )(inverse: A => Command): Either[CommandError, Outcome] =
    val (old, next) = edit(d.presentation)
    for
      p   <- next.left.map(Refused(c.name, _))
      _   <- Either.cond(p != d.presentation, (), NoChange(c.name))
      doc <- d.withPresentation(p).left.map(Refused(c.name, _))
    yield reversible(doc, inverse(old))
