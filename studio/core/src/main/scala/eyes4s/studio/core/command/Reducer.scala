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
import eyes4s.studio.core.preset.RecipePresets

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
/** Which mapping rule the reducer applies (S5.3). `Commit`: a mapping must
  * have every role import requires ([[ColumnMapping.admissible]]). `Replay`:
  * a journal replay reconstructs history, including commands S5.2 accepted
  * without a phase, so it checks only the stored roles; admission of such a
  * revision is still refused afterwards, at VerifyDataset.
  */
enum MappingRule derives CanEqual:
  case Commit, Replay

object Reducer:
  import Command.*
  import CommandError.*

  /** `document` after `command`, and the effects it requests. */
  def step(
      document: StudioDocument,
      command: Command
  ): Either[CommandError, (StudioDocument, Vector[Effect])] =
    run(document, command).map(o => (o.document, o.effects))

  def run(
      d: StudioDocument,
      c: Command,
      rule: MappingRule = MappingRule.Commit
  ): Either[CommandError, Outcome] = c match
    // --- Dataset · re-admit --------------------------------------------------
    case ImportSources(
          parent,
          sources,
          mapping,
          units,
          geometry,
          attributes,
          admission,
          inventory
        ) =>
      val id = DatasetRevision(d.datasets.lastOption.fold(1)(_.id.number + 1))
      for
        _    <- admissible(rule, d, c, mapping)
        _    <- inventoryMapped(rule, d, c, id, sources, inventory)
        from <- parent.traverse(p => d.dataset(p).toRight(UnknownDataset(p)))
        spec = DatasetRevisionSpec(
          id,
          parent,
          sources,
          mapping,
          units,
          geometry,
          admission.orElse(from.map(_.admission)).getOrElse(AdmissionChoice.default),
          AdmissionDecision.Pending,
          attributes,
          inventory
        )
        _    <- correctionsApart(rule, d, c, id, spec.admission)
        next <- rebuild(d, c)(datasets = d.datasets :+ spec)
      yield reversible(next, DiscardDataset(id))

    case RestoreDataset(spec) =>
      for
        _    <- Either.cond(d.dataset(spec.id).isEmpty, (), DatasetExists(spec.id))
        _    <- Either.cond(!spec.decision.isAdmitted, (), DatasetNotPending(spec.id))
        next <- rebuild(d, c)(datasets = (d.datasets :+ spec).sortBy(_.id.number))
      yield reversible(next, DiscardDataset(spec.id))

    case RestoreRepairedDataset(spec, relinks) =>
      for
        _ <- Either.cond(d.dataset(spec.id).isEmpty, (), DatasetExists(spec.id))
        _ <- Either.cond(!spec.decision.isAdmitted, (), DatasetNotPending(spec.id))
        _ <- relinks
          .find(_.dataset != spec.id)
          .map(r => refused(d, c)(DocumentError.RelinkUnknownDataset(r.dataset, r.file.value)))
          .toLeft(())
        next <- rebuild(d, c)(
          datasets = (d.datasets :+ spec).sortBy(_.id.number),
          relinks = d.relinks.plus(relinks)
        )
      yield reversible(next, DiscardDataset(spec.id))

    case DiscardDataset(id) =>
      // Its repairs go with it, and come back with its undo.
      for
        spec <- pending(d, id)
        next <- rebuild(d, c)(
          datasets = d.datasets.filterNot(_.id == id),
          relinks = d.relinks.without(id)
        )
        dropped = d.relinks.of(id)
      yield reversible(
        next,
        if dropped.isEmpty then RestoreDataset(spec) else RestoreRepairedDataset(spec, dropped)
      )

    case ReviseDataset(id, mapping, units, geometry, attributes, inventory) =>
      for
        spec <- editable(d, id)
        _    <- revisable(rule, d, c, spec.mapping, mapping)
        _    <- inventoryRevisable(rule, d, c, spec, inventory)
        revised = spec.copy(
          mapping = mapping,
          units = units,
          geometry = geometry,
          attributes = attributes,
          inventory = inventory
        )
        _    <- Either.cond(revised != spec, (), NoChange(c.name, targetOf(d, c)))
        next <- replaceDataset(d, c)(revised)
      yield reversible(
        next,
        ReviseDataset(
          id,
          spec.mapping,
          spec.units,
          spec.geometry,
          spec.attributes,
          spec.inventory
        )
      )

    case SetMapping(id, mapping) =>
      for
        spec <- editable(d, id)
        _    <- revisable(rule, d, c, spec.mapping, mapping)
        out  <- editDataset(d, c, id)(_.mapping, (s, v) => s.copy(mapping = v), mapping)(
          SetMapping(id, _)
        )
      yield out

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

    case AddCorrection(id, index, correction) =>
      for
        spec <- editable(d, id)
        rules = spec.admission.corrections
        _ <- Either.cond(
          index >= 0 && index <= rules.size,
          (),
          CorrectionIndex(id, index, rules.size)
        )
        added = spec.admission.copy(corrections = rules.patch(index, Vector(correction), 0))
        _    <- correctionsApart(rule, d, c, id, added)
        next <- replaceDataset(d, c)(spec.copy(admission = added))
      yield reversible(next, RemoveCorrection(id, index))

    case RemoveCorrection(id, index) =>
      for
        spec <- editable(d, id)
        rules = spec.admission.corrections
        rule <- rules.lift(index).toRight(CorrectionIndex(id, index, rules.size))
        next <- replaceDataset(d, c)(
          spec.copy(admission =
            spec.admission.copy(corrections = rules.patch(index, Vector.empty, 1))
          )
        )
      yield reversible(next, AddCorrection(id, index, rule))

    case VerifyDataset(id) =>
      for
        spec <- editable(d, id)
        _    <- admissible(rule, d, c, spec.mapping)
        _    <- inventoryMapped(rule, d, c, id, spec.sources, spec.inventory)
        _    <- keysAgree(rule, d, c, id, spec.sources, spec.mapping, spec.inventory)
        // A revision stored with overlapping rules is not admitted either.
        _       <- correctionsApart(rule, d, c, id, spec.admission)
        content <- contentOf(spec)
        next <- replaceDataset(d, c)(spec.copy(decision = AdmissionDecision.Verifying(content)))
      yield Outcome(
        next,
        Vector(Effect.RequestAdmission(id, content), Effect.Persist),
        Recording.Reversible(WithdrawVerification(id))
      )

    case WithdrawVerification(id) =>
      for
        spec    <- pending(d, id)
        content <- spec.decision match
          case AdmissionDecision.Verifying(content) => Right(content)
          case _                                    => Left(NotVerified(id))
        next <- replaceDataset(d, c)(spec.copy(decision = AdmissionDecision.Pending))
      yield reversible(next, ResumeVerification(id, content))

    case ResumeVerification(id, content) =>
      // Resuming is verifying again: VerifyDataset's checks, so a direct
      // command cannot bring a revision Verify refuses to admission.
      for
        spec <- editable(d, id)
        _    <- admissible(rule, d, c, spec.mapping)
        _    <- inventoryMapped(rule, d, c, id, spec.sources, spec.inventory)
        _    <- keysAgree(rule, d, c, id, spec.sources, spec.mapping, spec.inventory)
        _    <- correctionsApart(rule, d, c, id, spec.admission)
        next <- replaceDataset(d, c)(spec.copy(decision = AdmissionDecision.Verifying(content)))
      yield reversible(next, WithdrawVerification(id))

    case Admit(id, verified, policy, ledger, inventory) =>
      for
        spec     <- pending(d, id)
        recorded <- spec.decision match
          case AdmissionDecision.Verifying(content) => Right(content)
          case _                                    => Left(NotVerified(id))
        _ <- Either.cond(verified == recorded, (), VerificationMismatch(id, recorded, verified))
        // A backstop: VerifyDataset already refuses these, but a replayed or
        // stored Verifying revision did not pass them.
        _       <- admissible(rule, d, c, spec.mapping)
        _       <- inventoryMapped(rule, d, c, id, spec.sources, spec.inventory)
        _       <- keysAgree(rule, d, c, id, spec.sources, spec.mapping, spec.inventory)
        _       <- correctionsApart(rule, d, c, id, spec.admission)
        current <- contentOf(spec)
        _       <- Either.cond(
          current == recorded,
          (),
          ChangedSinceVerification(id, recorded, current)
        )
        next <- replaceDataset(d, c)(
          spec.copy(decision = AdmissionDecision.Admitted(policy, ledger, inventory))
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
        draft <- Draft.against(id, spec, dataset, changes).left.map(refused(d, c))
        next  <- rebuild(d, c)(draft = Some(draft))
      yield reversible(next, DiscardDraft)

    case RestoreDraft(draft) =>
      for
        _    <- d.draft.map(existing => DraftExists(existing.id)).toLeft(())
        next <- rebuild(d, c)(draft = Some(draft))
      yield reversible(next, DiscardDraft)

    case DiscardDraft =>
      for
        draft <- d.draft.toRight(NoDraft(c.name, targetOf(d, c)))
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
        _     <- Either.cond(!change.isIdentity, (), NoChange(c.name, targetOf(d, c)))
        draft <- redraft(d, c, id, base, prior.flatMap(_.dataset), change.applyTo(current))
        next  <- rebuild(d, c)(draft = draft)
      yield reversible(next, draftInverse(prior, draft, ChangeRecipe(change.inverse)))

    case ChangeRecipes(changes) =>
      for
        (base, id, prior) <- drafting(d, c)
        current = prior.fold(base.recipe)(_.recipe(base.recipe))
        _ <- Either.cond(changes.nonEmpty, (), NoChange(c.name, targetOf(d, c)))
        _ <- changes
          .groupBy(_.field)
          .collectFirst {
            case (field, cs) if cs.size > 1 =>
              Refused(c.name, targetOf(d, c), DocumentError.RepeatedField(id, field))
          }
          .toLeft(())
        _ <- changes.traverse_ { change =>
          change
            .mismatch(current)
            .map(StaleChange(change.field, change.renderedValues._1, _))
            .toLeft(())
        }
        _ <- Either.cond(changes.exists(!_.isIdentity), (), NoChange(c.name, targetOf(d, c)))
        target = changes.foldLeft(current)((r, change) => change.applyTo(r))
        draft <- redraft(d, c, id, base, prior.flatMap(_.dataset), target)
        next  <- rebuild(d, c)(draft = draft)
      yield reversible(next, draftInverse(prior, draft, ChangeRecipes(changes.map(_.inverse))))

    case RebaseDraft(target) =>
      for
        (base, id, prior) <- drafting(d, c)
        current = prior.flatMap(_.dataset).getOrElse(base.dataset)
        _ <- Either.cond(target != current, (), NoChange(c.name, targetOf(d, c)))
        recipe = prior.fold(base.recipe)(_.recipe(base.recipe))
        draft <- redraft(d, c, id, base, Option.when(target != base.dataset)(target), recipe)
        next  <- rebuild(d, c)(draft = draft)
      yield reversible(next, draftInverse(prior, draft, RebaseDraft(current)))

    case SaveAndRun(studio) =>
      for
        draft <- d.draft.toRight(NoDraft(c.name, targetOf(d, c)))
        base  <- d.analysis(draft.base).toRight(UnknownAnalysis(draft.base))
        target = draft.dataset.getOrElse(base.dataset)
        data <- d.dataset(target).toRight(UnknownDataset(target))
        _    <- Either.cond(data.decision.isAdmitted, (), DatasetNotAdmitted(target))
        recipe = draft.recipe(base.recipe)
        fields <- studioFields(draft, base, recipe, studio)
        revision = AnalysisRevisionSpec(
          draft.id,
          target,
          CoreBinding.unbound,
          recipe,
          fields
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

    case CancelRun(id) =>
      for
        run <- d.run(id).toRight(UnknownRun(id))
        _   <- Either.cond(run.state == RunLifecycle.Running, (), RunNotRunning(id, run.state))
        job <- d.job(id).toRight(NoJobHandle(id))
      yield Outcome(d, Vector(Effect.CancelJob(id, job)), Recording.Unrecorded)

    case BindPlan(id, plan, input) =>
      for
        spec <- d.analysis(id).toRight(UnknownAnalysis(id))
        _    <- spec.plan match
          case CoreBinding.Bound(bound) => Left(PlanAlreadyBound(id, bound))
          case CoreBinding.Unbound()    => Right(())
        _ <- spec.recipe.input match
          case Some(recorded) if recorded != input => Left(InputMismatch(id, recorded, input))
          case _                                   => Right(())
        bound = spec.copy(
          plan = CoreBinding.Bound(plan),
          recipe = spec.recipe.copy(input = Some(input))
        )
        next <- rebuild(d, c)(analyses = d.analyses.map(a => if a.id == id then bound else a))
      yield Outcome(next, Vector(Effect.Persist), Recording.Unrecorded)

    // --- Reporting · no rerun ------------------------------------------------
    case PutReporting(spec) =>
      d.reporting.find(_.id == spec.id) match
        case Some(old) if old == spec => Left(NoChange(c.name, targetOf(d, c)))
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
        id   <- d.nextFigureId.left.map(refused(d, c))
        spec <- FigureSpec.of(id, run, reporting, panels).left.map(refused(d, c))
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
        old <- figure(d, id)
        _   <- Either.cond(
          old.run != run || old.reporting != reporting,
          (),
          NoChange(c.name, targetOf(d, c))
        )
        spec <- FigureSpec.of(id, run, reporting, old.panels).left.map(refused(d, c))
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

    case AddPanel(id, index, panel) =>
      for
        old <- figure(d, id)
        _   <- Either.cond(
          index >= 0 && index <= old.panels.size,
          (),
          PanelIndex(id, index, old.panels.size)
        )
        panels = old.panels.patch(index, Vector(panel), 0)
        spec <- FigureSpec.of(id, old.run, old.reporting, panels).left.map(refused(d, c))
        next <- replaceFigure(d, c)(spec)
      yield reversible(next, RemovePanel(id, panel.letter))

    case RemovePanel(id, letter) =>
      for
        old <- figure(d, id)
        index = old.panels.indexWhere(_.letter == letter)
        panel <- old.panels.lift(index).toRight(UnknownPanel(id, letter))
        panels = old.panels.patch(index, Vector.empty, 1)
        spec <- FigureSpec.of(id, old.run, old.reporting, panels).left.map(refused(d, c))
        next <- replaceFigure(d, c)(spec)
      yield reversible(next, AddPanel(id, index, panel))

    case RetitlePanel(id, letter, title) =>
      editPanel(d, c, id, letter)(_.title, (p, v) => p.copy(title = v), title)(
        RetitlePanel(id, letter, _)
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
    case RelinkAsset(id, file, asset) =>
      for
        _ <- d.dataset(id).toRight(UnknownDataset(id))
        old = d.relinks.find(id, file).map(_.asset)
        _   <- Either.cond(old != asset, (), NoChange(c.name, targetOf(d, c)))
        doc <- d.withRelinks(d.relinks.set(id, file, asset)).left.map(refused(d, c))
      yield reversible(doc, RelinkAsset(id, file, old))

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
      jobs: Vector[JobHandle] = d.jobs,
      relinks: AssetRelinks = d.relinks
  ): Either[CommandError, StudioDocument] =
    StudioDocument
      .of(datasets, analyses, draft, runs, reporting, figures, d.presentation, jobs)
      // The repaired assets go with the science they repair.
      .flatMap(_.withRelinks(relinks))
      .left
      .map(refused(d, c))

  private def refused(d: StudioDocument, c: Command)(error: DocumentError): CommandError =
    Refused(c.name, targetOf(d, c), error)

  /** The document value `c` acts on in `d`. */
  /** The S5.3 commit check: `mapping` has every role import requires.
    * Replay reconstructs history and checks only the stored roles.
    */
  private def admissible(
      rule: MappingRule,
      d: StudioDocument,
      c: Command,
      mapping: ColumnMapping
  ): Either[CommandError, Unit] = rule match
    case MappingRule.Replay => Right(())
    case MappingRule.Commit =>
      ColumnMapping
        .admissible(mapping)
        .left
        .map(Refused(c.name, targetOf(d, c), _))
        .map(_ => ())

  /** The inventory rule (S5.4): under `Commit`, a trials source must have
    * its columns mapped. A replayed journal line from before S5.4 has none;
    * such a revision is refused at VerifyDataset until it is re-mapped.
    */
  private def inventoryMapped(
      rule: MappingRule,
      d: StudioDocument,
      c: Command,
      id: DatasetRevision,
      sources: Sources,
      inventory: Option[InventoryMapping]
  ): Either[CommandError, Unit] = rule match
    case MappingRule.Replay => Right(())
    case MappingRule.Commit =>
      DatasetRevisionSpec
        .inventoryMapped(id, sources, inventory)
        .left
        .map(Refused(c.name, targetOf(d, c), _))

  /** At most one correction rule covers a trial (eyes4s's own check, S5.5):
    * refused whatever the rule, since an overlapping policy is never admitted.
    */
  private def correctionsApart(
      rule: MappingRule,
      d: StudioDocument,
      c: Command,
      id: DatasetRevision,
      choice: AdmissionChoice
  ): Either[CommandError, Unit] = rule match
    case MappingRule.Replay => Right(())
    case MappingRule.Commit =>
      AdmissionChoice
        .overlap(choice)
        .map((a, b) =>
          Refused(c.name, targetOf(d, c), DocumentError.CorrectionsOverlap(id, a, b))
        )
        .toLeft(())

  /** The fixations and the inventory name a trial by the same key, at commit. */
  private def keysAgree(
      rule: MappingRule,
      d: StudioDocument,
      c: Command,
      id: DatasetRevision,
      sources: Sources,
      mapping: ColumnMapping,
      inventory: Option[InventoryMapping]
  ): Either[CommandError, Unit] = rule match
    case MappingRule.Replay => Right(())
    case MappingRule.Commit =>
      DatasetRevisionSpec
        .keysAgree(id, sources, mapping, inventory)
        .left
        .map(Refused(c.name, targetOf(d, c), _))

  /** A revision's inventory mapping replaced by `next` (ReviseDataset), as
    * [[revisable]] does for the phase: a revision stored before S5.4 with an
    * unmapped trials source is only edited into one with a mapping; over a
    * mapped one nothing is refused, so undoing a re-map restores it.
    */
  private def inventoryRevisable(
      rule: MappingRule,
      d: StudioDocument,
      c: Command,
      spec: DatasetRevisionSpec,
      next: Option[InventoryMapping]
  ): Either[CommandError, Unit] =
    DatasetRevisionSpec.inventoryMapped(spec.id, spec.sources, spec.inventory) match
      case Right(_) => Right(())
      case Left(_)  => inventoryMapped(rule, d, c, spec.id, spec.sources, next)

  /** A revision's mapping replaced by `next` (ReviseDataset, SetMapping).
    * A revision stored before S5.3 may lack the phase: `next` must then be
    * admissible, so an old revision is only edited into one that can be
    * committed. Over an admissible mapping nothing is refused, so undoing a
    * re-map restores the stored mapping; VerifyDataset and Admit keep an
    * inadmissible one from admission, and the wizard never commits one.
    */
  private def revisable(
      rule: MappingRule,
      d: StudioDocument,
      c: Command,
      current: ColumnMapping,
      next: ColumnMapping
  ): Either[CommandError, Unit] =
    if current.missingForImport.isEmpty then Right(()) else admissible(rule, d, c, next)

  def targetOf(d: StudioDocument, c: Command): Target = c match
    case _: ImportSources =>
      Target.OnDataset(DatasetRevision(d.datasets.lastOption.fold(1)(_.id.number + 1)))
    case RestoreDataset(spec)            => Target.OnDataset(spec.id)
    case RestoreRepairedDataset(spec, _) => Target.OnDataset(spec.id)
    case DiscardDataset(id)              => Target.OnDataset(id)
    case r: ReviseDataset                => Target.OnDataset(r.dataset)
    case SetMapping(id, _)               => Target.OnDataset(id)
    case SetUnits(id, _)                 => Target.OnDataset(id)
    case RelinkAsset(id, _, _)           => Target.OnDataset(id)
    case SetGeometry(id, _)              => Target.OnDataset(id)
    case SetOffScreenPolicy(id, _)       => Target.OnDataset(id)
    case AddCorrection(id, _, _)         => Target.OnDataset(id)
    case RemoveCorrection(id, _)         => Target.OnDataset(id)
    case VerifyDataset(id)               => Target.OnDataset(id)
    case WithdrawVerification(id)        => Target.OnDataset(id)
    case ResumeVerification(id, _)       => Target.OnDataset(id)
    case Admit(id, _, _, _, _)           => Target.OnDataset(id)
    case RestoreDraft(draft)             => Target.OnDraft(Some(draft.id))
    case _: (StartDraft | ChangeRecipe | ChangeRecipes | RebaseDraft | SaveAndRun) |
        DiscardDraft =>
      Target.OnDraft(
        d.draft.map(_.id).orElse(d.latestAnalysis.map(a => AnalysisRevision(a.id.number + 1)))
      )
    case RecordRunOutcome(run, _, _) => Target.OnRun(run)
    case CancelRun(run)              => Target.OnRun(run)
    case BindPlan(revision, _, _)    => Target.OnAnalysis(revision)
    case PutReporting(spec)          => Target.OnReporting(spec.id)
    case RemoveReporting(id)         => Target.OnReporting(id)
    case CreateFigure(_, _, _)       =>
      d.nextFigureId.fold(_ => Target.OnPresentation, Target.OnFigure(_))
    case RestoreFigure(spec)              => Target.OnFigure(spec.id)
    case DeleteFigure(id)                 => Target.OnFigure(id)
    case BindFigure(id, _, _)             => Target.OnFigure(id)
    case SetPanelScale(id, letter, _)     => Target.OnPanel(id, letter)
    case SetPanelSelection(id, letter, _) => Target.OnPanel(id, letter)
    case AddPanel(id, _, panel)           => Target.OnPanel(id, panel.letter)
    case RemovePanel(id, letter)          => Target.OnPanel(id, letter)
    case RetitlePanel(id, letter, _)      => Target.OnPanel(id, letter)
    case _: (SetPerspective | SetTheme | SetStage | SetMapOpacity | SetUnderlay | ShowRun |
          SaveLayout) =>
      Target.OnPresentation

  private def pending(d: StudioDocument, id: DatasetRevision) =
    d.dataset(id)
      .toRight(UnknownDataset(id))
      .flatMap(s => Either.cond(!s.decision.isAdmitted, s, DatasetNotPending(id)))

  /** A pending revision whose content may change: not under verification. */
  private def editable(d: StudioDocument, id: DatasetRevision) =
    pending(d, id).flatMap { s =>
      s.decision match
        case AdmissionDecision.Verifying(content) => Left(VerificationPending(id, content))
        case _                                    => Right(s)
    }

  private def contentOf(spec: DatasetRevisionSpec) =
    DatasetRevisionSpec
      .contentDigest(spec)
      .left
      .map(Undigestible(Target.OnDataset(spec.id), _))

  private def replaceDataset(d: StudioDocument, c: Command)(spec: DatasetRevisionSpec) =
    rebuild(d, c)(datasets = d.datasets.map(s => if s.id == spec.id then spec else s))

  private def editDataset[A](d: StudioDocument, c: Command, id: DatasetRevision)(
      get: DatasetRevisionSpec => A,
      set: (DatasetRevisionSpec, A) => DatasetRevisionSpec,
      value: A
  )(inverse: A => Command)(using CanEqual[A, A]): Either[CommandError, Outcome] =
    for
      spec <- editable(d, id)
      old = get(spec)
      _    <- Either.cond(old != value, (), NoChange(c.name, targetOf(d, c)))
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
      d: StudioDocument,
      c: Command,
      id: AnalysisRevision,
      base: AnalysisRevisionSpec,
      dataset: Option[DatasetRevision],
      target: Recipe
  ): Either[CommandError, Option[Draft]] =
    val changes = RecipeChange.between(base.recipe, target)
    if changes.isEmpty && dataset.isEmpty then Right(None)
    else Draft.against(id, base, dataset, changes).bimap(refused(d, c), Some(_))

  /** The studio fields Save & run records (S7.1): the caller's, if their
    * preset is held by the saved recipe; else the base's, its preset resolved
    * again only when the draft changes a field a preset declares, so an
    * unedited legacy revision keeps its preset and its science digest.
    *
    * So a revision saved before presets declared the layout (S7.1), with the
    * layout `participant-stimulus-phase` and preset `EncodingRetrieval`, and
    * rerun with only an undeclared field changed (the grid, a scale), keeps
    * `EncodingRetrieval` although its recipe no longer holds that preset; the
    * picker, which resolves the recipe, shows it as Custom. Changing a
    * declared field, or choosing a preset, records the resolved one.
    */
  private def studioFields(
      draft: Draft,
      base: AnalysisRevisionSpec,
      recipe: Recipe,
      studio: Option[StudioFields]
  ): Either[CommandError, StudioFields] =
    studio match
      case Some(fields) =>
        Either.cond(
          RecipePresets.of(fields.preset).forall(_.holds(recipe)),
          fields,
          PresetNotHeld(fields.preset, draft.id)
        )
      case None =>
        val touched = draft.changes.exists(ch => RecipePresets.declaredFields(ch.field))
        Right(
          if touched then
            base.studio.copy(preset = RecipePresets.resolve(base.studio.preset, recipe))
          else base.studio
        )

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
      _     <- Either.cond(get(panel) != value, (), NoChange(c.name, targetOf(d, c)))
      panels = old.panels.map(p => if p.letter == letter then set(p, value) else p)
      spec <- FigureSpec.of(id, old.run, old.reporting, panels).left.map(refused(d, c))
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
      p   <- next.left.map(refused(d, c))
      _   <- Either.cond(p != d.presentation, (), NoChange(c.name, targetOf(d, c)))
      doc <- d.withPresentation(p).left.map(refused(d, c))
    yield reversible(doc, inverse(old))
