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

import eyes4s.codec.CanonicalDigest
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.*
import eyes4s.studio.core.document.DocumentGen.right
import eyes4s.studio.core.fixture.StoryMoments

/** The reducer on the story moments: Save & run turns t2 into t3, admission
  * turns t1's pending r3 into data, and each refusal names what failed.
  */
class CommandReducerSuite extends munit.FunSuite:
  import Command.*
  import StoryMoments.{r2, r3, rev4, rev5, run7, run8}

  private val t1 = DocumentSamples.t1
  private val t2 = DocumentSamples.t2
  private val t3 = DocumentSamples.t3

  private def ok(e: Either[CommandError, Step]): Step =
    e.fold(err => fail(err.message), identity)

  private val figure1 = right(FigureId.of(1))
  private val figure2 = right(FigureId.of(2))
  private val panelA  = right(PanelLetter.of("A"))
  private val eight   = right(Sigma.of(8.0))

  test("Save & run on t2 makes rev 5 and run 8 on r3: the science of t3") {
    val step = ok(History.start(t2).apply(SaveAndRun(None)))
    assertEquals(step.history.document.science, t3.science)
    assertEquals(step.effects, Vector(Effect.RequestRun(run8, rev5, r3), Effect.Persist))
    assertEquals(step.history.document.draft, None)
  }

  test("Save & run is a barrier: undo stops there and never removes run 8") {
    val edited =
      ok(History.start(t2).apply(SetPanelScale(figure1, panelA, PanelScale.AllScales)))
    val saved = ok(edited.history.apply(SaveAndRun(None))).history
    assertEquals(
      saved.undo.map(_.history),
      Left(CommandError.UndoBlocked(HistoryBarrier.RunRequested(rev5, run8)))
    )
    assertEquals(
      saved.redo.map(_.history),
      Left(CommandError.NothingToRedo(HistoryStack.Science))
    )
    assert(saved.document.run(run8).nonEmpty)
    assert(
      CommandError
        .UndoBlocked(HistoryBarrier.RunRequested(rev5, run8))
        .message
        .contains("run 8")
    )
  }

  test("a barrier does not block undoing a view change") {
    val h     = ok(History.start(t2).apply(SetTheme(Theme.Dark))).history
    val saved = ok(h.apply(SaveAndRun(None))).history
    val back  = ok(saved.undoView).history
    assertEquals(back.document.presentation, t2.presentation)
    assertEquals(back.document.science, saved.document.science)
  }

  private val verified                                           = CommandSamples.verified
  private def admitR3As(v: CanonicalDigest[DatasetRevisionSpec]) =
    Admit(r3, v, CoreBinding.unbound, CoreBinding.unbound)
  private val admitR3 = admitR3As(verified)

  test("verifying r3 records its content digest and requests admission of exactly that") {
    val verify = ok(History.start(t1).apply(VerifyDataset(r3)))
    assertEquals(
      verify.history.document.dataset(r3).map(_.decision),
      Some(AdmissionDecision.Verifying(verified))
    )
    assertEquals(verify.effects, Vector(Effect.RequestAdmission(r3, verified), Effect.Persist))
    // Verifying does not change the content it digests.
    assertEquals(
      DatasetRevisionSpec.contentDigest(verify.history.document.dataset(r3).get),
      Right(verified)
    )
    assertEquals(ok(verify.history.undo).history.document, t1)
  }

  test("admitting the verified r3 is a barrier") {
    val verified = ok(History.start(t1).apply(VerifyDataset(r3))).history
    val admit    = ok(verified.apply(admitR3))
    assert(admit.history.document.dataset(r3).exists(_.decision.isAdmitted))
    assertEquals(
      admit.history.undo.map(_.history),
      Left(CommandError.UndoBlocked(HistoryBarrier.DatasetAdmitted(r3)))
    )
  }

  test("an admission needs a verification, for the digest that was verified") {
    assertEquals(Reducer.step(t1, admitR3), Left(CommandError.NotVerified(r3)))
    val verifying = Reducer.step(t1, VerifyDataset(r3)).toOption.get._1
    val other     = CanonicalDigest.parse[DatasetRevisionSpec]("ab" * 32).toOption.get
    assertEquals(
      Reducer.step(verifying, admitR3As(other)),
      Left(CommandError.VerificationMismatch(r3, verified, other))
    )
  }

  test("a revision verifying content it no longer has cannot be admitted") {
    val other = CanonicalDigest.parse[DatasetRevisionSpec]("ab" * 32).toOption.get
    val stale = t1.dataset(r3).get.copy(decision = AdmissionDecision.Verifying(other))
    val doc   = Reducer
      .step(t1, DiscardDataset(r3))
      .flatMap((d, _) => Reducer.step(d, RestoreDataset(stale)))
      .toOption
      .get
      ._1
    assertEquals(
      Reducer.step(doc, admitR3As(other)),
      Left(CommandError.ChangedSinceVerification(r3, other, verified))
    )
  }

  test("while r3 is verifying its content is not edited; withdrawing makes it editable") {
    val verifying = ok(History.start(t1).apply(VerifyDataset(r3))).history
    val edit      = SetOffScreenPolicy(r3, OffScreenChoice.QuarantineTrial)
    assertEquals(
      verifying.apply(edit).map(_.history),
      Left(CommandError.VerificationPending(r3, verified))
    )
    assertEquals(
      verifying.apply(RemoveCorrection(r3, 0)).map(_.history),
      Left(CommandError.VerificationPending(r3, verified))
    )
    val withdrawn = ok(verifying.apply(WithdrawVerification(r3))).history
    assertEquals(withdrawn.document, t1)
    val edited = ok(withdrawn.apply(edit)).history
    // A new verification digests the new content, and the old admission is refused.
    val again = ok(edited.apply(VerifyDataset(r3)))
    val now   = DatasetRevisionSpec.contentDigest(edited.document.dataset(r3).get).toOption.get
    assertNotEquals(now, verified)
    assertEquals(again.effects.head, Effect.RequestAdmission(r3, now))
    assertEquals(
      again.history.apply(admitR3).map(_.history),
      Left(CommandError.VerificationMismatch(r3, now, verified))
    )
  }

  test("cancelling run 8 asks the backend to cancel its job and changes nothing else") {
    val step = ok(History.start(t3).apply(CancelRun(run8)))
    assertEquals(step.history, History.start(t3))
    assertEquals(step.effects, Vector(Effect.CancelJob(run8, StoryMoments.run8Job)))
    assertEquals(
      Reducer.step(t3, CancelRun(run7)),
      Left(CommandError.RunNotRunning(run7, RunLifecycle.Completed))
    )
    val lost = t3.withJobs(Vector.empty).toOption.get
    assertEquals(Reducer.step(lost, CancelRun(run8)), Left(CommandError.NoJobHandle(run8)))
  }

  test("binding rev 4's plan records the plan and input digests once") {
    val plan  = CanonicalDigest.parse[StudyPlanArtifact]("0123456789abcdef" * 4).toOption.get
    val input = right(SemanticIdentity.of("00112233445566ff"))
    val step  = ok(History.start(t2).apply(BindPlan(rev4, plan, input)))
    val bound = step.history.document.analysis(rev4).get
    assertEquals(bound.plan, CoreBinding.Bound(plan))
    assertEquals(bound.recipe.input, Some(input))
    assertEquals(step.history.science, History.start(t2).science)
    assertEquals(
      Reducer.step(step.history.document, BindPlan(rev4, plan, input)),
      Left(CommandError.PlanAlreadyBound(rev4, plan))
    )
    val other = right(SemanticIdentity.of("ffeeddccbbaa9988"))
    val t2In  = Reducer.step(t2, BindPlan(AnalysisRevision(3), plan, other)).toOption.get._1
    assertEquals(t2In.analysis(AnalysisRevision(3)).map(_.recipe.input), Some(Some(other)))
    // rev 4 already carries the draft, whose recipe has no input change: binding is safe.
    assertEquals(step.history.document.draft, t2.draft)
  }

  test("a panel is added, retitled and removed, and each undoes") {
    val f     = t2.figures.head
    val panel = f.panels(3).copy(letter = right(PanelLetter.of("F")))
    val added = ok(History.start(t2).apply(AddPanel(figure1, 1, panel))).history
    assertEquals(
      added.document.figures.head.panels.map(_.letter.value),
      Vector("A", "F", "B", "C", "D", "E")
    )
    assertEquals(ok(added.undo).history.document, t2)
    val retitled =
      ok(History.start(t2).apply(RetitlePanel(figure1, panelA, "Encoding · P17"))).history
    assertEquals(retitled.document.figures.head.panels.head.title, "Encoding · P17")
    assertEquals(ok(retitled.undo).history.document, t2)
    val removed =
      ok(History.start(t2).apply(RemovePanel(figure1, right(PanelLetter.of("C"))))).history
    assertEquals(removed.science.done.head.inverse, AddPanel(figure1, 2, f.panels(2)))
    assertEquals(ok(removed.undo).history.document, t2)
    assertEquals(
      Reducer.step(t2, AddPanel(figure1, 9, panel)),
      Left(CommandError.PanelIndex(figure1, 9, 5))
    )
    assertEquals(
      Reducer.step(t2, AddPanel(figure1, 0, f.panels(0))),
      Left(
        CommandError.Refused(
          "AddPanel",
          Target.OnPanel(figure1, panelA),
          DocumentError.DuplicatePanels(figure1, Vector("A"))
        )
      )
    )
    assertEquals(
      Reducer.step(t2, RemovePanel(figure2, panelA)),
      Left(
        CommandError.Refused(
          "RemovePanel",
          Target.OnPanel(figure2, panelA),
          DocumentError.NoPanels(figure2)
        )
      )
    )
  }

  test("refusals name the targeted id") {
    assertEquals(
      Reducer.step(t3, DiscardDraft),
      Left(CommandError.NoDraft("DiscardDraft", Target.OnDraft(Some(AnalysisRevision(6)))))
    )
    assertEquals(
      Reducer.step(t2, SetTheme(t2.presentation.theme)),
      Left(CommandError.NoChange("SetTheme", Target.OnPresentation))
    )
    assertEquals(
      Reducer.step(t2, RetitlePanel(figure1, panelA, "Encoding gaze")),
      Left(CommandError.NoChange("RetitlePanel", Target.OnPanel(figure1, panelA)))
    )
    assert(
      CommandError
        .NoDraft("SaveAndRun", Target.OnDraft(Some(AnalysisRevision(6))))
        .message
        .contains("draft rev 6")
    )
  }

  test("an admitted dataset is not edited in place") {
    assertEquals(
      Reducer.step(t1, SetOffScreenPolicy(r2, OffScreenChoice.QuarantineTrial)),
      Left(CommandError.DatasetNotPending(r2))
    )
    assertEquals(
      Reducer
        .step(t1, SetOffScreenPolicy(r3, OffScreenChoice.QuarantineTrial))
        .map(_._1.dataset(r3).map(_.admission.offScreen)),
      Right(Some(OffScreenChoice.QuarantineTrial))
    )
  }

  test("a re-import takes the next id and its parent's admission choices") {
    val parent    = t1.dataset(r3).get
    val (next, _) = Reducer
      .step(
        t1,
        ImportSources(
          Some(r3),
          parent.sources,
          parent.mapping,
          parent.units,
          parent.geometry,
          DeclaredAttributes.empty,
          None
        )
      )
      .toOption
      .get
    val r4 = next.dataset(DatasetRevision(4)).get
    assertEquals(r4.parent, Some(r3))
    assertEquals(r4.admission, parent.admission)
    assertEquals(r4.decision, AdmissionDecision.Pending)
  }

  test("reverting the draft's only change removes it, and undo restores draft rev 5 exactly") {
    val only = t2.draft.get.changes.head
    val step = ok(History.start(t2).apply(ChangeRecipe(only.inverse)))
    assertEquals(step.history.document.draft, None)
    assertEquals(step.history.science.done.head.inverse, RestoreDraft(t2.draft.get))
    assertEquals(ok(step.history.undo).history.document, t2)
  }

  test("a recipe change without a draft drafts the latest revision") {
    val t3Done = ok(
      History
        .start(t3)
        .apply(RecordRunOutcome(run8, RunLifecycle.Completed, CoreBinding.unbound))
    ).history
    val latest = t3Done.document.latestAnalysis.get
    val change = RecipeChange.Grid(latest.recipe.grid, right(GridSize.of(32, 24)))
    val step   = ok(t3Done.apply(ChangeRecipe(change)))
    assertEquals(
      step.history.document.draft.map(d => (d.id, d.base)),
      Some((AnalysisRevision(6), rev5))
    )
    assertEquals(step.history.science.done.head.inverse, DiscardDraft)
  }

  test("a change that does not start from the draft's value is stale and names the field") {
    val scales = t2.analysis(rev4).get.recipe.scales
    assertEquals(
      Reducer
        .step(t2, ChangeRecipe(RecipeChange.Scales(scales, right(ScaleSet.of(Vector(eight)))))),
      Left(
        CommandError.StaleChange(
          RecipeField.Scales,
          scales.render,
          t2.draftRecipe.get.scales.render
        )
      )
    )
  }

  test("rebasing onto the base's own dataset changes nothing; onto pending data is refused") {
    assertEquals(
      Reducer.step(t2, RebaseDraft(r3)),
      Left(CommandError.NoChange("RebaseDraft", Target.OnDraft(Some(rev5))))
    )
    val rebased = ok(History.start(t2).apply(RebaseDraft(r2)))
    assertEquals(rebased.history.document.draft.flatMap(_.dataset), Some(r2))
    assertEquals(ok(rebased.history.undo).history.document, t2)
    assertEquals(
      Reducer.step(t1, RebaseDraft(r3)),
      Left(
        CommandError.Refused(
          "RebaseDraft",
          Target.OnDraft(Some(AnalysisRevision(4))),
          DocumentError.RebaseNotAdmitted(AnalysisRevision(4), r3)
        )
      )
    )
  }

  test("recording run 8's outcome drops its job handle and leaves the undo stacks") {
    val h =
      ok(History.start(t3).apply(SetPanelScale(figure1, panelA, PanelScale.AllScales))).history
    val done =
      ok(h.apply(RecordRunOutcome(run8, RunLifecycle.Completed, CoreBinding.unbound))).history
    assertEquals(done.document.jobs, Vector.empty)
    assertEquals(done.science, h.science)
    // The earlier figure edit is still undoable across the backend fact.
    val back = ok(done.undo).history.document
    assertEquals(back.figures, t3.figures)
    assertEquals(back.run(run8).map(_.state), Some(RunLifecycle.Completed))
    assertEquals(
      Reducer
        .step(done.document, RecordRunOutcome(run8, RunLifecycle.Failed, CoreBinding.unbound)),
      Left(CommandError.RunNotRunning(run8, RunLifecycle.Completed))
    )
  }

  test("a reporting spec bound by figures cannot be removed") {
    val id = t2.reporting.head.id
    assertEquals(
      Reducer.step(t2, RemoveReporting(id)),
      Left(CommandError.ReportingInUse(id, Vector(figure1, figure2)))
    )
  }

  test("a panel scale must belong to the figure's run") {
    assertEquals(
      Reducer
        .step(t2, SetPanelScale(figure1, panelA, PanelScale.At(eight)))
        .left
        .map(_.productPrefix),
      Left("Refused")
    )
    assertEquals(
      Reducer
        .step(t2, SetPanelScale(figure1, right(PanelLetter.of("Z")), PanelScale.AllScales)),
      Left(CommandError.UnknownPanel(figure1, right(PanelLetter.of("Z"))))
    )
  }

  test("rebinding Figure 2 from run 5 to run 7, and undoing it") {
    val step = ok(History.start(t2).apply(BindFigure(figure2, run7, t2.reporting.head.id)))
    assertEquals(step.history.document.figures(1).run, run7)
    assertEquals(ok(step.history.undo).history.document, t2)
  }

  test("an empty history refuses undo and redo with the stack's name") {
    val h = History.start(t2)
    assertEquals(h.undo.map(_.history), Left(CommandError.NothingToUndo(HistoryStack.Science)))
    assertEquals(
      h.redoView.map(_.history),
      Left(CommandError.NothingToRedo(HistoryStack.Presentation))
    )
  }

  test("showing a run that is not in the document is refused") {
    assertEquals(
      Reducer.step(t2, ShowRun(Some(RunId(42)))),
      Left(
        CommandError.Refused(
          "ShowRun",
          Target.OnPresentation,
          DocumentError.UnknownRun("the presentation", RunId(42))
        )
      )
    )
  }

  test("each command has one of the four kinds of change") {
    assertEquals(SaveAndRun(None).kind.label, "Analysis · rerun")
    assertEquals(SetTheme(Theme.Dark).kind, ChangeKind.ViewOnly)
    assertEquals(DiscardDraft.kind, ChangeKind.AnalysisRerun)
    assertEquals(SetMapping(r3, t1.dataset(r3).get.mapping).kind.label, "Dataset · re-admit")
    assertEquals(DeleteFigure(figure1).kind.label, "Reporting · no rerun")
  }
