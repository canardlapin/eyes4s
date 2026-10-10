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

import eyes4s.studio.app.{AppEffect, AppModel, Intent}
import eyes4s.studio.app.nav.{Location, Place}
import eyes4s.studio.core.backend.{AnalysisRevision, RunId}
import eyes4s.studio.core.command.{Command, HistoryStack}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.execution.ExecutionEffect

class FamilyNavigatorSuite extends munit.FunSuite:
  import FamilySamples.*

  private val runs = Vector(
    run(1, a1, RunLifecycle.Completed),
    run(2, b2, RunLifecycle.Completed),
    run(3, a3, RunLifecycle.Completed),
    run(4, b4, RunLifecycle.Running)
  )
  private def model = AppModel.open(document(runs, shown = Some(RunId(1))), None)
  private def inspect(m: AppModel, revision: AnalysisRevision): AppModel =
    val row = AnalysesNavigator.vm(m).groups.flatMap(_.rows).find(_.revision == revision).get
    AppModel.update(m, row.open)._1
  private def choose(m: AppModel, preset: Preset): Option[Intent] =
    PresetPicker
      .vm(m.document, AnalysesNavigator.selected(m))
      .options
      .find(_.preset == preset)
      .flatMap(_.choose)
  private def noSubmit(effects: Vector[AppEffect]): Unit = assert(!effects.exists {
    case AppEffect.Execution(_: ExecutionEffect.Submit) => true
    case _                                              => false
  })

  test("equal names are separate nominal groups, with every revision and run exactly once") {
    val groups = AnalysesNavigator.vm(model).groups
    assertEquals(groups.map(_.family), Vector(b, a))
    assertEquals(groups.map(_.name), Vector("Same name", "Same name"))
    assertEquals(groups.find(_.family == a).get.rows.map(_.revision), Vector(a3.id, a1.id))
    assertEquals(groups.find(_.family == b).get.rows.map(_.revision), Vector(b4.id, b2.id))
    assertEquals(groups.flatMap(_.rows.flatMap(_.run)).toSet, runs.map(_.id).toSet)
    assertEquals(groups.flatMap(_.rows).size, runs.size)
  }

  test(
    "family crumbs select that family's latest revision, while history keeps its exact base"
  ) {
    val latestA = AppModel
      .update(
        model,
        Intent.Navigate(Location(Perspective.Analysis, Vector(Place.Analyses, Place.Family(a))))
      )
      ._1
    assertEquals(AnalysesNavigator.selected(latestA), Some(a3.id))
    assertEquals(ResolvedDesign.target(latestA).map(_.revision), Some(a3.id))
    val historical = inspect(latestA, a1.id)
    assertEquals(AnalysesNavigator.selected(historical), Some(a1.id))
    assertEquals(
      historical.location.trail,
      AnalysisSelection.trail(historical.document, a1.id) :+ Place.Run(RunId(1))
    )
    assertEquals(historical.document.science, model.document.science)
    assertEquals(historical.document.presentation.shownRun, Some(RunId(1)))
  }

  test(
    "preset choice edits the explicitly inspected historical base, not another family's latest"
  ) {
    val before = inspect(model, a1.id)
    val intent = choose(before, Preset.Recognition).get
    assertEquals(intent, Intent.ChoosePreset(Preset.Recognition, Some(a1.id)))
    val (after, effects) = AppModel.update(before, intent)
    assertEquals(after.document.draft.map(_.origin), Some(DraftOrigin.Existing(a1.id)))
    assertEquals(after.document.familyOf(AnalysisRevision(5)), Some(a))
    assertEquals(after.document.analyses, before.document.analyses)
    assertEquals(after.document.runs, before.document.runs)
    assertEquals(after.document.presentation.shownRun, before.document.presentation.shownRun)
    assertEquals(AnalysesNavigator.selected(after), Some(AnalysisRevision(5)))
    assertEquals(
      after.location.trail,
      AnalysisSelection.trail(after.document, AnalysisRevision(5))
    )
    noSubmit(effects)
    assertEquals(
      AppModel.update(after, Intent.Undo(HistoryStack.Science))._1.document.science,
      before.document.science
    )
  }

  test(
    "a captured preset action keeps its target across navigation and cannot change a held draft"
  ) {
    val onA      = inspect(model, a1.id)
    val action   = choose(onA, Preset.Recognition).get
    val onB      = inspect(onA, b4.id)
    val targeted = AppModel.update(onB, action)._1
    assertEquals(targeted.document.draft.map(_.origin), Some(DraftOrigin.Existing(a1.id)))
    val heldB = AppModel.update(onB, choose(onB, Preset.PerceptionImagery).get)._1
    val (unchanged, effects) = AppModel.update(heldB, action)
    assertEquals(unchanged, heldB)
    assertEquals(effects, Vector.empty)
    val inspectingA = inspect(heldB, a1.id)
    assertEquals(choose(inspectingA, Preset.Recognition), None)
    assertEquals(
      AppModel.update(inspectingA, Intent.ChoosePreset(Preset.Recognition))._1,
      inspectingA
    )
    assertEquals(
      PresetPicker.command(model.document, Preset.Recognition, Some(AnalysisRevision(99))),
      None
    )
  }

  test("current run follows the selected family, not unrelated running or completed activity") {
    val onA = inspect(model, a1.id)
    val onB = inspect(model, b4.id)
    assertEquals(AnalysesNavigator.vm(onA).current.map(_.run), Some(RunId(3)))
    assertEquals(AnalysesNavigator.vm(onB).current.map(_.run), Some(RunId(2)))
    assertEquals(onB.document.presentation.shownRun, Some(RunId(1)))
    assertEquals(AnalysesNavigator.vm(onB).current.flatMap(_.show), None)
    val completed = AppModel
      .update(
        onA,
        Intent.Dispatch(
          Command.RecordRunOutcome(RunId(4), RunLifecycle.Completed, CoreBinding.unbound)
        )
      )
      ._1
    assertEquals(AnalysesNavigator.vm(completed).current.map(_.run), Some(RunId(3)))
    assertEquals(
      AnalysesNavigator.vm(inspect(completed, b4.id)).current.map(_.run),
      Some(RunId(4))
    )
    val draft = AppModel.update(onA, choose(onA, Preset.Recognition).get)._1
    assertEquals(AnalysesNavigator.vm(inspect(draft, b4.id)).current.map(_.run), Some(RunId(2)))
  }

  test("New analysis is independent, undoable, editable and stable through saved reopening") {
    val before             = inspect(model, b4.id)
    val (created, effects) = AppModel.update(before, Intent.NewAnalysis)
    val family             = get(AnalysisFamilyId.of(3))
    val revision           = AnalysisRevision(5)
    assert(created.document.draft.exists(_.isNewFamily))
    assertEquals(created.document.familyOf(revision), Some(family))
    assertEquals(created.document.analysisFamilies, before.document.analysisFamilies)
    assertEquals(AnalysisSelection.name(created.document, family), Some("Analysis 3"))
    assertEquals(
      created.document.draftContext.map(_.dataset),
      before.document.latestAdmitted.map(_.id)
    )
    assertEquals(AnalysesNavigator.vm(created).current, None)
    assertEquals(AnalysesNavigator.vm(created).create, None)
    assertEquals(created.document.analyses, before.document.analyses)
    noSubmit(effects)
    val reopenedDraft = AppModel.open(
      get(StudioDocument.decode(get(StudioDocument.encode(created.document)))),
      None
    )
    val selectedDraft = AppModel.update(reopenedDraft, Intent.ReviewDraft)._1
    assertEquals(AnalysesNavigator.selected(selectedDraft), Some(revision))
    assertEquals(AnalysesNavigator.vm(selectedDraft).groups.head.family, family)
    val edited = AppModel.update(created, choose(created, Preset.PerceptionImagery).get)._1
    val saved  = AppModel.update(edited, Intent.Dispatch(Command.SaveAndRun(None)))._1
    assertEquals(saved.document.familyOf(revision), Some(family))
    assertEquals(saved.document.analysisFamilies.get.owners.size, registry.owners.size + 1)
    val reopened = AppModel.open(
      get(StudioDocument.decode(get(StudioDocument.encode(saved.document)))),
      None
    )
    assertEquals(AnalysesNavigator.vm(reopened).groups.map(_.family).toSet, Set(a, b, family))
    assertEquals(
      reopened.document.analysis(revision).map(_.recipe.phases),
      edited.document.draftRecipe.map(_.phases)
    )
    val undone = AppModel.update(created, Intent.Undo(HistoryStack.Science))._1
    assertEquals(
      ResolvedDesign.target(undone).map(_.revision),
      AnalysesNavigator.selected(undone)
    )
    assert(AnalysesNavigator.selected(undone).nonEmpty)
    assertEquals(
      get(StudioDocument.encode(undone.document)),
      get(StudioDocument.encode(before.document))
    )
    assertEquals(
      AppModel.update(undone, Intent.Redo(HistoryStack.Science))._1.document.science,
      created.document.science
    )
  }

  test(
    "renaming a registry entry or a legacy revision never splits or merges family identity"
  ) {
    val before  = model.document
    val renamed = get(
      AnalysisFamilyRegistry.of(
        Vector(get(AnalysisFamily.of(a, "Renamed")), get(AnalysisFamily.of(b, "Same name"))),
        registry.owners,
        analyses.map(_.id)
      )
    )
    val next = get(
      StudioDocument.of(
        before.datasets,
        before.analyses,
        before.draft,
        before.runs,
        before.reporting,
        before.figures,
        before.presentation,
        before.jobs,
        Some(renamed)
      )
    )
    assertEquals(
      AnalysesNavigator.vm(AppModel.open(next, None)).groups.map(_.family),
      Vector(b, a)
    )
    assertEquals(AnalysisSelection.name(next, a), Some("Renamed"))
    val legacy  = eyes4s.studio.app.StoryModels.t2Analysis.document
    val revised = legacy.analyses.map(value =>
      value.copy(studio = value.studio.copy(name = get(RevisionName.of(value.id.label))))
    )
    val renamedLegacy = get(
      StudioDocument.of(
        legacy.datasets,
        revised,
        None,
        legacy.runs,
        legacy.reporting,
        legacy.figures,
        legacy.presentation,
        legacy.jobs
      )
    )
    val groups = AnalysesNavigator.vm(AppModel.open(renamedLegacy, None)).groups
    assertEquals(groups.size, 1)
    assertEquals(groups.head.family, AnalysisFamilyId.Legacy)
    assertEquals(groups.head.name, revised.last.studio.name.value)
  }

  test(
    "a ready current result changes the displayed run only through its explicit Show action"
  ) {
    import eyes4s.studio.app.StoryModels
    import eyes4s.studio.core.execution.{ExecutionEvent, ExecutionJob, JobPhase, RunReady}
    import eyes4s.studio.core.fixture.StoryMoments.{run7, run8, run8Job}
    val before = StoryModels.t3Summary
    val job    = ExecutionJob(
      run8Job,
      run8,
      StoryModels.run8Stamp,
      JobPhase.Succeeded(StoryModels.run8Progress(44845L))
    )
    val completed = AppModel.update(before, Intent.Execution(ExecutionEvent.Changed(job)))._1
    val ready     = AppModel
      .update(
        completed,
        Intent.Execution(ExecutionEvent.Ready(RunReady(run8Job, run8, job.stamp)))
      )
      ._1
    val action = AnalysesNavigator.vm(ready).current.flatMap(_.show)
    assertEquals(action, Some(Intent.ShowRun(run8)))
    assertEquals(ready.document.presentation.shownRun, Some(run7))
    val (shown, _) = AppModel.update(ready, action.get)
    assertEquals(shown.document.presentation.shownRun, Some(run8))
    assertEquals(shown.document.science, ready.document.science)
    assertEquals(AnalysesNavigator.vm(shown).current.flatMap(_.show), None)
  }
