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

import eyes4s.studio.app.{AppEffect, AppModel, Intent, StoryModels}
import eyes4s.studio.app.nav.Place
import eyes4s.studio.core.backend.AnalysisRevision
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.fixture.StoryMoments.*
import eyes4s.studio.core.execution.ExecutionEffect

class AnalysesNavigatorSuite extends munit.FunSuite:
  private def rows(model: AppModel) = AnalysesNavigator.vm(model).groups.flatMap(_.rows)

  test("history retains exact revision, run, dataset and freshness facts") {
    val history = rows(StoryModels.t2Analysis)
    assertEquals(
      history.map(_.label),
      Vector("Draft rev 5", "rev 4 · run 7", "rev 4 · run 6", "rev 3 · run 5")
    )
    assertEquals(history.find(_.run.contains(run5)).map(_.dataset), Some(r2))
    assert(history.find(_.run.contains(run5)).get.detail.contains("stale"))
    assert(history.find(_.run.contains(run6)).get.detail.contains("cancelled"))
    assert(history.find(_.run.contains(run7)).get.detail.contains("current"))
    assertEquals(history.filter(_.selected).map(_.revision), Vector(rev5))
  }

  test(
    "selecting a cancelled run inspects its revision and preserves the shown result and science"
  ) {
    val before           = StoryModels.t2Analysis
    val row              = rows(before).find(_.run.contains(run6)).get
    val (after, effects) = AppModel.update(before, row.open)
    assertEquals(after.location.trail.last, Place.Run(run6))
    assertEquals(ResolvedDesign.target(after).map(_.revision), Some(rev4))
    assertEquals(after.document.presentation.shownRun, before.document.presentation.shownRun)
    assertEquals(after.document.science, before.document.science)
    assertEquals(rows(after).filter(_.selected).map(_.run), Vector(Some(run6)))
    assert(!effects.exists {
      case AppEffect.Execution(_: ExecutionEffect.Submit) => true; case _ => false
    })
    assert(
      PresetPicker
        .vm(after.document, AnalysesNavigator.selected(after))
        .options
        .forall(_.choose.isEmpty)
    )
    val restored = AppModel.update(after, Intent.Back)._1
    assertEquals(AnalysesNavigator.selected(restored), Some(rev5))
  }

  test(
    "initial and independent creation are undoable and respect the held draft"
  ) {
    import eyes4s.studio.core.document.{PresentationState, StudioDocument}
    import eyes4s.studio.core.command.HistoryStack
    val held = StoryModels.t2Analysis
    assertEquals(AnalysesNavigator.vm(held).create, None)
    assertEquals(AppModel.update(held, Intent.NewAnalysis)._1.document, held.document)
    val clean = AppModel.update(held, Intent.Dispatch(Command.DiscardDraft))._1
    assertEquals(AnalysesNavigator.vm(clean).create, Some(Intent.NewAnalysis))
    val independent = AppModel.update(clean, Intent.NewAnalysis)._1
    assert(independent.document.draft.exists(_.isNewFamily))
    assertEquals(independent.document.analyses, clean.document.analyses)
    assertEquals(
      AppModel.update(independent, Intent.Undo(HistoryStack.Science))._1.document.science,
      clean.document.science
    )
    val data = held.document.datasets.last
      .copy(id = eyes4s.studio.core.backend.DatasetRevision(1), parent = None)
    val document = StudioDocument
      .of(
        Vector(data),
        Vector.empty,
        None,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        PresentationState.default,
        Vector.empty
      )
      .toOption
      .get
    val initial = AppModel.open(document, None)
    assertEquals(AnalysesNavigator.vm(initial).create, Some(Intent.NewAnalysis))
    val created = AppModel.update(initial, Intent.NewAnalysis)._1
    assertEquals(created.document.draft.map(_.id), Some(AnalysisRevision(1)))
    assertEquals(created.location.trail.last, Place.Revision(AnalysisRevision(1)))
    assertEquals(created.document.runs, Vector.empty)
    val undone = AppModel.update(created, Intent.Undo(HistoryStack.Science))._1
    assertEquals(undone.document.science, document.science)
  }

  test("empty and running projects expose only actions supported by their document") {
    val empty = AppModel.newProject.toOption.get
    assert(rows(empty).isEmpty)
    assertEquals(AnalysesNavigator.vm(empty).create, None)
    val running = rows(StoryModels.t3Summary)
    assert(running.exists(_.detail.contains("running")))
  }

  test("a preset edit from saved history selects the draft and keeps its picker editable") {
    import eyes4s.studio.core.document.Preset
    val clean =
      AppModel.update(StoryModels.t2Analysis, Intent.Dispatch(Command.DiscardDraft))._1
    val saved   = rows(clean).find(_.run.contains(run7)).get
    val viewing = AppModel.update(clean, saved.open)._1
    val option  = PresetPicker
      .vm(viewing.document, AnalysesNavigator.selected(viewing))
      .options
      .find(_.preset == Preset.PerceptionImagery)
      .get
    assert(option.choose.isDefined)
    val (changed, effects) = AppModel.update(viewing, option.choose.get)
    assertEquals(changed.location.trail.last, Place.Revision(rev5))
    assertEquals(ResolvedDesign.target(changed).map(_.revision), Some(rev5))
    val picker = PresetPicker.vm(changed.document, AnalysesNavigator.selected(changed))
    assertEquals(
      picker.options.filter(_.selected).map(_.preset),
      Vector(Preset.PerceptionImagery)
    )
    assert(picker.options.find(_.preset == Preset.Recognition).get.choose.isDefined)
    assertEquals(changed.document.analyses, viewing.document.analyses)
    assertEquals(changed.document.runs, viewing.document.runs)
    assertEquals(changed.document.presentation.shownRun, viewing.document.presentation.shownRun)
    assert(!effects.exists {
      case AppEffect.Execution(_: ExecutionEffect.Submit) => true; case _ => false
    })
  }

  test("choosing the already-held preset is a no-op that preserves saved-history selection") {
    import eyes4s.studio.core.document.Preset
    val clean =
      AppModel.update(StoryModels.t2Analysis, Intent.Dispatch(Command.DiscardDraft))._1
    val viewing = AppModel.update(clean, rows(clean).find(_.run.contains(run7)).get.open)._1
    val (same, effects) =
      AppModel.update(viewing, Intent.ChoosePreset(Preset.EncodingRetrieval))
    assertEquals(same, viewing)
    assertEquals(effects, Vector.empty)
  }
