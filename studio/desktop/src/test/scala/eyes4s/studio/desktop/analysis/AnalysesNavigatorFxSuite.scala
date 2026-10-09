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

package eyes4s.studio.desktop.analysis

import eyes4s.studio.app.{Intent, StoryModels}
import eyes4s.studio.app.analysis.ResolvedDesign
import eyes4s.studio.app.nav.Place
import eyes4s.studio.core.fixture.StoryMoments.*
import eyes4s.studio.desktop.harness.StudioTheme
import eyes4s.studio.desktop.shell.ShellFxSuite

/** S7.7: Analysis.dc.html history rows and immutable revision selection. */
class AnalysesNavigatorFxSuite extends ShellFxSuite:
  fxStage.test(
    "Analysis board history selects cancelled and stale runs without replacing the shown result"
  ) { fx =>
    val w      = boot(fx, StoryModels.t2Analysis)
    val labels = runOnFx(w.analyses.labels)
    assertEquals(labels.size, 4)
    assert(labels.exists(_.startsWith("Draft rev 5")))
    assert(labels.exists(l => l.contains("run 5") && l.contains("r2") && l.contains("stale")))
    assert(labels.exists(l => l.contains("run 6") && l.contains("cancelled")))
    assert(labels.exists(l => l.contains("run 7") && l.contains("current")))
    assert(runOnFx(w.analyses.create.isDisabled))
    val before = runOnFx(w.runtime.model.document.science)
    val shown  = runOnFx(w.runtime.model.document.presentation.shownRun)
    runOnFx(w.analyses.select(labels.find(_.contains("run 6")).get))
    fx.awaitLayout()
    assertEquals(runOnFx(w.runtime.model.location.trail.last), Place.Run(run6))
    assertEquals(runOnFx(ResolvedDesign.target(w.runtime.model).map(_.revision)), Some(rev4))
    assertEquals(runOnFx(w.runtime.model.document.science), before)
    assertEquals(runOnFx(w.runtime.model.document.presentation.shownRun), shown)
    val selected = labels.find(_.contains("run 6")).get
    runOnFx(w.analyses.select(selected))
    fx.awaitLayout()
    assertEquals(runOnFx(w.analyses.selectedLabels), Vector(selected))
    dispatch(fx, w, Intent.Back)
    stageSnapshot(fx)
  }

  private def stageSnapshot(fx: eyes4s.studio.desktop.harness.FxStage): Unit =
    fx.snapshot(StudioTheme.Light): Unit

  fxStage.test("initial analysis creation uses the existing command and undo seam") { fx =>
    import eyes4s.studio.app.AppModel
    import eyes4s.studio.core.document.{PresentationState, StudioDocument}
    import eyes4s.studio.core.backend.{AnalysisRevision, DatasetRevision}
    import eyes4s.studio.core.command.HistoryStack
    val data =
      StoryModels.t2Analysis.document.datasets.last.copy(id = DatasetRevision(1), parent = None)
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
    val w = boot(fx, AppModel.open(document, None))
    assert(!runOnFx(w.analyses.create.isDisabled))
    runOnFx(w.analyses.create.fire())
    fx.awaitLayout()
    assertEquals(runOnFx(w.runtime.model.document.draft.map(_.id)), Some(AnalysisRevision(1)))
    assert(runOnFx(w.analyses.create.isDisabled))
    dispatch(fx, w, Intent.Undo(HistoryStack.Science))
    assertEquals(runOnFx(w.runtime.model.document.draft), None)
  }

  fxStage.test(
    "keyboard activation retains its keyed history control and can continue with Tab"
  ) { fx =>
    import javafx.scene.input.KeyCode
    val w       = boot(fx, StoryModels.t2Analysis)
    val label   = runOnFx(w.analyses.labels.find(_.contains("run 6")).get)
    val control = runOnFx(w.analyses.button(label).get)
    runOnFx(control.requestFocus())
    fx.awaitLayout()
    fx.robot.press(KeyCode.SPACE)
    fx.awaitLayout()
    assert(runOnFx(fx.scene.getFocusOwner eq control))
    assertEquals(runOnFx(w.runtime.model.location.trail.last), Place.Run(run6))
    fx.robot.press(KeyCode.TAB)
    assert(runOnFx(fx.scene.getFocusOwner ne control))
    assert(runOnFx(fx.scene.getFocusOwner != null))
  }

  fxStage.test(
    "long history remains reachable and focused rows are revealed in the scroll viewport"
  ) { fx =>
    import eyes4s.studio.app.AppModel
    import eyes4s.studio.core.backend.AnalysisRevision
    import eyes4s.studio.core.document.StudioDocument
    val before   = StoryModels.t2Analysis.document
    val base     = before.analyses.last
    val more     = Vector.tabulate(30)(i => base.copy(id = AnalysisRevision(i + 5)))
    val document = StudioDocument
      .of(
        before.datasets,
        before.analyses ++ more,
        None,
        before.runs,
        before.reporting,
        before.figures,
        before.presentation,
        before.jobs
      )
      .toOption
      .get
    val w       = boot(fx, AppModel.open(document, None))
    val last    = runOnFx(w.analyses.labels.last)
    val control = runOnFx(w.analyses.button(last).get)
    runOnFx(control.requestFocus())
    fx.awaitLayout()
    eventually(fx, "focused last row is scrolled into view")(w.analyses.scroll.getVvalue > 0.5)
    val (bounds, viewport) = runOnFx(
      (
        w.analyses.scroll.sceneToLocal(control.localToScene(control.getBoundsInLocal)),
        w.analyses.scroll.getViewportBounds
      )
    )
    assert(bounds.getMinY >= 0)
    assert(bounds.getMinY < viewport.getHeight)
    fx.robot.press(javafx.scene.input.KeyCode.SPACE)
    assertEquals(runOnFx(w.runtime.model.location.trail.last), Place.Run(run5))
  }

  fxStage.test(
    "choosing a preset from the latest saved run displays and prepares its new draft"
  ) { fx =>
    import eyes4s.studio.core.command.Command
    import eyes4s.studio.core.document.Preset
    val w = boot(fx, StoryModels.t2Analysis)
    dispatch(fx, w, Intent.Dispatch(Command.DiscardDraft))
    val saved = runOnFx(w.analyses.labels.find(_.contains("run 7")).get)
    runOnFx(w.analyses.select(saved))
    fx.awaitLayout()
    assert(runOnFx(w.recipe.enabled(Preset.PerceptionImagery)))
    val before = runOnFx(w.runtime.model.document)
    runOnFx(w.recipe.choose(Preset.PerceptionImagery))
    fx.awaitLayout()
    assertEquals(runOnFx(w.runtime.model.location.trail.last), Place.Revision(rev5))
    assertEquals(runOnFx(ResolvedDesign.target(w.runtime.model).map(_.revision)), Some(rev5))
    assert(runOnFx(w.recipe.enabled(Preset.Recognition)))
    assertEquals(
      runOnFx(w.analyses.selectedLabels.map(_.startsWith("Draft rev 5"))),
      Vector(true)
    )
    assertEquals(runOnFx(w.runtime.model.document.analyses), before.analyses)
    assertEquals(runOnFx(w.runtime.model.document.runs), before.runs)
    assertEquals(
      runOnFx(w.runtime.model.document.presentation.shownRun),
      before.presentation.shownRun
    )
    eventually(fx, "new draft is the preview target")(
      w.resolvedDesign.state.target.exists(_.revision == rev5)
    )
  }

  fxStage.test(
    "duplicate family names retain independent history, preset targets and current runs"
  ) { fx =>
    import eyes4s.studio.core.document.*
    import eyes4s.studio.core.backend.RunId
    import eyes4s.studio.app.AppModel
    import FamilySamples.*
    val document = FamilySamples.document(
      Vector(
        run(1, a1, RunLifecycle.Completed),
        run(2, b2, RunLifecycle.Completed),
        run(3, a3, RunLifecycle.Completed)
      ),
      shown = Some(RunId(1))
    )
    val w = boot(
      fx,
      AppModel
        .update(AppModel.open(document, None), Intent.SwitchPerspective(Perspective.Analysis))
        ._1
    )
    assertEquals(
      runOnFx(w.analyses.groupLabels),
      Vector("Same name · analysis 2", "Same name · analysis 1")
    )
    stageSnapshot(fx)
    val historical = runOnFx(w.analyses.labels.find(_.startsWith("rev 1 ·")).get)
    runOnFx(w.analyses.select(historical))
    fx.awaitLayout()
    assertEquals(runOnFx(w.analyses.current.getText), "Current run · rev 3 · run 3")
    assertEquals(runOnFx(w.runtime.model.document.presentation.shownRun), Some(RunId(1)))
    assert(runOnFx(w.recipe.enabled(Preset.Recognition)))
    runOnFx(w.recipe.choose(Preset.Recognition))
    fx.awaitLayout()
    assertEquals(
      runOnFx(w.runtime.model.document.draft.map(_.origin)),
      Some(DraftOrigin.Existing(a1.id))
    )
    assertEquals(
      runOnFx(
        w.runtime.model.document.familyOf(eyes4s.studio.core.backend.AnalysisRevision(5))
      ),
      Some(a)
    )
    val other = runOnFx(w.analyses.labels.find(_.startsWith("rev 4 ·")).get)
    runOnFx(w.analyses.select(other))
    fx.awaitLayout()
    assert(!runOnFx(w.recipe.enabled(Preset.PerceptionImagery)))
    assertEquals(runOnFx(w.analyses.current.getText), "Current run · rev 2 · run 2")
    assertEquals(runOnFx(w.runtime.model.document.analyses), document.analyses)
    assertEquals(runOnFx(w.runtime.model.document.runs), document.runs)
  }

  fxStage.test("family name updates preserve keyed controls and keyboard focus") { fx =>
    import eyes4s.studio.core.document.*
    import eyes4s.studio.app.AppModel
    import FamilySamples.*
    val document = FamilySamples.document()
    val w        = boot(
      fx,
      AppModel
        .update(AppModel.open(document, None), Intent.SwitchPerspective(Perspective.Analysis))
        ._1
    )
    val label   = runOnFx(w.analyses.labels.find(_.startsWith("rev 1 ·")).get)
    val control = runOnFx(w.analyses.button(label).get)
    runOnFx(control.requestFocus())
    fx.awaitLayout()
    assert(runOnFx(fx.scene.getFocusOwner eq control))
    val renamed = get(
      AnalysisFamilyRegistry.of(
        Vector(get(AnalysisFamily.of(a, "Renamed")), get(AnalysisFamily.of(b, "Same name"))),
        registry.owners,
        analyses.map(_.id)
      )
    )
    val next = get(
      StudioDocument.of(
        document.datasets,
        document.analyses,
        None,
        document.runs,
        document.reporting,
        document.figures,
        document.presentation,
        document.jobs,
        Some(renamed)
      )
    )
    runOnFx(w.analyses.sync(AppModel.open(next, None)))
    fx.awaitLayout()
    assertEquals(runOnFx(w.analyses.groupLabels), Vector("Same name", "Renamed"))
    assert(runOnFx(w.analyses.button(label).get eq control))
    assert(runOnFx(fx.scene.getFocusOwner eq control))
    fx.robot.press(javafx.scene.input.KeyCode.TAB)
    assert(runOnFx(fx.scene.getFocusOwner ne control))
  }

  fxStage.test(
    "New analysis creates a separate editable family and undo restores saved history"
  ) { fx =>
    import eyes4s.studio.core.document.*
    import eyes4s.studio.core.backend.AnalysisRevision
    import eyes4s.studio.core.command.HistoryStack
    import eyes4s.studio.app.AppModel
    import FamilySamples.*
    val document = FamilySamples.document()
    val w        = boot(
      fx,
      AppModel
        .update(AppModel.open(document, None), Intent.SwitchPerspective(Perspective.Analysis))
        ._1
    )
    runOnFx(w.analyses.create.fire())
    fx.awaitLayout()
    assertEquals(
      runOnFx(w.runtime.model.document.familyOf(AnalysisRevision(5))),
      Some(get(AnalysisFamilyId.of(3)))
    )
    assertEquals(runOnFx(w.analyses.groupLabels.head), "Analysis 3")
    assert(runOnFx(w.analyses.create.isDisabled))
    assert(runOnFx(w.runtime.model.document.draft.exists(_.isNewFamily)))
    assertEquals(runOnFx(w.analyses.current.getText), "No current run for this analysis.")
    runOnFx(w.recipe.choose(Preset.PerceptionImagery))
    fx.awaitLayout()
    assertEquals(
      runOnFx(w.runtime.model.document.draftContext.map(_.recipe.phases.focal.label)),
      Some("Imagery")
    )
    assertEquals(runOnFx(w.runtime.model.document.analyses), document.analyses)
    dispatch(fx, w, Intent.Undo(HistoryStack.Science))
    dispatch(fx, w, Intent.Undo(HistoryStack.Science))
    assertEquals(runOnFx(w.runtime.model.document.science), document.science)
    assert(!runOnFx(w.analyses.create.isDisabled))
  }

  fxStage.test("reopened multi-family overflow keeps old history reachable by keyboard") { fx =>
    import eyes4s.studio.core.document.*
    import eyes4s.studio.core.backend.AnalysisRevision
    import eyes4s.studio.app.AppModel
    import FamilySamples.*
    val original = FamilySamples.document()
    val extra    = Vector.tabulate(30)(i => a1.copy(id = AnalysisRevision(i + 5)))
    val all      = original.analyses ++ extra
    val owners   = registry.owners ++ extra.zipWithIndex.map((value, i) =>
      get(AnalysisFamilyOwner.of(value.id, if i % 2 == 0 then a else b))
    )
    val families = get(AnalysisFamilyRegistry.of(registry.families, owners, all.map(_.id)))
    val saved    = get(
      StudioDocument.of(
        original.datasets,
        all,
        None,
        original.runs,
        original.reporting,
        original.figures,
        original.presentation,
        original.jobs,
        Some(families)
      )
    )
    val reopened = get(StudioDocument.decode(get(StudioDocument.encode(saved))))
    val w        = boot(
      fx,
      AppModel
        .update(AppModel.open(reopened, None), Intent.SwitchPerspective(Perspective.Analysis))
        ._1
    )
    assertEquals(runOnFx(w.analyses.groupLabels.size), 2)
    val last    = runOnFx(w.analyses.labels.last)
    val control = runOnFx(w.analyses.button(last).get)
    runOnFx(control.requestFocus())
    fx.awaitLayout()
    eventually(fx, "reopened family history is scrolled into view")(
      w.analyses.scroll.getVvalue > 0.5
    )
    val (bounds, viewport) = runOnFx {
      val visible = w.analyses.scroll.lookup(".viewport")
      (
        visible.sceneToLocal(control.localToScene(control.getLayoutBounds)),
        visible.getLayoutBounds
      )
    }
    assert(bounds.getMinY >= viewport.getMinY, s"row $bounds, viewport $viewport")
    assert(bounds.getMaxY <= viewport.getMaxY, s"row $bounds, viewport $viewport")
    fx.robot.press(javafx.scene.input.KeyCode.SPACE)
    fx.awaitLayout()
    assertEquals(runOnFx(ResolvedDesign.target(w.runtime.model).map(_.revision)), Some(a1.id))
    assert(runOnFx(fx.scene.getFocusOwner eq control))
    assertEquals(runOnFx(w.runtime.model.document.science), saved.science)
  }

  fxStage.test("the current result button sends Show only when explicitly activated") { fx =>
    import eyes4s.studio.app.AppModel
    import eyes4s.studio.core.execution.{ExecutionEvent, ExecutionJob, JobPhase, RunReady}
    import eyes4s.studio.core.document.Perspective
    val job = ExecutionJob(
      run8Job,
      run8,
      StoryModels.run8Stamp,
      JobPhase.Succeeded(StoryModels.run8Progress(44845L))
    )
    val completed =
      AppModel.update(StoryModels.t3Summary, Intent.Execution(ExecutionEvent.Changed(job)))._1
    val ready = AppModel
      .update(
        completed,
        Intent.Execution(ExecutionEvent.Ready(RunReady(run8Job, run8, job.stamp)))
      )
      ._1
    val w = boot(fx, AppModel.update(ready, Intent.SwitchPerspective(Perspective.Analysis))._1)
    assertEquals(runOnFx(w.runtime.model.document.presentation.shownRun), Some(run7))
    assert(runOnFx(w.analyses.showCurrent.isVisible))
    runOnFx(w.analyses.showCurrent.fire())
    fx.awaitLayout()
    assertEquals(runOnFx(w.runtime.model.document.presentation.shownRun), Some(run8))
    assert(!runOnFx(w.analyses.showCurrent.isVisible))
  }
