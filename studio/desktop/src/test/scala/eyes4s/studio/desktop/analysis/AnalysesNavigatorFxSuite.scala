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
