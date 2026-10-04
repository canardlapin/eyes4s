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

package eyes4s.studio.desktop.shell

import eyes4s.studio.app.{Intent, StoryModels}
import eyes4s.studio.core.execution.{ExecutionEvent, RunReady}
import eyes4s.studio.core.fixture.{StoryMoment, StoryMoments}
import eyes4s.studio.core.selection.StudioRef
import eyes4s.studio.desktop.harness.StudioTheme
import javafx.scene.control.{Button, Label}
import java.nio.file.{Files, StandardCopyOption}
import scala.jdk.CollectionConverters.*

/** E2E-08 on the real session event subscription and JavaFX Show button. */
class RunInProgressFxSuite extends ShellFxSuite:
  fxStage.test(
    "completion stays on run 7; Show promotes run 8 and preserves stable selection"
  ) { fx =>
    assumeFullStage(fx)
    val w        = boot(fx, StoryModels.t3Summary, StoryMoment.T3)
    val selected = Vector(StudioRef.Participant("P17"), StudioRef.Trial(StoryModels.p17ret07))
    dispatch(fx, w, runOnFx(StoryModels.select(w.runtime.model, "compare.summary", selected*)))
    assertEquals(
      runOnFx(w.runtime.model.document.presentation.shownRun),
      Some(StoryMoments.run7)
    )
    val runningWords = runOnFx(w.shell.banner.node.getChildren.asScala.collect {
      case l: Label => drawn(l)
    }.toVector)
    assertEquals(runningWords.head, "Showing run 7 (rev 4).")
    assert(runningWords.last.contains("until you choose Show"))
    fx.snapshot(StudioTheme.Light).foreach { path =>
      Files.copy(
        path,
        path.resolveSibling(s"running-${path.getFileName}"),
        StandardCopyOption.REPLACE_EXISTING
      ): Unit
    }

    w.session.await(w.session.backend.complete(StoryMoments.run8Job))
    eventually(fx, "run 8 ready notice") {
      w.runtime.model.jobs.ready.exists(_.run == StoryMoments.run8)
    }
    assertEquals(
      runOnFx(w.runtime.model.document.presentation.shownRun),
      Some(StoryMoments.run7)
    )
    assertEquals(runOnFx(w.runtime.model.selection.selected), selected)
    fx.snapshot(StudioTheme.Light).foreach { path =>
      Files.copy(
        path,
        path.resolveSibling(s"ready-${path.getFileName}"),
        StandardCopyOption.REPLACE_EXISTING
      ): Unit
    }
    val show = runOnFx(
      w.shell.banner.node.getChildren.asScala
        .collectFirst {
          case b: Button if drawn(b) == "Show run 8" => b
        }
        .getOrElse(fail("Show run 8 button missing"))
    )
    assert(!runOnFx(show.isDisabled))
    fx.robot.click(show)
    assertEquals(
      runOnFx(w.runtime.model.document.presentation.shownRun),
      Some(StoryMoments.run8)
    )
    assertEquals(runOnFx(w.runtime.model.selection.selected), selected)
    assertEquals(runOnFx(w.runtime.model.jobs.ready), None)
    assert(!runOnFx(w.shell.banner.node.isVisible))
    dispatch(
      fx,
      w,
      Intent.Execution(
        ExecutionEvent.Ready(
          RunReady(StoryMoments.run8Job, StoryMoments.run7, StoryModels.run8Stamp)
        )
      )
    )
    assertEquals(runOnFx(w.runtime.model.jobs.ready), None)
    assertNoDiff(texts(w).mkString("\n"), expected(w).mkString("\n"))
    fx.snapshot(StudioTheme.Light)
  }
