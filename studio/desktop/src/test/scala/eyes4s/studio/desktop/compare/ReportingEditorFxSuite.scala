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

package eyes4s.studio.desktop.compare

import eyes4s.studio.app.StoryModels
import eyes4s.studio.core.document.ReportingFilter
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.desktop.harness.StudioTheme
import eyes4s.studio.desktop.shell.ShellFxSuite

import eyes4s.studio.app.vm.{A11y, A11yRole}
import javafx.scene.AccessibleRole
import javafx.scene.Node
import javafx.scene.input.KeyCode

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import scala.concurrent.duration.Duration

/** Compare's reporting editor in the studio window (ticket S8.7;
  * Results.dc.html, reporting) at t3: it binds the board's spec against run
  * 7's summary, and turning the minimum and the outside-window filter on
  * changes only the document's spec: the run shown and the jobs stay as
  * they were.
  */
class ReportingEditorFxSuite extends ShellFxSuite:

  override val munitTimeout: Duration = Duration(180, "s")

  fxStage.test("t3: the board's spec; the minimum and the filter change the spec only") { fx =>
    val w = boot(fx, StoryModels.t3Summary, StoryMoment.T3)
    val r = w.reporting
    eventually(fx, "run 7's summary")(r.shown("reuses").contains("35,876"))
    assertEquals(runOnFx(r.shown("title")), "By retrieval response")
    assertEquals(runOnFx(r.shown("all")), "All contributing retrieval queries (454)")
    assertEquals(
      runOnFx(r.shown("minimum")),
      "off · would drop 2 Forgotten cells (P17 n 2, P21 n 2)"
    )
    assertEquals(runOnFx(r.toggles), (false, false, false))
    fx.snapshot(StudioTheme.Light)

    // The preview is the minimum's accessible help, so a reader hears it.
    assertEquals(runOnFx(r.minimumHelp), runOnFx(r.shown("minimum")))
    val nodes  = runOnFx(r.groupNodes)
    val before = runOnFx(w.runtime.model)
    runOnFx(r.clickMinimum())
    eventually(fx, "the minimum on")(r.toggles._2)
    assertEquals(
      runOnFx(r.shown("minimum")),
      "on · drops 2 Forgotten cells (P17 n 2, P21 n 2)"
    )
    // Renders keep the group-by choices: a keyboard user's focus stays.
    assert(runOnFx(r.groupNodes).zip(nodes).forall(_ eq _))
    runOnFx(r.clickOutside())
    eventually(fx, "the filter on")(r.toggles._1)
    // With a filter on, the preview waits for evaluation.
    assertEquals(
      runOnFx(r.shown("minimum")),
      "Cells dropped by the minimum appear after evaluation (the filters change n)"
    )
    val after = runOnFx(w.runtime.model)
    val spec  = after.document.reporting.find(_.id == StoryModels.reporting).get
    assertEquals(spec.minimumPerGroup.map(_.queries), Some(3))
    assert(spec.filters.exists {
      case ReportingFilter.OutsideWindowAtMost(s) => s.value == 0.25
      case _                                      => false
    })
    // No rerun: the runs, the shown run and the jobs are as they were.
    assertEquals(after.document.runs, before.document.runs)
    assertEquals(after.document.presentation.shownRun, before.document.presentation.shownRun)
    assertEquals(after.jobs, before.jobs)
  }

  private val buildRoot: Path = Paths.get(
    String(
      getClass.getClassLoader
        .getResourceAsStream("eyes4s/studio/desktop/build-root.txt")
        .readAllBytes,
      UTF_8
    ).trim
  )

  private def role(n: Node): String = n.getAccessibleRole match
    case AccessibleRole.RADIO_BUTTON => A11yRole.RadioButton.id
    case AccessibleRole.CHECK_BOX    => A11yRole.CheckBox.id
    case AccessibleRole.BUTTON       => A11yRole.Button.id
    case other                       => other.toString.toLowerCase

  fxStage.test(
    "Tab walks the editor's derived stops; docs/studio/a11y/tab-order-compare-reporting.txt"
  ) { fx =>
    val w = boot(fx, StoryModels.t3Summary, StoryMoment.T3)
    val r = w.reporting
    eventually(fx, "run 7's summary")(r.shown("reuses").contains("35,876"))
    val derived = A11y.render(runOnFx(r.focusStops))
    runOnFx(r.groupNodes.find(_.isSelected).foreach(_.requestFocus()))
    fx.awaitLayout()
    val walked = Vector.fill(runOnFx(r.focusStops).size) {
      val at   = runOnFx(fx.scene.getFocusOwner)
      val stop = runOnFx(s"${role(at)}: ${at.getAccessibleText}")
      fx.robot.press(KeyCode.TAB)
      stop
    }
    assertNoDiff(walked.mkString("", "\n", "\n"), derived)
    val file = buildRoot.resolve("docs/studio/a11y/tab-order-compare-reporting.txt")
    if sys.env.contains("EYES4S_UPDATE_GOLDENS") then
      Files.writeString(file, derived, UTF_8): Unit
    assert(Files.exists(file), s"missing $file; run with EYES4S_UPDATE_GOLDENS=1 to write it")
    assertNoDiff(Files.readString(file, UTF_8), derived)
  }
