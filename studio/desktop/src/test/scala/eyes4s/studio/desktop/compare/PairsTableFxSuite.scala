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
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.desktop.harness.StudioTheme
import eyes4s.studio.desktop.shell.ShellFxSuite
import javafx.scene.input.KeyCode
import javafx.scene.layout.StackPane

import eyes4s.studio.app.vm.A11yRole
import javafx.scene.{AccessibleRole, Node}

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import scala.concurrent.duration.Duration

/** Compare's pairs table and the query's scale profile in the studio window
  * (ticket S8.5; Main.dc.html, the contrast group's tabs) at t2: run 7's
  * 8,969 pair rows at σ 2° from protocol 1.9, virtualised (only the rows in
  * view are built), the focused pair the cursor, Enter selecting it on the
  * bus; the scale profile tab shows ret_07's own D at the four scales.
  */
class PairsTableFxSuite extends ShellFxSuite:

  override val munitTimeout: Duration = Duration(180, "s")

  fxStage.test(
    "t2: every pair row, virtualised; the focused pair is the cursor; Enter selects"
  ) { fx =>
    val w     = boot(fx, StoryModels.t2Compare, StoryMoment.T2)
    val table = w.summary.pairsTable
    eventually(fx, "run 7's pair rows")(table.source.exists(_.rows.size == 8969))
    // The table is shown on its own here, so the list lays out its rows.
    val holder = runOnFx(StackPane(w.summary.pairsNode))
    runOnFx(fx.scene.setRoot(holder))
    fx.awaitLayout()
    val built = runOnFx(table.builtRows)
    assert(built > 0 && built < 200, s"$built row nodes for 8,969 rows")
    assertEquals(runOnFx(table.state.cursor), Some(StoryModels.pair))
    val src = runOnFx(table.source).get
    assertEquals(
      src.cells(src.rowOf(StoryModels.pair).get).map(_.take(4)),
      Some(Vector("P17 · ret_07", "beach-042", "matched", "P17 · enc_03"))
    )
    assert(
      runOnFx(table.getAccessibleText).contains("ret_07"),
      runOnFx(table.getAccessibleText)
    )
    fx.snapshot(StudioTheme.Light)
    runOnFx(table.requestFocus())
    fx.awaitLayout()
    fx.robot.press(KeyCode.ENTER)
    eventually(fx, "the pair selected") {
      w.runtime.model.selection.isSelected(StoryModels.pair)
    }
  }

  fxStage.test("the scale profile tab shows the focused query's D at every scale") { fx =>
    val w = boot(fx, StoryModels.t2Compare, StoryMoment.T2)
    eventually(fx, "the query's profile") {
      w.summary.scaleProfile.table.source.exists(s =>
        s.rows.size == 4 && s.rows.forall(_.ref.toString.contains("ret_07"))
      )
    }
    val src = runOnFx(w.summary.scaleProfile.table.source).get
    assertEquals(
      src.rows.indices.flatMap(src.cells(_)).map(_(3)).toVector,
      Vector("+0.19", "+0.29", "+0.38", "+0.23")
    )
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
    case AccessibleRole.TABLE_VIEW => A11yRole.Table.id
    case AccessibleRole.PARENT     => A11yRole.Region.id
    case AccessibleRole.BUTTON     => A11yRole.Button.id
    case other                     => other.toString.toLowerCase

  fxStage.test("the pairs pane is one Tab stop; docs/studio/a11y/tab-order-compare-pairs.txt") {
    fx =>
      val w     = boot(fx, StoryModels.t2Compare, StoryMoment.T2)
      val table = w.summary.pairsTable
      eventually(fx, "run 7's pair rows")(table.source.exists(_.rows.size == 8969))
      runOnFx(fx.scene.setRoot(StackPane(w.summary.pairsNode)))
      fx.awaitLayout()
      runOnFx(table.requestFocus())
      fx.awaitLayout()
      val first = runOnFx(fx.scene.getFocusOwner)
      val stops = Vector.newBuilder[String]
      stops += runOnFx(s"${role(first)}: ${first.getAccessibleText}")
      fx.robot.press(KeyCode.TAB)
      // Tab leaves the table at once: no row or cell is a stop of its own.
      var at = runOnFx(fx.scene.getFocusOwner)
      var n  = 0
      while !(at eq first) && n < 10 do
        stops += runOnFx(s"${role(at)}: ${at.getAccessibleText}")
        fx.robot.press(KeyCode.TAB)
        at = runOnFx(fx.scene.getFocusOwner)
        n += 1
      val walked = stops.result().mkString("", "\n", "\n")
      val file   = buildRoot.resolve("docs/studio/a11y/tab-order-compare-pairs.txt")
      if sys.env.contains("EYES4S_UPDATE_GOLDENS") then
        Files.writeString(file, walked, UTF_8): Unit
      assert(Files.exists(file), s"missing $file; run with EYES4S_UPDATE_GOLDENS=1 to write it")
      assertNoDiff(Files.readString(file, UTF_8), walked)
  }
