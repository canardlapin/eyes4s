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

import eyes4s.studio.app.{AppModel, StoryModels}
import eyes4s.studio.app.tokens.{Theme, ThemedToken, Tokens}
import eyes4s.studio.app.vm.{A11y, A11yRole}
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.dock.StudioPanes
import eyes4s.studio.desktop.harness.FxStage
import javafx.scene.{AccessibleRole, Node, Parent}
import javafx.scene.control.Labeled
import javafx.scene.input.KeyCode
import javafx.scene.layout.Region
import javafx.scene.paint.Color

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

/** The accessibility baseline (ticket S1.11): no focusable node without an
  * accessible name and role (a tree audit of every perspective); Tab visits
  * exactly the stops studio-app derives, in order, and that order is
  * committed as docs/studio/a11y/tab-order-<perspective>.txt (regenerate
  * with `EYES4S_UPDATE_GOLDENS=1`); focus is drawn in the accent.
  */
class A11yTreeSuite extends ShellFxSuite:

  private val buildRoot: Path = Paths.get(
    String(
      getClass.getClassLoader
        .getResourceAsStream("eyes4s/studio/desktop/build-root.txt")
        .readAllBytes,
      UTF_8
    ).trim
  )

  /** Each perspective at its board's moment. */
  private val perspectives: Vector[(String, () => AppModel, StoryMoment)] = Vector(
    ("data", () => StoryModels.t1Data, StoryMoment.T1),
    ("explore", () => StoryModels.t2Explore, StoryMoment.T2),
    ("analysis", () => StoryModels.t2Analysis, StoryMoment.T2),
    ("compare", () => StoryModels.t2Compare, StoryMoment.T2),
    ("figures", () => StoryModels.t2Figures, StoryMoment.T2)
  )

  /** More states for the audit: the new project and the running run. */
  private val extra: Vector[(String, () => AppModel, StoryMoment)] = Vector(
    ("data-empty", () => StoryModels.firstRun, StoryMoment.T2),
    ("compare-t3", () => StoryModels.t3Summary, StoryMoment.T3)
  )

  private def all(n: Node): Vector[Node] = n +: (n match
    case p: Parent => p.getChildrenUnmodifiable.asScala.toVector.flatMap(all)
    case _         => Vector.empty)

  private def shown(n: Node): Boolean =
    Iterator.iterate(n)(_.getParent).takeWhile(_ != null).forall(_.isVisible)

  /** Every node Tab can reach: focus-traversable, enabled and shown. */
  private def focusable(w: StudioWindow): Vector[Node] = runOnFx(
    all(w.root).filter(n => n.isFocusTraversable && !n.isDisabled && shown(n))
  )

  private def role(n: Node): String = n.getAccessibleRole match
    case AccessibleRole.BUTTON        => A11yRole.Button.id
    case AccessibleRole.TOGGLE_BUTTON => A11yRole.ToggleButton.id
    case AccessibleRole.MENU_BUTTON   => A11yRole.MenuButton.id
    case AccessibleRole.IMAGE_VIEW    => A11yRole.Plot.id
    case AccessibleRole.TABLE_VIEW    => A11yRole.Table.id
    case AccessibleRole.LIST_VIEW     => A11yRole.List.id
    case AccessibleRole.PARENT        => A11yRole.Region.id
    case other                        => other.toString.toLowerCase

  private def stop(n: Node): String = runOnFx(s"${role(n)}: ${n.getAccessibleText}")

  (perspectives ++ extra).foreach { (name, model, moment) =>
    fxStage.test(s"$name: every focusable node has an accessible role and name") { fx =>
      val w     = boot(fx, model(), moment)
      val nodes = focusable(w)
      assert(nodes.nonEmpty)
      val unlabeled = runOnFx(nodes.filter { n =>
        val text = Option(n.getAccessibleText).map(_.trim).getOrElse("")
        text.isEmpty || n.getAccessibleRole == AccessibleRole.NODE
      })
      assertEquals(runOnFx(unlabeled.map(_.toString)), Vector.empty[String])
      // A labelled control reads as it looks: its accessible name holds its
      // drawn words (a crumb adds ", current location").
      runOnFx(nodes.collect { case l: Labeled if l.getText != null && l.getText.nonEmpty => l })
        .foreach(l =>
          assert(runOnFx(l.getAccessibleText.contains(l.getText)), runOnFx(l.toString))
        )
    }
  }

  /** Walk the Tab cycle from the first stop back to it. */
  private def walk(fx: FxStage, w: StudioWindow): Vector[String] =
    val first = runOnFx(w.shell.appBar.project)
    runOnFx(first.requestFocus())
    fx.awaitLayout()
    val seen = Vector.newBuilder[String]
    var n    = 0
    var at   = runOnFx(fx.scene.getFocusOwner)
    while n == 0 || !(at eq first) do
      assert(n < 80, s"Tab did not return to the first stop after 80 steps: ${seen.result()}")
      seen += stop(at)
      fx.robot.press(KeyCode.TAB)
      at = runOnFx(fx.scene.getFocusOwner)
      n += 1
    seen.result()

  perspectives.foreach { (name, model, moment) =>
    fxStage.test(s"$name: Tab visits the derived stops; docs/studio/a11y/tab-order-$name.txt") {
      fx =>
        val w       = boot(fx, model(), moment)
        val derived = A11y.render(A11y.tabOrder(runOnFx(w.runtime.model)))
        val walked  = walk(fx, w)
        assertNoDiff(walked.mkString("", "\n", "\n"), derived)
        val file = buildRoot.resolve(s"docs/studio/a11y/tab-order-$name.txt")
        if sys.env.contains("EYES4S_UPDATE_GOLDENS") then
          Files.createDirectories(file.getParent)
          Files.writeString(file, derived, UTF_8): Unit
        assert(
          Files.exists(file),
          s"missing $file; run with EYES4S_UPDATE_GOLDENS=1 to write it"
        )
        assertNoDiff(Files.readString(file, UTF_8), derived)
        // Shift+Tab walks the same cycle backwards.
        val back = runOnFx(fx.scene.getFocusOwner)
        fx.robot.press(KeyCode.TAB, eyes4s.studio.desktop.harness.Modifiers(shift = true))
        assertEquals(stop(runOnFx(fx.scene.getFocusOwner)), walked.last)
        assert(back ne runOnFx(fx.scene.getFocusOwner))
    }
  }

  private def fx(c: eyes4s.studio.app.tokens.Colour): Color =
    Color.rgb(c.red, c.green, c.blue, c.alphaPercent / 100.0)

  fxStage.test("focus is drawn in the accent: a crumb, a toggle and a pane") { stage =>
    val w      = boot(stage, StoryModels.t2Compare)
    val accent = fx(Tokens.themed(Theme.Light, ThemedToken.Accent))
    // JavaFX sets :focused only while the window has focus, which another
    // test JVM's window or the desktop can hold; the ring is checked when
    // this stage can take it.
    runOnFx { stage.stage.toFront(); stage.stage.requestFocus() }
    val deadline = System.nanoTime + 5_000_000_000L
    while !runOnFx(stage.stage.isFocused) && System.nanoTime < deadline do Thread.sleep(25)
    assume(runOnFx(stage.stage.isFocused), "the test stage could not take window focus")
    def ring(r: Region): Color = runOnFx {
      r.requestFocus()
      assert(r.isFocused, s"$r did not take focus")
      r.applyCss()
      r.getBackground.getFills.get(0).getFill.asInstanceOf[Color]
    }
    val crumb  = runOnFx(w.shell.contextStrip.crumbs.head)
    val toggle = runOnFx(w.shell.appBar.switcher(eyes4s.studio.core.document.Perspective.Data))
    val pane   =
      runOnFx(w.host.node(runOnFx(w.runtime.model.focusedPane)).get.asInstanceOf[Region])
    assertEquals(ring(crumb), accent)
    assertEquals(ring(toggle), accent)
    assertEquals(ring(pane), accent)
    assertEquals(runOnFx(pane.getAccessibleRole), StudioPanes.accessibleRole(A11yRole.Plot))
  }
