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

package eyes4s.studio.desktop.harness

import java.nio.file.Files
import javafx.scene.control.{Button, Label, TextField}
import javafx.scene.input.KeyCode
import javafx.scene.layout.{Pane, StackPane, VBox}
import javafx.stage.Screen
import javax.imageio.ImageIO

/** The harness itself: a board-sized stage, robot input and 1x/2x snapshots. */
class HarnessSmokeSuite extends StudioFxSuite:

  private final case class Controls(button: Button, label: Label, field: TextField)

  private def counter(fx: FxStage): Controls =
    val controls = fx.runOnFx {
      val label  = Label("Clicked 0")
      val button = Button("Count")
      button.setOnAction(_ =>
        label.setText(s"Clicked ${label.getText.stripPrefix("Clicked ").toInt + 1}")
      )
      Controls(button, label, TextField())
    }
    fx.show(fx.runOnFx(VBox(12, controls.button, controls.label, controls.field)))
    controls

  fxStage.test("opens a 1440x900 stage, clicks, asserts and writes 1x and 2x snapshots") { fx =>
    assumeFullStage(fx)
    assertEquals(fx.sceneSize, (1440.0, 900.0))
    val c = counter(fx)

    fx.robot.click(c.button)
    assertEquals(fx.runOnFx(c.label.getText), "Clicked 1")

    val files = fx.snapshot(StudioTheme.Light)
    assertEquals(
      files.map(f => StudioFxSuite.snapshotRoot.relativize(f).toString.replace('\\', '/')),
      List(
        "HarnessSmokeSuite/opens-a-1440x900-stage-clicks-asserts-and-writes-1x-and-2x-snapshots/light-1x.png",
        "HarnessSmokeSuite/opens-a-1440x900-stage-clicks-asserts-and-writes-1x-and-2x-snapshots/light-2x.png"
      )
    )
    val sizes = files.map { f =>
      assert(Files.size(f) > 0L, s"$f is empty")
      val image = ImageIO.read(f.toFile)
      (image.getWidth, image.getHeight)
    }
    assertEquals(sizes, List((1440, 900), (2880, 1800)))
  }

  fxStage.test("a headless run draws on Monocle's virtual screen, not the display") { fx =>
    assume(StudioFxSuite.headless, "the build asked for visible FX tests")
    assertEquals(StudioFxSuite.glassApplication, "com.sun.glass.ui.monocle.MonocleApplication")
    val screen = fx.runOnFx(Screen.getPrimary)
    assertEquals(
      fx.runOnFx(
        (screen.getBounds.getWidth, screen.getBounds.getHeight, screen.getOutputScaleX)
      ),
      (1920.0, 1200.0, 1.0)
    )
    assertEquals(fx.runOnFx(fx.stage.getOutputScaleX), 1.0)
    assertEquals(sys.props.get("java.awt.headless"), Some("true"))
  }

  fxStage.test("keys reach the focus owner") { fx =>
    val c = counter(fx)

    fx.robot.click(c.button) // a click focuses the button
    fx.robot.press(KeyCode.SPACE)
    assertEquals(fx.runOnFx(c.label.getText), "Clicked 2")

    fx.robot.click(c.field)
    fx.robot.typeText("P17")
    assertEquals(fx.runOnFx(c.field.getText), "P17")
  }

  fxStage.test("a click is hit-tested before it is delivered") { fx =>
    val c     = counter(fx)
    val cover = fx.runOnFx {
      val pane = Pane()
      pane.setPickOnBounds(true)
      pane
    }
    val elsewhere = fx.runOnFx(Button("Elsewhere"))

    def refusal(node: javafx.scene.Node): String =
      intercept[AssertionError](fx.robot.click(node)).getMessage

    assert(refusal(elsewhere).contains("not in this test's scene"))

    fx.runOnFx(c.button.setDisable(true))
    assert(refusal(c.button).contains("disabled"))
    fx.runOnFx(c.button.setDisable(false))

    fx.runOnFx(c.button.setVisible(false))
    assert(refusal(c.button).contains("not visible"))
    fx.runOnFx(c.button.setVisible(true))

    fx.show(fx.runOnFx(StackPane(VBox(12, c.button, c.label, c.field), cover)))
    assert(refusal(c.button).contains("lands on Pane"))
    assertEquals(fx.runOnFx(c.label.getText), "Clicked 0")

    fx.runOnFx(cover.setMouseTransparent(true))
    fx.robot.click(c.button)
    assertEquals(fx.runOnFx(c.label.getText), "Clicked 1")
  }

  test("snapshot path segments are file-name safe") {
    assertEquals(StudioFxSuite.segment("  a b/c:d  "), "a-b-c-d")
    assertEquals(StudioFxSuite.segment("***"), "unnamed")
  }
