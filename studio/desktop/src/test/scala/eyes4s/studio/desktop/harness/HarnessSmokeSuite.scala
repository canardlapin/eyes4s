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
import javafx.scene.layout.VBox
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

  fxStage.test("keys reach the focus owner") { fx =>
    val c = counter(fx)

    fx.robot.click(c.button) // a click focuses the button
    fx.robot.press(KeyCode.SPACE)
    assertEquals(fx.runOnFx(c.label.getText), "Clicked 2")

    fx.robot.click(c.field)
    fx.robot.typeText("P17")
    assertEquals(fx.runOnFx(c.field.getText), "P17")
  }

  test("snapshot path segments are file-name safe") {
    assertEquals(StudioFxSuite.segment("  a b/c:d  "), "a-b-c-d")
    assertEquals(StudioFxSuite.segment("***"), "unnamed")
  }
