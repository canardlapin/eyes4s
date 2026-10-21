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

import eyes4s.studio.app.AppModel
import eyes4s.studio.desktop.harness.StudioTheme
import scala.jdk.CollectionConverters.*

/** Measured parity, separate from the manual side-by-side review. */
class ParityChecklistSuite extends ShellFxSuite:
  fxStage.test("DataEmpty t0 shell bounds and texts match the board") { fx =>
    assumeFullStage(fx)
    val w       = boot(fx, AppModel.newProject.fold(e => fail(e.toString), identity))
    val metrics = runOnFx {
      val heights = Vector(
        "appBar"       -> w.shell.appBar.node.getLayoutBounds.getHeight,
        "contextStrip" -> w.shell.contextStrip.node.getLayoutBounds.getHeight,
        "statusBar"    -> w.shell.statusBar.node.getLayoutBounds.getHeight
      )
      val expected = Map("appBar" -> 44.0, "contextStrip" -> 32.0, "statusBar" -> 24.0)
      heights.foreach((name, value) => assertEqualsDouble(value, expected(name), 1))
      val headers = w.root
        .lookupAll(".dock-header")
        .asScala
        .toVector
        .filter(_.isVisible)
        .map(_.getLayoutBounds.getHeight)
      assertEquals(headers.size, 3)
      headers.foreach(h => assertEqualsDouble(h, 28, 1))
      val sidebars = Vector(
        "sources"   -> w.firstRun.sources.getWidth,
        "checklist" -> w.firstRun.checklist.getWidth
      )
      assertEqualsDouble(sidebars.toMap.apply("sources"), 252, 1)
      assertEqualsDouble(sidebars.toMap.apply("checklist"), 300, 1)
      ujson.Obj(
        "heights"                -> ujson.Obj.from(heights.map((k, v) => k -> ujson.Num(v))),
        "tabHeaders"             -> ujson.Arr.from(headers),
        "widths"                 -> ujson.Obj.from(sidebars.map((k, v) => k -> ujson.Num(v))),
        "toleranceLogicalPixels" -> 1,
        "nativeOutputScaleX"     -> fx.stage.getOutputScaleX,
        "nativeOutputScaleY"     -> fx.stage.getOutputScaleY
      )
    }
    val lines = texts(w)
    assert(lines.contains("app.project: Untitled project"))
    assert(lines.contains("context.trail: *New project*"))
    assert(lines.contains("context.freshness: No dataset · no analysis"))
    assertEquals(runOnFx(w.shell.statusBar.saved.getText), "Not saved")
    val captures = fx.snapshot(StudioTheme.Light)
    java.nio.file.Files.writeString(
      captures.head.getParent.resolve("metrics.json"),
      ujson.write(metrics, indent = 2)
    )
    ()
  }
