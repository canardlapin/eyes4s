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

package eyes4s.studio.viz.plot

import eyes4s.studio.app.tokens.Theme
import intaglio.{
  DeviceElement,
  DevicePoint,
  DevicePrimitive,
  Interval,
  RenderPlan,
  Scene,
  Viewport,
  value
}

/** The plot transform is Intaglio's own layout of the data panel (S4.1). */
class PlotTransformSuite extends munit.FunSuite:

  /** Positions agree to well below a device pixel. */
  private val Tolerance = 1e-9

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private val reference = right(ReferenceScene(Theme.Light))

  private def surface(w: Double, h: Double, scale: Double) = right(PlotSurface(w, h, scale))

  private val surfaces = List(
    surface(480, 320, 1.0),
    surface(480, 320, 2.0),
    surface(1000, 250, 1.0),
    surface(333.5, 777.25, 1.25)
  )

  // The device positions of the named point batch, as Intaglio lowers the scene.
  private def loweredMarks(s: PlotSurface): Vector[DevicePoint] =
    val context = right(s.renderContext(reference.id))
    val device  = right(RenderPlan(reference.scene, context).deviceScene)
    def find(elements: Vector[DeviceElement]): Vector[DevicePoint] =
      elements.flatMap {
        case DeviceElement.Mark(DevicePrimitive.PointBatch(points, _, _, _, Some(name)))
            if name.value == ReferenceScene.marksName =>
          points
        case DeviceElement.Group(_, _, _, children) => find(children)
        case DeviceElement.Annotated(_, children)   => find(children)
        case _                                      => Vector.empty
      }
    find(device.elements)

  private def assertNear(actual: DevicePoint, expected: DevicePoint, clue: String): Unit =
    assert(
      math.abs(actual.x - expected.x) <= Tolerance && math.abs(
        actual.y - expected.y
      ) <= Tolerance,
      s"$clue: $actual is not $expected"
    )

  test("the transform puts each data point where Intaglio draws its mark, on every surface") {
    surfaces.foreach { s =>
      val transform = right(PlotTransform.resolve(reference, s))
      val lowered   = loweredMarks(s)
      assertEquals(lowered.size, ReferenceScene.marks.size)
      ReferenceScene.marks.zip(lowered).foreach { (mark, drawn) =>
        assertNear(right(transform.dataToDevice(mark.at)), drawn, s"$mark on $s")
      }
    }
  }

  test("resizing moves marks on the canvas but maps them back to the same data points") {
    val before = right(PlotTransform.resolve(reference, surface(480, 320, 1.0)))
    val after  = right(PlotTransform.resolve(reference, surface(900, 500, 1.0)))
    ReferenceScene.marks.foreach { mark =>
      val b = right(before.dataToCanvas(mark.at))
      val a = right(after.dataToCanvas(mark.at))
      assertNotEquals(a, b)
      List(before.canvasToData(b), after.canvasToData(a)).foreach { back =>
        assertEqualsDouble(back.x, mark.at.x, Tolerance)
        assertEqualsDouble(back.y, mark.at.y, Tolerance)
      }
    }
    assertEquals(after.panel, before.panel)
  }

  test("a 2x surface draws the 1x layout at twice the pixels, at the same canvas positions") {
    val x1 = right(PlotTransform.resolve(reference, surface(480, 320, 1.0)))
    val x2 = right(PlotTransform.resolve(reference, surface(480, 320, 2.0)))
    assertEquals((x2.surface.deviceWidth, x2.surface.deviceHeight), (960, 640))
    ReferenceScene.marks.foreach { mark =>
      val one = right(x1.dataToCanvas(mark.at))
      val two = right(x2.dataToCanvas(mark.at))
      assertEqualsDouble(two.x, one.x, Tolerance)
      assertEqualsDouble(two.y, one.y, Tolerance)
    }
  }

  test("the device raster rounds up to whole pixels and snaps exact products") {
    assertEquals(surface(480, 320, 1.25).deviceWidth, 600)
    assertEquals(surface(100.2, 10, 1.0).deviceWidth, 101)
    assertEquals(surface(0.1, 0.1, 1.0).deviceHeight, 1)
  }

  test("invalid surfaces, blank ids and unmappable panels are refused, naming the values") {
    assertEquals(
      PlotSurface(0, 10, 1.0),
      Left(PlotSceneError.InvalidSurface(0, 10, 1.0))
    )
    assertEquals(
      PlotSurface(10, 10, Double.NaN).left
        .map(_.message)
        .left
        .toOption
        .exists(_.contains("NaN")),
      true
    )
    assertEquals(SceneId("  "), Left(PlotSceneError.BlankSceneId("  ")))
    val id = right(SceneId("s"))
    assertEquals(
      DataPanel(id, Viewport.unsafe(angleDegrees = 30.0)),
      Left(PlotSceneError.RotatedPanel("s", 30.0))
    )
    assertEquals(
      DataPanel(id, Viewport.unsafe(yScale = Interval.unsafe(2.0, 2.0))),
      Left(PlotSceneError.DegenerateDomain("s", "y", 2.0, 2.0))
    )
    assertEquals(
      PlotScene(id, Scene.empty, right(DataPanel(id, Viewport.unsafe()))),
      Left(PlotSceneError.PanelNotInScene("s", 0))
    )
    assertEquals(
      PlotSurface(8193, 10, 1.0),
      Left(PlotSceneError.InvalidSurface(8193, 10, 1.0))
    )
    val transform = right(PlotTransform.resolve(reference, surface(480, 320, 1.0)))
    assert(transform.dataToDevice(DataPoint(Double.NaN, 1.0)).isLeft)
  }

  test("the reference scene has an identity per theme") {
    assertEquals(
      Theme.values.toList.map(t => right(ReferenceScene(t)).id.value),
      List("studio.reference.light", "studio.reference.dark")
    )
  }
