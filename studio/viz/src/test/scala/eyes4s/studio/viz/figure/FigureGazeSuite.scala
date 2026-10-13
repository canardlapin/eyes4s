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

package eyes4s.studio.viz.figure

import eyes4s.studio.app.explore.MarkVM
import eyes4s.studio.app.figures.*
import eyes4s.studio.core.assets.Display
import eyes4s.studio.core.document.{FigureId, PanelLetter}
import eyes4s.studio.core.selection.StudioRef
import eyes4s.studio.viz.plot.{PlotSurface, PlotTransform}
import eyes4s.studio.viz.trial.{TrialScene, TrialSamples}
import intaglio.{DeviceElement, DevicePoint, DevicePrimitive, RenderPlan, value}

/** Figure cross arm (review ticket 22, DESIGN_SPEC section 15): a gaze
  * panel's fixation cross is the trial view's, a half-arm of
  * [[TrialScene.CrossArmPx]] native screen pixels, scaled with the panel
  * like the image frame and every other stimulus-space length.
  */
class FigureGazeSuite extends munit.FunSuite:

  /** Device lengths agree to well below a device pixel. */
  private val Tolerance = 1e-6

  private def right[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  // P17 ret_07 as panel B shows it: blank + fixation cross, 12 fixations.
  private val gaze = GazeTrialVM(
    TrialSamples.ret07,
    TrialSamples.display(TrialSamples.ret07, Display.BlankWithFixationCross),
    TrialSamples.screen,
    TrialSamples.ret07Fixations.map(f =>
      MarkVM(f.index, f.screenX, f.screenY, f.durationMs, f.placement)
    ),
    GazeRole.Query
  )

  private val panelMm = 47

  private val page =
    PageVM(
      right(FigureId.of(1)),
      "Figure 1",
      PageWidth.SingleColumn,
      "Page 89 mm · one-column",
      "100%",
      3.5,
      Vector(
        PanelVM(
          right(PanelLetter.of("B")),
          "Retrieval gaze",
          selected = true,
          widthMm = panelMm,
          PanelBody.Gaze(
            GazePanelVM(
              "Retrieval · ret_07",
              "Displayed: blank + fixation cross.",
              "12 fixations.",
              Some(gaze)
            )
          ),
          StudioRef.Trial(TrialSamples.ret07)
        )
      ),
      ComposerTab.Figure,
      None,
      "Open in Compare",
      "caption",
      "stamp",
      7,
      AppearanceVM(Vector.empty, Vector.empty, None, ("images", true)),
      greyscale = false,
      ExportVM(Vector.empty, "Export", None),
      AddPanelVM("Add panel", Vector.empty),
      PanelEditingVM(Some(right(PanelLetter.of("B"))), false, false, false)
    )

  private def primitives(elements: Vector[DeviceElement]): Vector[DevicePrimitive] =
    elements.flatMap {
      case DeviceElement.Mark(p)                  => Vector(p)
      case DeviceElement.Group(_, _, _, children) => primitives(children)
      case DeviceElement.Annotated(_, children)   => primitives(children)
    }

  private final case class Drawn(
      frame: DevicePrimitive.RectShape,
      cross: Vector[Vector[DevicePoint]]
  )

  // The image frame and the cross's two arms: the open two-point lines of
  // its ink grob (its casing is a grob of its own, under the ink).
  private def drawn(prims: Vector[DevicePrimitive]): Drawn =
    val frame = prims
      .collectFirst {
        case r: DevicePrimitive.RectShape if r.name.exists(_.value == TrialScene.FrameName) => r
      }
      .getOrElse(fail("no image frame drawn"))
    val cross = prims.collect {
      case DevicePrimitive.Polyline(points, false, _, Some(n))
          if points.size == 2 && n.value == TrialScene.CrossName =>
        points
    }
    assertEquals(cross.size, 2, s"the cross is not two arms: $cross")
    Drawn(frame, cross)

  // The vertical and horizontal half-arms, after checking each arm is
  // centred on the frame.
  private def halfArms(d: Drawn): (Double, Double) =
    val cx = d.frame.x + d.frame.width / 2
    val cy = d.frame.y + d.frame.height / 2
    d.cross.foreach { arm =>
      assertEqualsDouble((arm(0).x + arm(1).x) / 2, cx, Tolerance)
      assertEqualsDouble((arm(0).y + arm(1).y) / 2, cy, Tolerance)
    }
    val (vertical, horizontal) =
      d.cross.partition(a => math.abs(a(0).x - a(1).x) <= Tolerance)
    (vertical, horizontal) match
      case (Vector(v), Vector(h)) if math.abs(h(0).y - h(1).y) <= Tolerance =>
        (math.abs(v(1).y - v(0).y) / 2, math.abs(h(1).x - h(0).x) / 2)
      case _ => fail(s"the cross is not one vertical and one horizontal arm: ${d.cross}")

  // Device pixels per native screen pixel, down and across, from the frame.
  private def perScreenPx(d: Drawn): (Double, Double) =
    (
      d.frame.height / TrialSamples.placement.height,
      d.frame.width / TrialSamples.placement.width
    )

  // Panel B as the page draws it, at the SVG export's 96 px per inch.
  private def figureDrawn: Drawn =
    val built = right(FigurePage.build(page))
    drawn(primitives(right(right(built.plan(FigureSvg.PixelsPerInch)).deviceScene).elements))

  test("the settled half-arm is 20 native screen pixels") {
    assertEquals(TrialScene.CrossArmPx, 20.0)
  }

  test("a figure's cross half-arm is CrossArmPx native pixels times the panel's scale") {
    val d = figureDrawn
    // The panel's scale, page pixels per screen pixel, from its width and
    // the gaze extent it draws, independently of anything drawn.
    val panelPx = panelMm / FigurePage.MillimetresPerInch * FigureSvg.PixelsPerInch
    val scale   = panelPx / TrialScene.extentOf(FigureGaze.input(gaze, Map.empty)).width
    // The frame is drawn at that scale on both axes ...
    assertEqualsDouble(d.frame.width, TrialSamples.placement.width * scale, Tolerance)
    assertEqualsDouble(d.frame.height, TrialSamples.placement.height * scale, Tolerance)
    // ... and so is the cross.
    val (down, across) = halfArms(d)
    assertEqualsDouble(down, TrialScene.CrossArmPx * scale, Tolerance)
    assertEqualsDouble(across, TrialScene.CrossArmPx * scale, Tolerance)
  }

  test("the figure's cross is the trial view's, in native screen pixels") {
    val scene = right(TrialScene(FigureGaze.input(gaze, Map.empty)))
    val plan  = right(PlotTransform.resolve(scene.plot, right(PlotSurface(900, 700, 1.0))))
    val view  = drawn(
      primitives(right(RenderPlan(scene.plot.scene, plan.renderContext).deviceScene).elements)
    )
    // Each axis in its own scale: the trial view need not draw a surface at
    // the extent's aspect.
    def native(d: Drawn): Unit =
      val (down, across)       = halfArms(d)
      val (perDown, perAcross) = perScreenPx(d)
      assertEqualsDouble(down / perDown, TrialScene.CrossArmPx, Tolerance)
      assertEqualsDouble(across / perAcross, TrialScene.CrossArmPx, Tolerance)
    native(view)
    native(figureDrawn)
  }
