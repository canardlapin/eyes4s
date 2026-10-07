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

import eyes4s.studio.app.plot.ViewSelection
import eyes4s.studio.app.tokens.Theme
import eyes4s.studio.core.selection.{
  InputCause,
  SelectionMode,
  SelectionState,
  StudioRef,
  ViewId
}
import intaglio.{Grob, RenderPlan}
import intaglio.interaction.NamedPicking

class OverlaySelectionsSuite extends munit.FunSuite:
  private def right[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)
  private val view                            = right(ViewId.of("selected-profile"))
  private val profile                         = ProfileSamples.profile(
    Vector(0.5, 1, 2, 4, 8, 16),
    Vector.empty,
    Vector(
      ("P01", 10, Vector(Some(0.1), Some(0.2), None, Some(0.4), Some(0.5), None)),
      ("P02", 4, Vector(Some(0.3), None, Some(0.6), None, None, None))
    )
  )
  private def selected(refs: Vector[StudioRef]): MarkInputState[StudioRef] =
    val (_, input) = ViewSelection
      .initial(view, SelectionState.empty)
      .submit(SelectionMode.Replace, refs, InputCause.Pointer)
    val bus = input match
      case eyes4s.studio.app.Intent.Select(i) => right(SelectionState.empty.submit(i))
      case other                              => fail(other.toString)
    MarkInputState.initial[StudioRef](view, bus)

  private def all(g: Grob): Vector[Grob] = g +: g.children.flatMap(all)

  test("selection follows contiguous profile runs at 1x and 2x, never missing-value gaps") {
    val plot = right(
      ScaleProfilePlot(ProfileSamples.columns)
        .build(ProfileSamples.source(profile), Theme.Light)
    )
    List(1.0, 2.0).foreach { scale =>
      val transform =
        right(PlotTransform.resolve(plot.plot, right(PlotSurface(640, 400, scale))))
      val picking = right(NamedPicking.compile(plot.plot.scene, transform.renderContext))
      val targets = right(PlotTargets.resolve(plot, transform, picking))
      val refs    = profile.participants.head.points.map(_.ref)
      val state   = selected(refs)
      assertEquals(state.selectionRings(targets), Vector.empty)
      val lines = state.selectionLines(targets)
      assertEquals(lines.map(_.points.size), Vector(2, 2))
      val expected = Vector(
        Vector(DataPoint(math.log10(0.5), 0.1), DataPoint(0, 0.2)),
        Vector(DataPoint(math.log10(4), 0.4), DataPoint(math.log10(8), 0.5))
      )
      assertEquals(
        lines.map(_.points),
        expected.map(_.map(p => right(transform.dataToDevice(p))))
      )
      assertEquals(selected(Vector(refs(1))).selectionLines(targets), lines)
      val scene = right(
        OverlaySelections.scene(
          Vector.empty,
          lines,
          OverlayPalette.onSurface(Theme.Light),
          640 * scale,
          400 * scale,
          scale
        )
      )
      val painted = scene.grobs.flatMap(all).collect { case g: Grob.Lines => g }
      assertEquals(painted.size, 2)
      painted.foreach(g => assertEquals(g.gp.lineWidth, 3 * scale))
      val svg =
        right(intaglio.svg.SvgRenderer.render(RenderPlan(scene, transform.renderContext))).value
      assert(svg.contains(s"stroke-width=\"${(3 * scale).toInt}\""), svg)
      val dashes =
        "stroke-dasharray=\"([^\"]+)\"".r.findAllMatchIn(svg).map(_.group(1)).toVector
      val expectedDash = if scale == 1.0 then "6 3" else "12 6"
      assertEquals(dashes, Vector.fill(2)(expectedDash))
    }
  }

  test("isolated profile points retain rings, and an empty selection draws neither kind") {
    val plot = right(
      ScaleProfilePlot(ProfileSamples.columns)
        .build(ProfileSamples.source(profile), Theme.Light)
    )
    val transform = right(PlotTransform.resolve(plot.plot, right(PlotSurface(640, 400, 1))))
    val targets   = right(
      PlotTargets.resolve(
        plot,
        transform,
        right(NamedPicking.compile(plot.plot.scene, transform.renderContext))
      )
    )
    val state = selected(profile.participants(1).points.map(_.ref))
    assertEquals(state.selectionRings(targets).size, 2)
    assertEquals(state.selectionLines(targets), Vector.empty)
    val empty = MarkInputState.initial[StudioRef](view, SelectionState.empty)
    assertEquals(empty.selectionRings(targets), Vector.empty)
    assertEquals(empty.selectionLines(targets), Vector.empty)
  }
