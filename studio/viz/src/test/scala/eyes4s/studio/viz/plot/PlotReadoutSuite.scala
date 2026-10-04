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

import eyes4s.studio.app.plot.PlotSource
import eyes4s.studio.app.tokens.Theme
import eyes4s.studio.core.selection.StudioRef
import munit.FunSuite

/** The plot readout's choice (ticket S4.5c): the hovered mark's words,
  * else the first selected mark's, else nothing; a host only binds it.
  */
class PlotReadoutSuite extends FunSuite:
  import ParticipantSamples.*

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private val means: PlotSource = source(zeroAndMissing)
  private val plot: BuiltPlot   = right(ParticipantPlot(columns).build(means, Theme.Light))

  private def words(ref: StudioRef): String =
    right(plot.markOf(ref).flatMap(plot.readout).toRight(ref))

  private val remembered = zeroAndMissing.groups(0).ref
  private val forgotten  = zeroAndMissing.groups(1).ref
  private val p01        = zeroAndMissing.cells(0).ref
  private val elsewhere  = StudioRef.Participant("P99")

  test("nothing hovered or selected says nothing") {
    assertEquals(PlotReadout.of(plot, None, Vector.empty), None)
    assertEquals(PlotReadout.of(plot, Some(elsewhere), Vector(elsewhere)), None)
  }

  test("the hovered mark is said before the selection") {
    assertEquals(PlotReadout.of(plot, Some(remembered), Vector(p01)), Some(words(remembered)))
    assertEquals(PlotReadout.of(plot, Some(forgotten), Vector.empty), Some(words(forgotten)))
    assertNotEquals(words(remembered), words(forgotten))
  }

  test("with nothing hovered, the first selected row this plot draws is said") {
    assertEquals(
      PlotReadout.of(plot, None, Vector(elsewhere, p01, forgotten)),
      Some(words(p01))
    )
    // A hover off this plot's marks falls back to the selection.
    assertEquals(
      PlotReadout.of(plot, Some(elsewhere), Vector(forgotten)),
      Some(words(forgotten))
    )
  }
