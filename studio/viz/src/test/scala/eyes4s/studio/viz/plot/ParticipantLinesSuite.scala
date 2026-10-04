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

import eyes4s.studio.app.plot.ParticipantLines
import eyes4s.studio.app.tokens.Theme
import intaglio.Grob
import munit.FunSuite

/** The participant plot's participant lines (ticket S9.2b, the Figures
  * board's "Participant lines · Show Hide"): hiding them removes the lines
  * that join a participant's means and nothing else.
  */
class ParticipantLinesSuite extends FunSuite:
  import ParticipantSamples.*

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private def segments(plot: BuiltPlot): Vector[Grob.Segments] =
    def all(g: Grob): Vector[Grob] = g +: g.children.flatMap(all)
    plot.plot.scene.grobs.flatMap(all).collect { case s: Grob.Segments => s }

  test("hidden participant lines remove the joins, keeping every mark") {
    val shown  = right(ParticipantPlot(columns).build(source(board), Theme.Light))
    val hidden =
      right(ParticipantPlot(columns, ParticipantLines.Hidden).build(source(board), Theme.Light))
    assertEquals(hidden.marks, shown.marks)
    val joins = segments(shown).diff(segments(hidden))
    // One batch of joins: a segment per participant with a mean in both groups.
    assertEquals(joins.size, 1)
    assertEquals(segments(hidden).size, segments(shown).size - 1)
  }
