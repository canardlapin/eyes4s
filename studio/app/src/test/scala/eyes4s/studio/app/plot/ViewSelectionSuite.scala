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

package eyes4s.studio.app.plot

import eyes4s.studio.app.Intent
import eyes4s.studio.core.selection.{
  InputCause,
  SelectionError,
  SelectionInput,
  SelectionMode,
  SelectionState,
  StudioRef,
  ViewId
}

/** A view's side of the selection bus (S4.2, S4.5a): stamps strictly increase
  * per view, a projection never moves a view's sequence back, even when the
  * view is ahead of the state it is shown, and only a change of refs asks
  * for a redraw.
  */
class ViewSelectionSuite extends munit.FunSuite:

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private val view                 = right(ViewId.of("results.participant-plot"))
  private val other                = right(ViewId.of("results.participant-plot.table"))
  private def p(i: Int): StudioRef = StudioRef.Participant(s"P$i")

  private def input(intent: Intent): SelectionInput = intent match
    case Intent.Select(i) => i
    case other            => fail(s"unexpected $other")

  private def select(v: ViewSelection, ref: StudioRef): (ViewSelection, SelectionInput) =
    val (next, intent) = v.submit(SelectionMode.Replace, Vector(ref), InputCause.Pointer)
    (next, input(intent))

  test("each submit stamps the next sequence from the view, in its context") {
    val (a, first)  = select(ViewSelection.initial(view, SelectionState.empty), p(1))
    val (b, second) = select(a, p(2))
    assertEquals((first.stamp.sequence, second.stamp.sequence), (0L, 1L))
    assertEquals(b.sequence, 2L)
    assertEquals(first.stamp.origin, view)
    assertEquals(first.stamp.context, SelectionState.empty.context)
  }

  test("a view re-attached to the bus continues after its last applied input") {
    val (_, first) = select(ViewSelection.initial(view, SelectionState.empty), p(1))
    val bus        = right(SelectionState.empty.submit(first))
    assertEquals(ViewSelection.initial(view, bus).sequence, 1L)
    assertEquals(ViewSelection.initial(other, bus).sequence, 0L)
  }

  test("a view ahead of the bus keeps its sequence when an older state is projected") {
    // Three inputs leave the view; the bus has applied only the first when
    // its state is projected back.
    val start       = ViewSelection.initial(view, SelectionState.empty)
    val (a, first)  = select(start, p(1))
    val (b, second) = select(a, p(2))
    val (c, _)      = select(b, p(3))
    val behind      = right(SelectionState.empty.submit(first))
    val (seen, _)   = c.project(behind)
    assertEquals(seen.sequence, 3L, "the projection moved the view's sequence back")
    // The bus then applies the second: the view's next input is still accepted.
    val caught      = right(behind.submit(second))
    val (_, fourth) = select(seen, p(4))
    assert(caught.submit(fourth).isRight, "the view's next input was refused")
  }

  test("a late projection of an older state never makes the next input a duplicate") {
    val start      = ViewSelection.initial(view, SelectionState.empty)
    val (a, first) = select(start, p(1))
    val older      = right(SelectionState.empty.submit(first))
    val (b, sec)   = select(a, p(2))
    val newest     = right(older.submit(sec))
    // The older state arrives after the newer one was applied.
    val (late, _)  = b.project(newest)._1.project(older)
    val (_, third) = select(late, p(3))
    assertEquals(newest.submit(third).left.toOption, None: Option[SelectionError])
  }

  test("a projection redraws only when the selected refs change") {
    val (_, first)      = select(ViewSelection.initial(view, SelectionState.empty), p(1))
    val bus             = right(SelectionState.empty.submit(first))
    val shown           = ViewSelection.initial(other, SelectionState.empty)
    val (next, changed) = shown.project(bus)
    assert(changed)
    assertEquals(next.selected, Vector(p(1)))
    assertEquals(next.project(bus)._2, false)
  }
