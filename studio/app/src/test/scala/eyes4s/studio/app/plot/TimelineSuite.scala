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

import eyes4s.studio.core.backend.{Phase, TrialKey}
import eyes4s.studio.core.selection.{FixationIndex, StudioRef}

/** The timeline's values and its brush (ticket S4.5e): a trial's fixation
  * intervals as one value source, and a half-open span that selects exactly
  * the fixations whose half-open intervals overlap it.
  */
class TimelineSuite extends munit.FunSuite:

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private val trial   = TrialKey("P17", Phase.Encoding, "enc_03", 1)
  private val columns = right(TimelineColumns.standard)

  private def fixation(i: Int, onset: Long, duration: Long) =
    TimelineFixation(right(FixationIndex.of(i)), onset, duration)

  private def ref(i: Int): StudioRef = StudioRef.Fixation(trial, right(FixationIndex.of(i)))

  // Test values: 4 starts at 1200 and 6 at 2800, the span's two ends; 3 runs
  // into the span but starts before it.
  private val timeline = right(
    Timeline.of(
      trial,
      Vector(
        fixation(1, 0, 180),
        fixation(2, 336, 320),
        fixation(3, 1000, 400),
        fixation(4, 1200, 380),
        fixation(5, 1764, 240),
        fixation(6, 2800, 412),
        fixation(7, 3300, 200)
      )
    )
  )

  test("the source lists each fixation's onset and duration, with its fixation ref") {
    val source = right(Timeline.source(timeline, columns))
    assertEquals(source.rows.map(_.ref), (1 to 7).toVector.map(ref))
    assertEquals(source.cells(3), Some(Vector("4", "1,200", "380")))
    assertEquals(source.columns.map(_.header), Vector("Fixation", "Onset ms", "Duration ms"))
  }

  private val overlap = TimelineColumns.brushRule(columns)

  test("a brush selects exactly the fixations whose intervals overlap its span") {
    assertEquals(overlap, BrushRule.Overlaps(columns.onset, columns.duration))
    val source = right(Timeline.source(timeline, columns))
    val span   = right(HalfOpenSpan.between(2800.0, 1200.0).toRight("no span"))
    assertEquals((span.from, span.until), (1200.0, 2800.0))
    // 3 began before the span and runs into it; 4 begins at its start; 6
    // begins at its end, which only touches it.
    assertEquals(PlotBrush.rows(source, overlap, span), Vector(ref(3), ref(4), ref(5)))
    val all = right(HalfOpenSpan.between(0.0, 3300.5).toRight("no span"))
    assertEquals(PlotBrush.rows(source, overlap, all), (1 to 7).toVector.map(ref))
    // A span in the gap between 2 and 3 touches no bar.
    val gap = right(HalfOpenSpan.between(656.0, 1000.0).toRight("no span"))
    assertEquals(PlotBrush.rows(source, overlap, gap), Vector.empty)
  }

  test("overlap: touching at an endpoint is not overlap; containing the span is") {
    val source = right(
      Timeline.source(
        right(Timeline.of(trial, Vector(fixation(1, 800, 400), fixation(2, 1200, 100)))),
        columns
      )
    )
    // 1 is [800, 1200): it ends where the span begins.
    val after = right(HalfOpenSpan.between(1200.0, 1250.0).toRight("no span"))
    assertEquals(PlotBrush.rows(source, overlap, after), Vector(ref(2)))
    // 2 is [1200, 1300): it begins where the span ends.
    val before = right(HalfOpenSpan.between(1100.0, 1200.0).toRight("no span"))
    assertEquals(PlotBrush.rows(source, overlap, before), Vector(ref(1)))
    // A span inside 1 picks 1, which contains it.
    val inside = right(HalfOpenSpan.between(900.0, 950.0).toRight("no span"))
    assertEquals(PlotBrush.rows(source, overlap, inside), Vector(ref(1)))
    // A zero-length brush is no span, so it selects nothing.
    assertEquals(HalfOpenSpan.between(1000.0, 1000.0), None)
  }

  test("the holds rule picks rows by one value: fixations that begin in the span") {
    val source = right(Timeline.source(timeline, columns))
    val span   = right(HalfOpenSpan.between(1200.0, 2800.0).toRight("no span"))
    assertEquals(
      PlotBrush.rows(source, BrushRule.Holds(columns.onset), span),
      Vector(ref(4), ref(5))
    )
  }

  test("a span needs two different finite ends") {
    assertEquals(HalfOpenSpan.between(5.0, 5.0), None)
    assertEquals(HalfOpenSpan.between(Double.NaN, 5.0), None)
    assertEquals(HalfOpenSpan.between(0.0, Double.PositiveInfinity), None)
    val s = right(HalfOpenSpan.between(1.0, 2.0).toRight("no span"))
    assert(s.holds(1.0) && s.holds(1.999) && !s.holds(2.0) && !s.holds(0.999))
  }

  test("a timeline refuses negative onsets, empty durations, repeats and disorder") {
    assertEquals(
      Timeline.of(trial, Vector(fixation(1, -5, 10))),
      Left(TimelineError.NegativeOnset(trial, 1, -5))
    )
    assertEquals(
      Timeline.of(trial, Vector(fixation(1, 0, 0))),
      Left(TimelineError.DurationNotPositive(trial, 1, 0))
    )
    assertEquals(
      Timeline.of(trial, Vector(fixation(1, 0, 10), fixation(1, 20, 10))),
      Left(TimelineError.DuplicateFixation(trial, 1))
    )
    val e = Timeline.of(trial, Vector(fixation(1, 50, 10), fixation(2, 20, 10)))
    assertEquals(e, Left(TimelineError.OutOfOrder(trial, 2, 20, 50)))
    assert(e.left.toOption.get.message.contains("P17"), e)
  }
