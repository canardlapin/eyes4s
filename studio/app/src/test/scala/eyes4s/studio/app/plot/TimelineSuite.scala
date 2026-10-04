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
  * the fixations whose onset it holds.
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

  test("a brush selects exactly the fixations that begin in its half-open span") {
    val source = right(Timeline.source(timeline, columns))
    val span   = right(HalfOpenSpan.between(2800.0, 1200.0).toRight("no span"))
    assertEquals((span.from, span.until), (1200.0, 2800.0))
    // 4 begins at the span's start and is held; 6 begins at its end and is
    // not; 3 overlaps it but began before it.
    assertEquals(PlotBrush.rows(source, columns.onset, span), Vector(ref(4), ref(5)))
    val all = right(HalfOpenSpan.between(0.0, 3300.5).toRight("no span"))
    assertEquals(PlotBrush.rows(source, columns.onset, all), (1 to 7).toVector.map(ref))
    val none = right(HalfOpenSpan.between(10.0, 300.0).toRight("no span"))
    assertEquals(PlotBrush.rows(source, columns.onset, none), Vector.empty)
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
