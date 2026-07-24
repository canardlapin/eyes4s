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

package eyes4s.kernel

import scala.compiletime.testing.typeCheckErrors

class TimeSuite extends munit.FunSuite:

  val tracker  = ClockId("tracker")
  val stimulus = ClockId("stimulus")

  // -------------------------------------------------------------------------
  // Instant and Span
  // -------------------------------------------------------------------------

  test("instant arithmetic is exact at microsecond resolution") {
    val t = Instant.millis(1234)
    assertEquals((t + Span.millis(66)).toMicros, 1300000L)
    assertEquals((t - Span.millis(234)).toMicros, 1000000L)
  }

  test("until yields the signed span between two instants") {
    val a = Instant.millis(100)
    val b = Instant.millis(250)
    assertEquals(a.until(b).toMillis, 150.0)
    assertEquals(b.until(a).toMillis, -150.0)
  }

  test("spans form a commutative group") {
    val g = summon[cats.kernel.CommutativeGroup[Span]]
    val a = Span.millis(300)
    val b = Span.millis(-120)
    assertEquals(g.combine(a, g.empty), a)
    assertEquals(g.combine(a, b), g.combine(b, a))
    assertEquals(g.combine(a, g.inverse(a)), Span.zero)
  }

  test("Hz rejects non-positive and non-finite rates") {
    assert(Hz(1000.0).isRight)
    assert(Hz(0.0).isLeft)
    assert(Hz(-60.0).isLeft)
    assert(Hz(Double.NaN).isLeft)
    assertEquals(Hz(1000.0).map(_.period.toMicros), Right(1000L))
  }

  // -------------------------------------------------------------------------
  // Instant / Span separation  (stronger than PRD T-6 anticipated)
  // -------------------------------------------------------------------------

  test("an Instant is not assignable to a Span, even inside the library") {
    val errs = typeCheckErrors("""
      import eyes4s.kernel.*
      val s: Span = Instant.millis(5)
    """)
    assert(errs.nonEmpty, "Instant silently became a Span")
  }

  test("a Span is not assignable to an Instant, even inside the library") {
    val errs = typeCheckErrors("""
      import eyes4s.kernel.*
      val t: Instant = Span.millis(5)
    """)
    assert(errs.nonEmpty, "Span silently became an Instant")
  }

  // -------------------------------------------------------------------------
  // Interval
  // -------------------------------------------------------------------------

  test("interval construction rejects a reversed extent") {
    val bad = Interval.of(tracker, Instant.millis(500), Instant.millis(100))
    assert(bad.isLeft)
    bad.left.foreach { e =>
      assert(clue(e.message).contains("ends before it starts"))
    }
  }

  test("interval is half-open: onset included, offset excluded") {
    val i = Interval.of(tracker, Instant.millis(100), Instant.millis(200)).toOption.get
    assert(i.contains(Instant.millis(100)))
    assert(i.contains(Instant.millis(199)))
    assert(!i.contains(Instant.millis(200)))
    assertEquals(i.duration.toMillis, 100.0)
  }

  test("combining intervals from different clocks is an error, not a comparison") {
    val onTracker  = Interval.of(tracker, Instant.millis(0), Instant.millis(100)).toOption.get
    val onStimulus = Interval.of(stimulus, Instant.millis(0), Instant.millis(100)).toOption.get

    // These two extents are numerically identical. Only the clock distinguishes
    // them, and that is exactly the bug the split exists to catch.
    assertEquals(onTracker.onset.toMicros, onStimulus.onset.toMicros)

    assertEquals(
      onTracker.overlaps(onStimulus),
      Left(TimeError.ClockMismatch(tracker, stimulus))
    )
    assert(onTracker.encloses(onStimulus).isLeft)
  }

  // -------------------------------------------------------------------------
  // Window  (bead q-interval-clock)
  // -------------------------------------------------------------------------

  test("a window carries no clock and resolves against an anchor") {
    val w = Window.of(Span.zero, Span.millis(3000)).toOption.get
    assertEquals(w.width.toMillis, 3000.0)

    val resolved = w.at(tracker, Instant.millis(5000)).toOption.get
    assertEquals(resolved.clock, tracker)
    assertEquals(resolved.onset.toMillis, 5000.0)
    assertEquals(resolved.offset.toMillis, 8000.0)
  }

  test("the same window resolves onto different clocks independently") {
    val w = Window.of(Span.zero, Span.millis(1000)).toOption.get
    val a = w.at(tracker, Instant.millis(0)).toOption.get
    val b = w.at(stimulus, Instant.millis(0)).toOption.get
    assertEquals(a.clock, tracker)
    assertEquals(b.clock, stimulus)
    assert(a.overlaps(b).isLeft, "windows on different clocks must not compare")
  }

  test("window construction rejects a reversed extent") {
    assert(Window.of(Span.millis(500), Span.millis(100)).isLeft)
  }

  // -------------------------------------------------------------------------
  // Overlap policies
  // -------------------------------------------------------------------------

  test("overlap policies differ on a straddling extent") {
    val window    = Interval.of(tracker, Instant.millis(0), Instant.millis(1000)).toOption.get
    val straddles = Interval.of(tracker, Instant.millis(900), Instant.millis(1500)).toOption.get

    assertEquals(Overlap.OnsetInside.selects(straddles, window), Right(true))
    assertEquals(Overlap.FullyContained.selects(straddles, window), Right(false))
    assertEquals(Overlap.AnyIntersection.selects(straddles, window), Right(true))
  }

  test("an extent starting after the window is selected by no policy") {
    val window = Interval.of(tracker, Instant.millis(0), Instant.millis(1000)).toOption.get
    val after  = Interval.of(tracker, Instant.millis(1000), Instant.millis(1200)).toOption.get

    assertEquals(Overlap.OnsetInside.selects(after, window), Right(false))
    assertEquals(Overlap.FullyContained.selects(after, window), Right(false))
    assertEquals(Overlap.AnyIntersection.selects(after, window), Right(false))
  }

  test("every overlap policy checks clock identity") {
    val window = Interval.of(tracker, Instant.millis(0), Instant.millis(1000)).toOption.get
    val other  = Interval.of(stimulus, Instant.millis(100), Instant.millis(200)).toOption.get
    Overlap.values.foreach { p =>
      assert(clue(p).selects(other, window).isLeft)
    }
  }

  // -------------------------------------------------------------------------
  // Sync
  // -------------------------------------------------------------------------

  test("sync moves an interval between timelines") {
    val s = Sync.offsetOnly(tracker, stimulus, Span.millis(250))
    val i = Interval.of(tracker, Instant.millis(1000), Instant.millis(1100)).toOption.get
    val j = s(i).toOption.get
    assertEquals(j.clock, stimulus)
    assertEquals(j.onset.toMillis, 1250.0)
    assertEquals(j.duration, i.duration)
  }

  test("sync refuses an interval from the wrong source clock") {
    val s = Sync.offsetOnly(tracker, stimulus, Span.millis(250))
    val i = Interval.of(stimulus, Instant.millis(0), Instant.millis(10)).toOption.get
    assertEquals(s(i), Left(TimeError.WrongSourceClock(tracker, stimulus)))
  }

  test("sync inverse round-trips within a microsecond") {
    val s    = Sync(tracker, stimulus, Span.millis(250), drift = 1e-6)
    val i    = Interval.of(tracker, Instant.millis(1000), Instant.millis(61000)).toOption.get
    val back = s(i).flatMap(s.inverse.apply).toOption.get
    assertEquals(back.clock, tracker)
    assert(math.abs(back.onset.toMicros - i.onset.toMicros) <= 1L, clue(back.onset.toMicros))
    assert(math.abs(back.offset.toMicros - i.offset.toMicros) <= 1L, clue(back.offset.toMicros))
  }

  test("drift is not negligible over a session") {
    // One minute at 1e-6 drift is 60us; an hour is 3.6ms. Small, but it
    // accumulates monotonically and is exactly what a pure offset misses.
    val s    = Sync(tracker, stimulus, Span.zero, drift = 1e-6)
    val hour = Instant.seconds(3600)
    assertEquals(s.unsafeInstant(hour).toMicros - hour.toMicros, 3600L)
  }

  test("fromCommonEvent pins the offset from a shared trigger") {
    val s = Sync.fromCommonEvent(tracker, stimulus, Instant.millis(100), Instant.millis(350))
    assertEquals(s.offset.toMillis, 250.0)
    assertEquals(s.unsafeInstant(Instant.millis(100)).toMillis, 350.0)
  }

end TimeSuite
