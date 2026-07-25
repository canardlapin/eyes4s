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

package eyes4s.detect

import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Deg

import scala.compiletime.testing.typeCheckErrors

class DetectSuite extends munit.FunSuite:

  val clock = ClockId("tracker")

  /** 1000 Hz: one sample per millisecond. */
  private def at(ms: Long, x: Double, y: Double) =
    Sample(Instant.millis(ms), Gaze.Tracked(Pt[Deg](x, y), None))

  private def missing(ms: Long) = Sample(Instant.millis(ms), Gaze.Lost[Deg]())

  /** Holds still at `x` for `n` ms starting at `from`. */
  private def hold(from: Long, n: Int, x: Double) =
    (0 until n).map(i => at(from + i, x, 0.0)).toVector

  /** Moves from x0 to x1 over n ms. */
  private def sweep(from: Long, n: Int, x0: Double, x1: Double) =
    (0 until n).map(i => at(from + i, x0 + (x1 - x0) * i / n, 0.0)).toVector

  // -------------------------------------------------------------------------
  // The unit claim, made good on the DATA
  // -------------------------------------------------------------------------

  test("a pixel sample cannot reach the velocity detector") {
    val errs = typeCheckErrors("""
      import eyes4s.detect.*
      import eyes4s.core.*
      import eyes4s.kernel.*
      import eyes4s.kernel.Unit2D.Px
      val d = Detectors.ivt(Velocity.degPerSecond(30).toOption.get, Span.millis(60), ClockId("c"))
      val pixels: List[Sample[Px]] = Nil
      d.runAll(pixels)
    """)
    assert(errs.nonEmpty, "a pixel recording reached a degrees-per-second detector")
  }

  // -------------------------------------------------------------------------
  // I-VT
  // -------------------------------------------------------------------------

  val threshold = Velocity.degPerSecond(30.0).toOption.get
  def ivt       = Detectors.ivt(threshold, Span.millis(20), clock)

  test("a still eye yields one fixation") {
    val fixations = ivt.runAll(hold(0, 100, 5.0)).collect { case f: Event.Fixation[Deg] => f }
    assertEquals(fixations.length, 1)
    assertEqualsDouble(fixations.head.centre.x, 5.0, 1e-9)
    assert(fixations.head.duration.toMillis >= 20.0, clue(fixations.head.duration.toMillis))
  }

  test("a fixation, a sweep and a fixation yield fixation-saccade-fixation") {
    // 1 deg/ms is 1000 deg/s, far above threshold.
    val input  = hold(0, 60, 0.0) ++ sweep(60, 30, 0.0, 30.0) ++ hold(90, 60, 30.0)
    val events = ivt.runAll(input)
    val kinds  = events.map {
      case _: Event.Fixation[Deg] => "F"
      case _: Event.Saccade[Deg]  => "S"
      case _                      => "?"
    }
    assertEquals(kinds, Vector("F", "S", "F"))
  }

  test("the fixation centroids match where the eye actually was") {
    val input     = hold(0, 60, 0.0) ++ sweep(60, 30, 0.0, 30.0) ++ hold(90, 60, 30.0)
    val fixations = ivt.runAll(input).collect { case f: Event.Fixation[Deg] => f }
    assertEqualsDouble(fixations.head.centre.x, 0.0, 1e-6)
    assertEqualsDouble(fixations.last.centre.x, 30.0, 1e-6)
  }

  test("runs shorter than the minimum duration are discarded as noise") {
    // A 5ms twitch inside a long fixation is below the 20ms floor.
    val input  = hold(0, 60, 0.0) ++ sweep(60, 5, 0.0, 5.0) ++ hold(65, 60, 5.0)
    val events = ivt.runAll(input)
    assert(events.collect { case s: Event.Saccade[Deg] => s }.isEmpty, clue(events))
  }

  test("an event still open at the end is flushed, not lost") {
    assertEquals(ivt.runAll(hold(0, 50, 1.0)).length, 1)
  }

  test("no event is emitted from samples that were never tracked") {
    assertEquals(
      ivt.runAll((0 until 100).map(i => missing(i.toLong)).toVector),
      Vector.empty[Event[Deg]]
    )
  }

  // -------------------------------------------------------------------------
  // I-DT
  // -------------------------------------------------------------------------

  def idt = Detectors.idt[Deg](Extent.square[Deg](1.0).toOption.get, Span.millis(20), clock)

  test("dispersion identification finds a still period") {
    val fixations = idt.runAll(hold(0, 100, 5.0)).collect { case f: Event.Fixation[Deg] => f }
    assertEquals(fixations.length, 1)
    assertEqualsDouble(fixations.head.centre.x, 5.0, 1e-9)
  }

  test("a move beyond the dispersion box ends the fixation") {
    val input     = hold(0, 60, 0.0) ++ hold(60, 60, 10.0)
    val fixations = idt.runAll(input).collect { case f: Event.Fixation[Deg] => f }
    assertEquals(fixations.length, 2)
    assertEqualsDouble(fixations.head.centre.x, 0.0, 1e-9)
    assertEqualsDouble(fixations(1).centre.x, 10.0, 1e-9)
  }

  test("slow drift within the box stays one fixation") {
    // Drifts 0.9 deg over 100ms: below the 1.0 deg box, so not a new fixation.
    val fixations =
      idt.runAll(sweep(0, 100, 0.0, 0.9)).collect { case f: Event.Fixation[Deg] => f }
    assertEquals(fixations.length, 1)
  }

  test("dispersion takes an Extent, not a bandwidth") {
    val errs = typeCheckErrors("""
      import eyes4s.detect.*
      import eyes4s.kernel.*
      import eyes4s.kernel.Unit2D.Deg
      Detectors.idt[Deg](Sigma.deg(1.0).toOption.get, Span.millis(20), ClockId("c"))
    """)
    assert(errs.nonEmpty, "a standard deviation was accepted as a bounding box")
  }

  test("missing data breaks a fixation rather than being interpolated over") {
    val input     = hold(0, 40, 0.0) ++ Vector(missing(40)) ++ hold(41, 40, 0.0)
    val fixations = idt.runAll(input).collect { case f: Event.Fixation[Deg] => f }
    assertEquals(fixations.length, 2, "a gap is a gap unless a filter decided otherwise")
  }

  // -------------------------------------------------------------------------
  // Filters
  // -------------------------------------------------------------------------

  test("the median filter rejects an isolated spike") {
    val spiked = hold(0, 20, 5.0).updated(10, at(10, 500.0, 0.0))
    val worst  = Filter.median[Deg](2).runAll(spiked).flatMap(_.gaze.position).map(_.x).max
    assert(worst < 6.0, clue(worst))
  }

  test("a polynomial smoother would not have rejected it, which is why median is here") {
    val spiked = hold(0, 20, 5.0).updated(10, at(10, 500.0, 0.0))
    val sg     = Filter.savitzkyGolay[Deg](2).runAll(spiked)
    assert(sg.flatMap(_.gaze.position).map(_.x).max > 6.0)
  }

  test("Savitzky-Golay coefficients sum to one, so a constant signal is unchanged") {
    Filter
      .savitzkyGolay[Deg](3)
      .runAll(hold(0, 20, 7.0))
      .flatMap(_.gaze.position)
      .foreach(p => assertEqualsDouble(p.x, 7.0, 1e-9))
  }

  test("filters preserve the sample count") {
    val input = hold(0, 30, 1.0)
    assertEquals(Filter.median[Deg](2).runAll(input).length, input.length)
    assertEquals(Filter.savitzkyGolay[Deg](2).runAll(input).length, input.length)
  }

  test("short gaps interpolate; long ones stay missing") {
    val gap  = Vector(at(0, 0.0, 0.0), missing(1), missing(2), at(3, 3.0, 0.0))
    val outS = Filter.interpolateGaps[Deg](Span.millis(10)).runAll(gap)
    assert(outS.forall(_.gaze.isUsable), clue(outS))
    assertEqualsDouble(outS(1).gaze.position.get.x, 1.0, 1e-9)

    val outL = Filter.interpolateGaps[Deg](Span.micros(500)).runAll(gap)
    assertEquals(outL.count(_.gaze.isMissing), 2)
  }

  test("blink padding condemns the samples either side") {
    val input =
      Vector(at(0, 0.0, 0.0), at(1, 0.0, 0.0), missing(2), at(3, 0.0, 0.0), at(4, 0.0, 0.0))
    val out = Filter.padMissing[Deg](Span.millis(1)).runAll(input)
    assertEquals(out.length, input.length)
    // Samples at 1 and 3 are within 1ms of the blink at 2.
    assertEquals(out.count(_.gaze.isMissing), 3, clue(out.map(_.gaze.isMissing)))
  }

  // -------------------------------------------------------------------------
  // Composition
  // -------------------------------------------------------------------------

  test("a filter chain and a detector compose into one machine") {
    val pipeline: Machine[Sample[Deg], Event[Deg]] =
      Filter
        .padMissing[Deg](Span.millis(1))
        .andThen(Filter.interpolateGaps(Span.millis(10)))
        .andThen(Filter.median(2))
        .andThen(ivt)

    val input = hold(0, 60, 0.0) ++ sweep(60, 30, 0.0, 30.0) ++ hold(90, 60, 30.0)
    assert(pipeline.runAll(input).nonEmpty)
  }

end DetectSuite
