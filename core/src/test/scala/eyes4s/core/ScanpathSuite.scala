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

package eyes4s.core

import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px

import scala.compiletime.testing.typeCheckErrors

class ScanpathSuite extends munit.FunSuite:

  val screen = Frame.screen("display", 1000, 1000).toOption.get
  val clock  = ClockId("tracker")

  private def fix(fromMs: Long, toMs: Long, x: Double, y: Double) =
    Event.Fixation
      .of(
        Interval.of(clock, Instant.millis(fromMs), Instant.millis(toMs)).toOption.get,
        Pt[Px](x, y),
        dispersion = 5.0,
        method = DispersionMethod.RmsRadius,
        sampleCount = ((toMs - fromMs) * 1).toInt
      )
      .toOption
      .get

  // Three fixations tracing a right angle: (100,100) -> (400,100) -> (400,500)
  val path = Scanpath
    .of(
      screen,
      clock,
      IArray(fix(0, 200, 100, 100), fix(250, 450, 400, 100), fix(500, 700, 400, 500))
    )
    .toOption
    .get

  // -------------------------------------------------------------------------
  // n and n-1
  // -------------------------------------------------------------------------

  test("a scanpath has exactly one fewer transition than fixations") {
    assertEquals(path.n, 3)
    assertEquals(path.transitions.length, 2)
  }

  test("a single fixation has no transitions, and that is not an error") {
    val one = Scanpath.of(screen, clock, IArray(fix(0, 200, 500, 500))).toOption.get
    assertEquals(one.n, 1)
    assertEquals(one.transitions.length, 0)
  }

  test("transitions span the gaps between fixations, not the fixations") {
    val s = path.transitions.head
    assertEquals(s.span.onset.toMillis, 200.0)
    assertEquals(s.span.offset.toMillis, 250.0)
    assertEquals(s.from, Pt[Px](100, 100))
    assertEquals(s.to, Pt[Px](400, 100))
  }

  test("a positive-duration transition has a derived mean velocity") {
    assert(path.transitions.head.meanVelocity.isDefined)
  }

  test("transition geometry comes from the displacement") {
    val s = path.transitions.head
    assertEqualsDouble(s.amplitude, 300.0, 1e-9)
    assertEqualsDouble(s.direction.toDegrees, 0.0, 1e-9)
    assertEqualsDouble(s.displacement.dx, 300.0, 1e-9)

    val up = path.transitions(1)
    assertEqualsDouble(up.amplitude, 400.0, 1e-9)
    assertEqualsDouble(up.direction.toDegrees, 90.0, 1e-9)
  }

  // -------------------------------------------------------------------------
  // Construction invariants
  // -------------------------------------------------------------------------

  test("a scanpath needs at least one fixation") {
    assertEquals(
      Scanpath.of(screen, clock, IArray.empty[Event.Fixation[Px]]),
      Left(ScanpathError.NoFixations)
    )
  }

  test("overlapping fixations are rejected, and the error says why") {
    val bad = Scanpath.of(
      screen,
      clock,
      IArray(fix(0, 300, 100, 100), fix(200, 400, 400, 100))
    )
    assert(bad.isLeft)
    bad.left.foreach { e =>
      assert(clue(e.message).contains("negative"))
    }
  }

  test("abutting fixations yield an honest zero-duration transition, not a saccade") {
    val abut = Scanpath.of(screen, clock, IArray(fix(0, 200, 1, 1), fix(200, 400, 2, 2)))
    assert(abut.isRight)
    val transition = abut.toOption.get.transitions.head
    assertEquals(transition.duration.toMicros, 0L)
    assertEquals(transition.meanVelocity, None)
  }

  test("an inferred transition cannot inhabit the positive-duration saccade event type") {
    val errors = typeCheckErrors("""
      import eyes4s.core.*
      import eyes4s.kernel.Unit2D.Px
      summon[ScanpathTransition[Px] <:< Event.Saccade[Px]]
    """)
    assertEquals(errors.length, 1, clue(errors.map(_.message)))
  }

  test("fixations must be on the scanpath's own clock") {
    val stray = Event.Fixation
      .of(
        Interval
          .of(ClockId("stimulus"), Instant.millis(800), Instant.millis(900))
          .toOption
          .get,
        Pt[Px](1, 1),
        1.0,
        DispersionMethod.RmsRadius,
        1
      )
      .toOption
      .get
    val r = Scanpath.of(screen, clock, IArray(fix(0, 200, 1, 1), stray))
    assert(r.isLeft)
    r.left.foreach(e => assert(clue(e.message).contains("stimulus")))
  }

  // -------------------------------------------------------------------------
  // Order-dependent measures
  // -------------------------------------------------------------------------

  test("path length is order-dependent, which occupancy cannot recover") {
    assertEqualsDouble(path.pathLength, 700.0, 1e-9)

    // The same three fixations in a different order trace a different path...
    val reordered = Scanpath
      .of(
        screen,
        clock,
        IArray(fix(0, 200, 100, 100), fix(250, 450, 400, 500), fix(500, 700, 400, 100))
      )
      .toOption
      .get
    assert(math.abs(reordered.pathLength - path.pathLength) > 1.0)

    // ...but induce the same measure, because order is exactly what occupancy
    // forgets.
    val a = path.occupancy(Weight.Duration).toOption.get
    val b = reordered.occupancy(Weight.Duration).toOption.get
    assertEqualsDouble(a.total, b.total, 1e-12)
    val quadrant = Region.rect(Pt[Px](0, 0), Pt[Px](500, 300)).toOption.get
    assertEqualsDouble(a.massIn(quadrant), b.massIn(quadrant), 1e-12)
  }

  test("dwell total counts fixation time, not the whole extent") {
    assertEqualsDouble(path.dwellTotal.toMillis, 600.0, 1e-9)
    assertEqualsDouble(path.extent.duration.toMillis, 700.0, 1e-9)
  }

  // -------------------------------------------------------------------------
  // Occupancy weighting
  // -------------------------------------------------------------------------

  test("uniform weighting counts fixations; duration weighting counts time") {
    val counts = path.occupancy(Weight.Uniform).toOption.get
    assertEqualsDouble(counts.total, 3.0, 1e-12)

    val dwell = path.occupancy(Weight.Duration).toOption.get
    assertEqualsDouble(dwell.total, 0.6, 1e-12)
  }

  test("a long fixation dominates a dwell map but not a count map") {
    val uneven = Scanpath
      .of(screen, clock, IArray(fix(0, 50, 100, 100), fix(100, 1100, 900, 900)))
      .toOption
      .get
    val left = Region.rect(Pt[Px](0, 0), Pt[Px](500, 500)).toOption.get

    val counts = uneven.occupancy(Weight.Uniform).toOption.get
    assertEqualsDouble(counts.massIn(left) / counts.total, 0.5, 1e-12)

    val dwell = uneven.occupancy(Weight.Duration).toOption.get
    assert(dwell.massIn(left) < 0.06 * dwell.total)
  }

  // -------------------------------------------------------------------------
  // Windowing, where the straddling policy matters
  // -------------------------------------------------------------------------

  test("the straddling policy changes which fixations a window selects") {
    // A window ending at 300ms; the second fixation runs 250-450 and straddles.
    val w = Window.of(Span.zero, Span.millis(300)).toOption.get

    val onset = path.within(w, Instant.millis(0), Overlap.OnsetInside).toOption.get
    assertEquals(onset.n, 2, "onset-inside keeps the straddling fixation")

    val contained = path.within(w, Instant.millis(0), Overlap.FullyContained).toOption.get
    assertEquals(contained.n, 1, "fully-contained drops it")

    val any = path.within(w, Instant.millis(0), Overlap.AnyIntersection).toOption.get
    assertEquals(any.n, 2)
  }

  test("windowing to nothing is an error, not an empty scanpath") {
    val w = Window.of(Span.millis(5000), Span.millis(6000)).toOption.get
    val r = path.within(w, Instant.millis(0), Overlap.OnsetInside)
    assertEquals(r, Left(CoreError.OfScanpath(ScanpathError.NoFixations)))
  }

  // -------------------------------------------------------------------------
  // Warping and event selection
  // -------------------------------------------------------------------------

  test("a scanpath warps as a whole, preserving order and timing") {
    val half  = Frame.screen("half", 500, 500).toOption.get
    val w     = Warp.rescale(screen, half).toOption.get
    val moved = path.warp(w).toOption.get
    assertEquals(moved.n, path.n)
    assertEquals(moved.frame.id, half.id)
    assertEqualsDouble(moved.first.centre.x, 50.0, 1e-9)
    assertEqualsDouble(moved.pathLength, path.pathLength / 2.0, 1e-9)
    assertEquals(
      moved.first.dispersion,
      None,
      "a nonlinear-capable warp cannot preserve spread"
    )
    assertEquals(
      moved.first.dispersionStatus,
      DispersionStatus.Unavailable(
        DispersionUnavailable.SourceSupportUnavailable(screen.id, half.id)
      )
    )
  }

  test("a source-backed scanpath reconstructs spread and retains support when warped") {
    val source = RecordingRef("scanpath-source")
    val input  = Recording
      .of(
        screen,
        clock,
        Rate.Fixed(Hz(1000.0).toOption.get),
        Eye.Left,
        None,
        IArray(
          Sample(Instant.millis(0), Gaze.Tracked(Pt[Px](100.0, 100.0), None)),
          Sample(Instant.millis(1), Gaze.Tracked(Pt[Px](300.0, 100.0), None))
        )
      )
      .toOption
      .get
    val fixation = Event.Fixation
      .of(
        Interval.of(clock, Instant.millis(0), Instant.millis(2)).toOption.get,
        Pt[Px](200.0, 100.0),
        dispersion = 999.0,
        method = DispersionMethod.RmsRadius,
        sampleCount = 2
      )
      .toOption
      .get
    val range  = SampleRange.of(0, 2).toOption.get
    val series = EventSeries.of(input, source, Vector(fixation), Vector(range)).toOption.get
    val backed = Scanpath.fromEvents(series).toOption.get
    val half   = Frame.screen("backed-half", 500, 500).toOption.get
    val moved  = backed.warp(Warp.rescale(screen, half).toOption.get).toOption.get

    assertEquals(moved.source, Some(source))
    assertEquals(moved.sampleSupport, Some(Vector(range)))
    assertEqualsDouble(moved.first.dispersion.get.value, 50.0, 1e-12)
    assert(
      moved.first.dispersionStatus match
        case DispersionStatus.Available(_, _: SummaryEvidence.Recomputed) => true
        case _                                                            => false
    )
  }

  test("an undefined warp point is an error, never a dropped fixation") {
    val target = Frame.screen("projected", 1000, 1000).toOption.get
    // Homogeneous w = x - 100, so the first fixation lies on the line at
    // infinity while the other two remain mappable.
    val h = Mat3(1, 0, 0, 0, 1, 0, 1, 0, -100)
    val r = path.warp(Warp.homography(screen, target, h))

    assertEquals(
      r,
      Left(
        CoreError.OfScanpath(
          ScanpathError.UnmappableFixation(0, screen.id, target.id, 100.0, 100.0)
        )
      )
    )
    assert(clue(r.left.toOption.get.message).contains("was not shortened"))
  }

  test("event selection applies the same policy to any event type") {
    val window = Interval.of(clock, Instant.millis(0), Instant.millis(300)).toOption.get
    val events = Vector[Event[Px]](
      fix(0, 200, 1, 1),
      Event.Blink
        .of[Px](Interval.of(clock, Instant.millis(210), Instant.millis(240)).toOption.get)
        .toOption
        .get,
      fix(250, 450, 2, 2)
    )
    assertEquals(Event.select(events, window, Overlap.FullyContained).toOption.get.length, 2)
    assertEquals(Event.select(events, window, Overlap.OnsetInside).toOption.get.length, 3)
  }

  test("event selection refuses a window on another clock") {
    val elsewhere = Interval
      .of(ClockId("stimulus"), Instant.millis(0), Instant.millis(300))
      .toOption
      .get
    assert(
      Event.select(Vector[Event[Px]](fix(0, 200, 1, 1)), elsewhere, Overlap.OnsetInside).isLeft
    )
  }

end ScanpathSuite
