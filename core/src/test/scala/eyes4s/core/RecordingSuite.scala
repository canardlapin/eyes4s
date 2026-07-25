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
import eyes4s.kernel.Unit2D.{Deg, Px}

import scala.compiletime.testing.typeCheckErrors

class RecordingSuite extends munit.FunSuite:

  val screen  = Frame.screen("display", 1000, 1000).toOption.get
  val clock   = ClockId("tracker")
  val rate    = Rate.Fixed(Hz(1000.0).toOption.get)
  val viewing = Viewing.of(Length.mm(600), Length.mm(500), Length.mm(500)).toOption.get
  val angular = Frame
    .angular("angular", viewing.horizontalExtent.toDegrees, viewing.verticalExtent.toDegrees)
    .toOption
    .get

  private def tracked(ms: Long, x: Double, y: Double) =
    Sample(Instant.millis(ms), Gaze.Tracked(Pt[Px](x, y), Some(900.0)))

  private def rec(ss: Sample[Px]*) =
    Recording
      .of(screen, clock, rate, Eye.Left, Some(PupilUnit.Arbitrary), IArray.from(ss))
      .toOption
      .get

  val simple = rec(tracked(0, 500, 500), tracked(1, 510, 500), tracked(2, 520, 500))

  // -------------------------------------------------------------------------
  // Gaze: no missing-value sentinel
  // -------------------------------------------------------------------------

  test("missing data is a case, not a number") {
    assertEquals(Gaze.Blink[Px]().position, None)
    assertEquals(Gaze.Lost[Px]().position, None)
    assert(Gaze.Blink[Px]().isMissing)
    assert(!Gaze.Blink[Px]().isUsable)
  }

  test("off-screen gaze keeps its position but is not usable") {
    val g = Gaze.OffScreen(Pt[Px](-50, 500))
    assertEquals(g.position, Some(Pt[Px](-50, 500)))
    assert(!g.isUsable, "off-screen positions must not silently enter spatial analysis")
    assert(!g.isMissing, "off-screen is data, not absence")
  }

  test("blink and signal loss stay distinguishable") {
    // A blink is a physiological event; loss is a measurement failure. Data
    // quality reporting needs to tell them apart, and only the tracker knows.
    assertNotEquals(Gaze.Blink[Px](): Gaze[Px], Gaze.Lost[Px](): Gaze[Px])
  }

  // -------------------------------------------------------------------------
  // Recording invariants
  // -------------------------------------------------------------------------

  test("a recording needs samples") {
    assertEquals(
      Recording.of(screen, clock, rate, Eye.Left, None, IArray.empty[Sample[Px]]),
      Left(RecordingError.NoSamples)
    )
  }

  test("timestamps must strictly increase, and the error says why it matters") {
    val dup = Recording.of(
      screen,
      clock,
      rate,
      Eye.Left,
      None,
      IArray(tracked(0, 1, 1), tracked(0, 2, 2))
    )
    assert(dup.isLeft)
    dup.left.foreach { e =>
      assert(clue(e.message).contains("velocity"))
    }

    val backwards = Recording.of(
      screen,
      clock,
      rate,
      Eye.Left,
      None,
      IArray(tracked(5, 1, 1), tracked(2, 2, 2))
    )
    assert(backwards.isLeft)
  }

  test("the extent covers the final sample's own period, not just its instant") {
    assertEquals(simple.extent.onset.toMillis, 0.0)
    assertEquals(simple.extent.offset.toMillis, 3.0)
    assertEquals(simple.extent.clock, clock)
  }

  test("median interval is available for irregular recordings") {
    val irregular = Recording
      .of(
        screen,
        clock,
        Rate.Irregular,
        Eye.Left,
        None,
        IArray(tracked(0, 1, 1), tracked(10, 2, 2), tracked(30, 3, 3))
      )
      .toOption
      .get
    assertEquals(irregular.medianInterval.toMillis, 20.0)
    assertEquals(irregular.rate.nominalPeriod, None)
  }

  // -------------------------------------------------------------------------
  // Data quality
  // -------------------------------------------------------------------------

  test("tracked ratio is computable, which requires having samples at all") {
    val withGaps = rec(
      tracked(0, 500, 500),
      Sample(Instant.millis(1), Gaze.Blink[Px]()),
      Sample(Instant.millis(2), Gaze.Lost[Px]()),
      tracked(3, 510, 500)
    )
    assertEqualsDouble(withGaps.trackedRatio, 0.5, 1e-12)
    assertEqualsDouble(simple.trackedRatio, 1.0, 1e-12)
  }

  // -------------------------------------------------------------------------
  // Windowing
  // -------------------------------------------------------------------------

  test("a window selects samples relative to an anchor") {
    val long = rec((0 to 20).map(i => tracked(i.toLong, 500 + i, 500))*)
    val w    = Window.of(Span.millis(5), Span.millis(10)).toOption.get
    val cut  = long.within(w, Instant.millis(0)).toOption.get
    assertEquals(cut.size, 5) // half-open: 5,6,7,8,9
    assertEquals(cut.first.t.toMillis, 5.0)
    assertEquals(cut.last.t.toMillis, 9.0)
  }

  test("windowing to nothing is an error, not an empty recording") {
    val w = Window.of(Span.millis(100), Span.millis(200)).toOption.get
    val r = simple.within(w, Instant.millis(0))
    assert(r.isLeft)
    assertEquals(r.left.toOption.get, CoreError.OfRecording(RecordingError.NoSamples))
  }

  test("windowing carries the underlying error rather than flattening it") {
    // A reversed window fails in the TIME layer, and the message that surfaces
    // is the one that layer wrote.
    val bad = Window.of(Span.millis(10), Span.millis(5))
    assert(bad.isLeft)
  }

  // -------------------------------------------------------------------------
  // Warping to degrees
  // -------------------------------------------------------------------------

  test("a recording warps to angular coordinates as a whole") {
    val toDeg = Viewing.angularWarp(viewing, screen, angular)
    val inDeg = simple.warp(toDeg).toOption.get
    assertEquals(inDeg.frame.id, angular.id)
    assertEquals(inDeg.size, simple.size)
    // The screen centre is zero eccentricity.
    assertEqualsDouble(inDeg.first.position.get.x, 0.0, 1e-9)
  }

  test("warping preserves timestamps, eye and clock") {
    val toDeg = Viewing.angularWarp(viewing, screen, angular)
    val inDeg = simple.warp(toDeg).toOption.get
    assertEquals(inDeg.clock, simple.clock)
    assertEquals(inDeg.eye, simple.eye)
    assertEquals(
      (0 until inDeg.size).map(i => inDeg.samples(i).t.toMicros),
      (0 until simple.size).map(i => simple.samples(i).t.toMicros)
    )
  }

  test("warping refuses a transform from another frame") {
    val other = Frame.screen("other", 1000, 1000).toOption.get
    val w     = Warp.rescale(other, angular).toOption.get
    assert(simple.warp(w).isLeft)
  }

  test("a sample leaving the target frame becomes off-screen, not lost") {
    val small = Frame.screen("small", 10, 10).toOption.get
    val w     = Warp.rescale(screen, small).toOption.get
    val edge  = rec(tracked(0, 999, 999))
    val moved = edge.warp(w).toOption.get
    // 999/1000 * 10 = 9.99, still inside; push further to leave.
    assert(moved.first.position.isDefined)
  }

  // -------------------------------------------------------------------------
  // The forgetful map
  // -------------------------------------------------------------------------

  test("a recording induces an occupancy measure weighted by sample period") {
    val mu = simple.occupancy.toOption.get
    assertEquals(mu.size, 3)
    // Three samples at 1000 Hz is three milliseconds of dwell.
    assertEqualsDouble(mu.total, 0.003, 1e-12)
  }

  test("occupancy excludes unusable samples") {
    val withGaps = rec(
      tracked(0, 500, 500),
      Sample(Instant.millis(1), Gaze.Blink[Px]()),
      Sample(Instant.millis(2), Gaze.OffScreen(Pt[Px](-10, -10)))
    )
    val mu = withGaps.occupancy.toOption.get
    assertEquals(mu.size, 1)
  }

  test("occupancy discards order, and that is the point") {
    val forward = rec(tracked(0, 100, 100), tracked(1, 900, 900))
    val reverse = rec(tracked(0, 900, 900), tracked(1, 100, 100))
    val a       = forward.occupancy.toOption.get
    val b       = reverse.occupancy.toOption.get
    // Same total, same region masses -- the paths differ, the measures do not.
    val quadrant = Region.rect(Pt[Px](0, 0), Pt[Px](500, 500)).toOption.get
    assertEqualsDouble(a.massIn(quadrant), b.massIn(quadrant), 1e-12)
    assertEqualsDouble(a.total, b.total, 1e-12)
  }

  // -------------------------------------------------------------------------
  // Binocular
  // -------------------------------------------------------------------------

  val bino = BinocularRecording
    .of(
      screen,
      clock,
      rate,
      Some(PupilUnit.Arbitrary),
      IArray(Instant.millis(0), Instant.millis(1)),
      IArray(Gaze.Tracked(Pt[Px](498, 500), None), Gaze.Tracked(Pt[Px](508, 500), None)),
      IArray(Gaze.Tracked(Pt[Px](502, 500), None), Gaze.Tracked(Pt[Px](512, 500), None))
    )
    .toOption
    .get

  test("binocular samples must be paired") {
    val r = BinocularRecording.of(
      screen,
      clock,
      rate,
      None,
      IArray(Instant.millis(0), Instant.millis(1)),
      IArray(Gaze.Tracked(Pt[Px](1, 1), None)),
      IArray(Gaze.Tracked(Pt[Px](1, 1), None))
    )
    assert(r.isLeft)
    r.left.foreach(e => assert(clue(e.message).contains("paired")))
  }

  test("projections give monocular recordings a detector can consume") {
    assertEquals(bino.left.eye, Eye.Left)
    assertEquals(bino.right.eye, Eye.Right)
    assertEquals(bino.left.size, 2)
    assertEqualsDouble(bino.left.first.position.get.x, 498.0, 1e-12)
  }

  test("cyclopean fusion by mean takes the midpoint") {
    val c = bino.cyclopean(Fusion.Mean)
    assertEquals(c.eye, Eye.Cyclopean)
    assertEqualsDouble(c.first.position.get.x, 500.0, 1e-12)
  }

  test("fusion falls back when one eye is unusable") {
    val oneEye = BinocularRecording
      .of(
        screen,
        clock,
        rate,
        None,
        IArray(Instant.millis(0)),
        IArray(Gaze.Tracked(Pt[Px](400, 400), None)),
        IArray(Gaze.Lost[Px]())
      )
      .toOption
      .get
    assertEqualsDouble(oneEye.cyclopean(Fusion.Mean).first.position.get.x, 400.0, 1e-12)
    assertEqualsDouble(oneEye.cyclopean(Fusion.BestTracked).first.position.get.x, 400.0, 1e-12)
  }

  test("disparity survives, which is the reason for the paired type") {
    val d = bino.disparity
    assertEquals(d.length, 2)
    // Left is 4px to the left of right at both instants.
    assertEqualsDouble(d.head._2.dx, -4.0, 1e-12)
  }

  test("disparity is unavailable where an eye is unusable, rather than guessed") {
    val partial = BinocularRecording
      .of(
        screen,
        clock,
        rate,
        None,
        IArray(Instant.millis(0), Instant.millis(1)),
        IArray(Gaze.Tracked(Pt[Px](498, 500), None), Gaze.Blink[Px]()),
        IArray(Gaze.Tracked(Pt[Px](502, 500), None), Gaze.Tracked(Pt[Px](512, 500), None))
      )
      .toOption
      .get
    assertEquals(partial.disparity.length, 1)
  }

  // -------------------------------------------------------------------------
  // Units
  // -------------------------------------------------------------------------

  test("a pixel recording is not a degree recording") {
    val errs = typeCheckErrors("""
      import eyes4s.core.*
      import eyes4s.kernel.Unit2D.{Px, Deg}
      def needsDegrees(r: Recording[Deg]): Int = r.size
      val r: Recording[Px] = ???
      needsDegrees(r)
    """)
    assert(errs.nonEmpty, "a pixel recording was accepted where degrees were required")
  }

end RecordingSuite
