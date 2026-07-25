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

import eyes4s.kernel.Unit2D.{Norm, Px}

class MovingSuite extends munit.FunSuite:

  val clock  = ClockId("tracker")
  val screen = Frame.screen("display", 1000, 1000).toOption.get
  val stim   = Frame.unitSquare("stimulus").toOption.get

  /** A stimulus occupying a square that slides right over time. */
  private def placementAt(offsetPx: Double): Warp[Px, Norm] =
    Warp
      .affine(
        screen,
        stim,
        Mat3.affine(1.0 / 500.0, 0, -offsetPx / 500.0, 0, 1.0 / 500.0, 0)
      )
      .toOption
      .get

  private def seg(fromMs: Long, toMs: Long, offsetPx: Double) =
    Moving.Segment(
      Interval.of(clock, Instant.millis(fromMs), Instant.millis(toMs)).toOption.get,
      placementAt(offsetPx)
    )

  val frames = Vector(seg(0, 100, 0.0), seg(100, 200, 50.0), seg(200, 300, 100.0))

  // -------------------------------------------------------------------------
  // Construction
  // -------------------------------------------------------------------------

  test("requires at least one segment") {
    assertEquals(
      Moving.of(Vector.empty[Moving.Segment[Px, Norm]], Interp.Hold),
      Left(MovingError.NoSegments)
    )
  }

  test("rejects overlapping segments") {
    val bad = Vector(seg(0, 150, 0.0), seg(100, 200, 50.0))
    val r   = Moving.of(bad, Interp.Hold)
    assert(r.isLeft)
    r.left.foreach(e => assert(clue(e.message).contains("search order")))
  }

  test("rejects segments on different timelines") {
    val other = Moving.Segment(
      Interval.of(ClockId("stimulus"), Instant.millis(300), Instant.millis(400)).toOption.get,
      placementAt(150.0)
    )
    val r = Moving.of(frames :+ other, Interp.Hold)
    assert(r.isLeft)
    r.left.foreach(e => assert(clue(e.message).contains("one timeline")))
  }

  test("rejects segments mapping between different frames") {
    val otherScreen = Frame.screen("other-display", 1000, 1000).toOption.get
    val stray       = Moving.Segment(
      Interval.of(clock, Instant.millis(300), Instant.millis(400)).toOption.get,
      Warp.rescale(otherScreen, stim).toOption.get
    )
    val r = Moving.of(frames :+ stray, Interp.Hold)
    assert(r.isLeft)
    r.left.foreach(e => assert(clue(e.message).contains("share their frames")))
  }

  test("segments are ordered on construction, whatever order they arrive in") {
    val shuffled = Vector(frames(2), frames(0), frames(1))
    val m        = Moving.of(shuffled, Interp.Hold).toOption.get
    assertEquals(m.segments.map(_.interval.onset.toMillis), Vector(0.0, 100.0, 200.0))
  }

  // -------------------------------------------------------------------------
  // Lookup
  // -------------------------------------------------------------------------

  test("Hold returns the segment's transform across its whole extent") {
    val m = Moving.of(frames, Interp.Hold).toOption.get
    val p = Pt[Px](500.0, 500.0)

    // Anywhere inside the second segment, the stimulus is at offset 50.
    val a = m(Instant.millis(100), p).get
    val b = m(Instant.millis(199), p).get
    assertEquals(a, b)
    assertEqualsDouble(a.x, (500.0 - 50.0) / 500.0, 1e-12)
  }

  test("outside the defined extent there is no transform, not a clamped one") {
    val m = Moving.of(frames, Interp.Hold).toOption.get
    assertEquals(m.at(Instant.millis(-1)), None)
    assertEquals(m.at(Instant.millis(300)), None) // half-open: offset excluded
    assert(m.at(Instant.millis(299)).isDefined)
  }

  test("the extent spans first onset to last offset") {
    val m = Moving.of(frames, Interp.Hold).toOption.get
    assertEquals(m.extent.onset.toMillis, 0.0)
    assertEquals(m.extent.offset.toMillis, 300.0)
    assertEquals(m.extent.clock, clock)
  }

  test("lookup is correct across many segments") {
    val many = Vector.tabulate(200)(i => seg(i * 10L, (i + 1) * 10L, i.toDouble))
    val m    = Moving.of(many, Interp.Hold).toOption.get
    // Binary search must agree with a linear scan at every segment boundary.
    (0 until 200).foreach { i =>
      val t = Instant.millis(i * 10L + 5)
      val p = m(t, Pt[Px](500.0, 500.0)).get
      assertEqualsDouble(p.x, (500.0 - i.toDouble) / 500.0, 1e-12, clue(i))
    }
  }

  // -------------------------------------------------------------------------
  // Interpolation
  // -------------------------------------------------------------------------

  test("Lerp blends toward the next segment across the current extent") {
    val m = Moving.of(frames, Interp.Lerp).toOption.get
    val p = Pt[Px](500.0, 500.0)

    // At the start of segment 1 the offset is 0; at its end it approaches 50.
    val atStart = m(Instant.millis(0), p).get
    val atMid   = m(Instant.millis(50), p).get
    assertEqualsDouble(atStart.x, 500.0 / 500.0, 1e-12)
    assertEqualsDouble(atMid.x, (500.0 - 25.0) / 500.0, 1e-12)
  }

  test("Lerp is monotone where the motion is monotone") {
    val m  = Moving.of(frames, Interp.Lerp).toOption.get
    val p  = Pt[Px](500.0, 500.0)
    val xs = (0 to 290 by 10).map(ms => m(Instant.millis(ms.toLong), p).get.x)
    xs.sliding(2).foreach {
      case Seq(a, b) => assert(b <= a, clue((a, b)))
      case _         => ()
    }
  }

  test("Lerp holds on the final segment, having nothing to blend toward") {
    val m = Moving.of(frames, Interp.Lerp).toOption.get
    val p = Pt[Px](500.0, 500.0)
    assertEquals(m(Instant.millis(200), p), m(Instant.millis(299), p))
  }

  test("Hold and Lerp agree at every segment onset") {
    val hold = Moving.of(frames, Interp.Hold).toOption.get
    val lerp = Moving.of(frames, Interp.Lerp).toOption.get
    val p    = Pt[Px](700.0, 300.0)
    Seq(0L, 100L, 200L).foreach { ms =>
      assertEquals(hold(Instant.millis(ms), p), lerp(Instant.millis(ms), p), clue(ms))
    }
  }

  test("Lerp falls back to holding when a transform exposes no matrix") {
    // A tangent projection has no matrix to blend elementwise. Rather than
    // failing, the lookup holds -- documented behaviour, not silent breakage.
    val persp = Perspective.of(Length.mm(600), Length.mm(500), Length.mm(500)).toOption.get
    val ang   = Frame.angular("ang", 45.0, 45.0).toOption.get
    val tSeg  = (a: Long, b: Long) =>
      Moving.Segment(
        Interval.of(clock, Instant.millis(a), Instant.millis(b)).toOption.get,
        Warp.tangent(screen, ang, persp)
      )
    val m = Moving.of(Vector(tSeg(0, 100), tSeg(100, 200)), Interp.Lerp).toOption.get
    assert(m.at(Instant.millis(50)).isDefined)
  }

end MovingSuite
