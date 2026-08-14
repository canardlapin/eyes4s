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

import eyes4s.kernel.Unit2D.{Deg, Norm, Px}

import scala.compiletime.testing.typeCheckErrors

class WarpSuite extends munit.FunSuite:

  // A typical bench setup: 1280x1024 on a 530x300mm display at 600mm.
  val screen  = Frame.screen("bench", 1280, 1024).toOption.get
  val angular = Frame
    .angular(
      "angular",
      2 * math.atan(265.0 / 600.0) * 180 / math.Pi,
      2 * math.atan(150.0 / 600.0) * 180 / math.Pi
    )
    .toOption
    .get
  val persp = Perspective.of(Length.mm(600), Length.mm(530), Length.mm(300)).toOption.get
  val unit  = Frame.unitSquare("norm").toOption.get

  val toDeg = Warp.tangent(screen, angular, persp)

  private def close(a: Double, b: Double, eps: Double = 1e-9): Boolean =
    math.abs(a - b) <= eps

  // -------------------------------------------------------------------------
  // Perspective
  // -------------------------------------------------------------------------

  test("perspective rejects non-positive and non-finite geometry") {
    assert(Perspective.of(Length.mm(0), Length.mm(530), Length.mm(300)).isLeft)
    assert(Perspective.of(Length.mm(600), Length.mm(-1), Length.mm(300)).isLeft)
    assert(Perspective.of(Length.mm(Double.NaN), Length.mm(530), Length.mm(300)).isLeft)
  }

  test("perspective reports the angle the surface subtends") {
    // 530mm wide at 600mm subtends 2*atan(265/600) = 47.66 degrees.
    assertEqualsDouble(persp.horizontalExtent.toDegrees, 47.6589, 1e-3)
    // 300mm tall at 600mm subtends 2*atan(150/600) = 28.07 degrees.
    assertEqualsDouble(persp.verticalExtent.toDegrees, 28.0725, 1e-3)
  }

  // -------------------------------------------------------------------------
  // Tangent projection
  // -------------------------------------------------------------------------

  test("a tangent warp specifically requires pixels to degrees") {
    val wrongSource = typeCheckErrors("""
      import eyes4s.kernel.*
      val norm = Frame.unitSquare("norm").toOption.get
      val deg = Frame.angular("deg", 20, 20).toOption.get
      val p = Perspective.of(Length.mm(600), Length.mm(500), Length.mm(300)).toOption.get
      Warp.tangent(norm, deg, p)
    """)
    val wrongTarget = typeCheckErrors("""
      import eyes4s.kernel.*
      val px = Frame.screen("px", 1000, 800).toOption.get
      val norm = Frame.unitSquare("norm").toOption.get
      val p = Perspective.of(Length.mm(600), Length.mm(500), Length.mm(300)).toOption.get
      Warp.tangent(px, norm, p)
    """)
    assert(wrongSource.nonEmpty, "a normalised position was treated as a display pixel")
    assert(wrongTarget.nonEmpty, "a tangent projection produced a non-angular frame")
  }

  test("the screen centre maps to zero eccentricity") {
    val c = toDeg(screen.centre).get
    assert(close(c.x, 0.0, 1e-12), clue(c))
    assert(close(c.y, 0.0, 1e-12), clue(c))
  }

  test("px -> deg -> px round-trips (milestone v0.1 acceptance)") {
    val back    = toDeg.inverse.get
    val samples = Seq(
      Pt[Px](0.0, 0.0),
      Pt[Px](640.0, 512.0),
      Pt[Px](1279.0, 1023.0),
      Pt[Px](100.0, 900.0),
      Pt[Px](1000.0, 200.0)
    )
    samples.foreach { p =>
      val r = toDeg(p).flatMap(back.apply).get
      assert(close(r.x, p.x, 1e-9), clue((p, r)))
      assert(close(r.y, p.y, 1e-9), clue((p, r)))
    }
  }

  test("the projection is a tangent, not the linear approximation") {
    // A point at the right edge, 640px = 265mm from centre, at 600mm.
    val edge  = Pt[Px](1280.0, 512.0)
    val exact = math.atan(265.0 / 600.0) * 180.0 / math.Pi // 23.83 deg
    val got   = toDeg(edge).get.x
    assertEqualsDouble(got, exact, 1e-9)

    // The linear approximation used in much eye-tracking code takes the
    // degrees-per-pixel at the centre and multiplies. At the edge it is wrong
    // by several percent, and always in the same direction.
    val degPerPxAtCentre = (math.atan(1.0 / 600.0) * 180.0 / math.Pi) / (1.0 / (530.0 / 1280.0))
    val linear           = 640.0 * degPerPxAtCentre
    assert(linear > got, clue((linear, got)))
    assert((linear - got) / got > 0.05, clue(s"linear error only ${(linear - got) / got}"))
  }

  test("the y axis flip between screen and angular frames is applied") {
    // Screen y runs downward, angular y runs upward. A point ABOVE the screen
    // centre (smaller y in device coordinates) must be at POSITIVE elevation.
    val aboveCentre = Pt[Px](640.0, 312.0)
    val d           = toDeg(aboveCentre).get
    assert(d.y > 0.0, clue(d))

    val belowCentre = Pt[Px](640.0, 712.0)
    assert(toDeg(belowCentre).get.y < 0.0, clue(toDeg(belowCentre)))
  }

  // -------------------------------------------------------------------------
  // Composition is a partial category
  // -------------------------------------------------------------------------

  test("composition succeeds when the frames meet") {
    val toNorm   = Warp.rescale(screen, unit).toOption.get
    val composed = Warp.rescale(unit, screen).flatMap(back => toNorm.andThen(back))
    assert(composed.isRight)
  }

  test("composition refuses when the frames do not meet") {
    // Note what it takes to write this test at all. The UNIT parameters must
    // line up or the composition is rejected statically, before any frame
    // check runs. Isolating the runtime check therefore requires two warps
    // that agree on Norm but disagree on which Norm frame -- which is exactly
    // the situation the check exists for.
    val otherNorm =
      Frame.of(FrameId("other-norm"), Bounds.sized[Norm](1.0, 1.0).toOption.get, YAxis.Down)

    val toNorm    = Warp.rescale(screen, unit).toOption.get      // Px   -> Norm("norm")
    val fromOther = Warp.rescale(otherNorm, screen).toOption.get // Norm("other-norm") -> Px

    val bad = toNorm.andThen(fromOther)
    assertEquals(bad, Left(GeometryError.FrameMismatch(FrameId("norm"), FrameId("other-norm"))))
  }

  test("identically shaped frames are still different frames") {
    // Same bounds, same axis, same unit. Only the identity differs, and that
    // is enough: a warp into stimulus A does not compose with a warp out of
    // stimulus B.
    val a = Frame.of(FrameId("image-a"), Bounds.sized[Norm](1.0, 1.0).toOption.get, YAxis.Down)
    val b = Frame.of(FrameId("image-b"), Bounds.sized[Norm](1.0, 1.0).toOption.get, YAxis.Down)
    assertEquals(a.bounds, b.bounds)

    val into  = Warp.rescale(screen, a).toOption.get
    val outOf = Warp.rescale(b, screen).toOption.get
    assert(into.andThen(outOf).isLeft, "same geometry must not imply composable")
  }

  test("identity is a left and right unit of composition") {
    val toNorm = Warp.rescale(screen, unit).toOption.get
    val p      = Pt[Px](317.0, 811.0)

    val left  = Warp.id(screen).andThen(toNorm).toOption.get
    val right = toNorm.andThen(Warp.id(unit)).toOption.get

    val direct = toNorm(p).get
    assertEquals(left(p).get, direct)
    assertEquals(right(p).get, direct)
  }

  test("composition is associative where it is defined") {
    val a = Warp.rescale(screen, unit).toOption.get
    val b = Warp.rescale(unit, angular).toOption.get
    val c = Warp.rescale(angular, screen).toOption.get

    val leftAssoc  = a.andThen(b).flatMap(_.andThen(c)).toOption.get
    val rightAssoc = b.andThen(c).flatMap(a.andThen).toOption.get

    val p = Pt[Px](417.0, 233.0)
    val l = leftAssoc(p).get
    val r = rightAssoc(p).get
    assert(close(l.x, r.x, 1e-9), clue((l, r)))
    assert(close(l.y, r.y, 1e-9), clue((l, r)))
  }

  test("a composite inverts structurally: (g . f)^-1 = f^-1 . g^-1") {
    val a        = Warp.rescale(screen, unit).toOption.get
    val b        = Warp.rescale(unit, angular).toOption.get
    val composed = a.andThen(b).toOption.get
    val back     = composed.inverse.get

    assertEquals(back.from.id, angular.id)
    assertEquals(back.to.id, screen.id)

    val p = Pt[Px](900.0, 100.0)
    val r = composed(p).flatMap(back.apply).get
    assert(close(r.x, p.x, 1e-9), clue((p, r)))
    assert(close(r.y, p.y, 1e-9), clue((p, r)))
  }

  // -------------------------------------------------------------------------
  // Construction is closed
  // -------------------------------------------------------------------------

  test("Then cannot be constructed directly, bypassing the frame check") {
    val errs = typeCheckErrors("""
      import eyes4s.kernel.*
      import eyes4s.kernel.Unit2D.{Px, Norm}
      val s = Frame.screen("s", 100, 100).toOption.get
      val u = Frame.unitSquare("u").toOption.get
      val f = Warp.rescale(s, u).toOption.get
      Warp.Then(f, f)
    """)
    assert(errs.nonEmpty, "Then was constructible outside the checked path")
  }

  test("affine rejects a matrix that is not affine") {
    val projective = Mat3(1, 0, 0, 0, 1, 0, 0.001, 0, 1)
    val r          = Warp.affine(screen, unit, projective)
    assert(r.isLeft)
    r.left.foreach(e => assert(clue(e.message).contains("homography")))
  }

  test("a singular affine has no inverse, and says so rather than throwing") {
    val collapse = Mat3.affine(1, 0, 0, 0, 0, 0) // flattens y
    val w        = Warp.affine(screen, unit, collapse).toOption.get
    assertEquals(w.inverse, None)
  }

  // -------------------------------------------------------------------------
  // Rescale
  // -------------------------------------------------------------------------

  test("rescale maps corners to corners and centre to centre") {
    val toNorm = Warp.rescale(screen, unit).toOption.get
    val c      = toNorm(screen.centre).get
    assert(close(c.x, 0.5, 1e-12), clue(c))
    assert(close(c.y, 0.5, 1e-12), clue(c))

    val topLeft = toNorm(Pt[Px](0.0, 0.0)).get
    assert(close(topLeft.x, 0.0, 1e-12), clue(topLeft))
    assert(close(topLeft.y, 0.0, 1e-12), clue(topLeft))
  }

  test("rescale between opposite y-axis conventions flips the vertical") {
    val flipped = Frame
      .of(FrameId("flipped"), Bounds.sized[Norm](1.0, 1.0).toOption.get, YAxis.Up)
    val w = Warp.rescale(screen, flipped).toOption.get
    // A point near the top of a y-down screen is near the top of a y-up frame,
    // which means a HIGH y value there.
    val nearTop = w(Pt[Px](640.0, 10.0)).get
    assert(nearTop.y > 0.9, clue(nearTop))
  }

  // -------------------------------------------------------------------------
  // Rendering
  // -------------------------------------------------------------------------

  test("a composite warp prints its structure") {
    val a        = Warp.rescale(screen, unit).toOption.get
    val b        = Warp.rescale(unit, angular).toOption.get
    val composed = a.andThen(b).toOption.get
    val r        = composed.render
    assert(r.contains("then"), clue(r))
    assert(r.contains("bench"), clue(r))
    assert(r.contains("angular"), clue(r))
  }

  test("a tangent warp names its perspective") {
    assert(clue(toDeg.render).contains("600.0mm"))
  }

end WarpSuite
