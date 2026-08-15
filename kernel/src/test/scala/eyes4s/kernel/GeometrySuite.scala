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

import eyes4s.kernel.Unit2D.{Deg, Px}

import scala.compiletime.testing.typeCheckErrors

class GeometrySuite extends munit.FunSuite:

  val screen       = Frame.screen("bench", 1280, 1024).toOption.get
  val angularFrame = Frame.angular("angularFrame", 34.0, 27.0).toOption.get

  // -------------------------------------------------------------------------
  // Unit tagging  (a strengthening over the architecture note)
  // -------------------------------------------------------------------------

  test("a pixel position is not a degree position") {
    val errs = typeCheckErrors("""
      import eyes4s.kernel.*
      import eyes4s.kernel.Unit2D.{Px, Deg}
      val p: Pt[Deg] = Pt[Px](10.0, 10.0)
    """)
    assert(errs.nonEmpty, "Pt[Px] silently became Pt[Deg]")
  }

  test("a region-style containment test rejects the wrong unit") {
    val errs = typeCheckErrors("""
      import eyes4s.kernel.*
      import eyes4s.kernel.Unit2D.{Px, Deg}
      val b = Bounds.sized[Deg](10.0, 10.0).toOption.get
      b.contains(Pt[Px](1.0, 1.0))
    """)
    assert(errs.nonEmpty, "degree bounds accepted a pixel position")
  }

  test("unit labels are available for rendering and codecs") {
    assertEquals(UnitLabel[Px].symbol, "px")
    assertEquals(UnitLabel[Deg].symbol, "deg")
  }

  // -------------------------------------------------------------------------
  // Affine discipline: points and displacements
  // -------------------------------------------------------------------------

  test("two positions cannot be added") {
    val errs = typeCheckErrors("""
      import eyes4s.kernel.*
      import eyes4s.kernel.Unit2D.Px
      Pt[Px](1.0, 2.0) + Pt[Px](3.0, 4.0)
    """)
    assert(errs.nonEmpty, "adding two positions compiled")
  }

  test("difference of positions is a displacement; position plus displacement is a position") {
    val a = Pt[Px](100.0, 100.0)
    val b = Pt[Px](130.0, 140.0)
    val v = a.vectorTo(b)
    assertEquals(v, Vec2[Px](30.0, 40.0))
    assertEquals(v.norm, 50.0)
    assertEquals(a + v, b)
    assertEquals(b - v, a)
  }

  test("displacement round-trips through polar form") {
    val v = Vec2[Deg](3.0, 4.0)
    val w = Vec2.polar[Deg](v.norm, v.angle)
    assert(math.abs(w.dx - v.dx) < 1e-12, clue(w))
    assert(math.abs(w.dy - v.dy) < 1e-12, clue(w))
  }

  // -------------------------------------------------------------------------
  // Angle wrapping
  // -------------------------------------------------------------------------

  test("angular separation wraps the short way round") {
    val up    = Angle.degrees(3.0)
    val other = Angle.degrees(-3.0)
    assertEqualsDouble(up.separationFrom(other).toDegrees, 6.0, 1e-9)

    // The case that makes naive subtraction wrong: 350 and 10 are 20 apart.
    val a = Angle.degrees(350.0)
    val b = Angle.degrees(10.0)
    assertEqualsDouble(a.separationFrom(b).toDegrees, 20.0, 1e-9)
  }

  test("separation is symmetric and bounded by pi") {
    val samples = Seq(-359.0, -180.0, -3.0, 0.0, 17.0, 179.0, 180.0, 359.0)
    for x <- samples; y <- samples do
      val a = Angle.degrees(x)
      val b = Angle.degrees(y)
      assertEqualsDouble(a.separationFrom(b).toRadians, b.separationFrom(a).toRadians, 1e-12)
      assert(a.separationFrom(b).toRadians <= math.Pi + 1e-12, clue((x, y)))
      assert(a.separationFrom(b).toRadians >= 0.0, clue((x, y)))
  }

  test("normalised folds into (-pi, pi]") {
    assertEqualsDouble(Angle.degrees(270.0).normalised.toDegrees, -90.0, 1e-9)
    assertEqualsDouble(Angle.degrees(-270.0).normalised.toDegrees, 90.0, 1e-9)
    assertEqualsDouble(Angle.degrees(180.0).normalised.toDegrees, 180.0, 1e-9)
  }

  // -------------------------------------------------------------------------
  // Bounds
  // -------------------------------------------------------------------------

  test("bounds reject degenerate and inverted rectangles") {
    assert(Bounds.of[Px](0, 0, 0, 100).isLeft, "zero width accepted")
    assert(Bounds.of[Px](0, 0, 100, 0).isLeft, "zero height accepted")

    // The R convention encodes a y-flip by inverting the vertical pair. Here
    // that is a construction error, and YAxis carries the flip instead.
    val inverted = Bounds.of[Px](0, 1024, 1280, 0)
    assert(inverted.isLeft)
    inverted.left.foreach(e => assert(clue(e.message).contains("YAxis")))
  }

  test("bounds reject non-finite input") {
    assert(Bounds.of[Px](0, 0, Double.NaN, 100).isLeft)
    assert(Bounds.of[Px](0, 0, Double.PositiveInfinity, 100).isLeft)
  }

  test("bounds are half-open, matching Interval") {
    val b = Bounds.sized[Px](100.0, 100.0).toOption.get
    assert(b.contains(Pt(0.0, 0.0)))
    assert(b.contains(Pt(99.999, 99.999)))
    assert(!b.contains(Pt(100.0, 50.0)))
    assert(!b.contains(Pt(50.0, 100.0)))
  }

  test("diagonal is computed from the bounds, once") {
    val b = Bounds.sized[Px](3.0, 4.0).toOption.get
    assertEquals(b.diagonal, 5.0)
    assertEquals(b.centre, Pt[Px](1.5, 2.0))
    assertEquals(b.area, 12.0)
  }

  test("clamp brings a stray position back inside") {
    val b       = Bounds.sized[Px](100.0, 100.0).toOption.get
    val clamped = b.clamp(Pt(-10.0, 150.0))
    assertEquals(clamped, Pt[Px](0.0, java.lang.Math.nextDown(100.0)))
    assert(b.contains(clamped))
  }

  test("clamp is closed over half-open bounds for every finite side case") {
    val b           = Bounds.sized[Px](100.0, 100.0).toOption.get
    val coordinates = Vector(-Double.MaxValue, -1.0, 0.0, 50.0, 100.0, Double.MaxValue)

    coordinates.foreach { x =>
      coordinates.foreach { y =>
        assert(b.contains(b.clamp(Pt[Px](x, y))), clue((x, y)))
      }
    }
  }

  // -------------------------------------------------------------------------
  // Frame
  // -------------------------------------------------------------------------

  test("screen frames use device convention: origin top-left, y downward") {
    assertEquals(screen.yAxis, YAxis.Down)
    assertEquals(screen.bounds.xMin, 0.0)
    assertEquals(screen.centre, Pt[Px](640.0, 512.0))
    assertEqualsDouble(screen.diagonal, math.hypot(1280.0, 1024.0), 1e-12)
  }

  test("angular frames are centred on the origin with y upward") {
    assertEquals(angularFrame.yAxis, YAxis.Up)
    assertEquals(angularFrame.centre, Pt[Deg](0.0, 0.0))
    assertEqualsDouble(angularFrame.bounds.xMin, -17.0, 1e-12)
    assertEqualsDouble(angularFrame.bounds.yMax, 13.5, 1e-12)
  }

  test("frame identity is nominal, not structural") {
    val imageA = Frame.screen("image-a", 800, 600).toOption.get
    val imageB = Frame.screen("image-b", 800, 600).toOption.get

    // Same geometry, different things. Structural equality would conflate them.
    assertEquals(imageA.bounds, imageB.bounds)
    assertNotEquals(imageA.id, imageB.id)
    assertNotEquals(imageA, imageB)
  }

  test("withId preserves geometry and changes identity") {
    val other = screen.withId(FrameId("other"))
    assertEquals(other.bounds, screen.bounds)
    assertEquals(other.yAxis, screen.yAxis)
    assertNotEquals(other, screen)
  }

  test("frame mismatch error explains that the unit is not enough") {
    val e = GeometryError.FrameMismatch(FrameId("image-a"), FrameId("image-b"))
    assert(clue(e.message).contains("same unit"))
    assert(e.message.contains("Warp"))
  }

end GeometrySuite
