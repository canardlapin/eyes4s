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

class GridRegionSuite extends munit.FunSuite:

  val screen = Frame.screen("display", 100, 100).toOption.get
  val grid   = Grid.square(screen, 10).toOption.get

  // -------------------------------------------------------------------------
  // Sigma
  // -------------------------------------------------------------------------

  test("a bandwidth must be finite and positive") {
    assert(Sigma.deg(1.0).isRight)
    assert(Sigma.deg(0.0).isLeft)
    assert(Sigma.deg(-1.0).isLeft)
    assert(Sigma.deg(Double.NaN).isLeft)
  }

  test("the error names the convention it is defending") {
    Sigma.px(-30.0).left.foreach { e =>
      assert(clue(e.message).contains("standard deviation"))
      assert(e.message.contains("convert at"))
    }
  }

  test("a degree bandwidth is not a pixel bandwidth") {
    val errs = typeCheckErrors("""
      import eyes4s.kernel.*
      import eyes4s.kernel.Unit2D.Px
      val s: Sigma[Px] = Sigma.deg(1.0).toOption.get
    """)
    assert(errs.nonEmpty, "Sigma[Deg] silently became Sigma[Px]")
  }

  test("variance is the square, and scaling stays validated") {
    val s = Sigma.deg(2.0).toOption.get
    assertEqualsDouble(s.variance, 4.0, 1e-12)
    assert((s * 0.0).isLeft, "scaling to zero must not produce a valid bandwidth")
    assertEqualsDouble((s * 3.0).toOption.get.value, 6.0, 1e-12)
  }

  // -------------------------------------------------------------------------
  // Grid
  // -------------------------------------------------------------------------

  test("a grid needs cells in both axes") {
    assert(Grid.of(GridId("g"), screen, 0, 10).isLeft)
    assert(Grid.of(GridId("g"), screen, 10, -1).isLeft)
  }

  test("index order is x-fastest, and the accessors are mutually consistent") {
    assertEquals(grid.indexAt(3, 7), Some(73))
    assertEquals(grid.columnOf(73), Some(3))
    assertEquals(grid.rowOf(73), Some(7))
    (0 until grid.size).foreach { i =>
      val roundTrip =
        for
          ix <- grid.columnOf(i)
          iy <- grid.rowOf(i)
          k  <- grid.indexAt(ix, iy)
        yield k
      assertEquals(roundTrip, Some(i), clue(i))
      assertEquals(grid.unsafeIndexAt(grid.unsafeColumnOf(i), grid.unsafeRowOf(i)), i, clue(i))
    }
  }

  test("checked accessors answer None off the grid instead of wrapping") {
    // Under unchecked arithmetic a column overrun of one becomes the first
    // cell of the next row: an answer, not an error. The checked accessors
    // refuse it, and refuse every other way of naming a cell that is not there.
    assertEquals(grid.indexAt(grid.nx, 0), None)
    assertEquals(grid.indexAt(-1, 0), None)
    assertEquals(grid.indexAt(0, grid.ny), None)
    assertEquals(grid.indexAt(0, -1), None)
    assertEquals(grid.indexAt(Int.MaxValue, Int.MaxValue), None)
    assertEquals(grid.indexAt(0, 0), Some(0))
    assertEquals(grid.indexAt(grid.nx - 1, grid.ny - 1), Some(grid.size - 1))

    assertEquals(grid.columnOf(-1), None)
    assertEquals(grid.columnOf(grid.size), None)
    assertEquals(grid.rowOf(-1), None)
    assertEquals(grid.rowOf(grid.size), None)
    assertEquals(grid.columnOf(grid.size - 1), Some(grid.nx - 1))
    assertEquals(grid.rowOf(grid.size - 1), Some(grid.ny - 1))

    assertEquals(grid.cellCentre(-1), None)
    assertEquals(grid.cellCentre(grid.size), None)
    assertEquals(grid.cellCentre(Int.MinValue), None)
    assertEquals(grid.cellCentre(Int.MaxValue), None)
    assertEquals(grid.cellCentre(0), Some(Pt[Px](5.0, 5.0)))
    assertEquals(grid.centres.length, grid.size)
  }

  test("grid cardinality cannot overflow its index representation") {
    assertEquals(
      Grid.over(screen, 65536, 65536),
      Left(GeometryError.GridCellCountOverflow(65536, 65536, 4294967296L))
    )
    assertEquals(
      Grid.over(screen, Int.MaxValue, 2),
      Left(GeometryError.GridCellCountOverflow(Int.MaxValue, 2, 4294967294L))
    )
    assertEquals(Grid.over(screen, Int.MaxValue, 1).map(_.size), Right(Int.MaxValue))
  }

  test("a position round-trips to the cell containing it") {
    (0 until grid.size).foreach { i =>
      assertEquals(grid.cellCentre(i).flatMap(grid.indexOf), Some(i), clue(i))
    }
  }

  test("positions outside the frame have no cell") {
    assertEquals(grid.indexOf(Pt[Px](-1.0, 50.0)), None)
    assertEquals(grid.indexOf(Pt[Px](100.0, 50.0)), None) // half-open upper edge
    assertEquals(grid.indexOf(Pt[Px](99.999, 99.999)), Some(99))
  }

  test("cells tile the frame exactly") {
    assertEqualsDouble(grid.cellArea * grid.size, screen.bounds.area, 1e-9)
    assertEqualsDouble(grid.cellWidth, 10.0, 1e-12)
  }

  test("grid identity is nominal: same shape over different frames does not collide") {
    val other = Frame.screen("other", 100, 100).toOption.get
    val a     = Grid.over(screen, 10, 10).toOption.get
    val b     = Grid.over(other, 10, 10).toOption.get
    assertNotEquals(a.id, b.id)
    assertNotEquals(a, b)
  }

  test("the derived name encodes both frame and resolution") {
    assertEquals(Grid.over(screen, 8, 4).toOption.get.id, GridId("display@8x4"))
  }

  // -------------------------------------------------------------------------
  // Region shapes
  // -------------------------------------------------------------------------

  test("rectangles are half-open, so tiles partition rather than overlap") {
    val a      = Region.rect(Pt[Px](0, 0), Pt[Px](50, 100)).toOption.get
    val b      = Region.rect(Pt[Px](50, 0), Pt[Px](100, 100)).toOption.get
    val onSeam = Pt[Px](50.0, 50.0)
    assert(!a.contains(onSeam))
    assert(b.contains(onSeam))
  }

  test("degenerate shapes are rejected") {
    assert(Region.rect(Pt[Px](10, 10), Pt[Px](10, 20)).isLeft)
    assert(Region.ellipse(Pt[Px](0, 0), 0.0, 5.0).isLeft)
    assert(Region.polygon(Vector(Pt[Px](0, 0), Pt[Px](1, 1))).isLeft)
  }

  test("an ellipse contains its centre and excludes beyond its radii") {
    val e = Region.ellipse(Pt[Px](50, 50), 20.0, 10.0).toOption.get
    assert(e.contains(Pt(50, 50)))
    assert(e.contains(Pt(69, 50)))
    assert(!e.contains(Pt(71, 50)))
    assert(!e.contains(Pt(50, 61)))
  }

  test("a polygon contains interior points and excludes exterior ones") {
    // An L shape, which a bounding box would get wrong.
    val l = Region
      .polygon(
        Vector(
          Pt[Px](0, 0),
          Pt[Px](60, 0),
          Pt[Px](60, 20),
          Pt[Px](20, 20),
          Pt[Px](20, 60),
          Pt[Px](0, 60)
        )
      )
      .toOption
      .get
    assert(l.contains(Pt(10, 10)), "inside the corner")
    assert(l.contains(Pt(50, 10)), "inside the foot")
    assert(l.contains(Pt(10, 50)), "inside the stem")
    assert(!l.contains(Pt(50, 50)), "in the notch, which a bounding box would include")
    assert(!l.contains(Pt(70, 70)), "outside entirely")
  }

  // -------------------------------------------------------------------------
  // Boolean algebra
  // -------------------------------------------------------------------------

  val left  = Region.rect(Pt[Px](0, 0), Pt[Px](60, 100)).toOption.get
  val right = Region.rect(Pt[Px](40, 0), Pt[Px](100, 100)).toOption.get

  test("union, intersection, complement and difference behave") {
    val inBoth = Pt[Px](50, 50)
    val onlyL  = Pt[Px](10, 50)
    val onlyR  = Pt[Px](90, 50)

    assert((left || right).contains(onlyL) && (left || right).contains(onlyR))
    assert((left && right).contains(inBoth))
    assert(!(left && right).contains(onlyL))
    assert((left \ right).contains(onlyL))
    assert(!(left \ right).contains(inBoth))
    assert((!left).contains(onlyR))
  }

  test("complement of everything is empty, observationally") {
    val pts = grid.centres
    assert(pts.forall(p => !(!Region.everything[Px]).contains(p)))
    assert(pts.forall(p => Region.everything[Px].contains(p)))
    assert(pts.forall(p => !Region.empty[Px].contains(p)))
  }

  test("de Morgan holds observationally over the grid") {
    val pts = grid.centres
    pts.foreach { p =>
      assertEquals((!(left || right)).contains(p), ((!left) && (!right)).contains(p), clue(p))
      assertEquals((!(left && right)).contains(p), ((!left) || (!right)).contains(p), clue(p))
    }
  }

  test("a region cannot be applied to a position in another unit") {
    val errs = typeCheckErrors("""
      import eyes4s.kernel.*
      import eyes4s.kernel.Unit2D.{Px, Deg}
      val r = Region.rect(Pt[Deg](0, 0), Pt[Deg](1, 1)).toOption.get
      r.contains(Pt[Px](0.5, 0.5))
    """)
    assert(errs.nonEmpty, "a degree region accepted a pixel position")
  }

  // -------------------------------------------------------------------------
  // Area
  // -------------------------------------------------------------------------

  test("primitive shapes report exact area, ignoring the grid") {
    val r = Region.rect(Pt[Px](0, 0), Pt[Px](30, 20)).toOption.get
    assertEqualsDouble(r.area(grid), 600.0, 1e-9)
    assertEqualsDouble(r.area(Grid.square(screen, 2).toOption.get), 600.0, 1e-9)

    val e = Region.ellipse(Pt[Px](50, 50), 10.0, 5.0).toOption.get
    assertEqualsDouble(e.area(grid), math.Pi * 50.0, 1e-9)
  }

  test("composite area is sampled, and converges as the grid refines") {
    val union  = left || right // covers the whole 100x100 frame
    val coarse = union.area(Grid.square(screen, 4).toOption.get)
    val fine   = union.area(Grid.square(screen, 200).toOption.get)
    assertEqualsDouble(fine, 10000.0, 1.0)
    assert(math.abs(fine - 10000.0) <= math.abs(coarse - 10000.0) + 1e-9)
  }

  test("rasterise agrees with contains at every cell") {
    val r    = Region.ellipse(Pt[Px](50, 50), 25.0, 25.0).toOption.get
    val bits = r.rasterise(grid)
    assertEquals(bits.length, grid.size)
    (0 until grid.size).foreach { i =>
      assertEquals(bits(i), r.contains(grid.unsafeCellCentre(i)), clue(i))
    }
  }

end GridRegionSuite
