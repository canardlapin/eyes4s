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

package eyes4s.studio.app.maps

/** Contouring a map at its backend-supplied levels (ticket S4.3b): the
  * levels are the grid's, every segment lies where the grid crosses its
  * level, nothing is drawn through a missing value, saddles and cells
  * exactly at the level follow their stated rules, and the stored row
  * order never changes the picture.
  */
class IsolinesSuite extends munit.FunSuite:
  import MapSamples.*

  // Each cell's value is its column: the grid crosses 31.5 midway between
  // the centres of columns 31 and 32, at x = 32 cells.
  private val ramp: Vector[Option[Double]] =
    Vector.tabulate(Columns * Rows)(i => Some((i % Columns).toDouble))

  // An off-centre bump, peaking near column 20 and row 12 (from the top).
  private val offCentre: Vector[Option[Double]] =
    Vector.tabulate(Columns * Rows) { i =>
      val (x, y) = (i % Columns, i / Columns)
      val dx     = (x - 20.0) / 8.0
      val dy     = (y - 12.0) / 6.0
      Some(math.exp(-(dx * dx + dy * dy) / 2.0))
    }

  // The same map stored bottom row first.
  private def bottomFirst(cells: Vector[Option[Double]]): Vector[Option[Double]] =
    cells.grouped(Columns).toVector.reverse.flatten

  private def tiny(order: RowOrder, values: Double*): MapGrid =
    MapGrid
      .of(id("ret_01", 2), 2, values.size / 2, order, values.toVector.map(Some(_)), Vector(0.5))
      .fold(e => fail(e.message), identity)

  test("a level is contoured where the grid crosses it, at the grid's levels only") {
    val g     = grid(id("ret_01", 2), ramp, Vector(31.5))
    val lines = Isolines.of(g)
    assertEquals(lines.map(_.level), Vector(31.5))
    val segs = lines.head.segments
    // One vertical segment per row of squares, at x = 32.
    assertEquals(segs.size, Rows - 1)
    segs.foreach { (a, b) =>
      assertEqualsDouble(a.x, 32.0, 1e-12)
      assertEqualsDouble(b.x, 32.0, 1e-12)
      assertEqualsDouble(math.abs(b.y - a.y), 1.0, 1e-12)
    }
    // Without levels there is nothing to contour: studio derives none.
    assertEquals(Isolines.of(grid(id("ret_01", 2), ramp)), Vector.empty)
  }

  test(
    "every segment end lies on an edge whose ends straddle the level, off-centre in both axes"
  ) {
    val g = grid(id("ret_07", 2), offCentre, Vector(0.5, 0.9))
    Isolines.of(g).foreach { line =>
      assert(line.segments.nonEmpty, line.level)
      val ends = line.segments.flatMap((a, b) => Vector(a, b))
      ends.foreach { p =>
        val (cx, cy)    = (p.x - 0.5, p.y - 0.5)
        val onRow       = math.abs(cy - math.rint(cy)) < 1e-9
        val (v0, v1, t) =
          if onRow then
            val i = math.floor(cx).toInt; val j = math.rint(cy).toInt
            (g.atTop(i, j).get, g.atTop(i + 1, j).get, cx - i)
          else
            val i = math.rint(cx).toInt; val j = math.floor(cy).toInt
            (g.atTop(i, j).get, g.atTop(i, j + 1).get, cy - j)
        assert(math.min(v0, v1) <= line.level && line.level <= math.max(v0, v1), p)
        assertEqualsDouble(v0 + (v1 - v0) * t, line.level, 1e-9)
      }
      // The contour rings the bump where it is: near column 20, row 12 from the top.
      val (mx, my) = (ends.map(_.x).sum / ends.size, ends.map(_.y).sum / ends.size)
      assertEqualsDouble(mx, 20.5, 1.0)
      assertEqualsDouble(my, 12.5, 1.0)
    }
  }

  test("the stored row order never changes the contours: bottom-first is the same picture") {
    val top    = grid(id("ret_07", 2), offCentre, Vector(0.5), RowOrder.TopFirst)
    val bottom =
      grid(id("ret_07", 2), bottomFirst(offCentre), Vector(0.5), RowOrder.BottomFirst)
    assertEquals(Isolines.of(bottom), Isolines.of(top))
    // Read as top-first, the bottom-first cells would put the bump near the bottom.
    val misread = grid(id("ret_07", 2), bottomFirst(offCentre), Vector(0.5), RowOrder.TopFirst)
    assertNotEquals(Isolines.of(misread), Isolines.of(top))
    val ys = Isolines.of(misread).head.segments.flatMap((a, b) => Vector(a.y, b.y))
    assertEqualsDouble(ys.sum / ys.size, Rows - 12.5, 1.0)
  }

  test("no line is drawn through a missing value") {
    val holed = ramp.zipWithIndex.map((v, i) =>
      if i % Columns == 31 && i / Columns == 10 then None else v
    )
    val g    = grid(id("ret_01", 2), holed, Vector(31.5))
    val segs = Isolines.of(g).head.segments
    // The two squares that touch the missing cell draw nothing.
    assertEquals(segs.size, Rows - 1 - 2)
    assert(segs.forall((a, b) => !(math.min(a.y, b.y) >= 9.5 && math.max(a.y, b.y) <= 11.5)))
  }

  // Grid values are row-major from the top: (a, b) then (d, c), where a is
  // the top-left centre (0.5, 0.5), b top right, c bottom right, d bottom left.
  private def saddle(a: Double, b: Double, d: Double, c: Double) =
    Isolines.of(tiny(RowOrder.TopFirst, a, b, d, c)).head.segments

  private def same(
      got: Vector[(GridPoint, GridPoint)],
      want: Vector[((Double, Double), (Double, Double))]
  ): Unit =
    assertEquals(got.size, want.size, got)
    got.zip(want).foreach { case ((p, q), ((px, py), (qx, qy))) =>
      Vector(p.x -> px, p.y -> py, q.x -> qx, q.y -> qy)
        .foreach((g, w) => assertEqualsDouble(g, w, 1e-12, got))
    }

  test("saddle case 10 (a and c in) splits by the corner mean, exactly") {
    // Mean 0.5, in: the middle joins a and c, so d's corner and b's are cut off.
    same(saddle(1.0, 0.0, 0.0, 1.0), Vector(((0.5, 1.0), (1.0, 1.5)), ((1.0, 0.5), (1.5, 1.0))))
    // Mean 0.3, out: a's and c's corners are cut off.
    val sixth = 1.0 / 6.0
    same(
      saddle(0.6, 0.0, 0.0, 0.6),
      Vector(((0.5, 0.5 + sixth), (0.5 + sixth, 0.5)), ((1.5 - sixth, 1.5), (1.5, 1.5 - sixth)))
    )
  }

  test("saddle case 5 (b and d in) splits by the corner mean, exactly") {
    // Mean 0.5, in: the middle joins b and d, so a's and c's corners are cut off.
    same(saddle(0.0, 1.0, 1.0, 0.0), Vector(((0.5, 1.0), (1.0, 0.5)), ((1.0, 1.5), (1.5, 1.0))))
    // Mean 0.3, out: b's and d's corners are cut off.
    val sixth = 1.0 / 6.0
    same(
      saddle(0.0, 0.6, 0.6, 0.0),
      Vector(((0.5, 1.5 - sixth), (0.5 + sixth, 1.5)), ((1.5 - sixth, 0.5), (1.5, 0.5 + sixth)))
    )
  }

  test("a cell exactly at the level is inside") {
    // Only a (top left) is at 0.5: with >= it is inside, a corner case
    // whose segment is cut at a's centre itself; with > there would be none.
    val segs = Isolines.of(tiny(RowOrder.TopFirst, 0.5, 0.0, 0.0, 0.0)).head.segments
    assertEquals(segs, Vector((GridPoint(0.5, 0.5), GridPoint(0.5, 0.5))))
  }
