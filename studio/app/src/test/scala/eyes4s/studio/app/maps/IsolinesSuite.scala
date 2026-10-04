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
  * level, and no line is drawn through a missing value.
  */
class IsolinesSuite extends munit.FunSuite:
  import MapSamples.*

  // Each cell's value is its column: the grid crosses 31.5 midway between
  // the centres of columns 31 and 32, at x = 32 cells.
  private val ramp: Vector[Option[Double]] =
    Vector.tabulate(Columns * Rows)(i => Some((i % Columns).toDouble))

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

  test("every segment end lies on an edge whose ends straddle the level") {
    val levels = Vector(0.5, 0.9)
    val g      = grid(id("ret_07", 2), bump, levels)
    Isolines.of(g).foreach { line =>
      assert(line.segments.nonEmpty, line.level)
      line.segments.flatMap((a, b) => Vector(a, b)).foreach { p =>
        // A point on a horizontal edge has an integer-plus-half y, on a
        // vertical edge an integer-plus-half x; interpolate along it.
        val (cx, cy)    = (p.x - 0.5, p.y - 0.5)
        val onRow       = math.abs(cy - math.rint(cy)) < 1e-9
        val (v0, v1, t) =
          if onRow then
            val i = math.floor(cx).toInt; val j = math.rint(cy).toInt
            (g.at(i, j).get, g.at(i + 1, j).get, cx - i)
          else
            val i = math.rint(cx).toInt; val j = math.floor(cy).toInt
            (g.at(i, j).get, g.at(i, j + 1).get, cy - j)
        assert(math.min(v0, v1) <= line.level && line.level <= math.max(v0, v1), p)
        assertEqualsDouble(v0 + (v1 - v0) * t, line.level, 1e-9)
      }
    }
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

  test("a saddle is split by the mean of its corners") {
    // 2 × 2 grid: a and c in, b and d out (case 10).
    def two(values: Double*) =
      MapGrid
        .of(id("ret_01", 2), 2, 2, values.toVector.map(Some(_)), Vector(0.5))
        .fold(e => fail(e.message), identity)
    val joined = Isolines.contour(two(1.0, 0.0, 0.0, 1.0), 0.5) // mean 0.5: inside, joined
    val apart  = Isolines.contour(two(0.6, 0.0, 0.0, 0.6), 0.5) // mean 0.3: outside
    assertEquals(joined.size, 2)
    assertEquals(apart.size, 2)
    assertNotEquals(joined.toSet, apart.toSet)
  }
