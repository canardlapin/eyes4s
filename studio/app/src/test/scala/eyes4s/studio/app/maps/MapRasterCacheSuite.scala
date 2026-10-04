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

import eyes4s.studio.core.fixture.MockStudy
import eyes4s.studio.core.selection.ScaleIndex

/** Map rasters and their cache (ticket S4.4): a raster colours each cell
  * and changes nothing in its grid; the cache keeps the most recently used
  * rasters within its byte budget, also under ten times the fixture's maps;
  * isoline levels pass through as the backend served them.
  */
class MapRasterCacheSuite extends munit.FunSuite:

  // Ten times the fixture's maps is about 19,000 rasters.
  override val munitTimeout: scala.concurrent.duration.Duration =
    scala.concurrent.duration.Duration(180, "s")
  import MapSamples.*

  private def raster(trial: String, scale: Int = 2): MapRaster =
    MapRaster.render(grid(id(trial, scale), bump), massStyle)

  private def budgetOf(rasters: Int): RasterBudget =
    RasterBudget.of(rasters * raster("ret_01").bytes).getOrElse(fail("no budget"))

  test("a raster has one pixel per cell, in the palette's colour, missing transparent") {
    val g = grid(id("ret_01", 2), bump)
    val r = MapRaster.render(g, massStyle)
    assertEquals((r.width, r.height), (Columns, Rows))
    assertEquals(r.key, RasterKey(g.map, massStyle))
    for
      y <- 0 until Rows
      x <- 0 until Columns
    do assertEquals(r.pixel(x, y), MapColours.argb(massStyle, g.at(x, y)))
    assertEquals(r.pixel(Columns - 1, 0), MapColours.NoCoverage)
    assertEquals(r.bytes, Columns * Rows * 4L)
  }

  test("isoline levels are kept exactly as served and never change a raster") {
    val levels = Vector(0.4124, 0.0871)
    val with_  = grid(id("ret_01", 2), bump, levels)
    val bare   = grid(id("ret_01", 2), bump)
    assertEquals(with_.levels, levels)
    assertEquals(bare.levels, Vector.empty)
    assertEquals(
      MapRaster.render(with_, massStyle).argb.toVector,
      MapRaster.render(bare, massStyle).argb.toVector
    )
  }

  test("a grid refuses an empty shape, a wrong cell count and a non-finite value or level") {
    val m = id("ret_01", 2)
    assertEquals(
      MapGrid.of(m, 0, 2, Vector.empty, Vector.empty),
      Left(MapGridError.Empty(m, 0, 2))
    )
    assertEquals(
      MapGrid.of(m, 2, 2, Vector.fill(3)(Some(1.0)), Vector.empty),
      Left(MapGridError.CellCount(m, 2, 2, 3))
    )
    assertEquals(
      MapGrid
        .of(m, 2, 1, Vector(Some(1.0), Some(Double.NaN)), Vector.empty)
        .left
        .map(_.productPrefix),
      Left("NotFinite")
    )
    assertEquals(
      MapGrid
        .of(m, 2, 1, Vector(Some(1.0), None), Vector(Double.PositiveInfinity))
        .left
        .map(_.productPrefix),
      Left("Level")
    )
  }

  test("the cache evicts the least recently used raster, and a lookup refreshes one") {
    val (a, b, c, d) = (raster("ret_01"), raster("ret_02"), raster("ret_03"), raster("ret_04"))
    val c0           = RasterCache.empty(budgetOf(3))
    val (c1, e1)     = c0.put(a)
    val (c2, _)      = c1.put(b)
    val (c3, _)      = c2.put(c)
    assertEquals(e1, Vector.empty)
    assertEquals(c3.size, 3)
    // Touching a makes b the least recently used.
    val (hit, c4) = c3.get(a.key)
    assertEquals(hit.map(_.key), Some(a.key))
    val (c5, evicted) = c4.put(d)
    assertEquals(evicted, Vector(b.key))
    assert(
      c5.contains(a.key) && c5.contains(c.key) && c5.contains(d.key) && !c5.contains(b.key)
    )
    assertEquals(c5.bytes, 3 * a.bytes)
    // A miss leaves the cache as it was.
    assertEquals(c5.get(b.key), (None, c5))
    // Putting a key again replaces its raster without growing the cache.
    val (c6, none) =
      c5.put(MapRaster.render(grid(id("ret_01", 2), bump), massStyle))
    assertEquals((c6.size, c6.bytes, none), (3, 3 * a.bytes, Vector.empty))
    // A raster larger than the whole budget is not kept.
    val (c7, _) = RasterCache.empty(RasterBudget.of(100L).get).put(a)
    assertEquals(c7.size, 0)
  }

  test("a raster of another palette or limits is another entry") {
    val g       = grid(id("ret_01", 2), bump)
    val (c1, _) = RasterCache.empty(budgetOf(5)).put(MapRaster.render(g, massStyle))
    val (c2, _) = c1.put(MapRaster.render(g, mass(0.0, 2.0)))
    val (c3, _) = c2.put(MapRaster.render(g, differenceStyle))
    assertEquals(c3.size, 3)
    // The same trial and scale in another run is another map: a rerun's
    // raster never stands in for this run's.
    val rerun   = grid(id("ret_01", 2, run = 8), bump.map(_.map(_ * 0.5)))
    val (c4, _) = c3.put(MapRaster.render(rerun, massStyle))
    assertEquals(c4.size, 4)
    val (old, _)  = c4.get(RasterKey(g.map, massStyle))
    val (next, _) = c4.get(RasterKey(rerun.map, massStyle))
    assertNotEquals(old.map(_.argb.toVector), next.map(_.argb.toVector))
    assertEquals(
      next.map(_.argb.toVector),
      Some(MapRaster.render(rerun, massStyle).argb.toVector)
    )
    // Invalidating a map drops all its rasters and no other.
    val (c5, gone) = c4.invalidate(g.map)
    assertEquals(
      gone.toSet,
      Set(
        RasterKey(g.map, massStyle),
        RasterKey(g.map, mass(0.0, 2.0)),
        RasterKey(g.map, differenceStyle)
      )
    )
    assertEquals(c5.size, 1)
    assertEquals(c5.bytes, MapRaster.render(rerun, massStyle).bytes)
    assert(c5.contains(RasterKey(rerun.map, massStyle)))
  }

  test("the cache stays within its budget under ten times the fixture's maps") {
    val study  = MockStudy.load.fold(e => fail(e), identity)
    val trials = study.inventory.map(_.trial)
    val scales = 4
    assert(trials.size >= 400, trials.size)
    val budget = RasterBudget.Default
    val each   = raster("ret_01").bytes
    val keep   = (budget.bytes / each).toInt
    // Ten times the fixture: every trial at every scale, ten occurrences each.
    val maps = for
      copy  <- (1 to 10).iterator
      t     <- trials.iterator
      scale <- (0 until scales).iterator
    yield MapId(
      eyes4s.studio.core.backend.RunId(7),
      t.copy(occurrence = copy),
      ScaleIndex.of(scale).toOption.get
    )
    var cache   = RasterCache.empty(budget)
    var count   = 0
    var evicted = 0
    var last    = Option.empty[RasterKey]
    maps.foreach { m =>
      val (next, gone) = cache.put(MapRaster.render(grid(m, bump), massStyle))
      cache = next
      count += 1
      evicted += gone.size
      last = Some(RasterKey(m, massStyle))
      assert(cache.bytes <= budget.bytes, s"${cache.bytes} bytes after $count rasters")
    }
    assertEquals(count, trials.size * scales * 10)
    assert(count > keep, s"$count rasters fit in $keep")
    assertEquals(cache.size, keep)
    assertEquals(evicted, count - keep)
    assert(last.exists(cache.contains))
  }
