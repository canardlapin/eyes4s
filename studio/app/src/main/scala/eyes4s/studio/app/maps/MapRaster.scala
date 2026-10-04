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

import scala.collection.immutable.TreeMap

/** What a raster is of: a map, by its run's result identity, drawn in a
  * style (ticket S4.4). The map's opacity is not part of it: one global
  * opacity is applied when the raster is drawn ([[MapRaster.drawn]]).
  */
final case class RasterKey(map: MapId, style: MapStyle) derives CanEqual

/** A map's cells as ARGB pixels, one per cell, x fastest, the map's top
  * row first whatever its grid's [[RowOrder]].
  */
final case class MapRaster private (key: RasterKey, width: Int, height: Int, argb: IArray[Int]):

  /** The ARGB of the cell at column `x` and row `y`. */
  def pixel(x: Int, y: Int): Int = argb(y * width + x)

  /** The pixels as drawn at `opacity`: each stored pixel's alpha scaled
    * ([[MapOpacity.over]]). The raster itself is unchanged.
    */
  def drawn(opacity: MapOpacity): IArray[Int] = argb.map(opacity.over)

  /** What the raster holds in memory: four bytes a pixel. */
  def bytes: Long = argb.length.toLong * 4L

object MapRaster:

  /** The raster of `grid` in `style`: each cell's colour
    * ([[MapColours.argb]]). Nothing in the grid changes.
    */
  def render(grid: MapGrid, style: MapStyle): MapRaster =
    MapRaster(
      RasterKey(grid.map, style),
      grid.columns,
      grid.rows,
      IArray.tabulate(grid.columns * grid.rows) { i =>
        MapColours.argb(style, grid.atTop(i % grid.columns, i / grid.columns))
      }
    )

/** A byte budget for cached rasters, at least one byte. */
final case class RasterBudget private (bytes: Long) derives CanEqual

object RasterBudget:

  /** 64 MiB: about 5,000 rasters of the fixture's 64 × 48 grid. */
  val Default: RasterBudget = RasterBudget(64L * 1024 * 1024)

  def of(bytes: Long): Option[RasterBudget] = Option.when(bytes > 0)(RasterBudget(bytes))

/** A least-recently-used cache of rasters bounded by a byte budget (ticket
  * S4.4). Immutable: every operation returns the next cache. Adding a raster
  * evicts the least recently used ones until the cache is within budget; a
  * raster larger than the whole budget is not kept. Looking a raster up
  * makes it the most recently used.
  */
final case class RasterCache private (
    budget: RasterBudget,
    entries: Map[RasterKey, (MapRaster, Long)],
    order: TreeMap[Long, RasterKey],
    tick: Long,
    bytes: Long
):

  /** How many rasters the cache holds. */
  def size: Int = entries.size

  /** The cache without any raster of `map`, as when its run's result is
    * dropped, and the keys it removed.
    */
  def invalidate(map: MapId): (RasterCache, Vector[RasterKey]) =
    val gone = entries.keys.filter(_.map == map).toVector
    val next = gone.foldLeft(this) { (c, k) =>
      val (raster, used) = c.entries(k)
      c.copy(
        entries = c.entries.removed(k),
        order = c.order.removed(used),
        bytes = c.bytes - raster.bytes
      )
    }
    (next, gone)

  /** Whether the cache holds the raster of `key`, without touching it. */
  def contains(key: RasterKey): Boolean = entries.contains(key)

  /** The raster of `key`, if held, and the cache with it most recently used. */
  def get(key: RasterKey): (Option[MapRaster], RasterCache) =
    entries.get(key) match
      case Some((raster, used)) =>
        (
          Some(raster),
          copy(
            entries = entries.updated(key, (raster, tick)),
            order = order.removed(used).updated(tick, key),
            tick = tick + 1
          )
        )
      case None => (None, this)

  /** The cache holding `raster` as the most recently used, and the keys it
    * evicted to stay within budget, least recently used first.
    */
  def put(raster: MapRaster): (RasterCache, Vector[RasterKey]) =
    val key     = raster.key
    val without = entries.get(key) match
      case Some((old, used)) =>
        copy(
          entries = entries.removed(key),
          order = order.removed(used),
          bytes = bytes - old.bytes
        )
      case None => this
    if raster.bytes > budget.bytes then (without, Vector.empty)
    else
      val added = without.copy(
        entries = without.entries.updated(key, (raster, tick)),
        order = without.order.updated(tick, key),
        tick = tick + 1,
        bytes = without.bytes + raster.bytes
      )
      added.evict(Vector.empty)

  // Drops the least recently used rasters until the cache is within budget.
  @annotation.tailrec
  private def evict(evicted: Vector[RasterKey]): (RasterCache, Vector[RasterKey]) =
    if bytes <= budget.bytes then (this, evicted)
    else
      val (used, key) = order.head
      val raster      = entries(key)._1
      copy(
        entries = entries.removed(key),
        order = order.removed(used),
        bytes = bytes - raster.bytes
      ).evict(evicted :+ key)

object RasterCache:

  /** An empty cache within `budget`. */
  def empty(budget: RasterBudget): RasterCache =
    RasterCache(budget, Map.empty, TreeMap.empty, 0L, 0L)
