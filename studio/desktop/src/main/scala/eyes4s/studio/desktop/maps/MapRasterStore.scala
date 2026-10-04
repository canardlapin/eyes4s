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

package eyes4s.studio.desktop.maps

import eyes4s.studio.app.maps.{
  ColourLimits,
  MapGrid,
  MapPalette,
  MapRaster,
  RasterBudget,
  RasterCache,
  RasterKey
}
import javafx.application.Platform

import java.util.concurrent.{Executor, ExecutorService, Executors}

/** The desktop's map rasters (ticket S4.4): a [[RasterCache]] within a byte
  * budget, filled by rendering off the FX thread. A request for a cached
  * raster is answered from the cache; otherwise the raster is rendered on
  * `renderer` once, however many views ask for it meanwhile, cached, and
  * handed to every asker through `deliver` (the FX thread by default). The
  * colouring is the pure [[MapRaster.render]]; the store only schedules it.
  * Thread-safe.
  */
final class MapRasterStore private (
    budget: RasterBudget,
    renderer: Executor,
    deliver: Runnable => Unit,
    owned: Option[ExecutorService]
):

  private var cache: RasterCache                                 = RasterCache.empty(budget)
  private var pending: Map[RasterKey, Vector[MapRaster => Unit]] = Map.empty
  private var renders: Long                                      = 0L

  /** Asks for the raster of `grid` under `palette` and `limits`; `onReady`
    * receives it through `deliver`.
    */
  def request(grid: MapGrid, palette: MapPalette, limits: ColourLimits)(
      onReady: MapRaster => Unit
  ): Unit =
    val key   = RasterKey(grid.map, palette, limits)
    val ready = synchronized {
      val (hit, next) = cache.get(key)
      cache = next
      hit match
        case Some(raster) => Some(raster)
        case None         =>
          val waiting = pending.get(key)
          pending = pending.updated(key, waiting.getOrElse(Vector.empty) :+ onReady)
          if waiting.isEmpty then
            renders += 1
            renderer.execute(() => rendered(MapRaster.render(grid, palette, limits)))
          None
    }
    ready.foreach(r => deliver(() => onReady(r)))

  /** The cache as it stands. */
  def snapshot: RasterCache = synchronized(cache)

  /** How many rasters the store has rendered. */
  def renderCount: Long = synchronized(renders)

  /** Stops the store's own render thread, if it made one. */
  def close(): Unit = owned.foreach(_.shutdown())

  private def rendered(raster: MapRaster): Unit =
    val waiting = synchronized {
      cache = cache.put(raster)._1
      val w = pending.getOrElse(raster.key, Vector.empty)
      pending = pending.removed(raster.key)
      w
    }
    waiting.foreach(f => deliver(() => f(raster)))

object MapRasterStore:

  /** A store within `budget` rendering on its own daemon thread and
    * delivering on the FX thread.
    */
  def apply(budget: RasterBudget = RasterBudget.Default): MapRasterStore =
    val pool = Executors.newSingleThreadExecutor { r =>
      val t = Thread(r, "eyes-studio-map-raster")
      t.setDaemon(true)
      t
    }
    new MapRasterStore(budget, pool, r => Platform.runLater(r), Some(pool))

  /** A store rendering on `renderer` and delivering through `deliver`. */
  def on(budget: RasterBudget, renderer: Executor, deliver: Runnable => Unit): MapRasterStore =
    new MapRasterStore(budget, renderer, deliver, None)
