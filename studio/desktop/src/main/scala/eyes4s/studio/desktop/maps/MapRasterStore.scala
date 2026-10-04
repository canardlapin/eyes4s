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
  MapGrid,
  MapId,
  MapRaster,
  MapStyle,
  RasterBudget,
  RasterCache,
  RasterKey
}
import javafx.application.Platform

import java.util.concurrent.{Executor, ExecutorService, Executors, RejectedExecutionException}
import scala.util.control.NonFatal

/** Why a map raster could not be given. Every case names the raster. */
enum RasterRefusal derives CanEqual:

  /** The store is closed; nothing more is rendered or delivered. */
  case Closed(key: RasterKey)

  /** Rendering failed with `reason`; a later request renders again. */
  case RenderFailed(key: RasterKey, reason: String)

  /** A raster of `key` is being rendered from a grid with other values: one
    * map identity was served with two contents, so neither is assumed.
    */
  case ContentMismatch(key: RasterKey)

  def message: String = this match
    case Closed(k)          => s"Map ${k.map.label}: the raster store is closed."
    case RenderFailed(k, r) => s"Map ${k.map.label}: rendering its raster failed: $r"
    case ContentMismatch(k) =>
      s"Map ${k.map.label}: asked for with values other than those being rendered."

/** The desktop's map rasters (ticket S4.4): a [[RasterCache]] within a byte
  * budget, filled by rendering off the FX thread. A request for a cached
  * raster is answered from the cache; otherwise the raster is rendered on
  * `renderer` once, however many views ask for it meanwhile, cached, and
  * handed to every asker through `deliver` (the FX thread by default), as a
  * raster or a [[RasterRefusal]]. A failed render leaves nothing pending.
  *
  * Rasters are keyed by the map's result identity (run, trial, scale) and
  * style; a request whose grid differs from the one being rendered for the
  * same key is refused rather than given the other grid's raster.
  * [[invalidate]] drops a map's rasters. After [[close]] nothing is
  * rendered or delivered, and a request is refused at once. The colouring
  * is the pure [[MapRaster.render]]; the store only schedules it.
  * Thread-safe.
  */
final class MapRasterStore private (
    budget: RasterBudget,
    renderer: Executor,
    deliver: Runnable => Unit,
    owned: Option[ExecutorService],
    paint: (MapGrid, MapStyle) => MapRaster
):
  private type Answer = Either[RasterRefusal, MapRaster] => Unit

  private var cache: RasterCache = RasterCache.empty(budget)
  // Each key being rendered: the content of the grid it is rendered from
  // and everyone waiting for it.
  private var pending: Map[RasterKey, (Long, Vector[Answer])] = Map.empty
  private var renders: Long                                   = 0L
  private var closed: Boolean                                 = false

  /** Asks for the raster of `grid` in `style`; `onReady` receives it (or why
    * not) through `deliver`. Refused at once, with nothing delivered, when
    * the store is closed or cannot schedule the render.
    */
  def request(grid: MapGrid, style: MapStyle)(onReady: Answer): Either[RasterRefusal, Unit] =
    val key     = RasterKey(grid.map, style)
    val content = grid.contentHash
    val step: Either[RasterRefusal, Option[Either[RasterRefusal, MapRaster]]] = synchronized {
      if closed then Left(RasterRefusal.Closed(key))
      else
        val (hit, next) = cache.get(key)
        cache = next
        hit match
          case Some(raster) => Right(Some(Right(raster)))
          case None         =>
            pending.get(key) match
              case Some((c, _)) if c != content =>
                Right(Some(Left(RasterRefusal.ContentMismatch(key))))
              case Some((c, waiting)) =>
                pending = pending.updated(key, (c, waiting :+ onReady))
                Right(None)
              case None =>
                pending = pending.updated(key, (content, Vector(onReady)))
                renders += 1
                try
                  renderer.execute(() => render(key, grid, style))
                  Right(None)
                catch
                  case _: RejectedExecutionException =>
                    pending = pending.removed(key)
                    Left(RasterRefusal.Closed(key))
    }
    step.map(_.foreach(answer => send(onReady, answer)))

  /** Drops every raster of `map`, as when its run's result is dropped. */
  def invalidate(map: MapId): Unit = synchronized {
    cache = cache.invalidate(map)._1
  }

  /** The cache as it stands. */
  def snapshot: RasterCache = synchronized(cache)

  /** How many renders the store has started. */
  def renderCount: Long = synchronized(renders)

  /** Whether the store is closed. */
  def isClosed: Boolean = synchronized(closed)

  /** Closes the store: nothing more is rendered or delivered, pending
    * requests are dropped and the store's own render thread is stopped.
    * Idempotent.
    */
  def close(): Unit =
    synchronized {
      closed = true
      pending = Map.empty
    }
    owned.foreach(_.shutdownNow())

  private def render(key: RasterKey, grid: MapGrid, style: MapStyle): Unit =
    val result =
      try Right(paint(grid, style))
      catch case NonFatal(e) => Left(RasterRefusal.RenderFailed(key, e.toString))
    val waiting = synchronized {
      val w = pending.get(key).map(_._2).getOrElse(Vector.empty)
      pending = pending.removed(key)
      if closed then Vector.empty
      else
        result.foreach(r => cache = cache.put(r)._1)
        w
    }
    waiting.foreach(send(_, result))

  // Delivers unless the store has closed meanwhile.
  private def send(onReady: Answer, answer: Either[RasterRefusal, MapRaster]): Unit =
    deliver(() => if !isClosed then onReady(answer))

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
    new MapRasterStore(budget, pool, r => Platform.runLater(r), Some(pool), MapRaster.render)

  /** A store rendering on `renderer` with `paint` (the pure render, unless
    * a test stands in for it) and delivering through `deliver`.
    */
  def on(
      budget: RasterBudget,
      renderer: Executor,
      deliver: Runnable => Unit,
      paint: (MapGrid, MapStyle) => MapRaster = MapRaster.render
  ): MapRasterStore =
    new MapRasterStore(budget, renderer, deliver, None, paint)
