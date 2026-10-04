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

import eyes4s.studio.app.maps.*
import eyes4s.studio.core.backend.{Phase, RunId, TrialKey}
import eyes4s.studio.core.selection.ScaleIndex

import java.util.concurrent.{CountDownLatch, Executor, Executors, TimeUnit}
import scala.collection.mutable

/** The desktop's map raster store (ticket S4.4): rasters are rendered off
  * the asking thread, once however many views ask, cached within the byte
  * budget and answered from the cache; failures and a closed store are
  * typed refusals that leave nothing pending; a closed store delivers
  * nothing more; a rerun's map or a grid with other values never takes
  * another's raster. No JavaFX: deliveries go through a supplied function.
  */
class MapRasterStoreSuite extends munit.FunSuite:

  private val Columns = 64
  private val Rows    = 48

  private def right[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private val style = right(
    ColourLimits.sequential(0.0, 1.0).flatMap(MapStyle.of(MapPalette.Mass, _))
  )

  private def grid(trial: String, run: Int = 7, scale: Double = 1.0): MapGrid =
    val map = MapId(
      RunId(run),
      TrialKey("P17", Phase.Retrieval, trial, 1),
      ScaleIndex.of(2).toOption.get
    )
    right(
      MapGrid.of(
        map,
        Columns,
        Rows,
        RowOrder.TopFirst,
        Vector.tabulate(Columns * Rows)(i => Some(scale * i / 3072.0)),
        Vector.empty
      )
    )

  /** An executor that holds its tasks until [[runAll]]. */
  private final class Held extends Executor:
    val tasks                      = mutable.Queue.empty[Runnable]
    def execute(r: Runnable): Unit = synchronized { tasks.enqueue(r); () }
    def runAll(): Unit             =
      while synchronized(tasks.nonEmpty) do synchronized(tasks.dequeue()).run()

  private type Got = mutable.Buffer[Either[RasterRefusal, MapRaster]]
  private def answers(): Got = mutable.Buffer.empty

  private val each = 64L * 48 * 4

  test("a raster is rendered off the asking thread and delivered as rendered") {
    val pool             = Executors.newSingleThreadExecutor(r => Thread(r, "test-map-raster"))
    var thread           = ""
    val tagged: Executor = r =>
      pool.execute { () =>
        thread = Thread.currentThread.getName; r.run()
      }
    val store = MapRasterStore.on(RasterBudget.Default, tagged, _.run())
    val done  = CountDownLatch(1)
    var got   = Option.empty[Either[RasterRefusal, MapRaster]]
    val g     = grid("ret_01")
    assertEquals(store.request(g, style) { r => got = Some(r); done.countDown() }, Right(()))
    assert(done.await(10, TimeUnit.SECONDS), "never delivered")
    pool.shutdown()
    assertEquals(thread, "test-map-raster")
    assertNotEquals(thread, Thread.currentThread.getName)
    assertEquals(
      got.flatMap(_.toOption).map(_.argb.toVector),
      Some(MapRaster.render(g, style).argb.toVector)
    )
    assert(store.snapshot.contains(RasterKey(g.map, style)))
  }

  test(
    "asking twice before it is rendered renders once; asking again is answered from the cache"
  ) {
    val held  = Held()
    val store = MapRasterStore.on(RasterBudget.Default, held, _.run())
    val got   = answers()
    val g     = grid("ret_01")
    store.request(g, style)(got += _)
    store.request(g, style)(got += _)
    assertEquals((got.size, store.renderCount, held.tasks.size), (0, 1L, 1))
    held.runAll()
    assertEquals(got.size, 2)
    assert(right(got(0)) eq right(got(1)))
    store.request(g, style)(got += _)
    assertEquals((got.size, store.renderCount, held.tasks.size), (3, 1L, 0))
    assert(right(got(2)) eq right(got(0)))
  }

  test(
    "a rerun's map is another raster; a grid with other values for a pending key is refused"
  ) {
    val held  = Held()
    val store = MapRasterStore.on(RasterBudget.Default, held, _.run())
    val got   = answers()
    val run7  = grid("ret_01")
    val run8  = grid("ret_01", run = 8, scale = 0.5)
    store.request(run7, style)(got += _)
    // Another grid under the same identity while the first renders.
    val other = grid("ret_01", scale = 0.5)
    store.request(other, style)(got += _)
    assertEquals(
      got.toVector,
      Vector(Left(RasterRefusal.ContentMismatch(RasterKey(run7.map, style))))
    )
    held.runAll()
    assertEquals(right(got(1)).argb.toVector, MapRaster.render(run7, style).argb.toVector)
    // The rerun renders its own raster rather than reusing run 7's.
    store.request(run8, style)(got += _)
    held.runAll()
    assertEquals(store.renderCount, 2L)
    assertEquals(right(got(2)).argb.toVector, MapRaster.render(run8, style).argb.toVector)
    // Invalidated, run 7's map renders again.
    store.invalidate(run7.map)
    store.request(run7, style)(got += _)
    assertEquals(store.renderCount, 3L)
  }

  test("a failed render is refused to every asker and leaves nothing pending") {
    val held   = Held()
    var broken = true
    val store  = MapRasterStore.on(
      RasterBudget.Default,
      held,
      _.run(),
      (g, s) => if broken then throw IllegalStateException("boom") else MapRaster.render(g, s)
    )
    val got = answers()
    val g   = grid("ret_01")
    store.request(g, style)(got += _)
    store.request(g, style)(got += _)
    held.runAll()
    assertEquals(got.size, 2)
    got.foreach {
      case Left(RasterRefusal.RenderFailed(k, reason)) =>
        assertEquals(k, RasterKey(g.map, style))
        assert(reason.contains("boom"), reason)
      case other => fail(s"$other")
    }
    assert(!store.snapshot.contains(RasterKey(g.map, style)))
    // Nothing is left pending: a later request renders again, and succeeds.
    broken = false
    store.request(g, style)(got += _)
    held.runAll()
    assertEquals(store.renderCount, 2L)
    assert(got(2).isRight)
  }

  test("a closed store refuses requests at once and delivers nothing more") {
    val held  = Held()
    val queue = mutable.Queue.empty[Runnable]
    val store = MapRasterStore.on(RasterBudget.Default, held, r => queue.enqueue(r))
    val got   = answers()
    val g     = grid("ret_01")
    store.request(g, style)(got += _)
    store.close()
    assert(store.isClosed)
    // A queued render after close caches and delivers nothing.
    held.runAll()
    assertEquals(queue.size, 0)
    assert(!store.snapshot.contains(RasterKey(g.map, style)))
    // A request after close is refused at once, without a callback.
    assertEquals(
      store.request(g, style)(got += _),
      Left(RasterRefusal.Closed(RasterKey(g.map, style)))
    )
    assertEquals(got.size, 0)
  }

  test("a delivery already posted when the store closes does not reach its view") {
    val held  = Held()
    val queue = mutable.Queue.empty[Runnable]
    val store = MapRasterStore.on(RasterBudget.Default, held, r => queue.enqueue(r))
    val got   = answers()
    store.request(grid("ret_01"), style)(got += _)
    held.runAll()
    assertEquals(queue.size, 1)
    store.close()
    queue.dequeue().run()
    assertEquals(got.size, 0)
  }

  test("a renderer that rejects the work is a refusal, and leaves nothing pending") {
    val pool = Executors.newSingleThreadExecutor()
    pool.shutdown()
    val store = MapRasterStore.on(RasterBudget.Default, pool, _.run())
    val g     = grid("ret_01")
    assertEquals(
      store.request(g, style)(_ => ()),
      Left(RasterRefusal.Closed(RasterKey(g.map, style)))
    )
    // Not pending: the next request tries to render again.
    assertEquals(
      store.request(g, style)(_ => ()),
      Left(RasterRefusal.Closed(RasterKey(g.map, style)))
    )
    assertEquals(store.renderCount, 2L)
  }

  test("the store's cache stays within its byte budget") {
    val held   = Held()
    val budget = RasterBudget.of(2 * each).get
    val store  = MapRasterStore.on(budget, held, _.run())
    (1 to 5).foreach(k => store.request(grid(f"ret_$k%02d"), style)(_ => ()))
    held.runAll()
    assertEquals(store.renderCount, 5L)
    assertEquals(store.snapshot.size, 2)
    assert(store.snapshot.bytes <= budget.bytes)
    assert(store.snapshot.contains(RasterKey(grid("ret_05").map, style)))
  }
