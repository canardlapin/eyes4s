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
import eyes4s.studio.core.backend.{Phase, TrialKey}
import eyes4s.studio.core.selection.ScaleIndex

import java.util.concurrent.{CountDownLatch, Executor, Executors, TimeUnit}
import scala.collection.mutable

/** The desktop's map raster store (ticket S4.4): rasters are rendered off
  * the asking thread, once however many views ask, cached within the byte
  * budget and answered from the cache thereafter. No JavaFX: deliveries go
  * through a supplied function.
  */
class MapRasterStoreSuite extends munit.FunSuite:

  private val Columns = 64
  private val Rows    = 48
  private val mass    = ColourLimits.Sequential(0.0, 1.0)

  private def grid(trial: String): MapGrid =
    val map = MapId(TrialKey("P17", Phase.Retrieval, trial, 1), ScaleIndex.of(2).toOption.get)
    MapGrid
      .of(
        map,
        Columns,
        Rows,
        Vector.tabulate(Columns * Rows)(i => Some(i.toDouble / 3072.0)),
        Vector.empty
      )
      .fold(e => fail(e.message), identity)

  /** An executor that holds its tasks until [[runAll]]. */
  private final class Held extends Executor:
    val tasks                      = mutable.Queue.empty[Runnable]
    def execute(r: Runnable): Unit = synchronized { tasks.enqueue(r); () }
    def runAll(): Unit             =
      while synchronized(tasks.nonEmpty) do synchronized(tasks.dequeue()).run()

  private val each = 64L * 48 * 4

  test("a raster is rendered off the asking thread and delivered as rendered") {
    val pool             = Executors.newSingleThreadExecutor(r => Thread(r, "test-map-raster"))
    var thread           = ""
    val tagged: Executor =
      r => pool.execute { () => thread = Thread.currentThread.getName; r.run() }
    val store = MapRasterStore.on(RasterBudget.Default, tagged, _.run())
    val done  = CountDownLatch(1)
    var got   = Option.empty[MapRaster]
    val g     = grid("ret_01")
    store.request(g, MapPalette.Mass, mass) { r => got = Some(r); done.countDown() }
    assert(done.await(10, TimeUnit.SECONDS), "never delivered")
    pool.shutdown()
    assertEquals(thread, "test-map-raster")
    assertNotEquals(thread, Thread.currentThread.getName)
    assertEquals(
      got.map(_.argb.toVector),
      Some(MapRaster.render(g, MapPalette.Mass, mass).argb.toVector)
    )
    assert(store.snapshot.contains(RasterKey(g.map, MapPalette.Mass, mass)))
  }

  test(
    "asking twice before it is rendered renders once; asking again is answered from the cache"
  ) {
    val held  = Held()
    val store = MapRasterStore.on(RasterBudget.Default, held, _.run())
    val got   = mutable.Buffer.empty[MapRaster]
    val g     = grid("ret_01")
    store.request(g, MapPalette.Mass, mass)(got += _)
    store.request(g, MapPalette.Mass, mass)(got += _)
    assertEquals((got.size, store.renderCount, held.tasks.size), (0, 1L, 1))
    held.runAll()
    assertEquals(got.size, 2)
    assert(got(0) eq got(1))
    // Cached: delivered at once, nothing rendered.
    store.request(g, MapPalette.Mass, mass)(got += _)
    assertEquals((got.size, store.renderCount, held.tasks.size), (3, 1L, 0))
    assert(got(2) eq got(0))
    // Another palette is another raster.
    store.request(g, MapPalette.Difference, ColourLimits.Symmetric(1.0))(got += _)
    assertEquals(store.renderCount, 2L)
  }

  test("the store's cache stays within its byte budget") {
    val held   = Held()
    val budget = RasterBudget.of(2 * each).get
    val store  = MapRasterStore.on(budget, held, _.run())
    (1 to 5).foreach(k => store.request(grid(f"ret_$k%02d"), MapPalette.Mass, mass)(_ => ()))
    held.runAll()
    assertEquals(store.renderCount, 5L)
    assertEquals(store.snapshot.size, 2)
    assert(store.snapshot.bytes <= budget.bytes)
    assert(store.snapshot.contains(RasterKey(grid("ret_05").map, MapPalette.Mass, mass)))
  }
