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

package eyes4s.studio.core.real

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import java.util.concurrent.{CompletableFuture, CountDownLatch, Executors, TimeUnit}
import scala.concurrent.ExecutionContext

class RealReportGateJvmSuite extends munit.FunSuite:
  test("waiting report bindings leave a compute worker available for independent reads") {
    val executor = Executors.newFixedThreadPool(2, r =>
      val thread = Thread(r, "report-gate-compute")
      thread.setDaemon(true)
      thread
    )
    val runtime = IORuntime.builder().setCompute(
      ExecutionContext.fromExecutor(executor),
      () => { executor.shutdownNow(): Unit }
    ).build()
    val entered = CountDownLatch(1)
    val release = CountDownLatch(1)
    val queued = CountDownLatch(1)
    lazy val binding: Int =
      entered.countDown()
      if !release.await(10, TimeUnit.SECONDS) then throw IllegalStateException("binding release timed out")
      42
    def start[A](io: IO[A]): CompletableFuture[Either[Throwable, A]] =
      val result = CompletableFuture[Either[Throwable, A]]()
      io.unsafeRunAsync(value => { result.complete(value): Unit })(using runtime)
      result
    val gate = RealReportGate.create[IO].unsafeRunSync()(using runtime)
    try
      val first = start(gate.evaluate(binding))
      assert(entered.await(5, TimeUnit.SECONDS), "first report did not start")
      val second = start(IO(queued.countDown()) >> gate.evaluate(binding))
      assert(queued.await(5, TimeUnit.SECONDS), "second report was not queued")
      val independent = start(IO.pure("image bytes"))
      assertEquals(independent.get(2, TimeUnit.SECONDS), Right("image bytes"))
      assert(!first.isDone && !second.isDone, "reports must still await the binding")
      release.countDown()
      assertEquals(first.get(5, TimeUnit.SECONDS), Right(42))
      assertEquals(second.get(5, TimeUnit.SECONDS), Right(42))
    finally
      release.countDown()
      runtime.shutdown()
  }
