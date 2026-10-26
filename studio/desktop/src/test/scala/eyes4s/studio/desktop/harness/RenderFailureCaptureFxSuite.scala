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

package eyes4s.studio.desktop.harness

import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.desktop.runtime.{ListenerFailureException, StudioRuntime}
import javafx.application.Platform
import javafx.scene.layout.StackPane
import com.sun.javafx.tk.{RenderJob, Toolkit}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.util.concurrent.atomic.AtomicReference

/** The harness's render-failure capture: what JavaFX only prints on its own
  * threads reaches the test. Each test provokes a failure, so each is tagged
  * [[StudioFxSuite.RenderFailureExpected]]; without the tag each would fail.
  */
class RenderFailureCaptureFxSuite extends StudioFxSuite:

  private val expected = StudioFxSuite.RenderFailureExpected

  private def frames(e: Throwable): List[String] =
    e.getStackTrace.toList.map(f => s"${f.getClassName}.${f.getMethodName}")

  fxStage.test("a failing Quantum render job is captured deterministically".tag(expected)) {
    _ =>
      val thrown    = AtomicReference[Throwable]()
      val completed = CountDownLatch(1)
      runOnFx {
        val toolkit = Toolkit.getToolkit
        toolkit.addRenderJob(new RenderJob(() => {
          val boom = IllegalStateException("deterministic render job failure")
          thrown.set(boom)
          throw boom
        }))
        // Same renderer queue: this fence follows the failing job's catch/print route.
        toolkit.addRenderJob(new RenderJob(() => completed.countDown()))
      }
      assert(completed.await(30, TimeUnit.SECONDS), "renderer did not reach the fence")
      val failures = takeRenderFailures()
      assertEquals(failures.size, 1, failures.map(_.describe).mkString("\n"))
      val first = failures.head
      assert(first.error eq thrown.get(), first.describe)
      assertEquals(first.route, RenderFailureRoute.PrintedByToolkit)
      assert(first.thread.startsWith("QuantumRenderer"), first.describe)
      assert(frames(first.error).contains("com.sun.javafx.tk.RenderJob.run"), first.describe)
      val verdict = StudioFxSuite.renderFailed(munitTests().head, failures, "during this test")
      assert(verdict.getCause eq first.error)
  }

  fxStage.test("a failure during a pulse's layout is captured".tag(expected)) { fx =>
    val boom = IllegalStateException("layout failed")
    fx.runOnFx {
      val failing = new StackPane:
        override def layoutChildren(): Unit = throw boom
      fx.scene.setRoot(failing)
      failing.requestLayout()
      Platform.requestNextPulse()
    }
    val failures = takeRenderFailures()
    assert(failures.exists(_.error eq boom), failures.map(_.describe).mkString("\n"))
    assert(failures.forall(_.route == RenderFailureRoute.Uncaught))
  }

  test("a view that fails to render in the desktop runtime is captured".tag(expected)) {
    val boom = IllegalStateException("render failed")
    runOnFx {
      val runtime = StudioRuntime(
        AppModel.newProject.fold(e => fail(e.toString), identity),
        (_, _) => ()
      )
      var armed = false
      runtime.listen(_ => if armed then throw boom)
      armed = true
      runtime.dispatch(Intent.ShowProjectInfo)
    }
    takeRenderFailures().map(_.error).toList match
      case List(e: ListenerFailureException) =>
        assertEquals(e.failure.intent, Intent.ShowProjectInfo)
        assert(e.getCause eq boom)
      case other => fail(s"unexpected $other")
  }

  test("a test thread's own printed stack trace is not a render failure") {
    System.err.println(
      IllegalStateException("printed by the test thread, not a render failure")
    )
    assertEquals(takeRenderFailures(), Vector.empty)
  }
