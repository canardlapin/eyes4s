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
import javafx.scene.canvas.Canvas
import javafx.scene.layout.{Pane, StackPane}

/** The harness's render-failure capture: what JavaFX only prints on its own
  * threads reaches the test. Each test provokes a failure, so each is tagged
  * [[StudioFxSuite.RenderFailureExpected]]; without the tag each would fail.
  */
class RenderFailureCaptureFxSuite extends StudioFxSuite:

  private val expected = StudioFxSuite.RenderFailureExpected

  private def frames(e: Throwable): List[String] =
    e.getStackTrace.toList.map(f => s"${f.getClassName}.${f.getMethodName}")

  // Canvases whose textures together exceed Prism's texture pool. Each canvas
  // keeps a permanent texture of its full device size; five of 6000 x 6000
  // logical pixels need 720 MB at 1x and 2.9 GB at 2x, over the default
  // 512 MiB pool of both the ES2 and the software pipeline, while every edge
  // stays within the ES2 maximum texture size (16384) at 2x. When the pool
  // cannot make room, `createRTTexture` returns null and NGCanvas dereferences
  // it: the NullPointerException of the 3228aed gate.
  private val PoolBusters = 5
  private val BusterEdge  = 6000.0

  fxStage.test(
    "canvases that exhaust Prism's texture pool are captured from the render thread"
      .tag(expected)
  ) { fx =>
    // On the ES2 pipeline a render job that fails part-way can leave the GL
    // context unusable; one local run then crashed the JVM in native code. CI
    // renders with the software pipeline, where the failure is contained.
    assume(
      sys.props.get("prism.order").exists(_.split(',').headOption.contains("sw")),
      "provokes a texture-pool failure only on the software pipeline (-Dprism.order=sw)"
    )
    val canvases = fx.runOnFx(Vector.fill(PoolBusters) {
      val canvas = Canvas(BusterEdge, BusterEdge)
      canvas.getGraphicsContext2D.fillRect(0.0, 0.0, 10.0, 10.0)
      canvas
    })
    fx.show(fx.runOnFx(Pane(canvases*)))
    val failures = takeRenderFailures()
    // Zero-size canvases release their textures at the next render.
    fx.runOnFx(canvases.foreach { c => c.setWidth(0.0); c.setHeight(0.0) })
    fx.awaitLayout()
    takeRenderFailures(): Unit

    assert(failures.nonEmpty, "the render failure was not captured")
    val first = failures.head
    assertEquals(first.route, RenderFailureRoute.PrintedByToolkit)
    assert(first.thread.startsWith("QuantumRenderer"), first.thread)
    assert(first.error.isInstanceOf[NullPointerException], first.describe)
    assert(
      frames(first.error).contains("com.sun.javafx.sg.prism.NGCanvas$RenderBuf.validate"),
      first.describe
    )
    // The verdict an untagged test gets carries the stack trace.
    val verdict = StudioFxSuite.renderFailed(munitTests().head, failures, "during this test")
    assert(verdict.getMessage.contains("NGCanvas$RenderBuf.validate"), verdict.getMessage)
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
