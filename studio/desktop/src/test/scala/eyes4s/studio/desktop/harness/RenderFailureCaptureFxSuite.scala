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

  // Canvases whose textures exceed Prism's texture pool. NGCanvas keeps a
  // permanent texture of the canvas's size times ceil(output scale); each
  // canvas here is BusterDevicePx device pixels square, 576 MB on its own,
  // over the default 512 MiB pool of both pipelines at any output scale, so
  // the pool cannot make room by evicting another canvas. When it cannot,
  // `createRTTexture` returns null and NGCanvas dereferences it: the
  // NullPointerException of the 3228aed gate.
  private val PoolBusters    = 2
  private val BusterDevicePx = 12000.0

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
    val edge     = BusterDevicePx / fx.runOnFx(math.ceil(fx.stage.getOutputScaleX))
    val canvases = fx.runOnFx(Vector.fill(PoolBusters) {
      val canvas = Canvas(edge, edge)
      canvas.getGraphicsContext2D.fillRect(0.0, 0.0, 10.0, 10.0)
      canvas
    })
    fx.show(fx.runOnFx(Pane(canvases*)))
    // show returns after layout; the render thread may not have drawn yet.
    StudioFxSuite.settle()
    val failures = takeRenderFailures()
    // Zero-size canvases release their textures at the next render.
    fx.runOnFx(canvases.foreach { c => c.setWidth(0.0); c.setHeight(0.0) })
    fx.awaitLayout()
    takeRenderFailures(): Unit

    // Whether Prism refuses the texture depends on the environment: hosted CI
    // (Linux xvfb and macOS, software pipeline, scale 1) printed no failure at
    // all, while every local run (scale 1 and 2) did. Where nothing failed the
    // precondition is unmet, not the capture; the other tests here provoke
    // failures deterministically.
    assume(
      failures.nonEmpty,
      "Prism did not exhaust its texture pool in this environment; nothing to capture"
    )
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
