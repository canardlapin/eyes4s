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

package eyes4s.studio.desktop.platform

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import eyes4s.studio.app.StoryModels
import eyes4s.studio.app.figures.ExportFormat
import eyes4s.studio.app.report.{ErrorDialog, LogState}
import eyes4s.studio.core.platform.*
import eyes4s.studio.core.report.{ErrorBundle, FailureOrigin, FailureTrace}
import eyes4s.studio.desktop.explore.NavigatorDisplays
import eyes4s.studio.desktop.figures.FigureInputs
import eyes4s.studio.desktop.report.{ErrorDialogView, ErrorReporter}
import eyes4s.studio.desktop.shell.ShellFxSuite
import javafx.scene.control.{Button, Label}
import java.util.concurrent.{CompletableFuture, TimeUnit}
import scala.jdk.CollectionConverters.*

class PlatformRoutesFxSuite extends ShellFxSuite:
  fxStage.test(
    "figure save uses the injected chooser and filesystem; cancellation writes nothing"
  ) { fx =>
    val w      = boot(fx, StoryModels.t2Figures)
    val memory = InMemoryPlatform.create[IO]().unsafeRunSync()
    val target = HostPath.of("/figure.svg").toOption.get
    val inputs = FigureInputs.of(
      w.session,
      NavigatorDisplays.golden,
      () => Some(fx.stage),
      platform = Some(memory.platform)
    )
    val bytes = IArray[Byte](1, 2, 3)
    memory.answer(Some(target)).unsafeRunSync()
    val saved = CompletableFuture[Either[String, String]]()
    runOnFx(
      inputs.save(
        "figure.svg",
        ExportFormat.Svg,
        bytes,
        answer => { saved.complete(answer); () }
      )
    )
    assertEquals(saved.get(10, TimeUnit.SECONDS), Right(target.value))
    assertEquals(
      memory.platform.files.read(target).unsafeRunSync().map(b => b.toVector),
      Right(bytes.toVector)
    )
    memory.answer(None).unsafeRunSync()
    val canceled = CompletableFuture[Either[String, String]]()
    runOnFx(
      inputs.save(
        "other.svg",
        ExportFormat.Svg,
        bytes,
        answer => { canceled.complete(answer); () }
      )
    )
    assert(canceled.get(10, TimeUnit.SECONDS).isLeft)
    val requests = memory.requests.unsafeRunSync()
    assertEquals(requests.size, 2)
    assert(requests.forall {
      case DialogRequest.Save(request) => request.suggestedName.isDefined; case _ => false
    })
  }

  fxStage.test("report copy confirms actual host success and exposes a host refusal") { fx =>
    val bundle = ErrorBundle(
      "2026-10-21T12:00:00Z",
      ErrorReporter.facts,
      FailureOrigin.UiThread("test"),
      FailureTrace.of(IllegalStateException("private content")),
      None
    )
    val vm     = ErrorDialog.vm(bundle, LogState.Unavailable("test"))
    val memory = InMemoryPlatform.create[IO]().unsafeRunSync()
    val alert  = runOnFx(
      ErrorDialogView.show(vm, () => (), Some(fx.stage), Some(memory.platform.clipboard))
    )
    try
      val copy = runOnFx(alert.getDialogPane.lookup("#error-report-copy").asInstanceOf[Button])
      runOnFx(copy.fire())
      eventually(fx, "successful copy label")(copy.getText == vm.copied)
      assertEquals(memory.platform.clipboard.readText.unsafeRunSync(), Right(Some(vm.report)))
    finally runOnFx(alert.close())
    val refusal = new Clipboard[IO]:
      def readText                = IO.pure(Right(None))
      def writeText(text: String) =
        IO.pure(Left(PlatformError.Unsupported("clipboard", "copy report")))
    val refused = runOnFx(ErrorDialogView.show(vm, () => (), Some(fx.stage), Some(refusal)))
    try
      runOnFx(refused.getDialogPane.lookup("#error-report-copy").asInstanceOf[Button].fire())
      eventually(fx, "visible clipboard refusal") {
        refused.getDialogPane
          .lookupAll(".label")
          .asScala
          .collect { case l: Label => l.getText }
          .exists(_.contains("copy report"))
      }
      assertEquals(
        runOnFx(
          refused.getDialogPane.lookup("#error-report-copy").asInstanceOf[Button].getText
        ),
        vm.copy
      )
    finally runOnFx(refused.close())
  }

  fxStage.test("scanpath import uses the injected chooser request and accepts cancellation") {
    fx =>
      val memory = InMemoryPlatform.create[IO]().unsafeRunSync()
      memory.answer(None).unsafeRunSync()
      val stage = runOnFx(
        eyes4s.studio.desktop.scanpath.ScanpathWorkbench.open(fx.stage, Some(memory.platform))
      )
      try
        val open = runOnFx(
          stage.getScene.getRoot
            .lookupAll(".button")
            .asScala
            .collectFirst {
              case button: Button if button.getText.startsWith("Open scanpath") => button
            }
            .getOrElse(fail("missing scanpath import action"))
        )
        runOnFx(open.fire())
        eventually(fx, "host scanpath chooser request")(
          memory.requests.unsafeRunSync().size == 1
        )
        assert(memory.requests.unsafeRunSync().head match
          case DialogRequest.Open(request) =>
            request.kinds.exists(_.extensions == Vector("json"))
          case _ => false)
        assert(runOnFx(stage.isShowing))
      finally runOnFx(stage.close())
  }

  fxStage.test(
    "a pending scanpath chooser serializes requests; cancel and disposal reject late answers"
  ) { fx =>
    var answers = Vector.empty[Option[java.nio.file.Path] => Unit]
    val view    = runOnFx(
      eyes4s.studio.desktop.scanpath.ScanpathWorkbench(
        () => None,
        () => None,
        Some((_, answer) => answers :+= answer)
      )
    )
    fx.show(view.node)
    try
      runOnFx {
        view.open.fire()
        assert(view.busy)
        assert(view.open.isDisabled)
        view.open.fire()
        assertEquals(answers.size, 1)
        answers.head(None)
        assert(!view.busy)
        assert(!view.open.isDisabled)
        view.open.fire()
        assertEquals(answers.size, 2)
        view.dispose()
        answers.last(Some(java.nio.file.Paths.get("/no-late-file-read.json")))
        assert(!view.busy)
        assertEquals(view.result, None)
      }
    finally runOnFx(view.dispose())
  }
