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

package eyes4s.studio.desktop.report

import ch.qos.logback.classic.LoggerContext
import eyes4s.studio.app.{AppModel, StoryModels}
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import eyes4s.studio.app.AppEffect
import eyes4s.studio.core.backend.{BackendError, DatasetRevision, JobId, Phase, TrialKey}
import eyes4s.studio.core.execution.{ExecutionEffect, ExecutionError}
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.core.report.{ErrorBundle, FailureOrigin}
import eyes4s.studio.desktop.runtime.{DesktopEffects, StudioSession}
import eyes4s.studio.desktop.harness.StudioFxSuite
import javafx.application.Platform
import javafx.scene.control.{Alert, Button, TextArea}
import javafx.scene.input.Clipboard
import org.slf4j.LoggerFactory

import java.nio.file.{Files, Path}
import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.*

/** Logging and error reporting (ticket S1.12): an injected failure on the
  * JavaFX thread, or a job's defect, opens the report dialog and writes a
  * log entry, and neither the dialog's report nor the log holds the data
  * values the failure's message carried.
  */
class ErrorReportingSuite extends StudioFxSuite:

  /** Data values an exception message might carry: a participant, a trial,
    * a coordinate and a file path.
    */
  private val secrets =
    Vector("P17", "ret_07", "1148.5", "/Users/someone/memory-study", "fixations.csv")
  private val message =
    "P17 ret_07 at (1148.5, 456.5) in /Users/someone/memory-study/fixations.csv"

  private val digest = ErrorReporter.digestOf(AppModel.open(StoryModels.t2, None))

  private final case class Fixture(dir: Path, log: StudioLog, reporter: ErrorReporter):
    val shown           = ConcurrentLinkedQueue[Alert]()
    def logText: String = Files.readString(log.file)

  private val reporting = FunFixture[Fixture](
    setup = _ =>
      val dir                   = Files.createTempDirectory("eyes-studio-logs")
      val log                   = StudioLog.open(dir).fold(e => fail(e.message), identity)
      lazy val fixture: Fixture = Fixture(
        dir,
        log,
        ErrorReporter(
          ErrorReporter.facts,
          () => digest,
          Some(log.logger("eyes4s.studio")),
          log.state,
          (vm, closed) => fixture.shown.add(ErrorDialogView.show(vm, closed)): Unit
        )
      )
      fixture
    ,
    teardown = f =>
      runOnFx(f.shown.asScala.foreach(_.close()))
      LoggerFactory.getILoggerFactory match
        case c: LoggerContext => c.reset()
        case _                => ()
  )

  private def await(what: String)(ok: => Boolean): Unit =
    val deadline = System.nanoTime + 10_000_000_000L
    while !ok && System.nanoTime < deadline do Thread.sleep(20)
    assert(ok, s"timed out waiting for $what")

  private def reportOf(alert: Alert): String =
    runOnFx(alert.getDialogPane.lookup("#error-report-text").asInstanceOf[TextArea].getText)

  private def assertClean(text: String, where: String): Unit =
    secrets.foreach(s => assert(!text.contains(s), s"$where holds $s:\n$text"))

  reporting.test("an exception on the JavaFX thread opens the report dialog and logs it") { f =>
    val installed = f.reporter.install()
    try
      Platform.runLater(() => throw IllegalStateException(message))
      await("the dialog")(f.shown.size == 1)
      val alert = f.shown.peek
      assert(runOnFx(alert.isShowing))
      assertEquals(runOnFx(alert.getDialogPane.getId), ErrorDialogView.PaneId)
      assertEquals(runOnFx(alert.getHeaderText), "Something went wrong in Eyes Studio")
      val report = reportOf(alert)
      assert(report.startsWith(ErrorBundle.Heading), report)
      assert(report.contains("Where: UI thread (JavaFX Application Thread)"), report)
      assert(report.contains("java.lang.IllegalStateException"), report)
      assert(report.contains(s"Project: ${digest.getOrElse("none")}"), report)
      assert(digest.exists(_.startsWith("sha256:")), digest)
      assertClean(report, "the report")
      await("the log entry")(f.logText.contains(ErrorBundle.Heading))
      val logged = f.logText
      assert(logged.contains(" ERROR [JavaFX Application Thread] eyes4s.studio - "), logged)
      assert(logged.contains(report), logged)
      assertClean(logged, "the log")
      assert(f.log.file.getFileName.toString == StudioLog.FileName)
    finally installed.restore()
  }

  reporting.test("a job's defect opens the dialog once; later failures are logged only") { f =>
    f.reporter.jobFailed("Submit", RuntimeException(message))
    await("the dialog")(f.shown.size == 1)
    val report = reportOf(f.shown.peek)
    assert(report.contains("Where: job (Submit)"), report)
    assertClean(report, "the report")
    // While the dialog is open, another failure is logged, not shown again.
    f.reporter.report(FailureOrigin.Background("io-1"), RuntimeException(message)): Unit
    await("two log entries")(f.logText.split(ErrorBundle.Heading, -1).length == 3)
    runOnFx(()): Unit
    assertEquals(f.shown.size, 1)
    assertClean(f.logText, "the log")
    // Closing it lets the next failure open a dialog.
    runOnFx(f.shown.peek.close())
    f.reporter.jobFailed("Cancel", RuntimeException(message))
    await("a second dialog")(f.shown.size == 2)
  }

  reporting.test("Copy report puts exactly the report on the clipboard") { f =>
    f.reporter.jobFailed("Submit", RuntimeException(message))
    await("the dialog")(f.shown.size == 1)
    val alert  = f.shown.peek
    val copied = runOnFx {
      val copy = alert.getDialogPane.lookup("#error-report-copy").asInstanceOf[Button]
      copy.fire()
      (Clipboard.getSystemClipboard.getString, copy.getText)
    }
    assertEquals(copied, (reportOf(alert), "Copied"))
  }

  test("the log directory is the platform's, shown with ~ for the home directory") {
    val home = Path.of(sys.props("user.home"))
    assertEquals(
      StudioLog.shown(home.resolve("Library/Logs/Eyes Studio/eyes-studio.log")),
      "~/Library/Logs/Eyes Studio/eyes-studio.log"
    )
    if sys.props("os.name").toLowerCase.contains("mac") then
      assertEquals(StudioLog.defaultDirectory, home.resolve("Library/Logs/Eyes Studio"))
  }

  reporting.test("a defect and a refusal through DesktopEffects log their kinds, no values") {
    f =>
      val session = StudioSession.start(StoryMoment.T2, _ => ())
      try
        // The first Cancel fails with a defect, the second is refused naming a trial.
        val calls   = java.util.concurrent.atomic.AtomicInteger(0)
        val refusal = ExecutionError.Backend(
          BackendError.UnknownTrial(
            DatasetRevision(3),
            TrialKey("P17", Phase.Retrieval, "ret_07", 1)
          )
        )
        val effects = DesktopEffects(
          session,
          (_, _) => (),
          _ => (),
          _ => (),
          ui = g => Platform.runLater(() => g()),
          defect = f.reporter.jobFailed,
          execute = Some(_ =>
            if calls.getAndIncrement() == 0 then IO.raiseError(IllegalStateException(message))
            else IO.pure(Left(refusal))
          )
        )
        val cancel = AppEffect.Execution(ExecutionEffect.Cancel(JobId(1)))
        effects.perform(cancel, _ => ())
        await("the dialog")(f.shown.size == 1)
        effects.perform(cancel, _ => ())
        await("the refusal")(f.logText.contains("refused Cancel"))
        val logged = f.logText
        assert(
          logged.contains(
            "The execution service failed on Cancel: java.lang.IllegalStateException"
          ),
          logged
        )
        assert(
          logged.contains(s"The execution service refused Cancel: ${refusal.code}"),
          logged
        )
        assert(reportOf(f.shown.peek).contains("Where: job (Cancel)"))
        assertClean(logged, "the log")
        assertClean(runOnFx(effects.problems.map(_.logLine).mkString("\n")), "the problems")
      finally session.close()
  }

  reporting.test("the handlers are restored from the JavaFX thread as from any other") { f =>
    val before    = runOnFx(Thread.currentThread.getUncaughtExceptionHandler)
    val installed = f.reporter.install()
    assert(runOnFx(Thread.currentThread.getUncaughtExceptionHandler) eq f.reporter.handler)
    // runOnFx fails on a timeout if restore() waits for the thread it runs on.
    runOnFx(installed.restore())
    assert(runOnFx(Thread.currentThread.getUncaughtExceptionHandler) eq before)
  }
