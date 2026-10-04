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

import eyes4s.studio.app.AppModel
import eyes4s.studio.app.report.{ErrorDialog, ErrorDialogVM, LogState}
import eyes4s.studio.core.backend.ProtocolVersion
import eyes4s.studio.core.document.StudioDocument
import eyes4s.studio.core.report.{BuildFacts, ErrorBundle, FailureOrigin, FailureTrace}
import eyes4s.studio.desktop.StudioBuild
import javafx.application.Platform
import org.slf4j.Logger

import java.time.Instant
import java.util.concurrent.atomic.AtomicBoolean
import scala.util.control.NonFatal

/** Reports unexpected failures (ticket S1.12): an uncaught exception on the
  * JavaFX thread or any other thread, and a job's defect. Each becomes an
  * [[ErrorBundle]], is logged at ERROR and opens the report dialog on the
  * JavaFX thread. While a dialog is open, further reports are logged only.
  * Reporting never throws: a failure while reporting goes to stderr.
  *
  * The log holds the bundle, never the exception's message, so the log and
  * the dialog carry the same data-free report.
  */
final class ErrorReporter(
    facts: BuildFacts,
    project: () => Option[String],
    logger: Option[Logger],
    log: LogState,
    show: (ErrorDialogVM, () => Unit) => Unit,
    clock: () => Instant = () => Instant.now()
):
  private val open = AtomicBoolean(false)

  /** Report `error` from `origin`; the bundle reported. */
  def report(origin: FailureOrigin, error: Throwable): ErrorBundle =
    val bundle = ErrorBundle(
      clock().toString,
      facts,
      origin,
      FailureTrace.of(error),
      safely(project()).flatten
    )
    safely(logger.fold(System.err.println(bundle.render))(_.error(bundle.render)))
    if open.compareAndSet(false, true) then
      val vm = ErrorDialog.vm(bundle, log)
      onFx { () =>
        safely(show(vm, () => open.set(false))).getOrElse(open.set(false))
      }
    bundle

  /** The handler for uncaught exceptions: the JavaFX thread's are UI
    * failures, every other thread's background failures.
    */
  val handler: Thread.UncaughtExceptionHandler = (thread, error) =>
    val origin =
      if Platform.isFxApplicationThread then FailureOrigin.UiThread(thread.getName)
      else FailureOrigin.Background(thread.getName)
    report(origin, error): Unit

  /** A job's defect, from the execution service. */
  def jobFailed(effect: String, error: Throwable): Unit =
    report(FailureOrigin.Job(effect), error): Unit

  /** Make [[handler]] the default handler and the JavaFX thread's handler;
    * returns the handlers it replaced, for [[ErrorReporter.restore]].
    */
  def install(): ErrorReporter.Installed =
    val previousDefault = Thread.getDefaultUncaughtExceptionHandler
    Thread.setDefaultUncaughtExceptionHandler(handler)
    val previousFx = runOnFx {
      val t    = Thread.currentThread
      val prev = t.getUncaughtExceptionHandler
      t.setUncaughtExceptionHandler(handler)
      prev
    }
    ErrorReporter.Installed(previousDefault, previousFx)

  private def onFx(f: () => Unit): Unit =
    if Platform.isFxApplicationThread then f() else Platform.runLater(() => f())

  private def runOnFx[A](body: => A): A =
    if Platform.isFxApplicationThread then body
    else
      val result = java.util.concurrent.CompletableFuture[A]()
      Platform.runLater { () =>
        try result.complete(body): Unit
        catch case t: Throwable => result.completeExceptionally(t): Unit
      }
      result.get()

  private def safely[A](body: => A): Option[A] =
    try Some(body)
    catch
      case NonFatal(e) =>
        System.err.println(s"Eyes Studio could not report a failure: ${e.getClass.getName}")
        None

object ErrorReporter:

  /** The handlers [[ErrorReporter.install]] replaced. */
  final case class Installed(
      default: Thread.UncaughtExceptionHandler,
      fx: Thread.UncaughtExceptionHandler
  ):
    /** Put them back (the FX thread's on the FX thread). */
    def restore(): Unit =
      Thread.setDefaultUncaughtExceptionHandler(default)
      val done = java.util.concurrent.CompletableFuture[Unit]()
      Platform.runLater { () =>
        Thread.currentThread.setUncaughtExceptionHandler(fx)
        done.complete(()): Unit
      }
      done.get()

  /** This build on this JVM. */
  def facts: BuildFacts =
    def prop(k: String) = sys.props.getOrElse(k, "unknown")
    BuildFacts(
      StudioBuild.version,
      StudioBuild.commit,
      StudioBuild.modified,
      ProtocolVersion.Current,
      Vector(
        "Java"   -> prop("java.version"),
        "JavaFX" -> prop("javafx.runtime.version"),
        "OS"     -> s"${prop("os.name")} ${prop("os.version")} ${prop("os.arch")}"
      )
    )

  /** The open project's scientific digest, `None` if it cannot be computed. */
  def digestOf(model: AppModel): Option[String] =
    StudioDocument.scienceDigest(model.document).toOption.map(_.display)
