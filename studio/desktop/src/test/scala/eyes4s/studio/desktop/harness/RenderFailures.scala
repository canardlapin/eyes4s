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

import java.io.{PrintStream, PrintWriter, StringWriter}
import java.util.concurrent.ConcurrentLinkedQueue
import javafx.application.Platform

/** How a [[RenderFailure]] reached the harness. */
enum RenderFailureRoute derives CanEqual:

  /** It escaped a task on the FX application thread to that thread's uncaught
    * exception handler (an event handler, a `runLater` body, a pulse listener,
    * or the desktop runtime's report of a view that failed to render).
    */
  case Uncaught

  /** A JavaFX toolkit thread caught it and printed its stack trace to
    * `System.err`, which is all Quantum and Prism do with a failure during a
    * pulse or a render job (`PresentingPainter`, `RenderJob`, snapshots and the
    * `QuantumRenderer` thread's own handler).
    */
  case PrintedByToolkit

/** A throwable reported on a JavaFX thread instead of thrown to a test. */
final case class RenderFailure(thread: String, route: RenderFailureRoute, error: Throwable):

  /** The thread, the route and the full stack trace. */
  def describe: String =
    val trace = StringWriter()
    error.printStackTrace(PrintWriter(trace))
    s"on thread \"$thread\" ($route):\n$trace"

/** Collects the failures JavaFX reports on its own threads, so an FX test
  * cannot pass while its scene failed to pulse or render (S0.4 harness).
  *
  * JavaFX gives a test no signal when rendering fails: Quantum and Prism catch
  * every throwable of a paint or render job and print it. The collector
  * therefore has two inputs, installed once per test JVM before the toolkit
  * starts:
  *
  *   - `System.err` is wrapped. A throwable printed with `printStackTrace` from
  *     the FX application thread or a toolkit thread (`QuantumRenderer-*`,
  *     `JavaFX*`, `Prism*`) is recorded as it is printed; the text still
  *     reaches the original stream. `Throwable.printStackTrace(PrintStream)`
  *     passes the throwable itself to `println(Object)`, so the collector keeps
  *     the throwable, not a parse of its text. Throwables printed from any
  *     other thread, including test threads, are not recorded.
  *   - The FX application thread gets an uncaught-exception handler that
  *     records the throwable and prints it to the original stream.
  *
  * Not captured: what JavaFX routes through `PlatformLogger` or
  * `System.Logger` rather than `printStackTrace`, such as CSS parse and lookup
  * warnings and image loading errors, and throwables printed only as text.
  */
object RenderFailures:

  private val captured = ConcurrentLinkedQueue[RenderFailure]()

  private def toolkitThread(t: Thread): Boolean =
    val name = t.getName
    Platform.isFxApplicationThread || name.startsWith("QuantumRenderer") ||
    name.startsWith("JavaFX") || name.startsWith("Prism")

  private final class Capturing(original: PrintStream)
      extends PrintStream(original, true, original.charset()):
    override def println(x: Object): Unit =
      x match
        case t: Throwable if toolkitThread(Thread.currentThread) =>
          captured.add(
            RenderFailure(Thread.currentThread.getName, RenderFailureRoute.PrintedByToolkit, t)
          ): Unit
        case _ => ()
      super.println(x)

  private lazy val originalErr: PrintStream = System.err

  private lazy val streamInstalled: Unit =
    System.setErr(Capturing(originalErr))

  /** Wraps `System.err`; idempotent. Call before the toolkit starts. */
  def installStream(): Unit = streamInstalled

  /** Sets the recording handler on the current thread, which must be the FX
    * application thread; idempotent.
    */
  def installOnFxThread(): Unit =
    val fx = Thread.currentThread
    fx.getUncaughtExceptionHandler match
      case _: Recording => ()
      case _            => fx.setUncaughtExceptionHandler(Recording())

  private final class Recording extends Thread.UncaughtExceptionHandler:
    def uncaughtException(t: Thread, e: Throwable): Unit =
      captured.add(RenderFailure(t.getName, RenderFailureRoute.Uncaught, e))
      // The original stream, not the wrapped one, so it is recorded once.
      originalErr.println(
        s"Exception in thread \"${t.getName}\" (recorded for the running test)"
      )
      e.printStackTrace(originalErr)

  /** Removes and returns every failure recorded so far, oldest first. */
  def drain(): Vector[RenderFailure] =
    Iterator.continually(captured.poll()).takeWhile(_ != null).toVector
