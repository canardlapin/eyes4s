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

package eyes4s.studio.desktop.runtime

import eyes4s.studio.app.{AppEffect, AppModel, Intent}

import scala.collection.mutable

/** A listener threw while rendering the model after `intent`. */
final case class ListenerFailure(intent: Intent, error: Throwable):
  def message: String = s"A view failed to render after $intent: $error"

/** A [[ListenerFailure]] as a throwable, with the listener's error as its cause,
  * for a thread's uncaught-exception handler.
  */
final class ListenerFailureException(val failure: ListenerFailure)
    extends RuntimeException(failure.message, failure.error)

object StudioRuntime:

  /** Hands the failure, as a [[ListenerFailureException]], to the current
    * thread's uncaught-exception handler without ending the thread: the default
    * handler prints it with both stack traces, and the FX test harness records
    * it against the running test.
    */
  val reportToThread: ListenerFailure => Unit = failure =>
    val thread = Thread.currentThread
    thread.getUncaughtExceptionHandler.uncaughtException(
      thread,
      ListenerFailureException(failure)
    )

/** Performs the effects [[AppModel.update]] returns (DESIGN_SPEC section 13):
  * services, platform dialogs, the dock. Results come back as intents
  * through `dispatch`, never as a direct model change.
  */
trait EffectPerformer:
  def perform(effect: AppEffect, dispatch: Intent => Unit): Unit

/** The desktop's Elm loop (tickets S1.4, S1.5a): it holds the one
  * [[AppModel]], applies each intent with the pure update, tells every
  * listener about the new model, and hands the effects to the performer.
  *
  * Confined to the JavaFX application thread. An intent dispatched while
  * another is being applied (by a listener or a performer) is queued and
  * applied next, so listeners always see models in update order.
  */
final class StudioRuntime(
    initial: AppModel,
    performer: EffectPerformer,
    report: ListenerFailure => Unit = StudioRuntime.reportToThread
):

  private var current   = initial
  private var running   = false
  private val queued    = mutable.Queue.empty[Intent]
  private val listeners = mutable.ArrayBuffer.empty[AppModel => Unit]

  def model: AppModel = current

  /** Call `f` now with the current model, and after every update. */
  def listen(f: AppModel => Unit): Unit =
    listeners += f
    f(current)

  /** A listener that throws is recorded, passed to `report` and skipped: the
    * other listeners and the intent's effects still run.
    */
  private def notify(f: AppModel => Unit, model: AppModel, intent: Intent): Unit =
    try f(model)
    catch
      case scala.util.control.NonFatal(e) =>
        val failure = ListenerFailure(intent, e)
        failures += failure
        report(failure)

  private val failures = mutable.ArrayBuffer.empty[ListenerFailure]

  /** Every listener failure so far, in order. */
  def listenerFailures: Vector[ListenerFailure] = failures.toVector

  def dispatch(intent: Intent): Unit =
    queued.enqueue(intent)
    if !running then
      running = true
      try
        while queued.nonEmpty do
          val next             = queued.dequeue()
          val (model, effects) = AppModel.update(current, next)
          current = model
          listeners.foreach(notify(_, model, next))
          effects.foreach(performer.perform(_, dispatch))
      finally running = false
