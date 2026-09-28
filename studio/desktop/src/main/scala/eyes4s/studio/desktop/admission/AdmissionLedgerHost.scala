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

package eyes4s.studio.desktop.admission

import eyes4s.codec.CanonicalDigest
import eyes4s.studio.app.admission.*
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.backend.{
  AdmissionSummary,
  BackendError,
  DatasetRevision,
  LedgerEntry,
  LedgerPages
}
import eyes4s.studio.core.document.{DatasetRevisionSpec, Perspective}
import eyes4s.studio.desktop.runtime.StudioSession
import javafx.application.Platform

/** Where the admission ledger asks for a revision's admission counts and its
  * whole ledger. `done` may be called on any thread.
  */
trait LedgerInputs:
  def admission(dataset: DatasetRevision, done: AdmissionAnswer => Unit): Unit
  def ledger(dataset: DatasetRevision, done: Either[String, Vector[LedgerEntry]] => Unit): Unit

object LedgerInputs:
  /** The window's backend. */
  def of(session: StudioSession): LedgerInputs =
    new LedgerInputs:
      def admission(dataset: DatasetRevision, done: AdmissionAnswer => Unit): Unit =
        session.run(session.backend.admission(dataset))(r => done(answer(r)))
      def ledger(
          dataset: DatasetRevision,
          done: Either[String, Vector[LedgerEntry]] => Unit
      ): Unit =
        session.run(LedgerPages.all(session.backend.ledger(dataset, _))) {
          case Left(e)          => done(Left(reason(e)))
          case Right(Left(err)) => done(Left(err.message))
          case Right(Right(es)) => done(Right(es))
        }

  private def reason(e: Throwable): String = Option(e.getMessage).getOrElse(e.toString)

  /** A backend call's outcome as an admission answer. */
  def answer(
      result: Either[Throwable, Either[BackendError, AdmissionSummary]]
  ): AdmissionAnswer = result match
    case Left(e)          => AdmissionAnswer.Failed(reason(e))
    case Right(Left(err)) => AdmissionAnswer.Refused(err)
    case Right(Right(s))  => AdmissionAnswer.Answered(s)

/** The Data perspective's admission ledger on the desktop (Data.dc.html,
  * admission; see [[AdmissionLedger]]). It follows the model's selected
  * dataset revision, asks the backend for its counts and its whole ledger,
  * and sends the ledger's commands and navigations to the app through
  * `app`. The answer to a verification the app requested arrives through
  * [[verified]]. Use on the JavaFX thread.
  */
final class AdmissionLedgerHost(
    model: () => AppModel,
    app: Intent => Unit,
    inputs: LedgerInputs
):
  private var ledger  = AdmissionLedger.empty
  private var started = false

  val view: AdmissionLedgerView = AdmissionLedgerView(dispatch)
  def node: javafx.scene.Node   = view.node

  /** The ledger's state now. */
  def state: AdmissionLedger = ledger

  /** The view-model now shown. */
  def vm: AdmissionLedgerVM = AdmissionLedgerVM.of(ledger, model())

  /** Follow the model. Nothing is asked until the Data perspective has been
    * shown.
    */
  def sync(m: AppModel): Unit =
    started = started || m.perspective == Perspective.Data || ledger.shown.isDefined
    if started then
      val (next, effects) = AdmissionLedger.sync(ledger, m)
      ledger = next
      perform(effects)
      view.render(AdmissionLedgerVM.of(ledger, m))

  /** A user action on the ledger, or a backend answer. */
  def dispatch(intent: LedgerIntent): Unit =
    val (next, effects) = AdmissionLedger.update(ledger, model(), intent)
    ledger = next
    perform(effects)
    view.render(AdmissionLedgerVM.of(ledger, model()))

  /** The backend's answer to the app's `RequestAdmission` of `content`. */
  def verified(
      dataset: DatasetRevision,
      content: CanonicalDigest[DatasetRevisionSpec],
      answer: AdmissionAnswer
  ): Unit = dispatch(LedgerIntent.Verified(dataset, content, answer))

  private def perform(effects: Vector[LedgerEffect]): Unit =
    effects.foreach {
      case LedgerEffect.App(i) => app(i)
      case LedgerEffect.RequestCounts(d) =>
        inputs.admission(
          d,
          a => Platform.runLater(() => dispatch(LedgerIntent.CountsRead(d, a)))
        )
      case LedgerEffect.RequestLedger(d) =>
        inputs.ledger(
          d,
          r => Platform.runLater(() => dispatch(LedgerIntent.LedgerRead(d, r)))
        )
    }

  def dispose(): Unit = view.dispose()
