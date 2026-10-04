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

import eyes4s.codec.CanonicalDigest
import eyes4s.studio.app.admission.AdmissionAnswer
import eyes4s.studio.app.{AppEffect, ClockTime, DockCommand, Intent, PlatformDialog}
import eyes4s.studio.core.backend.DatasetRevision
import eyes4s.studio.core.document.{DatasetRevisionSpec, Perspective}
import eyes4s.studio.desktop.admission.LedgerInputs
import eyes4s.studio.core.execution.{ExecutionEffect, ExecutionError}

import cats.effect.IO

import scala.collection.mutable

/** The dialogs only the platform can show. Each answers, if at all, with an
  * intent through `dispatch`.
  */
trait PlatformDialogs:
  def open(dialog: PlatformDialog, dispatch: Intent => Unit): Unit

/** Why an effect was not carried out. */
enum EffectProblem derives CanEqual:
  /** No service performs this effect yet; it is recorded, not lost silently. */
  case NotWired(effect: AppEffect, ticket: String)

  /** The execution service refused the effect (a value, not a defect). */
  case Refused(effect: ExecutionEffect, error: ExecutionError)

  /** The execution service failed: a defect of the backend, named by the
    * failure's class (its message can hold data values).
    */
  case Failed(effect: ExecutionEffect, failure: String)

  def message: String = this match
    case NotWired(e, t) => s"$e is not performed until $t."
    case Refused(e, r)  => s"The execution service refused $e: ${r.message}"
    case Failed(e, f)   => s"The execution service failed on $e: $f"

  /** What the studio log records: the kind of effect and the error's stable
    * code or the failure's class only, never
    * their values or messages, which can hold participant data (S1.12).
    */
  def logLine: String = this match
    case NotWired(e, t) => s"${e.productPrefix} is not performed until $t."
    case Refused(e, r)  => s"The execution service refused ${e.productPrefix}: ${r.code}"
    case Failed(e, f)   => s"The execution service failed on ${e.productPrefix}: $f"

/** Performs app effects on the desktop (tickets S1.4, S1.5a).
  *
  * Execution effects go to the session's execution service, whose events
  * come back as [[Intent.Execution]]; dialogs go to the platform; a layout
  * reset and tab cycling go to the perspective host. The journal and saves
  * go to the window's [[ProjectPort]], when it has one (S1.8): a finished
  * save comes back as [[Intent.Saved]] at the platform clock's time, a
  * failed one as [[Intent.SaveFailed]]. A verification asks the backend for
  * the revision's admission, and its answer goes to `verified` on the UI
  * thread (the admission ledger, S5.6). Effects whose service does not exist
  * yet are recorded in [[problems]]. `ui` runs a callback on the UI thread.
  */
final class DesktopEffects(
    session: StudioSession,
    dialogs: PlatformDialogs,
    resetLayouts: Perspective => Unit,
    dock: DockCommand => Unit,
    ui: (() => Unit) => Unit,
    project: Option[ProjectPort] = None,
    clock: () => Option[ClockTime] = DesktopEffects.wallClock,
    verified: (DatasetRevision, CanonicalDigest[DatasetRevisionSpec], AdmissionAnswer) => Unit =
      (_, _, _) => (),
    defect: (String, Throwable) => Unit = (_, _) => (),
    // The execution service's handling of an effect; tests plant a defect.
    execute: Option[ExecutionEffect => IO[Either[ExecutionError, Unit]]] = None
) extends EffectPerformer:

  private val found = mutable.ArrayBuffer.empty[EffectProblem]

  /** Every effect not carried out, in order. Read on the UI thread. */
  def problems: Vector[EffectProblem] = found.toVector

  /** Refusals and failures also go to the studio log (S1.12); effects not
    * wired yet are only recorded (every view change journals).
    */
  private def report(problem: EffectProblem): Unit =
    found += problem
    problem match
      case _: EffectProblem.NotWired => ()
      case _                         => DesktopEffects.log.warn(problem.logLine)

  def perform(effect: AppEffect, dispatch: Intent => Unit): Unit = effect match
    case AppEffect.Execution(e) =>
      session.run(execute.fold(ExecutionEffect.perform(session.service)(e))(_(e))) {
        case Right(Right(()))   => ()
        case Right(Left(error)) =>
          ui { () =>
            report(EffectProblem.Refused(e, error))
            // A refused prepared design falls back to a plain submission.
            e match
              case ExecutionEffect.SubmitPreview(ready) =>
                dispatch(Intent.PreparedRefused(ready, error))
              case _ => ()
          }
        case Left(failure) =>
          ui { () =>
            report(EffectProblem.Failed(e, failure.getClass.getName))
            // A job's defect opens the error report (S1.12).
            defect(e.productPrefix, failure)
          }
      }
    case AppEffect.OpenDialog(d)   => dialogs.open(d, dispatch)
    case AppEffect.ResetLayouts(p) => resetLayouts(p)
    case AppEffect.Dock(command)   => dock(command)
    // Without a project there is nothing to check, and so nothing is known
    // to be as recorded: the check fails, and runs stay blocked (S2.5).
    case AppEffect.CheckInputs =>
      project match
        case None => dispatch(Intent.InputsCheckFailed("this window has no project to check"))
        case Some(port) =>
          port.checkInputs { answer =>
            ui { () =>
              dispatch(answer.fold(Intent.InputsCheckFailed(_), Intent.InputsChecked(_)))
            }
          }
    case e @ AppEffect.RevealProject => report(EffectProblem.NotWired(e, "S2.9"))
    case e @ AppEffect.Persist(mark) =>
      project.fold(report(EffectProblem.NotWired(e, "S2.9"))) {
        _.save { outcome =>
          ui { () =>
            outcome match
              case Left(reason) => dispatch(Intent.SaveFailed(reason))
              case Right(_)     => clock().foreach(t => dispatch(Intent.Saved(t, mark)))
          }
        }
      }
    case e: AppEffect.Journal =>
      project.fold(report(EffectProblem.NotWired(e, "S2.9")))(_.journal(e.entry))
    case AppEffect.RequestAdmission(dataset, content) =>
      session.run(session.backend.admission(dataset)) { result =>
        ui(() => verified(dataset, content, LedgerInputs.answer(result)))
      }

object DesktopEffects:
  private val log = org.slf4j.LoggerFactory.getLogger("eyes4s.studio.effects")

  /** The local wall-clock time now, as the status bar shows it. */
  val wallClock: () => Option[ClockTime] = () =>
    val now = java.time.LocalTime.now()
    ClockTime.of(now.getHour, now.getMinute).toOption
