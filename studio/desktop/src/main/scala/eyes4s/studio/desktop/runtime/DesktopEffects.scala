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

import eyes4s.studio.app.{AppEffect, Intent, PlatformDialog}
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.core.execution.{ExecutionEffect, ExecutionError}

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

  /** The execution service failed: a defect of the backend. */
  case Failed(effect: ExecutionEffect, error: String)

  def message: String = this match
    case NotWired(e, t) => s"$e is not performed until $t."
    case Refused(e, r)  => s"The execution service refused $e: ${r.message}"
    case Failed(e, r)   => s"The execution service failed on $e: $r"

/** Performs app effects on the desktop (tickets S1.4, S1.5a).
  *
  * Execution effects go to the session's execution service, whose events
  * come back as [[Intent.Execution]]; dialogs go to the platform; a layout
  * reset goes to the perspective host. Effects whose service does not exist
  * yet are recorded in [[problems]]. `ui` runs a callback on the UI thread.
  */
final class DesktopEffects(
    session: StudioSession,
    dialogs: PlatformDialogs,
    resetLayouts: Perspective => Unit,
    ui: (() => Unit) => Unit
) extends EffectPerformer:

  private val found = mutable.ArrayBuffer.empty[EffectProblem]

  /** Every effect not carried out, in order. Read on the UI thread. */
  def problems: Vector[EffectProblem] = found.toVector

  /** Refusals and failures also go to stderr until S1.12's log exists;
    * effects not wired yet are only recorded (every view change journals).
    */
  private def report(problem: EffectProblem): Unit =
    found += problem
    problem match
      case _: EffectProblem.NotWired => ()
      case _                         => System.err.println(problem.message)

  def perform(effect: AppEffect, dispatch: Intent => Unit): Unit = effect match
    case AppEffect.Execution(e) =>
      session.run(ExecutionEffect.perform(session.service)(e)) {
        case Right(Right(()))   => ()
        case Right(Left(error)) => ui(() => report(EffectProblem.Refused(e, error)))
        case Left(defect)       =>
          ui(() => report(EffectProblem.Failed(e, String.valueOf(defect.getMessage))))
      }
    case AppEffect.OpenDialog(d)       => dialogs.open(d, dispatch)
    case AppEffect.ResetLayouts(p)     => resetLayouts(p)
    case e @ AppEffect.RevealProject   => report(EffectProblem.NotWired(e, "S2.9"))
    case e @ AppEffect.Persist         => report(EffectProblem.NotWired(e, "S2.4a"))
    case e: AppEffect.Journal          => report(EffectProblem.NotWired(e, "S2.4b"))
    case e: AppEffect.RequestAdmission => report(EffectProblem.NotWired(e, "S5.6"))
