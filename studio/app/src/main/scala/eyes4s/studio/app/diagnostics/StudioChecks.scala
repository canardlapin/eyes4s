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

package eyes4s.studio.app.diagnostics

import eyes4s.plan.DiagnosticCode
import eyes4s.studio.core.backend.{
  DiagnosticLevel,
  DiagnosticLocus,
  DiagnosticOrigin,
  StudioDiagnostic,
  TrialKey
}

/** Why a studio check could not be made; every case names its operand. */
enum StudioCheckError derives CanEqual:
  /** The check's name is not a kebab-case code name. */
  case InvalidName(name: String, reason: String)

  /** eyes4s now makes this check itself: the studio rule is retired. */
  case Retired(code: String, replacement: String)

  def message: String = this match
    case InvalidName(n, r) => s"Studio check '$n' is not a valid code name: $r"
    case Retired(c, r)     => s"Studio check $c is retired; eyes4s reports $r."

/** The studio's own checks (ticket S3.5): rules of Eyes Studio, not of eyes4s,
  * reported as diagnostics of origin `Host` in the `studio-check` code family,
  * so they are never mistaken for eyes4s's findings. eyes4s's
  * `DiagnosticCode.host` makes the code, so it follows eyes4s's code grammar.
  *
  * The two rules the plan named are retired, because eyes4s now reports them
  * itself (UI-B and UI-A have landed): a query with two or more matched
  * references is eyes4s's `study-finding.matched-cardinality`, a blocker
  * under a one-reference rule; a trial with no fixation in the window is its
  * `study-finding.no-fixation-in-window`. A retired rule cannot be made
  * again, so it cannot shadow eyes4s's finding.
  */
object StudioChecks:

  /** Each retired studio rule and the eyes4s code that replaced it. */
  val retired: Map[String, String] = Map(
    "studio-check.matched-cardinality" -> "study-finding.matched-cardinality",
    "studio-check.empty-map-in-window" -> "study-finding.no-fixation-in-window"
  )

  /** A studio check's finding about `trials`, at `level`, worded `message`
    * (its default English; the presenter words it from its code).
    */
  def finding(
      name: String,
      level: DiagnosticLevel,
      trials: Vector[TrialKey],
      message: String
  ): Either[StudioCheckError, StudioDiagnostic] =
    for
      code <- DiagnosticCode
        .host(DiagnosticsPresenter.StudioFamily, name)
        .left
        .map(e => StudioCheckError.InvalidName(name, e.message))
      rendered = code.render
      _ <- retired.get(rendered).map(StudioCheckError.Retired(rendered, _)).toLeft(())
    yield StudioDiagnostic(
      rendered,
      level,
      DiagnosticOrigin.Host,
      trials.map(DiagnosticLocus.Trial(_)),
      message,
      trials
    )
