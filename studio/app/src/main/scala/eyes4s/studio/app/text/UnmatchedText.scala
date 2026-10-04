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

package eyes4s.studio.app.text

import eyes4s.plan.{TrialDisposition, UnmatchedKind}

/** The words for eyes4s's `UnmatchedKind` (ticket S7.1): why a query has no
  * matched reference. eyes4s decides the kind against the trial inventory;
  * the studio only words it, and never decides it.
  */
object UnmatchedText:

  def apply(kind: UnmatchedKind): String = kind match
    case UnmatchedKind.NoReferenceInDesign => "No corresponding study trial (by design)"
    case UnmatchedKind.ReferenceNotAdmitted(references) =>
      "No match · " + references
        .map(r => s"${r.trial.trial} ${disposition(r.disposition)}")
        .mkString(", ")
    case UnmatchedKind.ReferenceNotPairable(references) =>
      "No match · " + references.map(_.trial).mkString(", ") + " cannot be paired"
    case UnmatchedKind.Undetermined => "No match"

  private def disposition(d: TrialDisposition): String = d match
    case TrialDisposition.Quarantined(cause) => s"quarantined (${cause.productPrefix})"
    case other                               => other.label
