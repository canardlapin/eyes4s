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

/** The strings of the Analysis perspective's preflight pane and run card
  * (ticket S7.6; Analysis.dc.html, preflight). Templates name their arguments
  * by position, as [[Messages]] does.
  */
enum PreflightTextId derives CanEqual:
  case Eyes4sHeading, StudyReport, NoStudioChecks
  case NotCheckedHeading, NotChecked
  case RunHeading, RunKind, PairRows, PairRowsValue, ChangeVs, NoChange
  case RunButton, Ready, BlockedShort, Blocked, Checking, CheckingProgress, NoDraft,
    NothingToCheck, Refused
  case Verdict

object PreflightText:

  def english(id: PreflightTextId): String =
    import PreflightTextId.*
    id match
      case Eyes4sHeading     => "eyes4s preflight"
      case StudyReport       => "StudyReport · {0}"
      case NoStudioChecks    => "No studio check applies"
      case NotCheckedHeading => "Not checked"
      case NotChecked        =>
        "Stimulus image content · calibration quality · sample-level validity (fixation input only)"
      case RunHeading       => "Run"
      case RunKind          => "Analysis · rerun"
      case PairRows         => "Pair rows"
      case PairRowsValue    => "{0} × {1} scales = {2}"
      case ChangeVs         => "Change vs {0}"
      case NoChange         => "none"
      case RunButton        => "Save & run {0} · {1} pairs"
      case Ready            => "Ready"
      case BlockedShort     => "Blocked"
      case Blocked          => "Save & run disabled: {0} blockers"
      case Checking         => "Checking {0}…"
      case CheckingProgress => "Checking {0}: {1} of {2} participants"
      case NoDraft          => "Only a draft can be saved and run; {0} is saved"
      case NothingToCheck   => "No analysis revision to check"
      case Refused          => "The check of {0} could not be made: {1}"
      case Verdict          => "{0} · {1} blockers · {2} warnings reported with the run"

  def apply(id: PreflightTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)
