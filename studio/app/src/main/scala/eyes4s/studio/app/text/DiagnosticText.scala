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

/** The studio's words for eyes4s diagnostics and its own checks (ticket
  * S3.5), chosen by stable code, never from a diagnostic's message.
  * Templates name their arguments by position, as [[Messages]] does.
  */
enum DiagnosticTextId derives CanEqual:
  /** Section headings and the studio-check note. */
  case Eyes4sSection, StudioSection, StudioRuleNote

  /** Severities. */
  case Blocker, Warning

  /** Titles of the codes the studio knows. */
  case MatchedCardinality, NoFixationInWindow, OffWindowFixations, UnmatchedFocal,
    UncontrolledFocal, DuplicateTrial, AmbiguousReferences, UnmatchedFocalRefused,
    MatchItemConflict, NoFixationKept, FailsOffWindow

  /** Details: references of a focal trial, trials affected, and a code the
    * studio has no words for.
    */
  case ReferencesOf, ReferenceOf, NoReferenceOf, TrialsAffected, TrialAffected,
    TrialsAffectedIn, TrialAffectedIn, CategoryAndCount, UnknownCode

  /** eyes4s's finding classes. */
  case ClassUnavailableInput, ClassIncompatibleInput, ClassInvalidSetting, ClassDataDependent

  /** Remedies. */
  case ChooseOccurrence, ReviewWindow, OpenTrials, ResolveDuplicates, ResolveItemConflict,
    ReviseInitialFixations

  /** The run's verdict: ready or blocked, with its counts. */
  case Ready, Blocked, BlockerCount, BlockersCount, WarningCount, WarningsCount

object DiagnosticText:

  def english(id: DiagnosticTextId): String =
    import DiagnosticTextId.*
    id match
      case Eyes4sSection          => "eyes4s preflight"
      case StudioSection          => "Studio check"
      case StudioRuleNote         => "Eyes Studio rule, not eyes4s"
      case Blocker                => "Blocker"
      case Warning                => "Warning"
      case MatchedCardinality     => "Matched cardinality"
      case NoFixationInWindow     => "Empty map in window"
      case OffWindowFixations     => "Fixations outside the window"
      case UnmatchedFocal         => "Focal trials without match"
      case UncontrolledFocal      => "Focal trials without control"
      case DuplicateTrial         => "Duplicate trial"
      case AmbiguousReferences    => "Ambiguous references"
      case UnmatchedFocalRefused  => "Unmatched focal trial refused"
      case MatchItemConflict      => "Match item conflict"
      case NoFixationKept         => "No fixation kept"
      case FailsOffWindow         => "Trial fails: fixations outside the window"
      case ReferencesOf           => "{0} matched references for {1} (occurrences {2})"
      case ReferenceOf            => "1 matched reference for {1} (occurrence {2})"
      case NoReferenceOf          => "No matched reference for {1}"
      case TrialsAffected         => "{0} trials affected"
      case TrialAffected          => "1 trial affected"
      case TrialsAffectedIn       => "{0} trials ({1})"
      case TrialAffectedIn        => "1 trial ({1})"
      case ClassUnavailableInput  => "Unavailable input"
      case ClassIncompatibleInput => "Incompatible input"
      case ClassInvalidSetting    => "Invalid setting"
      case ClassDataDependent     => "Depends on the data"
      case CategoryAndCount       => "{0} · {1}"
      case UnknownCode            => "{0} · no Studio text for this code"
      case ChooseOccurrence       => "Choose occurrence…"
      case ReviewWindow           => "Review the analysis window"
      case OpenTrials             => "Open the affected trials"
      case ResolveDuplicates      => "Resolve the duplicate trials"
      case ResolveItemConflict    => "Resolve the item conflict"
      case ReviseInitialFixations => "Revise the initial-fixation policy"
      case Ready                  => "Ready"
      case Blocked                => "Save & run disabled"
      case BlockerCount           => "1 blocker"
      case BlockersCount          => "{0} blockers"
      case WarningCount           => "1 warning"
      case WarningsCount          => "{0} warnings"

  def apply(id: DiagnosticTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)

  // The singular form for one, the plural otherwise (0 included).
  private def counted(n: Int, one: DiagnosticTextId, many: DiagnosticTextId, rest: String*) =
    apply(if n == 1 then one else many, (n.toString +: rest)*)

  /** "1 trial affected", "9 trials affected". */
  def trialsAffected(n: Int): String =
    counted(n, DiagnosticTextId.TrialAffected, DiagnosticTextId.TrialsAffected)

  /** "1 trial (P11)", "9 trials (P03, P07)"; "0 trials affected" for none. */
  def trialsAffectedIn(n: Int, participants: String): String =
    if n == 0 then trialsAffected(0)
    else
      counted(
        n,
        DiagnosticTextId.TrialAffectedIn,
        DiagnosticTextId.TrialsAffectedIn,
        participants
      )

  /** "1 matched reference for P11 · ret_05 (occurrence 1)", or "2 … (occurrences 1, 2)". */
  def referencesOf(n: Int, focal: String, occurrences: String): String =
    if n == 0 then apply(DiagnosticTextId.NoReferenceOf, "0", focal)
    else
      counted(
        n,
        DiagnosticTextId.ReferenceOf,
        DiagnosticTextId.ReferencesOf,
        focal,
        occurrences
      )

  /** "1 blocker", "2 blockers". */
  def blockers(n: Int): String =
    counted(n, DiagnosticTextId.BlockerCount, DiagnosticTextId.BlockersCount)

  /** "1 warning", "0 warnings". */
  def warnings(n: Int): String =
    counted(n, DiagnosticTextId.WarningCount, DiagnosticTextId.WarningsCount)
