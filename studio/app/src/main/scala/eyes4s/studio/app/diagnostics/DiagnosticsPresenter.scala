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

import eyes4s.studio.app.Intent
import eyes4s.studio.app.nav.{Location, Place}
import eyes4s.studio.app.plot.ViewSelection
import eyes4s.studio.app.text.{DiagnosticText, DiagnosticTextId}
import eyes4s.studio.core.backend.{
  DiagnosticLevel,
  DiagnosticLocus,
  DiagnosticOrigin,
  StudioDiagnostic,
  TrialKey
}
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.core.selection.{InputCause, SelectionMode, StudioRef}

/** How much a finding stops: a blocker disables Save & run; a warning is
  * reported with the run.
  */
enum FindingSeverity derives CanEqual:
  case Blocker, Warning

  def label: String = this match
    case Blocker => DiagnosticText(DiagnosticTextId.Blocker)
    case Warning => DiagnosticText(DiagnosticTextId.Warning)

/** A remedy: its words and the trials it opens, exactly the finding's
  * affected trials.
  */
final case class RemedyVM(label: String, trials: Vector[TrialKey]) derives CanEqual

/** One finding as the preflight pane shows it (Analysis.dc.html, preflight):
  * its code (its identity), severity, title and detail in the studio's words
  * for the code, the trials it names, and its remedy.
  */
final case class FindingVM(
    code: String,
    severity: FindingSeverity,
    title: String,
    detail: String,
    affected: Vector[TrialKey],
    remedy: Option[RemedyVM]
) derives CanEqual

/** The findings of a check, eyes4s's and the studio's own kept apart, and the
  * verdict: Save & run is disabled while any finding blocks.
  */
final case class DiagnosticsVM(
    eyes4s: Vector[FindingVM],
    studio: Vector[FindingVM],
    blockers: Int,
    warnings: Int,
    runnable: Boolean,
    verdict: String
) derives CanEqual

/** Presents diagnostics (ticket S3.5): eyes4s's by their stable code and the
  * studio's own checks (origin `Host`, code family `studio-check`) in a
  * separate section. Words come from the code, through [[DiagnosticText]];
  * a diagnostic's message is never shown as its title. Every number shown is
  * a count of what the diagnostic names. Pure.
  */
object DiagnosticsPresenter:

  /** The code family studio checks use. */
  val StudioFamily: String = "studio-check"

  def present(diagnostics: Vector[StudioDiagnostic]): DiagnosticsVM =
    val (studio, eyes) = diagnostics.partition(isStudioCheck)
    val all            = diagnostics.map(finding)
    val blockers       = all.count(_.severity == FindingSeverity.Blocker)
    val warnings       = all.size - blockers
    DiagnosticsVM(
      eyes.map(finding),
      studio.map(finding),
      blockers,
      warnings,
      blockers == 0,
      if blockers == 0 then DiagnosticText(DiagnosticTextId.Ready)
      else DiagnosticText(DiagnosticTextId.Blocked)
    )

  /** Whether a diagnostic is the studio's own check rather than eyes4s's. */
  def isStudioCheck(d: StudioDiagnostic): Boolean =
    d.origin == DiagnosticOrigin.Host || d.code.startsWith(StudioFamily + ".")

  /** The trials a diagnostic names: those it says it affects, else its
    * subject's.
    */
  def affected(d: StudioDiagnostic): Vector[TrialKey] =
    if d.affected.nonEmpty then d.affected
    else
      d.subject.flatMap {
        case DiagnosticLocus.Trial(k)   => Vector(k)
        case DiagnosticLocus.Trials(ks) => ks
        case DiagnosticLocus.Pair(f, r) => Vector(f, r)
        case _                          => Vector.empty
      }.distinct

  def finding(d: StudioDiagnostic): FindingVM =
    import DiagnosticTextId.*
    val trials   = affected(d)
    val severity = d.level match
      case DiagnosticLevel.Error   => FindingSeverity.Blocker
      case DiagnosticLevel.Warning => FindingSeverity.Warning
    val count    = trials.size.toString
    val people   = trials.map(_.participant).distinct.mkString(", ")
    val inTrials =
      if trials.isEmpty then DiagnosticText(TrialsAffected, "0")
      else DiagnosticText(TrialsAffectedIn, count, people)
    def withCategory(text: String) =
      d.category.fold(text)(c => DiagnosticText(CategoryAndCount, c, text))
    val (title, detail) = d.code match
      case "study-finding.matched-cardinality" =>
        val focal      = d.subject.collectFirst { case DiagnosticLocus.Trial(k) => k }
        val references = trials.filterNot(focal.contains)
        (
          DiagnosticText(MatchedCardinality),
          focal.fold(inTrials)(f =>
            DiagnosticText(
              ReferencesOf,
              references.size.toString,
              f.label,
              references.map(_.occurrence.toString).mkString(", ")
            )
          )
        )
      case "study-finding.no-fixation-in-window" =>
        (DiagnosticText(NoFixationInWindow), inTrials)
      case "study-finding.off-window-fixations" =>
        (DiagnosticText(OffWindowFixations), inTrials)
      case "study-finding.unmatched-focal" =>
        (DiagnosticText(UnmatchedFocal), withCategory(DiagnosticText(TrialsAffected, count)))
      case "study-finding.uncontrolled-focal" =>
        (DiagnosticText(UncontrolledFocal), withCategory(DiagnosticText(TrialsAffected, count)))
      case "study-finding.duplicate-trial" =>
        (DiagnosticText(DuplicateTrial), withCategory(DiagnosticText(TrialsAffected, count)))
      case "study-finding.ambiguous-references" =>
        (DiagnosticText(AmbiguousReferences), inTrials)
      case "study-finding.unmatched-focal-refused" =>
        (DiagnosticText(UnmatchedFocalRefused), inTrials)
      case "study-finding.match-item-conflict" => (DiagnosticText(MatchItemConflict), inTrials)
      case "study-finding.no-fixation-kept"    => (DiagnosticText(NoFixationKept), inTrials)
      case "study-failure.off-window"          => (DiagnosticText(FailsOffWindow), inTrials)
      case other                               => (other, DiagnosticText(UnknownCode, inTrials))
    FindingVM(d.code, severity, title, detail, trials, remedyOf(d, trials))

  // A remedy names the action and opens exactly the affected trials; one
  // that names no trial has nothing to open.
  private def remedyOf(d: StudioDiagnostic, trials: Vector[TrialKey]): Option[RemedyVM] =
    import DiagnosticTextId.*
    Option.when(trials.nonEmpty) {
      val label = d.remedy match
        case Some("ChooseMatchedReference")      => DiagnosticText(ChooseOccurrence)
        case Some("ReviewAnalysisWindow")        => DiagnosticText(ReviewWindow)
        case Some("ResolveDuplicateTrials")      => DiagnosticText(ResolveDuplicates)
        case Some("ResolveMatchItemConflict")    => DiagnosticText(ResolveItemConflict)
        case Some("ReviseInitialFixationPolicy") => DiagnosticText(ReviseInitialFixations)
        case _                                   => DiagnosticText(OpenTrials)
      RemedyVM(label, trials)
    }

  /** What a remedy does: Explore on its first trial, with exactly its trials
    * selected, from the view `selection`.
    */
  def open(remedy: RemedyVM, selection: ViewSelection): (ViewSelection, Vector[Intent]) =
    val refs           = remedy.trials.map(StudioRef.Trial(_))
    val (next, select) = selection.submit(SelectionMode.Replace, refs, InputCause.Pointer)
    val go             = refs.headOption.map(first =>
      Intent.Navigate(Location(Perspective.Explore, Vector(Place.At(first))))
    )
    (next, go.toVector :+ select)
