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

import eyes4s.plan.{FindingClass, Remedy}
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

  /** The findings with one entry per finding kind, as a preflight lists them
    * (Analysis.dc.html, preflight): findings of one code, severity and remedy
    * become one, naming every trial its members name, in first-seen order. A
    * kind with one member is shown as it is. The blocker and warning counts
    * are of the entries; the verdict is unchanged.
    */
  def grouped(vm: DiagnosticsVM): DiagnosticsVM =
    def merge(findings: Vector[FindingVM]): Vector[FindingVM] =
      val keyOf = (f: FindingVM) => (f.code, f.severity, f.remedy.map(_.label))
      val keys  = findings.map(keyOf).distinct
      keys.map { k =>
        findings.filter(f => keyOf(f) == k) match
          case Vector(one) => one
          case many        =>
            val trials = many.flatMap(_.affected).distinct
            val people = trials.map(_.participant).distinct.mkString(", ")
            val detail = DiagnosticText.trialsAffectedIn(trials.size, people)
            many.head.copy(
              detail = detail,
              affected = trials,
              remedy = many.head.remedy.map(_.copy(trials = trials))
            )
      }
    val eyes   = merge(vm.eyes4s)
    val studio = merge(vm.studio)
    val all    = eyes ++ studio
    val block  = all.count(_.severity == FindingSeverity.Blocker)
    vm.copy(eyes4s = eyes, studio = studio, blockers = block, warnings = all.size - block)

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

  // What a code's detail says: a focal trial's references, the trials it
  // names, or how many trials, after the finding's class.
  private enum Detail derives CanEqual:
    case References, InTrials, Counted

  // The studio's words for each code it knows: a title and a detail.
  private val wording: Map[String, (DiagnosticTextId, Detail)] =
    import DiagnosticTextId.*
    Map(
      "study-finding.matched-cardinality"     -> (MatchedCardinality, Detail.References),
      "study-finding.no-fixation-in-window"   -> (NoFixationInWindow, Detail.InTrials),
      "study-finding.off-window-fixations"    -> (OffWindowFixations, Detail.InTrials),
      "study-finding.unmatched-focal"         -> (UnmatchedFocal, Detail.Counted),
      "study-finding.uncontrolled-focal"      -> (UncontrolledFocal, Detail.Counted),
      "study-finding.duplicate-trial"         -> (DuplicateTrial, Detail.Counted),
      "study-finding.ambiguous-references"    -> (AmbiguousReferences, Detail.InTrials),
      "study-finding.unmatched-focal-refused" -> (UnmatchedFocalRefused, Detail.InTrials),
      "study-finding.match-item-conflict"     -> (MatchItemConflict, Detail.InTrials),
      "study-finding.no-fixation-kept"        -> (NoFixationKept, Detail.InTrials),
      "study-failure.off-window"              -> (FailsOffWindow, Detail.InTrials)
    )

  /** The codes the studio has words for; each is an eyes4s catalog code. */
  val wordedCodes: Set[String] = wording.keySet

  /** A diagnostic's finding class, when it names one eyes4s has. */
  def categoryOf(d: StudioDiagnostic): Option[FindingClass] =
    d.category.flatMap(c => FindingClass.values.find(_.toString == c))

  /** A diagnostic's remedy, when it names one eyes4s has. */
  def remedyOf(d: StudioDiagnostic): Option[Remedy] =
    d.remedy.flatMap(r => Remedy.values.find(_.toString == r))

  def finding(d: StudioDiagnostic): FindingVM =
    import DiagnosticTextId.*
    val trials   = affected(d)
    val severity = d.level match
      case DiagnosticLevel.Error   => FindingSeverity.Blocker
      case DiagnosticLevel.Warning => FindingSeverity.Warning
    val people                     = trials.map(_.participant).distinct.mkString(", ")
    val inTrials                   = DiagnosticText.trialsAffectedIn(trials.size, people)
    def withCategory(text: String) =
      categoryOf(d).fold(text) { c =>
        val name = c match
          case FindingClass.UnavailableInput  => DiagnosticText(ClassUnavailableInput)
          case FindingClass.IncompatibleInput => DiagnosticText(ClassIncompatibleInput)
          case FindingClass.InvalidSetting    => DiagnosticText(ClassInvalidSetting)
          case FindingClass.DataDependent     => DiagnosticText(ClassDataDependent)
        DiagnosticText(CategoryAndCount, name, text)
      }
    val (title, detail) = wording.get(d.code) match
      case Some((t, Detail.References)) =>
        val focal      = d.subject.collectFirst { case DiagnosticLocus.Trial(k) => k }
        val references = trials.filterNot(focal.contains)
        (
          DiagnosticText(t),
          focal.fold(inTrials)(f =>
            DiagnosticText.referencesOf(
              references.size,
              f.label,
              references.map(_.occurrence.toString).mkString(", ")
            )
          )
        )
      case Some((t, Detail.InTrials)) => (DiagnosticText(t), inTrials)
      case Some((t, Detail.Counted))  =>
        (DiagnosticText(t), withCategory(DiagnosticText.trialsAffected(trials.size)))
      case None => (d.code, DiagnosticText(UnknownCode, inTrials))
    FindingVM(d.code, severity, title, detail, trials, remedyVM(d, trials))

  // A remedy names the action and opens exactly the affected trials; one
  // that names no trial has nothing to open.
  private def remedyVM(d: StudioDiagnostic, trials: Vector[TrialKey]): Option[RemedyVM] =
    import DiagnosticTextId.*
    Option.when(trials.nonEmpty) {
      val label = remedyOf(d) match
        case Some(Remedy.ChooseMatchedReference)      => DiagnosticText(ChooseOccurrence)
        case Some(Remedy.ReviewAnalysisWindow)        => DiagnosticText(ReviewWindow)
        case Some(Remedy.ResolveDuplicateTrials)      => DiagnosticText(ResolveDuplicates)
        case Some(Remedy.ResolveMatchItemConflict)    => DiagnosticText(ResolveItemConflict)
        case Some(Remedy.ReviseInitialFixationPolicy) => DiagnosticText(ReviseInitialFixations)
        case _                                        => DiagnosticText(OpenTrials)
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
