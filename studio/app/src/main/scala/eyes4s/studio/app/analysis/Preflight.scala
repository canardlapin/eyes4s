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

package eyes4s.studio.app.analysis

import eyes4s.studio.app.diagnostics.{DiagnosticsPresenter, DiagnosticsVM}
import eyes4s.studio.app.text.{
  DiagnosticText,
  Format,
  PreflightText,
  PreflightTextId,
  SourcesText,
  SourcesTextId
}
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.RecipeChange
import eyes4s.studio.core.freshness.FreshnessText
import eyes4s.studio.core.selection.{DesignCount, SelectionError, StudioRef, ViewId}

/** A value of the run card, with the refs its numbers trace to. */
final case class RunLine(label: String, value: String, refs: Vector[StudioRef]) derives CanEqual

/** The run card: its lines, the Save & run button's label and whether it is
  * enabled, and the reason when it is not, or the verdict when it is.
  */
final case class RunCardVM(
    lines: Vector[RunLine],
    button: String,
    enabled: Boolean,
    reason: Option[String],
    run: Option[Intent],
    verdict: String
) derives CanEqual

/** What the preflight pane shows (ticket S7.6; Analysis.dc.html, preflight):
  * the checked revision's findings, eyes4s's and the studio's apart, what is
  * not checked, and the run card.
  */
final case class PreflightVM(
    status: Option[String],
    report: String,
    findings: Option[DiagnosticsVM],
    notChecked: String,
    card: RunCardVM
) derives CanEqual

/** The preflight pane over the resolved design's preview (ticket S7.6): the
  * findings of its ready receipt, presented by [[DiagnosticsPresenter]], and
  * the run card. Save & run is enabled only for the draft, once its design is
  * checked and no finding blocks; otherwise the card says why. Every count is
  * the backend's. Pure.
  */
object Preflight:

  /** The view the pane's remedies select from. */
  val viewId: Either[SelectionError, ViewId] = ViewId.of("analysis.preflight")

  def vm(design: ResolvedDesign, m: AppModel): PreflightVM =
    import PreflightTextId.*
    val target   = design.target
    val label    = target.fold("")(_.revision.label)
    val isDraft  = target.exists(t => m.document.draft.exists(_.id == t.revision))
    val findings = design.preview.receipt.map(r =>
      DiagnosticsPresenter.grouped(DiagnosticsPresenter.present(r.diagnostics))
    )
    val status = (target, design.preview) match
      case (None, _)                             => Some(PreflightText(NothingToCheck))
      case (Some(_), DesignPreview.Refused(why)) => Some(PreflightText(Refused, label, why))
      case _                                     => None
    // The run card's lines: pair rows per scale, the scales and the total;
    // and what the draft changes against its base.
    val counts = design.preview.receipt.map(_.counts)
    // The scales the target recipe declares; the receipt is of that recipe.
    val scales = target.map(_.recipe.scales.values.size)
    val pairs  = for
      t <- target
      c <- counts
      n <- scales
    yield RunLine(
      PreflightText(PairRows),
      PreflightText(
        PairRowsValue,
        Format.count(c.eligiblePairsPerScale),
        n.toString,
        Format.count(c.eligiblePairs)
      ),
      Vector(
        StudioRef.DesignTally(t.revision, DesignCount.EligiblePairsPerScale),
        StudioRef.DesignTally(t.revision, DesignCount.Scales),
        StudioRef.DesignTally(t.revision, DesignCount.EligiblePairs)
      )
    )
    val change = for
      t       <- target
      context <- m.document.draftContext.filter(_.id == t.revision)
      base    <- context.savedBase
    yield
      val changes = RecipeChange.between(base.recipe, t.recipe)
      RunLine(
        PreflightText(ChangeVs, base.id.label),
        if changes.isEmpty then PreflightText(NoChange) else FreshnessText.describe(changes),
        Vector.empty
      )
    val blockers = findings.fold(0)(_.blockers)
    val reason   = (target, design.preview) match
      case (None, _)                => Some(PreflightText(NothingToCheck))
      case (Some(t), _) if !isDraft => Some(PreflightText(NoDraft, t.revision.label))
      // A changed or missing source blocks the run until repaired or replaced (S2.5).
      case (Some(_), _) if m.runBlock.isDefined =>
        m.runBlock.map((dataset, block) => SourcesText.blocked(dataset, block))
      case (Some(_), DesignPreview.Refused(why)) => Some(PreflightText(Refused, label, why))
      case (Some(_), DesignPreview.Counting(_, _, _, Some(p))) =>
        Some(
          PreflightText(
            CheckingProgress,
            label,
            p.completedParticipants.toString,
            p.totalParticipants.toString
          )
        )
      case (Some(_), DesignPreview.Ready(_)) if blockers > 0 =>
        Some(PreflightText(Blocked, DiagnosticText.blockers(blockers)))
      case (Some(_), DesignPreview.Ready(_)) => None
      case (Some(_), _)                      => Some(PreflightText(Checking, label))
    val button = counts.fold(PreflightText(RunButtonUncounted, label))(c =>
      PreflightText(RunButton, label, Format.count(c.eligiblePairs))
    )
    val verdict = findings.fold("")(f =>
      PreflightText(
        Verdict,
        if f.runnable then PreflightText(Ready)
        else PreflightText(BlockedShort),
        DiagnosticText.blockers(f.blockers),
        DiagnosticText.warnings(f.warnings)
      )
    )
    // A Save & run waiting for its check can be called off from the card (S2.5).
    val waiting = m.checks.runAfter.isDefined
    PreflightVM(
      status,
      PreflightText(StudyReport, label),
      findings,
      PreflightText(NotChecked),
      if waiting then
        RunCardVM(
          pairs.toVector ++ change.toVector,
          SourcesText(SourcesTextId.CancelWaiting),
          true,
          Some(SourcesText(SourcesTextId.RunWaiting)),
          Some(Intent.CancelWaitingRun),
          verdict
        )
      else
        RunCardVM(
          pairs.toVector ++ change.toVector,
          button,
          reason.isEmpty,
          reason,
          Option.when(reason.isEmpty)(Intent.Dispatch(Command.SaveAndRun(None))),
          verdict
        )
    )
