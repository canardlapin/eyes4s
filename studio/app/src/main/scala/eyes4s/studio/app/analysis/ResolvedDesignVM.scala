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

import eyes4s.studio.app.text.{DesignText, DesignTextId, Format}
import eyes4s.studio.app.vm.{A11yRole, FocusStop}
import eyes4s.studio.core.backend.{Eligibility, PreviewRow, TrialDisposition, TrialKey}
import eyes4s.studio.core.selection.{DesignCount, StudioRef}

/** A filter chip: its label, the backend count it shows ("—" when the
  * recipe has no such category, "…" before the count arrives), `ref`, the
  * count's [[StudioRef.DesignTally]], and whether it can be chosen.
  */
final case class ChipVM(
    filter: DesignFilter,
    label: String,
    count: String,
    ref: Option[StudioRef],
    enabled: Boolean,
    on: Boolean
) derives CanEqual

/** How a status reads: plainly, or as a warning (a query that does not enter
  * the comparisons).
  */
enum StatusTone derives CanEqual:
  case Plain, Warning

/** One row of the table. `ref` is the row's query trial: the row opens it,
  * and its numbers describe it.
  */
final case class DesignRowVM(
    query: TrialKey,
    participant: String,
    trial: String,
    item: String,
    matched: Option[String],
    controls: Option[String],
    status: String,
    tone: StatusTone,
    ref: StudioRef,
    focused: Boolean,
    accessible: String
) derives CanEqual

/** A line under the table and the counts its numbers are. */
final case class DesignNoteVM(text: String, refs: Vector[StudioRef], mono: Boolean)
    derives CanEqual

/** Everything the resolved-design pane shows, in the board's order. */
final case class ResolvedDesignVM(
    title: String,
    empty: Option[String],
    chips: Vector[ChipVM],
    mode: String,
    exact: Boolean,
    columns: Vector[String],
    rows: Vector[DesignRowVM],
    cursor: Option[Int],
    rowsNote: Option[String],
    counting: Option[DesignNoteVM],
    notes: Vector[DesignNoteVM],
    problem: Option[String],
    accessible: String,
    accessibleHelp: String
) derives CanEqual:

  /** The pane's focus stops after its own, in Tab order: each chip that can
    * be chosen, then the table, one stop with its row cursor.
    */
  def focusStops: Vector[FocusStop] =
    chips.filter(_.enabled).map(c => FocusStop(A11yRole.ToggleButton, c.label)) :+
      FocusStop(A11yRole.Table, accessible)

object ResolvedDesignVM:
  import DesignTextId.*

  private def t(id: DesignTextId, args: String*): String = DesignText(id, args*)

  def of(panel: ResolvedDesign): ResolvedDesignVM =
    val revision                                     = panel.target.map(_.revision)
    val candidates                                   = panel.preview.knownCandidates
    val ready                                        = panel.preview.receipt
    def tally(count: DesignCount): Option[StudioRef] =
      revision.map(StudioRef.DesignTally(_, count))
    def known(value: Option[Long], count: DesignCount): (String, Option[StudioRef]) =
      value.fold((t(Pending), None))(v => (Format.count(v), tally(count)))

    val chips = DesignFilter.values.toVector.map { f =>
      val (label, (count, ref), enabled) = f match
        case DesignFilter.All =>
          (
            t(ChipAll),
            known(
              candidates.map(_.requestedQueries.value.toLong),
              DesignCount.RequestedQueries
            ),
            true
          )
        case DesignFilter.Eligible =>
          (
            t(ChipEligible),
            known(
              ready.map(_.counts.eligibleQueries.value.toLong),
              DesignCount.EligibleQueries
            ),
            true
          )
        case DesignFilter.NoMatch =>
          (
            t(ChipNoMatch),
            known(
              ready.map(_.counts.unmatchedQueries.value.toLong),
              DesignCount.UnmatchedQueries
            ),
            true
          )
        case DesignFilter.NotAdmitted =>
          (
            t(ChipNotAdmitted),
            known(
              candidates.map(_.queriesNotAdmitted.value.toLong),
              DesignCount.QueriesNotAdmitted
            ),
            true
          )
        case DesignFilter.ByDesign =>
          candidates.map(_.byDesignQueries) match
            // The recipe has no by-design category: not applicable, not zero.
            case Some(None)    => (t(ChipByDesign), (t(NotApplicable), None), false)
            case Some(Some(n)) =>
              (t(ChipByDesign), known(Some(n.value.toLong), DesignCount.ByDesignQueries), true)
            case None => (t(ChipByDesign), (t(Pending), None), false)
      ChipVM(f, label, count, ref, enabled, panel.filter == f)
    }

    val (mode, exact) = panel.preview match
      case DesignPreview.Ready(_)                        => (t(ModeExact), true)
      case DesignPreview.Counting(_, _, _, _)            => (t(ModeCounting), false)
      case DesignPreview.Preparing                       => (t(ModePreparing), false)
      case DesignPreview.Idle | DesignPreview.Refused(_) => (t(ModeRefused), false)

    val visible = panel.visible
    val cursor  = panel.cursorIndex
    val rows    = visible.zipWithIndex.map((r, i) => row(r, cursor.contains(i)))

    val counting = panel.preview match
      case DesignPreview.Counting(_, _, c, progress) =>
        val done = progress.fold(0)(_.completedParticipants)
        // The total is the candidates' participant count; the done count is
        // progress, as a job's meter is.
        Some(
          DesignNoteVM(
            t(CountingChip, Format.count(done.toLong), Format.count(c.participants.toLong)),
            tally(DesignCount.Participants).toVector,
            mono = false
          )
        )
      case _ => None

    val candidateNote = candidates.map { c =>
      DesignNoteVM(
        t(
          CandidatesNote,
          Format.count(c.candidatePairsPerScale),
          Format.count(c.focalTrials.toLong),
          Format.count(c.referenceTrials.toLong)
        ),
        Vector(
          DesignCount.CandidatePairsPerScale,
          DesignCount.FocalTrials,
          DesignCount.ReferenceTrials
        ).flatMap(tally),
        mono = false
      )
    }
    val exactNote = ready.map { r =>
      DesignNoteVM(
        t(ExactNote, Format.count(r.counts.eligiblePairsPerScale)),
        tally(DesignCount.EligiblePairsPerScale).toVector,
        mono = false
      )
    }
    val stamp = panel.preview match
      case DesignPreview.Counting(_, s, _, _) => Some(s)
      case DesignPreview.Ready(r)             => Some(r.stamp)
      case _                                  => None
    val stampNote = stamp.map { s =>
      DesignNoteVM(
        t(StampNote, s.input.render, s.plan.render, s.label),
        Vector.empty,
        mono = true
      )
    }
    val countingNote = counting.map(c => DesignNoteVM(t(CountingNote, c.text), c.refs, false))
    val same         = ready.map(_ => DesignNoteVM(t(SameDesign), Vector.empty, mono = false))

    val rowsNote = panel.rowState match
      case DesignRows.Waiting        => Some(t(RowsWaiting))
      case DesignRows.Failed(reason) => Some(t(RowsFailed, reason))
      case DesignRows.Complete if panel.target.isDefined && visible.isEmpty =>
        Some(t(EmptyFilter))
      case DesignRows.Complete => None

    val problem = panel.preview match
      case DesignPreview.Refused(reason) => Some(t(PreviewFailed, reason))
      case _                             => None

    ResolvedDesignVM(
      title = t(Title),
      empty = Option.when(panel.target.isEmpty)(t(NoRevision)),
      chips = chips,
      mode = mode,
      exact = exact,
      columns =
        Vector(ColParticipant, ColQuery, ColItem, ColMatched, ColControls, ColStatus).map(t(_)),
      rows = rows,
      cursor = cursor,
      rowsNote = rowsNote,
      counting = counting,
      notes = (candidateNote ++ exactNote ++ countingNote ++ same ++ stampNote).toVector,
      problem = problem,
      accessible = revision.fold(t(NoRevision))(r => t(Accessible, r.label)),
      accessibleHelp = t(AccessibleHelp, chips.head.count)
    )

  /** The row of `r`: its matched reference and controls only when it enters
    * the comparisons, and its status in the board's words.
    */
  def row(r: PreviewRow, focused: Boolean): DesignRowVM =
    val eligible       = r.eligibility == Eligibility.Eligible
    val (status, tone) = r.eligibility match
      case Eligibility.Eligible            => (t(StatusEligible), StatusTone.Plain)
      case Eligibility.NoMatch(diagnostic) =>
        val text =
          if diagnostic.message.isEmpty then t(StatusNoMatch)
          else t(StatusNoMatchBecause, diagnostic.message)
        (text, StatusTone.Warning)
      case Eligibility.QueryNotAdmitted(d) =>
        (t(StatusNotAdmitted, disposition(d)), StatusTone.Warning)
    DesignRowVM(
      query = r.query,
      participant = r.query.participant,
      trial = r.query.trial,
      item = r.item,
      matched = Option.when(eligible)(r.matched.trial),
      controls = if eligible then r.controls.map(c => Format.count(c.toLong)) else None,
      status = status,
      tone = tone,
      ref = StudioRef.Trial(r.query),
      focused = focused,
      accessible = t(RowAccessible, r.query.label, r.item, status)
    )

  private def disposition(d: TrialDisposition): String = d match
    case TrialDisposition.Admitted       => t(DispositionAdmitted)
    case TrialDisposition.Absent         => t(DispositionAbsent)
    case TrialDisposition.NoFixations    => t(DispositionNoFixations)
    case TrialDisposition.Quarantined(c) => t(DispositionQuarantined, c.message)
