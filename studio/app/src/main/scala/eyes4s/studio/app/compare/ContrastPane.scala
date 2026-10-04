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

package eyes4s.studio.app.compare

import eyes4s.studio.app.Intent
import eyes4s.studio.app.nav.Place
import eyes4s.studio.app.vm.{A11yRole, FocusStop}
import eyes4s.studio.app.plot.{LadderColumns, LadderScale, PlotSource, ScaleLadder}
import eyes4s.studio.app.text.{ContrastText, ContrastTextId, Format}
import eyes4s.studio.core.backend.{PairDesign, RunId, TrialKey}
import eyes4s.studio.core.selection.StudioRef

/** The backend's answer for a query's scale ladder. */
enum LadderAnswer derives CanEqual:
  case Answered(ladder: ScaleLadder)
  case Failed(reason: String)

enum ContrastEffect derives CanEqual:
  /** Read the ladder of `query` in `run` at the run's scales. */
  case LoadLadder(run: RunId, query: TrialKey)

/** A value of the readout and the ref it traces to. */
final case class ReadoutValue(label: String, value: String, ref: StudioRef) derives CanEqual

/** The inspected pair: its role and item, score, what it is, where it ranks,
  * and the intents that step to the previous and next reference.
  */
final case class InspectedVM(
    heading: String,
    score: ReadoutValue,
    note: String,
    rank: String,
    prev: Option[Intent],
    next: Option[Intent]
) derives CanEqual

/** What the contrast pane shows (ticket S8.3): the ladder's source and the
  * scale it focuses, and the readout: caption, hero D, M and B, the confound
  * sentence, and the inspected pair.
  */
final case class ContrastVM(
    status: Option[String],
    retry: Option[String],
    ladder: Option[PlotSource],
    focusScale: Option[String],
    caption: String,
    hero: Option[ReadoutValue],
    m: Option[ReadoutValue],
    b: Option[ReadoutValue],
    confound: String,
    inspected: Option[InspectedVM]
) derives CanEqual

/** Compare's contrast pane (ticket S8.3; Main.dc.html, contrast): the
  * focused query's scale ladder, read from the backend once per query, and
  * the readout at the focused scale. Every number is the backend's: M is the
  * matched pair's score, B the control reduction (a mean over the control
  * pairs' cosines, never a cosine to an averaged map), D the contrast row.
  * Pure; a host performs the effects.
  */
final case class ContrastPane(key: Option[(RunId, TrialKey)], ladder: Option[LadderAnswer])
    derives CanEqual

object ContrastPane:

  val empty: ContrastPane = ContrastPane(None, None)

  /** Follows the panels' focus: a new query's ladder is read once. */
  def sync(s: ContrastPane, focus: Option[PanelFocus]): (ContrastPane, Vector[ContrastEffect]) =
    val key = focus.map(f => (f.run, f.query))
    if key == s.key then (s, Vector.empty)
    else
      (
        ContrastPane(key, None),
        key.toVector.map((run, query) => ContrastEffect.LoadLadder(run, query))
      )

  /** Read the focused query's ladder again after a failed read. */
  def retry(s: ContrastPane): (ContrastPane, Vector[ContrastEffect]) =
    (s.key, s.ladder) match
      case (Some((run, query)), Some(LadderAnswer.Failed(_))) =>
        (s.copy(ladder = None), Vector(ContrastEffect.LoadLadder(run, query)))
      case _ => (s, Vector.empty)

  /** A ladder read for `run` and `query`; kept only if still focused. */
  def read(s: ContrastPane, run: RunId, query: TrialKey, answer: LadderAnswer): ContrastPane =
    if s.key.contains((run, query)) then s.copy(ladder = Some(answer)) else s

  /** The references at `at`, in the order Prev and Next walk them: the
    * matched pair, then the controls by descending cosine (a control with
    * no served cosine last), ties by trial.
    */
  def ranked(at: LadderScale): Vector[StudioRef] =
    at.matched +: at.controls
      .sortBy(c => (c.cosine.fold(1)(_ => 0), c.cosine.fold(0.0)(-_), c.reference.trial))
      .map(_.ref)

  /** The controls inside the contrast pane's own stop after the ladder's:
    * Prev and Next, while each can step.
    */
  def focusStops(vm: ContrastVM): Vector[FocusStop] =
    vm.inspected.toVector.flatMap(i =>
      Vector(
        i.prev.map(_ => FocusStop(A11yRole.Button, ContrastText(ContrastTextId.PrevName))),
        i.next.map(_ => FocusStop(A11yRole.Button, ContrastText(ContrastTextId.NextName)))
      ).flatten
    )

  def vm(
      s: ContrastPane,
      focus: Option[PanelFocus],
      reference: Option[(PairDesign, TrialKey)],
      columns: LadderColumns
  ): ContrastVM =
    val confound                              = ContrastText(ContrastTextId.Confound)
    def bare(status: String, caption: String) =
      ContrastVM(Some(status), None, None, None, caption, None, None, None, confound, None)
    (focus, s.ladder) match
      case (None, _)       => bare(ContrastText(ContrastTextId.NoQuery), "")
      case (Some(f), None) =>
        bare(ContrastText(ContrastTextId.Reading, f.query.trial), "")
      case (Some(f), Some(LadderAnswer.Failed(why))) =>
        bare(ContrastText(ContrastTextId.Unreadable, f.query.trial, why), "")
          .copy(retry = Some(ContrastText(ContrastTextId.Retry)))
      case (Some(f), Some(LadderAnswer.Answered(ladder))) =>
        val at      = ladder.scales.find(_.scale == f.scale)
        val label   = at.fold(f.scale.value.toString)(_.label)
        val caption = ContrastText(ContrastTextId.Caption, f.query.trial, label)
        val source  = ScaleLadder.source(ladder, columns)
        // A scale the ladder lacks says so; the ladder is still drawn.
        val missing =
          Option.when(at.isEmpty)(ContrastText(ContrastTextId.NoScale, f.query.trial, label))
        ContrastVM(
          source.left.toOption.map(_.message).orElse(missing),
          None,
          source.toOption,
          at.map(_.label),
          caption,
          at.map(a =>
            ReadoutValue(ContrastText(ContrastTextId.D), Format.signed(a.d, 2), a.contrast)
          ),
          at.map(a =>
            ReadoutValue(ContrastText(ContrastTextId.M), Format.decimal(a.m, 2), a.matched)
          ),
          at.map(a =>
            ReadoutValue(
              ContrastText(ContrastTextId.B, a.members.toString),
              Format.decimal(a.b, 2),
              a.mean
            )
          ),
          confound,
          at.flatMap(a => inspected(a, f, reference))
        )

  private def inspected(
      at: LadderScale,
      f: PanelFocus,
      reference: Option[(PairDesign, TrialKey)]
  ): Option[InspectedVM] =
    val order = ranked(at)
    val shown = reference.map((d, key) => f.pair(d, key)).getOrElse(at.matched)
    order.indexOf(shown) match
      case -1 => None
      case i  =>
        val walk = (j: Int) => order.lift(j).map(r => Intent.Explain(Place.At(r)))
        // One count of controls throughout: B's members, as the backend
        // served them; the controls listed without a cosine are said apart.
        val members                      = at.members.toString
        val unscored                     = at.controls.count(_.cosine.isEmpty)
        val ranked                       = (at.controls.size - unscored).toString
        val (heading, score, note, rank) =
          if shown == at.matched then
            (
              ContrastText(ContrastTextId.RoleItem, PanelRole.Matched.label, at.matchedItem),
              Format.decimal(at.m, 2),
              ContrastText(ContrastTextId.MatchedNote),
              if unscored == 0 then ContrastText(ContrastTextId.MatchedRank, members)
              else ContrastText(ContrastTextId.MatchedRankUnscored, members, unscored.toString)
            )
          else
            val c      = at.controls.find(_.ref == shown)
            val cosine = c.flatMap(_.cosine)
            val role   = ContrastText(
              ContrastTextId.RoleItem,
              PanelRole.Control.label,
              c.flatMap(_.item).getOrElse(c.fold("")(_.reference.trial))
            )
            cosine match
              case Some(v) =>
                (
                  role,
                  Format.decimal(v, 2),
                  ContrastText(ContrastTextId.ControlNote, members),
                  ContrastText(ContrastTextId.ControlRank, i.toString, ranked)
                )
              case None =>
                (
                  role,
                  ContrastText(ContrastTextId.NotServed),
                  ContrastText(ContrastTextId.UnscoredNote, members),
                  ContrastText(ContrastTextId.UnscoredRank, ranked)
                )
        Some(
          InspectedVM(
            heading,
            ReadoutValue(ContrastText(ContrastTextId.Inspected), score, shown),
            note,
            rank,
            walk(i - 1),
            walk(i + 1)
          )
        )
