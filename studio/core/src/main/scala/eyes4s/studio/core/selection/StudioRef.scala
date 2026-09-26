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

package eyes4s.studio.core.selection

import eyes4s.studio.core.backend.{PairDesign, ResultAddress, Response, RunId, TrialKey}
import eyes4s.studio.core.document.{FigureId, PanelLetter, ReportingId, SourceRole}
import io.circe.Codec

/** What a ref denotes (ticket S3.3). Selecting an aggregate never selects the
  * observations it summarizes, and selecting an observation never selects
  * the aggregates it contributes to.
  */
enum RefKind derives CanEqual:
  /** A unit of the study (a participant), neither measured nor derived. */
  case Entity

  /** A measured or row-level value: a trial, fixation, source record or pair
    * score.
    */
  case Observation

  /** A value computed over observations: a query contrast, a participant
    * summary, a group cell, a figure panel.
    */
  case Aggregate

/** A typed reference to anything a studio view can show, select or trace a
  * number to (ticket S3.3; DESIGN_SPEC section 3's trail grammar).
  *
  * Result refs are scoped by the run whose result they name and mirror eyes4s
  * `ResultRef` where eyes4s has a shape: [[StudioRef.Pair]] is `PairRow` and
  * [[StudioRef.QueryContrast]] is `ContrastRow` (see [[StudioRef.address]]).
  * Scales are scale indices, as in `ResultRef`. Reporting refs
  * ([[StudioRef.ParticipantSummary]], [[StudioRef.GroupCell]]) name the
  * reporting spec that defines them (UI-C). UI-G may refine these shapes.
  */
enum StudioRef derives CanEqual, Codec.AsObject:
  case Participant(participant: String)
  case Trial(key: TrialKey)

  /** The `index`-th fixation of a trial (1-based, "fixation 6"), as the fixation source orders them. */
  case Fixation(trial: TrialKey, index: Int)

  /** One record of a source file ("fixations.csv record 7,214"). A fixation
    * source record names its fixation; a trial-inventory record names none.
    */
  case SourceRecord(trial: TrialKey, fixation: Option[Int], source: SourceRole, record: Int)

  /** eyes4s `ResultRef.PairRow` of one run. */
  case Pair(run: RunId, scale: Int, design: PairDesign, focal: TrialKey, reference: TrialKey)

  /** eyes4s `ResultRef.ContrastRow` of one run: a query's M, B and D. */
  case QueryContrast(run: RunId, scale: Int, query: TrialKey)

  /** One participant's summary under a reporting spec, within a group when
    * the spec groups.
    */
  case ParticipantSummary(
      run: RunId,
      reporting: ReportingId,
      scale: Int,
      group: Option[Response],
      participant: String
  )

  /** One group's cell of a reporting spec's summary. */
  case GroupCell(run: RunId, reporting: ReportingId, scale: Int, group: Response)

  case FigurePanel(figure: FigureId, panel: PanelLetter)

  def kind: RefKind = this match
    case Participant(_) => RefKind.Entity
    case Trial(_) | Fixation(_, _) | SourceRecord(_, _, _, _) | Pair(_, _, _, _, _) =>
      RefKind.Observation
    case QueryContrast(_, _, _) | ParticipantSummary(_, _, _, _, _) | GroupCell(_, _, _, _) |
        FigurePanel(_, _) =>
      RefKind.Aggregate

  def isAggregate: Boolean = kind == RefKind.Aggregate

  /** The eyes4s result address, for the refs eyes4s has one for. */
  def address: Option[ResultAddress] = this match
    case Pair(_, scale, design, focal, reference) =>
      Some(ResultAddress.PairRow(scale, design, focal, reference))
    case QueryContrast(_, scale, query) => Some(ResultAddress.ContrastRow(scale, query))
    case _                              => None

  /** The structural container of this ref, derivable from the ref alone:
    *
    *  - participant ⊃ trial ⊃ fixation ⊃ fixation record, and trial ⊃
    *    inventory record;
    *  - query contrast ⊃ its pairs (same run and scale);
    *  - group cell ⊃ participant summary (same run, spec and scale).
    *
    * Which group a query belongs to depends on the data; a view that knows it
    * supplies it through [[Lineage]].
    */
  def parent: Option[StudioRef] = this match
    case Participant(_)                            => None
    case Trial(key)                                => Some(Participant(key.participant))
    case Fixation(trial, _)                        => Some(Trial(trial))
    case SourceRecord(trial, Some(fixation), _, _) => Some(Fixation(trial, fixation))
    case SourceRecord(trial, None, _, _)           => Some(Trial(trial))
    case Pair(run, scale, _, focal, _)             => Some(QueryContrast(run, scale, focal))
    case QueryContrast(_, _, _)                    => None
    case ParticipantSummary(run, reporting, scale, Some(group), _) =>
      Some(GroupCell(run, reporting, scale, group))
    case ParticipantSummary(_, _, _, None, _) => None
    case GroupCell(_, _, _, _)                => None
    case FigurePanel(_, _)                    => None

/** Where refs sit in a containment hierarchy: the structural one of
  * [[StudioRef.parent]], optionally extended by data a view holds (such as
  * which participant summary a query contrast contributes to). The graph must
  * be acyclic.
  */
trait Lineage:
  def parents(ref: StudioRef): Vector[StudioRef]

  /** Every container of `ref`, nearest first, each once. */
  final def ancestors(ref: StudioRef): Vector[StudioRef] =
    @annotation.tailrec
    def go(frontier: Vector[StudioRef], seen: Vector[StudioRef]): Vector[StudioRef] =
      val next = frontier.flatMap(parents).distinct.filterNot(seen.contains)
      if next.isEmpty then seen else go(next, seen ++ next)
    go(Vector(ref), Vector.empty)

  final def contains(container: StudioRef, ref: StudioRef): Boolean =
    ancestors(ref).contains(container)

object Lineage:
  /** Only the containment a ref states by itself. */
  val structural: Lineage = ref => ref.parent.toVector

  /** The structural lineage plus `extra` parents. */
  def extended(extra: StudioRef => Vector[StudioRef]): Lineage =
    ref => (ref.parent.toVector ++ extra(ref)).distinct
