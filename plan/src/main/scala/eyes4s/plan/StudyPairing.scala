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

package eyes4s.plan

import eyes4s.design.*
import eyes4s.kernel.*

/** Which presentation of an item a trial is: 1 for the first, and so on. */
opaque type TrialOccurrence = Int

object TrialOccurrence:
  val first: TrialOccurrence = 1

  def of(value: Int): Either[PlanError, TrialOccurrence] =
    if value >= 1 then Right(value) else Left(PlanError.InvalidOccurrence(value))

  extension (o: TrialOccurrence) def value: Int = o

  given CanEqual[TrialOccurrence, TrialOccurrence] = CanEqual.derived
  given KeyDigest[TrialOccurrence]                 = KeyDigest[Int].digest(_)

/** Which occurrence of an item a selection keeps. */
enum OccurrenceChoice derives CanEqual:
  case First, Last
  case At(occurrence: TrialOccurrence)

  def render: String = this match
    case First => "first"
    case Last  => "last"
    case At(n) => s"at:${n.value}"

/** How a focal trial's matched reference is chosen when the layout allows
  * more than one: exactly one is required (the default), the reference of the
  * same occurrence, one occurrence selected by rule, or, explicitly, the mean
  * over all of them (the version-1 meaning).
  */
enum MatchedReferences derives CanEqual:
  /** More than one matched reference refuses the study. */
  case RequireOne

  /** The reference whose occurrence is the focal trial's own. */
  case SameOccurrence

  /** One occurrence of each item, chosen by rule, for matches and controls. */
  case Select(choice: OccurrenceChoice)

  /** Every matched reference contributes to the focal trial's mean. */
  case MeanOfAll

  def render: String = this match
    case RequireOne     => "requireOne"
    case SameOccurrence => "sameOccurrence"
    case Select(c)      => s"select:${c.render}"
    case MeanOfAll      => "meanOfAll"

/** Which references form a focal trial's control pool. `SameSelection`
  * applies the matched-reference rule to every other item, so each
  * contributes exactly one reference per participant; `AllOccurrences` uses
  * every occurrence of every other item.
  */
enum ControlReferences derives CanEqual:
  case SameSelection, AllOccurrences

  /** The stable name the plan's description and codec use. */
  def name: String = this match
    case SameSelection  => "sameSelection"
    case AllOccurrences => "allOccurrences"

/** What a focal trial without a matched reference does: reported as no match
  * (the default), or refusing the study.
  */
enum UnmatchedFocalPolicy derives CanEqual:
  case ReportNoMatch, Refuse

  /** The stable name the plan's description and codec use. */
  def name: String = this match
    case ReportNoMatch => "reportNoMatch"
    case Refuse        => "refuse"

/** How a study pairs focal trials with matched and control references. */
final case class StudyPairing(
    matched: MatchedReferences,
    controls: ControlReferences,
    unmatched: UnmatchedFocalPolicy
) derives CanEqual:
  def isVersion1: Boolean = this == StudyPairing.version1

  /** True when some input could make this pairing refuse a study. */
  def canRefuse: Boolean =
    matched != MatchedReferences.MeanOfAll || unmatched == UnmatchedFocalPolicy.Refuse

  def description: Vector[(String, Vector[Provenance.Param])] =
    import Provenance.Param.*
    Vector(
      "pairing" -> Vector(
        Text(matched.render),
        Text(controls.name),
        Text(unmatched.name)
      )
    )

object StudyPairing:
  /** New plans: exactly one matched reference, controls selected alike, and
    * focal trials without a match reported.
    */
  val default: StudyPairing = StudyPairing(
    MatchedReferences.RequireOne,
    ControlReferences.SameSelection,
    UnmatchedFocalPolicy.ReportNoMatch
  )

  /** The version-1 meaning: every matched reference averaged, every other
    * reference a control, focal trials without a match reported.
    */
  val version1: StudyPairing = StudyPairing(
    MatchedReferences.MeanOfAll,
    ControlReferences.SameSelection,
    UnmatchedFocalPolicy.ReportNoMatch
  )

/** Whether a prepared study's matched references are well defined, computed
  * once from its exact matched schedule and the selected references.
  *
  *  - `multiple`: focal trials with more than one matched reference, each with
  *    its references in schedule order;
  *  - `ambiguousReferences`: groups of references that the pairing rule
  *    should reduce to one per participant and item (and occurrence under
  *    `SameOccurrence`) but does not, as the control pool uses them;
  *  - `unmatched`: focal trials without a matched reference;
  *  - `itemConflicts`: keys the layout identifies as one trial (participant,
  *    phase and trial label) that disagree on its occurrence or match item.
  *
  * The same value drives preflight, the refusal of `StudyWork` and the
  * preview.
  */
final class MatchedCardinality[K] private[plan] (
    val pairing: StudyPairing,
    val multiple: Vector[(K, Vector[K])],
    val ambiguousReferences: Vector[Vector[K]],
    val unmatched: Vector[K],
    val itemConflicts: Vector[Vector[K]]
):
  private def oneReference: Boolean = pairing.matched != MatchedReferences.MeanOfAll

  /** Groups whose ambiguity refuses the study under this pairing. */
  def blockingReferences: Vector[Vector[K]] =
    if oneReference && pairing.controls == ControlReferences.SameSelection then
      ambiguousReferences
    else Vector.empty

  def blocking: Boolean =
    itemConflicts.nonEmpty || (oneReference && multiple.nonEmpty) ||
      blockingReferences.nonEmpty ||
      (pairing.unmatched == UnmatchedFocalPolicy.Refuse && unmatched.nonEmpty)

  /** The refusal this value implies, naming the trials by key digest in
    * the layout's key order, so the value does not depend on input order.
    */
  def refusal(layout: StudyLayout[K]): Option[PlanError] =
    given Ordering[K]                          = layout.ordering
    def names(keys: Vector[K]): Vector[String] =
      keys.sorted.map(k => layout.digest.digest(k).render)
    def groups(gs: Vector[Vector[K]]): Vector[Vector[String]] =
      gs.map(_.sorted).sortBy(_.head).map(names)
    if itemConflicts.nonEmpty then Some(PlanError.MatchItemConflict(groups(itemConflicts)))
    else if oneReference && (multiple.nonEmpty || blockingReferences.nonEmpty) then
      Some(
        PlanError.MatchedCardinality(
          pairing.matched,
          names(multiple.map(_._1)),
          groups(blockingReferences)
        )
      )
    else if pairing.unmatched == UnmatchedFocalPolicy.Refuse && unmatched.nonEmpty then
      Some(PlanError.UnmatchedFocalRefused(names(unmatched)))
    else None
