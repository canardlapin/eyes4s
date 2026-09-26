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

import scala.annotation.tailrec

/** The relations a pairing rule schedules, and the cardinality it implies.
  *
  * A selection by occurrence depends only on a reference's own participant
  * and item, so it is a relation on the reference: the keys the rule selects,
  * matched against a constant on the focal side. The same selection narrows
  * the control pool unless the plan asks for every occurrence.
  */
private[plan] object StudyPairingWork:
  def relations[K](
      layout: StudyLayout[K],
      pairing: StudyPairing,
      references: Vector[K]
  ): (Relation[K, K], Relation[K, K]) =
    val sameParticipant = Relation.sameOn(layout.participant)
    val matchedBase     = sameParticipant.and(Relation.sameOn(layout.stimulus))
    val controlBase     = sameParticipant.and(Relation.differentOn(layout.stimulus))
    val narrowing: Option[Relation[K, K]] = (pairing.matched, layout.occurrence) match
      case (MatchedReferences.SameOccurrence, Some(occurrence)) =>
        Some(Relation.sameOn(occurrence))
      case (MatchedReferences.Select(choice), Some(_)) =>
        val chosen = chosenReferences(layout, pairing, references).toSet
        Some(
          Relation.SameOn(
            Projection.named[K, Boolean]("selectable")(_ => true),
            Projection.named[K, Boolean](s"occurrence is ${choice.render}")(chosen.contains)
          )
        )
      case _ => None
    val controlNarrowing = pairing.controls match
      case ControlReferences.SameSelection  => narrowing
      case ControlReferences.AllOccurrences => None
    (
      narrowing.fold(matchedBase)(matchedBase.and),
      controlNarrowing.fold(controlBase)(controlBase.and)
    )

  /** The references a choice keeps: per participant and item, those of the
    * first, last or given occurrence.
    */
  def selected[K](
      layout: StudyLayout[K],
      occurrence: Projection[K, TrialOccurrence],
      choice: OccurrenceChoice,
      references: Vector[K]
  ): Set[K] =
    references
      .groupBy(k => (layout.participant(k), layout.stimulus(k)))
      .values
      .flatMap { group =>
        val occurrences = group.map(occurrence(_).value)
        val target      = choice match
          case OccurrenceChoice.First => occurrences.min
          case OccurrenceChoice.Last  => occurrences.max
          case OccurrenceChoice.At(n) => n.value
        group.filter(occurrence(_).value == target)
      }
      .toSet

  /** The references a pairing rule can draw on: every reference key that
    * occurs once (a repeated key is excluded from pairing), narrowed by a
    * `Select` rule to its chosen occurrence of each item. The schedule's
    * relations and the cardinality both use this one definition.
    */
  def chosenReferences[K](
      layout: StudyLayout[K],
      pairing: StudyPairing,
      references: Vector[K]
  ): Vector[K] =
    val counts = references.groupMapReduce(identity)(_ => 1)(_ + _)
    val unique = references.filter(counts(_) == 1)
    (pairing.matched, layout.occurrence) match
      case (MatchedReferences.Select(choice), Some(o)) =>
        val kept = selected(layout, o, choice, unique)
        unique.filter(kept.contains)
      case _ => unique

  /** Keys the layout identifies as one trial (participant, phase and trial
    * label) that disagree on its occurrence or its match item, each conflict
    * in the layout's key order.
    */
  def itemConflicts[K](layout: StudyLayout[K], keys: Vector[K]): Vector[Vector[K]] =
    given Ordering[K] = layout.ordering
    layout.trial.toVector.flatMap { trial =>
      keys.distinct
        .groupBy(k => (layout.participant(k), layout.phase(k), trial(k)))
        .values
        .collect {
          case group
              if group
                .map(k => (layout.stimulus(k), layout.occurrence.map(o => o(k).value)))
                .distinct
                .size > 1 =>
            group.sorted
        }
        .toVector
        .sortBy(_.head)
    }

  /** Every matched pair of the schedule, by focal key in schedule order. */
  private def matchedPairs[K](
      schedule: DirectedPairSchedule[K, K]
  ): Either[PairScheduleError, (Vector[(K, K)], PairingReport[K, K])] =
    @tailrec
    def loop(
        cursor: PairCursor[K, K],
        acc: Vector[(K, K)]
    ): Either[PairScheduleError, (Vector[(K, K)], PairingReport[K, K])] =
      cursor.advance(PairQuantum.default) match
        case Left(e)                                => Left(e)
        case Right(PairPage.Done(pairs, _, report)) =>
          Right((acc ++ pairs.map(p => p.left -> p.right), report))
        case Right(PairPage.More(pairs, _, next)) =>
          loop(next, acc ++ pairs.map(p => p.left -> p.right))
    loop(schedule.start, Vector.empty)

  def cardinality[K](
      layout: StudyLayout[K],
      pairing: StudyPairing,
      keys: Vector[K],
      references: Vector[K],
      matched: DirectedPairSchedule[K, K]
  ): Either[PlanError, MatchedCardinality[K]] =
    matchedPairs(matched).left.map(PlanError.Schedule.apply).map { (pairs, report) =>
      val byFocal  = pairs.groupBy(_._1)
      val multiple = pairs
        .map(_._1)
        .distinct
        .collect { case k if byFocal(k).size > 1 => k -> byFocal(k).map(_._2) }
      val chosen   = chosenReferences(layout, pairing, references)
      val grouping = (pairing.matched, layout.occurrence) match
        case (MatchedReferences.SameOccurrence, Some(o)) =>
          (k: K) => (layout.participant(k), layout.stimulus(k), Some(o(k).value))
        case _ => (k: K) => (layout.participant(k), layout.stimulus(k), None)
      val order     = chosen.zipWithIndex.toMap
      val ambiguous =
        if pairing.matched == MatchedReferences.MeanOfAll then Vector.empty
        else
          chosen
            .groupBy(grouping)
            .values
            .collect { case group if group.size > 1 => group }
            .toVector
            .sortBy(g => order(g.head))
      new MatchedCardinality(
        pairing,
        multiple,
        ambiguous,
        report.unmatchedLeft,
        itemConflicts(layout, keys)
      )
    }
