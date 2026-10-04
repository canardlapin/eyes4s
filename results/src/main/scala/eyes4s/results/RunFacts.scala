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

package eyes4s.results

import cats.syntax.all.*
import eyes4s.kernel.Unit2D
import eyes4s.plan.{
  ControlCount,
  Fact,
  FactCode,
  FactError,
  FactSlot,
  FactSource,
  FactValue,
  QueryTotal,
  StudyResult
}

/** The design facts of a run's methods text (CR6d), read from one scale's
  * query table and, for the controls, the run's control reductions. Each
  * fact's source is the run total it states (`FactSource.Run`).
  */
object RunFacts:
  /** The query totals of `table`, by each query's stored contrast row:
    *
    *   - `ContributingQueries`: a scored difference;
    *   - `FailedQueries`, and `FailureCause(code)` for each failure code: a
    *     stored failure;
    *   - `EligibleQueries`: the compared queries, contributing or failed;
    *   - `UnmatchedQueries`: no stored contrast row, because the pairing found
    *     no matched reference.
    *
    * The requested and not-admitted queries are counted against the trial
    * inventory, which a query table does not hold; a host states them.
    */
  def of[K](table: QueryTable[K]): Either[FactError, Vector[Fact]] =
    val outcomes = table.queries.map(_.difference)
    val failures = outcomes.collect { case RoleOutcome.Failed(code, _) => code.render }
    val scored   = outcomes.count {
      case RoleOutcome.Scored(_) => true
      case _                     => false
    }
    def total(slot: FactSlot, kind: QueryTotal, n: Long) =
      Fact.of(slot, FactSource.Run(kind), FactValue.Count(n))
    for
      codes <- failures.distinct.sorted.traverse(FactCode.of)
      head  <- Vector(
        total(FactSlot.EligibleQueries, QueryTotal.Eligible, (scored + failures.size).toLong),
        total(FactSlot.ContributingQueries, QueryTotal.Contributing, scored.toLong),
        total(FactSlot.FailedQueries, QueryTotal.Failed, failures.size.toLong)
      ).sequence
      causes <- codes.traverse(code =>
        total(
          FactSlot.FailureCause(code),
          QueryTotal.Failure(code),
          failures.count(_ == code.value).toLong
        )
      )
      unmatched <- total(
        FactSlot.UnmatchedQueries,
        QueryTotal.Unmatched,
        (outcomes.size - scored - failures.size).toLong
      )
    yield head ++ causes :+ unmatched

  /** The controls of the compared queries of `table` (contributing or
    * failed): how many queries had each number of controls, from `selected`,
    * the control pairs each query's control reduction selected (none when it
    * has no reduction). Why a query had fewer is not known here. No compared
    * query, no fact.
    */
  def controls[K](
      table: QueryTable[K],
      selected: K => Option[Int]
  ): Either[FactError, Option[Fact]] =
    val compared = table.queries.filter(_.difference match
      case RoleOutcome.NotStored => false
      case _                     => true)
    val counts = compared
      .groupMapReduce(q => selected(q.key).getOrElse(0))(_ => 1L)(_ + _)
      .toVector
      .sortBy(-_._1)
      .map((controls, queries) => ControlCount(controls, queries, None))
    Option
      .when(counts.nonEmpty)(counts)
      .traverse(cs =>
        Fact.of(
          FactSlot.ControlsPerQuery,
          FactSource.Run(QueryTotal.ControlsPerQuery),
          FactValue.Controls(cs)
        )
      )

  /** The facts of `table` ([[of]]) and its controls ([[controls]]) from the
    * control reductions of the same scale of `result`.
    */
  def study[K, U <: Unit2D, S, D](
      result: StudyResult[K, U, S, D],
      table: QueryTable[K]
  ): Either[FactError, Vector[Fact]] =
    val selected = result.scales
      .lift(table.scale)
      .fold(Map.empty[K, Int])(_.analyses.control.entries.map(r => r.key -> r.selected).toMap)
    for
      totals   <- of(table)
      controls <- controls(table, selected.get)
    yield totals ++ controls.toVector
