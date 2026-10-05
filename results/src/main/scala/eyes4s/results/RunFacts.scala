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
  * query table, the focal trials its matched pairing left unmatched and, for
  * the controls, the run's control reductions. Each fact's source is the run
  * total it states (`FactSource.Run`).
  */
object RunFacts:
  /** The query totals of `table`, given which of its queries the matched
    * pairing left `unmatched`:
    *
    *   - `UnmatchedQueries`: the table's `unmatched` focal trials (not
    *     eligible, so never among its queries, bead S0.7b), and any query
    *     `unmatched` names or without a stored contrast row;
    *   - `ContributingQueries`: any other query with a scored difference;
    *   - `FailedQueries`, and `FailureCause(code)` for each failure code: any
    *     other query with a stored failure;
    *   - `EligibleQueries`: the compared queries, contributing or failed.
    *
    * The requested and not-admitted queries are counted against the trial
    * inventory, which a query table does not hold; a host states them.
    */
  def of[K](table: QueryTable[K], unmatched: K => Boolean): Either[FactError, Vector[Fact]] =
    val compared = comparedQueries(table, unmatched).map(_.difference)
    val failures = compared.collect { case RoleOutcome.Failed(code, _) => code.render }
    val scored   = compared.size - failures.size
    def total(slot: FactSlot, kind: QueryTotal, n: Long) =
      Fact.of(slot, FactSource.Run(kind), FactValue.Count(n))
    for
      codes <- failures.distinct.sorted.traverse(FactCode.of)
      head  <- Vector(
        total(FactSlot.EligibleQueries, QueryTotal.Eligible, compared.size.toLong),
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
      left <- total(
        FactSlot.UnmatchedQueries,
        QueryTotal.Unmatched,
        (table.unmatched.size + table.queries.size - compared.size).toLong
      )
    yield head ++ causes :+ left

  /** The compared queries: matched, with a stored contrast row. */
  private def comparedQueries[K](table: QueryTable[K], unmatched: K => Boolean) =
    table.queries.filter(q =>
      !unmatched(q.key) && (q.difference match
        case RoleOutcome.NotStored => false
        case _                     => true)
    )

  /** The controls of the compared queries of `table` (matched, contributing
    * or failed): how many queries had each number of controls, from
    * `selected`, the control pairs each query's control reduction selected
    * (none when it has no reduction). Why a query had fewer is not known
    * here. No compared query, no fact.
    */
  def controls[K](
      table: QueryTable[K],
      unmatched: K => Boolean,
      selected: K => Option[Int]
  ): Either[FactError, Option[Fact]] =
    val counts = comparedQueries(table, unmatched)
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

  /** The facts of `table` ([[of]]) and its controls ([[controls]]), with the
    * unmatched focal trials of the matched pairing and the control
    * reductions of the same scale of `result`.
    */
  def study[K, U <: Unit2D, S, D](
      result: StudyResult[K, U, S, D],
      table: QueryTable[K]
  ): Either[FactError, Vector[Fact]] =
    val scale     = result.scales.lift(table.scale)
    val unmatched =
      scale.fold(Set.empty[K])(_.analyses.matchedSource.diagnostics.unmatchedLeft.toSet)
    val selected = scale
      .fold(Map.empty[K, Int])(_.analyses.control.entries.map(r => r.key -> r.selected).toMap)
    for
      totals   <- of(table, unmatched)
      controls <- controls(table, unmatched, selected.get)
    yield totals ++ controls.toVector
