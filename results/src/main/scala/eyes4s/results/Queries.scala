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

import eyes4s.plan.*

/** One role's stored value for a query: its score components, a failure the
  * result recorded, or no stored row at all.
  */
enum RoleOutcome derives CanEqual:
  /** The component values, in the query table's component order. */
  case Scored(components: Vector[Double])
  case Failed(code: DiagnosticCode, message: String)
  case NotStored

  def value(index: Int): Value[Double] = this match
    case Scored(values)        => Value.Present(values(index))
    case Failed(code, message) => Value.Missing(Absence.Failed(code, message))
    case NotStored             => Value.Missing(Absence.NotRecorded)

/** One focal query of a scale: its key and layout fields, the covariate and
  * window values a report may read, and each role's stored outcome. A query
  * is built from stored result rows; nothing is recomputed.
  */
final case class Query[K] private (
    key: K,
    participant: String,
    item: String,
    phase: String,
    occurrence: Int,
    covariates: Vector[(CovariateName, Value[CovariateValue])],
    unparsed: Vector[(CovariateName, String)],
    window: Vector[(WindowMeasure, Value[Double])],
    matched: RoleOutcome,
    control: RoleOutcome,
    difference: RoleOutcome
) derives CanEqual:
  def outcome(role: Role): RoleOutcome = role match
    case Role.Matched    => matched
    case Role.Control    => control
    case Role.Difference => difference

  def covariate(name: CovariateName): Value[CovariateValue] =
    covariates
      .collectFirst { case (`name`, v) => v }
      .getOrElse(Value.Missing(Absence.NotRecorded))

  def measure(m: WindowMeasure): Value[Double] =
    window.collectFirst { case (`m`, v) => v }.getOrElse(Value.Missing(Absence.NotRecorded))

  /** The same query under another participant label. */
  def relabel(participant: String): Query[K] = copy(participant = participant)

  /** The same query under another key. */
  def rekey[K2](key: K2): Query[K2] = copy(key = key)

  /** The address of this query's `role` row in scale `scale`. */
  def ref(role: Role, scale: Int): ResultRef[K] = role match
    case Role.Matched    => ResultRef.Reduction(scale, StudyDesign.Matched, key)
    case Role.Control    => ResultRef.Reduction(scale, StudyDesign.Control, key)
    case Role.Difference => ResultRef.ContrastRow(scale, key)

object Query:
  /** A query with a non-blank participant, a positive occurrence, finite
    * scored components and finite present window values.
    */
  def of[K](
      key: K,
      participant: String,
      item: String,
      phase: String,
      occurrence: Int,
      covariates: Vector[(CovariateName, Value[CovariateValue])],
      unparsed: Vector[(CovariateName, String)],
      window: Vector[(WindowMeasure, Value[Double])],
      matched: RoleOutcome,
      control: RoleOutcome,
      difference: RoleOutcome
  ): Either[ReportError[K], Query[K]] =
    val scores = Vector(matched, control, difference).collect { case RoleOutcome.Scored(v) =>
      v
    }.flatten
    val numbers = covariates.collect { case (_, Value.Present(CovariateValue.Number(d))) =>
      d
    } ++ window.collect { case (_, Value.Present(d)) => d }
    if participant.trim.isEmpty then Left(ReportError.BlankParticipant(key))
    else if occurrence < 1 then Left(ReportError.InvalidQuery(key, s"occurrence $occurrence"))
    else if !(scores ++ numbers).forall(_.isFinite) then
      Left(ReportError.InvalidQuery(key, "a non-finite value"))
    else
      Right(
        new Query(
          key,
          participant,
          item,
          phase,
          occurrence,
          covariates,
          unparsed,
          window,
          matched,
          control,
          difference
        )
      )

/** The focal queries of one scale, with the score components they carry and
  * the covariates they were joined with: what [[Report.reduce]] reads. Keys
  * are distinct and every scored role has one value per component.
  */
final class QueryTable[K] private (
    val scale: Int,
    val components: Vector[String],
    val covariates: CovariateSchema,
    val queries: Vector[Query[K]]
):
  /** The same queries in another order. */
  def reorder(
      order: Vector[Query[K]] => Vector[Query[K]]
  ): Either[ReportError[K], QueryTable[K]] =
    QueryTable.of(scale, components, covariates, order(queries))

object QueryTable:
  def of[K](
      scale: Int,
      components: Vector[String],
      covariates: CovariateSchema,
      queries: Vector[Query[K]]
  ): Either[ReportError[K], QueryTable[K]] =
    val keys = queries.map(_.key)
    keys.diff(keys.distinct).headOption match
      case Some(key) => Left(ReportError.DuplicateQuery(key))
      case None      =>
        queries
          .flatMap(q =>
            Role.values.toVector.flatMap(role =>
              q.outcome(role) match
                case RoleOutcome.Scored(v) if v.size != components.size =>
                  Some(ReportError.ComponentCount(q.key, components, v.size))
                case _ => None
            )
          )
          .headOption
          .toLeft(new QueryTable(scale, components, covariates, queries))
