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

import cats.syntax.all.*
import eyes4s.kernel.Provenance

/** A declared text attribute selection and its trial-identity assignments.
  * Assignments are retained in the plan, before admission, so restoring a plan
  * cannot turn a lure into a match when its item collides with a studied item.
  * They are scientific inputs, not classifications inferred from item names.
  */
sealed abstract case class ExpectedUnmatched private (
    column: String,
    values: Vector[String],
    assignments: Vector[(TrialIdentity, String)]
) derives CanEqual:
  private lazy val byTrial  = assignments.toMap
  private lazy val selected = assignments.collect {
    case (trial, value) if values.contains(value) => trial
  }.toSet

  def contains[K](layout: StudyLayout[K], key: K): Boolean =
    TrialIdentity.projection(layout).exists(identity => selected(identity(key)))

  /** Refuse missing/blank declarations rather than guessing that a probe is old. */
  def check[K](layout: StudyLayout[K], focal: Vector[K]): Either[PlanError, Unit] =
    TrialIdentity.projection(layout) match
      case None =>
        Left(
          PlanError.ExpectedUnmatchedSelection(
            column,
            layout.id.name,
            "layout does not declare trial identities"
          )
        )
      case Some(identity) =>
        focal
          .collectFirst {
            case key if byTrial.get(identity(key)).forall(_.trim.isEmpty) =>
              PlanError.ExpectedUnmatchedSelection(
                column,
                identity(key).render,
                "focal trial has no nonblank declaration"
              )
          }
          .toLeft(())

  def description: Vector[Provenance.Param] =
    import Provenance.Param.*
    Vector(Text(column), Num(values.size.toDouble)) ++ values.map(Text(_)) ++
      assignments.flatMap { (t, value) =>
        Vector(
          Text(t.participant),
          Text(t.phase),
          Text(t.trial),
          Num(t.occurrence.value.toDouble),
          Text(value)
        )
      }

object ExpectedUnmatched:
  private def apply(
      column: String,
      values: Vector[String],
      assignments: Vector[(TrialIdentity, String)]
  ): ExpectedUnmatched =
    new ExpectedUnmatched(column, values, assignments) {}

  /** Parse explicit declarations. Trial identities are unique; order is canonical.
    * Empty assignment values are retained and refused only when used as focal trials.
    */
  def of(
      column: String,
      values: Vector[String],
      assignments: Vector[(TrialIdentity, String)]
  ): Either[PlanError, ExpectedUnmatched] =
    def invalid(value: String, reason: String) = Left(
      PlanError.ExpectedUnmatchedSelection(column, value, reason)
    )
    if column.trim.isEmpty then invalid(column, "attribute column must be nonblank")
    else if values.isEmpty || values.exists(
        _.trim.isEmpty
      ) || values.distinct.size != values.size
    then
      invalid(
        values.mkString(", "),
        "expected-unmatched values must be nonempty, nonblank and distinct"
      )
    else if assignments.map(_._1).distinct.size != assignments.size then
      invalid(assignments.map(_._1.render).mkString(", "), "trial assignments must be unique")
    else Right(ExpectedUnmatched(column, values.sorted, assignments.sortBy(_._1)))

  /** Resolve the declared text column against every pre-admission inventory trial.
    * Admission dispositions never change the selection.
    */
  def fromInventory(
      column: String,
      values: Vector[String],
      inventory: InventoryLedger
  ): Either[PlanError, ExpectedUnmatched] =
    for
      _ <- Either.cond(
        inventory.attributeColumns.exists(c =>
          c.name == column && c.kind == AttributeKind.Text
        ),
        (),
        PlanError.ExpectedUnmatchedSelection(
          column,
          inventory.attributeColumns.map(_.name).mkString(", "),
          "selection requires a declared text attribute"
        )
      )
      assignments <- inventory.trials.traverse { trial =>
        trial.attributes.get(column) match
          case Some(AttributeValue.Text(value)) => Right(trial.identity -> value)
          case Some(AttributeValue.Blank)       => Right(trial.identity -> "")
          case _                                =>
            Left(
              PlanError.ExpectedUnmatchedSelection(
                column,
                trial.identity.render,
                "trial has no text attribute declaration"
              )
            )
      }
      selection <- of(column, values, assignments)
    yield selection

/** The reusable recipe declaration, rebound to the inventory of each dataset revision. */
sealed abstract case class ExpectedUnmatchedRule private (
    column: String,
    values: Vector[String]
) derives CanEqual:
  def bind(inventory: InventoryLedger): Either[PlanError, ExpectedUnmatched] =
    ExpectedUnmatched.fromInventory(column, values, inventory)

object ExpectedUnmatchedRule:
  private def apply(column: String, values: Vector[String]): ExpectedUnmatchedRule =
    new ExpectedUnmatchedRule(column, values) {}

  def of(column: String, values: Vector[String]): Either[PlanError, ExpectedUnmatchedRule] =
    ExpectedUnmatched
      .of(column, values, Vector.empty)
      .map(s => ExpectedUnmatchedRule(s.column, s.values))
