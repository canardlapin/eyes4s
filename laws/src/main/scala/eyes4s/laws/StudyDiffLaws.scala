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

package eyes4s.laws

import eyes4s.kernel.*
import eyes4s.plan.*

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Prop}
import org.typelevel.discipline.Laws

/** Laws for a structural plan diff and the revision that applies it.
  *
  * `between` gives the typed changes from one plan to another and `revise`
  * applies changes to a plan. Over generated pairs of plans:
  *
  *  - the diff of a plan with itself is empty, and a diff is empty exactly
  *    when the two plans describe the same study;
  *  - the diff is antisymmetric: the diff in the other direction is the
  *    inverse of every change;
  *  - the diff lists at most one change per field, in [[StudyField]] order;
  *  - applying the diff of `a` to `b` to `a` gives a plan equal to `b`,
  *    with an empty diff to it; and
  *  - a change that does not start from the plan's value is refused.
  *
  * Pairs should mix independent plans, which differ in most fields, with a
  * plan and a variant that changes a few, so each field's change is
  * exercised alone as well as together with others.
  */
object StudyDiffLaws extends Laws:
  type Between[K, U <: Unit2D, P, S, D] =
    (StudyPlan[K, U, P, S, D], StudyPlan[K, U, P, S, D]) => Vector[StudyChange[K, U, P, S, D]]
  type Revise[K, U <: Unit2D, P, S, D] =
    (
        StudyPlan[K, U, P, S, D],
        Vector[StudyChange[K, U, P, S, D]]
    ) => Either[StudyRevisionError, StudyPlan[K, U, P, S, D]]

  def shipped[K, U <: Unit2D, P, S, D]: (Between[K, U, P, S, D], Revise[K, U, P, S, D]) =
    (StudyDiff.between(_, _), (plan, changes) => plan.revise(changes))

  def diff[K, U <: Unit2D, P, S, D](
      between: Between[K, U, P, S, D],
      revise: Revise[K, U, P, S, D],
      pairs: Gen[(StudyPlan[K, U, P, S, D], StudyPlan[K, U, P, S, D])]
  )(using UnitLabel[U]): RuleSet =
    def show(changes: Vector[StudyChange[K, U, P, S, D]]) = StudyDiff.render(changes)
    new SimpleRuleSet(
      "studyDiff",
      "the diff of a plan with itself is empty" -> forAll(pairs) { case (a, b) =>
        Prop(between(a, a).isEmpty) :| show(between(a, a)) &&
        Prop(between(b, b).isEmpty) :| show(between(b, b))
      },
      "a diff is empty exactly when the plans describe the same study" -> forAll(pairs) {
        case (a, b) =>
          Prop(between(a, b).isEmpty == (a.description == b.description)) :| show(between(a, b))
      },
      "the diff is antisymmetric" -> forAll(pairs) { case (a, b) =>
        Prop(between(b, a) == between(a, b).map(_.inverse)) :|
          s"${show(between(a, b))} / ${show(between(b, a))}"
      },
      "a diff has one change per field, in field order" -> forAll(pairs) { case (a, b) =>
        val ordinals = between(a, b).map(_.field.ordinal)
        Prop(ordinals.zip(ordinals.drop(1)).forall(_ < _)) :| show(between(a, b))
      },
      "applying the diff of a to b to a gives b" -> forAll(pairs) { case (a, b) =>
        revise(a, between(a, b)) match
          case Left(e)        => Prop(false) :| e.message
          case Right(revised) =>
            Prop(revised.description == b.description) :| show(between(revised, b)) &&
            Prop(between(revised, b).isEmpty)
      },
      "a change that does not start from the plan's value is refused" -> forAll(pairs) {
        case (a, b) =>
          val changes = between(a, b)
          if changes.isEmpty then Prop(revise(b, changes).isRight)
          else
            revise(b, changes) match
              case Left(StudyRevisionError.Stale(field, _, _)) =>
                Prop(changes.exists(_.field == field))
              case other => Prop(false) :| s"applied a stale revision: $other"
      }
    )
