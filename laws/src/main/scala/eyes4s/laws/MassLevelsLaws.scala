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

import org.scalacheck.{Gen, Prop}
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

/** Laws for a highest-density-region selector.
  *
  * `select` is an observation adapter so downstream implementations and the
  * deliberate mutants in this module's conformance suite can be tested without
  * making [[MassLevel]] constructible outside the kernel.
  */
trait MassLevelsLaws extends Laws:

  final case class Observed(
      coverage: Double,
      threshold: Double,
      coveredMass: Double,
      cells: Int
  )

  type Selector[U <: Unit2D] =
    (Mass[U], Vector[Double]) => Either[MassLevelError, Vector[Observed]]

  def hierarchy[U <: Unit2D](
      select: Selector[U],
      masses: Gen[Mass[U]],
      tolerance: Tolerance
  ): RuleSet =
    val coverages = Gen.choose(1, 19).map(_.toDouble / 20.0).flatMap { first =>
      Gen
        .choose(1, 19)
        .map(_.toDouble / 20.0)
        .map(second => Vector(math.min(first, second), math.max(first, second)))
    }

    new SimpleRuleSet(
      "massLevels.hierarchy",
      "thresholds do not rise with requested coverage" -> forAll(masses, coverages) {
        (mass, requested) =>
          select(mass, requested) match
            case Left(error)   => Prop(false) :| error.message
            case Right(levels) =>
              Prop(levels.length == requested.length) &&
              (if levels.length == requested.length then
                 Prop(levels(0).threshold + tolerance.absolute >= levels(1).threshold)
               else Prop(false))
      },
      "every returned region covers its request" -> forAll(masses, coverages) {
        (mass, requested) =>
          select(mass, requested) match
            case Left(error)   => Prop(false) :| error.message
            case Right(levels) =>
              Prop.all(levels.map { level =>
                Prop(level.coveredMass + tolerance.absolute >= level.coverage) :|
                  s"covered=${level.coveredMass}, requested=${level.coverage}"
              }*)
      },
      "a threshold includes all and only cells at or above it" -> forAll(masses, coverages) {
        (mass, requested) =>
          select(mass, requested) match
            case Left(error)   => Prop(false) :| error.message
            case Right(levels) =>
              Prop.all(levels.map { level =>
                val selected = mass.values.iterator.filter(_ >= level.threshold).toVector
                val total    = selected.sum
                (Prop(level.cells == selected.length) :|
                  s"cells=${level.cells}, expected=${selected.length}") &&
                (Prop(tolerance.approxEquals(level.coveredMass, total)) :|
                  s"covered=${level.coveredMass}, expected=$total")
              }*)
      }
    )

end MassLevelsLaws

object MassLevelsLaws extends MassLevelsLaws
