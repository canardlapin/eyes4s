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

import eyes4s.design.*
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

/** Reusable component-wise conformance for downstream score algebras.
  * Generators must stay in a finite, non-overflowing subtraction domain.
  * The required independently computed example prevents a zero implementation
  * from satisfying only self-zero and antisymmetry.
  */
trait ContrastLaws extends Laws:
  def subtraction[S, D](
      instance: Contrastable[S, D],
      gen: Gen[S],
      input: S => Vector[Double],
      output: D => Vector[Double],
      oracle: (S, S, Vector[Double]),
      tolerance: Tolerance
  ): RuleSet =
    def agrees(actual: Either[DifferenceError, D], expected: Vector[Double]): Boolean =
      actual.exists { value =>
        val components = output(value)
        components.size == instance.components.size && components.size == expected.size &&
        components.zip(expected).forall { case (a, b) =>
          a.isFinite && tolerance.approxEquals(a, b)
        }
      }
    new SimpleRuleSet(
      "contrast",
      "independent nonzero oracle" -> (oracle._3
        .exists(value => !tolerance.approxEquals(value, 0.0)) &&
        agrees(instance.subtract(oracle._1, oracle._2), oracle._3)),
      "self zero" -> forAll(gen) { a =>
        agrees(instance.subtract(a, a), Vector.fill(instance.components.size)(0.0))
      },
      "component-wise subtraction" -> forAll(gen, gen) { (a, b) =>
        val left  = input(a)
        val right = input(b)
        left.size == instance.components.size && right.size == left.size &&
        agrees(instance.subtract(a, b), left.zip(right).map { case (x, y) => x - y })
      },
      "antisymmetric" -> forAll(gen, gen) { (a, b) =>
        instance
          .subtract(b, a)
          .exists(value => agrees(instance.subtract(a, b), output(value).map(-_)))
      }
    )

object ContrastLaws extends ContrastLaws
