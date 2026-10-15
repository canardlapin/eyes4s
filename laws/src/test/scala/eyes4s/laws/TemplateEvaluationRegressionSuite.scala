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

/** Independent exact integer predictions for the identity training design. */
class TemplateEvaluationRegressionSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val ExactIntegerPredictionTolerance: Tolerance = Tolerance(0.0, 0.0)

  test("every page cut preserves independently known predictions and residuals exactly") {
    val basis = get(TemplateBasis.of("integer-oracle", Vector("x", "z"), "score"))
    val train = Vector(
      get(TemplateObservation.of("train-x", "train", Vector(1.0, 0.0), 2.0)),
      get(TemplateObservation.of("train-z", "train", Vector(0.0, 1.0), 3.0))
    )
    for size <- Vector(1, 2, 3, 9, 37) do
      val held = (0 until size).toVector.map { i =>
        val x        = (i % 7 - 3).toDouble; val z = (i % 5 - 2).toDouble;
        val residual = (i % 3 - 1).toDouble
        get(TemplateObservation.of(s"held-$i", "held", Vector(x, z), 2 * x + 3 * z + residual))
      }
      val input = get(TemplateSplit.of(TemplateDesign.fixed(basis), train ++ held, Set("held")))
      val fitted = get(Template.fit(input.training))
      for amount <- Vector(1, 2, 8, Int.MaxValue) do
        val result =
          get(fitted.evaluationWork(input.heldOut)).complete(get(PairQuantum.of(amount)))
        assertEquals(result.rows.size, held.size)
        result.rows.zip(held).foreach { (found, source) =>
          val expectedPrediction     = 2 * source.input(0) + 3 * source.input(1)
          val expectedResidual       = source.response - expectedPrediction
          val (prediction, residual) = get(found.result)
          assert(ExactIntegerPredictionTolerance.approxEquals(prediction, expectedPrediction))
          assert(ExactIntegerPredictionTolerance.approxEquals(residual, expectedResidual))
          assertEquals(found.key, source.key)
        }
  }
