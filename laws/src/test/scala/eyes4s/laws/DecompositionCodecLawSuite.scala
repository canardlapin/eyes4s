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

import eyes4s.codec.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import org.scalacheck.Gen

private[laws] object DecompositionFamilyCodecFixture:
  private def get[E, A](value: Either[E, A]): A = value.toOption.get
  val codec: DecompositionCodec[Px]             = new DecompositionCodec[Px]
  private val frame                             = get(Frame.screen("decomposition-laws", 3, 1))
  val grid: Grid[Px]                            = get(Grid.over(frame, 3, 1))
  def mass(a: Double, b: Double, c: Double): Mass[Px] =
    val values = IArray(a, b, c)
    get(
      Surface
        .intensity(grid, values, Provenance.raw(ContentHash.of(values)))
        .flatMap(_.normalised)
    )
  val predictors: PredictorSet[Px] = get(
    PredictorSet.of(
      Vector(
        get(PredictorId.of("a λ")) -> mass(3, 2, 0),
        get(PredictorId.of("b λ")) -> mass(0, 2, 3)
      )
    )
  )

class DecompositionCodecLawSuite extends munit.DisciplineSuite:
  import DecompositionFamilyCodecFixture.*
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val NativeSolverTolerance: Tolerance  = Tolerance(1e-12, 1e-12)
  private val plans                             = for
    a         <- Gen.choose(1, 5)
    b         <- Gen.choose(0, 5)
    c         <- Gen.choose(0, 5)
    method    <- Gen.choose(0, 2)
    intercept <- Gen.oneOf(Intercept.values.toVector)
    rankValue <- Gen.oneOf(1e-4, 1e-7, 1e-10)
    dualValue <- Gen.oneOf(1e-8, 1e-12)
    reverse   <- Gen.oneOf(true, false)
  yield
    val rank = get(RelativeRankTolerance.of(rankValue));
    val dual = get(RelativeDualTolerance.of(dualValue))
    val spec = method match
      case 0 => DecompositionSpec.Ols(intercept, rank)
      case 1 => DecompositionSpec.NonNegative(rank, dual)
      case _ => DecompositionSpec.Mixture(intercept, rank, dual)
    val refs = if reverse then get(PredictorSet.of(predictors.entries.reverse)) else predictors
    get(DecompositionPlan.of(mass(a.toDouble, b.toDouble, c.toDouble), refs, spec))
  private def sameFit(a: DecompositionFit[Px], b: DecompositionFit[Px]): Boolean = (a, b) match
    case (DecompositionFit.Ols(x), DecompositionFit.Ols(y)) =>
      x.coefficients == y.coefficients && x.intercept == y.intercept && x.fitted.values.toVector == y.fitted.values.toVector && x.residual.values.toVector == y.residual.values.toVector && x.diagnostics == y.diagnostics
    case (DecompositionFit.NonNegative(x), DecompositionFit.NonNegative(y)) =>
      x.coefficients == y.coefficients && x.fitted.values.toVector == y.fitted.values.toVector && x.residual.values.toVector == y.residual.values.toVector && x.diagnostics == y.diagnostics
    case (DecompositionFit.Mixture(x), DecompositionFit.Mixture(y)) =>
      x.weights == y.weights && x.background == y.background && x.fitted.values.toVector == y.fitted.values.toVector && x.residual.values.toVector == y.residual.values.toVector && x.diagnostics == y.diagnostics
    case _ => false
  checkAll(
    "decomposition plan",
    CodecLaws.roundTrip(
      codec.plans,
      plans,
      (a, b) =>
        a.description == b.description && a.inputHash == b.inputHash && a.planHash == b.planHash
    )
  )
  checkAll(
    "decomposition result",
    CodecLaws.roundTrip(
      codec.results,
      plans.map(p => get(DecompositionArchive.run(p))),
      (a, b) =>
        a.plan.description == b.plan.description && a.result.inputHash == b.result.inputHash && a.result.planHash == b.result.planHash && sameFit(
          a.result.fit,
          b.result.fit
        )
    )
  )

  test("independent rational oracle distinguishes signed OLS, NNLS scale and simplex mixture") {
    val response = mass(1, 0, 0)
    val rank     = RelativeRankTolerance.default; val dual = RelativeDualTolerance.default
    def run(spec: DecompositionSpec) =
      get(get(DecompositionPlan.of(response, predictors, spec)).run).fit
    def close(actual: Double, expected: Double): Unit =
      assert(NativeSolverTolerance.approxEquals(actual, expected), s"$actual != $expected")
    run(DecompositionSpec.Ols(Intercept.Exclude, rank)) match
      case DecompositionFit.Ols(value) =>
        close(value.coefficients(0)._2, 65.0 / 51.0);
        close(value.coefficients(1)._2, -20.0 / 51.0)
        close(value.diagnostics.residualSumSquares, 4.0 / 17.0)
      case other => fail(s"$other")
    run(DecompositionSpec.NonNegative(rank, dual)) match
      case DecompositionFit.NonNegative(value) =>
        close(value.coefficients(0)._2, 15.0 / 13.0); close(value.coefficients(1)._2, 0.0)
        close(value.diagnostics.residualSumSquares, 4.0 / 13.0)
        assert(value.fitted.values.toVector.sum > 1.0)
      case other => fail(s"$other")
    run(DecompositionSpec.Mixture(Intercept.Exclude, rank, dual)) match
      case DecompositionFit.Mixture(value) =>
        close(value.weights(0)._2, 1.0); close(value.weights(1)._2, 0.0)
        close(value.diagnostics.residualSumSquares, 8.0 / 25.0)
      case other => fail(s"$other")
  }
