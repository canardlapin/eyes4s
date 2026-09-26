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

package eyes4s.surface

import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px

class KdeReferenceSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val GaussianTolerance                 = 1e-12
  private val frame                             = get(Frame.screen("native-gaussian", 5, 3))
  private val grid                              = get(Grid.over(frame, 5, 3))
  private val positions = IArray(Pt[Px](0.5, 0.5), Pt[Px](2.5, 2.5), Pt[Px](4.5, 1.5))

  test("native 5 by 3 Gaussian agrees with a direct two-dimensional discrete-kernel oracle") {
    Vector(EdgePolicy.Truncate, EdgePolicy.Renormalise).foreach { policy =>
      Vector(Vector(1.0, 1.0, 1.0), Vector(5.0, 3.0, 1.0)).foreach { weights =>
        val measure     = get(PointMeasure.of(frame, positions, IArray.from(weights)))
        val smooth      = Smoother.gaussian(get(Sigma.px(1)), policy)
        val actual      = get(smooth.smooth(measure, grid)).values.toVector
        val denominator = (-3 to 3).map(i => math.exp(-0.5 * i * i)).sum
        def kernel(dx: Int, dy: Int): Double =
          if math.abs(dx) > 3 || math.abs(dy) > 3 then 0.0
          else math.exp(-0.5 * (dx * dx + dy * dy)) / (denominator * denominator)
        val cells    = Vector((0, 0), (2, 2), (4, 1))
        val expected = (0 until 15).map { i =>
          val x = i % 5; val y = i / 5
          cells
            .zip(weights)
            .map { case ((px, py), weight) =>
              val retained = if policy == EdgePolicy.Truncate then 1.0
              else (0 until 15).map(j => kernel(j % 5 - px, j / 5 - py)).sum
              weight * kernel(x - px, y - py) / retained
            }
            .sum
        }.toVector
        actual.zip(expected).foreach((a, b) => assertEqualsDouble(a, b, GaussianTolerance))
        val normalized = get(smooth.density(measure, grid)).values.toVector
        normalized
          .zip(expected.map(_ / expected.sum))
          .foreach((a, b) => assertEqualsDouble(a, b, GaussianTolerance))
        if policy == EdgePolicy.Renormalise then
          assertEqualsDouble(actual.sum, weights.sum, GaussianTolerance)
        else assert(actual.sum < weights.sum)
      }
    }
  }

  test("explicit zero mass and incompatible frames remain failures instead of dropped groups") {
    val smoother = Smoother.gaussian(get(Sigma.px(1)), EdgePolicy.Truncate)
    assert(smoother.smooth(PointMeasure.empty(frame), grid).isLeft)
    val foreign =
      get(PointMeasure.of(get(Frame.screen("other", 5, 3)), positions, IArray(1.0, 1.0, 1.0)))
    assert(smoother.smooth(foreign, grid).isLeft)
  }

  test(
    "type-7 IQR suggestion and display clamps match the pinned convention separately from native rules"
  ) {
    val measure = get(PointMeasure.of(frame, positions, IArray(5.0, 3.0, 1.0)))
    val raw     = get(IqrBandwidth.suggest(measure, IqrBandwidthClamp.Unclamped)).value
    val bounded =
      get(IqrBandwidth.suggest(measure, IqrBandwidthClamp.DisplayOneToFifteenPercent)).value
    assertEqualsDouble(raw, KdeReference.suggested(0), GaussianTolerance)
    assertEqualsDouble(bounded, KdeReference.suggested(1), GaussianTolerance)
    val constant =
      get(PointMeasure.of(frame, IArray.fill(3)(Pt[Px](1, 1)), IArray(1.0, 1.0, 1.0)))
    assertEqualsDouble(
      get(IqrBandwidth.suggest(constant, IqrBandwidthClamp.DisplayOneToFifteenPercent)).value,
      KdeReference.suggested(2),
      GaussianTolerance
    )
    assert(IqrBandwidth.suggest(constant, IqrBandwidthClamp.Unclamped).isLeft)
    assert(IqrBandwidth.suggest(PointMeasure.empty(frame), IqrBandwidthClamp.Unclamped).isLeft)
    val singleton = get(PointMeasure.of(frame, IArray(Pt[Px](1, 1)), IArray(1.0)))
    assert(IqrBandwidth.suggest(singleton, IqrBandwidthClamp.DisplayOneToFifteenPercent).isLeft)
    assert(math.abs(get(Bandwidth.silverman(measure)).value - raw) > 0.01)
    assert(math.abs(get(Bandwidth.scott(measure)).value - raw) > 0.01)
  }
