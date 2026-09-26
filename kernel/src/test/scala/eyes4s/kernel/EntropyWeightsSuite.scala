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

package eyes4s.kernel

/** Entropy of unnormalised weights is the entropy of the mass they normalise to. */
class EntropyWeightsSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val Exact                             = 0.0

  test("weights give the entropy and relative entropy of their normalised mass") {
    val weights = IArray(2.0, 1.0, 1.0, 0.0)
    val grid    = get(Grid.over(get(Frame.screen("weights", 2, 2)), 2, 2))
    val mass    =
      get(
        Surface.mass(
          grid,
          IArray(0.5, 0.25, 0.25, 0.0),
          Provenance.raw(ContentHash.of(weights))
        )
      )
    Vector(LogBase.E, LogBase.Two).foreach { base =>
      val h = get(Entropy.ofWeights(weights, base))
      assertEqualsDouble(h.value, mass.entropy(base).value, Exact)
      assertEquals(h.base, base)
      assertEqualsDouble(Entropy.relative(h, 4), mass.relativeEntropy(base), Exact)
    }
    assertEqualsDouble(get(Entropy.ofWeights(weights, LogBase.Two)).value, 1.5, 1e-15)
  }

  test("one cell has zero relative entropy, not 0 / 0") {
    val h = get(Entropy.ofWeights(IArray(3.0), LogBase.E))
    assertEquals(h.value, 0.0)
    assertEquals(Entropy.relative(h, 1), 0.0)
  }

  test("negative, non-finite and massless weights are refused with the offending index") {
    assertEquals(
      Entropy.ofWeights(IArray(1.0, -0.5), LogBase.E),
      Left(SurfaceError.NegativeValue(1, -0.5))
    )
    assertEquals(
      Entropy.ofWeights(IArray(1.0, 2.0, Double.PositiveInfinity), LogBase.E),
      Left(SurfaceError.NonFiniteValue(2, Double.PositiveInfinity))
    )
    assertEquals(
      Entropy.ofWeights(IArray(0.0, 0.0), LogBase.E),
      Left(SurfaceError.DegenerateTotal(0.0))
    )
    assertEquals(
      Entropy.ofWeights(IArray.empty[Double], LogBase.E),
      Left(SurfaceError.DegenerateTotal(0.0))
    )
  }

end EntropyWeightsSuite
