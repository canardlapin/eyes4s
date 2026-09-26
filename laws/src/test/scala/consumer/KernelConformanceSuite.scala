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

package consumer

import eyes4s.compare.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Norm
import eyes4s.laws.Tolerance

class KernelConformanceSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val GramTolerance                     = Tolerance(1e-12, 1e-12)

  test(
    "cosine inhabits Kernel and its Gram quadratic forms equal squared feature-vector norms"
  ) {
    val grid    = get(Grid.over(get(Frame.unitSquare("kernel-conformance")), 3, 1))
    val vectors = Vector(
      Vector(1.0, 0.0, 0.0),
      Vector(0.0, 1.0, 0.0),
      Vector(0.0, 0.0, 1.0),
      Vector(1.0, 1.0, 1.0),
      Vector(1.0, 2.0, 3.0)
    )
    val masses = vectors.map(v =>
      get(
        Surface.mass(
          grid,
          IArray.from(v.map(_ / v.sum)),
          Provenance.raw(ContentHash.of(IArray.from(v)))
        )
      )
    )
    val kernel: Kernel[Mass[Norm]] = Distribution.cosine[Norm]
    val gram         = masses.map(a => masses.map(b => get(kernel.compare(a, b)).value))
    val features     = vectors.map(v => v.map(_ / math.sqrt(v.map(x => x * x).sum)))
    val coefficients = Vector(
      Vector(1.0, -2.0, 3.0, -4.0, 5.0),
      Vector(1.0, 1.0, 1.0, -math.sqrt(3.0), 0.0),
      Vector(0.0, 0.0, 0.0, 0.0, 1.0)
    )
    coefficients.foreach { weights =>
      val quadratic = (for i <- weights.indices; j <- weights.indices
      yield weights(i) * weights(j) * gram(i)(j)).sum
      val squaredNorm = (0 until 3).map { axis =>
        val coordinate = weights.indices.map(i => weights(i) * features(i)(axis)).sum
        coordinate * coordinate
      }.sum
      assert(GramTolerance.approxEquals(quadratic, squaredNorm), s"$quadratic != $squaredNorm")
      assert(quadratic >= -GramTolerance.absolute)
    }
    assertEquals(gram(0)(1), 0.0)
    assertEquals(gram(0)(0), 1.0)
    assert(GramTolerance.approxEquals(gram(0)(3), 1 / math.sqrt(3.0)))
  }
