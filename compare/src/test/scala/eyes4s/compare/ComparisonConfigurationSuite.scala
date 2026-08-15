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

package eyes4s.compare

class ComparisonConfigurationSuite extends munit.FunSuite:

  test("every admitted projection count is positive") {
    List(1, 2, 8, 16, 1024, Int.MaxValue).foreach { supplied =>
      val directions = ProjectionDirections.of(supplied).toOption.get
      assert(directions.value > 0, clue(supplied))
    }

    List(0, -1, Int.MinValue).foreach { supplied =>
      assertEquals(
        ProjectionDirections.of(supplied),
        Left(
          ComparisonConfigurationError.InvalidProjectionDirections(
            "sliced Wasserstein-1",
            supplied
          )
        )
      )
    }
  }

  test("every admitted Sinkhorn configuration has finite positive local parameters") {
    List(0.001, 1.0, Double.MaxValue).foreach { supplied =>
      assert(SinkhornRegularisation.of(supplied).exists(_.value > 0.0))
    }
    List(Double.NaN, Double.NegativeInfinity, Double.PositiveInfinity, 0.0, -1.0)
      .foreach { supplied =>
        assert(SinkhornRegularisation.of(supplied).isLeft, clue(supplied))
      }

    List(1, 10, Int.MaxValue).foreach { supplied =>
      assert(SinkhornIterations.of(supplied).exists(_.value > 0))
      assert(SinkhornCellLimit.of(supplied).exists(_.value > 0))
    }
    List(0, -1, Int.MinValue).foreach { supplied =>
      assert(SinkhornIterations.of(supplied).isLeft, clue(supplied))
      assert(SinkhornCellLimit.of(supplied).isLeft, clue(supplied))
    }
  }

  test("every admitted probability floor is finite and in (0, 1]") {
    List(Double.MinPositiveValue, 1e-12, 0.5, 1.0).foreach { supplied =>
      val floor = ProbabilityFloor.of(supplied).toOption.get
      assert(floor.value.isFinite)
      assert(floor.value > 0.0)
      assert(floor.value <= 1.0)
    }
    List(Double.NaN, Double.NegativeInfinity, Double.PositiveInfinity, 0.0, -1.0, 1.1)
      .foreach { supplied =>
        assert(ProbabilityFloor.of(supplied).isLeft, clue(supplied))
      }
  }

  test("every admitted ScanMatch gap is finite and positive") {
    List(Double.MinPositiveValue, 1.0, Double.MaxValue).foreach { supplied =>
      val gap = ScanMatchGap.of(supplied).toOption.get
      assert(gap.value.isFinite)
      assert(gap.value > 0.0)
    }
    List(Double.NaN, Double.NegativeInfinity, Double.PositiveInfinity, 0.0, -1.0)
      .foreach { supplied =>
        assert(ScanMatchGap.of(supplied).isLeft, clue(supplied))
      }
  }

  test("raw numerical values cannot bypass comparison configuration constructors") {
    val errors = scala.compiletime.testing.typeCheckErrors("""
      import eyes4s.compare.*
      import eyes4s.kernel.*
      import eyes4s.kernel.Unit2D.Px

      val directions: ProjectionDirections = 1
      val regularisation: SinkhornRegularisation = 1.0
      val iterations: SinkhornIterations = 10
      val limit: SinkhornCellLimit = 100
      val floor: ProbabilityFloor = 1e-12
      val gap: ScanMatchGap = 1.0
      val projected = Transport.slicedWasserstein[Px](1)
      val kl = Distribution.flooredKullbackLeibler[Px](floor = 1e-12)
      val scan = ScanMatch.similarity[Char](ScanMatch.exactMatch, 1.0)
    """)
    assert(errors.length >= 9, clue(errors.map(_.message)))
  }

end ComparisonConfigurationSuite
