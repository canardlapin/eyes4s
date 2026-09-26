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

import eyes4s.compare.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Norm

class BaselineMapLawSuite extends munit.DisciplineSuite:
  private val grid =
    Grid.over(Frame.unitSquare("baseline-map-laws").toOption.get, 5, 3).toOption.get
  private val massGen      = Generators.genMass(grid)
  private val MapTolerance = Tolerance(1e-12, 1e-12)
  MapSimilarityMethod.values.foreach { method =>
    val instance = method.instance[Norm]
    checkAll(
      s"$method.symmetry",
      MeasureLaws.symmetry[Mass[Norm], Similarity](
        instance,
        massGen,
        (a, b) => MapTolerance.approxEquals(a.value, b.value)
      )
    )
    checkAll(s"$method.described", MeasureLaws.described(instance))
  }
