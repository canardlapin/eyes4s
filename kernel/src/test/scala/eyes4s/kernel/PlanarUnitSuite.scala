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

import eyes4s.kernel.Unit2D.*

class PlanarUnitSuite extends munit.FunSuite:
  test("each phantom unit's label names its own planar unit, symbol and name") {
    assertEquals(
      Vector(UnitLabel[Px], UnitLabel[Deg], UnitLabel[Norm], UnitLabel[Mm]).map(l =>
        (l.planar, l.symbol, l.name)
      ),
      Vector(
        (PlanarUnit.Px, "px", "pixels"),
        (PlanarUnit.Deg, "deg", "degrees of visual angle"),
        (PlanarUnit.Norm, "norm", "normalised stimulus coordinates"),
        (PlanarUnit.Mm, "mm", "millimetres")
      )
    )
    assertEquals(PlanarUnit.of[Deg], PlanarUnit.Deg)
  }

  test("planar units are distinct in symbol and cover every phantom unit") {
    assertEquals(PlanarUnit.values.map(_.symbol).distinct.length, 4)
    assertEquals(
      PlanarUnit.values.toVector,
      Vector(PlanarUnit.Px, PlanarUnit.Deg, PlanarUnit.Norm, PlanarUnit.Mm)
    )
  }
