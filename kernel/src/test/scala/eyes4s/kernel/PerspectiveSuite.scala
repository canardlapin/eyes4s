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

import scala.compiletime.testing.typeCheckErrors

class PerspectiveSuite extends munit.FunSuite:

  test("Length constructors preserve units and admit coherent zero") {
    assertEquals(Length.mm(25.0).map(_.toMm), Right(25.0))
    assertEquals(Length.cm(2.5).map(_.toMm), Right(25.0))
    assertEquals(Length.m(0.025).map(_.toMm), Right(25.0))
    assertEquals(Length.mm(0.0), Right(Length.zero))
  }

  test("Length rejects negative, non-finite, and conversion-overflow operands") {
    assertEquals(
      Length.cm(-1.0),
      Left(GeometryError.NegativeLength(-1.0, LengthUnit.Centimetres))
    )
    assert(
      Length.mm(Double.NaN) match
        case Left(GeometryError.NonFiniteLength(value, LengthUnit.Millimetres)) => value.isNaN
        case _                                                                  => false
    )
    assertEquals(
      Length.m(Double.MaxValue),
      Left(GeometryError.NonFiniteLength(Double.MaxValue, LengthUnit.Metres))
    )
  }

  test("Perspective owns strict positivity and offers raw-millimetre construction") {
    assert(Perspective.millimetres(600.0, 500.0, 300.0).isRight)
    assertEquals(
      Perspective.of(
        Length.zero,
        Length.mm(500.0).toOption.get,
        Length.mm(300.0).toOption.get
      ),
      Left(GeometryError.NonPositivePerspective(0.0, 500.0, 300.0))
    )
    assertEquals(
      Perspective.millimetres(600.0, -500.0, 300.0),
      Left(GeometryError.NegativeLength(-500.0, LengthUnit.Millimetres))
    )
  }

  test("raw Doubles cannot inhabit Length") {
    val errors = typeCheckErrors("""
      import eyes4s.kernel.*
      val negative: Length = -1.0
      val nonFinite: Length = Double.NaN
    """)
    assertEquals(errors.length, 2)
  }

end PerspectiveSuite
