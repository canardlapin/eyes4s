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

import scala.compiletime.testing.typeCheckErrors

/** Valid public types cannot provide an unchecked constructing product mirror. */
class IoDomainProductMirrorSuite extends munit.FunSuite:
  test("eyes4s.io.AdmittedFixationRow[String] rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.io.AdmittedFixationRow[String]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.io.AdmittedFixationRow[String]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.io.FixationColumns rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.io.FixationColumns; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors =
      typeCheckErrors("summon[scala.deriving.Mirror.ProductOf[eyes4s.io.FixationColumns]]")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.io.FixationPlacementTally rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.io.FixationPlacementTally; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.io.FixationPlacementTally]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.io.FixationPosition[eyes4s.kernel.Unit2D.Px] rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.io.FixationPosition[eyes4s.kernel.Unit2D.Px]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.io.FixationPosition[eyes4s.kernel.Unit2D.Px]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test(
    "eyes4s.io.PlacedFixationPosition[eyes4s.kernel.Unit2D.Px] rejects constructing mirrors"
  ) {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.io.PlacedFixationPosition[eyes4s.kernel.Unit2D.Px]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.io.PlacedFixationPosition[eyes4s.kernel.Unit2D.Px]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.io.RejectedFixationRow[String] rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.io.RejectedFixationRow[String]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.io.RejectedFixationRow[String]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.io.SampleOriginCounts rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.io.SampleOriginCounts; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors =
      typeCheckErrors("summon[scala.deriving.Mirror.ProductOf[eyes4s.io.SampleOriginCounts]]")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.io.TrialColumns rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.io.TrialColumns; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors =
      typeCheckErrors("summon[scala.deriving.Mirror.ProductOf[eyes4s.io.TrialColumns]]")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.io.UnplacedFixationPosition rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.io.UnplacedFixationPosition; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.io.UnplacedFixationPosition]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }
