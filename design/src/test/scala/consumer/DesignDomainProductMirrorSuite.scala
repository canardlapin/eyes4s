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
class DesignDomainProductMirrorSuite extends munit.FunSuite:
  test(
    "eyes4s.design.DirectedPaired[Unit, Unit, Unit, Unit, Unit, Unit] rejects constructing mirrors"
  ) {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.design.DirectedPaired[Unit, Unit, Unit, Unit, Unit, Unit]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.design.DirectedPaired[Unit, Unit, Unit, Unit, Unit, Unit]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test(
    "eyes4s.design.DirectedPairwiseAnalysis[Unit, Unit, Unit, Unit] rejects constructing mirrors"
  ) {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.design.DirectedPairwiseAnalysis[Unit, Unit, Unit, Unit]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.design.DirectedPairwiseAnalysis[Unit, Unit, Unit, Unit]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.design.DuplicateTrials[String, Unit, Unit] rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.design.DuplicateTrials[String, Unit, Unit]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.design.DuplicateTrials[String, Unit, Unit]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.design.IndexedTrial[String, Unit, Unit] rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.design.IndexedTrial[String, Unit, Unit]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.design.IndexedTrial[String, Unit, Unit]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.design.Pairing.Between[Unit, Unit] rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.design.Pairing.Between[Unit, Unit]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.design.Pairing.Between[Unit, Unit]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.design.Pairing.Within[String] rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.design.Pairing.Within[String]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.design.Pairing.Within[String]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.design.PairingReport[Unit, Unit] rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.design.PairingReport[Unit, Unit]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.design.PairingReport[Unit, Unit]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.design.ReductionReport[String] rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.design.ReductionReport[String]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.design.ReductionReport[String]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.design.ReductionRow[String, Unit] rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.design.ReductionRow[String, Unit]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.design.ReductionRow[String, Unit]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.design.ScheduledPair[Unit, Unit] rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.design.ScheduledPair[Unit, Unit]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.design.ScheduledPair[Unit, Unit]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.design.UndirectedPaired[String, Unit, Unit] rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.design.UndirectedPaired[String, Unit, Unit]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.design.UndirectedPaired[String, Unit, Unit]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test(
    "eyes4s.design.UndirectedPairwiseAnalysis[String, Unit, Unit] rejects constructing mirrors"
  ) {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.design.UndirectedPairwiseAnalysis[String, Unit, Unit]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.design.UndirectedPairwiseAnalysis[String, Unit, Unit]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }
