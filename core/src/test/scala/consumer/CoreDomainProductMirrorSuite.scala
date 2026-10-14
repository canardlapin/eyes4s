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
class CoreDomainProductMirrorSuite extends munit.FunSuite:
  test("eyes4s.core.EdgeSupport.Fixed rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.core.EdgeSupport.Fixed; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors =
      typeCheckErrors("summon[scala.deriving.Mirror.ProductOf[eyes4s.core.EdgeSupport.Fixed]]")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.core.MaximumSupportGap.AtMost rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.core.MaximumSupportGap.AtMost; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.core.MaximumSupportGap.AtMost]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.core.SamplingEvidence.Fixed rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.core.SamplingEvidence.Fixed; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.core.SamplingEvidence.Fixed]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.core.TemporalSupport.Fixed rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.core.TemporalSupport.Fixed; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.core.TemporalSupport.Fixed]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }
