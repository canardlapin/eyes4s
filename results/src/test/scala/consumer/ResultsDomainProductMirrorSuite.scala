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
class ResultsDomainProductMirrorSuite extends munit.FunSuite:
  test("eyes4s.results.Accounting rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.results.Accounting; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors =
      typeCheckErrors("summon[scala.deriving.Mirror.ProductOf[eyes4s.results.Accounting]]")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.results.Bin rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.results.Bin; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors("summon[scala.deriving.Mirror.ProductOf[eyes4s.results.Bin]]")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.results.BindingDigest rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.results.BindingDigest; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors =
      typeCheckErrors("summon[scala.deriving.Mirror.ProductOf[eyes4s.results.BindingDigest]]")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.results.Bins rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.results.Bins; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors("summon[scala.deriving.Mirror.ProductOf[eyes4s.results.Bins]]")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.results.CovariateName rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.results.CovariateName; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors =
      typeCheckErrors("summon[scala.deriving.Mirror.ProductOf[eyes4s.results.CovariateName]]")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.results.CovariateRow[String] rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.results.CovariateRow[String]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.results.CovariateRow[String]]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.results.CovariateSchema rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.results.CovariateSchema; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors =
      typeCheckErrors("summon[scala.deriving.Mirror.ProductOf[eyes4s.results.CovariateSchema]]")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.results.Levels rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.results.Levels; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors =
      typeCheckErrors("summon[scala.deriving.Mirror.ProductOf[eyes4s.results.Levels]]")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.results.MinimumQueries rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.results.MinimumQueries; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors =
      typeCheckErrors("summon[scala.deriving.Mirror.ProductOf[eyes4s.results.MinimumQueries]]")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.results.NumericUnit rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.results.NumericUnit; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors =
      typeCheckErrors("summon[scala.deriving.Mirror.ProductOf[eyes4s.results.NumericUnit]]")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.results.Query[String] rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.results.Query[String]; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors =
      typeCheckErrors("summon[scala.deriving.Mirror.ProductOf[eyes4s.results.Query[String]]]")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.results.ReportId rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.results.ReportId; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors =
      typeCheckErrors("summon[scala.deriving.Mirror.ProductOf[eyes4s.results.ReportId]]")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.results.ReportRef.Cell rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.results.ReportRef.Cell; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors =
      typeCheckErrors("summon[scala.deriving.Mirror.ProductOf[eyes4s.results.ReportRef.Cell]]")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.results.ReportRef.Participant rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.results.ReportRef.Participant; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.results.ReportRef.Participant]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.results.ReportSelection rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.results.ReportSelection; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors =
      typeCheckErrors("summon[scala.deriving.Mirror.ProductOf[eyes4s.results.ReportSelection]]")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.results.ReportSpec rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.results.ReportSpec; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors =
      typeCheckErrors("summon[scala.deriving.Mirror.ProductOf[eyes4s.results.ReportSpec]]")
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }

  test("eyes4s.results.TableJson.Number rejects constructing mirrors") {
    assertEquals(
      typeCheckErrors(
        "type Validated = eyes4s.results.TableJson.Number; val value: Option[Validated] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "summon[scala.deriving.Mirror.ProductOf[eyes4s.results.TableJson.Number]]"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("Mirror.ProductOf")))
  }
