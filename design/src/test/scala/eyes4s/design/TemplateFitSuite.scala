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

package eyes4s.design

import munit.FunSuite
import scala.compiletime.testing.typeCheckErrors

class TemplateFitSuite extends FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(e => fail(s"$e"), identity)
  private val basis = get(TemplateBasis.of("fixed/1", Vector("a", "b"), "score"))
  private val train = Vector(
    get(TemplateObservation.of("a", "training", Vector(1.0, 0.0), 1.0)),
    get(TemplateObservation.of("b", "training", Vector(0.0, 1.0), 2.0))
  )
  private def split(response: Double = 7.0, features: Vector[Double] = Vector(3.0, 2.0)) =
    get(
      TemplateSplit.of(
        TemplateDesign.importedLm(basis),
        train :+ get(TemplateObservation.of("e", "test", features, response)),
        Set("test")
      )
    )
  private def model(s: TemplateSplit[String, Vector[Double]]) =
    get(
      Template.importFit(
        s.training,
        s.training.hash.render,
        basis.columns,
        Vector(1.0, 2.0),
        2,
        2,
        "analytic-exact"
      )
    )

  test("held-out response and features cannot change training identity or coefficients") {
    val a = split()
    val b = split(700.0)
    assertEquals(a.training.hash, b.training.hash)
    assertEquals(a.training.hash, split(features = Vector(30.0, 20.0)).training.hash)
    assertNotEquals(a.heldOut.hash, b.heldOut.hash)
    val fit = model(a)
    assertEquals(get(fit.evaluate(a.heldOut)).rows.map(_.result), Vector(Right((7.0, 0.0))))
    assertEquals(get(fit.evaluate(b.heldOut)).rows.map(_.result), Vector(Right((7.0, 693.0))))
    assertEquals(get(get(fit.evaluate(b.heldOut)).meanSquaredError), 480249.0)
  }

  test("split accounts for every row and rejects duplicate keys or nonexistent folds") {
    val s = split()
    assertEquals(s.training.rows.size + s.heldOut.rows.size, s.rows.size)
    assert(
      TemplateSplit
        .of(TemplateDesign.importedLm(basis), s.rows :+ train.head, Set("test"))
        .isLeft
    )
    assert(TemplateSplit.of(TemplateDesign.importedLm(basis), s.rows, Set("typo")).isLeft)
    assert(TemplateSplit.of(TemplateDesign.importedLm(basis), s.rows, Set.empty[String]).isLeft)
    assert(
      TemplateSplit.of(TemplateDesign.importedLm(basis), s.rows, Set("test", "training")).isLeft
    )
    val duplicateAcrossFolds =
      train :+ get(TemplateObservation.of("a", "test", Vector(1.0, 1.0), 2.0))
    assert(
      TemplateSplit
        .of(TemplateDesign.importedLm(basis), duplicateAcrossFolds, Set("test"))
        .isLeft
    )
  }

  test("admission rejects non-finite data, invalid basis and width without dropping rows") {
    assert(TemplateBasis.of("fixed", Vector("a", "a"), "score").isLeft)
    val nonFinite = get(TemplateObservation.of("bad", "test", Vector(Double.NaN, 1.0), 1.0))
    assertEquals(
      TemplateSplit
        .of(TemplateDesign.importedLm(basis), train :+ nonFinite, Set("test"))
        .left
        .toOption
        .map(_.productPrefix),
      Some("Features")
    )
    assert(TemplateObservation.of("bad", "", Vector(1.0), 1.0).isLeft)
    assert(TemplateObservation.of("bad", "train", Vector(1.0), 1.0, Some(" ")).isLeft)
    assert(TemplateObservation.of("bad", "train", Vector(1.0), Double.PositiveInfinity).isLeft)
    assert(
      TemplateSplit
        .of(
          TemplateDesign.importedLm(basis),
          train :+ get(TemplateObservation.of("e", "test", Vector(1.0), 2.0)),
          Set("test")
        )
        .isLeft
    )
  }

  test(
    "receipt rejects changed training, reordered columns, deficient rank and wrong cardinality"
  ) {
    val a = split()
    def accept(
        hash: String = a.training.hash.render,
        columns: Vector[String] = basis.columns,
        beta: Vector[Double] = Vector(1.0, 2.0),
        rank: Int = 2,
        n: Int = 2
    ) =
      Template.importFit(a.training, hash, columns, beta, rank, n, "test")
    assert(accept(hash = "wrong").isLeft)
    assert(accept(columns = basis.columns.reverse).isLeft)
    assert(accept(beta = Vector(1.0)).isLeft)
    assert(accept(beta = Vector(Double.NaN, 1.0)).isLeft)
    assert(accept(rank = 1).isLeft)
    assert(accept(n = 3).isLeft)
    val changed = get(
      TemplateSplit.of(
        TemplateDesign.importedLm(basis),
        a.rows.updated(0, get(TemplateObservation.of("a", "training", Vector(1.0, 0.0), 9.0))),
        Set("test")
      )
    )
    assertNotEquals(a.training.hash, changed.training.hash)
    assert(model(a).evaluate(changed.heldOut).isLeft)
  }

  test("overflow remains a failed keyed prediction, not a dropped row or infinity") {
    val s      = split(features = Vector(Double.MaxValue, Double.MaxValue))
    val result = get(model(s).evaluate(s.heldOut))
    assertEquals(result.rows.size, 1)
    assertEquals(result.rows.head.key, "e")
    assert(result.rows.head.result.isLeft)
    assert(result.meanSquaredError.isLeft)
  }

  test("held-out data cannot be substituted for the training capability") {
    assert(typeCheckErrors("""
      import eyes4s.design.*
      def wrong(h: TemplateHeldOut[String, Vector[Double]]): TemplateTraining[String, Vector[Double]] = h
    """).nonEmpty)
  }
