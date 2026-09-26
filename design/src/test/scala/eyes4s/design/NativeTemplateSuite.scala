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

package eyes4s.templateconsumer

import eyes4s.design.*

import scala.compiletime.testing.typeCheckErrors

class NativeTemplateSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val FitTolerance                      = 1e-12
  private def close(actual: Double, expected: Double): Unit =
    assertEqualsDouble(actual, expected, FitTolerance)
  private def split(
      y: Double = 7.0,
      x: Vector[Double] = Vector(3.0, 2.0),
      offset: Double = 0.0
  ): TemplateSplit[String] =
    val basis = get(TemplateBasis.of("fixed/1", Vector("a", "b"), "score"))
    val train = Vector(Vector(1.0, 0.0), Vector(0.0, 1.0), Vector(1.0, 1.0), Vector(2.0, 1.0))
      .zip(Vector(1.0, 2.0, 3.0, 4.0))
      .zipWithIndex
      .map { case ((features, response), i) =>
        get(TemplateObservation.of(i.toString, "train", features, response + offset))
      }
    get(
      TemplateSplit.of(
        basis,
        train :+ get(TemplateObservation.of("held", "test", x, y)),
        Set("test")
      )
    )

  test("native scaled QR recovers exact coefficients and held-out prediction") {
    val s = split(); val fit = get(FittedTemplate.fitNoIntercept(s.training))
    fit.coefficients.zip(Vector(1.0, 2.0)).foreach((a, b) => close(a, b))
    assertEquals(fit.methodId, FittedTemplate.nativeMethod)
    assertNotEquals(fit.methodId, FittedTemplate.method)
    val evaluation = get(fit.evaluate(s.heldOut))
    close(get(evaluation.rows.head.result)._1, 7.0)
    close(get(evaluation.meanSquaredError), 0.0)
  }

  test("held-out responses and features cannot change the fit") {
    val a    = split(); val b = split(700.0); val c = split(700.0, Vector(4.0, 2.0))
    val fits = Vector(a, b, c).map(s => get(FittedTemplate.fitNoIntercept(s.training)))
    assertEquals(fits.map(_.training.hash).distinct.size, 1)
    assertEquals(fits.map(_.coefficients).distinct.size, 1)
    val resultB = get(get(fits(1).evaluate(b.heldOut)).rows.head.result)
    close(resultB._1, 7.0); close(resultB._2, 693.0)
    val resultC = get(get(fits(2).evaluate(c.heldOut)).rows.head.result)
    close(resultC._1, 8.0); close(resultC._2, 692.0)
  }

  test("offset responses do not insert an implicit intercept") {
    // Exact normal equations: X'X=((6,3),(3,3)), X'1=(4,3); slopes shift by (1/3,2/3).
    val fit = get(FittedTemplate.fitNoIntercept(split(offset = 3.0).training))
    close(fit.coefficients(0), 2.0); close(fit.coefficients(1), 4.0)
  }

  test("rank and shape failure preserve training identity and solver operands") {
    val basis = get(TemplateBasis.of("collinear", Vector("a", "b"), "score"))
    val rows  = Vector(
      ("a", "train", Vector(1.0, 2.0)),
      ("b", "train", Vector(2.0, 4.0)),
      ("c", "test", Vector(3.0, 6.0))
    )
      .map((k, f, x) => get(TemplateObservation.of(k, f, x, 1.0)))
    val s     = get(TemplateSplit.of(basis, rows, Set("test")))
    val error = FittedTemplate.fitNoIntercept(s.training).swap.toOption.get
    assert(error.isInstanceOf[TemplateFitError.Fit])
    assert(error.message.contains(s.training.hash.render))
    val short = get(TemplateSplit.of(basis, rows.take(1) ++ rows.takeRight(1), Set("test")))
    assert(FittedTemplate.fitNoIntercept(short.training).isLeft)
  }

  test("native fitting cannot consume held-out capability or unvalidated training") {
    assert(typeCheckErrors("""
      def fit(h: eyes4s.design.TemplateHeldOut[String]) = eyes4s.design.FittedTemplate.fitNoIntercept(h)
    """).nonEmpty)
    assert(typeCheckErrors("""
      new eyes4s.design.TemplateTraining[String](null, Vector.empty, null)
    """).nonEmpty)
  }
