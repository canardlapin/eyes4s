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

package eyes4s.io

import eyes4s.examples.TemplateFitGuide
import io.circe.Json
import munit.FunSuite
import scala.compiletime.testing.typeCheckErrors

class TemplateFitSuite extends FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(e => fail(s"$e"), identity)
  private val LinearFitTolerance            = 1e-12
  private def split(y: Double = 7.0)        = get(TemplateFitGuide.input(y))
  private def prepared(y: Double = 7.0)     = get(TemplateFitGuide.prepare(split(y)))

  test("training CSV bytes have the same pinned decimal spelling on JVM and Scala.js") {
    // The independent R input fixture predates the canonical renderer; only its numeric
    // cells change spelling. Keep that historical fixture and its provenance intact.
    val rows     = get(Rfc4180.decode(TemplateFitReference.trainingCsv))
    val expected = Rfc4180.encode(
      rows.head +: rows.tail.map(row => row.take(6) ++ row.drop(6).map(_.stripSuffix(".0")))
    )
    assertEquals(prepared().trainingCsv, expected)
  }

  test("Scala training export matches R input metadata exactly and numeric fields as Doubles") {
    val expected = get(Rfc4180.decode(TemplateFitReference.trainingCsv))
    val actual   = get(Rfc4180.decode(prepared().trainingCsv))
    assertEquals(actual.size, expected.size)
    assertEquals(actual.head, expected.head)
    actual.drop(1).zip(expected.drop(1)).foreach { (a, e) =>
      assertEquals(a.take(6), e.take(6))
      assertEquals(a.drop(6).map(_.toDoubleOption), e.drop(6).map(_.toDoubleOption))
    }
    assertEquals(prepared(700.0).trainingCsv, prepared().trainingCsv)
    val csv = get(Rfc4180.decode(prepared().trainingCsv))
    assertEquals(csv.size, 5)
    assert(csv.drop(1).forall(_(5) != "test"))
  }

  test("exact guide saves, restores, imports the R fit and evaluates the held-out key") {
    val p      = prepared()
    val result = get(TemplateFitGuide.evaluate(p.savedRecipe, TemplateFitReference.receipt))
    assertEquals(result.rows.size, 1)
    assertEquals(result.rows.head.key, "e")
    assertEquals(result.rows.head.fold, "test")
    val (prediction, residual) = get(result.rows.head.result)
    assertEqualsDouble(prediction, 7.0, LinearFitTolerance)
    assertEqualsDouble(residual, 0.0, LinearFitTolerance)
    assertEqualsDouble(get(result.meanSquaredError), 0.0, LinearFitTolerance)
    val fit = get(TemplateFitCsv.importFit(split().training, TemplateFitReference.receipt))
    assertEquals(fit.coefficients.size, 2)
    fit.coefficients.zip(Vector(1.0, 2.0)).foreach { (a, b) =>
      assertEqualsDouble(a, b, LinearFitTolerance)
    }
  }

  test("held-out response mutant changes evaluation only, never training or predictions") {
    val a = get(TemplateFitGuide.evaluate(prepared().savedRecipe, TemplateFitReference.receipt))
    val b =
      get(TemplateFitGuide.evaluate(prepared(700.0).savedRecipe, TemplateFitReference.receipt))
    assertEquals(a.trainingHash, b.trainingHash)
    assertNotEquals(a.heldOutHash, b.heldOutHash)
    assertEqualsDouble(get(b.rows.head.result)._1, 7.0, LinearFitTolerance)
    assertEqualsDouble(get(b.rows.head.result)._2, 693.0, LinearFitTolerance)
    assertEqualsDouble(get(b.meanSquaredError), 480249.0, LinearFitTolerance)
  }

  test(
    "import refuses stale hashes, rank/row mismatches, duplicate features and malformed receipts"
  ) {
    val source = get(Rfc4180.decode(TemplateFitReference.receipt))
    def reject(update: Vector[Vector[String]] => Vector[Vector[String]]) =
      assert(TemplateFitCsv.importFit(split().training, Rfc4180.encode(update(source))).isLeft)
    reject(_.updated(1, source(1).updated(1, "0000000000000000")))
    reject(_.updated(1, source(1).updated(4, "1")))
    reject(_.updated(1, source(1).updated(5, "5")))
    reject(_.updated(1, source(1).updated(3, "NaN")))
    reject(_.updated(1, source(1).updated(7, "0.1")))
    reject(_.updated(2, source(2).updated(2, source(1)(2))))
    reject(_.dropRight(1))
    reject(_ :+ source(1))
    reject(_.updated(1, Vector("too short")))
    reject(_.updated(0, source.head.reverse))
    assert(TemplateFitCsv.importFit(split().training, "\"unterminated").isLeft)
  }

  test(
    "recipe roundtrip retains fold assignment, typed keys, data and refuses stale versions/hashes"
  ) {
    val codec    = get(TemplateFitGuide.codec)
    val json     = get(codec.encode(split()))
    val restored = get(codec.decode(json))
    assertEquals(restored.heldOutFolds, Set("test"))
    assertEquals(restored.rows.map(_.key), Vector("a", "b", "c", "d", "e"))
    assertEquals(TemplateFitCsv.training(restored.training), prepared().trainingCsv)
    val payload = json.hcursor.downField("value").focus.get
    val altered = payload.mapObject(_.add("trainingHash", Json.fromString("bad")))
    assert(codec.decode(json.mapObject(_.add("value", altered))).isLeft)
    val wrongMethod = payload.mapObject(_.add("method", Json.fromString("with-intercept")))
    assert(codec.decode(json.mapObject(_.add("value", wrongMethod))).isLeft)
    val wrongVersion = json.mapObject(
      _.add(
        "schema",
        Json.obj("name" -> Json.fromString(codec.schema.name), "version" -> Json.fromInt(2))
      )
    )
    assert(codec.decode(wrongVersion).isLeft)
  }

  test("the fitting export does not accept a held-out capability") {
    assert(typeCheckErrors("""
      import eyes4s.io.*
      import eyes4s.design.*
      def leak(h: TemplateHeldOut[String]): String = TemplateFitCsv.training(h)
    """).nonEmpty)
  }

  test("native guide saves, reopens and evaluates without an external receipt") {
    val saved      = get(TemplateFitGuide.saveNative(split()))
    val evaluation = get(TemplateFitGuide.evaluateNative(saved))
    assertEqualsDouble(get(evaluation.rows.head.result)._1, 7.0, LinearFitTolerance)
    val changed =
      get(TemplateFitGuide.evaluateNative(get(TemplateFitGuide.saveNative(split(700.0)))))
    assertEquals(evaluation.trainingHash, changed.trainingHash)
    assertEqualsDouble(get(changed.rows.head.result)._2, 693.0, LinearFitTolerance)
    assert(get(TemplateFitGuide.codec).parse(saved).isLeft)
    assert(get(TemplateFitGuide.nativeCodec).parse(prepared().savedRecipe).isLeft)
  }

  test("native recipe refuses forged method and fitting conventions") {
    val codec = get(TemplateFitGuide.nativeCodec)
    val json  = get(codec.encode(split()))
    val value = json.hcursor.downField("value").focus.get
    Vector(
      "method"        -> Json.fromString(eyes4s.design.FittedTemplate.method),
      "intercept"     -> Json.fromBoolean(true),
      "rankTolerance" -> Json.fromDoubleOrNull(1e-7),
      "trainingHash"  -> Json.fromString("bad")
    ).foreach { (field, replacement) =>
      assert(
        codec
          .decode(json.mapObject(_.add("value", value.mapObject(_.add(field, replacement)))))
          .isLeft
      )
    }
  }
