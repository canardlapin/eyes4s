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

package eyes4s.codec

import eyes4s.plan.*
import io.circe.Json
import io.circe.syntax.*

/** `eyes4s.form-view@1` and `eyes4s.form-values@1` on both platforms: the
  * pinned mirrors decode to the fixtures and re-encode to the same JSON
  * value, the shipped forms round-trip, and every stored spelling a host
  * could hand-edit wrongly is refused with a located error.
  */
class FormCodecSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)
  private def parse(text: String): Json     = get(io.circe.parser.parse(text))

  test("the pinned mirrors decode to the fixtures and re-encode to the same value") {
    assertEquals(get(FormCodecs.view.decode(parse(FormMirrors.view))), FormFixtures.view)
    assertEquals(get(FormCodecs.view.encode(FormFixtures.view)), parse(FormMirrors.view))
    assertEquals(get(FormCodecs.values.decode(parse(FormMirrors.values))), FormFixtures.values)
    assertEquals(get(FormCodecs.values.encode(FormFixtures.values)), parse(FormMirrors.values))
  }

  test("the shipped method forms round-trip, and the restored view checks as the original") {
    val views = Vector(
      FormView(
        DefinitionId.cosine,
        Vector(RecipeParameters.forms.sigma[eyes4s.kernel.Unit2D.Deg].view)
      ),
      FormView(DefinitionId.study, new TemporalForm().views)
    )
    views.foreach { v =>
      val back = get(FormCodecs.view.decode(get(FormCodecs.view.encode(v))))
      assertEquals(back, v)
      v.fields.zip(back.fields).foreach { (a, b) =>
        Vector(
          RawValue.Number("0"),
          RawValue.Number("-1"),
          RawValue.Number("x"),
          RawValue.Absent
        )
          .foreach(raw => assertEquals(b.check(raw), a.check(raw), s"${a.id} $raw"))
      }
    }
  }

  private def value(document: Json): Json = get(
    document.hcursor.downField("value").focus.toRight("no value")
  )
  private def withValue(document: Json, v: Json): Json =
    document.mapObject(_.add("value", v))
  private val viewDoc   = parse(FormMirrors.view)
  private val valuesDoc = parse(FormMirrors.values)

  private def fields(v: Json): Vector[Json] = get(
    v.hcursor.downField("fields").as[Vector[Json]]
  )

  test("form values in another order, repeated or absent are refused") {
    val entries  = fields(value(valuesDoc))
    val reversed = withValue(valuesDoc, Json.obj("fields" -> Json.arr(entries.reverse*)))
    assert(
      FormCodecs.values.decode(reversed).left.exists {
        case CodecError.NonCanonical("fields", _, _, _) => true
        case _                                          => false
      },
      "reversed fields"
    )
    val repeated =
      withValue(valuesDoc, Json.obj("fields" -> Json.arr((entries.head +: entries)*)))
    assert(
      FormCodecs.values.decode(repeated).left.exists(_.message.contains("appears 2 times")),
      FormCodecs.values.decode(repeated).toString
    )
    val absent = Json.obj("id" -> "aaa".asJson, "value" -> Json.obj("kind" -> "absent".asJson))
    val withAbsent = withValue(valuesDoc, Json.obj("fields" -> Json.arr((absent +: entries)*)))
    assert(FormCodecs.values.decode(withAbsent).left.exists(_.message.contains("is absent")))
    val badId = Json.obj(
      "id"    -> "a b".asJson,
      "value" -> Json.obj("kind" -> "flag".asJson, "value" -> true.asJson)
    )
    assert(
      FormCodecs.values
        .decode(withValue(valuesDoc, Json.obj("fields" -> Json.arr(badId))))
        .left
        .exists(_.message.contains("a b"))
    )
    val unknown = Json.obj("id" -> "a".asJson, "value" -> Json.obj("kind" -> "date".asJson))
    assert(
      FormCodecs.values
        .decode(withValue(valuesDoc, Json.obj("fields" -> Json.arr(unknown))))
        .left
        .exists(_.message.contains("unknown value date"))
    )
  }

  private def field(name: String): Json =
    get(fields(value(viewDoc)).find(_.hcursor.get[String]("id").contains(name)).toRight(name))
  private def viewWith(fieldsJson: Vector[Json]): Json =
    withValue(viewDoc, value(viewDoc).mapObject(_.add("fields", Json.arr(fieldsJson*))))
  private def refusal(fieldsJson: Vector[Json]): String =
    FormCodecs.view.decode(viewWith(fieldsJson)).fold(_.message, v => fail(s"decoded $v"))

  test("a stored view the plan's constructors would refuse is refused") {
    val sigma                                              = field("sigma")
    def set(json: Json, path: List[String], v: Json): Json = path match
      case Nil       => v
      case h :: rest =>
        json.mapObject(o => o.add(h, set(o(h).getOrElse(Json.obj()), rest, v)))
    // Bounds out of order.
    val inverted = set(
      sigma,
      List("kind", "bounds"),
      Json.obj(
        "lower" -> Json.obj("kind" -> "closed".asJson, "value" -> 2.0.asJson),
        "upper" -> Json.obj("kind" -> "closed".asJson, "value" -> 1.0.asJson)
      )
    )
    assert(refusal(Vector(inverted)).contains("bounds"), refusal(Vector(inverted)))
    // A default the field's own bounds refuse.
    val badDefault = set(
      sigma,
      List("default", "raw"),
      Json.obj("kind" -> "number".asJson, "text" -> "0".asJson)
    )
    assert(refusal(Vector(badDefault)).contains("sigma"), refusal(Vector(badDefault)))
    // A numeric field must measure a number: a nominal or composite quantity is refused.
    Vector("nominal", "composite").foreach { q =>
      val named = set(sigma, List("kind", "quantity"), Json.obj("kind" -> q.asJson))
      assert(refusal(Vector(named)).contains(s"measures $q"), refusal(Vector(named)))
    }
    // A non-positive version, an unknown unit and an unknown kind.
    assert(refusal(Vector(sigma.mapObject(_.add("version", 0.asJson)))).contains("version 0"))
    val unit = set(sigma, List("kind", "quantity", "unit"), "furlong".asJson)
    assert(refusal(Vector(unit)).contains("furlong"), refusal(Vector(unit)))
    val kind = set(sigma, List("kind", "kind"), "slider".asJson)
    assert(refusal(Vector(kind)).contains("slider"), refusal(Vector(kind)))
    // An ordered rule naming a part the group does not have.
    val window = field("window")
    val rule   = set(
      window,
      List("kind", "rule", "pairs"),
      Json.arr(Json.arr("xMin".asJson, "yMax".asJson))
    )
    assert(refusal(Vector(rule)).contains("yMax"), refusal(Vector(rule)))
    // A repeated field id, on decoding and on encoding.
    assert(refusal(Vector(sigma, sigma)).contains("appears 2 times"))
    val twice =
      FormView(FormFixtures.view.definition, Vector.fill(2)(FormFixtures.view.fields.head))
    assert(FormCodecs.view.encode(twice).left.exists(_.message.contains("appears 2 times")))
  }

  test("a 64-bit raw number keeps its text exactly on both platforms") {
    val read = get(FormCodecs.values.decode(parse(FormMirrors.values)))
    assertEquals(
      read.values.get(get(FieldId.of("gap"))),
      Some(RawValue.Number("9007199254740993"))
    )
  }
