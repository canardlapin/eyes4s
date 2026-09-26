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

import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.DefinitionId
import io.circe.Json

class LearnedTemplateRecipeSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val grid  = get(Grid.over(get(Frame.screen("template", 2, 1)), 2, 1))
  private val codec = LearnedTemplateRecipeCodec.of[String, Px](
    get(DefinitionId.of("test.learned-template", 1)),
    VersionedCodec.string(get(DefinitionId.of("test.key", 1)))
  )
  private def row(
      key: String,
      group: String,
      matched: String,
      values: Vector[Double],
      response: Double
  ) =
    val mass = get(
      Surface.mass(grid, IArray.from(values), Provenance.raw(ContentHash.ofString(key)))
    )
    get(MapTemplateObservation.of(key, group, matched, mass, response))
  private val split = get(
    MapTemplateSplit.of(
      Vector(
        row("a", "p1", "a", Vector(1, 0), 2),
        row("b", "p2", "b", Vector(1, 0), 2),
        row("c", "p3", "c", Vector(0, 1), 1),
        row("excluded", "p1", "held", Vector(0, 1), 900),
        row("held", "p4", "held", Vector(0.5, 0.5), 7)
      ),
      Set("p4"),
      "participant",
      "score"
    )
  )

  test("saved learned inputs reopen, reconstruct exclusions and reproduce the complete fit") {
    val encoded = get(codec.encode(split))
    assertEquals(encoded, get(io.circe.parser.parse(LearnedTemplatePortableFixture.versionOne)))
    assertEquals(
      get(codec.parse(LearnedTemplatePortableFixture.versionOne)).training.hash,
      split.training.hash
    )
    val restored = get(codec.parse(encoded.noSpaces))
    assertEquals(restored.training.hash, split.training.hash)
    assertEquals(restored.heldOut.hash, split.heldOut.hash)
    assertEquals(restored.excluded.map(_.row.key), Vector("excluded"))
    assertEquals(restored.rows.map(_.key), split.rows.map(_.key))
    val a = get(LearnedTemplate.fit(split.training));
    val b = get(LearnedTemplate.fit(restored.training))
    assertEquals(a.mean.values.toVector, b.mean.values.toVector)
    assertEquals(a.mean.provenance.digest, b.mean.provenance.digest)
    assertEquals(a.slope, b.slope)
    assertEquals(get(a.evaluate(split.heldOut)).rows, get(b.evaluate(restored.heldOut)).rows)
    assertEquals(get(codec.encode(restored)), encoded)
  }

  test(
    "method, hash, split rules, frame units and invalid numerical payloads cannot be forged"
  ) {
    val encoded               = get(codec.encode(split));
    val value                 = encoded.hcursor.downField("value").focus.get
    def reject(v: Json): Unit =
      assert(codec.decode(encoded.mapObject(_.add("value", v))).isLeft)
    reject(value.mapObject(_.add("method", Json.fromString("other"))))
    reject(value.mapObject(_.add("trainingHash", Json.fromString("stale"))))
    reject(
      value.mapObject(
        _.add("heldOutGroups", Json.arr(Json.fromString("p4"), Json.fromString("p4")))
      )
    )
    val rows = value.hcursor.get[Vector[Json]]("rows").toOption.get
    val bad  = rows.head.mapObject(_.add("values", Json.arr(Json.fromInt(-1), Json.fromInt(2))))
    reject(value.mapObject(_.add("rows", Json.arr((bad +: rows.tail)*))))
    reject(value.mapObject(_.add("rows", Json.arr((rows :+ rows.head)*))))
    val angularCodec = LearnedTemplateRecipeCodec.of[String, Unit2D.Deg](
      codec.schema,
      VersionedCodec.string(get(DefinitionId.of("test.key", 1)))
    )
    assert(angularCodec.decode(encoded).isLeft)
  }

  test("held-out groups have one canonical wire order; unsorted input is refused") {
    val twoHeld = get(
      MapTemplateSplit.of(
        Vector(
          row("a", "p1", "a", Vector(1, 0), 2),
          row("b", "p2", "b", Vector(1, 0), 2),
          row("c", "p3", "c", Vector(0, 1), 1),
          row("held", "p4", "held", Vector(0.5, 0.5), 7)
        ),
        Set("p4", "p3"),
        "participant",
        "score"
      )
    )
    val encoded = get(codec.encode(twoHeld))
    val value   = encoded.hcursor.downField("value").focus.get
    assertEquals(
      value.hcursor.get[Vector[String]]("heldOutGroups"),
      Right(Vector("p3", "p4"))
    )
    val reversed = value.mapObject(
      _.add("heldOutGroups", Json.arr(Json.fromString("p4"), Json.fromString("p3")))
    )
    assert(
      codec.decode(encoded.mapObject(_.add("value", reversed))) match
        case Left(CodecError.Field("heldOutGroups", _, reason)) => reason.contains("ascending")
        case _                                                  => false
    )
  }
