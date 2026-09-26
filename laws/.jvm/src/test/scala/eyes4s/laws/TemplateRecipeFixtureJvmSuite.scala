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

package eyes4s.laws

import eyes4s.design.*
import io.circe.Json

/** The pinned template recipes: those the three earlier codecs wrote, and a
  * version-2 recipe with match groups. JVM-only, as it reads test resources.
  */
class TemplateRecipeFixtureJvmSuite extends munit.FunSuite:
  import TemplateRecipeCodecs.*

  /** The exact fixtures' analytic coefficients, to rounding. */
  private val FitTolerance = 1e-12

  private def resource(name: String): Json =
    val stream = getClass.getResourceAsStream(s"/eyes4s/$name")
    assert(stream != null, s"missing pinned fixture $name")
    val text =
      try new String(stream.readAllBytes(), "UTF-8")
      finally stream.close()
    get(io.circe.parser.parse(text))

  test("recipes saved by the three earlier codecs decode unchanged and refit") {
    val native = resource("template-recipe-v1.json")
    val split  = get(features.codec.decode(native))
    assertEquals(split.design.method, TemplateDesign.nativeMethod)
    assertEquals(get(features.codec.encode(split)), native)
    val fitted = get(Template.fit(split.training))
    fitted.coefficients
      .zip(Vector(1.0, 2.0))
      .foreach((a, b) => assertEqualsDouble(a, b, FitTolerance))

    val lm      = resource("template-recipe-lm-v1.json")
    val history = get(features.codec.decode(lm))
    assertEquals(history.design.method, TemplateDesign.importedLmMethod)
    assertEquals(get(features.codec.encode(history)), lm)
    assertEquals(history.training.hash, split.training.hash)
    assertEquals(
      Template.fit(history.training).left.toOption,
      Some(TemplateError.Route(TemplateDesign.importedLmMethod, "native fit"))
    )

    val mapped  = resource("template-recipe-maps-v1.json")
    val learned = get(maps.codec.decode(mapped))
    assertEquals(get(maps.codec.encode(learned)), mapped)
    assertEquals(learned.excluded.map(_.row.key), Vector("excluded"))
    assertEqualsDouble(
      get(Template.fit(learned.training)).coefficients.head,
      math.sqrt(5),
      FitTolerance
    )
    // A map recipe is not a feature recipe, and the reverse.
    assert(features.codec.decode(mapped).isLeft)
    assert(maps.codec.decode(native).isLeft)
  }

  test("a fixed-feature split with match groups is version 2 and excludes held-out items") {
    val written = get(features.codec.encode(grouped))
    assertEquals(written, resource("template-recipe-grouped-v2.json"))
    assertEquals(grouped.excluded.map(_.row.key), Vector("c"))
    assertEquals(grouped.training.rows.map(_.key), Vector("a", "b", "d"))
    // The first version cannot express it, and refuses its rows.
    assert(features.upTo(features.versions.head).flatMap(_.codec.decode(written)).isLeft)
    val v1 = written.mapObject(
      _.add(
        "schema",
        Json.obj(
          "name"    -> Json.fromString(schema.name),
          "version" -> Json.fromInt(1)
        )
      )
    )
    assert(features.codec.decode(v1).isLeft)
  }
