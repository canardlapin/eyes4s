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

import cats.syntax.all.*
import eyes4s.codec.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import io.circe.Json
import org.scalacheck.Gen

/** The one template recipe codec under its conventional schema
  * `eyes4s.template-recipe`, as the pinned fixtures use it.
  */
object TemplateRecipeCodecs:
  def get[E, A](v: Either[E, A]): A =
    v.fold(e => throw new IllegalStateException(e.toString), identity)
  val schema: DefinitionId         = get(DefinitionId.of("eyes4s.template-recipe", 1))
  val keys: VersionedCodec[String] =
    VersionedCodec.string(get(DefinitionId.of("example.trial-key", 1)))
  val features: SchemaLadder[TemplateSplit[String, Vector[Double]]] =
    TemplateRecipeCodec.ladder[String, Vector[Double]](schema, keys)
  val maps: SchemaLadder[TemplateSplit[String, Mass[Px]]] =
    TemplateRecipeCodec.ladder[String, Mass[Px]](schema, keys)

  /** The codec that reads `document`: map recipes carry the mean-map method. */
  def of(document: Json): VersionedCodec[?] =
    val method = document.hcursor.downField("value").get[String]("method").toOption
    if method.contains(TemplateDesign.meanMapMethod) then maps.codec else features.codec

  /** Splits with the same design, rows, partition and identities. */
  def same[X](a: TemplateSplit[String, X], b: TemplateSplit[String, X]): Boolean =
    a.design.method == b.design.method && a.design.featureNames == b.design.featureNames &&
      a.design.responseUnit == b.design.responseUnit && a.heldOutGroups == b.heldOutGroups &&
      a.rows.map(r => (r.key, r.splitGroup, r.matchGroup, r.response)) ==
      b.rows.map(r => (r.key, r.splitGroup, r.matchGroup, r.response)) &&
      a.training.hash == b.training.hash && a.heldOut.hash == b.heldOut.hash &&
      a.excluded.map(_.row.key) == b.excluded.map(_.row.key)

  /** The fixed-feature split with match groups pinned as version 2. */
  val grouped: TemplateSplit[String, Vector[Double]] = get(
    for
      basis <- TemplateBasis.of("fixed-template-features/1", Vector("template-a"), "score")
      rows  <- Vector(
        ("a", "train", Some("item-1"), 1.0, 1.0),
        ("b", "train", Some("item-2"), 2.0, 2.0),
        ("c", "train", Some("item-3"), 3.0, 3.1),
        ("d", "train", None, 4.0, 3.9),
        ("e", "test", Some("item-3"), 5.0, 5.0)
      ).traverse((k, g, m, x, y) => TemplateObservation.of(k, g, Vector(x), y, m))
      split <- TemplateSplit.of(TemplateDesign.fixed(basis), rows, Set("test"))
    yield split
  )

class TemplateRecipeLawSuite extends munit.DisciplineSuite:
  import TemplateRecipeCodecs.*

  /** Rows by construction: the first row is training with a match group no
    * held-out row uses, and the last is held out, so every generated split is
    * admitted and nothing is discarded. Middle rows fall in either partition,
    * and a training row may share the held-out match group and be excluded.
    */
  private def layout(n: Int): Gen[Vector[(String, String)]] =
    Gen
      .listOfN(
        n - 2,
        for
          group   <- Gen.oneOf("train-a", "train-b", "held")
          matched <- Gen.oneOf("item-held", "item-a", "item-b")
        yield (group, matched)
      )
      .map(middle => ("train-a", "item-first") +: middle.toVector :+ ("held", "item-held"))

  private val fixedSplits: Gen[TemplateSplit[String, Vector[Double]]] = for
    width   <- Gen.choose(1, 3)
    n       <- Gen.choose(3, 8)
    grouped <- Gen.oneOf(false, true)
    native  <- Gen.oneOf(false, true)
    groups  <- layout(n)
    inputs  <- Gen.listOfN(
      n,
      for
        xs <- Gen.listOfN(width, Gen.choose(-100.0, 100.0))
        y  <- Gen.choose(-1000.0, 1000.0)
      yield (xs.toVector, y)
    )
  yield
    val basis  = get(TemplateBasis.of("basis/1", Vector.tabulate(width)(i => s"f$i"), "score"))
    val design =
      if native then TemplateDesign.fixed(basis) else TemplateDesign.importedLm(basis)
    val rows = groups.zip(inputs).zipWithIndex.map { case (((g, m), (x, y)), i) =>
      get(TemplateObservation.of(s"k$i", g, x, y, Option.when(grouped)(m)))
    }
    get(TemplateSplit.of(design, rows, Set("held")))

  private val grid = get(Grid.over(get(Frame.screen("template-laws", 2, 2)), 2, 2))
  private val mapSplits: Gen[TemplateSplit[String, Mass[Px]]] = for
    n      <- Gen.choose(3, 7)
    groups <- layout(n)
    inputs <- Gen.listOfN(
      n,
      for
        values <- Gen.listOfN(4, Gen.choose(0.01, 1.0))
        y      <- Gen.choose(-10.0, 10.0)
      yield (values, y)
    )
  yield
    val design = get(TemplateDesign.meanMap[Px]("participant", "score"))
    val rows   = groups.zip(inputs).zipWithIndex.map { case (((g, m), (v, y)), i) =>
      val values = IArray.from(v.map(_ / v.sum))
      val mass   = get(Surface.mass(grid, values, Provenance.raw(ContentHash.of(values))))
      get(TemplateObservation.of(s"k$i", g, mass, y, Some(m)))
    }
    get(TemplateSplit.of(design, rows, Set("held")))

  checkAll("fixed template recipe", CodecLaws.roundTrip(features.codec, fixedSplits, same))
  checkAll("map template recipe", CodecLaws.roundTrip(maps.codec, mapSplits, same))
  checkAll(
    "grouped template recipe",
    CodecLaws.roundTrip(features.codec, Gen.const(grouped), same)
  )
  checkAll("template recipe versions", SchemaLadderLaws.ladder(features, fixedSplits, same))
  checkAll("map template recipe versions", SchemaLadderLaws.ladder(maps, mapSplits, same))

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

  test("the ladder laws kill a first version that claims to express match groups") {
    val v1      = features.versions.head
    val v2      = features.versions(1)
    val claimed = SchemaLadder
      .of[TemplateSplit[String, Vector[Double]]](features.role, v1)(features.writeAt(v1, _))(
        features.readAt(v1, _)
      )
      .next(_ => true, identity)(features.writeAt(v2, _))(features.readAt(v2, _))
    val parameters = org.scalacheck.Test.Parameters.default
      .withMinSuccessfulTests(100)
      .withInitialSeed(0x74656d706c617465L)
    def failed(ladder: SchemaLadder[TemplateSplit[String, Vector[Double]]]): Boolean =
      SchemaLadderLaws
        .ladder(ladder, fixedSplits, same)
        .all
        .properties
        .exists((_, prop) => !org.scalacheck.Test.check(parameters, prop).passed)
    assert(!failed(features))
    assert(failed(claimed))
  }
