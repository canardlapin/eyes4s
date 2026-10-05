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

import eyes4s.kernel.Unit2D.Px
import io.circe.Json

/** A described document digests exactly as its JSON does: the same bytes,
  * the same refusal at the same path, each array item made once, in order,
  * and dropped before the next. Pinned vectors fix the study-result digests,
  * so the JVM and Scala.js are held to the same values.
  */
class CanonicalDocSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(x => fail(s"$x"), identity)

  import CanonicalDoc.{Arr, Leaf, Obj}

  private def documented(doc: CanonicalDoc): Either[CodecError, String] =
    doc.json.flatMap(CanonicalDigest.document[Json]).map(_.sha256.hex)
  private def streamed(doc: CanonicalDoc): Either[CodecError, String] =
    CanonicalDigest.streamed[Json](doc).map(_.sha256.hex)

  private val nested: CanonicalDoc = CanonicalDoc.obj(
    "a"     -> Leaf(Json.fromInt(1)),
    "empty" -> Arr(0, _ => Left(CodecError.Field("never", Json.Null, "never made"))),
    "rows"  -> Arr(
      3,
      i =>
        Right(
          CanonicalDoc.obj(
            "i"    -> Leaf(Json.fromInt(i)),
            "x"    -> Leaf(Json.fromDoubleOrNull(i * 0.1)),
            "big"  -> Leaf(Json.fromLong(Long.MaxValue - i)),
            "s"    -> Leaf(Json.fromString(s"row-${i}\u00e9")),
            "deep" -> Arr(
              i,
              j => Right(Leaf(Json.arr(Json.fromBoolean(j % 2 == 0), Json.Null)))
            )
          )
        )
    ),
    "obj"  -> Obj(scala.collection.immutable.ListMap.empty),
    "leaf" -> Leaf(Json.obj("k" -> Json.arr(Json.fromDoubleOrNull(-0.0))))
  )

  test("a described document digests as its JSON, nested arrays and objects included") {
    assertEquals(streamed(nested), documented(nested))
    assertEquals(streamed(Leaf(Json.Null)), documented(Leaf(Json.Null)))
    assertEquals(streamed(Arr(0, _ => Right(Leaf(Json.Null)))), documented(Leaf(Json.arr())))
    // Members are not reordered: a described object is its members in order.
    assertNotEquals(
      streamed(CanonicalDoc.obj("a" -> Leaf(Json.True), "b" -> Leaf(Json.False))),
      streamed(CanonicalDoc.obj("b" -> Leaf(Json.False), "a" -> Leaf(Json.True)))
    )
    // An array of described items is the array of their JSON, not one leaf.
    assertNotEquals(
      streamed(Arr(1, _ => Right(Leaf(Json.arr())))),
      streamed(Leaf(Json.arr()))
    )
  }

  test("an unrepresentable number is refused at the same path either way") {
    val number = Json.fromBigDecimal(BigDecimal("0.10000000000000001"))
    val doc    = CanonicalDoc.obj(
      "rows" -> Arr(
        2,
        i => Right(Leaf(Json.obj("v" -> (if i == 1 then number else Json.fromInt(1)))))
      )
    )
    val expected = doc.json.flatMap(CanonicalDigest.document[Json]).map(_.sha256.hex)
    assertEquals(streamed(doc), expected)
    assert(
      expected.left.exists {
        case CodecError.Unsupported("$.value.rows[1].v", _) => false
        case CodecError.Unsupported("$.rows[1].v", _)       => true
        case _                                              => false
      },
      expected
    )
  }

  test("a failure making an item fails the document, and mapError locates it") {
    val failing = CanonicalDoc
      .obj(
        "rows" -> Arr(
          3,
          i =>
            if i == 1 then Left(CodecError.Field("x", Json.Null, "bad"))
            else Right(Leaf(Json.fromInt(i)))
        )
      )
      .mapError(Wire.at("outer"))
    val expected = Left(CodecError.Entry("outer", CodecError.Field("x", Json.Null, "bad")))
    assertEquals(failing.json, expected)
    assertEquals(CanonicalDigest.streamed[Json](failing), expected)
  }

  test("an object holds a key once, so its JSON and its rendering agree") {
    val pairs = Vector("a" -> Json.True, "b" -> Json.Null, "a" -> Json.False)
    val doc   = CanonicalDoc.obj(pairs.map((k, v) => k -> (Leaf(v): CanonicalDoc))*)
    doc match
      case Obj(members) => assertEquals(members.keys.toVector, Vector("a", "b"))
      case other        => fail(s"$other")
    // As a JSON object built from the same pairs: first position, last value.
    assertEquals(doc.json, Right(Json.fromFields(pairs)))
    assertEquals(doc.json, Right(Json.obj("a" -> Json.False, "b" -> Json.Null)))
    assertEquals(streamed(doc), documented(doc))
  }

  test("mapError reaches a failure inside an object inside an array, once") {
    val e   = CodecError.Field("x", Json.Null, "bad")
    val doc = Arr(1, _ => Right(CanonicalDoc.obj("x" -> Arr(1, _ => Left(e)))))
      .mapError(Wire.at("outer"))
    // The outer item is made; the failure is the nested array's item, which
    // the outer mapError reaches through the object and maps exactly once.
    val expected = Left(Wire.at("outer")(e))
    assertEquals(doc.json, expected)
    assertEquals(CanonicalDigest.streamed[Json](doc), expected)
    assertNotEquals[Either[CodecError, Json], Either[CodecError, Json]](doc.json, Left(e))
  }

  test("every fixture result and policy digests as its encoded archive") {
    import StudyResultFixtures.*
    val cosine = StudyResultCodecs.cosine[Px].codec
    val runs   = for
      input  <- Vector(mixed, clean)
      policy <- Vector(requireAll, successfulOnly)
      est    <- Vector(scales, scales.take(1))
    yield cosinePlan(input, policy, est).run(input)
    val two = for
      input  <- Vector(mixed, clean)
      policy <- Vector(requireAll, successfulOnly)
    yield twoComponentPlan(input, policy).run(input)
    assertEquals(runs.count(_.isRight) + two.count(_.isRight) >= 8, true)
    runs.collect { case Right(r) => r }.foreach { r =>
      assertEquals(
        cosine.digest(r).map(_.sha256),
        cosine.encode(r).flatMap(CanonicalDigest.document[Json]).map(_.sha256)
      )
    }
    two.collect { case Right(r) => r }.foreach { r =>
      val c = twoComponentCodec.codec
      assertEquals(
        c.digest(r).map(_.sha256),
        c.encode(r).flatMap(CanonicalDigest.document[Json]).map(_.sha256)
      )
    }
  }

  test("each item is made once, in order, and a long array is streamed through") {
    var made = Vector.empty[Int]
    val doc  = Arr(5, i => { made = made :+ i; Right(Leaf(Json.fromInt(i))) })
    get(streamed(doc))
    assertEquals(made, Vector(0, 1, 2, 3, 4))
    // Two hundred thousand rows of a few hundred bytes each: none is retained.
    val long = Arr(
      200000,
      i => Right(Leaf(Json.obj("i" -> Json.fromInt(i), "pad" -> Json.fromString("x" * 200))))
    )
    assertEquals(streamed(long).map(_.length), Right(64))
  }

  // ---------------------------------------------------------------- study results

  private val results = StudyResultCodecs.cosine[Px]

  private def both(
      result: eyes4s.plan.StudyResult[
        eyes4s.plan.StudyKey,
        Px,
        eyes4s.compare.Similarity,
        eyes4s.design.SignedDifference
      ]
  ): (String, String) =
    (
      get(results.codec.digest(result)).sha256.hex,
      get(results.codec.encode(result).flatMap(CanonicalDigest.document[Json])).sha256.hex
    )

  test("the pinned result-v1 archive: its digest is the digest of its document") {
    val document      = get(io.circe.parser.parse(StudyResultFixtures.resultVersionOne))
    val result        = get(results.codec.decode(document))
    val (fast, whole) = both(result)
    assertEquals(fast, whole)
    assertEquals(fast, get(CanonicalDigest.document[Json](document)).sha256.hex)
    assertEquals(fast, PinnedResultV1)
  }

  test("study results with every failure family digest as their documents, pinned") {
    import StudyResultFixtures.*
    val digests = Vector(
      cosinePlan(mixed, requireAll)     -> mixed,
      cosinePlan(mixed, successfulOnly) -> mixed,
      cosinePlan(clean, requireAll)     -> clean
    ).map { (plan, input) =>
      val (fast, whole) = both(get(plan.run(input)))
      assertEquals(fast, whole)
      fast
    }
    assertEquals(digests, PinnedStudies)
  }

  test("encoding and digesting refuse an unencodable row with the same located error") {
    import StudyResultFixtures.*
    val refusing =
      VersionedCodec.checked[eyes4s.compare.Similarity](eyes4s.plan.DefinitionId.similarity)(
        _ => Left(CodecError.Field("probe", Json.Null, "refused"))
      )(_ => Left(CodecError.Field("probe", Json.Null, "never read")))
    val codec  = StudyCodecs.cosine[Px].results(refusing, StudyResultCodecs.signedDifference())
    val result = get(cosinePlan(clean, requireAll).run(clean))
    val error  = codec.codec.encode(result).swap.getOrElse(fail("encoded"))
    assertEquals(codec.codec.digest(result).swap.toOption, Some(error))
    assert(
      error.message.startsWith("At scales[0].analyses.matched.source.rows[0]: "),
      error.message
    )
  }

  // Computed by origin/main's whole-document digest before this change.
  private val PinnedResultV1 =
    "c1cda37abdd2b927c9cc147f8e20a591e035d51a715819bd50fb88c041c98a8f"
  private val PinnedStudies = Vector(
    "cec04473407178f222f87f716d7bfd3d7187067ec6e00eafe5d16acec711f9c4",
    "3aa5ca67cc3500fdae38b4c99402a848ffed372371a35b1d59d2b84a6c952559",
    "007e153d69001202ceed434489115b1a12c9a999875ad6483eab9e60c5ef3135"
  )
