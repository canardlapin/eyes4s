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

import io.circe.Json
import org.scalacheck.{Gen, Prop}
import CanonicalDoc.{Arr, Leaf}

/** A described document digests as its JSON across generated documents:
  * nested arrays and objects, every number form, refused numbers and failing
  * items (one at most per document, so both report the same failure).
  */
class CanonicalDocPropertySuite extends munit.ScalaCheckSuite:
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(300)

  private val strings: Gen[String] = Gen.oneOf(
    Gen.const(""),
    Gen.const("é中😀"),
    Gen.const("\ud800"), // lone surrogate
    Gen.const("\u0000\n\"\\"),
    Gen.alphaStr,
    Gen.listOf(Gen.choose(0.toChar, 0xffff.toChar)).map(_.mkString)
  )
  private val numbers: Gen[Json] = Gen.oneOf(
    Gen
      .oneOf(
        -0.0,
        0.0,
        Double.MinPositiveValue,
        -Double.MinPositiveValue,
        Double.MaxValue,
        -Double.MaxValue,
        1e-300,
        1e300,
        0.1,
        9007199254740993.0
      )
      .map(Json.fromDoubleOrNull),
    Gen.oneOf(Long.MinValue, Long.MaxValue, 0L, -1L, (1L << 53) + 1).map(Json.fromLong),
    Gen.oneOf(Double.NaN, Double.PositiveInfinity).map(Json.fromDoubleOrNull),
    Gen.choose(-1e12, 1e12).map(Json.fromDoubleOrNull),
    Gen.chooseNum(Long.MinValue, Long.MaxValue).map(Json.fromLong)
  )
  private val refused: Gen[Json] =
    Gen
      .oneOf("0.10000000000000001", "1e400", "18446744073709551616")
      .map(s => Json.fromBigDecimal(BigDecimal(s)))
  private def json(depth: Int, refuse: Boolean): Gen[Json] =
    val leaves = Gen.frequency(
      3                         -> numbers,
      3                         -> strings.map(Json.fromString),
      1                         -> Gen.oneOf(Json.Null, Json.True, Json.False),
      (if refuse then 1 else 0) -> refused
    )
    if depth == 0 then leaves
    else
      Gen.frequency(
        4 -> leaves,
        1 -> Gen.listOfN(3, json(depth - 1, refuse)).map(Json.fromValues),
        1 -> Gen
          .listOfN(3, Gen.zip(strings, json(depth - 1, refuse)))
          .map(kv => Json.fromFields(kv.distinctBy(_._1)))
      )
  private def doc(depth: Int, refuse: Boolean, fail: Boolean): Gen[CanonicalDoc] =
    val leaf = json(1, refuse).map(Leaf(_))
    if depth == 0 then leaf
    else
      val item: Gen[CanonicalDoc] = doc(depth - 1, refuse, fail)
      Gen.frequency(
        2 -> leaf,
        2 -> Gen
          .choose(0, 4)
          .flatMap(n =>
            Gen.listOfN(n, item).flatMap { items =>
              Gen.choose(-1, if fail then n else -1).map { bad =>
                Arr(
                  n,
                  i =>
                    if i == bad then Left(CodecError.Field(s"bad$i", Json.Null, "made to fail"))
                    else Right(items(i))
                )
              }
            }
          ),
        2 -> Gen
          .listOf(Gen.zip(strings, item))
          .map(kv => CanonicalDoc.obj(kv.take(4)*))
      )

  private def documented(d: CanonicalDoc) =
    d.json.flatMap(CanonicalDigest.document[Json]).map(_.sha256.hex)
  private def streamed(d: CanonicalDoc) = CanonicalDigest.streamed[Json](d).map(_.sha256.hex)

  property("streamed == document(json): no refusal, no failure") {
    Prop.forAll(doc(4, false, false))(d => assertEquals(streamed(d), documented(d)))
  }
  property("streamed == document(json): refusals, same path") {
    Prop.forAll(doc(4, true, false))(d => assertEquals(streamed(d), documented(d)))
  }
  property("streamed == document(json): item failures (single at most)") {
    Prop.forAll(doc(4, false, true))(d => assertEquals(streamed(d), documented(d)))
  }
