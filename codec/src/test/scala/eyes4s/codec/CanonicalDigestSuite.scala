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
import eyes4s.plan.DefinitionId
import io.circe.Json

/** The persisted identity of a value is the SHA-256 of its canonical
  * document, the same on the JVM and Scala.js, typed by what it identifies.
  */
class CanonicalDigestSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)

  /** Numbers the JVM and Scala.js print differently, a non-ASCII string, a
    * signed zero and a 64-bit integer beyond 2^53.
    */
  private val probe = Json.obj(
    "small"  -> Json.fromDoubleOrNull(1e-7),
    "large"  -> Json.fromDoubleOrNull(1e21),
    "whole"  -> Json.fromDoubleOrNull(3.0),
    "count"  -> Json.fromInt(3),
    "zero"   -> Json.fromDoubleOrNull(-0.0),
    "tenth"  -> Json.fromDoubleOrNull(0.1),
    "big"    -> Json.fromLong(9007199254740993L),
    "text"   -> Json.fromString("é\u0000z"),
    "list"   -> Json.arr(Json.Null, Json.True, Json.False, Json.arr(), Json.obj()),
    "nested" -> Json.obj("a" -> Json.fromInt(1))
  )
  private val codec =
    VersionedCodec.of[Json](get(DefinitionId.of("test.probe", 1)))(identity)(Right(_))

  test("the digest is pinned, so the JVM and Scala.js agree on it") {
    val digest = get(codec.digest(probe))
    assertEquals(
      digest.display,
      "sha256:0903e0e3ed3cde2b08810207e2e813b52ffa707b9f22cf1f9dfe9306ffd9b05d"
    )
    assertEquals(get(CanonicalDigest.parse[Json](digest.sha256.hex)), digest)
    assertEquals(digest.toString, digest.display)
    // Printed and re-parsed text renders the same numbers, whatever the platform
    // printed. Scala.js prints -0.0 as 0 and parses integers as doubles, so the
    // signed zero and the integer beyond 2^53 are left out here.
    val unsigned = probe.mapObject(_.remove("zero").remove("big"))
    val reparsed = get(io.circe.parser.parse(unsigned.noSpaces))
    assertEquals(get(codec.digest(reparsed)), get(codec.digest(unsigned)))
  }

  test("the rendering is prefix-free: nearby documents have different digests") {
    val digests = Vector(
      Json.arr(Json.fromString("ab"), Json.fromString("c")),
      Json.arr(Json.fromString("a"), Json.fromString("bc")),
      Json.arr(Json.arr(Json.fromString("a")), Json.fromString("bc")),
      Json.obj("a" -> Json.fromString("b")),
      Json.obj("b" -> Json.fromString("a")),
      Json.fromDoubleOrNull(0.0),
      Json.fromDoubleOrNull(-0.0),
      Json.fromLong(9007199254740993L),
      Json.fromLong(9007199254740992L),
      Json.Null,
      Json.False
    ).map(json => get(codec.digest(json)))
    assertEquals(digests.distinct.size, digests.size)
  }

  test("a value's digest survives decoding, lifting and re-encoding, and a change moves it") {
    val studies  = StudyCodecs.cosine[Px]
    val document = get(io.circe.parser.parse(SavedStudyFixtures.versionOne))
    val plan     = get(studies.codec.decode(document))
    val digest   = get(studies.codec.digest(plan))
    val lifted   = get(studies.codec.decode(get(studies.ladder.lift(document))))
    assert(get(studies.codec.digest(lifted)).sameAs(digest))
    val inputs = StudyInputCodecs.study[Px]
    val input  = get(inputs.input.digest(InputPayloadFixtures.temporalStudy))
    val other  = get(inputs.input.digest(InputPayloadFixtures.sourceSupportedStudy))
    assert(!input.sameAs(other))
    assertEquals(input.sha256.hex.length, 64)
  }

  test("a plan digest cannot be compared with an input digest") {
    assert(
      compileErrors(
        """
        val plan: CanonicalDigest[String] = ???
        val input: CanonicalDigest[Int] = ???
        plan.sameAs(input)
        """
      ).contains("Found:")
    )
  }
