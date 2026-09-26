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

import eyes4s.codec.RecordIdentityCodecs.given
import eyes4s.plan.*
import io.circe.{Decoder, Encoder, Json}
import io.circe.parser.parse
import io.circe.syntax.*
import org.scalacheck.Gen
import org.scalacheck.Prop.forAllNoShrink

/** The JSON forms of the record and fixation identities: pinned spellings on
  * the JVM and Scala.js, exact round trips, and refusals of any other
  * member, an extra member or a value outside the range.
  */
class RecordIdentityCodecSuite extends munit.ScalaCheckSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)
  private def json(text: String): Json      = get(parse(text))

  test("the pinned document holds the fixture's record 7,214 and its fixation 6") {
    assertEquals(RecordIdentityFixtures.document.spaces2, RecordIdentityFixtures.versionOne)
    val document = json(RecordIdentityFixtures.versionOne)
    val cursor   = document.hcursor
    assertEquals(
      get(cursor.downField("dataRecord").as[DataRecord]),
      RecordIdentityFixtures.record
    )
    assertEquals(
      get(cursor.downField("fixationNumber").as[FixationNumber]),
      RecordIdentityFixtures.number
    )
    assertEquals(
      get(cursor.downField("scanpathPosition").as[ScanpathPosition]),
      RecordIdentityFixtures.position
    )
  }

  test("each identity is one member named for its counting convention") {
    assertEquals(RecordIdentityFixtures.record.asJson.noSpaces, """{"data-record":7214}""")
    assertEquals(RecordIdentityFixtures.number.asJson.noSpaces, """{"fixation-number":6}""")
    assertEquals(RecordIdentityFixtures.position.asJson.noSpaces, """{"scanpath-position":5}""")
  }

  private def refused[A: Decoder](text: String, message: String): Unit =
    json(text).as[A] match
      case Right(value) => fail(s"$text decoded to $value")
      case Left(error) => assert(error.message.contains(message), s"${error.message} for $text")

  test(
    "another convention, an extra member, a non-object or an out-of-range value is refused"
  ) {
    refused[DataRecord]("""{"csv-record":7215}""", "single member 'data-record'")
    refused[DataRecord]("""{"data-record":7214,"line":7215}""", "members [data-record, line]")
    refused[DataRecord]("""7214""", "no object")
    refused[DataRecord]("""{"data-record":0}""", "Data record 0")
    refused[DataRecord]("""{"data-record":"7214"}""", "expected a JSON number, got \"7214\"")
    refused[DataRecord](
      """{"data-record":7214.5}""",
      "expected an integer spelled as one, got 7214.5"
    )
    refused[FixationNumber]("""{"scanpath-position":5}""", "single member 'fixation-number'")
    refused[FixationNumber]("""{"fixation-number":0}""", "Fixation number 0")
    refused[ScanpathPosition]("""{"fixation-number":6}""", "single member 'scanpath-position'")
    refused[ScanpathPosition]("""{"scanpath-position":-1}""", "Scanpath position -1")
  }

  private def roundTrips[A: Encoder: Decoder](value: A): Boolean =
    json(value.asJson.noSpaces).as[A] == Right(value)

  property("every identity round-trips exactly, at the ends of its range too") {
    forAllNoShrink(
      Gen.oneOf(
        Gen.oneOf(0, 1, ScanpathPosition.maximum),
        Gen.choose(0, ScanpathPosition.maximum)
      )
    ) { p =>
      val position = get(ScanpathPosition.of(p))
      val record   = get(DataRecord.of(math.min(p + 1, DataRecord.maximum)))
      roundTrips(position) && roundTrips(position.number) && roundTrips(record)
    }
  }
