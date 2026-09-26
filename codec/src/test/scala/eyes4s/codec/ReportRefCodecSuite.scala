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

import eyes4s.codec.ReportRefCodecs.given
import eyes4s.results.*
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax.*

/** The JSON form of report references: pinned on the JVM and Scala.js, an
  * exact round trip, and refusals naming what they found.
  */
class ReportRefCodecSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)
  private def json(text: String): Json      = get(parse(text))
  private def decoded(text: String)         = json(text).as[ReportRef]

  test("the pinned document holds a cell and a participant of it") {
    import ReportRefFixtures.*
    assertEquals(document.spaces2, versionOne)
    val cursor = json(versionOne).hcursor
    assertEquals(get(cursor.downField("cell").as[ReportRef]), cell)
    assertEquals(get(cursor.downField("participant").as[ReportRef]), participant)
  }

  test("every role and an empty group round-trip exactly") {
    Role.values.foreach { role =>
      val cell = get(ReportRef.cell(3, GroupKey.all, role, "overlap"))
      assertEquals(decoded((cell: ReportRef).asJson.noSpaces), Right(cell))
      val p = get(ReportRef.participant(cell, "p2"))
      assertEquals(decoded((p: ReportRef).asJson.noSpaces), Right(p))
    }
    assertEquals(
      (get(ReportRef.cell(1, GroupKey.all, Role.Matched, "value")): ReportRef).asJson.noSpaces,
      """{"cell":{"scale":1,"group":[],"role":"matched","component":"value"}}"""
    )
  }

  private def refused(text: String, message: String): Unit =
    decoded(text) match
      case Right(value) => fail(s"$text decoded to $value")
      case Left(error) => assert(error.message.contains(message), s"${error.message} for $text")

  private val cellText = """{"scale":0,"group":[],"role":"control","component":"value"}"""

  test("another level, a missing or extra member, a bad role, scale or name is refused") {
    refused("""{"row":{}}""", "single member 'cell' or 'participant'")
    refused("""[]""", "no object")
    refused("""{"cell":{"scale":0,"group":[],"role":"control"}}""", "Expected the members")
    refused(
      """{"cell":{"scale":0,"group":[],"role":"control","component":"value","x":1}}""",
      "found [scale, group, role, component, x]"
    )
    refused(
      """{"cell":{"scale":0,"group":[],"role":"treatment","component":"v"}}""",
      "Unknown role 'treatment'"
    )
    refused(
      """{"cell":{"scale":-1,"group":[],"role":"control","component":"v"}}""",
      "scale is 0 or more, not -1"
    )
    refused(
      """{"cell":{"scale":"0","group":[],"role":"control","component":"v"}}""",
      "expected a JSON number"
    )
    refused(
      """{"cell":{"scale":0,"group":[],"role":"control","component":" "}}""",
      "names a component"
    )
    refused(s"""{"participant":{"cell":$cellText,"name":""}}""", "A participant is named")
    refused(s"""{"participant":{"cell":$cellText}}""", "Expected the members [cell, name]")
    refused(
      """{"cell":{"scale":0,"group":[{"term":"t"}],"role":"control","component":"v"}}""",
      "[term, level]"
    )
  }
