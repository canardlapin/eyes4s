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

/** Lifting reads a document with its own version first, so it never admits
  * a document that version refuses.
  */
class LadderLiftSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)

  test("a version-1 ledger naming a version-2 cause is refused by lift as by decode") {
    val ledgers = StudyInputCodecs.study[Px]
    val v1      = get(io.circe.parser.parse(StudyInputFixtures.ledgerVersionOne))
    val cause   = Json.obj(
      "kind"        -> Json.fromString("occurrenceConflict"),
      "occurrences" -> Json.arr(Json.fromInt(0), Json.fromInt(1))
    )
    val records =
      v1.hcursor.downField("value").downField("records").focus.flatMap(_.asArray).get
    val quarantined =
      records.indexWhere(_.hcursor.downField("reason").downField("cause").succeeded)
    assert(quarantined >= 0, "the v1 ledger fixture quarantines a record")
    val bad = v1.hcursor
      .downField("value")
      .downField("records")
      .downN(quarantined)
      .downField("reason")
      .downField("cause")
      .set(cause)
      .top
      .get
    val refused = ledgers.ledger.decode(bad)
    assert(refused.isLeft)
    assertEquals(ledgers.ledgerLadder.lift(bad), refused.map(_ => Json.Null))
    // The unedited document still lifts, and decodes to the same ledger.
    assertEquals(
      ledgers.ledgerLadder.lift(v1).flatMap(ledgers.ledger.decode),
      ledgers.ledger.decode(v1)
    )
  }
