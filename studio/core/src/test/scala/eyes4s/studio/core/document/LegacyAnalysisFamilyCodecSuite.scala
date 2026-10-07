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

package eyes4s.studio.core.document

import eyes4s.studio.core.command.{CommandJournal, CommandPins}
import io.circe.Json
import io.circe.syntax.*

/** Family identity foundation is not a new document capability. */
class LegacyAnalysisFamilyCodecSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val document                          = DocumentSamples.t2
  private val registryShapes                    = Vector(
    Json.Null,
    Json.obj(),
    Json.obj(
      "families" -> Json.arr(Json.obj("id" -> Json.fromInt(1), "name" -> Json.fromString("A"))),
      "owners"   -> Json.arr()
    )
  )

  test("reserved family payloads cannot be silently decoded as one legacy family") {
    registryShapes.foreach { registry =>
      val raw = document.asJson.deepMerge(Json.obj("analysisFamilies" -> registry))
      assert(raw.as[StudioDocument].isLeft, raw.noSpaces)
      val stored = get(StudioDocument.encode(document))
      val forged = stored.hcursor
        .downField("value")
        .withFocus(_.deepMerge(Json.obj("analysisFamilies" -> registry)))
        .top
        .get
      assert(StudioDocument.decode(forged).isLeft, forged.noSpaces)
    }
  }

  test("reserved family ownership is refused through the science codec as well") {
    val codec  = get(ScienceContent.codec)
    val stored = get(codec.encode(document.science))
    registryShapes.foreach { registry =>
      val forged = stored.hcursor
        .downField("value")
        .withFocus(_.deepMerge(Json.obj("analysisFamilies" -> registry)))
        .top
        .get
      assert(codec.decode(forged).isLeft, forged.noSpaces)
    }
  }

  test("pinned legacy documents and journal checkpoints retain their stored interpretation") {
    val pinned = get(io.circe.parser.parse(DocumentPins.pins("document.t1")))
    val loaded = get(StudioDocument.decode(pinned))
    assertEquals(get(StudioDocument.encode(loaded)), pinned)
    assertEquals(
      get(StudioDocument.scienceDigest(document)).display,
      DocumentPins.science("t2")
    )
    val keys   = CommandPins.journalKeys.toVector.sortBy(_.stripPrefix("journal.").toInt)
    val replay = get(CommandJournal.replay(document, keys.map(CommandPins.pins).mkString("\n")))
    assertEquals(replay.checkpoints, Vector(4))
    assertEquals(replay.torn, None)
    val recovered = replay.history.document
    assertEquals(
      recovered.analyses.map(a => LegacyAnalysisFamily.familyOf(recovered, a.id)),
      Vector.fill(recovered.analyses.size)(Some(AnalysisFamilyId.Legacy))
    )
  }
