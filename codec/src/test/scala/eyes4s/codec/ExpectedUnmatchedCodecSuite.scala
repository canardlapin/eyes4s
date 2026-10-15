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

import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

class ExpectedUnmatchedCodecSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(e => fail(e.toString), identity)
  private val frame                         = get(Frame.screen("selection-codec", 4, 4))
  private val grid                          = get(Grid.over(frame, 2, 2))
  private val trial = get(TrialIdentity.of("p1", "recognition", "lure", TrialOccurrence.first))
  private val selected = get(
    ExpectedUnmatched.of("old_new", Vector("new"), Vector(trial -> "new"))
  )
  private val codec                                      = StudyCodecs.trialCosine[Px]
  private def plan(selection: Option[ExpectedUnmatched]) = get(
    StudyPlan.configure(
      get(ArtifactRef.parse[StudyInput[TrialKey, Px]]("0123456789abcdef")),
      TrialKey.layout(TrialKeyDefinitions.trialLayout),
      StudyGeometry.WholeFrame(grid),
      "recognition",
      "study",
      Weight.Duration,
      Vector(StudyScale.Native(StudyEstimate.Binned())),
      None,
      FailurePolicy.RequireAll,
      StudyMethod.cosine[Px](DefinitionId.cosine),
      (),
      expectedUnmatched = selection
    )
  )

  test("version 4 preserves the declared selection, assignments and scientific description") {
    val p        = plan(Some(selected))
    val json     = get(codec.codec.encode(p))
    val restored = get(codec.codec.decode(json))
    assertEquals(restored.expectedUnmatched, Some(selected))
    assertEquals(restored.description, p.description)
    assertEquals(get(codec.codec.encode(restored)), json)
    assert(codec.ladder.versions.exists(_.version == 4))
  }

  test("legacy plans keep their bytes and do not acquire a selection") {
    val p   = plan(None)
    val old = get(codec.codec.encode(p))
    assertEquals(get(codec.codec.decode(old)).expectedUnmatched, None)
    assertEquals(get(codec.codec.encode(get(codec.codec.decode(old)))), old)
  }

  test("duplicate trial assignments and blank selectors are refused on decode") {
    val raw       = StudyWire.expectedUnmatched(selected)
    val duplicate = raw.mapObject(
      _.add(
        "assignments",
        io.circe.Json.arr(
          raw.hcursor.downField("assignments").focus.get.asArray.get.head,
          raw.hcursor.downField("assignments").focus.get.asArray.get.head
        )
      )
    )
    assert(StudyWire.readExpectedUnmatched(duplicate).isLeft)
    assert(
      StudyWire
        .readExpectedUnmatched(raw.mapObject(_.add("column", io.circe.Json.fromString(""))))
        .isLeft
    )
  }

  test("earlier schema writers and relabelled payloads cannot erase selection") {
    val p       = plan(Some(selected))
    val payload = get(codec.ladder.writeAt(codec.schemaV4, p))
    Vector(codec.schema, codec.schemaV2, codec.schemaV3).foreach { version =>
      assert(codec.ladder.writeAt(version, p).isLeft)
      assert(codec.ladder.readAt(version, payload).isLeft)
    }
  }
