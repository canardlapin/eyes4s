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

import eyes4s.studio.core.backend.AnalysisRevision
import io.circe.Json
import io.circe.syntax.*
import scala.compiletime.testing.typeCheckErrors

class FamilyPersistenceSuite extends munit.FunSuite:
  import FamilySamples.*
  private val value = document(draft = Some(draftA))

  test("saved and draft ownership is nominal; unknown ids never form a family") {
    assertEquals(value.familyOf(a1.id), Some(a))
    assertEquals(value.familyOf(b4.id), Some(b))
    assertEquals(value.familyOf(draftA.id), Some(a))
    assertEquals(LegacyAnalysisFamily.familyOf(value, b4.id), Some(b))
    assert(value.sameFamily(a1.id, a3.id))
    assert(value.sameFamily(a1.id, draftA.id))
    assert(!value.sameFamily(a1.id, b2.id))
    assert(!value.sameFamily(AnalysisRevision(99), AnalysisRevision(99)))
  }

  test("document checks the registry against its authoritative saved revisions") {
    val partial = get(
      AnalysisFamilyRegistry.of(
        Vector(get(AnalysisFamily.of(a, "A"))),
        Vector(get(AnalysisFamilyOwner.of(a1.id, a))),
        Vector(a1.id)
      )
    )
    val refused = StudioDocument.of(
      value.datasets,
      value.analyses,
      value.draft,
      value.runs,
      value.reporting,
      value.figures,
      value.presentation,
      value.jobs,
      Some(partial)
    )
    assertEquals(
      refused,
      Left(
        DocumentError.FamilyOwnership(
          AnalysisFamilyError.ScopeMismatch(Vector(a1.id), analyses.map(_.id))
        )
      )
    )
  }

  test(
    "initial legacy draft cannot coexist with an explicit registry, including an empty one"
  ) {
    val initial = get(
      Draft.initial(
        AnalysisRevision(1),
        a1.dataset,
        a1.recipe,
        a1.studio.copy(preset = Preset.Custom)
      )
    )
    val empty   = get(AnalysisFamilyRegistry.of(Vector.empty, Vector.empty, Vector.empty))
    val refused = StudioDocument.of(
      value.datasets,
      Vector.empty,
      Some(initial),
      Vector.empty,
      Vector.empty,
      Vector.empty,
      PresentationState.default,
      Vector.empty,
      Some(empty)
    )
    assertEquals(
      refused,
      Left(DocumentError.InitialDraftWithFamilies(initial.id, Vector.empty))
    )
  }

  test(
    "explicit ownership roundtrips through document and science and changes scientific identity"
  ) {
    val encoded = get(StudioDocument.encode(value))
    assertEquals(encoded.hcursor.downField("schema").get[Int]("version"), Right(10))
    assertEquals(get(StudioDocument.decode(encoded)), value)
    val scienceCodec = get(ScienceContent.codec)
    assertEquals(
      get(scienceCodec.encode(value.science)).hcursor.downField("schema").get[Int]("version"),
      Right(6)
    )
    assertEquals(
      get(scienceCodec.decode(get(scienceCodec.encode(value.science)))),
      value.science
    )
    val legacy = get(
      StudioDocument.of(
        value.datasets,
        value.analyses,
        value.draft,
        value.runs,
        value.reporting,
        value.figures,
        value.presentation,
        value.jobs
      )
    )
    assertNotEquals(
      get(StudioDocument.scienceDigest(value)),
      get(StudioDocument.scienceDigest(legacy))
    )
    assertEquals(
      value.asJson.hcursor.downField("analysisFamilies").focus,
      Some(AnalysisFamilyRegistry.encode(registry))
    )
  }

  test(
    "every earlier document/science rung refuses explicit ownership in readers and writers"
  ) {
    val ladder = get(StudioDocument.ladder)
    val raw    = value.asJson
    ladder.versions.dropRight(1).foreach { version =>
      assert(ladder.readAt(version, raw).isLeft, version.toString)
      assert(ladder.writeAt(version, value).isLeft, version.toString)
    }
    val science = get(ScienceContent.ladder)
    science.versions.dropRight(1).foreach { version =>
      assert(science.readAt(version, value.science.asJson).isLeft, version.toString)
      assert(science.writeAt(version, value.science).isLeft, version.toString)
    }
  }

  test(
    "malformed ownership cannot be ignored: null, missing members, duplicates and wrong scope"
  ) {
    val raw       = value.asJson
    val good      = AnalysisFamilyRegistry.encode(registry)
    val duplicate = good.mapObject(o =>
      o.add(
        "owners",
        Json.fromValues(
          registry.owners.map(_.asJson) :+ registry.owners.head.asJson
        )
      )
    )
    Vector(
      Json.Null,
      Json.obj(),
      duplicate,
      AnalysisFamilyRegistry.encode(
        get(AnalysisFamilyRegistry.of(Vector.empty, Vector.empty, Vector.empty))
      )
    ).foreach { malformed =>
      assert(raw.mapObject(_.add("analysisFamilies", malformed)).as[StudioDocument].isLeft)
    }
  }

  test("document and science cannot be built or copied around ownership validation") {
    assert(
      typeCheckErrors(
        "new eyes4s.studio.core.document.ScienceContent(Vector.empty, Vector.empty, None, Vector.empty, Vector.empty, Vector.empty, None)"
      ).nonEmpty
    )
    assert(
      typeCheckErrors(
        "new eyes4s.studio.core.document.StudioDocument(eyes4s.studio.core.document.DocumentSamples.t2.science, eyes4s.studio.core.document.PresentationState.default, Vector.empty, eyes4s.studio.core.document.AssetRelinks.empty)"
      ).nonEmpty
    )
    assert(
      typeCheckErrors(
        "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.ScienceContent]]"
      ).nonEmpty
    )
    assert(
      typeCheckErrors(
        "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.StudioDocument]]"
      ).nonEmpty
    )
    assert(
      typeCheckErrors(
        "eyes4s.studio.core.document.DocumentSamples.t2.science.copy(analysisFamilies = None)"
      ).nonEmpty
    )
    assert(
      typeCheckErrors(
        "eyes4s.studio.core.document.DocumentSamples.t2.copy(science = eyes4s.studio.core.document.DocumentSamples.t2.science)"
      ).nonEmpty
    )
  }
