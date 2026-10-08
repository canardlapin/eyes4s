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
import eyes4s.studio.core.fixture.StoryMoments
import io.circe.Json
import io.circe.syntax.*
import scala.compiletime.testing.typeCheckErrors

class AnalysisFamilySuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val a                                 = get(AnalysisFamilyId.of(1))
  private val b                                 = get(AnalysisFamilyId.of(2))
  private val r3                                = AnalysisRevision(3)
  private val r4                                = AnalysisRevision(4)
  private val fa                                = get(AnalysisFamily.of(a, "Same name"))
  private val fb                                = get(AnalysisFamily.of(b, "Same name"))
  private def owner(revision: AnalysisRevision, family: AnalysisFamilyId) = get(
    AnalysisFamilyOwner.of(revision, family)
  )

  test("identities and names parse at their boundary, without name-based identity") {
    assert(AnalysisFamilyId.of(0).isLeft)
    assert(AnalysisFamilyId.of(-1).isLeft)
    assert(AnalysisFamily.of(a, "  ").isLeft)
    assert(AnalysisFamily.of(a, null).isLeft)
    assertEquals(fa.name, fb.name)
    assert(a != b)
    assertEquals(a.asJson.as[AnalysisFamilyId], Right(a))
    assert(Json.fromInt(0).as[AnalysisFamilyId].isLeft)
    assert(Json.fromString("1").as[AnalysisFamilyId].isLeft)
    assert(AnalysisFamilyOwner.of(AnalysisRevision(0), a).isLeft)
    assert(typeCheckErrors("new eyes4s.studio.core.document.AnalysisFamilyId(0)").nonEmpty)
    assert(typeCheckErrors("val id: eyes4s.studio.core.document.AnalysisFamilyId = 1").nonEmpty)
  }

  test("ownership covers exactly its scoped revisions and survives canonical round trip") {
    val registry = get(
      AnalysisFamilyRegistry.of(
        Vector(fb, fa),
        Vector(owner(r4, b), owner(r3, a)),
        Vector(r4, r3)
      )
    )
    assertEquals(registry.familyOf(r3), Some(a))
    assertEquals(registry.familyOf(r4), Some(b))
    assertEquals(registry.familyOf(AnalysisRevision(5)), None)
    assertEquals(registry.families.map(_.id), Vector(a, b))
    assertEquals(registry.owners.map(_.revision), Vector(r3, r4))
    assertEquals(
      AnalysisFamilyRegistry.decode(AnalysisFamilyRegistry.encode(registry), Vector(r3, r4)),
      Right(registry)
    )
    assert(registry.checkScope(Vector(r3)).isLeft)
    assertEquals(get(registry.nextId).value, 3)
  }

  test("each ownership invariant is rejected independently with its operands") {
    val one = Vector(owner(r3, a))
    assertEquals(
      AnalysisFamilyRegistry.of(Vector(fa, fa), one, Vector(r3)),
      Left(AnalysisFamilyError.DuplicateFamilies(Vector(a)))
    )
    assertEquals(
      AnalysisFamilyRegistry.of(Vector(fa), one ++ one, Vector(r3)),
      Left(AnalysisFamilyError.DuplicateRevisions(Vector(r3)))
    )
    assertEquals(
      AnalysisFamilyRegistry.of(Vector(fa), Vector(owner(r3, b), owner(r4, a)), Vector(r3, r4)),
      Left(AnalysisFamilyError.UnknownFamily(r3, b, Vector(a)))
    )
    assertEquals(
      AnalysisFamilyRegistry.of(Vector(fa), one, Vector(r3, r4)),
      Left(AnalysisFamilyError.MissingRevisions(Vector(r4), Vector(r3, r4)))
    )
    assertEquals(
      AnalysisFamilyRegistry.of(Vector(fa), one :+ owner(r4, a), Vector(r3)),
      Left(AnalysisFamilyError.UnexpectedRevisions(Vector(r4), Vector(r3)))
    )
    assertEquals(
      AnalysisFamilyRegistry.of(Vector(fa), one, Vector(r3, r3)),
      Left(AnalysisFamilyError.DuplicateExpected(Vector(r3)))
    )
    assertEquals(
      AnalysisFamilyRegistry.of(Vector(fa, fb), one, Vector(r3)),
      Left(AnalysisFamilyError.UnownedFamilies(Vector(b)))
    )
    val good      = Vector(owner(r3, a), owner(r4, b))
    val registry  = get(AnalysisFamilyRegistry.of(Vector(fa, fb), good, Vector(r3, r4)))
    val duplicate = AnalysisFamilyRegistry
      .encode(registry)
      .deepMerge(Json.obj("owners" -> (good :+ owner(r3, b)).asJson))
    assertEquals(
      AnalysisFamilyRegistry.decode(duplicate, Vector(r3, r4)),
      Left(AnalysisFamilyError.DuplicateRevisions(Vector(r3)))
    )
  }

  test("canonical bytes and equality retain exact ownership independent of input order") {
    val first = get(
      AnalysisFamilyRegistry.of(
        Vector(fa, fb),
        Vector(owner(r3, a), owner(r4, b)),
        Vector(r3, r4)
      )
    )
    val permuted = get(
      AnalysisFamilyRegistry.of(
        Vector(fb, fa),
        Vector(owner(r4, b), owner(r3, a)),
        Vector(r4, r3)
      )
    )
    val expected =
      """{"families":[{"id":1,"name":"Same name"},{"id":2,"name":"Same name"}],"owners":[{"revision":3,"family":1},{"revision":4,"family":2}]}"""
    assertEquals(AnalysisFamilyRegistry.encode(first).noSpaces, expected)
    assertEquals(AnalysisFamilyRegistry.encode(permuted).noSpaces, expected)
    assertEquals(first, permuted)
    assertEquals(first.hashCode, permuted.hashCode)
  }

  test("checked types expose neither constructor, copy, nor Product mirror bypasses") {
    assert(
      typeCheckErrors(
        "new eyes4s.studio.core.document.AnalysisFamilyRegistry(Vector.empty,Vector.empty,Vector.empty)"
      ).nonEmpty
    )
    assert(
      typeCheckErrors(
        "eyes4s.studio.core.document.AnalysisFamilyId.of(1).toOption.get.copy(value = 0)"
      ).nonEmpty
    )
    assert(
      typeCheckErrors(
        "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.AnalysisFamilyId]]"
      ).nonEmpty
    )
    assert(
      typeCheckErrors(
        "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.AnalysisFamilyRegistry]]"
      ).nonEmpty
    )
  }

  test("empty and exhausted registries have explicit allocation outcomes") {
    val empty = get(AnalysisFamilyRegistry.of(Vector.empty, Vector.empty, Vector.empty))
    assertEquals(empty.nextId, Right(a))
    val last     = get(AnalysisFamilyId.of(Int.MaxValue))
    val registry = get(
      AnalysisFamilyRegistry.of(
        Vector(get(AnalysisFamily.of(last, "Last"))),
        Vector(owner(r3, last)),
        Vector(r3)
      )
    )
    assertEquals(registry.nextId, Left(AnalysisFamilyError.Exhausted(last)))
  }

  test("legacy membership is independent of revision names and uses existing pinned science") {
    val document = get(StoryMoments.t2)
    val ids      = document.analyses.map(_.id) ++ document.draft.toVector.map(_.id)
    assertEquals(
      ids.map(LegacyAnalysisFamily.familyOf(document, _)),
      Vector.fill(ids.size)(Some(a))
    )
    assertEquals(LegacyAnalysisFamily.familyOf(document, AnalysisRevision(999)), None)
    assertEquals(
      get(StudioDocument.scienceDigest(document)).display,
      DocumentPins.science("t2")
    )
    val legacy   = get(io.circe.parser.parse(DocumentPins.pins("document.t1")))
    val reopened = get(StudioDocument.decode(legacy))
    assertEquals(get(StudioDocument.encode(reopened)), legacy)
    assertEquals(
      reopened.analyses.map(value => LegacyAnalysisFamily.familyOf(reopened, value.id)),
      Vector.fill(reopened.analyses.size)(Some(a))
    )
    val renamed = document.analyses.map(value =>
      value.copy(studio = value.studio.copy(name = get(RevisionName.of("Renamed"))))
    )
    val changed = get(
      StudioDocument.of(
        document.datasets,
        renamed,
        document.draft,
        document.runs,
        document.reporting,
        document.figures,
        document.presentation,
        document.jobs
      )
    )
    assertEquals(
      ids.map(LegacyAnalysisFamily.familyOf(changed, _)),
      Vector.fill(ids.size)(Some(a))
    )
  }

  test(
    "empty and initial-draft-only legacy projects have no invented saved family or revision"
  ) {
    val empty = get(
      StudioDocument.of(
        Vector.empty,
        Vector.empty,
        None,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        PresentationState.default,
        Vector.empty
      )
    )
    assertEquals(LegacyAnalysisFamily.familyOf(empty, AnalysisRevision(1)), None)
    val sample = get(StoryMoments.t2)
    val data   = sample.datasets.last
      .copy(id = eyes4s.studio.core.backend.DatasetRevision(1), parent = None)
    val source  = sample.analyses.last
    val initial = get(Draft.initial(AnalysisRevision(1), data.id, source.recipe, source.studio))
    val document = get(
      StudioDocument.of(
        Vector(data),
        Vector.empty,
        Some(initial),
        Vector.empty,
        Vector.empty,
        Vector.empty,
        PresentationState.default,
        Vector.empty
      )
    )
    assertEquals(LegacyAnalysisFamily.familyOf(document, initial.id), Some(a))
    assertEquals(document.analyses, Vector.empty)
    assertEquals(LegacyAnalysisFamily.familyOf(document, AnalysisRevision(2)), None)
  }
