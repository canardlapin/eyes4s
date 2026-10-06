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

package eyes4s.studio.core.real

import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.StoryMoments

/** Authoritative ids cannot silently change immutable science or lose the
  * explicit run the document requested. Draft edits remain version-local.
  */
class RealRegistrySuite extends munit.FunSuite:
  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)
  private val original                     = ok(StoryMoments.t2)
  private def document(
      datasets: Vector[DatasetRevisionSpec] = original.datasets,
      analyses: Vector[AnalysisRevisionSpec] = original.analyses,
      draft: Option[Draft] = original.draft,
      runs: Vector[RunRef] = original.runs
  ) = ok(
    StudioDocument.of(
      datasets,
      analyses,
      draft,
      runs,
      original.reporting,
      original.figures,
      original.presentation,
      original.jobs
    )
  )

  test("fresh revisions register; same-id draft edits invalidate current work") {
    val registry = RealRegistry.of(original)
    val base     = original.analyses.last
    val edited   = ok(
      Draft.between(StoryMoments.rev5, base, base.recipe.copy(grid = ok(GridSize.of(32, 24))))
    )
    val change = ok(registry.synchronize(document(draft = Some(edited))))
    assert(change.revisions(StoryMoments.rev5))
    assertEquals(change.next.revisions(StoryMoments.rev5)._2.grid, ok(GridSize.of(32, 24)))
    assertEquals(
      registry.revisions(StoryMoments.rev5)._2,
      original.draft.get.recipe(base.recipe)
    )
    val saved = AnalysisRevisionSpec(
      StoryMoments.rev5,
      base.dataset,
      CoreBinding.unbound,
      edited.recipe(base.recipe),
      base.studio
    )
    val extended =
      ok(change.next.synchronize(document(analyses = original.analyses :+ saved, draft = None)))
    assert(extended.next.revisions.contains(StoryMoments.rev5))
  }

  test("admitted dataset content and saved recipes cannot change under their ids") {
    val registry = RealRegistry.of(original)
    val changed  = original.datasets.map(d =>
      if d.id == StoryMoments.r3 then
        d.copy(geometry =
          ok(
            Geometry.of(d.geometry.screen, d.geometry.image, ok(DeclaredPixelsPerDegree.of(36)))
          )
        )
      else d
    )
    assert(
      registry
        .synchronize(document(datasets = changed))
        .left
        .toOption
        .exists(_.diagnostic.subject.contains(DiagnosticLocus.Dataset(StoryMoments.r3)))
    )
    val analyses = original.analyses.map(a =>
      if a.id == StoryMoments.rev4 then
        a.copy(recipe = a.recipe.copy(grid = ok(GridSize.of(32, 24))))
      else a
    )
    assert(
      registry
        .synchronize(document(analyses = analyses))
        .left
        .toOption
        .exists(_.diagnostic.subject.contains(DiagnosticLocus.Revision(StoryMoments.rev4)))
    )
    assertEquals(registry.revisions(StoryMoments.rev4)._2, original.analyses.last.recipe)
  }

  test("only newly synchronized Running records reserve an explicit id; ambiguity is refused") {
    val registry = RealRegistry.of(original)
    assertEquals(registry.reserved(StoryMoments.rev4, StoryMoments.r3, Set.empty), Right(None))
    val run8 = RunRef(
      RunId(8),
      StoryMoments.rev4,
      StoryMoments.r3,
      RunLifecycle.Running,
      CoreBinding.unbound
    )
    val next = ok(registry.synchronize(document(runs = original.runs :+ run8))).next
    assertEquals(
      next.reserved(StoryMoments.rev4, StoryMoments.r3, Set.empty),
      Right(Some(RunId(8)))
    )
    assertEquals(next.reserved(StoryMoments.rev4, StoryMoments.r3, Set(RunId(8))), Right(None))
    val removed   = ok(next.synchronize(original)).next
    val relabeled = run8.copy(analysis = original.analyses.head.id)
    assert(
      removed
        .synchronize(document(runs = original.runs :+ relabeled))
        .left
        .toOption
        .exists(_.diagnostic.subject.contains(DiagnosticLocus.Run(run8.id)))
    )
    val readded = ok(removed.synchronize(document(runs = original.runs :+ run8))).next
    assertEquals(readded.reserved(StoryMoments.rev4, StoryMoments.r3, Set.empty), Right(None))
    val run9      = run8.copy(id = RunId(9))
    val ambiguous =
      ok(next.synchronize(document(runs = original.runs ++ Vector(run8, run9)))).next
    val refusal =
      ambiguous.reserved(StoryMoments.rev4, StoryMoments.r3, Set.empty).left.toOption.get
    assert(refusal.message.contains("run 8") && refusal.message.contains("run 9"))
    val reopened = RealRegistry.of(document(runs = original.runs :+ run8))
    assertEquals(reopened.reserved(StoryMoments.rev4, StoryMoments.r3, Set.empty), Right(None))
  }
