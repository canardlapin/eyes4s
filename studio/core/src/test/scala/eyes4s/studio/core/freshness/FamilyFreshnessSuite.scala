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

package eyes4s.studio.core.freshness

import cats.data.NonEmptyVector
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.*

class FamilyFreshnessSuite extends munit.FunSuite:
  import FamilySamples.*
  private val first = run(1, a1, RunLifecycle.Completed)
  private def derive(runs: Vector[RunRef], draft: Option[Draft] = None, figures: Vector[FigureSpec] = Vector.empty) =
    Freshness.of(document(runs, draft, Some(first.id), figures), SessionFacts.empty)

  test("another family's later completion never supersedes a shown run or its figure") {
    val other = run(2, b4, RunLifecycle.Completed)
    val figure = get(FigureSpec.of(
      get(FigureId.of(1)), first.id, DocumentSamples.t2.reporting.head.id,
      DocumentSamples.t2.figures.head.panels
    ))
    val result = derive(Vector(first, other), figures = Vector(figure))
    assertEquals(result.standing(first.id), Some(RunStanding.Current))
    assertEquals(result.banner, None)
    assertEquals(result.figures.head.standing, RunStanding.Current)
  }

  test("same-family completion supersedes even when another family completes afterward") {
    val own = run(2, a3, RunLifecycle.Completed)
    val other = run(3, b4, RunLifecycle.Completed)
    val result = derive(Vector(first, own, other))
    assertEquals(result.standing(first.id), Some(RunStanding.Stale(
      NonEmptyVector.of(StaleReason.Superseded(own.id, own.analysis))
    )))
    assertEquals(result.banner, Some(Banner.NewerCompleted(first, own, Vector.empty)))
  }

  test("same-family running activity survives a later unrelated run in every lifecycle") {
    val own = run(2, a3, RunLifecycle.Running)
    Vector(RunLifecycle.Completed, RunLifecycle.Running, RunLifecycle.Failed,
      RunLifecycle.Cancelled(None)).foreach { state =>
      val other = run(3, b4, state)
      val result = derive(Vector(first, own, other))
      val running = RunActivity.Running(own.id, own.analysis, None)
      assertEquals(result.badge, Badge.Shown(first, RunStanding.Current, Some(running)))
      assertEquals(result.banner, Some(Banner.NewerRunning(first, running, Vector.empty)))
      assertEquals(result.activity.map(_.run), Some(
        if state == RunLifecycle.Running then other.id else own.id
      ))
    }
  }

  test("failed and cancelled result banners use only the shown family") {
    Vector(RunLifecycle.Failed, RunLifecycle.Cancelled(Some(StageKind.Comparing))).foreach { state =>
      val own = run(2, a3, state)
      val other = run(3, b4, RunLifecycle.Running)
      val result = derive(Vector(first, own, other))
      assertEquals(result.standing(first.id), Some(RunStanding.Current))
      result.banner match
        case Some(Banner.NewerEnded(shown, ended)) =>
          assertEquals(shown, first)
          assertEquals(ended.run, own.id)
        case other => fail(s"Expected family-relative ended banner, found $other")
      assertEquals(result.activity.map(_.run), Some(other.id))
    }
  }

  test("an unrelated draft never produces a recipe or rebase comparison with the shown run") {
    val unrelated = get(Draft.against(draftA.id, b4, None, Vector(change)))
    assertEquals(derive(Vector(first), Some(unrelated)).banner, None)
    val otherData = DocumentSamples.t2.datasets.head.id
    val rebased = get(Draft.against(draftA.id, b4, Some(otherData), Vector.empty))
    assertEquals(derive(Vector(first), Some(rebased)).banner, None)
    val ownRebase = get(Draft.against(draftA.id, a1, Some(otherData), Vector.empty))
    assertEquals(derive(Vector(first), Some(ownRebase)).banner,
      Some(Banner.DraftNotRun(first, ownRebase, Vector.empty, Some(otherData))))
    assertEquals(derive(Vector(first), Some(draftA)).banner,
      Some(Banner.DraftNotRun(first, draftA, Vector(change), None)))
  }

  test("dataset movement remains project-wide, independent of family ownership") {
    val old = first.copy(dataset = DocumentSamples.t2.datasets.head.id)
    val original = document(Vector(old), shown = Some(old.id))
    val coherent = get(StudioDocument.of(
      original.datasets, original.analyses.map(spec =>
        if spec.id == a1.id then spec.copy(dataset = old.dataset) else spec
      ), original.draft, original.runs, original.reporting, original.figures,
      original.presentation, original.jobs, original.analysisFamilies
    ))
    val result = Freshness.of(coherent, SessionFacts.empty)
    assertEquals(result.standing(old.id), Some(RunStanding.Stale(
      NonEmptyVector.of(StaleReason.DatasetMoved(old.dataset, a1.dataset))
    )))
  }

  test("jobs remain visible when the shown other family's later run has completed") {
    val working = run(1, a1, RunLifecycle.Running)
    val completed = run(2, b4, RunLifecycle.Completed)
    val result = Freshness.of(document(Vector(working, completed), shown = Some(completed.id)), SessionFacts.empty)
    assertEquals(result.activity.map(_.run), Some(working.id))
    assertEquals(result.badge, Badge.Shown(completed, RunStanding.Current, None))
    assertEquals(result.banner, None)
  }

  test("NoRun contextual revision never names another family's newer saved revision") {
    val working = run(1, a1, RunLifecycle.Running)
    val result = Freshness.of(document(Vector(working), shown = Some(working.id)), SessionFacts.empty)
    assertEquals(result.banner, Some(Banner.NoRun(Some(a3.id))))
  }
