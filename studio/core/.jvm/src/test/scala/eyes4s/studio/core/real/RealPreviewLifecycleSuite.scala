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

import cats.effect.IO
import cats.syntax.all.*
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.fixture.{GoldenAssets, GoldenCsv, StoryMoments}
import eyes4s.studio.core.preview.*
import munit.CatsEffectSuite

class RealPreviewLifecycleSuite extends CatsEffectSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private lazy val document                     = get(RealBackendConformanceSuite.trialLayout)
  private lazy val prepared                     =
    val dataset  = get(document.dataset(StoryMoments.r3).toRight("no dataset"))
    val admitted = get(
      RealAdmission.admit(
        dataset,
        GoldenCsv.fixations,
        GoldenCsv.trials,
        get(GoldenAssets.registry(dataset))
      )
    )
    val recipe = get(document.analysis(StoryMoments.rev4).toRight("no analysis")).recipe
    get(RealPrepared.of(StoryMoments.rev4, StoryMoments.r3, recipe, admitted))

  test("native preview rows page every requested query with faithful missing references") {
    val requested =
      prepared.admitted.ledger.filter(_.trial.phase == prepared.recipe.phases.focal)
    RealStudyBackend.resource[IO](document, RealBackendConformanceSuite.golden).use { backend =>
      for
        early   <- backend.previewRows(StoryMoments.rev4, get(PageRequest.of(0, 37)))
        initial <- backend
          .previewCounting(StoryMoments.rev4, get(PreviewBudget.of(1)))
          .take(1)
          .compile
          .toVector
        id = get(initial.head) match
          case PreviewEvent.Initial(id, _, _) => id
          case event                          => fail(s"expected Initial, got $event")
        incomplete <- backend.previewRows(StoryMoments.rev4, get(PageRequest.of(0, 37)))
        done       <- backend
          .continuePreview(id, get(PreviewBudget.of(PreviewBudget.MaximumPages)))
          .compile
          .toVector
        pages <- (0 until requested.size by 37).toVector.traverse(offset =>
          backend.previewRows(StoryMoments.rev4, get(PageRequest.of(offset, 37))).map(get)
        )
        past <- backend
          .previewRows(StoryMoments.rev4, get(PageRequest.of(requested.size, 37)))
          .map(get)
      yield
        assert(
          early.isLeft && incomplete.isLeft,
          "row requests must never complete counting eagerly"
        )
        assert(done.exists { case Right(PreviewEvent.Ready(_)) => true; case _ => false })
        val rows = pages.flatMap(_.rows)
        assertEquals(rows.map(_.query), requested.map(_.trial))
        assert(pages.forall(_.page.total == requested.size))
        assertEquals(past.rows, Vector.empty)
        val eligible    = rows.filter(_.eligibility == Eligibility.Eligible)
        val unmatched   = rows.filter(_.eligibility.isInstanceOf[Eligibility.NoMatch])
        val notAdmitted = rows.filter(_.eligibility.isInstanceOf[Eligibility.QueryNotAdmitted])
        assertEquals(eligible.size.toLong, prepared.counts.eligibleQueries)
        assertEquals(unmatched.size, prepared.counts.cardinality.unmatched.size)
        assertEquals(
          notAdmitted.size,
          requested.count(_.disposition != TrialDisposition.Admitted)
        )
        assert(eligible.forall(_.matched.isDefined))
        assert(
          (unmatched ++ notAdmitted).forall(row => row.matched.isEmpty && row.controls.isEmpty)
        )
        assertEquals(
          eligible.flatMap(_.controls).map(_.toLong).sum,
          prepared.counts.controls.eligiblePairs
        )
        assertEquals(rows.size, eligible.size + unmatched.size + notAdmitted.size)
        unmatched.foreach { row =>
          val key = prepared.preview.focalKeys
            .find(k => RealResults.key(k) == row.query)
            .getOrElse(fail(s"missing ${row.query}"))
          val reason = get(
            prepared.work.unmatchedReasons(prepared.admitted.evidence.inventory.get)
          ).reason(key).getOrElse(fail(s"no reason for $key"))
          val expected = RealResults.diagnostic(
            eyes4s.plan.Diagnostic.of(
              eyes4s.plan.StudyFinding
                .UnmatchedFocal[eyes4s.plan.TrialKey, eyes4s.kernel.Unit2D.Px](key, reason)
            )
          )
          assertEquals(row.eligibility, Eligibility.NoMatch(expected))
        }
    }
  }

  test("bounded ready receipt owns exact canonical artifacts and is required for submission") {
    val budget = get(PreviewBudget.of(PreviewBudget.MaximumPages))
    RealStudyBackend.resource[IO](document, RealBackendConformanceSuite.golden).use { backend =>
      for
        responses <- backend.previewCounting(StoryMoments.rev4, budget).compile.toVector
        events = responses.map(get)
        ready  = events
          .collectFirst { case PreviewEvent.Ready(value) => value }
          .getOrElse(fail("no ready receipt"))
        changed = get(
          PreviewCounts.of(
            ready.counts.eligiblePairsPerScale + 1,
            ready.counts.eligiblePairs + 1,
            ready.counts.eligibleQueries.value,
            ready.counts.unmatchedQueries.value,
            ready.counts.ambiguousMatches
          )
        )
        forged = get(
          PreviewReady.of(
            ready.id,
            ready.stamp,
            ready.candidates,
            changed,
            ready.diagnostics,
            ready.recipe
          )
        )
        refused   <- backend.submitPreview(forged)
        before    <- backend.jobs
        continued <- backend
          .continuePreview(ready.id, get(PreviewBudget.of(1)))
          .compile
          .toVector
        accepted <- backend.submitPreview(ready)
        unknown  <- backend.continuePreview(PreviewId(99999), budget).compile.toVector
      yield
        assertEquals(ready.candidates.requestedQueries.value, prepared.summary.requestedQueries)
        assertEquals(ready.candidates.byDesignQueries, None)
        assertEquals(ready.counts.eligibleQueries.value, prepared.summary.eligibleQueries)
        assertEquals(ready.counts.eligiblePairs, prepared.counts.totalPairs)
        assertEquals(ready.recipe, Some(prepared.recipe))
        val configured = get(
          RealPrepared
            .configure(prepared.revision, prepared.dataset, prepared.recipe, prepared.admitted)
        )
        assertEquals(ready.stamp, get(RealPreview.stamp(configured)))
        assertEquals(refused, Left(BackendError.TamperedPreview(forged, ready)))
        assertEquals(before, Vector.empty)
        assertEquals(continued, Vector(Right(PreviewEvent.Ready(ready))))
        val job = get(accepted)
        assertEquals((job.revision, job.dataset), (ready.stamp.revision, ready.stamp.dataset))
        assertEquals(
          unknown.head.left.toOption.map(_.code),
          Some("studio-backend.unknown-preview")
        )
    }
  }
