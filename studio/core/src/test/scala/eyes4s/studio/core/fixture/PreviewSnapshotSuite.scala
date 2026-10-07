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

package eyes4s.studio.core.fixture

import cats.effect.IO
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.execution.{ExecutionEffect, ExecutionService}
import eyes4s.studio.core.preview.*
import fs2.Stream
import fs2.concurrent.SignallingRef

/** Deliberate fixture-state injection distinguishes retained preparation from
  * rebuilding a script by revision. It does not model a real input digest.
  */
class PreviewSnapshotSuite extends munit.CatsEffectSuite:
  private val revision                            = AnalysisRevision(5)
  private val dataset                             = DatasetRevision(3)
  private def right[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private def budget(n: Int): PreviewBudget       = right(PreviewBudget.of(n))
  private def events(
      stream: Stream[IO, Either[BackendError, PreviewEvent]]
  ): IO[Vector[PreviewEvent]] =
    stream.map(right(_)).compile.toVector
  private def ready(events: Vector[PreviewEvent]): PreviewReady =
    events
      .collectFirst { case PreviewEvent.Ready(value) => value }
      .getOrElse(fail("no ready receipt"))

  private def subject: IO[(FakeStudyBackend[IO], SignallingRef[IO, FakeStudyBackend.State])] =
    SignallingRef
      .of[IO, FakeStudyBackend.State](
        FakeStudyBackend.State(
          Map(dataset  -> DatasetState.Admitted),
          Map(revision -> dataset),
          Vector.empty,
          Vector.empty,
          Map.empty,
          Map.empty
        )
      )
      .map(state =>
        (new FakeStudyBackend[IO](right(MockStudy.load), StoryMoment.T2, state), state)
      )

  test("submission keeps snapshot identity and execution consumes its captured script") {
    for
      (fake, state) <- subject
      counted       <- events(fake.previewCounting(revision, budget(24)))
      receipt = ready(counted)
      before <- state.get
      retained = before.previews(receipt.id)
      // Seven pairs deliberately differ from the revision-5 fixture script.
      captured = retained.snapshot.copy(script =
        Vector(
          ScriptedSegment(Segment.Comparing(0, PairDesign.Matched), ProgressTotal.Exact(7))
        )
      )
      customReceipt = ProtocolSamples.remade(receipt)(counts =
        right(
          PreviewCounts.of(
            7L,
            7L,
            receipt.counts.eligibleQueries.value,
            receipt.counts.unmatchedQueries.value,
            receipt.counts.ambiguousMatches
          )
        )
      )
      _ <- state.update(s =>
        s.copy(previews =
          s.previews
            .updated(receipt.id, retained.copy(snapshot = captured, ready = customReceipt))
        )
      )
      submitted <- fake.submitPreview(customReceipt).map(right(_))
      after     <- state.get
      outcome   <- fake.complete(submitted.job).map(right(_))
    yield
      assert(after.jobSnapshots(submitted.job) eq captured)
      assertEquals(outcome.progress.map(_.totals.completedPairs), Some(7L))
      assertEquals(outcome.progress.map(_.totals.totalPairs), Some(ProgressTotal.Exact(7L)))
  }

  test("ExecutionEffect.SubmitPreview runs the retained snapshot, not a rebuilt one (E2E-05)") {
    for
      (fake, state) <- subject
      counted       <- events(fake.previewCounting(revision, budget(24)))
      receipt = ready(counted)
      before <- state.get
      retained = before.previews(receipt.id)
      captured = retained.snapshot.copy(script =
        Vector(
          ScriptedSegment(Segment.Comparing(0, PairDesign.Matched), ProgressTotal.Exact(7))
        )
      )
      _ <- state.update(s =>
        s.copy(previews = s.previews.updated(receipt.id, retained.copy(snapshot = captured)))
      )
      outcome <- ExecutionService.resource[IO](fake).use { service =>
        ExecutionEffect.perform(service)(ExecutionEffect.SubmitPreview(receipt))
      }
      after <- state.get
    yield
      assertEquals(outcome, Right(()))
      assertEquals(
        ExecutionEffect.submitted(ExecutionEffect.SubmitPreview(receipt)),
        Some(receipt.stamp)
      )
      val jobs = after.jobSnapshots.values.toVector
      assertEquals(jobs.size, 1)
      assert(jobs.head eq captured)
  }

  test("a changed dataset stops an incomplete preview without advancing or emitting Ready") {
    for
      (fake, state) <- subject
      first         <- events(fake.previewCounting(revision, budget(1)))
      id = first.collectFirst { case PreviewEvent.Initial(id, _, _) => id }.get
      before <- state.get
      _      <- state.update(s =>
        s.copy(revisions = s.revisions.updated(revision, DatasetRevision(4)))
      )
      refused <- fake.continuePreview(id, budget(23)).compile.toVector
      after   <- state.get
    yield
      assertEquals(refused.size, 1)
      assertEquals(refused.head.left.map(_.code), Left("studio-backend.stale-preview"))
      assertEquals(after.previews(id).progress, before.previews(id).progress)
  }

  test("a completed but stale preview cannot be offered again or submitted") {
    for
      (fake, state) <- subject
      counted       <- events(fake.previewCounting(revision, budget(24)))
      receipt = ready(counted)
      _ <- state.update(s =>
        s.copy(revisions = s.revisions.updated(revision, DatasetRevision(4)))
      )
      resumed <- fake.continuePreview(receipt.id, budget(1)).compile.toVector
      refused <- fake.submitPreview(receipt)
      after   <- state.get
    yield
      assertEquals(
        resumed.map(_.left.map(_.code)),
        Vector(Left("studio-backend.stale-preview"))
      )
      assertEquals(refused.left.map(_.code), Left("studio-backend.stale-preview"))
      assertEquals(after.jobs, Vector.empty)
  }

  test("changing the dataset between Counting and Ready refuses the Ready boundary") {
    for
      (fake, state) <- subject
      counted       <- fake
        .previewCounting(revision, budget(24))
        .evalTap {
          case Right(PreviewEvent.Counting(_, progress)) if progress.isComplete =>
            state.update(s =>
              s.copy(revisions = s.revisions.updated(revision, DatasetRevision(4)))
            )
          case _ => IO.unit
        }
        .compile
        .toVector
    yield
      assert(!counted.exists { case Right(PreviewEvent.Ready(_)) => true; case _ => false })
      assertEquals(counted.last.left.map(_.code), Left("studio-backend.stale-preview"))
  }

  test("stopping the direct stream at Initial performs no participant count") {
    for
      (fake, state) <- subject
      first         <- events(fake.previewCounting(revision, budget(24)).take(1))
      id = first
        .collectFirst { case PreviewEvent.Initial(value, _, _) => value }
        .getOrElse(fail("no preview id"))
      after <- state.get
    yield assertEquals(after.previews(id).progress.completedParticipants, 0)
  }
