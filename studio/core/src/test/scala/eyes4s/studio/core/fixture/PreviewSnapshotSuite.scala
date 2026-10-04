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
      .map(state => (new FakeStudyBackend[IO](right(MockStudy.load), state), state))

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
      customReceipt = receipt.copy(counts =
        right(
          PreviewCounts.of(
            7L,
            7L,
            receipt.counts.unmatchedQueries,
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

  for changedDuringCounting <- List(false, true) do
    test(
      s"a changed authoritative dataset refuses submission; changed during counting=$changedDuringCounting"
    ) {
      for
        (fake, state) <- subject
        first         <- events(
          fake.previewCounting(revision, budget(if changedDuringCounting then 1 else 24))
        )
        id = first
          .collectFirst { case PreviewEvent.Initial(value, _, _) => value }
          .getOrElse(fail("no preview id"))
        _ <- state.update(s =>
          s.copy(revisions = s.revisions.updated(revision, DatasetRevision(4)))
        )
        all <-
          if changedDuringCounting then events(fake.continuePreview(id, budget(23)))
          else IO.pure(first)
        refusal <- fake.submitPreview(ready(all))
        after   <- state.get
      yield
        assertEquals(refusal.left.map(_.code), Left("studio-backend.stale-preview"))
        assertEquals(after.jobs, Vector.empty)
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
