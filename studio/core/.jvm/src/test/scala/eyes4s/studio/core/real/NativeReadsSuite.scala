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

import cats.effect.{Deferred, IO, Ref}
import eyes4s.studio.core.assets.AssetRegistry
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.{DatasetRevisionSpec, Source, SourceRole}
import eyes4s.studio.core.fixture.StoryMoments
import eyes4s.studio.core.headless.NativeReads
import munit.CatsEffectSuite
import scala.concurrent.duration.*

class NativeReadsSuite extends CatsEffectSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private lazy val document                     = get(RealBackendConformanceSuite.trialLayout)
  private def pending(value: Either[BackendError, ResultSummary]): JobId = value match
    case Left(BackendError.ResultPending(StoryMoments.run7, job)) => job
    case other => fail(s"expected a native recomputation job, got $other")

  test("raw reads report an ordinary recomputation job; local reads await its real result") {
    RealStudyBackend
      .resource[IO](document, RealBackendConformanceSuite.golden)
      .use { backend =>
        NativeReads.resource[IO](backend, backend.navigator).use { reads =>
          for
            raw <- backend.result(StoryMoments.run7)
            job = pending(raw)
            summary <- reads.result(StoryMoments.run7).map(get)
            outcome <- backend.outcome(job).map(get)
            held    <- backend.held(StoryMoments.run7).map(get)
          yield
            assertEquals(summary.run, StoryMoments.run7)
            assert(outcome.exists(_.isInstanceOf[JobOutcome.Completed]))
            assertEquals(summary.pairRows, held.prepared.counts.totalPairs)
            assert(held.origin.isInstanceOf[RealStudyBackend.RunOrigin.Recomputed])
        }
      }
      .timeout(30.seconds)
  }

  test("cancelling the exact recomputation remains available while a local reader waits") {
    RealStudyBackend
      .resource[IO](document, RealBackendConformanceSuite.golden)
      .use { backend =>
        NativeReads.resource[IO](backend, backend.navigator).use { reads =>
          for
            raw <- backend.result(StoryMoments.run7)
            job = pending(raw)
            entered   <- Deferred[IO, Unit]
            reader    <- reads.read(StoryMoments.run7)(entered.complete(()).void.as(raw)).start
            _         <- entered.get
            cancelled <- backend.cancel(job).map(get)
            result    <- reader.joinWithNever
            jobs      <- backend.jobs
          yield
            assert(cancelled.state.isInstanceOf[JobState.Finished])
            assertEquals(
              result,
              Left(BackendError.ResultRecomputationCancelled(StoryMoments.run7, job))
            )
            assertEquals(
              jobs.size,
              1,
              "a cancelled local read must not start another recomputation"
            )
        }
      }
      .timeout(20.seconds)
  }

  test("closing local read resources completes pending reads without a fabricated outcome") {
    RealStudyBackend
      .resource[IO](document, RealBackendConformanceSuite.golden)
      .use { backend =>
        for
          raw <- backend.result(StoryMoments.run7)
          job = pending(raw)
          allocated <- NativeReads.resource[IO](backend, backend.navigator).allocated
          (reads, close) = allocated
          entered <- Deferred[IO, Unit]
          reader  <- reads.read(StoryMoments.run7)(entered.complete(()).void.as(raw)).start
          _       <- entered.get
          _       <- close
          result  <- reader.joinWithNever
          after   <- reads.result(StoryMoments.run7)
          _       <- backend.cancel(job)
        yield
          assertEquals(
            result.left.toOption.map(_.code),
            Some("studio-backend.result-read-closed")
          )
          assertEquals(after, Left(BackendError.ResultReadClosed(StoryMoments.run7, None)))
      }
      .timeout(20.seconds)
  }

  test("concurrent stored-run readers identify their shared recomputation as pending") {
    (for
      arrivals <- Ref.of[IO, Int](0)
      released <- Deferred[IO, Unit]
      sources = new DatasetSources[IO]:
        def bytes(dataset: DatasetRevisionSpec, source: Source): IO[Option[IArray[Byte]]] =
          val barrier = if source.role != SourceRole.Fixations then IO.unit
          else
            arrivals
              .updateAndGet(_ + 1)
              .flatMap { count =>
                if count == 2 then released.complete(()).void else IO.unit
              }
              .flatMap(_ => released.get)
          barrier.flatMap(_ => RealBackendConformanceSuite.golden.bytes(dataset, source))
        def assets(dataset: DatasetRevisionSpec): IO[Option[AssetRegistry]] =
          RealBackendConformanceSuite.golden.assets(dataset)
      _ <- RealStudyBackend.resource[IO](document, sources).use { backend =>
        for
          answers <- backend.result(StoryMoments.run7).both(backend.result(StoryMoments.run7))
          jobs    <- backend.jobs
          _       <- jobs.headOption.fold(IO.unit)(status => backend.cancel(status.job).void)
        yield
          assertEquals(jobs.size, 1)
          val pendingJobs = Vector(answers._1, answers._2).flatMap {
            case Left(BackendError.ResultPending(StoryMoments.run7, job)) => Vector(job)
            case Right(summary)                                           =>
              assertEquals(summary.run, StoryMoments.run7)
              Vector.empty
            case other =>
              fail(s"expected the shared pending job or its completed result, got $other")
          }
          assert(pendingJobs.nonEmpty)
          assertEquals(pendingJobs.distinct, Vector(jobs.head.job))
      }
    yield ()).timeout(30.seconds)
  }

  test(
    "a reader still preparing after another recomputation finishes reuses the exact held result"
  ) {
    (for
      arrivals     <- Ref.of[IO, Int](0)
      firstEntered <- Deferred[IO, Unit]
      releaseFirst <- Deferred[IO, Unit]
      sources = new DatasetSources[IO]:
        def bytes(dataset: DatasetRevisionSpec, source: Source): IO[Option[IArray[Byte]]] =
          val barrier = if source.role != SourceRole.Fixations then IO.unit
          else
            arrivals.updateAndGet(_ + 1).flatMap {
              case 1 => firstEntered.complete(()).void.flatMap(_ => releaseFirst.get)
              case _ => IO.unit
            }
          barrier.flatMap(_ => RealBackendConformanceSuite.golden.bytes(dataset, source))
        def assets(dataset: DatasetRevisionSpec): IO[Option[AssetRegistry]] =
          RealBackendConformanceSuite.golden.assets(dataset)
      _ <- RealStudyBackend.resource[IO](document, sources).use { backend =>
        NativeReads.resource[IO](backend, backend.navigator).use { reads =>
          backend.result(StoryMoments.run7).start.flatMap { preparing =>
            val check = for
              _   <- firstEntered.get
              raw <- backend.result(StoryMoments.run7)
              job = pending(raw)
              completed  <- reads.result(StoryMoments.run7).map(get)
              heldBefore <- backend.held(StoryMoments.run7).map(get)
              outcome    <- backend.outcome(job).map(get)
              _ = assert(outcome.exists(_.isInstanceOf[JobOutcome.Completed]))
              _         <- releaseFirst.complete(())
              late      <- preparing.joinWithNever
              heldAfter <- backend.held(StoryMoments.run7).map(get)
              jobs      <- backend.jobs
              visits    <- arrivals.get
            yield
              assertEquals(
                late,
                Right(completed),
                "the late preparer must return the already-computed result"
              )
              assert(
                heldBefore eq heldAfter,
                "retained scientific work/results must not be replaced"
              )
              assertEquals(
                jobs.map(_.job),
                Vector(job),
                "a completed job is not permission to recompute again"
              )
              assertEquals(
                visits,
                2,
                "both readers entered genuine source preparation before the first job ran"
              )
            check.guarantee(releaseFirst.complete(()).void.flatMap(_ => preparing.cancel))
          }
        }
      }
    yield ()).timeout(40.seconds)
  }
