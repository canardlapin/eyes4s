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
import eyes4s.studio.core.document.{DatasetRevisionSpec, Source}
import eyes4s.studio.core.fixture.StoryMoments
import eyes4s.studio.core.headless.NativeReads
import munit.CatsEffectSuite
import scala.concurrent.duration.*

class NativeReadsSuite extends CatsEffectSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private lazy val document                     = get(RealBackendConformanceSuite.trialLayout)
  private lazy val before = RealBackendConformanceSuite.beforeRun7(document)

  test("raw and local stored-run reads refuse missing archives without computation") {
    RealStudyBackend.resource[IO](document, RealBackendConformanceSuite.golden).use { backend =>
      NativeReads.resource[IO](backend, backend.navigator).use { reads =>
        for
          raw   <- backend.result(StoryMoments.run7)
          local <- reads.result(StoryMoments.run7)
          jobs  <- backend.jobs
        yield
          assertEquals(raw, Left(BackendError.ResultArchiveUnavailable(StoryMoments.run7)))
          assertEquals(local, raw)
          assertEquals(jobs, Vector.empty)
      }
    }
  }

  test("local reads await an explicitly submitted job and retain its genuine result") {
    RealStudyBackend
      .resource[IO](before, RealBackendConformanceSuite.golden)
      .use { backend =>
        NativeReads.resource[IO](backend, backend.navigator).use { reads =>
          for
            submitted <- backend.submit(StoryMoments.rev4).map(get)
            raw       <- backend.result(submitted.run)
            result    <- reads.result(submitted.run).map(get)
            held      <- backend.held(submitted.run).map(get)
            jobs      <- backend.jobs
          yield
            assert(
              raw == Left(
                BackendError.ResultPending(submitted.run, submitted.job)
              ) || raw.isRight,
              raw
            )
            assertEquals(result.run, StoryMoments.run7)
            assertEquals(result.pairRows, held.prepared.counts.totalPairs)
            assertEquals(held.origin, RealStudyBackend.RunOrigin.Computed)
            assertEquals(jobs.map(_.job), Vector(submitted.job))
        }
      }
      .timeout(30.seconds)
  }

  test("cancelling an explicitly submitted job settles its waiting local reader") {
    RealStudyBackend
      .resource[IO](before, RealBackendConformanceSuite.golden)
      .use { backend =>
        NativeReads.resource[IO](backend, backend.navigator).use { reads =>
          for
            submitted <- backend.submit(StoryMoments.rev4).map(get)
            entered   <- Deferred[IO, Unit]
            pending = Left(BackendError.ResultPending(submitted.run, submitted.job)): Either[
              BackendError,
              ResultSummary
            ]
            reader    <- reads.read(submitted.run)(entered.complete(()).void.as(pending)).start
            _         <- entered.get
            cancelled <- backend.cancel(submitted.job).map(get)
            result    <- reader.joinWithNever
            jobs      <- backend.jobs
          yield
            assert(cancelled.state.isInstanceOf[JobState.Finished])
            assertEquals(
              result,
              Left(BackendError.ResultRecomputationCancelled(submitted.run, submitted.job))
            )
            assertEquals(jobs.size, 1)
        }
      }
      .timeout(20.seconds)
  }

  test("closing local resources completes pending reads without a fabricated outcome") {
    RealStudyBackend
      .resource[IO](before, RealBackendConformanceSuite.golden)
      .use { backend =>
        for
          submitted <- backend.submit(StoryMoments.rev4).map(get)
          allocated <- NativeReads.resource[IO](backend, backend.navigator).allocated
          (reads, close) = allocated
          entered <- Deferred[IO, Unit]
          pending = Left(BackendError.ResultPending(submitted.run, submitted.job)): Either[
            BackendError,
            ResultSummary
          ]
          reader <- reads.read(submitted.run)(entered.complete(()).void.as(pending)).start
          _      <- entered.get
          _      <- close
          result <- reader.joinWithNever
          after  <- reads.result(submitted.run)
          _      <- backend.cancel(submitted.job)
        yield
          assertEquals(
            result.left.toOption.map(_.code),
            Some("studio-backend.result-read-closed")
          )
          assertEquals(after, Left(BackendError.ResultReadClosed(submitted.run, None)))
      }
      .timeout(20.seconds)
  }

  test("concurrent unavailable-archive reads never touch raw sources or register jobs") {
    for
      visits <- Ref.of[IO, Int](0)
      sources = new DatasetSources[IO]:
        def bytes(dataset: DatasetRevisionSpec, source: Source): IO[Option[IArray[Byte]]] =
          visits.update(_ + 1).as(None)
        def assets(dataset: DatasetRevisionSpec): IO[Option[AssetRegistry]] =
          visits.update(_ + 1).as(None)
      _ <- RealStudyBackend.resource[IO](document, sources).use { backend =>
        for
          values <- backend.result(StoryMoments.run7).both(backend.result(StoryMoments.run7))
          jobs   <- backend.jobs
          calls  <- visits.get
        yield
          assertEquals(
            values._1,
            Left(BackendError.ResultArchiveUnavailable(StoryMoments.run7))
          )
          assertEquals(values._2, values._1)
          assertEquals(jobs, Vector.empty)
          assertEquals(calls, 0)
      }
    yield ()
  }
