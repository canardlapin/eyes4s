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

package eyes4s.studio.core.headless

import cats.effect.{IO, Ref, Resource}
import eyes4s.studio.core.artifacts.*
import eyes4s.studio.core.assets.AssetRegistry
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.{DatasetRevisionSpec, RunRef, Source}
import eyes4s.studio.core.fixture.StoryMoments
import eyes4s.studio.core.real.DatasetSources
import scala.concurrent.duration.*

/** The real native factory must preserve host archive refusals without
  * touching raw source bytes or silently starting recomputation.
  */
class NativeArchiveHostSuite extends munit.CatsEffectSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private def future[A](effect: => scala.concurrent.Future[A]): IO[A] =
    IO.fromFuture(IO(effect))

  test(
    "the native headless factory preserves a cached archive refusal and never reads raw sources"
  ) {
    val document = get(StoryMoments.t2)
    val run      = RunId(7)
    val ref      = document.run(run).get
    val failure  =
      NativeArtifactError.persistence(run, "load archive", "native payload is corrupt")
    for
      sourceReads  <- Ref.of[IO, Int](0)
      archiveReads <- Ref.of[IO, Int](0)
      sources = new DatasetSources[IO]:
        def bytes(dataset: DatasetRevisionSpec, source: Source): IO[Option[IArray[Byte]]] =
          sourceReads.update(_ + 1).as(None)
        def assets(dataset: DatasetRevisionSpec): IO[Option[AssetRegistry]] =
          sourceReads.update(_ + 1).as(None)
      archives = new NativeArtifactSource[IO]:
        def load(
            requested: RunRef,
            budget: NativeArtifactBudget
        ): IO[Either[NativeArtifactError, Option[NativeArtifactPackage]]] =
          IO {
            assertEquals(requested, ref)
            assertEquals(budget, NativeArtifactBudget.Default)
          }.flatMap(_ => archiveReads.update(_ + 1).as(Left(failure)))
      _ <- Resource
        .make(
          future(NativeHeadlessSession.open(document, sources, artifactSource = Some(archives)))
        )(session => future(session.close))
        .use { session =>
          for
            first  <- future(session.result(run)).timeout(5.seconds)
            second <- future(session.queries(run, get(PageRequest.of(0, 1)))).timeout(5.seconds)
            jobs   <- session.backend.jobs
            rawCount     <- sourceReads.get
            archiveCount <- archiveReads.get
          yield
            val expected = BackendError.ArchiveRestoreRefused(run, failure.diagnostic)
            assertEquals(first.left.toOption, Some(expected))
            assertEquals(second.left.toOption, Some(expected))
            assertEquals(jobs, Vector.empty)
            assertEquals(rawCount, 0)
            assertEquals(archiveCount, 1)
        }
    yield ()
  }
