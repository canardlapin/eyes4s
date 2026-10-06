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

import cats.effect.{IO, Ref}
import eyes4s.studio.core.artifacts.{NativeArtifactBudget, NativeArtifactError}
import eyes4s.studio.core.assets.AssetRegistry
import eyes4s.studio.core.backend.RunId
import eyes4s.studio.core.document.{DatasetRevisionSpec, Source}
import eyes4s.studio.core.fixture.StoryMoments

class NativeArtifactProviderSuite extends munit.CatsEffectSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)

  test("unretained and unknown artifact reads never admit sources or start recomputation") {
    val document     = get(StoryMoments.t2)
    val retainedOnly = RunId(7)
    val unknown      = RunId(99)
    assert(document.run(retainedOnly).nonEmpty)
    for
      reads <- Ref.of[IO, Int](0)
      sources = new DatasetSources[IO]:
        def bytes(dataset: DatasetRevisionSpec, source: Source): IO[Option[IArray[Byte]]] =
          reads.update(_ + 1).as(None)
        def assets(dataset: DatasetRevisionSpec): IO[Option[AssetRegistry]] =
          reads.update(_ + 1).as(None)
      answer <- RealStudyBackend.resource[IO](document, sources).use { backend =>
        for
          jobsBefore  <- backend.jobs
          runsBefore  <- backend.runs
          existing    <- backend.nativeArtifacts(retainedOnly, NativeArtifactBudget.Default)
          absent      <- backend.nativeArtifacts(unknown, NativeArtifactBudget.Default)
          jobsAfter   <- backend.jobs
          runsAfter   <- backend.runs
          sourceReads <- reads.get
        yield
          assertEquals(
            existing.left.toOption,
            Some(NativeArtifactError.NotRetained(retainedOnly, Vector.empty))
          )
          assertEquals(
            absent.left.toOption,
            Some(NativeArtifactError.NotRetained(unknown, Vector.empty))
          )
          assertEquals(jobsAfter, jobsBefore)
          assertEquals(runsAfter, runsBefore)
          assertEquals(sourceReads, 0)
      }
    yield answer
  }
