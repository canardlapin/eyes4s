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
import eyes4s.studio.core.assets.AssetRegistry
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.StoryMoments
import java.nio.charset.StandardCharsets.UTF_8
import munit.CatsEffectSuite

class OptionalDisplaysSuite extends CatsEffectSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private def dataset(mapped: Boolean): DatasetRevisionSpec =
    val original = TrialDurationNativeFixture.spec(None)
    val displays = Option.when(mapped)(
      DisplayColumns(get(ColumnName.of("display_kind")), Some(get(ColumnName.of("image_file"))))
    )
    original.copy(inventory =
      original.inventory.map(m => get(InventoryMapping.withDisplays(m, displays)))
    )

  private val sources = new DatasetSources[IO]:
    def bytes(dataset: DatasetRevisionSpec, source: Source): IO[Option[IArray[Byte]]] =
      val text = if source.role == SourceRole.Fixations then TrialDurationNativeFixture.fixes
      else TrialDurationNativeFixture.trials("")
      IO.pure(Some(IArray.from(text.getBytes(UTF_8))))
    def assets(dataset: DatasetRevisionSpec): IO[Option[AssetRegistry]] = IO.pure(None)

  private def document(
      spec: DatasetRevisionSpec,
      focal: Phase = Phase.Retrieval
  ): StudioDocument =
    val sample   = get(StoryMoments.t2)
    val original = sample.timedAnalyses.last
    val recipe   =
      original.recipe.copy(input = None, phases = original.recipe.phases.copy(focal = focal))
    val analysis = original.copy(dataset = spec.id, recipe = recipe, plan = CoreBinding.unbound)
    get(
      StudioDocument.of(
        Vector(spec.copy(parent = None)),
        Vector(analysis),
        None,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        PresentationState.default,
        Vector.empty
      )
    )

  test(
    "unmapped displays admit and retain all trial and fixation counts without an asset registry"
  ) {
    val spec = dataset(false)
    RealStudyBackend.resource[IO](document(spec), sources).use { backend =>
      backend.admission(spec.id).map { result =>
        val admitted = get(result)
        assertEquals(admitted.admitted, 3)
        assertEquals(admitted.fixationRecords, 3)
        assertEquals(admitted.quarantinedTrials, 0)
        assertEquals(admitted.imagesFound, 0)
      }
    }
  }

  test("declared displays still require their registry") {
    val spec = dataset(true)
    assert(spec.inventory.flatMap(_.displays).nonEmpty)
    RealStudyBackend.resource[IO](document(spec), sources).use { backend =>
      backend
        .admission(spec.id)
        .map(result =>
          assertEquals(
            result,
            Left(BackendError.Unavailable(DiagnosticLocus.Field("stimulus registry")))
          )
        )
    }
  }

  test("an unavailable focal phase refuses direct submission before allocating a job") {
    val doc = document(dataset(false), Phase("Retrieval1"))
    RealStudyBackend.resource[IO](doc, sources).use { backend =>
      for
        result <- backend.submit(doc.timedAnalyses.head.id)
        jobs   <- backend.jobs
      yield
        assert(
          result.left.toOption.exists(_.isInstanceOf[BackendError.NoExecutablePairs]),
          result
        )
        assertEquals(jobs, Vector.empty)
    }
  }

  test("oversized maps refuse direct submission before allocating a scientific job") {
    val original = document(dataset(false))
    val analysis = original.timedAnalyses.head
    val updated  =
      analysis.copy(recipe = analysis.recipe.copy(grid = get(GridSize.of(4096, 4096))))
    val doc = get(
      StudioDocument.of(
        original.timedDatasets,
        Vector(updated),
        None,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        PresentationState.default,
        Vector.empty
      )
    )
    RealStudyBackend.resource[IO](doc, sources).use { backend =>
      for
        result <- backend.submit(updated.id)
        jobs   <- backend.jobs
      yield
        assert(
          result.left.toOption.exists(_.isInstanceOf[BackendError.ArchiveDensityCapacity]),
          result
        )
        assertEquals(jobs, Vector.empty)
    }
  }
