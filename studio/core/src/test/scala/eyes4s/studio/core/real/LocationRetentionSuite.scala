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
import eyes4s.codec.ByteDigest
import eyes4s.studio.core.artifacts.*
import eyes4s.studio.core.backend.{AnalysisRevision, DatasetRevision, RunId}
import eyes4s.studio.core.document.*
import java.nio.charset.StandardCharsets.UTF_8

class LocationRetentionSuite extends munit.CatsEffectSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private def bytes(value: String): IndexedSeq[Byte] = value.getBytes(UTF_8).toIndexedSeq

  private val trials =
    "trial_id,observer_slot,image,category,phase,sdt,image_index,image_family\ne1,1,a.jpg,c,enc,1,1,a.jpg\nr1,1,a.jpg,c,rec,2,1,a.jpg\nc1,1,b.jpg,c,enc,1,2,b.jpg\n"
  private val locations = "trial_id\tx_px\ty_px\ne1\t0\t0\nr1\t0\t0\nc1\t900\t900\n"
  private val pairs     = "pair_id\tfocal_id\tencoding_id\tcontrol_id\np1\tr1\te1\tc1\n"
  private val phases    =
    "image_index\timage\tcategory\tobserver_slot\trecording_label\tinitials\tsource_trial\tphase\tslot_state\tgaze_state\tsdt\tresponse_state\tsource_locations\tadmitted_locations\timage_family\n" +
      "1\ta.jpg\tc\t1\tlabel\tAB\t1\tenc\tmetadata_present\tlocations\t1\trecorded\t1\t1\ta.jpg\n" +
      "1\ta.jpg\tc\t1\tlabel\tAB\t1\trec\tmetadata_present\tlocations\t2\trecorded\t1\t1\ta.jpg\n" +
      "2\tb.jpg\tc\t1\tlabel\tAB\t2\tenc\tmetadata_present\tlocations\t1\trecorded\t1\t1\tb.jpg\n"
  private val ledger =
    "image_index\tobserver_slot\tphase\tordinal\traw_x\traw_y\trounded_x\trounded_y\tadmitted\tx_px\ty_px\traw_zero_x\traw_zero_y\treason\n" +
      "1\t1\tenc\t1\t1\t1\t1\t1\t1\t0\t0\t0\t0\tadmitted\n" +
      "1\t1\trec\t1\t1\t1\t1\t1\t1\t0\t0\t0\t0\tadmitted\n" +
      "2\t1\tenc\t1\t901\t901\t901\t901\t1\t900\t900\t900\t900\tadmitted\n"
  private val files = Vector(
    SourceRole.LocationTrials -> trials,
    SourceRole.Locations      -> locations,
    SourceRole.LocationPairs  -> pairs,
    SourceRole.PhaseLedger    -> phases,
    SourceRole.LocationLedger -> ledger
  ).map((role, text) => role -> bytes(text))
  private val sources = get(LocationBundleSources.of(files.map { (role, data) =>
    Source(
      role,
      get(SourcePath.of(s"${role.label.replace(' ', '-')}.txt")),
      ByteDigest.sha256(IArray.from(data)),
      None
    )
  }))
  private val pending = LocationDatasetRevision(
    DatasetRevision(1),
    None,
    sources,
    LocationProfile.FigrimV1,
    LocationAdmissionDecision.Pending
  )
  private val admitted = get(LocationNativePackage.admit(pending, files))
  private val analysis = LocationAnalysisRevision(
    AnalysisRevision(1),
    admitted.dataset.id,
    CoreBinding.Bound(admitted.planCanonical),
    LocationRecipe(admitted.dataset.profile, CoreBinding.Bound(admitted.inputCanonical)),
    get(RevisionName.of("Spatial retention")),
    "Declared slots, no invented timing"
  )

  private def document(runs: Vector[RunRef] = Vector.empty): StudioDocument = get(
    StudioDocument.of(
      Vector(admitted.dataset),
      Vector(analysis),
      None,
      runs,
      Vector.empty,
      Vector.empty,
      PresentationState.default,
      Vector.empty
    )
  )

  private def settled(
      backend: RealStudyBackend[IO],
      run: RunId
  ): IO[RealStudyBackend.LocationRun] =
    backend.locationResult(run).flatMap {
      case Right(value)                                                     => IO.pure(value)
      case Left(eyes4s.studio.core.backend.BackendError.ResultRestoring(_)) =>
        backend
          .awaitRestore(run)
          .flatMap(
            _.fold(error => IO.raiseError(new AssertionError(error.message)), _ => IO.unit)
          ) >>
          backend
            .locationResult(run)
            .flatMap(_.fold(error => IO.raiseError(new AssertionError(error.message)), IO.pure))
      case Left(error) => IO.raiseError(new AssertionError(error.message))
    }

  test("one exact spatial revision reuses its immutable admitted source closure") {
    for
      reads <- Ref.of[IO, Int](0)
      source = new DatasetSources[IO]:
        def bytes(dataset: DatasetRevisionSpec, source: Source) = IO.pure(None)
        def assets(dataset: DatasetRevisionSpec)                = IO.pure(None)
        override def locationBytes(dataset: LocationDatasetRevision, source: Source)(using
            cats.Applicative[IO]
        ) = reads
          .update(_ + 1)
          .as(files.find(_._1 == source.role).map((_, value) => IArray.from(value)))
      _ <- RealStudyBackend.resource(document(), source).use { backend =>
        for
          first  <- backend.locationAdmission(admitted.dataset.id).map(get)
          second <- backend.locationAdmission(admitted.dataset.id).map(get)
          count  <- reads.get
        yield
          assert(first eq second)
          assertEquals(count, files.size)
      }
    yield ()
  }

  test("only the newest archive-backed spatial run stays resident") {
    val firstPackage =
      get(LocationNativePackage.create(RunId(1), admitted, analysis, admitted.plan.run))
    val secondPackage =
      get(LocationNativePackage.create(RunId(2), admitted, analysis, admitted.plan.run))
    val runs = Vector(firstPackage, secondPackage).map(value =>
      RunRef(
        value.facts.run,
        analysis.id,
        admitted.dataset.id,
        RunLifecycle.Completed,
        CoreBinding.Bound(value.facts.result)
      )
    )
    for
      loads <- Ref.of[IO, Map[RunId, Int]](Map.empty)
      source = new DatasetSources[IO]:
        def bytes(dataset: DatasetRevisionSpec, source: Source) = IO.pure(None)
        def assets(dataset: DatasetRevisionSpec)                = IO.pure(None)
      archive = new NativeArtifactSource[IO]:
        def load(ref: RunRef, budget: NativeArtifactBudget) =
          IO.pure(Left(NativeArtifactError.Layout("timed load", "spatial test")))
        override def loadCompletion(ref: RunRef, budget: NativeArtifactBudget)(using
            cats.Functor[IO]
        ) =
          loads
            .update(counts => counts.updated(ref.id, counts.getOrElse(ref.id, 0) + 1))
            .as(
              Right(Some(NativeCompletion.Locations(if ref.id == RunId(1) then firstPackage
              else secondPackage)))
            )
      _ <- RealStudyBackend.resource(document(runs), source, Some(archive)).use { backend =>
        for
          first  <- settled(backend, RunId(1))
          second <- settled(backend, RunId(2))
          _      <- settled(backend, RunId(1))
          count  <- loads.get
        yield
          assert(first.admitted eq second.admitted)
          assertEquals(count, Map(RunId(1) -> 2, RunId(2) -> 1))
      }
    yield ()
  }

  test("archive retention never evicts a computed spatial result") {
    val computed = RealStudyBackend.LocationRun(
      admitted,
      analysis,
      admitted.plan.run,
      RealStudyBackend.RunOrigin.Computed
    )
    val restored = computed.copy(origin =
      RealStudyBackend.RunOrigin.Restored(ByteDigest.sha256(IArray(1.toByte)))
    )
    val initial = RealStudyBackend.Jobs
      .of[IO](document())
      .copy(
        results = Map(RunId(1) -> computed, RunId(2) -> restored)
      )
    val retained = initial.retainRestoredLocation(RunId(3), restored)
    assertEquals(retained.results.keySet, Set(RunId(1), RunId(3)))
    assert(retained.results(RunId(1)).isInstanceOf[RealStudyBackend.LocationRun])
    assert(
      !RealStudyBackend.Jobs.isRestored(
        retained.results(RunId(1)).asInstanceOf[RealStudyBackend.LocationRun]
      )
    )
  }

  test("only confirmed archive bindings release spatial completions") {
    val computed = RealStudyBackend.LocationRun(
      admitted,
      analysis,
      admitted.plan.run,
      RealStudyBackend.RunOrigin.Computed
    )
    val initial =
      RealStudyBackend.Jobs.of[IO](document()).copy(results = Map(RunId(1) -> computed))
    assertEquals(initial.releaseArchivedLocations(Set.empty).results.keySet, Set(RunId(1)))
    assertEquals(initial.releaseArchivedLocations(Set(RunId(1))).results, Map.empty)
  }
