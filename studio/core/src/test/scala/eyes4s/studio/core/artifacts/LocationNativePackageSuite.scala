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

package eyes4s.studio.core.artifacts

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.command.{Command, JournalEntry}
import eyes4s.studio.core.session.ProjectSession
import eyes4s.codec.ByteDigest
import eyes4s.plan.{LocationComparisonResult, LocationStoredOutcome}
import eyes4s.studio.core.backend.{
  AnalysisRevision,
  DatasetRevision,
  RunId,
  JobEvent,
  JobOutcome,
  LocationJobProgress,
  BackendError
}
import eyes4s.studio.core.real.{DatasetSources, RealStudyBackend}
import eyes4s.studio.core.assets.AssetRegistry
import eyes4s.studio.core.document.*
import java.nio.charset.StandardCharsets.UTF_8

class LocationNativePackageSuite extends munit.CatsEffectSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private def bytes(value: String): IndexedSeq[Byte] = value.getBytes(UTF_8).toIndexedSeq
  private val trials                                 =
    "trial_id,observer_slot,image,category,phase,sdt,image_index,image_family\ne1,1,a.jpg,c,enc,1,1,a.jpg\nr1,1,a.jpg,c,rec,2,1,a.jpg\nc1,1,b.jpg,c,enc,1,2,b.jpg\n"
  private val locations = "trial_id\tx_px\ty_px\ne1\t0\t0\nr1\t0\t0\nc1\t900\t900\n"
  private val pairs     = "pair_id\tfocal_id\tencoding_id\tcontrol_id\np1\tr1\te1\tc1\n"
  private val phases = "image_index\timage\tcategory\tobserver_slot\trecording_label\tinitials\tsource_trial\tphase\tslot_state\tgaze_state\tsdt\tresponse_state\tsource_locations\tadmitted_locations\timage_family\n" +
    "1\ta.jpg\tc\t1\tlabel\tAB\t1\tenc\tmetadata_present\tlocations\t1\trecorded\t1\t1\ta.jpg\n" +
    "1\ta.jpg\tc\t1\tlabel\tAB\t1\trec\tmetadata_present\tlocations\t2\trecorded\t1\t1\ta.jpg\n" +
    "2\tb.jpg\tc\t1\tlabel\tAB\t2\tenc\tmetadata_present\tlocations\t1\trecorded\t1\t1\tb.jpg\n" +
    "1\ta.jpg\tc\t2\tlabel\tAB\t\trec2\tmetadata_present\tmissing_phase\t\tabsent\t0\t0\ta.jpg\n"
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
    pending.id,
    CoreBinding.Bound(admitted.planCanonical),
    LocationRecipe(pending.profile, CoreBinding.Bound(admitted.inputCanonical)),
    get(RevisionName.of("Spatial witness")),
    "Declared slots, no invented timing"
  )

  test(
    "native closure restores exact metadata, missingness, maps and retained scores without rescore"
  ) {
    val computed = admitted.plan.run
    // A codec contract witness: valid stored values deliberately differ from
    // the computed score, so recomputation during restore would be detected.
    val retained = get(
      LocationComparisonResult.restore(
        admitted.plan,
        admitted.plan.inputHash,
        admitted.plan.planHash,
        computed.maps.map(m => m.trial -> m.result.map(_.bins)),
        Vector(LocationStoredOutcome("p1", Right(0.75), Right(0.25)))
      )
    )
    val packed   = get(LocationNativePackage.create(RunId(1), admitted, analysis, retained))
    val reopened = get(LocationNativePackage.verify(packed.files))
    assertEquals(reopened.facts, packed.facts)
    assertEquals(reopened.retained.result.outcomes.head.difference, Right(0.5))
    assertEquals(computed.outcomes.head.difference, Right(1.0))
    assertEquals(reopened.admitted.imported.trialMetadata, admitted.imported.trialMetadata)
    assertEquals(reopened.admitted.imported.phaseLedger, admitted.imported.phaseLedger)
    assertEquals(reopened.admitted.imported.locationLedger, admitted.imported.locationLedger)
    assertEquals(
      reopened.admitted.imported.unpairedRetrievals,
      admitted.imported.unpairedRetrievals
    )
    assertEquals(
      reopened.admitted.imported.phaseLedger.count(_.gazeState == "missing_phase"),
      1
    )
    assertEquals(reopened.archive.index, packed.archive.index)
    assertEquals(packed.files.size, 7)
  }

  test("missing, duplicate, changed sources and foreign plan bindings are refused") {
    val packed =
      get(LocationNativePackage.create(RunId(1), admitted, analysis, admitted.plan.run))
    assert(LocationNativePackage.verify(packed.files.tail).isLeft)
    assert(LocationNativePackage.verify(packed.files :+ packed.files.head).isLeft)
    val name    = LocationNativePackage.sourceNames.find(_._1 == SourceRole.PhaseLedger).get._2
    val changed = packed.files.map((n, data) =>
      if n == name then n -> bytes(phases.replace("AB", "CD")) else n -> data
    )
    assert(LocationNativePackage.verify(changed).isLeft)
    assert(
      LocationNativePackage
        .create(
          RunId(1),
          admitted,
          analysis.copy(plan = CoreBinding.unbound),
          admitted.plan.run
        )
        .isLeft
    )
    assert(LocationNativePackage.admit(pending, files.dropRight(1)).isLeft)
  }

  test("declared density, source row and byte budgets refuse before publishing a package") {
    assert(
      LocationNativePackage
        .admit(pending, files, get(NativeArtifactBudget.of(maxDensityCells = 299)))
        .isLeft
    )
    assert(
      LocationNativePackage
        .admit(pending, files, get(NativeArtifactBudget.of(maxRows = 1)))
        .isLeft
    )
    assert(
      LocationNativePackage
        .admit(pending, files, get(NativeArtifactBudget.of(maxTotalBytes = 10)))
        .isLeft
    )
    assert(
      LocationNativePackage
        .create(
          RunId(1),
          admitted,
          analysis,
          admitted.plan.run,
          get(NativeArtifactBudget.of(maxRows = 1))
        )
        .isLeft
    )
    val packed =
      get(LocationNativePackage.create(RunId(1), admitted, analysis, admitted.plan.run))
    assert(
      LocationNativePackage
        .verify(packed.files, get(NativeArtifactBudget.of(maxEntries = 6)))
        .isLeft
    )
  }

  test("completion facts cannot be forged with a public copy method") {
    val errors = scala.compiletime.testing.typeCheckErrors("""
      def replace(f: eyes4s.studio.core.artifacts.LocationBindingFacts) =
        f.copy(run = eyes4s.studio.core.backend.RunId(-1))
    """)
    assert(errors.exists(_.message.contains("copy")))
  }

  test("main project stores, binds, saves and cold-restores spatial completion") {
    val retained = get(
      LocationComparisonResult.restore(
        admitted.plan,
        admitted.plan.inputHash,
        admitted.plan.planHash,
        admitted.plan.run.maps.map(m => m.trial -> m.result.map(_.bins)),
        Vector(LocationStoredOutcome("p1", Right(0.75), Right(0.25)))
      )
    )
    val packed  = get(LocationNativePackage.create(RunId(1), admitted, analysis, retained))
    val initial = get(
      StudioDocument.of(
        Vector(admitted.dataset),
        Vector(analysis),
        None,
        Vector(
          RunRef(
            RunId(1),
            analysis.id,
            admitted.dataset.id,
            RunLifecycle.Completed,
            CoreBinding.unbound
          )
        ),
        Vector.empty,
        Vector.empty,
        PresentationState.default,
        Vector.empty
      )
    )
    val owner = get(LockOwner.of("spatial project persistence"))
    def ok[E, A](value: IO[Either[E, A]]): IO[A] = value.map(get)
    for
      store  <- InMemoryProjectStore.create[IO]
      lock   <- ok(store.acquire(owner))
      inputs <- files
        .traverse { (role, data) =>
          val name = sources.entries.find(_.role == role).get.path.value
          ok(
            ProjectBundle
              .importInput(store, lock, InputKind.Source(role), name, IArray.from(data))
          )
        }
        .guarantee(store.release(lock).void)
      session <- ok(
        ProjectSession.create(store, owner, initial, SharingOptions.complete, inputs)
      )
      stored <- ok(
        NativeArtifactSink.project(session).storeCompletion(NativeCompletion.Locations(packed))
      )
      beforeBind <- session.document
      _          <- ok(
        session.perform(
          JournalEntry.Apply(Command.BindLocationCompletedArtifacts(packed.facts))
        )
      )
      _      <- ok(session.save)
      _      <- ok(session.close)
      opened <- ok(ProjectSession.open(store, owner))
      saved  <- opened.session.document
      found  <- ok(
        NativeArtifactSource.project(opened.session).loadCompletion(saved.run(RunId(1)).get)
      )
      timedRefusal <- opened.session.loadNativeArtifacts(RunId(1))
      _            <- ok(opened.session.close)
    yield
      assertEquals(stored, NativeCompletionFacts.Locations(packed.facts))
      assertEquals(beforeBind, initial)
      assertEquals(saved.run(RunId(1)).get.archive, packed.archive.index.archive)
      assert(timedRefusal.isLeft)
      found match
        case Some(NativeCompletion.Locations(value)) =>
          assertEquals(value.retained.result.outcomes.head.difference, Right(0.5))
          assertEquals(value.admitted.imported.phaseLedger, admitted.imported.phaseLedger)
        case other => fail(s"expected a spatial completion, got $other")
  }

  test("spatial submission uses shared jobs and exports a native completion") {
    val initial = get(
      StudioDocument.of(
        Vector(admitted.dataset),
        Vector(analysis),
        None,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        PresentationState.default,
        Vector.empty
      )
    )
    val source = new DatasetSources[IO]:
      def bytes(dataset: DatasetRevisionSpec, source: Source)             = IO.pure(None)
      def assets(dataset: DatasetRevisionSpec): IO[Option[AssetRegistry]] = IO.pure(None)
      override def locationBytes(dataset: LocationDatasetRevision, source: Source)(using
          cats.Applicative[IO]
      ) =
        IO.pure(files.find(_._1 == source.role).map((_, data) => IArray.from(data)))
    RealStudyBackend.resource(initial, source).use { backend =>
      for
        status <- backend.submit(analysis.id).map(get)
        events <- backend.subscribe(status.job).map(get).flatMap(_.compile.toVector)
        packed <- backend.nativeCompletion(status.run, NativeArtifactBudget.Default).map(get)
        result <- backend.locationResult(status.run).map(get)
        jobs   <- backend.jobs
      yield
        assertEquals(jobs.size, 1)
        assertEquals(result.result.outcomes.head.difference, Right(1.0))
        events.last match
          case JobEvent.Finished(JobOutcome.Completed(_, _, progress: LocationJobProgress)) =>
            assertEquals(progress.completedMaps, 3L)
            assertEquals(progress.completedQueries, 1L)
          case other => fail(s"expected spatial completion progress, got $other")
        packed match
          case NativeCompletion.Locations(value) => assertEquals(value.facts.analysis, analysis)
          case other => fail(s"expected spatial native package, got $other")
    }
  }

  test("cold backend restores spatial archive without source reads or jobs") {
    val packed =
      get(LocationNativePackage.create(RunId(1), admitted, analysis, admitted.plan.run))
    val initial = get(
      StudioDocument.of(
        Vector(admitted.dataset),
        Vector(analysis),
        None,
        Vector(
          RunRef(
            RunId(1),
            analysis.id,
            admitted.dataset.id,
            RunLifecycle.Completed,
            CoreBinding.Bound(packed.facts.result)
          )
        ),
        Vector.empty,
        Vector.empty,
        PresentationState.default,
        Vector.empty
      )
    )
    for
      reads <- Ref.of[IO, Int](0)
      source = new DatasetSources[IO]:
        def bytes(dataset: DatasetRevisionSpec, source: Source) = reads.update(_ + 1).as(None)
        def assets(dataset: DatasetRevisionSpec): IO[Option[AssetRegistry]] =
          reads.update(_ + 1).as(None)
        override def locationBytes(dataset: LocationDatasetRevision, source: Source)(using
            cats.Applicative[IO]
        ) = reads.update(_ + 1).as(None)
      archive = new NativeArtifactSource[IO]:
        def load(ref: RunRef, budget: NativeArtifactBudget) =
          IO.pure(Left(NativeArtifactError.Layout("timed load", "spatial test")))
        override def loadCompletion(ref: RunRef, budget: NativeArtifactBudget)(using
            cats.Functor[IO]
        ) =
          IO.pure(Right(Some(NativeCompletion.Locations(packed))))
      _ <- RealStudyBackend.resource(initial, source, Some(archive)).use { backend =>
        for
          first <- backend.locationResult(RunId(1))
          _     <- first match
            case Left(BackendError.ResultRestoring(_)) =>
              backend.awaitRestore(RunId(1)).map(get)
            case Right(_) => IO.unit
            case other    => fail(s"unexpected restore outcome $other")
          restored <- backend.locationResult(RunId(1)).map(get)
          jobs     <- backend.jobs
          count    <- reads.get
        yield
          assertEquals(restored.result.outcomes.head.difference, Right(1.0))
          assertEquals(jobs.size, 0)
          assertEquals(count, 0)
      }
    yield ()
  }

  test("execution service preserves spatial stamps and publishes verified storage facts") {
    import eyes4s.studio.core.execution.*
    import io.circe.syntax.*
    import scala.concurrent.duration.*
    val initial = get(
      StudioDocument.of(
        Vector(admitted.dataset),
        Vector(analysis),
        None,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        PresentationState.default,
        Vector.empty
      )
    )
    val stamp =
      LocationRunStamp(analysis.id, analysis.dataset, analysis.plan, analysis.recipe.input)
    val encoded = (stamp: ExecutionStamp).asJson
    assertEquals(get(encoded.as[ExecutionStamp]), stamp)
    assert(encoded.hcursor.get[String]("kind").contains("locations"))
    assert(
      encoded
        .mapObject(_.add("kind", io.circe.Json.fromString("unknown")))
        .as[ExecutionStamp]
        .isLeft
    )
    val wrong =
      RunStamp(analysis.id, analysis.dataset, CoreBinding.unbound, CoreBinding.unbound)
    assert(!stamp.agreesWithDeclarations(wrong))
    val source = new DatasetSources[IO]:
      def bytes(dataset: DatasetRevisionSpec, source: Source)             = IO.pure(None)
      def assets(dataset: DatasetRevisionSpec): IO[Option[AssetRegistry]] = IO.pure(None)
      override def locationBytes(dataset: LocationDatasetRevision, source: Source)(using
          cats.Applicative[IO]
      ) =
        IO.pure(files.find(_._1 == source.role).map((_, data) => IArray.from(data)))
    val sink = new NativeArtifactSink[IO]:
      def store(value: NativeArtifactPackage) =
        IO.pure(Left(NativeArtifactError.Layout("timed store", "spatial test")))
      override def storeCompletion(value: NativeCompletion)(using cats.Applicative[IO]) =
        IO.pure(Right(value.facts))
    RealStudyBackend.resource(initial, source).use { backend =>
      ExecutionService
        .resource[IO](
          backend,
          completions =
            Some(job => NativeArtifactDelivery.storeCompletion(backend, sink, job.run))
        )
        .use { service =>
          service.subscribe.use { events =>
            for
              refused   <- service.submit(wrong)
              before    <- backend.jobs
              submitted <- service.submit(stamp).map(get)
              observed  <- events
                .takeThrough {
                  case ExecutionEvent.LocationArtifactsStored(_, _) => false
                  case ExecutionEvent.ArtifactsRefused(_, _, _)     => false
                  case _                                            => true
                }
                .compile
                .toVector
                .timeout(5.seconds)
            yield
              assert(refused.isLeft)
              assertEquals(before.size, 0)
              assertEquals(submitted.stamp, stamp)
              assert(observed.exists {
                case ExecutionEvent.Ready(ready) => ready.stamp == stamp
                case _                           => false
              })
              observed.last match
                case ExecutionEvent.LocationArtifactsStored(_, facts) =>
                  assertEquals(facts.analysis, analysis)
                  assertEquals(facts.dataset, admitted.dataset)
                case other => fail(s"expected spatial stored facts, got $other")
          }
        }
    }
  }
