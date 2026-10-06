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

import cats.effect.{IO, Ref, Deferred}
import eyes4s.codec.{ByteDigest, CanonicalDigest}
import eyes4s.studio.core.assets.AssetRegistry
import eyes4s.studio.core.artifacts.*
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.command.{Command, Reducer}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.StoryMoments
import eyes4s.studio.core.headless.NativeReads
import java.nio.charset.StandardCharsets.UTF_8
import munit.CatsEffectSuite
import scala.concurrent.duration.*

class NativeArchiveRestoreSuite extends CatsEffectSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val run                               = RunId(1)
  val fixes                                     =
    "participant,phase,trial,occurrence,ordinal,onset_ms,duration_ms,x,y,sample_count\n" +
      "P01,Encoding,enc_01,1,1,0,100,600,300,1\n" +
      "P01,Encoding,enc_02,1,1,0,100,620,320,1\n" +
      "P01,Retrieval,ret_01,1,1,0,100,604,305,1\n"
  val trials =
    "participant,phase,trial,occurrence,item,response,display_kind,image_file\n" +
      "P01,Encoding,enc_01,1,item-a,,image,item-a.png\n" +
      "P01,Encoding,enc_02,1,item-b,,image,item-b.png\n" +
      "P01,Retrieval,ret_01,1,item-a,Remembered,image,item-a.png\n"
  lazy val sample   = get(StoryMoments.t2)
  lazy val prepared =
    val original = sample.datasets.last
    val byRole   = Map(SourceRole.Fixations -> fixes, SourceRole.Trials -> trials)
    val sources  = get(
      Sources.of(
        original.sources.entries.map(source =>
          source.copy(
            bytes = ByteDigest.sha256(IArray.from(byRole(source.role).getBytes(UTF_8))),
            semantic = None
          )
        )
      )
    )
    val spec   = original.copy(parent = None, sources = sources)
    val assets = get(
      AssetRegistry.of(spec.id, sources.trials.get.bytes, spec.geometry.screen, Vector.empty)
    )
    val admitted = get(RealAdmission.admit(spec, fixes, trials, assets))
    val recipe   = sample.analyses.last.recipe.copy(
      grid = get(GridSize.of(8, 8)),
      scales = get(ScaleSet.of(Vector(get(Sigma.of(1)), get(Sigma.of(2)))))
    )
    get(RealPrepared.of(AnalysisRevision(1), spec.id, recipe, admitted))
  lazy val exported = get(
    NativeArtifacts.build(
      run,
      RealStudyBackend
        .RealRun(prepared, get(prepared.work.run), RealStudyBackend.RunOrigin.Computed),
      NativeArtifactBudget.Default
    )
  )
  private lazy val unbound = get(
    StudioDocument.of(
      Vector(prepared.admitted.spec),
      Vector(
        AnalysisRevisionSpec(
          prepared.revision,
          prepared.dataset,
          CoreBinding.unbound,
          prepared.recipe,
          sample.analyses.last.studio.copy(preset = Preset.Custom)
        )
      ),
      None,
      Vector(
        RunRef(
          run,
          prepared.revision,
          prepared.dataset,
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
  private lazy val document = get(
    Reducer.run(unbound, Command.BindCompletedArtifacts(exported.facts))
  ).document
  private lazy val ref                             = document.run(run).get
  private val page                                 = get(PageRequest.of(0, 20))
  private def reads(backend: RealStudyBackend[IO]) = NativeReads.resource[IO](
    backend,
    backend.navigator,
    () => IO.pure(Right(())),
    Some(backend.awaitRestore)
  )
  private def denied(calls: Ref[IO, Int]): DatasetSources[IO] = new DatasetSources[IO]:
    def bytes(dataset: DatasetRevisionSpec, source: Source) = calls.update(_ + 1).as(None)
    def assets(dataset: DatasetRevisionSpec)                = calls.update(_ + 1).as(None)

  test(
    "concurrent reads coalesce one archive load, preserve lifecycle and serve verified science source-free"
  ) {
    for
      hostCalls <- Ref.of[IO, Int](0)
      loads     <- Ref.of[IO, Int](0)
      entered   <- Deferred[IO, Unit]
      release   <- Deferred[IO, Unit]
      source = new NativeArtifactSource[IO]:
        def load(expected: RunRef, budget: NativeArtifactBudget) =
          loads.update(_ + 1) >> entered.complete(()).void >> release.get.as(
            Right(Some(exported))
          )
      _ <- RealStudyBackend.resource[IO](document, denied(hostCalls), Some(source)).use {
        backend =>
          reads(backend).use { local =>
            for
              before <- backend.runs
              first  <- backend.result(run)
              _      <- entered.get
              second <- backend.queries(run, page)
              _ = assertEquals(first, Left(BackendError.ResultRestoring(run)))
              _ = assertEquals(second, Left(BackendError.ResultRestoring(run)))
              _         <- backend.synchronize(document).map(get).timeout(2.seconds)
              _         <- release.complete(())
              _         <- backend.awaitRestore(run).map(get)
              summary   <- local.result(run).map(get)
              queryPage <- local.queries(run, page).map(get)
              held      <- backend.held(run).map(get)
              direct = get(
                RealResults.of(
                  run,
                  RealStudyBackend.RealRun(
                    prepared,
                    get(prepared.work.run),
                    RealStudyBackend.RunOrigin.Computed
                  )
                )
              )
              jobs  <- backend.jobs
              after <- backend.runs
              count <- loads.get
              raw   <- hostCalls.get
            yield
              assertEquals(summary, get(direct.summary))
              assertEquals(queryPage, get(direct.queries(page)))
              assertEquals(
                held.origin,
                RealStudyBackend.RunOrigin.Restored(exported.manifestAddress)
              )
              assertEquals(jobs, Vector.empty)
              assertEquals(after, before)
              assertEquals(count, 1)
              assertEquals(raw, 0)
          }
      }
    yield ()
  }

  test(
    "present archive refusal is retained and never falls back to source reads or execution"
  ) {
    for
      raw   <- Ref.of[IO, Int](0)
      loads <- Ref.of[IO, Int](0)
      error  = NativeArtifactError.persistence(run, "read archive", "corrupt native bytes")
      source = new NativeArtifactSource[IO]:
        def load(ref: RunRef, budget: NativeArtifactBudget) =
          loads.update(_ + 1).as(Left(error))
      _ <- RealStudyBackend.resource[IO](document, denied(raw), Some(source)).use { backend =>
        reads(backend).use { local =>
          for
            first  <- local.result(run)
            second <- local.queries(run, page)
            jobs   <- backend.jobs
            count  <- loads.get
            host   <- raw.get
          yield
            assertEquals(first, Left(BackendError.ArchiveRestoreRefused(run, error.diagnostic)))
            assertEquals(
              second,
              Left(BackendError.ArchiveRestoreRefused(run, error.diagnostic))
            )
            assertEquals(jobs, Vector.empty)
            assertEquals(count, 1)
            assertEquals(host, 0)
        }
      }
    yield ()
  }

  test(
    "a verified package with conflicting current result binding is refused without execution"
  ) {
    val wrong   = get(CanonicalDigest.parse[ResultArchiveArtifact]("ab" * 32))
    val changed = get(
      StudioDocument.of(
        document.datasets,
        document.analyses,
        None,
        Vector(ref.copy(archive = CoreBinding.Bound(wrong))),
        Vector.empty,
        Vector.empty,
        document.presentation,
        Vector.empty
      )
    )
    for
      raw <- Ref.of[IO, Int](0)
      source = new NativeArtifactSource[IO]:
        def load(ref: RunRef, budget: NativeArtifactBudget) = IO.pure(Right(Some(exported)))
      _ <- RealStudyBackend.resource[IO](changed, denied(raw), Some(source)).use { backend =>
        reads(backend).use { local =>
          for
            refused <- local.result(run)
            again   <- local.result(run)
            jobs    <- backend.jobs
            host    <- raw.get
          yield
            assert(refused.left.toOption.exists(_.isInstanceOf[BackendError.RegistryRefused]))
            assertEquals(again, refused)
            assertEquals(jobs, Vector.empty)
            assertEquals(host, 0)
        }
      }
    yield ()
  }

  test("releasing the backend cancels archive host IO and completes its exact wait signal") {
    for
      raw       <- Ref.of[IO, Int](0)
      entered   <- Deferred[IO, Unit]
      cancelled <- Deferred[IO, Unit]
      source = new NativeArtifactSource[IO]:
        def load(ref: RunRef, budget: NativeArtifactBudget) =
          (entered.complete(()).void >> IO
            .never[Either[NativeArtifactError, Option[NativeArtifactPackage]]])
            .onCancel(cancelled.complete(()).void)
      allocation <- RealStudyBackend.resource[IO](document, denied(raw), Some(source)).allocated
      (backend, close) = allocation
      _      <- backend.result(run)
      _      <- entered.get
      waiter <- backend.awaitRestore(run).start
      _      <- close
      _      <- cancelled.get.timeout(2.seconds)
      result <- waiter.joinWithNever.timeout(2.seconds)
      jobs   <- backend.jobs
    yield
      assertEquals(result, Left(BackendError.ResultReadClosed(run, None)))
      assertEquals(jobs, Vector.empty)
  }

  test("genuinely absent native storage retains ordinary recomputation and probes only once") {
    for
      loads <- Ref.of[IO, Int](0)
      source = new NativeArtifactSource[IO]:
        def load(ref: RunRef, budget: NativeArtifactBudget) =
          loads.update(_ + 1).as(Right(None))
      inputs = new DatasetSources[IO]:
        def bytes(dataset: DatasetRevisionSpec, source: Source) = IO.pure(
          Some(
            IArray.from(
              (if source.role == SourceRole.Fixations then fixes else trials).getBytes(UTF_8)
            )
          )
        )
        def assets(dataset: DatasetRevisionSpec) = IO.pure(
          Some(
            get(
              AssetRegistry.of(
                dataset.id,
                dataset.sources.trials.get.bytes,
                dataset.geometry.screen,
                Vector.empty
              )
            )
          )
        )
      _ <- RealStudyBackend.resource[IO](document, inputs, Some(source)).use { backend =>
        reads(backend).use { local =>
          for
            summary <- local.result(run).map(get)
            again   <- local.result(run).map(get)
            held    <- backend.held(run).map(get)
            jobs    <- backend.jobs
            count   <- loads.get
          yield
            assertEquals(again, summary)
            assert(held.origin.isInstanceOf[RealStudyBackend.RunOrigin.Recomputed])
            assertEquals(jobs.size, 1)
            assertEquals(count, 1)
        }
      }
    yield ()
  }

  test("restored source records fetch exact fixation CSV lazily and verify its byte identity") {
    def check(rawText: String, valid: Boolean): IO[Unit] =
      for
        calls <- Ref.of[IO, Int](0)
        source = new NativeArtifactSource[IO]:
          def load(ref: RunRef, budget: NativeArtifactBudget) = IO.pure(Right(Some(exported)))
        inputs = new DatasetSources[IO]:
          def bytes(dataset: DatasetRevisionSpec, source: Source) =
            calls.update(_ + 1).as(Some(IArray.from(rawText.getBytes(UTF_8))))
          def assets(dataset: DatasetRevisionSpec) = calls.update(_ + 1).as(None)
        _ <- RealStudyBackend.resource[IO](document, inputs, Some(source)).use { backend =>
          reads(backend).use { local =>
            for
              _       <- local.result(run).map(get)
              before  <- calls.get
              records <- backend.sourceRecords(prepared.revision, 1, 3)
              after   <- calls.get
              jobs    <- backend.jobs
            yield
              assertEquals(before, 0)
              assertEquals(after, 1)
              assertEquals(jobs, Vector.empty)
              if valid then
                assertEquals(
                  get(records),
                  get(get(RealTrialViews.of(prepared)).sourceRecords(1, 3))
                )
              else
                assert(
                  records.left.toOption
                    .exists(_.isInstanceOf[BackendError.SourceDigestMismatch])
                )
          }
        }
      yield ()
    check(fixes, true) >> check(fixes + "\n", false)
  }
