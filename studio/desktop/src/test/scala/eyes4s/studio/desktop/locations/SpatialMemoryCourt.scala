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

package eyes4s.studio.desktop.locations

import cats.effect.{IO, Ref}
import cats.effect.unsafe.implicits.global
import cats.syntax.all.*
import eyes4s.codec.{ByteDigest, LocationComparisonResultCodec, LocationComparisonRun}
import eyes4s.studio.core.artifacts.*
import eyes4s.studio.core.assets.AssetRegistry
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.command.{Command, JournalEntry}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.real.{DatasetSources, RealStudyBackend}
import eyes4s.studio.core.session.ProjectSession
import eyes4s.studio.desktop.platform.FileProjectStore
import java.nio.file.{Files, Path}
import java.lang.management.ManagementFactory

/** Opt-in full-data qualification through the normal native project store and executor. */
object SpatialMemoryCourt:
  private def get[E, A](value: Either[E, A]): A =
    value.fold(e => throw IllegalStateException(e.toString), identity)
  private def ok[E, A](value: IO[Either[E, A]]): IO[A] = value.map(get)
  private val owner = get(LockOwner.of("spatial native memory court"))
  private val names = Vector(
    SourceRole.LocationTrials -> "trials.csv",
    SourceRole.Locations      -> "locations.tsv",
    SourceRole.LocationPairs  -> "pairs.tsv",
    SourceRole.PhaseLedger    -> "phase-ledger.tsv",
    SourceRole.LocationLedger -> "location-ledger.tsv"
  )
  private def measure[A](phase: String)(work: IO[A]): IO[A] = for
    start  <- IO.monotonic
    result <- work
    end    <- IO.monotonic
    _      <- IO {
      System.gc();
      println(
        s"SPATIAL_NATIVE phase=$phase elapsed_ms=${(end - start).toMillis} live_heap=${ManagementFactory.getMemoryMXBean.getHeapMemoryUsage.getUsed} max_heap=${Runtime.getRuntime.maxMemory()}"
      )
    }
  yield result

  private def output(held: RealStudyBackend.LocationRun, directory: Path): IO[Unit] =
    IO.blocking {
      Files.createDirectories(directory): Unit
      val plan   = held.admitted.plan
      val result = held.result
      val scores = result.outcomes.map(o =>
        s"${o.query.id}\t${get(o.matched).value}\t${get(o.control).value}\t${get(o.difference)}"
      )
      Files.writeString(
        directory.resolve("scores.tsv"),
        ("pair_id\tmatched\tcontrol\tdifference" +: scores).mkString("\n") + "\n"
      ): Unit
      val bins = result.maps.flatMap(e =>
        get(e.result).bins.zipWithIndex.collect {
          case (count, index) if count > 0 =>
            s"${e.trial.value}\t${index % plan.grid.nx}\t${index / plan.grid.nx}\t$count"
        }
      )
      Files.writeString(
        directory.resolve("bins.tsv"),
        ("trial_id\tbin_x\tbin_y\tcount" +: bins).mkString("\n") + "\n"
      ): Unit
      Files.writeString(
        directory.resolve("location-run.json"),
        get(
          LocationComparisonResultCodec.codec.encode(LocationComparisonRun(plan, result))
        ).noSpaces
      ): Unit
      println(
        s"SPATIAL_NATIVE maps=${result.maps.size} queries=${result.outcomes.size} occupied_bins=${bins.size}"
      )
    }

  private def create(sourceDir: Path, store: ProjectStore[IO]): IO[ProjectSession[IO]] = for
    files <- IO.blocking(
      names.map((role, name) =>
        role -> Files.readAllBytes(sourceDir.resolve(name)).toIndexedSeq
      )
    )
    sources = get(
      LocationBundleSources.of(
        files.map((role, bytes) =>
          Source(
            role,
            get(SourcePath.of(names.find(_._1 == role).get._2)),
            ByteDigest.sha256(IArray.from(bytes)),
            None
          )
        )
      )
    )
    admitted <- measure("admission")(
      IO(
        get(
          LocationNativePackage.admit(
            LocationDatasetRevision(
              DatasetRevision(1),
              None,
              sources,
              LocationProfile.FigrimV1,
              LocationAdmissionDecision.Pending
            ),
            files
          )
        )
      )
    )
    analysis = LocationAnalysisRevision(
      AnalysisRevision(1),
      admitted.dataset.id,
      CoreBinding.Bound(admitted.planCanonical),
      LocationRecipe(admitted.dataset.profile, CoreBinding.Bound(admitted.inputCanonical)),
      get(RevisionName.of("FIGRIM spatial comparison")),
      "Declared observer slots; no timing"
    )
    doc = get(
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
    lock    <- ok(store.acquire(owner))
    entries <- files
      .traverse((role, bytes) =>
        ok(
          ProjectBundle.importInput(
            store,
            lock,
            InputKind.Source(role),
            names.find(_._1 == role).get._2,
            IArray.from(bytes)
          )
        )
      )
      .guarantee(store.release(lock).void)
    session <- ok(ProjectSession.create(store, owner, doc, SharingOptions.complete, entries))
  yield session

  private def run(session: ProjectSession[IO], out: Path): IO[Unit] = for
    doc <- session.document
    source = new DatasetSources[IO]:
      def bytes(d: DatasetRevisionSpec, s: Source): IO[Option[IArray[Byte]]] = IO.pure(None)
      def assets(d: DatasetRevisionSpec): IO[Option[AssetRegistry]]          = IO.pure(None)
      override def locationBytes(d: LocationDatasetRevision, s: Source)(using
          cats.Applicative[IO]
      ): IO[Option[IArray[Byte]]] = ok(session.readInput(s)).map(Some(_))
    _ <- RealStudyBackend
      .resource(doc, source, Some(NativeArtifactSource.project(session)))
      .use { backend =>
        for
          _ <- ok(
            session
              .perform(JournalEntry.Apply(Command.ReserveLocationRun(doc.analyses.head.id)))
          )
          reserved <- session.document
          _        <- ok(backend.synchronize(reserved))
          status   <- ok(backend.submit(doc.analyses.head.id))
          events   <- ok(backend.subscribe(status.job)).flatMap(_.compile.toVector)
          _        <- IO {
            require(events.last.isInstanceOf[JobEvent.Finished]);
            events.last match
              case JobEvent.Finished(_: JobOutcome.Completed) => ()
              case other => throw IllegalStateException(s"run did not complete: $other")
          }
          packed <- measure("package")(
            ok(backend.nativeCompletion(status.run, NativeArtifactBudget.Default))
          )
          facts <- measure("store-readback")(
            ok(NativeArtifactSink.project(session).storeCompletion(packed))
          )
          _ <- ok(
            session.perform(
              JournalEntry.Apply(
                Command
                  .RecordRunOutcome(status.run, RunLifecycle.Completed, CoreBinding.unbound)
              )
            )
          )
          _ <- facts match
            case NativeCompletionFacts.Locations(value) =>
              ok(
                session.perform(
                  JournalEntry.Apply(Command.BindLocationCompletedArtifacts(value))
                )
              ).void
            case _ => IO.raiseError(IllegalStateException("expected spatial completion"))
          _    <- measure("save")(ok(session.save))
          held <- ok(backend.locationResult(status.run))
          _    <- output(held, out)
        yield ()
      }
  yield ()

  private def restore(session: ProjectSession[IO], out: Path): IO[Unit] = for
    doc   <- session.document
    reads <- Ref.of[IO, Int](0)
    source = new DatasetSources[IO]:
      def bytes(d: DatasetRevisionSpec, s: Source): IO[Option[IArray[Byte]]] =
        reads.update(_ + 1).as(None)
      def assets(d: DatasetRevisionSpec): IO[Option[AssetRegistry]] =
        reads.update(_ + 1).as(None)
      override def locationBytes(d: LocationDatasetRevision, s: Source)(using
          cats.Applicative[IO]
      ): IO[Option[IArray[Byte]]] = reads.update(_ + 1).as(None)
    _ <- RealStudyBackend
      .resource(doc, source, Some(NativeArtifactSource.project(session)))
      .use { backend =>
        for
          first <- backend.locationResult(doc.runs.last.id)
          _     <- first match
            case Left(BackendError.ResultRestoring(_)) =>
              ok(backend.awaitRestore(doc.runs.last.id)).void
            case Right(_)    => IO.unit
            case Left(error) => IO.raiseError(IllegalStateException(error.message))
          held  <- ok(backend.locationResult(doc.runs.last.id))
          jobs  <- backend.jobs
          count <- reads.get
          _     <- IO {
            require(jobs.isEmpty, s"cold restore created jobs: $jobs");
            require(count == 0, s"cold restore read sources: $count");
            println("SPATIAL_NATIVE cold_jobs=0 external_source_reads=0")
          }
          _ <- output(held, out)
        yield ()
      }
  yield ()

  def main(args: Array[String]): Unit =
    require(args.length == 4, "write|read SOURCE_DIRECTORY PROJECT_DIRECTORY OUTPUT_DIRECTORY")
    require(Runtime.getRuntime.maxMemory() <= 1073741824L, "run in a fresh JVM with -Xmx950m")
    val program = for
      store <- FileProjectStore.at[IO](Path.of(args(2)))
      _     <-
        if args(0) == "write" then
          IO(
            require(
              !Files.exists(Path.of(args(2)).resolve("project.json")),
              "refuse overwriting an existing project"
            )
          ) *>
            create(Path.of(args(1)), store).flatMap(s =>
              measure("run-save-export")(run(s, Path.of(args(3)))).guarantee(ok(s.close))
            )
        else if args(0) == "read" then
          ok(ProjectSession.open(store, owner)).flatMap(p =>
            measure("cold-restore-export")(restore(p.session, Path.of(args(3))))
              .guarantee(ok(p.session.close))
          )
        else IO.raiseError(IllegalArgumentException("unknown mode"))
    yield ()
    program.unsafeRunSync()
