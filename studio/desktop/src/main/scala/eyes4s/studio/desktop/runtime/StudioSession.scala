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

package eyes4s.studio.desktop.runtime

import cats.effect.{IO, Resource}
import cats.effect.unsafe.IORuntime
import cats.syntax.all.*
import eyes4s.studio.app.AppModel
import eyes4s.studio.core.document.StudioDocument
import eyes4s.studio.core.backend.{BackendError, RunId, StudyBackend}
import eyes4s.studio.core.headless.{NativeReads, SynchronizedBackend}
import java.util.concurrent.atomic.AtomicReference
import eyes4s.studio.core.navigation.StudyNavigator
import eyes4s.studio.core.real.{DatasetSources, RealStudyBackend}
import eyes4s.studio.core.artifacts.{
  NativeArtifactError,
  NativeArtifactSink,
  NativeArtifactSource,
  NativeBindingFacts
}
import eyes4s.studio.core.execution.{
  ExecutionError,
  ExecutionEvent,
  ExecutionJob,
  ExecutionService,
  NativeArtifactDelivery
}
import eyes4s.studio.core.fixture.{FakeStudyBackend, StoryMoment}

/** The backend, navigator and execution service one window owns. Host
  * source bytes/assets enter through DatasetSources; backend resources and
  * subscriptions are released when the window's session closes.
  *
  * Events of the execution service are handed to `deliver` on the service's
  * own fibre, in publication order; the caller moves them to the UI thread.
  */
final class StudioSession private (
    val rawBackend: StudyBackend[IO],
    val backend: StudyBackend[IO],
    documentSource: AtomicReference[Option[() => StudioDocument]],
    val reads: NativeReads[IO],
    synchronizeDocument: StudioDocument => IO[Either[BackendError, Unit]],
    val fixture: Option[FakeStudyBackend[IO]],
    val service: ExecutionService[IO],
    release: IO[Unit]
)(using runtime: IORuntime):

  val navigator: StudyNavigator[IO] = reads.navigator

  /** The getter reads a safely published immutable snapshot, never JavaFX state. */
  def bindDocument(source: () => StudioDocument): Unit = documentSource.set(Some(source))

  /** Register the authoritative document before performing its backend effects. */
  def synchronize(document: StudioDocument): IO[Either[BackendError, Unit]] =
    synchronizeDocument(document)

  /** Run `io` on the service's runtime and hand its result to `done`, on
    * the runtime's thread.
    */
  def run[A](io: IO[A])(done: Either[Throwable, A] => Unit): Unit =
    io.unsafeRunAsync(done)

  /** Run `io` and wait for it (boot and tests only; never on the UI thread
    * while a job event is expected).
    */
  def await[A](io: IO[A]): A = io.unsafeRunSync()

  /** Track each job the document's running runs name (a reopened project,
    * t3's run 8), then return the service's jobs.
    */
  def adopt(document: StudioDocument): Vector[Either[ExecutionError, ExecutionJob]] =
    await(
      document.running
        .flatMap(r =>
          document.job(r.id).map(j => (j, AppModel.stampOf(document, r.analysis, r.dataset)))
        )
        .traverse((job, stamp) => service.adopt(job, stamp))
    )

  def jobs: Vector[ExecutionJob] = await(service.jobs)

  /** Release event subscriptions and the owned backend resource. */
  def close(): Unit = await(release)

object StudioSession:

  /** The fixture session remains available for explicit story/demo tests. */
  def start(moment: StoryMoment, deliver: ExecutionEvent => Unit)(using
      runtime: IORuntime
  ): StudioSession =
    acquire(
      Resource
        .eval(FakeStudyBackend.create[IO](moment))
        .map(backend =>
          (
            backend: StudyBackend[IO],
            backend.navigator,
            Some(backend),
            (_: StudioDocument) => IO.pure(Right(())),
            None,
            None
          )
        ),
      deliver
    )

  /** Native services over the document's exact dataset and analysis revisions. */
  def start(
      document: StudioDocument,
      sources: DatasetSources[IO],
      deliver: ExecutionEvent => Unit,
      artifactSink: Option[NativeArtifactSink[IO]] = None,
      artifactSource: Option[NativeArtifactSource[IO]] = None
  )(using runtime: IORuntime): StudioSession =
    acquire(
      RealStudyBackend
        .resource[IO](document, sources, artifactSource)
        .map { backend =>
          val completion = artifactSink.map(sink =>
            (job: ExecutionJob) => NativeArtifactDelivery.store(backend, sink, job.run)
          )
          (
            backend: StudyBackend[IO],
            backend.navigator,
            None,
            backend.synchronize,
            completion,
            Some(backend.awaitRestore)
          )
        },
      deliver
    )

  private def acquire(
      backendResource: Resource[
        IO,
        (
            StudyBackend[IO],
            StudyNavigator[IO],
            Option[FakeStudyBackend[IO]],
            StudioDocument => IO[Either[BackendError, Unit]],
            Option[ExecutionJob => IO[Either[NativeArtifactError, NativeBindingFacts]]],
            Option[RunId => IO[Either[BackendError, Unit]]]
        )
      ],
      deliver: ExecutionEvent => Unit
  )(using runtime: IORuntime): StudioSession =
    val documentSource = new AtomicReference[Option[() => StudioDocument]](None)
    val resources      = for
      (raw, navigator, fixture, synchronize, completion, awaitRestore) <- backendResource
      sync = () =>
        IO.defer(
          documentSource
            .get()
            .fold(IO.pure[Either[BackendError, Unit]](Right(())))(source =>
              synchronize(source())
            )
        )
      backend = new SynchronizedBackend[IO](raw, sync)
      reads   <- NativeReads.resource[IO](backend, navigator, sync, awaitRestore)
      service <- ExecutionService.resource[IO](backend, artifacts = completion)
      events  <- service.subscribe
      _       <- events.evalMap(event => IO(deliver(event))).compile.drain.background
    yield (raw, backend, reads, synchronize, fixture, service)
    val ((raw, backend, reads, synchronize, fixture, service), release) =
      resources.allocated.unsafeRunSync()
    new StudioSession(
      raw,
      backend,
      documentSource,
      reads,
      synchronize,
      fixture,
      service,
      release
    )
