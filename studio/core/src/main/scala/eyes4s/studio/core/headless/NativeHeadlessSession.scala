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

import cats.arrow.FunctionK
import cats.effect.std.Queue
import cats.effect.unsafe.IORuntime
import cats.effect.{IO, Resource}
import cats.syntax.all.*
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.StudioDocument
import eyes4s.studio.core.execution.{
  ExecutionEffect,
  ExecutionError,
  ExecutionEvent,
  ExecutionService
}
import eyes4s.studio.core.navigation.StudyNavigator
import eyes4s.studio.core.real.{DatasetSources, RealStudyBackend}
import eyes4s.studio.core.preview.{PreviewBudget, PreviewId}
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.Future
import scala.concurrent.duration.*

/** The real backend and execution service, with a portable Future host port.
  * Closing releases the service subscriptions and every backend job fiber.
  */
final class NativeHeadlessSession private (
    val rawBackend: StudyBackend[IO],
    val backend: StudyBackend[IO],
    documentSource: AtomicReference[() => StudioDocument],
    reads: NativeReads[IO],
    synchronizeDocument: StudioDocument => IO[Either[BackendError, Unit]],
    service: ExecutionService[IO],
    queue: Queue[IO, ExecutionEvent],
    release: IO[Unit]
)(using runtime: IORuntime)
    extends StudioServices[Future]:
  private def run[A](effect: IO[A]): Future[A] = effect.unsafeToFuture()
  private val toFuture: FunctionK[IO, Future]  = new FunctionK[IO, Future]:
    def apply[A](effect: IO[A]): Future[A] = run(effect)

  val navigator: StudyNavigator[Future]   = StudyNavigator.mapK(reads.navigator)(toFuture)
  def admission(dataset: DatasetRevision) = run(backend.admission(dataset))
  def verify(
      dataset: DatasetRevision,
      content: eyes4s.codec.CanonicalDigest[eyes4s.studio.core.document.DatasetRevisionSpec]
  ) =
    run(backend.verify(dataset, content))

  /** The source must return a safely published immutable document snapshot. */
  def bindDocument(source: () => StudioDocument): Unit = documentSource.set(source)
  def synchronize(document: StudioDocument): Future[Either[BackendError, Unit]] =
    run(synchronizeDocument(document).flatTap {
      case Right(_) => IO(documentSource.set(() => document))
      case Left(_)  => IO.unit
    })
  def execute(effect: ExecutionEffect): Future[Either[ExecutionError, Unit]] =
    run(ExecutionEffect.perform(service)(effect))
  def events: Future[Vector[ExecutionEvent]] = run(queue.tryTakeN(None).map(_.toVector))

  def awaitEvent(
      until: ExecutionEvent => Boolean,
      within: FiniteDuration = HeadlessSession.Patience
  ): Future[Either[HeadlessError, Vector[ExecutionEvent]]] =
    run(IO.ref(Vector.empty[ExecutionEvent]).flatMap { seen =>
      val takeOne = IO.uncancelable(poll =>
        poll(queue.take).flatMap(event => seen.updateAndGet(_ :+ event).map(event -> _))
      )
      def loop: IO[Vector[ExecutionEvent]] =
        takeOne.flatMap((event, all) => if until(event) then IO.pure(all) else loop)
      loop
        .map(_.asRight[HeadlessError])
        .timeoutTo(within, seen.get.map(found => Left(HeadlessError.Timeout(within, found))))
    })

  def result(id: RunId)                                   = run(reads.result(id))
  def queries(id: RunId, page: PageRequest)               = run(reads.queries(id, page))
  def inspect(id: RunId, address: ResultAddress)          = run(reads.inspect(id, address))
  def mapGrid(id: RunId, scale: Int, trial: TrialKey)     = run(reads.mapGrid(id, scale, trial))
  def ledger(dataset: DatasetRevision, page: PageRequest) = run(backend.ledger(dataset, page))
  def preview(revision: AnalysisRevision)                 = run(backend.preview(revision))
  def previewRows(revision: AnalysisRevision, page: PageRequest) = run(
    backend.previewRows(revision, page)
  )
  def previewCounting(revision: AnalysisRevision, budget: PreviewBudget) = run(
    backend.previewCounting(revision, budget).compile.toVector
  )
  def continuePreview(id: PreviewId, budget: PreviewBudget) = run(
    backend.continuePreview(id, budget).compile.toVector
  )
  def provenance(id: RunId, address: ResultAddress) = run(reads.provenance(id, address))
  def trialFixations(revision: AnalysisRevision, trial: TrialKey) = run(
    backend.trialFixations(revision, trial)
  )
  def trialPreview(revision: AnalysisRevision, trial: TrialKey) = run(
    backend.trialPreview(revision, trial)
  )
  def sourceRecords(revision: AnalysisRevision, from: Int, count: Int) = run(
    backend.sourceRecords(revision, from, count)
  )
  def pairRows(id: RunId, scale: Int, page: PageRequest) = run(reads.pairRows(id, scale, page))
  def report(id: RunId, reporting: eyes4s.studio.core.document.ReportingSpec, scale: Int) = run(
    reads.report(id, reporting, scale)
  )
  def wholeLedger(dataset: DatasetRevision) = run(LedgerPages.all(backend.ledger(dataset, _)))
  def runs: Future[Vector[RunSummary]]      = run(backend.runs)
  def close: Future[Unit]                   = run(release)

object NativeHeadlessSession:
  def open(
      document: StudioDocument,
      sources: DatasetSources[IO]
  ): Future[NativeHeadlessSession] =
    given IORuntime    = cats.effect.unsafe.implicits.global
    val documentSource = new AtomicReference[() => StudioDocument](() => document)
    val resources      = for
      raw <- RealStudyBackend.resource[IO](document, sources)
      sync    = () => IO.defer(raw.synchronize(documentSource.get()()))
      backend = new SynchronizedBackend[IO](raw, sync)
      reads   <- NativeReads.resource[IO](backend, raw.navigator, sync)
      service <- ExecutionService.resource[IO](backend)
      events  <- service.subscribe
      queue   <- Resource.eval(Queue.unbounded[IO, ExecutionEvent])
      _       <- events.evalMap(queue.offer).compile.drain.background
    yield (raw, backend, reads, service, queue)
    resources.allocated
      .map { case ((raw, backend, reads, service, queue), release) =>
        new NativeHeadlessSession(
          raw,
          backend,
          documentSource,
          reads,
          raw.synchronize,
          service,
          queue,
          release
        )
      }
      .unsafeToFuture()
