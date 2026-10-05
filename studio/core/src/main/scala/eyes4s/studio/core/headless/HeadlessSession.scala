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
import eyes4s.studio.core.execution.{
  ExecutionEffect,
  ExecutionError,
  ExecutionEvent,
  ExecutionService
}
import eyes4s.studio.core.fixture.{
  FakeControlError,
  FakeStudyBackend,
  InventoryScenario,
  StoryMoment
}
import eyes4s.studio.core.navigation.StudyNavigator

import scala.concurrent.Future
import scala.concurrent.duration.*

/** Why a headless session's test control was refused. */
enum HeadlessError derives CanEqual:
  case Control(error: FakeControlError)
  case Backend(error: BackendError)

  /** No event matched within `waited`; `seen` are the events that arrived. */
  case Timeout(waited: FiniteDuration, seen: Vector[ExecutionEvent])

  def message: String = this match
    case Control(e)          => e.message
    case Backend(e)          => e.message
    case Timeout(waited, as) =>
      s"No matching execution event within $waited (${as.size} arrived)."

/** `FakeStudyBackend` and a live `ExecutionService` over it, driven from
  * code that names no effect library: every operation returns a `Future`
  * (ticket S3.6). studio-app's pure driver and its headless suites run on
  * it; the cats-effect runtime stays here in studio-core.
  *
  * Events are collected from the moment the session opens, so none is
  * missed; [[events]] and [[awaitEvent]] hand them out in order, each once.
  */
final class HeadlessSession private (
    val fake: FakeStudyBackend[IO],
    service: ExecutionService[IO],
    queue: Queue[IO, ExecutionEvent],
    release: IO[Unit]
)(using runtime: IORuntime)
    extends StudioServices[Future]:

  private def run[A](io: IO[A]): Future[A] = io.unsafeToFuture()

  private val toFuture: FunctionK[IO, Future] = new FunctionK[IO, Future]:
    def apply[A](io: IO[A]): Future[A] = run(io)

  def admission(dataset: DatasetRevision): Future[Either[BackendError, AdmissionSummary]] =
    run(fake.admission(dataset))

  /** A submission first declares its revision to the fake, as the saved
    * analysis the document now holds; a conflicting declaration surfaces as
    * the service's own `StampMismatch`.
    */
  def execute(effect: ExecutionEffect): Future[Either[ExecutionError, Unit]] =
    val declare = effect match
      case ExecutionEffect.Submit(stamp) => fake.declare(stamp.revision, stamp.dataset).void
      case _                             => IO.unit
    run(declare >> ExecutionEffect.perform(service)(effect))

  def events: Future[Vector[ExecutionEvent]] = run(queue.tryTakeN(None).map(_.toVector))

  /** Wait for the first event `until` accepts: every event up to and
    * including it, or a timeout carrying what arrived.
    */
  def awaitEvent(
      until: ExecutionEvent => Boolean,
      within: FiniteDuration = HeadlessSession.Patience
  ): Future[Either[HeadlessError, Vector[ExecutionEvent]]] =
    run(
      IO.ref(Vector.empty[ExecutionEvent]).flatMap { seen =>
        // Every event taken is kept, so a timeout still hands them out. Only
        // the wait is cancelable: once an event is taken it is recorded, so a
        // timeout cannot fall between the take and the record.
        val takeOne: IO[(ExecutionEvent, Vector[ExecutionEvent])] =
          IO.uncancelable(poll =>
            poll(queue.take).flatMap(e => seen.updateAndGet(_ :+ e).map(e -> _))
          )
        def loop: IO[Vector[ExecutionEvent]] =
          takeOne.flatMap((e, all) => if until(e) then IO.pure(all) else loop)
        loop
          .map(_.asRight[HeadlessError])
          .timeoutTo(within, seen.get.map(s => Left(HeadlessError.Timeout(within, s))))
      }
    )

  lazy val navigator: StudyNavigator[Future] = StudyNavigator.mapK(fake.navigator)(toFuture)

  // --- Test control and reads ------------------------------------------------

  /** Hold a job where `pairs` pairs have been compared over the run. */
  def holdAtPairs(job: JobId, pairs: Long): Future[Either[HeadlessError, JobProgress]] =
    run(fake.advanceToPairs(job, pairs).map(_.leftMap(HeadlessError.Control(_))))

  /** Finish a job successfully. */
  def complete(job: JobId): Future[Either[HeadlessError, JobOutcome]] =
    run(fake.complete(job).map(_.leftMap(HeadlessError.Control(_))))

  def result(run: RunId): Future[Either[BackendError, ResultSummary]] =
    this.run(fake.result(run))

  def queries(run: RunId, page: PageRequest): Future[Either[BackendError, QueryPage]] =
    this.run(fake.queries(run, page))

  def inspect(run: RunId, address: ResultAddress): Future[Either[BackendError, Inspection]] =
    this.run(fake.inspect(run, address))

  def ledger(
      dataset: DatasetRevision,
      page: PageRequest
  ): Future[Either[BackendError, LedgerPage]] =
    run(fake.ledger(dataset, page))

  /** The resolved design of `revision`: its candidate and eligible counts. */
  def preview(revision: AnalysisRevision): Future[Either[BackendError, PreviewSummary]] =
    run(fake.preview(revision))

  /** `trial`'s admitted fixations under `revision` (protocol 1.6). */
  def trialFixations(
      revision: AnalysisRevision,
      trial: TrialKey
  ): Future[Either[BackendError, TrialFixations]] =
    run(fake.trialFixations(revision, trial))

  /** `trial`'s σ 2° preview under `revision` (protocol 1.6). */
  def trialPreview(
      revision: AnalysisRevision,
      trial: TrialKey
  ): Future[Either[BackendError, TrialPreview]] =
    run(fake.trialPreview(revision, trial))

  /** Records `from` to `from + count - 1` of `revision`'s fixation file
    * (protocol 1.7).
    */
  def sourceRecords(
      revision: AnalysisRevision,
      from: Int,
      count: Int
  ): Future[Either[BackendError, SourceRecordPage]] =
    run(fake.sourceRecords(revision, from, count))

  def pairRows(
      run: RunId,
      scale: Int,
      page: PageRequest
  ): Future[Either[BackendError, PairRowPage]] =
    this.run(fake.pairRows(run, scale, page))

  /** Every entry of `dataset`'s ledger, in inventory order. */
  def wholeLedger(
      dataset: DatasetRevision
  ): Future[Either[LedgerReadError, Vector[LedgerEntry]]] =
    run(LedgerPages.all(fake.ledger(dataset, _)))

  /** Answer for `dataset`'s trial inventory under `scenario` from now on. */
  def serveInventory(dataset: DatasetRevision, scenario: InventoryScenario): Future[Unit] =
    run(fake.serveInventory(dataset, scenario))

  def runs: Future[Vector[RunSummary]] = run(fake.runs)

  /** Stop watching jobs and release the service. */
  def close: Future[Unit] = run(release)

object HeadlessSession:

  /** How long [[HeadlessSession.awaitEvent]] waits by default. */
  val Patience: FiniteDuration = 10.seconds

  /** A session on the fake at `moment`, with the global runtime. */
  def open(moment: StoryMoment): Future[HeadlessSession] =
    given IORuntime = cats.effect.unsafe.implicits.global
    val acquire     =
      for
        fake    <- Resource.eval(FakeStudyBackend.create[IO](moment))
        service <- ExecutionService.resource[IO](fake)
        events  <- service.subscribe
        queue   <- Resource.eval(Queue.unbounded[IO, ExecutionEvent])
        _       <- events.evalMap(queue.offer).compile.drain.background
      yield (fake, service, queue)
    acquire.allocated
      .map { case ((fake, service, queue), release) =>
        new HeadlessSession(fake, service, queue, release)
      }
      .unsafeToFuture()
