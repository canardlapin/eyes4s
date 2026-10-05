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

package eyes4s.studio.core.execution

import cats.effect.{Deferred, IO, Resource}
import cats.syntax.all.*
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.fixture.{FakeControlError, FakeStudyBackend, StoryMoment}
import fs2.Stream
import munit.CatsEffectSuite

import scala.concurrent.duration.*

/** The execution service on `FakeStudyBackend` with held jobs (run 8 at
  * Comparing 21,400 / 44,845 pairs): cancellation timing (E2E-06), stale
  * completion (E2E-07), failure diagnostics (E2E-13), Show (E2E-08) and event
  * ordering.
  */
class ExecutionServiceSuite extends CatsEffectSuite:
  import ExecutionTrackerSuite.{r3, rev4, rev5, stamps}

  private val stamp4        = stamps(0)
  private val stamp5        = stamps(1)
  private val stamp5Changed = stamps(2)
  private val run7          = RunId(7)
  private val run8          = RunId(8)
  private val Held          = StoryMoment.RunningPairs
  private val Total         = 44845L

  /** E2E-06: from cancel() to the Cancelled event on the fixture. */
  private val CancelBudget = 500.millis

  private def ok[E, A](fa: IO[Either[E, A]]): IO[A] =
    fa.flatMap(e => IO.fromEither(e.leftMap(err => new AssertionError(err.toString))))

  private type Timed = Vector[(FiniteDuration, ExecutionEvent)]

  private def setup(
      moment: StoryMoment
  ): Resource[IO, (FakeStudyBackend[IO], ExecutionService[IO], Stream[IO, ExecutionEvent])] =
    for
      fake    <- Resource.eval(FakeStudyBackend.create[IO](moment))
      service <- ExecutionService.resource[IO](fake)
      events  <- service.subscribe
    yield (fake, service, events)

  /** The end of a job's story: its Ready notice, or any other terminal phase. */
  private def ends(e: ExecutionEvent): Boolean = e match
    case ExecutionEvent.Ready(_)   => true
    case ExecutionEvent.Changed(j) =>
      j.phase.isTerminal && !j.phase.isInstanceOf[JobPhase.Succeeded]

  /** Every event, with the monotonic time it was received, up to the end. */
  private def collect(events: Stream[IO, ExecutionEvent]): IO[Timed] =
    events
      .evalMap(e => IO.monotonic.map(_ -> e))
      .takeThrough((_, e) => !ends(e))
      .compile
      .toVector
      .timeout(5.seconds)

  /** Wait until the service has observed `p` of the job. */
  private def await(service: ExecutionService[IO], id: JobId)(
      p: PartialFunction[JobPhase, Boolean]
  ): IO[ExecutionJob] =
    ok(service.job(id))
      .flatMap(j =>
        if p.applyOrElse(j.phase, _ => false) then IO.pure(Some(j))
        else IO.sleep(1.milli).as(None)
      )
      .untilDefinedM
      .timeout(5.seconds)

  private def phases(out: Timed): Vector[JobPhase] =
    out.collect { case (_, ExecutionEvent.Changed(j)) => j.phase }

  /** The shelf of a reducer that asked for `stamp5` (the worst case for the
    * stale tests: it would accept a wrong notice for the submitted stamp).
    */
  private def shelf(out: Timed): RunShelf =
    out.map(_._2).foldLeft(RunShelf.of(Some(run7)).require(stamp5))(_.receive(_))

  private def untouched(s: RunShelf): Unit =
    assertEquals((s.shown, s.pending), (Some(run7), None))

  private def pairsAt(p: JobPhase): Option[Long] = p.lastReport.map(_.pairs.done)

  test("t3 run 8 reads 'Comparing · 21,400 / 44,845 pairs' once adopted") {
    setup(StoryMoment.T3).use { (fake, service, _) =>
      for
        held    <- fake.jobs.map(_.head)
        adopted <- ok(service.adopt(held.job, stamp5))
        again   <- service.adopt(held.job, stamp5)
        busy    <- service.submit(stamp4)
      yield
        assertEquals(adopted.run, run8)
        adopted.phase match
          case JobPhase.Running(p) =>
            assertEquals(p.stage, StageKind.Comparing)
            assertEquals(p.pairs, Meter(CountUnit.Pairs, Held, MeterTotal.Exact(Total)))
            assertEquals(p.stageMeter.unit, CountUnit.Pairs)
          case other => fail(s"expected Running, got $other")
        assertEquals(again, Left(ExecutionError.AlreadyTracked(held.job)))
        assertEquals(busy.left.map(_.code), Left("studio-backend.already-running"))
    }
  }

  test("E2E-06: cancel stops after the in-flight chunk, within 500 ms; run 7 stays current") {
    setup(StoryMoment.T2).use { (fake, service, events) =>
      for
        collector <- collect(events).start
        job       <- ok(service.submit(stamp5))
        _         <- ok(fake.advanceToPairs(job.id, Held))
        _         <- await(service, job.id) { case JobPhase.Running(p) => p.pairs.done == Held }
        t0        <- IO.monotonic
        answered  <- ok(service.cancel(job.id))
        out       <- collector.joinWithNever
        late      <- fake.advanceToPairs(job.id, Held + 1)
        runs      <- fake.runs
      yield
        val (settledAt, last) = out.last
        assert(
          settledAt - t0 < CancelBudget,
          s"Cancelled after ${(settledAt - t0).toMillis} ms"
        )
        assert(
          answered.phase.isInstanceOf[JobPhase.Cancelling] ||
            answered.phase.isInstanceOf[JobPhase.Cancelled],
          answered.phase
        )
        last match
          case ExecutionEvent.Changed(j) =>
            assert(j.phase.isInstanceOf[JobPhase.Cancelled], j.phase)
            assertEquals(pairsAt(j.phase), Some(Held))
          case other => fail(s"expected Cancelled, got $other")
        val ps     = phases(out)
        val cancel = ps.indexWhere(_.isInstanceOf[JobPhase.Cancelling])
        assert(cancel > 0, ps)
        assert(ps.drop(cancel).forall(p => pairsAt(p).forall(_ == Held)), ps)
        assert(!ps.drop(cancel).exists(_.isInstanceOf[JobPhase.Running]), ps)
        assert(late.left.exists(_.isInstanceOf[FakeControlError.NotRunning]), late)
        assertEquals(runs.find(_.run == run7).map(_.state), Some(RunState.Current))
        assertEquals(
          runs.find(_.run == run8).map(_.state),
          Some(RunState.Cancelled(Some(StageKind.Comparing)))
        )
        untouched(shelf(out))
    }
  }

  test("the t3 inline Cancel settles run 8 within 500 ms") {
    setup(StoryMoment.T3).use { (fake, service, events) =>
      for
        held      <- fake.jobs.map(_.head)
        collector <- collect(events).start
        _         <- ok(service.adopt(held.job, stamp5))
        t0        <- IO.monotonic
        _         <- ok(service.cancel(held.job))
        out       <- collector.joinWithNever
      yield
        assert(out.last._1 - t0 < CancelBudget, s"${(out.last._1 - t0).toMillis} ms")
        assertEquals(phases(out).last, JobPhase.Cancelled(phases(out).last.lastReport))
        assertEquals(pairsAt(phases(out).last), Some(Held))
    }
  }

  test("E2E-07: a completion whose input changed meanwhile is Superseded, never Ready") {
    setup(StoryMoment.T2).use { (fake, service, events) =>
      for
        collector <- collect(events).start
        job       <- ok(service.submit(stamp5))
        _         <- ok(fake.advanceToPairs(job.id, Held))
        _         <- await(service, job.id) { case JobPhase.Running(_) => true }
        _         <- service.require(stamp5Changed)
        _         <- ok(fake.complete(job.id))
        out       <- collector.joinWithNever
      yield
        phases(out).last match
          case JobPhase.Superseded(current, last) =>
            assertEquals(current, Some(stamp5Changed))
            assertEquals(last.map(_.pairs.done), Some(fake.totalPairs(rev5)))
          case other => fail(s"expected Superseded, got $other")
        assert(!out.exists(_._2.isInstanceOf[ExecutionEvent.Ready]), out)
        untouched(shelf(out))
    }
  }

  test(
    "E2E-07: a completion of an older revision is Superseded once a newer one is requested"
  ) {
    setup(StoryMoment.T2).use { (fake, service, events) =>
      for
        collector <- collect(events).start
        job       <- ok(service.submit(stamp4))
        _         <- service.require(stamp5)
        _         <- ok(fake.complete(job.id))
        out       <- collector.joinWithNever
        req       <- service.requested
      yield
        assertEquals(job.stamp.revision, rev4)
        assert(phases(out).last.isInstanceOf[JobPhase.Superseded], phases(out))
        assertEquals(req, Some(stamp5))
        assertEquals(shelf(out).shown, Some(run7))
    }
  }

  test("completion is a notice; only Show promotes the run (E2E-08)") {
    setup(StoryMoment.T2).use { (fake, service, events) =>
      for
        collector <- collect(events).start
        job       <- ok(service.submit(stamp5))
        _         <- ok(fake.complete(job.id))
        out       <- collector.joinWithNever
      yield
        val tail = out.takeRight(2).map(_._2)
        assert(tail.head.isInstanceOf[ExecutionEvent.Changed], tail)
        assert(phases(out).last.isInstanceOf[JobPhase.Succeeded], phases(out))
        assertEquals(tail.last, ExecutionEvent.Ready(RunReady(job.id, run8, stamp5)))
        val waiting = shelf(out)
        assertEquals(waiting.shown, Some(run7))
        assertEquals(waiting.pending.map(_.run), Some(run8))
        assertEquals(RunReady(job.id, run8, stamp5).label, "Run 8 ready")
        assertEquals(
          waiting.show(RunId(9)),
          Left(ExecutionError.NotReady(RunId(9), Some(run8)))
        )
        assertEquals(waiting.show(run8).map(_.shown), Right(Some(run8)))
        assertEquals(waiting.require(stamp5Changed).pending, None)
    }
  }

  test("E2E-13: a failed run exposes its diagnostics with stable codes") {
    val offWindow = StudioDiagnostic(
      "study-failure.off-window",
      DiagnosticLevel.Error,
      DiagnosticOrigin.EyesCore,
      Vector(DiagnosticLocus.Trial(TrialKey("P17", Phase.Retrieval, "ret_07", 1))),
      "no fixation of the trial lies in its map"
    )
    setup(StoryMoment.T2).use { (fake, service, events) =>
      for
        collector <- collect(events).start
        job       <- ok(service.submit(stamp5))
        _         <- ok(fake.advanceToPairs(job.id, Held))
        _         <- ok(fake.fail(job.id, Vector(offWindow, offWindow)))
        out       <- collector.joinWithNever
        runs      <- fake.runs
      yield
        phases(out).last match
          case JobPhase.Failed(ds, _) =>
            assertEquals(
              ds.map(_.code),
              Vector("study-failure.off-window", "study-failure.off-window")
            )
          case other => fail(s"expected Failed, got $other")
        assertEquals(runs.find(_.run == run8).map(_.state), Some(RunState.Failed))
        untouched(shelf(out))
    }
  }

  test("a failure without diagnostics still carries a coded one") {
    setup(StoryMoment.T2).use { (fake, service, events) =>
      for
        collector <- collect(events).start
        job       <- ok(service.submit(stamp5))
        _         <- ok(fake.fail(job.id, Vector.empty))
        out       <- collector.joinWithNever
      yield phases(out).last match
        case JobPhase.Failed(ds, _) =>
          assertEquals(ds.map(_.code), Vector("studio-execution.unexplained-failure"))
          assertEquals(ds.head.subject, Vector(DiagnosticLocus.Job(job.id)))
        case other => fail(s"expected Failed, got $other")
    }
  }

  test("a job whose stream ends before it settles fails as lost") {
    for
      fake <- FakeStudyBackend.create[IO](StoryMoment.T2)
      out  <- ExecutionService.resource[IO](Silent(fake)).use { service =>
        for
          job <- ok(service.submit(stamp5))
          end <- await(service, job.id) { case p => p.isTerminal }
        yield end.phase
      }
    yield out match
      case JobPhase.Failed(ds, None) =>
        assertEquals(ds.map(_.code), Vector("studio-execution.lost-job"))
      case other => fail(s"expected Failed, got $other")
  }

  test("events are ordered: Queued first, steps and done units never move back") {
    setup(StoryMoment.T2).use { (fake, service, events) =>
      def to(id: JobId, pairs: Long) =
        ok(fake.advanceToPairs(id, pairs)) >>
          await(service, id) { case JobPhase.Running(p) => p.pairs.done == pairs }
      for
        collector <- collect(events).start
        job       <- ok(service.submit(stamp5))
        _         <- ok(fake.advanceTo(job.id, Segment.Estimating(0), 100L))
        _         <- await(service, job.id) { case JobPhase.Running(_) => true }
        _         <- to(job.id, 5000L)
        back      <- fake.advanceToPairs(job.id, 4000L)
        _         <- to(job.id, Held)
        _         <- to(job.id, 30000L)
        _         <- ok(fake.complete(job.id))
        out       <- collector.joinWithNever
      yield
        assert(back.left.exists(_.isInstanceOf[FakeControlError.Regressed]), back)
        val ps      = phases(out)
        val reports = ps.flatMap(_.lastReport)
        assertEquals(ps.head, JobPhase.Queued)
        assert(reports.size >= 5, reports)
        assert(reports.zip(reports.tail).forall((a, b) => a.step < b.step), reports.map(_.step))
        assert(
          reports.zip(reports.tail).forall((a, b) => a.completedUnits <= b.completedUnits),
          reports.map(_.completedUnits)
        )
        assertEquals(
          reports.map(_.pairs.done).filter(_ > 0).take(3),
          Vector(5000L, Held, 30000L)
        )
        assert(ps.last.isInstanceOf[JobPhase.Succeeded], ps.last)
        assert(out.last._2.isInstanceOf[ExecutionEvent.Ready], out.last)
    }
  }

  test("E2E-07 race: require(B) while submit(A) awaits the backend supersedes A") {
    for
      fake    <- FakeStudyBackend.create[IO](StoryMoment.T2)
      entered <- Deferred[IO, Unit]
      gate    <- Deferred[IO, Unit]
      result  <- ExecutionService.resource[IO](Gated(fake, entered, gate)).use { service =>
        service.subscribe.use { events =>
          for
            collector <- collect(events).start
            pending   <- service.submit(stamp5).start
            _         <- entered.get
            _         <- service.require(stamp5Changed)
            _         <- gate.complete(())
            job       <- pending.joinWithNever.flatMap(e => ok(IO.pure(e)))
            out       <- collector.joinWithNever
            requested <- service.requested
            backend   <- ok(fake.job(job.id))
            late      <- fake.complete(job.id)
          yield (job, out, requested, backend, late)
        }
      }
    yield
      val (job, out, requested, backend, late) = result
      assertEquals(job.phase, JobPhase.Superseded(Some(stamp5Changed), None))
      assertEquals(requested, Some(stamp5Changed))
      assert(!out.exists(_._2.isInstanceOf[ExecutionEvent.Ready]), out)
      assert(!phases(out).exists(_.isInstanceOf[JobPhase.Succeeded]), out)
      assert(backend.state.isInstanceOf[JobState.Finished], backend)
      assert(late.left.exists(_.isInstanceOf[FakeControlError.NotRunning]), late)
      val waiting =
        out.map(_._2).foldLeft(RunShelf.of(Some(run7)).require(stamp5))(_.receive(_))
      assertEquals(waiting.require(stamp5Changed).pending, None)
      assertEquals(waiting.pending, None)
  }

  test("a stalled subscriber never blocks submit, progress or cancel") {
    for
      fake    <- FakeStudyBackend.create[IO](StoryMoment.T2)
      settled <- ExecutionService.resource[IO](fake, subscriberBuffer = 1).use { service =>
        service.subscribe.use { _ =>
          for
            job <- ok(service.submit(stamp5))
            _   <- List(1000L, 5000L, Held).traverse_ { p =>
              ok(fake.advanceToPairs(job.id, p)) >>
                await(service, job.id) { case JobPhase.Running(q) => q.pairs.done == p }
            }
            _       <- ok(service.cancel(job.id)).timeout(1.second)
            settled <- await(service, job.id) { case JobPhase.Cancelled(_) => true }
          yield settled
        }
      }
    yield assertEquals(pairsAt(settled.phase), Some(Held))
  }

  test("refusals name their operands") {
    setup(StoryMoment.T2).use { (fake, service, _) =>
      val wrongData = RunStamp(rev5, DatasetRevision(2), stamp5.plan, stamp5.input)
      for
        mismatch <- service.submit(wrongData)
        unknown  <- service.cancel(JobId(9))
        jobs     <- fake.jobs
        missing  <- service.adopt(JobId(9), stamp5)
      yield
        assertEquals(
          mismatch,
          Left(ExecutionError.StampMismatch(wrongData, JobId(1), rev5, r3))
        )
        assert(jobs.head.state.isInstanceOf[JobState.Finished], jobs)
        assertEquals(unknown, Left(ExecutionError.UnknownJob(JobId(9), Vector.empty)))
        assertEquals(missing.left.map(_.code), Left("studio-backend.unknown-job"))
        assertEquals(mismatch.left.map(_.code), Left("studio-execution.stamp-mismatch"))
    }
  }

  test("effects as data run on the service") {
    setup(StoryMoment.T2).use { (fake, service, _) =>
      val perform = ExecutionEffect.perform(service)
      for
        submitted <- perform(ExecutionEffect.Submit(stamp5))
        job       <- service.jobs.map(_.head)
        required  <- perform(ExecutionEffect.Require(stamp5Changed))
        req       <- service.requested
        cancelled <- perform(ExecutionEffect.Cancel(job.id))
        settled   <- await(service, job.id) { case JobPhase.Cancelled(_) => true }
        backend   <- ok(fake.job(job.id))
      yield
        assertEquals((submitted, required, cancelled), (Right(()), Right(()), Right(())))
        assertEquals(req, Some(stamp5Changed))
        assert(settled.phase.isInstanceOf[JobPhase.Cancelled])
        assert(backend.state.isInstanceOf[JobState.Finished], backend)
    }
  }

/** The fake with a subscription that ends at once: a lost job. */
private final class Silent(fake: FakeStudyBackend[IO]) extends StudyBackend[IO]:
  export fake.{
    admission,
    cancel,
    inspect,
    job,
    jobs,
    ledger,
    outcome,
    preview,
    previewCounting,
    previewRows,
    provenance,
    queries,
    result,
    runs,
    submit,
    submitPreview,
    trialFixations,
    trialPreview,
    sourceRecords,
    pairRows,
    continuePreview
  }

  def subscribe(id: JobId): IO[Either[BackendError, Stream[IO, JobEvent]]] =
    IO.pure(Right(Stream.empty))

/** The fake whose `submit` waits at `gate` after signalling `entered`: a
  * submission held while the backend answers.
  */
private final class Gated(
    fake: FakeStudyBackend[IO],
    entered: Deferred[IO, Unit],
    gate: Deferred[IO, Unit]
) extends StudyBackend[IO]:
  export fake.{
    admission,
    cancel,
    inspect,
    job,
    jobs,
    ledger,
    outcome,
    preview,
    previewCounting,
    previewRows,
    provenance,
    queries,
    result,
    runs,
    subscribe,
    submitPreview,
    trialFixations,
    trialPreview,
    sourceRecords,
    pairRows,
    continuePreview
  }

  def submit(revision: AnalysisRevision): IO[Either[BackendError, JobStatus]] =
    entered.complete(()) >> gate.get >> fake.submit(revision)
