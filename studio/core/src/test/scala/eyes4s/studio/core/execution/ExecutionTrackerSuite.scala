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

import eyes4s.codec.CanonicalDigest
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.CoreBinding
import io.circe.syntax.*
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

/** The rules of [[ExecutionTracker]] and [[RunShelf]] over generated
  * interleavings of submission intents, backend acceptances (which may
  * arrive after newer intents), requirement changes, progress, cancel
  * requests, outcomes and Show (ticket S3.1, E2E-07, E2E-08).
  *
  * Which completions may be offered is judged by [[ExecutionTrackerSuite.Oracle]],
  * a model of "the latest intent" computed from the op script alone, never
  * from the tracker's own `requested` field.
  */
class ExecutionTrackerSuite extends munit.ScalaCheckSuite:
  import ExecutionTrackerSuite.*

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(300)

  property("a run is offered exactly when the script's latest intent says so (E2E-07)") {
    forAll(script)(ops => run(ops).forall(s => s.readies == s.expected))
  }

  property("the requested stamp is the script's latest intent") {
    forAll(script)(ops =>
      run(ops).forall(s => s.world.tracker.requestedStamp == s.oracle.latest)
    )
  }

  property("every Succeeded is followed by exactly its Ready, and nothing else is Ready") {
    forAll(script) { ops =>
      run(ops).forall { s =>
        val succeeded = s.events.collect {
          case ExecutionEvent.Changed(j) if j.phase.isInstanceOf[JobPhase.Succeeded] =>
            RunReady(j.id, j.run, j.stamp)
        }
        succeeded == s.readies &&
        s.readies.headOption.forall(n => s.events.lastOption.contains(ExecutionEvent.Ready(n)))
      }
    }
  }

  property("done units and steps never move back along a job") {
    forAll(script) { ops =>
      changes(ops).groupBy(_.id).values.forall { js =>
        val seen = js.flatMap(_.phase.lastReport)
        seen
          .zip(seen.drop(1))
          .forall((a, b) => a.step <= b.step && a.completedUnits <= b.completedUnits)
      }
    }
  }

  property("a terminal phase absorbs, and Cancelling never runs or succeeds") {
    forAll(script) { ops =>
      changes(ops).groupBy(_.id).values.forall { js =>
        val phases   = js.map(_.phase)
        val terminal = phases.indexWhere(_.isTerminal)
        val cancel   = phases.indexWhere(_.isInstanceOf[JobPhase.Cancelling])
        (terminal < 0 || terminal == phases.size - 1) &&
        (cancel < 0 || phases.drop(cancel).forall {
          case JobPhase.Running(_) | JobPhase.Queued | JobPhase.Succeeded(_) => false
          case _                                                             => true
        })
      }
    }
  }

  property("a failed job always carries diagnostics with codes (E2E-13)") {
    forAll(script) { ops =>
      changes(ops).forall(_.phase match
        case JobPhase.Failed(ds, _) => ds.nonEmpty && ds.forall(_.code.contains('.'))
        case _                      => true)
    }
  }

  property("live jobs are kept; settled ones only the latest ten per revision") {
    forAll(script) { ops =>
      run(ops).forall { s =>
        val jobs    = s.world.tracker.jobs
        val settled = jobs.filter(_.phase.isTerminal).groupBy(_.stamp.revision).values
        settled.forall(_.size <= ExecutionTracker.RetainedPerRevision) &&
        s.oracle.jobs
          .collect { case (id, j) if j.live => JobId(id) }
          .forall(id => jobs.exists(_.id == id))
      }
    }
  }

  property("the shown run changes only by Show, to a run the oracle offered") {
    forAll(script) { ops =>
      val start   = RunShelf.of(Some(RunId(7)))
      val (_, ok) = run(ops).zip(ops).foldLeft((start, true)) { case ((shelf, ok), (s, op)) =>
        val received     = s.events.foldLeft(shelf)(_.receive(_))
        val kept         = ok && received.shown == shelf.shown
        val (next, fine) = op match
          case Op.Show(i) if s.oracle.created > 0 =>
            val r = runOf(i % s.oracle.created + 1)
            received.show(r) match
              case Right(shown) =>
                (shown, kept && shown.shown.contains(r) && s.oracle.offered.contains(r))
              case Left(_) => (received, kept)
          case Op.Intend(k)  => (received.require(stamps(k)), kept)
          case Op.Require(k) => (received.require(stamps(k)), kept)
          case _             => (received, kept)
        (next, fine && next.pending.forall(p => s.oracle.latest.contains(p.stamp)))
      }
      ok
    }
  }

  test("a submission accepted after a newer intent is Superseded from the start") {
    val (intended, generation) = ExecutionTracker.empty.intend(stamps(1))
    val required               = intended.require(stamps(2))
    val status                 = JobStatus(JobId(1), RunId(8), rev5, r3, JobState.Queued)
    required.track(status, stamps(1), generation) match
      case Right((after, events, job)) =>
        assertEquals(job.phase, JobPhase.Superseded(Some(stamps(2)), None))
        assertEquals(events, Vector(ExecutionEvent.Changed(job)))
        assertEquals(after.requestedStamp, Some(stamps(2)))
      case Left(e) => fail(e.message)
  }

  test("of twelve settled rev 5 jobs the latest ten are kept, with every live one") {
    def settledJob(t: ExecutionTracker, id: Int): ExecutionTracker =
      val (intended, g) = t.intend(stamps(1))
      val status        = JobStatus(JobId(id), RunId(id + 7), rev5, r3, JobState.Queued)
      val outcome       = JobOutcome.Cancelled(JobId(id), RunId(id + 7), None)
      (for
        tracked <- intended.track(status, stamps(1), g)
        settled <- tracked._1.observe(JobId(id), JobEvent.Finished(outcome))
      yield settled._1).fold(e => fail(e.message), identity)
    val many          = (1 to 12).foldLeft(ExecutionTracker.empty)(settledJob)
    val (withLive, g) = many.intend(stamps(0))
    val live          = JobStatus(JobId(13), RunId(20), rev4, r3, JobState.Queued)
    val after         = withLive.track(live, stamps(0), g).fold(e => fail(e.message), _._1)
    assertEquals(after.jobs.map(_.id.number), (3 to 13).toVector)
    assertEquals(
      after.job(JobId(1)).left.map(_.code),
      Left("studio-execution.unknown-job")
    )
  }

  test("a job being cancelled that completes is Cancelled, not Ready") {
    val (intended, generation) = ExecutionTracker.empty.intend(stamps(1))
    val status                 = JobStatus(JobId(1), RunId(8), rev5, r3, JobState.Queued)
    val last   = report(JobId(1), RunId(8), 4L, Total, ProgressTotal.Exact(Total))
    val result =
      for
        tracked   <- intended.track(status, stamps(1), generation)
        cancelled <- tracked._1.cancelling(JobId(1))
        settled   <- cancelled._1.observe(
          JobId(1),
          JobEvent.Finished(JobOutcome.Completed(JobId(1), RunId(8), last))
        )
      yield (settled._2, settled._3.phase)
    result match
      case Right((events, phase)) =>
        assert(phase.isInstanceOf[JobPhase.Cancelled], phase)
        assert(!events.exists(_.isInstanceOf[ExecutionEvent.Ready]), events)
      case Left(e) => fail(e.message)
  }

  test("the shelf refuses a notice it did not require, and keeps the newer one") {
    val older = RunReady(JobId(1), RunId(8), stamps(1))
    val newer = RunReady(JobId(2), RunId(9), stamps(1))
    val shelf = RunShelf.of(Some(RunId(7)))
    assertEquals(shelf.receive(ExecutionEvent.Ready(newer)).pending, None)
    val wanting = shelf.require(stamps(1))
    val other   = RunReady(JobId(3), RunId(10), stamps(2))
    assertEquals(wanting.receive(ExecutionEvent.Ready(other)).pending, None)
    val both = wanting.receive(ExecutionEvent.Ready(newer)).receive(ExecutionEvent.Ready(older))
    assertEquals(both.pending, Some(newer))
  }

  test("an unknown total reads as counting") {
    assertEquals(MeterTotal.of(ProgressTotal.Unknown), MeterTotal.Counting)
    assertEquals(MeterTotal.of(ProgressTotal.Exact(44845L)), MeterTotal.Exact(44845L))
    assertEquals(MeterTotal.of(ProgressTotal.AtMost(457L)), MeterTotal.AtMost(457L))
    val p = ExecutionProgress(report(JobId(1), RunId(8), 3L, 21400L, ProgressTotal.Unknown))
    assertEquals(p.pairs, Meter(CountUnit.Pairs, 21400L, MeterTotal.Counting))
    assertEquals(p.stageMeter.total, MeterTotal.Counting)
  }

  test("tracking the same job twice is refused, naming it") {
    val status = JobStatus(JobId(1), RunId(8), rev5, r3, JobState.Queued)
    val (t, g) = ExecutionTracker.empty.intend(stamps(1))
    val once   = t.track(status, stamps(1), g).map(_._1)
    assertEquals(
      once.flatMap(_.track(status, stamps(1), g)).left.map(_.code),
      Left("studio-execution.already-tracked")
    )
    assertEquals(
      ExecutionTracker.empty.cancelling(JobId(3)),
      Left(ExecutionError.UnknownJob(JobId(3), Vector.empty))
    )
  }

  test("stamps, jobs and events round-trip through JSON") {
    val job = ExecutionJob(
      JobId(1),
      RunId(8),
      stamps(2),
      JobPhase.Superseded(Some(stamps(1)), Some(progress0))
    )
    val evts = Vector(
      ExecutionEvent.Changed(job),
      ExecutionEvent.Ready(RunReady(JobId(1), RunId(8), stamps(2)))
    )
    evts.foreach(e => assertEquals(e.asJson.as[ExecutionEvent], Right(e)))
    stamps.foreach(s => assertEquals(s.asJson.as[RunStamp], Right(s)))
  }

object ExecutionTrackerSuite:

  val r3: DatasetRevision    = DatasetRevision(3)
  val rev4: AnalysisRevision = AnalysisRevision(4)
  val rev5: AnalysisRevision = AnalysisRevision(5)

  private val digest: CanonicalDigest[StudyInputArtifact] =
    CanonicalDigest
      .parse[StudyInputArtifact]("9f2c" * 16)
      .fold(e => throw new AssertionError(e), identity)

  /** Rev 4, rev 5, and rev 5 whose study input has since changed. */
  val stamps: Vector[RunStamp] = Vector(
    RunStamp(rev4, r3, CoreBinding.unbound, CoreBinding.unbound),
    RunStamp(rev5, r3, CoreBinding.unbound, CoreBinding.unbound),
    RunStamp(rev5, r3, CoreBinding.unbound, CoreBinding.Bound(digest))
  )

  val Total: Long = 44845L

  def report(
      job: JobId,
      run: RunId,
      step: Long,
      pairs: Long,
      total: ProgressTotal
  ): JobProgress =
    (for
      meter  <- StageMeter.of(StageKind.Comparing, CountUnit.Pairs, pairs, total)
      totals <- RunTotals.of(0L, ProgressTotal.Exact(0L), pairs, total)
      p      <- JobProgress.of(
        job,
        run,
        step,
        Segment.Comparing(0, PairDesign.Matched),
        meter,
        totals
      )
    yield p).fold(e => throw new AssertionError(e.message), identity)

  val progress0: ExecutionProgress =
    ExecutionProgress(report(JobId(1), RunId(8), 1L, 21400L, ProgressTotal.Exact(Total)))

  enum Op:
    /** A submission is sent: its intent is recorded, the backend has not answered. */
    case Intend(stamp: Int)

    /** The backend answers one in-flight submission, not necessarily the oldest. */
    case Accept(submission: Int)
    case Require(stamp: Int)
    case Advance(job: Int, step: Long, pairs: Long)
    case Complete(job: Int, step: Long, pairs: Long)
    case CancelRequest(job: Int)
    case CancelledOutcome(job: Int, step: Long, pairs: Long)
    case FailedOutcome(job: Int, explained: Boolean)
    case Show(job: Int)

  private val stampIx = Gen.choose(0, stamps.size - 1)
  private val jobIx   = Gen.choose(0, 40)
  private val step    = Gen.choose(0L, 30L)
  private val pairs   = Gen.choose(0L, Total)

  val op: Gen[Op] = Gen.frequency(
    4 -> stampIx.map(Op.Intend(_)),
    4 -> Gen.choose(0, 3).map(Op.Accept(_)),
    2 -> stampIx.map(Op.Require(_)),
    6 -> Gen.zip(jobIx, step, pairs).map(Op.Advance(_, _, _)),
    3 -> Gen.zip(jobIx, step, pairs).map(Op.Complete(_, _, _)),
    2 -> jobIx.map(Op.CancelRequest(_)),
    1 -> Gen.zip(jobIx, step, pairs).map(Op.CancelledOutcome(_, _, _)),
    1 -> Gen.zip(jobIx, Gen.oneOf(true, false)).map(Op.FailedOutcome(_, _)),
    2 -> jobIx.map(Op.Show(_))
  )

  val script: Gen[Vector[Op]] =
    Gen.choose(1, 120).flatMap(n => Gen.listOfN(n, op).map(_.toVector))

  def runOf(job: Int): RunId = RunId(job + 7)

  // -------------------------------------------------------------------------
  // The oracle: the script's own account of intents and job lives
  // -------------------------------------------------------------------------

  final case class OracleJob(stamp: RunStamp, live: Boolean, cancelling: Boolean)

  /** `epoch` counts intents; a submission is accepted only if no intent came
    * between it and the backend's answer. A completion is offered only for a
    * live, uncancelled job whose stamp is the latest intent's.
    */
  final case class Oracle(
      epoch: Int,
      latest: Option[RunStamp],
      inFlight: Vector[(Int, Int)],
      jobs: Map[Int, OracleJob],
      created: Int,
      offered: Set[RunId]
  ):
    private def job(i: Int): Option[(Int, OracleJob)] =
      Option.when(created > 0)(i % created + 1).flatMap(id => jobs.get(id).map(id -> _))

    private def settle(i: Int): Oracle = job(i).fold(this) { (id, j) =>
      copy(jobs = jobs.updated(id, j.copy(live = false)))
    }

    /** The next oracle and the notices this op must publish. */
    def step(op: Op): (Oracle, Vector[RunReady]) = op match
      case Op.Intend(k) =>
        val next = epoch + 1
        (
          copy(epoch = next, latest = Some(stamps(k)), inFlight = inFlight :+ (k -> next)),
          Vector.empty
        )
      case Op.Require(k) =>
        (copy(epoch = epoch + 1, latest = Some(stamps(k))), Vector.empty)
      case Op.Accept(i) if inFlight.nonEmpty =>
        val at         = i % inFlight.size
        val (k, since) = inFlight(at)
        val id         = created + 1
        val next       = copy(
          inFlight = inFlight.patch(at, Nil, 1),
          jobs = jobs.updated(id, OracleJob(stamps(k), since == epoch, false)),
          created = id
        )
        (next, Vector.empty)
      case Op.Complete(i, _, _) =>
        job(i) match
          case Some((id, j)) if j.live && !j.cancelling && latest.contains(j.stamp) =>
            val notice = RunReady(JobId(id), runOf(id), j.stamp)
            (settle(i).copy(offered = offered + notice.run), Vector(notice))
          case _ => (settle(i), Vector.empty)
      case Op.CancelledOutcome(i, _, _) => (settle(i), Vector.empty)
      case Op.FailedOutcome(i, _)       => (settle(i), Vector.empty)
      case Op.CancelRequest(i)          =>
        job(i) match
          case Some((id, j)) if j.live =>
            (copy(jobs = jobs.updated(id, j.copy(cancelling = true))), Vector.empty)
          case _ => (this, Vector.empty)
      case _ => (this, Vector.empty)

  object Oracle:
    val empty: Oracle = Oracle(0, None, Vector.empty, Map.empty, 0, Set.empty)

  // -------------------------------------------------------------------------
  // The world: the tracker driven as ExecutionService drives it
  // -------------------------------------------------------------------------

  /** The tracker and the submissions whose backend answer is pending. */
  final case class World(
      tracker: ExecutionTracker,
      inFlight: Vector[(Int, Long)],
      created: Int
  )

  private val diagnostic = StudioDiagnostic(
    "study-failure.off-window",
    DiagnosticLevel.Error,
    DiagnosticOrigin.EyesCore,
    Vector.empty,
    "no fixation in the window"
  )

  /** Apply one op; the events it published. Ops naming no job do nothing. */
  def apply(w: World, op: Op): (World, Vector[ExecutionEvent]) =
    val t = w.tracker
    def on(i: Int)(
        f: JobId => Either[ExecutionError, ExecutionTracker.Step[ExecutionJob]]
    ): (World, Vector[ExecutionEvent]) =
      Option
        .when(w.created > 0)(JobId(i % w.created + 1))
        .flatMap(id => f(id).toOption)
        .fold((w, Vector.empty))((n, e, _) => (w.copy(tracker = n), e))
    def at(id: JobId, st: Long, p: Long) =
      report(id, runOf(id.number), st, p, ProgressTotal.Exact(Total))
    op match
      case Op.Intend(k) =>
        val (n, g) = t.intend(stamps(k))
        (w.copy(tracker = n, inFlight = w.inFlight :+ (k -> g)), Vector.empty)
      case Op.Accept(i) if w.inFlight.nonEmpty =>
        val index  = i % w.inFlight.size
        val (k, g) = w.inFlight(index)
        val id     = w.created + 1
        val status = JobStatus(JobId(id), runOf(id), stamps(k).revision, r3, JobState.Queued)
        val rest   = w.inFlight.patch(index, Nil, 1)
        t.track(status, stamps(k), g)
          .fold(
            _ => (w.copy(inFlight = rest, created = id), Vector.empty),
            (n, e, _) => (World(n, rest, id), e)
          )
      case Op.Accept(_)         => (w, Vector.empty)
      case Op.Require(k)        => (w.copy(tracker = t.require(stamps(k))), Vector.empty)
      case Op.Advance(i, st, p) =>
        on(i)(id => t.observe(id, JobEvent.Advanced(at(id, st, p))))
      case Op.Complete(i, st, p) =>
        on(i)(id =>
          t.observe(
            id,
            JobEvent.Finished(JobOutcome.Completed(id, runOf(id.number), at(id, st, p)))
          )
        )
      case Op.CancelRequest(i)           => on(i)(id => t.cancelling(id))
      case Op.CancelledOutcome(i, st, p) =>
        on(i)(id =>
          t.observe(
            id,
            JobEvent.Finished(JobOutcome.Cancelled(id, runOf(id.number), Some(at(id, st, p))))
          )
        )
      case Op.FailedOutcome(i, explained) =>
        on(i)(id =>
          t.observe(
            id,
            JobEvent.Finished(
              JobOutcome.Failed(
                id,
                runOf(id.number),
                if explained then Vector(diagnostic) else Vector.empty,
                None
              )
            )
          )
        )
      case Op.Show(_) => (w, Vector.empty)

  /** One op's outcome: the world and oracle after it, the events the tracker
    * published and the notices the oracle expected.
    */
  final case class Stepped(
      world: World,
      oracle: Oracle,
      events: Vector[ExecutionEvent],
      expected: Vector[RunReady]
  ):
    def readies: Vector[RunReady] = events.collect { case ExecutionEvent.Ready(n) => n }

  private val start =
    Stepped(
      World(ExecutionTracker.empty, Vector.empty, 0),
      Oracle.empty,
      Vector.empty,
      Vector.empty
    )

  def run(ops: Vector[Op]): Vector[Stepped] =
    ops
      .scanLeft(start) { (prev, op) =>
        val (w, events)   = apply(prev.world, op)
        val (o, expected) = prev.oracle.step(op)
        Stepped(w, o, events, expected)
      }
      .drop(1)

  def changes(ops: Vector[Op]): Vector[ExecutionJob] =
    run(ops).flatMap(_.events).collect { case ExecutionEvent.Changed(j) => j }
