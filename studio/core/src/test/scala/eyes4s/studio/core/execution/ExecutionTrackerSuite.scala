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
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen

/** The rules of [[ExecutionTracker]] and [[RunShelf]] over generated
  * interleavings of submissions, requirement changes, progress, cancel
  * requests, outcomes and Show (ticket S3.1, E2E-07, E2E-08).
  */
class ExecutionTrackerSuite extends munit.ScalaCheckSuite:
  import ExecutionTrackerSuite.*

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(300)

  property("a completion is Ready only if its stamp is the requested one (E2E-07)") {
    forAll(script) { ops =>
      run(ops).forall { case Transition(before, _, events) =>
        events.forall {
          case ExecutionEvent.Ready(n)   => before.requested.contains(n.stamp)
          case ExecutionEvent.Changed(j) =>
            j.phase match
              case JobPhase.Succeeded(_)     => before.requested.contains(j.stamp)
              case JobPhase.Superseded(c, _) =>
                !before.requested.contains(j.stamp) && c == before.requested
              case _ => true
        }
      }
    }
  }

  property("every Succeeded is followed by exactly its Ready, and nothing else is Ready") {
    forAll(script) { ops =>
      run(ops).forall { t =>
        val succeeded = t.events.collect {
          case ExecutionEvent.Changed(j) if j.phase.isInstanceOf[JobPhase.Succeeded] =>
            RunReady(j.id, j.run, j.stamp)
        }
        val ready = t.events.collect { case ExecutionEvent.Ready(n) => n }
        succeeded == ready && (ready.isEmpty || t.events.last == ExecutionEvent.Ready(
          ready.head
        ))
      }
    }
  }

  property("done units and steps never move back along a job") {
    forAll(script) { ops =>
      val reports = changes(ops).groupBy(_.id).values
      reports.forall { js =>
        val seen = js.flatMap(_.phase.lastReport)
        seen
          .zip(seen.drop(1))
          .forall((a, b) => a.step <= b.step && a.completedUnits <= b.completedUnits)
      }
    }
  }

  property("a terminal phase absorbs, and Cancelling never runs again") {
    forAll(script) { ops =>
      changes(ops).groupBy(_.id).values.forall { js =>
        val phases   = js.map(_.phase)
        val terminal = phases.indexWhere(_.isTerminal)
        val cancel   = phases.indexWhere(_.isInstanceOf[JobPhase.Cancelling])
        (terminal < 0 || terminal == phases.size - 1) &&
        (cancel < 0 || phases.drop(cancel).forall {
          case JobPhase.Running(_) | JobPhase.Queued => false
          case _                                     => true
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

  property("the shown run changes only by Show, and only to a run that was Ready") {
    forAll(script) { ops =>
      val start   = RunShelf.of(Some(RunId(7)))
      val ready   = scala.collection.mutable.Set.empty[RunId]
      val (_, ok) = run(ops).zip(ops).foldLeft((start, true)) { case ((shelf, ok), (t, op)) =>
        val received = t.events.foldLeft(shelf)(_.receive(_))
        t.events.foreach { case ExecutionEvent.Ready(n) => ready += n.run; case _ => () }
        val kept = ok && received.shown == shelf.shown
        op match
          case Op.Show(i) =>
            val candidate = t.after.jobs.lift(i % (t.after.jobs.size max 1)).map(_.run)
            candidate.fold((received, kept)) { r =>
              received.show(r) match
                case Right(next) => (next, kept && next.shown.contains(r) && ready.contains(r))
                case Left(_)     => (received, kept)
            }
          case Op.Require(s) => (received.require(stamps(s)), kept)
          case _             => (received, kept)
      }
      ok
    }
  }

  property("a superseded run is never offered or shown") {
    forAll(script) { ops =>
      val transitions = run(ops)
      val superseded  = transitions
        .flatMap(_.events)
        .collect {
          case ExecutionEvent.Changed(j) if j.phase.isInstanceOf[JobPhase.Superseded] => j.run
        }
        .toSet
      val offered =
        transitions.flatMap(_.events).collect { case ExecutionEvent.Ready(n) => n.run }
      offered.forall(r => !superseded.contains(r))
    }
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
    val once   = ExecutionTracker.empty.track(status, stamps(1)).map(_._1)
    assertEquals(
      once.flatMap(_.track(status, stamps(1))).left.map(_.code),
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
      JobPhase.Superseded(Some(stamps(1)), progress0)
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
    case Track(stamp: Int)
    case Require(stamp: Int)
    case Advance(job: Int, step: Long, pairs: Long)
    case Complete(job: Int, step: Long, pairs: Long)
    case CancelRequest(job: Int)
    case CancelledOutcome(job: Int, step: Long, pairs: Long)
    case FailedOutcome(job: Int, explained: Boolean)
    case Show(job: Int)

  private val stampIx = Gen.choose(0, stamps.size - 1)
  private val jobIx   = Gen.choose(0, 5)
  private val step    = Gen.choose(0L, 30L)
  private val pairs   = Gen.choose(0L, Total)

  val op: Gen[Op] = Gen.frequency(
    3 -> stampIx.map(Op.Track(_)),
    2 -> stampIx.map(Op.Require(_)),
    8 -> Gen.zip(jobIx, step, pairs).map(Op.Advance(_, _, _)),
    2 -> Gen.zip(jobIx, step, pairs).map(Op.Complete(_, _, _)),
    2 -> jobIx.map(Op.CancelRequest(_)),
    1 -> Gen.zip(jobIx, step, pairs).map(Op.CancelledOutcome(_, _, _)),
    1 -> Gen.zip(jobIx, Gen.oneOf(true, false)).map(Op.FailedOutcome(_, _)),
    2 -> jobIx.map(Op.Show(_))
  )

  val script: Gen[Vector[Op]] =
    Gen.choose(1, 60).flatMap(n => Gen.listOfN(n, op).map(_.toVector))

  final case class Transition(
      before: ExecutionTracker,
      after: ExecutionTracker,
      events: Vector[ExecutionEvent]
  )

  private val diagnostic = StudioDiagnostic(
    "study-failure.off-window",
    DiagnosticLevel.Error,
    DiagnosticOrigin.EyesCore,
    Vector.empty,
    "no fixation in the window"
  )

  /** Apply one op; ops naming a job when none is tracked do nothing. */
  def apply(t: ExecutionTracker, op: Op): Transition =
    def pick(i: Int) = t.jobs.lift(i % (t.jobs.size max 1))
    def on(
        i: Int
    )(f: ExecutionJob => Either[ExecutionError, ExecutionTracker.Step[ExecutionJob]]) =
      pick(i)
        .flatMap(j => f(j).toOption)
        .fold(Transition(t, t, Vector.empty))((n, e, _) => Transition(t, n, e))
    op match
      case Op.Track(s) =>
        val n      = t.jobs.size
        val status =
          JobStatus(JobId(n + 1), RunId(n + 8), stamps(s).revision, r3, JobState.Queued)
        t.track(status, stamps(s))
          .fold(_ => Transition(t, t, Vector.empty), (n, e, _) => Transition(t, n, e))
      case Op.Require(s)        => Transition(t, t.require(stamps(s)), Vector.empty)
      case Op.Advance(i, st, p) =>
        on(i)(j =>
          t.observe(
            j.id,
            JobEvent.Advanced(report(j.id, j.run, st, p, ProgressTotal.Exact(Total)))
          )
        )
      case Op.Complete(i, st, p) =>
        on(i)(j =>
          t.observe(
            j.id,
            JobEvent.Finished(
              JobOutcome.Completed(
                j.id,
                j.run,
                report(j.id, j.run, st, p, ProgressTotal.Exact(Total))
              )
            )
          )
        )
      case Op.CancelRequest(i)           => on(i)(j => t.cancelling(j.id))
      case Op.CancelledOutcome(i, st, p) =>
        on(i)(j =>
          t.observe(
            j.id,
            JobEvent.Finished(
              JobOutcome.Cancelled(
                j.id,
                j.run,
                Some(report(j.id, j.run, st, p, ProgressTotal.Exact(Total)))
              )
            )
          )
        )
      case Op.FailedOutcome(i, explained) =>
        on(i)(j =>
          t.observe(
            j.id,
            JobEvent.Finished(
              JobOutcome.Failed(
                j.id,
                j.run,
                if explained then Vector(diagnostic) else Vector.empty,
                None
              )
            )
          )
        )
      case Op.Show(_) => Transition(t, t, Vector.empty)

  def run(ops: Vector[Op]): Vector[Transition] =
    ops
      .scanLeft(Transition(ExecutionTracker.empty, ExecutionTracker.empty, Vector.empty))(
        (prev, op) => apply(prev.after, op)
      )
      .drop(1)

  def changes(ops: Vector[Op]): Vector[ExecutionJob] =
    run(ops).flatMap(_.events).collect { case ExecutionEvent.Changed(j) => j }
