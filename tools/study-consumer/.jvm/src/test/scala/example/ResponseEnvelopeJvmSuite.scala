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

package example

import cats.effect.{Deferred, IO}
import cats.syntax.all.*
import eyes4s.compare.ComparisonBudget
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.fs2.*
import eyes4s.io.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import io.circe.Json

import java.util.concurrent.atomic.AtomicLong
import scala.concurrent.duration.*

/** UI-G1: the declared JVM response envelope, measured from the consumer
  * through published APIs only. A small smoke run, not a benchmark: for each
  * route it times every step of the family's `Stepwise` cursor exactly as the
  * runner's uncancelable region calls it, and the latency from `cancel` to a
  * settled outcome through `Execution.start`, cancelling as soon as the hook
  * before a target step has run: one trial targets the slowest step, three are
  * spread over the run. The canceller is woken asynchronously, so a trial can
  * land just before its target step, during it, or a few short steps later;
  * each trial records where the run stopped (`steps_past_target`: -1 before
  * the target step, 0 at it). It adds the widest study the envelope supports
  * (a 256 x 256 grid, Gaussian sigma 32 cells, on 12 trials;
  * `docs/EXECUTION_RESPONSIVENESS.md` measures 100) and a 10 s recording at 1 kHz.
  *
  * Nothing here asserts a time. The only assertions are the runner's outcome
  * contract: a cancelled run holds no result and stopped short of its
  * terminal step, and a run that finished did every step. Each workload
  * prints `EYES4S_ENVELOPE=`; verify.py records them in its receipt.
  */
class ResponseEnvelopeJvmSuite extends munit.CatsEffectSuite:
  import JourneySetup.get

  override def munitIOTimeout: Duration = 10.minutes

  /** A timed workload: a fresh submission per run, and its stage names. */
  private final class Probe[Id, C, Stage, Segment, E, R](
      val workload: String,
      val route: String,
      val submission: () => Submission[Id, C, Stage, Segment, E, R],
      val kind: Stage => String
  )

  private final case class Profile(
      steps: Long,
      units: Long,
      durations: Vector[Long],
      stages: Vector[String]
  ):
    def max: Long           = durations.max
    def slowest: Long       = durations.indexOf(max).toLong + 1
    def slowestKind: String = stages(durations.indexOf(max))
    def p95: Long           =
      val sorted = durations.sorted
      sorted(math.min(sorted.size - 1, math.ceil(sorted.size * 0.95).toInt - 1))

  /** Drive the cursor to completion, timing every `advance`. */
  private def profile[Id, C, Stage, Segment, E, R](
      probe: Probe[Id, C, Stage, Segment, E, R]
  ): Profile =
    val submission = probe.submission()
    val stepwise   = submission.stepwise
    val durations  = Vector.newBuilder[Long]
    val stages     = Vector.newBuilder[String]
    @annotation.tailrec
    def loop(cursor: C, steps: Long, units: Long): (Long, Long) =
      val before = stepwise.stage(cursor)
      val t0     = System.nanoTime()
      val step   = stepwise.advance(cursor, submission.quanta)
      durations += System.nanoTime() - t0
      step match
        case Left(error)                          => fail(s"${probe.workload} failed: $error")
        case Right(WorkStep.More(stage, u, next)) =>
          stages += probe.kind(stage)
          loop(next, steps + 1, units + u)
        case Right(WorkStep.Done(u, _)) =>
          stages += probe.kind(before)
          (steps + 1, units + u)
    val (steps, units) = loop(get(submission.begin()), 0L, 0L)
    Profile(steps, units, durations.result(), stages.result())

  /** Cancel once the hook before step `target` has run, and time the settlement. */
  private def cancelAt[Id, C, Stage, Segment, E, R](
      probe: Probe[Id, C, Stage, Segment, E, R],
      target: Long,
      steps: Long
  ): IO[(Long, Long, String, Option[Long])] =
    val counter = new AtomicLong(0L)
    Deferred[IO, Unit]
      .flatMap { reached =>
        val between = IO.cede >> IO(counter.incrementAndGet())
          .flatMap(n => if n == target then reached.complete(()).void else IO.unit)
        Execution[IO].start(probe.submission(), between).use { run =>
          reached.get >> IO(System.nanoTime()).flatMap(t0 =>
            run.cancel >> run.outcome.flatMap(o => IO(System.nanoTime() - t0).map(_ -> o))
          )
        }
      }
      .map { (latency, outcome) =>
        // Lawful only: a cancelled run holds no result and stopped short of its
        // last step; a run the cancellation reached too late did every step.
        val (settled, past) = outcome match
          case RunOutcome.Cancelled(_, last) =>
            val at = last.fold(0L)(_.step)
            assert(
              at >= target - 1 && at < steps,
              s"cancelled at step $at of $steps, aimed at $target"
            )
            (s"cancelled-at-$at", Some(at - target))
          case RunOutcome.Completed(_, last, _) =>
            assertEquals(last.step, steps)
            ("completed", None)
          case RunOutcome.Failed(_, error, _) => fail(s"${probe.workload} failed: $error")
        (target, latency, settled, past)
      }

  private def ms(nanos: Long): Json = Json.fromDoubleOrNull(math.round(nanos / 1e3) / 1e3)

  private def measure[Id, C, Stage, Segment, E, R](
      probe: Probe[Id, C, Stage, Segment, E, R]
  ): IO[Unit] =
    val _        = profile(probe) // warm the JIT on the same path
    val measured = profile(probe)
    val spread   = (1 to 3).map(i => math.max(1L, i * measured.steps / 4)).toVector
    val targets  = (measured.slowest +: spread.filterNot(_ == measured.slowest)).distinct
    targets.traverse(cancelAt(probe, _, measured.steps)).map { trials =>
      val latencies = trials.map(_._2)
      println(
        "EYES4S_ENVELOPE=" + Json
          .obj(
            "workload"                      -> Json.fromString(probe.workload),
            "route"                         -> Json.fromString(probe.route),
            "steps"                         -> Json.fromLong(measured.steps),
            "units"                         -> Json.fromLong(measured.units),
            "max_step_ms"                   -> ms(measured.max),
            "max_step_stage"                -> Json.fromString(measured.slowestKind),
            "p95_step_ms"                   -> ms(measured.p95),
            "cancel_adversarial_ms"         -> ms(latencies.head),
            "adversarial_steps_past_target" -> trials.head._4.fold(Json.Null)(Json.fromLong),
            "cancel_max_ms"                 -> ms(latencies.max),
            "cancel_median_ms"              -> ms(latencies.sorted.apply(latencies.size / 2)),
            "cancel_trials" -> Json.arr(trials.map { (target, latency, settled, past) =>
              Json.obj(
                "target_step"       -> Json.fromLong(target),
                "latency_ms"        -> ms(latency),
                "outcome"           -> Json.fromString(settled),
                "steps_past_target" -> past.fold(Json.Null)(Json.fromLong)
              )
            }*),
            "within_100ms" -> Json.fromBoolean(
              (measured.max +: latencies).forall(_ <= 100_000_000L)
            )
          )
          .noSpaces
      )
    }

  private def studyKind(stage: StudyStage): String = stage match
    case StudyStage.Estimating(_, _) => "estimating"
    case StudyStage.Comparing(_, _)  => "comparing"
    case StudyStage.Reducing(_, _)   => "reducing"
    case StudyStage.Contrasting(_)   => "contrasting"

  private def recordingKind(stage: RecordingStage): String = stage match
    case RecordingStage.Synchronizing    => "synchronizing"
    case RecordingStage.Warping          => "warping"
    case RecordingStage.Interpolating(_) => "interpolating"
    case RecordingStage.Detecting(_)     => "detecting"
    case RecordingStage.Assigning        => "assigning"

  private def temporalKind(stage: TemporalStage): String = stage match
    case TemporalStage.Preparing(_, _, _)    => "preparing"
    case TemporalStage.Studying(_, _, inner) => studyKind(inner)

  private def study[K, P, S, D](j: Journey[K, P, S, D]) = new Probe(
    s"study-journey-${j.c.name}",
    "study",
    () => StudyExecution.submission(j.work, JourneySetup.comparison, JourneySetup.quanta),
    studyKind
  )

  private def recording[P](run: RecordingCase[P]) = new Probe(
    s"recording-fixture-${run.route.name}",
    "recording",
    () =>
      RecordingExecution
        .submission(run.plan, RecordingFixtures.recording, RecordingFixtures.quanta),
    recordingKind
  )

  private def temporal[K, P, S, D](t: TemporalCase[K, P, S, D]) = new Probe(
    s"temporal-fixture-${t.c.name}",
    "temporal",
    () => TemporalExecution.submission(t.work, JourneySetup.comparison, JourneySetup.quanta),
    temporalKind
  )

  /** The widest study the envelope supports: 12 trials of ten
    * seeded fixations on a 256 x 256 display, one-pixel cells, Gaussian
    * sigma 32 cells (Truncate), under the default quanta.
    */
  private def envelope =
    val j      = Journey(JourneyCases.cosine)
    val random = new scala.util.Random(0x4731L)
    val rows   = for
      phase       <- Vector("encode", "recall")
      participant <- Vector("s1", "s2")
      image       <- Vector("a", "b", "c")
      fixation    <- 0 until 10
    yield Vector(
      participant,
      image,
      phase,
      fixation.toString,
      (random.nextInt(25600) / 100.0).toString,
      (random.nextInt(25600) / 100.0).toString,
      (fixation * 110000L).toString,
      "100000",
      "100"
    ).mkString(",")
    val table = (FixationColumnsHeader +: rows).mkString("", "\n", "\n")
    val frame = get(Frame.screen("envelope-display", 256, 256))
    val grid  = get(Grid.over(frame, 256, 256))
    val input = get(
      get(
        FixationCsv.read(
          table,
          JourneySetup.columns,
          j.route.reader,
          frame,
          TimestampUnit.Microseconds
        )
      ).requireComplete
    )
    val plan = get(
      j.route.plan(
        input.reference,
        grid,
        JourneySetup.phases,
        Weight.Duration,
        Vector(
          StudyEstimate.Gaussian(
            get(RecipeParameters.sigma[Px].parse(32.0)),
            get(RecipeParameters.edges.parse(eyes4s.surface.EdgePolicy.Truncate))
          )
        ),
        JourneySetup.policy
      )
    )
    val work = get(plan.prepare(input, JourneySetup.pairs))
    new Probe(
      "study-256x256-sigma32",
      "study",
      () =>
        StudyExecution.submission(work, get(ComparisonBudget.of(65536)), WorkQuanta.default),
      studyKind
    )

  private val FixationColumnsHeader =
    "participant,image,phase,fixation,x_px,y_px,onset_us,duration_us,sample_count"

  /** Ten seconds at 1 kHz: 250 ms fixations on a 5 x 5 lattice, 20 ms saccades. */
  private def longRecording =
    import RecordingFixtures.{analysisClock, angular, display, source, trackerClock, viewing}
    val samples                = 10_000
    def gaze(i: Int): Gaze[Px] =
      val cycle      = i / 270
      val phase      = i % 270
      def at(c: Int) = Pt[Px](200.0 + 150.0 * (c % 5), 200.0 + 150.0 * ((c / 5) % 5))
      if phase < 250 then Gaze.Tracked(at(cycle), None)
      else
        val (a, b) = (at(cycle), at(cycle + 1))
        val f      = (phase - 250 + 1) / 21.0
        Gaze.Tracked(Pt[Px](a.x + f * (b.x - a.x), a.y + f * (b.y - a.y)), None)
    val recording = get(
      Recording.of(
        display,
        trackerClock,
        Rate.Fixed(get(Hz(1000.0))),
        Eye.Left,
        None,
        IArray.from((0 until samples).map(i => Sample(Instant.millis(i.toLong), gaze(i))))
      )
    )
    val route = RecordingRun.ivt.route
    val plan  = get(
      RecordingPlan.of(
        ArtifactRef.of[Recording[Px]](recording.contentHash),
        source,
        display,
        trackerClock,
        analysisClock,
        angular,
        Some(viewing),
        SyncFitMode.OffsetOnly,
        Vector(
          get(SyncMark.of("start", Instant.millis(0), Instant.millis(0))),
          get(SyncMark.of("end", Instant.millis(samples - 1L), Instant.millis(samples - 1L)))
        ),
        None,
        RecordingFixtures.gap,
        RecordingFixtures.areas,
        route.method,
        route.parameters
      )
    )
    new Probe(
      "recording-10s",
      "recording",
      () => RecordingExecution.submission(plan, recording, WorkQuanta.default),
      recordingKind
    )

  test("report the JVM response envelope of every route, without asserting a time") {
    val runtime = Runtime.getRuntime
    println(
      "EYES4S_ENVELOPE_RUNTIME=" + Json
        .obj(
          "java"         -> Json.fromString(System.getProperty("java.version")),
          "vm"           -> Json.fromString(System.getProperty("java.vm.name")),
          "vendor"       -> Json.fromString(System.getProperty("java.vendor")),
          "os"           -> Json.fromString(System.getProperty("os.name")),
          "os_version"   -> Json.fromString(System.getProperty("os.version")),
          "arch"         -> Json.fromString(System.getProperty("os.arch")),
          "processors"   -> Json.fromInt(runtime.availableProcessors),
          "max_heap_mib" -> Json.fromLong(runtime.maxMemory / (1024L * 1024L)),
          "effect"       -> Json.fromString("munit-cats-effect IORuntime")
        )
        .noSpaces
    )
    for
      _ <- measure(study(Journey(JourneyCases.cosine)))
      _ <- measure(study(Journey(JourneyCases.scaled)))
      _ <- measure(recording(RecordingRun.ivt))
      _ <- measure(recording(RecordingRun.lab))
      _ <- measure(temporal(TemporalRun.cosine))
      _ <- measure(temporal(TemporalRun.scaled))
      _ <- measure(envelope)
      _ <- measure(longRecording)
    yield ()
  }
