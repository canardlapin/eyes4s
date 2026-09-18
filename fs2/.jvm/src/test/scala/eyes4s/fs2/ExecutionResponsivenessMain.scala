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

package eyes4s.fs2

import cats.effect.{Deferred, IO}
import cats.effect.unsafe.implicits.global
import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Deg, Px}
import eyes4s.plan.*
import eyes4s.surface.EdgePolicy

import java.lang.management.ManagementFactory
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, StandardOpenOption}
import java.util.concurrent.atomic.AtomicLong

import scala.collection.mutable
import scala.jdk.CollectionConverters.*

/** One submission to measure: the runner's own [[Submission]] and a coarse
  * name for each stage, so step times can be attributed to estimation,
  * comparison, reduction, detection and so on.
  */
private[fs2] trait ResponsivenessProbe:
  type Id
  type C
  type Stage
  type Segment
  type E
  type R
  def submission: Submission[Id, C, Stage, Segment, E, R]
  def kind(stage: Stage): String

private[fs2] object ResponsivenessProbe:
  def apply[I, C0, S0, G, E0, R0](
      work: Submission[I, C0, S0, G, E0, R0],
      name: S0 => String
  ): ResponsivenessProbe = new ResponsivenessProbe:
    type Id      = I
    type C       = C0
    type Stage   = S0
    type Segment = G
    type E       = E0
    type R       = R0
    val submission: Submission[I, C0, S0, G, E0, R0] = work
    def kind(stage: S0): String                      = name(stage)

private[fs2] final case class ResponsivenessWorkload(
    id: String,
    route: String,
    envelope: String,
    quanta: String,
    probe: () => ResponsivenessProbe
)

/** Streaming duration statistics: exact count, sum and maximum, and a
  * logarithmic histogram (ratio 1.05) for the 95th percentile, so tens of
  * millions of steps are summarised without retaining them. The reported
  * p95 is the upper edge of its bucket, at most 5% above the true value.
  */
private[fs2] final class DurationStats:
  var count: Long     = 0L
  var sum: Long       = 0L
  var max: Long       = 0L
  var maxStep: Long   = 0L
  private val buckets = new Array[Long](DurationStats.Buckets)

  def add(nanos: Long, step: Long): Unit =
    count += 1
    sum += nanos
    if nanos > max then
      max = nanos
      maxStep = step
    buckets(DurationStats.bucket(nanos)) += 1

  def p95: Long =
    if count == 0 then 0L
    else
      val wanted = math.ceil(0.95 * count.toDouble).toLong
      var seen   = 0L
      var b      = 0
      while b < buckets.length && seen + buckets(b) < wanted do
        seen += buckets(b)
        b += 1
      math.min(max, math.ceil(math.pow(DurationStats.Ratio, b.toDouble)).toLong)

private[fs2] object DurationStats:
  val Ratio: Double            = 1.05
  val Buckets: Int             = 640
  def bucket(nanos: Long): Int =
    if nanos <= 1L then 0
    else math.min(Buckets - 1, math.ceil(math.log(nanos.toDouble) / math.log(Ratio)).toInt)

private[fs2] final case class PureProfile(
    steps: Long,
    units: Long,
    wallNanos: Long,
    overall: DurationStats,
    byKind: mutable.LinkedHashMap[String, DurationStats],
    slowestKind: String
)

private[fs2] final case class RunnerProfile(
    wallNanos: Long,
    gaps: DurationStats
)

private[fs2] final case class CancelTrial(
    target: Long,
    adversarial: Boolean,
    latencyNanos: Long,
    outcome: String
)

private[fs2] final case class ResponsivenessResult(
    workload: ResponsivenessWorkload,
    pure: PureProfile,
    runner: Option[RunnerProfile],
    cancels: Vector[CancelTrial],
    reach: Long,
    violations: Vector[String]
):
  def cancelMax: Long    = cancels.map(_.latencyNanos).maxOption.getOrElse(0L)
  def cancelMedian: Long =
    val sorted = cancels.map(_.latencyNanos).sorted
    if sorted.isEmpty then 0L else sorted(sorted.size / 2)
  def adversarial: Option[Long] = cancels.find(_.adversarial).map(_.latencyNanos)
  def worst: Long               =
    (Vector(pure.overall.max, cancelMax) ++ runner.map(_.gaps.max).toVector).max
  def meets(budgetNanos: Long): Boolean = worst <= budgetNanos

/** Measures how long the shipped runner can go without yielding and how long
  * a cancellation takes to settle, on declared workloads. Wall-clock timing
  * on a pinned JVM (see `fs2ModuleJVM / Test / run / javaOptions`), never
  * `TestControl`, and never asserted: the harness fails only when the
  * runner breaks its outcome contract, never because a machine is slow.
  *
  * Three measurements per workload and quanta:
  *
  *   - Step duration: the family's cursor is driven by its [[Stepwise]]
  *     instance, exactly as the runner's uncancelable region calls it, and
  *     each `advance` is timed. The maximum is the longest time the runner
  *     can be unable to yield; it is attributed to a stage kind.
  *   - Runner gap: the same submission through [[Execution.start]] under
  *     the global `IORuntime`, with a between-step hook that timestamps each
  *     yield point. The maximum gap is the observed time to yield, runner
  *     bookkeeping, progress publication and `cede` included.
  *   - Cancellation latency: a fresh run is started per trial; a fiber on
  *     another worker waits for the hook to announce step `n` and cancels at
  *     once, while that step is in flight, then awaits the outcome. The
  *     latency is from `cancel` to the settled outcome. One trial targets
  *     the slowest step found above (the adversarial case); the others are
  *     spread over the run.
  *
  * Runs longer than `runnerCap` steps are profiled purely and cancelled only
  * within their first `runnerCap` steps; the report says so.
  */
private[fs2] object ExecutionResponsivenessHarness:
  val schemaVersion: String = "eyes4s-execution-responsiveness-v1"
  val budgetNanos: Long     = 100L * 1000L * 1000L
  val runnerCap: Long       = 2_000_000L
  val spreadTrials: Int     = 6

  private def get[E, A](either: Either[E, A]): A =
    either.fold(
      e => throw new IllegalArgumentException(s"fixture construction failed: $e"),
      identity
    )

  // ---------------------------------------------------------------------------
  // Quanta
  // ---------------------------------------------------------------------------

  val smallest: WorkQuanta =
    WorkQuanta(get(PairQuantum.of(1)), get(ComparisonQuantum.of(1)), get(SampleQuantum.of(1)))

  private def quantaNamed(name: String): WorkQuanta = name match
    case "smallest" => smallest
    case _          => WorkQuanta.default

  // ---------------------------------------------------------------------------
  // Stage kinds
  // ---------------------------------------------------------------------------

  def studyKind(stage: StudyStage): String = stage match
    case StudyStage.Estimating(_, _) => "estimating"
    case StudyStage.Comparing(_, _)  => "comparing"
    case StudyStage.Reducing(_, _)   => "reducing"
    case StudyStage.Contrasting(_)   => "contrasting"

  def recordingKind(stage: RecordingStage): String = stage match
    case RecordingStage.Synchronizing    => "synchronizing"
    case RecordingStage.Warping          => "warping"
    case RecordingStage.Interpolating(_) => "interpolating"
    case RecordingStage.Detecting(_)     => "detecting"
    case RecordingStage.Assigning        => "assigning"

  def temporalKind(stage: TemporalStage): String = stage match
    case TemporalStage.Preparing(_, _, _)    => "preparing"
    case TemporalStage.Studying(_, _, inner) => studyKind(inner)

  // ---------------------------------------------------------------------------
  // Fixtures: the designated study and temporal fixture, the recording fixture
  // ---------------------------------------------------------------------------

  private def key(label: String): StudyKey =
    val pieces = label.split("/")
    StudyKey(pieces(0), pieces(1), pieces(2))
  private def oracleClock(k: StudyKey): ClockId =
    ClockId(s"fixation-trial:${KeyDigest[StudyKey].digest(k).render}")
  private def oracleInterval(k: StudyKey, start: Long, end: Long) =
    get(Interval.of(oracleClock(k), Instant.micros(start), Instant.micros(end)))
  private val oracleFrame                             = get(Frame.screen("temporal", 2, 2))
  private val oracleGrid                              = get(Grid.over(oracleFrame, 2, 2))
  private val oracleScales: Vector[StudyEstimate[Px]] =
    Vector(
      StudyEstimate.Binned(),
      StudyEstimate.Gaussian(get(Sigma.px(1)), EdgePolicy.Truncate)
    )

  /** The R-pinned fixation fixture (rational cosine, decimal Gaussian), all three phases. */
  private def oracleTrials(phases: Set[String]): StudyInput[StudyKey, Px] =
    val trials = TemporalFixtures.csv.linesIterator
      .drop(1)
      .map(_.split(",").toVector)
      .toVector
      .filter(row => phases.contains(row(2)))
      .groupBy(row => StudyKey(row(0), row(1), row(2)))
      .toVector
      .sortBy(_._1)
      .map { case (k, rows) =>
        val fixes = rows
          .sortBy(_(3).toInt)
          .map(row =>
            get(
              Event.Fixation.withoutDispersion(
                oracleInterval(k, row(6).toLong, row(6).toLong + row(7).toLong),
                Pt[Px](row(4).toDouble, row(5).toDouble),
                row(8).toInt
              )
            )
          )
        Trial(k, (), get(Scanpath.of(oracleFrame, oracleClock(k), IArray.from(fixes))))
      }
    StudyInput(Trials(trials))

  def studyFixture(quanta: WorkQuanta): ResponsivenessProbe =
    val input    = oracleTrials(Set("encode", "recall"))
    val prepared = get(
      StudyPlan
        .cosine(
          input.reference,
          oracleGrid,
          "recall",
          "encode",
          Weight.Duration,
          oracleScales,
          FailurePolicy.RequireAll
        )
        .flatMap(_.prepare(input))
    )
    ResponsivenessProbe(StudyExecution.submission(prepared, quanta = quanta), studyKind)

  def temporalFixture(quanta: WorkQuanta): ResponsivenessProbe =
    val study  = oracleTrials(Set("encode", "recall", "retest"))
    val epochs = TemporalFixtures.coverage.toVector.map { case (name, spans) =>
      val k = key(name)
      k -> TrialEpoch(
        Instant.micros(0),
        get(
          ObservedCoverage.of(
            oracleClock(k),
            spans.map { case (a, b) => oracleInterval(k, a, b) }
          )
        )
      )
    }
    val input = get(TemporalStudyInput.of(study, epochs))
    val base  = get(
      StudyPlan.cosine(
        study.reference,
        oracleGrid,
        "recall",
        "encode",
        Weight.Duration,
        oracleScales,
        FailurePolicy.RequireAll
      )
    )
    val plan = get(
      TemporalStudyPlan.of(
        base,
        input.reference,
        TemporalFixtures.windows.map { case (name, a, b) =>
          get(StudyWindow.of(name, get(Window.of(Span.micros(a), Span.micros(b)))))
        },
        TemporalFixtures.repetitions.map { case (name, f, r) =>
          get(RepetitionContrast.withinParticipant(name, f, r))
        },
        FixationBoundary.ClipDuration
      )
    )
    ResponsivenessProbe(
      TemporalExecution.submission(get(plan.prepare(input)), quanta = quanta),
      temporalKind
    )

  private val display       = get(Frame.screen("responsiveness-display", 1000, 1000))
  private val trackerClock  = ClockId("responsiveness-tracker")
  private val analysisClock = ClockId("responsiveness-analysis")
  private val viewing       = get(Viewing.millimetres(600.0, 500.0, 500.0))
  private val ivtId         = get(DefinitionId.of("eyes4s.ivt", 1))
  private val area          = get(
    RecordingArea.of("centre", "Central area", get(Bounds.of[Px](300.0, 300.0, 700.0, 700.0)))
  )

  /** The recording suite's fixture: 40 samples at 100 Hz, a fixation, a
    * two-sample saccade, a fixation with two lost samples.
    */
  private def fixtureGaze(i: Int): Gaze[Px] =
    if i == 25 || i == 26 then Gaze.Lost()
    else if i < 15 then Gaze.Tracked(Pt[Px](400.0, 500.0), None)
    else if i == 15 then Gaze.Tracked(Pt[Px](480.0, 500.0), None)
    else if i == 16 then Gaze.Tracked(Pt[Px](560.0, 500.0), None)
    else Gaze.Tracked(Pt[Px](600.0, 500.0), None)

  /** A synthetic recording at `rateHz`: 250 ms fixations cycling over a 5x5
    * lattice joined by 20 ms linear saccades, with a 40 ms signal-loss burst
    * inside every seventh fixation (longer than the 30 ms interpolation gap).
    */
  private def syntheticGaze(i: Int, rateHz: Int): Gaze[Px] =
    val fixation   = rateHz / 4
    val saccade    = math.max(2, rateHz / 50)
    val period     = fixation + saccade
    val cycle      = i / period
    val offset     = i % period
    def at(c: Int) = Pt[Px](200.0 + 150.0 * (c % 5), 200.0 + 150.0 * ((c / 5) % 5))
    if offset < fixation then
      val lossFrom = fixation / 3
      if cycle % 7 == 6 && offset >= lossFrom && offset < lossFrom + rateHz / 25 then
        Gaze.Lost()
      else Gaze.Tracked(at(cycle), None)
    else
      val from = at(cycle)
      val to   = at(cycle + 1)
      val t    = (offset - fixation + 1).toDouble / (saccade + 1).toDouble
      Gaze.Tracked(Pt[Px](from.x + t * (to.x - from.x), from.y + t * (to.y - from.y)), None)

  private def recordingOf(size: Int, rateHz: Int, gaze: Int => Gaze[Px]): Recording[Px] =
    val stepMicros = 1_000_000L / rateHz
    get(
      Recording.of(
        display,
        trackerClock,
        Rate.Fixed(get(Hz(rateHz.toDouble))),
        Eye.Left,
        None,
        IArray.from(
          (0 until size).map(i => Sample(Instant.micros(i.toLong * stepMicros), gaze(i)))
        )
      )
    )

  private def recordingProbe(
      recording: Recording[Px],
      quanta: WorkQuanta
  ): ResponsivenessProbe =
    val last = recording.samples(recording.size - 1).t
    val plan = get(
      RecordingPlan.of(
        ArtifactRef.of(recording.contentHash),
        RecordingRef("responsiveness-recording"),
        display,
        trackerClock,
        analysisClock,
        FrameId("responsiveness-angular"),
        Some(viewing),
        SyncFitMode.OffsetOnly,
        Vector(
          get(SyncMark.of("start", Instant.micros(0), Instant.micros(0))),
          get(SyncMark.of("end", last, last))
        ),
        None,
        get(InterpolationGap.of(Span.micros(30000))),
        Vector(area),
        RecordingMethod.ivt(ivtId),
        IvtParameters(
          get(IvtThreshold.of(get(Velocity.perSecond[Deg](30)))),
          get(MinimumEventDuration.of(Span.micros(20000)))
        )
      )
    )
    ResponsivenessProbe(RecordingExecution.submission(plan, recording, quanta), recordingKind)

  def recordingFixture(quanta: WorkQuanta): ResponsivenessProbe =
    recordingProbe(recordingOf(40, 100, fixtureGaze), quanta)

  def recordingSynthetic(seconds: Int, rateHz: Int, quanta: WorkQuanta): ResponsivenessProbe =
    recordingProbe(recordingOf(seconds * rateHz, rateHz, syntheticGaze(_, rateHz)), quanta)

  // ---------------------------------------------------------------------------
  // Scaled-up synthetic study: participants x stimuli x {encode, recall}
  // ---------------------------------------------------------------------------

  /** `participants * stimuli * 2` trials of ten fixations each at seeded
    * uniform positions on an `n` by `n` pixel frame with an `n` by `n` grid
    * (one-pixel cells), estimated binned or with a Gaussian of `sigma` cells.
    */
  def studySynthetic(
      participants: Int,
      stimuli: Int,
      n: Int,
      sigma: Option[Double],
      quanta: WorkQuanta
  ): ResponsivenessProbe =
    val frame  = get(Frame.screen(s"synthetic-$n", n, n))
    val grid   = get(Grid.over(frame, n, n))
    val random = new scala.util.Random(20260917L)
    val trials = for
      p     <- (1 to participants).toVector
      s     <- (1 to stimuli).toVector
      phase <- Vector("encode", "recall")
    yield
      val k     = StudyKey(s"p$p", s"s$s", phase)
      val clock = ClockId(s"synthetic/p$p/s$s/$phase")
      var onset = 0L
      val fixes = (0 until 10).map { _ =>
        val duration = 150_000L + random.nextInt(250_000).toLong
        val fix      = get(
          Event.Fixation.withoutDispersion(
            get(Interval.of(clock, Instant.micros(onset), Instant.micros(onset + duration))),
            Pt[Px](random.nextDouble() * n, random.nextDouble() * n),
            (duration / 1000L).toInt
          )
        )
        onset += duration + 20_000L
        fix
      }
      Trial(k, (), get(Scanpath.of(frame, clock, IArray.from(fixes))))
    val input    = StudyInput(Trials(trials))
    val estimate = sigma.fold[StudyEstimate[Px]](StudyEstimate.Binned())(s =>
      StudyEstimate.Gaussian(get(Sigma.px(s)), EdgePolicy.Truncate)
    )
    val prepared = get(
      StudyPlan
        .cosine(
          input.reference,
          grid,
          "recall",
          "encode",
          Weight.Duration,
          Vector(estimate),
          FailurePolicy.RequireAll
        )
        .flatMap(_.prepare(input))
    )
    ResponsivenessProbe(StudyExecution.submission(prepared, quanta = quanta), studyKind)

  // ---------------------------------------------------------------------------
  // Profiles
  // ---------------------------------------------------------------------------

  private def both(
      id: String,
      route: String,
      envelope: String,
      build: WorkQuanta => ResponsivenessProbe
  ): Vector[ResponsivenessWorkload] =
    Vector("default", "smallest").map(q =>
      ResponsivenessWorkload(id, route, envelope, q, () => build(quantaNamed(q)))
    )

  private def synthetic(
      id: String,
      participants: Int,
      stimuli: Int,
      n: Int,
      sigma: Option[Double],
      quanta: String
  ): ResponsivenessWorkload =
    val estimator = sigma.fold("binned")(s => s"gaussian sigma ${s.toInt} cells, truncate")
    ResponsivenessWorkload(
      id,
      "study",
      s"${participants * stimuli * 2} trials x 10 fixations, ${n}x$n grid, $estimator",
      quanta,
      () => studySynthetic(participants, stimuli, n, sigma, quantaNamed(quanta))
    )

  val smokeWorkloads: Vector[ResponsivenessWorkload] =
    both(
      "study-fixture",
      "study",
      "R-pinned fixture: 24 trials, 2x2 grid, binned + gaussian sigma 1",
      studyFixture
    ) ++
      both(
        "temporal-fixture",
        "temporal",
        "R-pinned fixture: 36 trials, 4 windows x 2 repetitions, 2x2 grid, 2 scales",
        temporalFixture
      ) ++
      both("recording-fixture", "recording", "40 samples at 100 Hz, IVT", recordingFixture) ++
      both(
        "recording-60s",
        "recording",
        "60000 samples at 1000 Hz, IVT, one AOI",
        recordingSynthetic(60, 1000, _)
      ) ++
      Vector(
        synthetic("study-100x256-binned", 5, 10, 256, None, "default"),
        synthetic("study-100x256-sigma8", 5, 10, 256, Some(8.0), "default"),
        synthetic("study-100x256-sigma32", 5, 10, 256, Some(32.0), "default")
      )

  val fullWorkloads: Vector[ResponsivenessWorkload] =
    smokeWorkloads ++ Vector(
      synthetic("study-100x256-binned", 5, 10, 256, None, "smallest"),
      synthetic("study-100x256-sigma8", 5, 10, 256, Some(8.0), "smallest"),
      synthetic("study-100x256-sigma32", 5, 10, 256, Some(32.0), "smallest"),
      synthetic("study-4x512-sigma32", 1, 2, 512, Some(32.0), "default"),
      synthetic("study-4x1024-sigma32", 1, 2, 1024, Some(32.0), "default")
    ) ++ both(
      "recording-600s",
      "recording",
      "600000 samples at 1000 Hz, IVT, one AOI",
      recordingSynthetic(600, 1000, _)
    )

  // ---------------------------------------------------------------------------
  // Measurement
  // ---------------------------------------------------------------------------

  private def begin(probe: ResponsivenessProbe): probe.C =
    probe.submission
      .begin()
      .fold(e => throw new IllegalStateException(s"begin failed: $e"), identity)

  /** Drive the cursor to completion, timing every `advance`. */
  def pure(probe: ResponsivenessProbe): PureProfile =
    val stepwise = probe.submission.stepwise
    val quanta   = probe.submission.quanta
    val overall  = new DurationStats
    val byKind   = mutable.LinkedHashMap.empty[String, DurationStats]
    var cursor   = begin(probe)
    var steps    = 0L
    var units    = 0L
    var done     = false
    val started  = System.nanoTime()
    while !done do
      val before = stepwise.stage(cursor)
      val t0     = System.nanoTime()
      val step   = stepwise.advance(cursor, quanta)
      val t1     = System.nanoTime()
      steps += 1
      val (stage, stepUnits) = step match
        case Left(error) => throw new IllegalStateException(s"step $steps failed: $error")
        case Right(WorkStep.More(stage, u, next)) =>
          cursor = next
          (stage, u)
        case Right(WorkStep.Done(u, _)) =>
          done = true
          (before, u)
      units += stepUnits
      overall.add(t1 - t0, steps)
      byKind.getOrElseUpdate(probe.kind(stage), new DurationStats).add(t1 - t0, steps)
    val wall        = System.nanoTime() - started
    val slowestKind = byKind.find(_._2.maxStep == overall.maxStep).map(_._1).getOrElse("?")
    PureProfile(steps, units, wall, overall, byKind, slowestKind)

  /** Timestamps each yield point of one run through the shipped runner. */
  private final class GapClock:
    val gaps          = new DurationStats
    private var last  = 0L
    private var index = 0L
    def tick(): Unit  =
      val now = System.nanoTime()
      if last != 0L then gaps.add(now - last, index)
      last = now
      index += 1

  def runner(
      probe: ResponsivenessProbe,
      expectedSteps: Long,
      violations: mutable.Builder[String, Vector[String]]
  ): RunnerProfile =
    val clock   = new GapClock
    val started = System.nanoTime()
    val outcome = Execution[IO]
      .start(probe.submission, IO.cede >> IO(clock.tick()))
      .use(_.outcome)
      .unsafeRunSync()
    clock.tick()
    val wall = System.nanoTime() - started
    outcome match
      case RunOutcome.Completed(_, last, _) if last.step == expectedSteps => ()
      case other                                                          =>
        violations += s"full run settled ${summary(other)}, expected Completed at step $expectedSteps"
    RunnerProfile(wall, clock.gaps)

  private def summary(outcome: RunOutcome[?, ?, ?, ?, ?]): String = outcome match
    case RunOutcome.Completed(_, last, _) => s"Completed(step ${last.step})"
    case RunOutcome.Cancelled(_, last)    => s"Cancelled(step ${last.fold(0L)(_.step)})"
    case RunOutcome.Failed(_, error, _)   => s"Failed($error)"

  /** Cancel while step `target` is in flight and time the settlement. */
  def cancelAt(
      probe: ResponsivenessProbe,
      target: Long,
      steps: Long,
      adversarial: Boolean,
      violations: mutable.Builder[String, Vector[String]]
  ): CancelTrial =
    val counter = new AtomicLong(0L)
    val program = Deferred[IO, Unit].flatMap { reached =>
      val between = IO.cede >> IO(counter.incrementAndGet()).flatMap(n =>
        if n == target then reached.complete(()).void else IO.unit
      )
      Execution[IO].start(probe.submission, between).use { run =>
        reached.get >> IO(System.nanoTime()).flatMap(t0 =>
          run.cancel >> run.outcome.flatMap(o => IO(System.nanoTime() - t0).map(_ -> o))
        )
      }
    }
    val (latency, outcome) = program.unsafeRunSync()
    // The canceller races the run fiber, so a run of microsecond steps may
    // commit a few more steps before the cancel lands; that cancellation is
    // observed only between steps is proved deterministically in
    // ExecutionConformanceSuite. Here the outcome must only be lawful: a
    // Cancelled run holds no result and stopped short of the terminal step,
    // and a Completed one finished every step (the first commit won).
    val lawful = outcome match
      case RunOutcome.Cancelled(_, last) =>
        val at = last.fold(0L)(_.step)
        at >= target - 1 && at < steps
      case RunOutcome.Completed(_, last, _) => last.step == steps
      case RunOutcome.Failed(_, _, _)       => false
    if !lawful then
      violations += s"cancelling at step $target of $steps settled ${summary(outcome)}"
    CancelTrial(target, adversarial, latency, summary(outcome))

  def measure(workload: ResponsivenessWorkload): ResponsivenessResult =
    val violations = Vector.newBuilder[String]
    // Warm the JIT on the same paths at the default quanta, then measure.
    val warm = workload.probe()
    val _    = pure(
      ResponsivenessProbe(
        new Submission(
          warm.submission.id,
          WorkQuanta.default,
          warm.submission.begin,
          warm.submission.segment,
          warm.submission.total
        )(using warm.submission.stepwise),
        warm.kind
      )
    )
    val probe         = workload.probe()
    val profile       = pure(probe)
    val reach         = math.min(profile.steps, runnerCap)
    val runnerProfile =
      Option.when(profile.steps <= runnerCap)(runner(probe, profile.steps, violations))
    val spread =
      (1 to spreadTrials).map(i => math.max(1L, i.toLong * reach / spreadTrials)).toVector
    val slowest = profile.overall.maxStep
    val trials  =
      Option
        .when(slowest <= reach)(cancelAt(probe, slowest, profile.steps, true, violations))
        .toVector ++
        spread.distinct
          .filterNot(_ == slowest)
          .map(cancelAt(probe, _, profile.steps, false, violations))
    ResponsivenessResult(workload, profile, runnerProfile, trials, reach, violations.result())

  // ---------------------------------------------------------------------------
  // Rendering
  // ---------------------------------------------------------------------------

  private def ms(nanos: Long): String = f"${nanos.toDouble / 1e6}%.3f"

  def render(
      environment: Vector[(String, String)],
      results: Vector[ResponsivenessResult]
  ): String =
    val out = new StringBuilder
    out ++= s"# $schemaVersion\n"
    environment.foreach((k, v) => out ++= s"# $k: $v\n")
    out ++= "\n"
    out ++= "| workload | route | quanta | envelope | steps | max step ms (stage) | p95 step ms | runner max gap ms | cancel adversarial ms | cancel max ms | cancel median ms | wall s | 100 ms |\n"
    out ++= "|---|---|---|---|---:|---|---:|---:|---:|---:|---:|---:|---|\n"
    results.foreach { r =>
      val w = r.workload
      out ++= Vector(
        w.id,
        w.route,
        w.quanta,
        w.envelope,
        r.pure.steps.toString,
        s"${ms(r.pure.overall.max)} (${r.pure.slowestKind})",
        ms(r.pure.overall.p95),
        r.runner.fold(s"not run (> $runnerCap steps)")(p => ms(p.gaps.max)),
        r.adversarial.fold("n/a")(ms),
        ms(r.cancelMax) + (if r.reach < r.pure.steps then s" (first ${r.reach} steps)" else ""),
        ms(r.cancelMedian),
        f"${r.runner.fold(r.pure.wallNanos)(_.wallNanos).toDouble / 1e9}%.2f",
        if r.meets(budgetNanos) then "meets" else "MISSES"
      ).mkString("| ", " | ", " |\n")
    }
    out ++= "\nPer stage kind (pure step durations):\n\n"
    out ++= "| workload | quanta | stage | steps | max ms | p95 ms | mean ms |\n"
    out ++= "|---|---|---|---:|---:|---:|---:|\n"
    results.foreach { r =>
      r.pure.byKind.foreach { (kind, s) =>
        out ++= Vector(
          r.workload.id,
          r.workload.quanta,
          kind,
          s.count.toString,
          ms(s.max),
          ms(s.p95),
          ms(if s.count == 0 then 0L else s.sum / s.count)
        ).mkString("| ", " | ", " |\n")
      }
    }
    val violations =
      results.flatMap(r => r.violations.map(v => s"${r.workload.id}/${r.workload.quanta}: $v"))
    out ++= s"\nContract violations: ${if violations.isEmpty then "none" else ""}\n"
    violations.foreach(v => out ++= s"- $v\n")
    out.result()

end ExecutionResponsivenessHarness

/** `sbt "fs2ModuleJVM/Test/runMain eyes4s.fs2.ExecutionResponsivenessMain --profile smoke"`.
  *
  * Options: `--profile smoke|full`, `--hardware <label>`, `--source-revision
  * <sha>`, `--output <path>`. Exits 1 only on an outcome-contract violation;
  * timing never fails the run.
  */
object ExecutionResponsivenessMain:
  def main(arguments: Array[String]): Unit =
    val args                                = arguments.toVector
    def value(flag: String): Option[String] =
      args.sliding(2).collectFirst { case Vector(`flag`, selected) => selected }
    val profile   = value("--profile").getOrElse("smoke")
    val workloads = profile match
      case "smoke" => ExecutionResponsivenessHarness.smokeWorkloads
      case "full"  => ExecutionResponsivenessHarness.fullWorkloads
      case other   =>
        System.err.println(s"unsupported --profile value='$other'; expected smoke or full")
        sys.exit(2)
    val runtime     = ManagementFactory.getRuntimeMXBean
    val environment = Vector(
      "measured_at_utc"  -> java.time.Instant.now().toString,
      "profile"          -> profile,
      "source_revision"  -> value("--source-revision").getOrElse("unrecorded"),
      "hardware"         -> value("--hardware").getOrElse("unrecorded"),
      "operating_system" ->
        s"${System.getProperty("os.name")} ${System.getProperty("os.version")} ${System.getProperty("os.arch")}",
      "runtime" -> s"${System.getProperty("java.vm.name")} ${System.getProperty("java.runtime.version")}",
      "jvm_arguments"        -> runtime.getInputArguments.asScala.mkString(" "),
      "available_processors" -> Runtime.getRuntime.availableProcessors().toString,
      "max_heap_bytes"       -> Runtime.getRuntime.maxMemory().toString,
      "io_runtime"           -> "cats.effect.unsafe.implicits.global",
      "budget_ms"            -> "100"
    )
    // Warm the runner and the IORuntime's workers before anything is recorded.
    (1 to 3).foreach(_ =>
      ExecutionResponsivenessHarness.measure(ExecutionResponsivenessHarness.smokeWorkloads.head)
    )
    val results = workloads.map { workload =>
      System.err.println(s"measuring ${workload.id} at ${workload.quanta} quanta")
      ExecutionResponsivenessHarness.measure(workload)
    }
    val rendered = ExecutionResponsivenessHarness.render(environment, results)
    value("--output") match
      case Some(path) =>
        Files.writeString(
          Path.of(path),
          rendered,
          StandardCharsets.UTF_8,
          StandardOpenOption.CREATE,
          StandardOpenOption.TRUNCATE_EXISTING,
          StandardOpenOption.WRITE
        ): Unit
        print(rendered)
      case None => print(rendered)
    if results.exists(_.violations.nonEmpty) then sys.exit(1)
