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

package eyes4s.laws

import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Deg, Px}
import eyes4s.plan.*
import eyes4s.surface.EdgePolicy

import eyes4s.examples.MatchedControlFixtures

import org.scalacheck.{Gen, Prop, Test}
import org.scalacheck.Prop.propBoolean

import scala.concurrent.duration.*

/** The published execution laws over the three shipped families, and the
  * mutants that show each law is load-bearing.
  *
  * Every mutant is a complete `Stepwise` instance or `Family` evidence for
  * the study cursor with a single injected fault, written in this file. The
  * shipped instance must pass every law; each mutant must be observed to fail
  * the law that names it. The suite checks the receipts below mechanically:
  * for each mutant it runs every law alone and asserts that exactly the
  * named laws are falsified (`Test.Failed`; an exception or an exhausted
  * generator does not count as a kill).
  *
  * Fixtures: five study, two recording and two temporal families that
  * complete; two temporal families that fail lawfully (a comparison budget
  * refused after four preparation steps, and on the very first advance),
  * whose reference is the literal expected error; and the R-pinned
  * matched/control fixture checked against an independent oracle.
  *
  * ==Mutation execution receipts==
  *
  * Observed on both JVM and Scala.js (`lawsJVM/test`, `lawsJS/test`). Law
  * names are abbreviated: determinism (same cursor, same quanta, same steps
  * and end), terminal (one terminal step within budget), completion (any
  * quanta, and the finest cut, reach the reference run), cut-invariance (any
  * sequence of quanta reaches the reference run), contiguity (units
  * non-negative, one block per segment), position-independent (every step of
  * a segment states the same total), totals (Exact met, AtMost never
  * exceeded), accounting (per-segment work independent of the cuts, the
  * finest cut always included), independent (every run's end satisfies an
  * oracle computed without the cursor).
  *
  * {{{
  * | mutant             | injected fault                                           | killed by                                    |
  * |--------------------|----------------------------------------------------------|----------------------------------------------|
  * | units-dropped      | every step reports 0 units                               | totals                                       |
  * | trial-skipped      | the first trial's estimation step is hidden from the run | totals                                       |
  * | units-drift        | units grow with a call counter across runs               | determinism, totals, accounting              |
  * | premature-done     | the first step returns Done with a decoy study's result  | completion, cut-invariance, totals           |
  * | finest-swapped     | at pair quantum 1 the result is a decoy study's          | completion, cut-invariance                   |
  * | looping            | Done is reported as More back to the same cursor         | terminal, completion, cut-invariance, totals, accounting |
  * | cut-charged        | one extra unit per comparing step at one specific cut    | accounting                                   |
  * | segment-revisited  | odd trials' estimation counts toward the contrast        | contiguity, totals                           |
  * | exact-overclaimed  | estimation claims Exact(trials + 1)                      | totals                                       |
  * | atmost-understated | comparison claims AtMost(0)                              | totals                                       |
  * | wavering-total     | estimation states Unknown, then Exact, inside one block  | position-independent                         |
  * | self-vouching      | every Done carries a decoy result, and the reference run | independent                                  |
  * |                    | is this same instance (on the R-pinned fixture)          |                                              |
  * }}}
  *
  * Laws a mutant also happens to fail are listed after the one it targets;
  * every mutant fails at least its target and the shipped instance fails none.
  */
class ExecutionLawsSuite extends munit.DisciplineSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(e => fail(s"$e"), identity)

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(40).withInitialSeed(0x45584543L)

  // The mutant matrix runs every mutant against every law: ~11 s locally, ~35 s on a
  // hosted 4-vCPU runner, past munit's 30 s default. The work is fixed by the seeds.
  override val munitTimeout: Duration = 3.minutes

  private val killParameters =
    Test.Parameters.default.withMinSuccessfulTests(40).withInitialSeed(0x4b494c4cL)

  private val quanta = ExecutionLaws.quanta(
    pairs = Seq(1, 2, 3, 7),
    comparison = Seq(1, 2, 3, 5),
    samples = Seq(1, 4, 7)
  )

  // -------------------------------------------------------------------------
  // Result equality: field by field, as the fs2 suites assert it
  // -------------------------------------------------------------------------

  private def sameAnalysis[K, S](m: Analysis[K, S], n: Analysis[K, S]): Boolean =
    m.entries == n.entries && m.diagnostics == n.diagnostics &&
      m.provenance == n.provenance && m.evaluation.name == n.evaluation.name &&
      m.evaluation.scale == n.evaluation.scale &&
      m.evaluation.specification.map(s => (s.method, s.revision, s.parameters, s.components)) ==
      n.evaluation.specification.map(s => (s.method, s.revision, s.parameters, s.components)) &&
      (m.source.rows: Any) == (n.source.rows: Any) &&
      (m.source.diagnostics: Any) == (n.source.diagnostics: Any) &&
      m.source.provenance == n.source.provenance

  private def sameEvaluation(m: EvaluationInfo, n: EvaluationInfo): Boolean =
    m.name == n.name && m.scale == n.scale &&
      m.specification.map(s => (s.method, s.revision, s.parameters, s.components)) ==
      n.specification.map(s => (s.method, s.revision, s.parameters, s.components))

  /** A typed pairwise source, as S4's `StudyScaleResult.analyses` carries it. */
  private def sameSource[K, E, S](
      m: DirectedPairwiseAnalysis[K, K, E, S],
      n: DirectedPairwiseAnalysis[K, K, E, S]
  ): Boolean =
    m.rows == n.rows && m.diagnostics == n.diagnostics && m.provenance == n.provenance &&
      sameEvaluation(m.evaluation, n.evaluation)

  private def sameAnalyses[K, S](m: StudyAnalyses[K, S], n: StudyAnalyses[K, S]): Boolean =
    sameSource(m.matchedSource, n.matchedSource) && sameAnalysis(m.matched, n.matched) &&
      sameSource(m.controlSource, n.controlSource) && sameAnalysis(m.control, n.control)

  private def sameStudy[K, U <: Unit2D, S, D](
      o: StudyResult[K, U, S, D],
      e: StudyResult[K, U, S, D]
  ): Boolean =
    o.input == e.input && o.description == e.description && o.scales.size == e.scales.size &&
      o.scales.zip(e.scales).forall { (a, b) =>
        a.estimate == b.estimate && a.excludedPhases == b.excludedPhases &&
        sameAnalyses(a.analyses, b.analyses) &&
        a.estimation.size == b.estimation.size &&
        a.estimation.zip(b.estimation).forall {
          case ((k, Right(x)), (l, Right(y))) =>
            k == l && x.values.toVector == y.values.toVector && x.provenance == y.provenance
          case ((k, Left(x)), (l, Left(y))) => k == l && x == y
          case _                            => false
        } &&
        ((a.contrast, b.contrast) match
          case (Right(x), Right(y)) =>
            x.rows.map(r => (r.key, r.difference, r.matched, r.control)) ==
              y.rows.map(r => (r.key, r.difference, r.matched, r.control)) &&
              sameAnalysis(x.matched, y.matched) && sameAnalysis(x.control, y.control)
          case (Left(x), Left(y)) => x == y
          case _                  => false)
      }

  private def sameRecording[P](o: RecordingAnalysis[P], e: RecordingAnalysis[P]): Boolean =
    o.description == e.description &&
      o.angular.samples.toVector == e.angular.samples.toVector &&
      o.prepared.samples.toVector == e.prepared.samples.toVector &&
      o.detection.identity == e.detection.identity &&
      o.detection.labels.toVector == e.detection.labels.toVector &&
      o.detection.eventSeries.events == e.detection.eventSeries.events &&
      o.detection.eventSeries.support == e.detection.eventSeries.support &&
      o.detection.report == e.detection.report &&
      o.detection.provenance == e.detection.provenance &&
      o.assignment.toVector == e.assignment.toVector &&
      o.assignment.report == e.assignment.report

  private def ledger[K, U <: Unit2D, P, S, D](cell: TemporalCell[K, U, P, S, D]) =
    cell.occupancy.map { case (key, value) =>
      key -> value.map(o =>
        (o.interval, o.boundary, o.observedMicros, o.missingMicros, o.fixationTimes)
      )
    }

  private def sameTemporal[K, U <: Unit2D, P, S, D](
      o: TemporalStudyResult[K, U, P, S, D],
      e: TemporalStudyResult[K, U, P, S, D]
  ): Boolean =
    o.description == e.description && o.cells.size == e.cells.size &&
      o.cells.zip(e.cells).forall { (a, b) =>
        a.repetition.name == b.repetition.name && a.window.name == b.window.name &&
        ledger(a) == ledger(b) && sameStudy(a.result, b.result)
      }

  // -------------------------------------------------------------------------
  // Study family
  // -------------------------------------------------------------------------

  private val frame = get(Frame.screen("execution-laws", 2, 2))
  private val grid  = get(Grid.over(frame, 2, 2))
  private val a     = StudyKey("p1", "a", "recall")
  private val b     = StudyKey("p1", "b", "recall")
  private val ar    = StudyKey("p1", "a", "encode")
  private val br    = StudyKey("p1", "b", "encode")
  private val lone  = StudyKey("p2", "c", "recall")

  private def clock(key: StudyKey): ClockId =
    ClockId(key.participant + "/" + key.stimulus + "/" + key.phase)

  /** Fixations of 1000 microseconds at onsets 0, 1000, 2000. */
  private def trial(key: StudyKey, points: (Double, Double)*) =
    val fixes = points.zipWithIndex.map { case ((x, y), i) =>
      get(
        Event.Fixation.withoutDispersion(
          get(
            Interval
              .of(clock(key), Instant.micros(i * 1000L), Instant.micros(i * 1000L + 1000L))
          ),
          Pt[Px](x, y),
          1
        )
      )
    }
    Trial(key, (), get(Scanpath.of(frame, clock(key), IArray.from(fixes))))

  private val rows = Vector(
    trial(a, (0.5, 0.5), (1.5, 0.5), (0.5, 1.5)),
    trial(b, (0.5, 1.5), (1.5, 1.5)),
    trial(ar, (0.5, 0.5), (0.5, 0.5), (1.5, 1.5)),
    trial(br, (1.5, 0.5), (0.5, 1.5)),
    trial(lone, (0.5, 0.5), (0.5, 0.5))
  )
  private val input = StudyInput(Trials(rows))
  private val full  = StudyInput(Trials(Vector(rows(0), rows(2))))
  private val alone = StudyInput(Trials(Vector(rows(0))))
  private val decoy = StudyInput(Trials(rows.filterNot(_.key == b)))

  private val cosine      = StudyMethod.cosine[Px](DefinitionId.cosine)
  private val synchronous = new StudyMethod[Unit, Px, Similarity, SignedDifference](
    DefinitionId.cosine,
    "synchronous cosine",
    _ => Vector.empty,
    _ => Distribution.cosine[Px]: Compare[Mass[Px], Mass[Px], Similarity],
    Some(ComparisonMethods.cosine.descriptor)
  )
  private val gaussian = StudyEstimate.Gaussian(get(Sigma.px(0.5)), EdgePolicy.Truncate)

  private def studyPlan(
      source: StudyInput[StudyKey, Px],
      scales: Vector[StudyEstimate[Px]] = Vector(StudyEstimate.Binned()),
      method: StudyMethod[Unit, Px, Similarity, SignedDifference] = cosine
  ) =
    get(
      StudyPlan.of(
        source.reference,
        StudyKey.layout(DefinitionId.studyLayout),
        grid,
        "recall",
        "encode",
        Weight.Duration,
        scales,
        FailurePolicy.RequireAll,
        method,
        ()
      )
    )

  private type Cursor = StudyCursor[StudyKey, Px, Similarity, SignedDifference]
  private type Result = StudyResult[StudyKey, Px, Similarity, SignedDifference]
  private type Study  = Stepwise[Cursor, StudyStage, PlanError, Result]

  private val prepared = Vector(
    "binned"     -> get(studyPlan(input).prepare(input)),
    "two scales" -> get(
      studyPlan(input, Vector(StudyEstimate.Binned(), gaussian)).prepare(input)
    ),
    "matched"     -> get(studyPlan(full).prepare(full)),
    "unmatched"   -> get(studyPlan(alone).prepare(alone)),
    "synchronous" -> get(studyPlan(input, method = synchronous).prepare(input))
  )
  private val decoyResult: Result = get(studyPlan(decoy).prepare(decoy).flatMap(_.run))

  private type Prepared = PreparedStudy[StudyKey, Px, Unit, Similarity, SignedDifference]

  /** The shipped family: `StudySegment.of` counts stages and
    * `StudySegment.total` states what the fs2 wrapper reports.
    */
  private def studyFamily(
      work: Prepared,
      stepwise: Study = Stepwise.study,
      segment: StudyStage => StudySegment = StudySegment.of,
      total: Option[(StudySegment, Cursor) => SegmentTotal] = None
  ) =
    new ExecutionLaws.Family[Cursor, StudyStage, StudySegment, PlanError, Result](
      segment,
      total.getOrElse(StudySegment.total(work, _, _)),
      _ => work.run,
      sameStudy,
      _ == _,
      stepBudget = 20_000
    )(using stepwise)

  prepared.foreach { (name, work) =>
    checkAll(
      s"study.$name",
      ExecutionLaws.conformance(studyFamily(work), Gen.const(get(work.work())), quanta)
    )
  }

  // -------------------------------------------------------------------------
  // Recording family
  // -------------------------------------------------------------------------

  private val display       = get(Frame.screen("execution-laws-display", 1000, 1000))
  private val trackerClock  = ClockId("execution-laws-tracker")
  private val analysisClock = ClockId("execution-laws-analysis")
  private val viewing       = get(Viewing.millimetres(600.0, 500.0, 500.0))

  /** 40 samples at 100 Hz: a fixation, a two-sample saccade, a fixation with two lost samples. */
  private def gaze(i: Int): Gaze[Px] =
    if i == 25 || i == 26 then Gaze.Lost()
    else if i < 15 then Gaze.Tracked(Pt[Px](400.0, 500.0), None)
    else if i == 15 then Gaze.Tracked(Pt[Px](480.0, 500.0), None)
    else if i == 16 then Gaze.Tracked(Pt[Px](560.0, 500.0), None)
    else Gaze.Tracked(Pt[Px](600.0, 500.0), None)

  private val recording = get(
    Recording.of(
      display,
      trackerClock,
      Rate.Fixed(get(Hz(100.0))),
      Eye.Left,
      None,
      IArray.from((0 until 40).map(i => Sample(Instant.millis(i.toLong * 10L), gaze(i))))
    )
  )
  private val marks = Vector(
    get(SyncMark.of("start", Instant.millis(0), Instant.millis(0))),
    get(SyncMark.of("end", Instant.millis(390), Instant.millis(390)))
  )
  private val area = get(
    RecordingArea.of("centre", "Central area", get(Bounds.of[Px](300.0, 300.0, 700.0, 700.0)))
  )
  private val ivtId = get(DefinitionId.of("eyes4s.ivt", 1))

  private def recordingPlan(minimumMicros: Long) = get(
    RecordingPlan.of(
      ArtifactRef.of(recording.contentHash),
      RecordingRef("execution-laws-recording"),
      display,
      trackerClock,
      analysisClock,
      FrameId("execution-laws-angular"),
      Some(viewing),
      SyncFitMode.OffsetOnly,
      marks,
      None,
      get(InterpolationGap.of(Span.micros(30000))),
      Vector(area),
      RecordingMethod.ivt(ivtId),
      IvtParameters(
        get(IvtThreshold.of(get(Velocity.perSecond[Deg](30)))),
        get(MinimumEventDuration.of(Span.micros(minimumMicros)))
      )
    )
  )

  private def recordingFamily(plan: RecordingPlan[IvtParameters]) = new ExecutionLaws.Family[
    RecordingCursor[IvtParameters],
    RecordingStage,
    RecordingSegment,
    RecordingPlanError,
    RecordingAnalysis[IvtParameters]
  ](
    RecordingSegment.of,
    (segment, cursor) => RecordingSegment.total(cursor.samples, segment),
    _ => plan.run(recording),
    sameRecording,
    _ == _,
    20_000
  )

  /** Two minimum durations: the second merges the short saccade away. */
  private val recordingPlans =
    Vector("20ms" -> 20000L, "60ms" -> 60000L).map((n, m) => n -> recordingPlan(m))

  recordingPlans.foreach { (name, plan) =>
    checkAll(
      s"recording.$name",
      ExecutionLaws.conformance(
        recordingFamily(plan),
        Gen.const(get(plan.work(recording))),
        quanta
      )
    )
  }

  // -------------------------------------------------------------------------
  // Temporal family
  // -------------------------------------------------------------------------

  private def interval(key: StudyKey, from: Long, until: Long) =
    get(Interval.of(clock(key), Instant.micros(from), Instant.micros(until)))

  private def epoch(key: StudyKey, anchor: Long = 0L, until: Long = 3000L) =
    key -> TrialEpoch(
      Instant.micros(anchor),
      get(ObservedCoverage.of(clock(key), Vector(interval(key, 0L, until))))
    )

  /** Every trial but `lone` has an epoch, `br` overflows, `b` is observed half. */
  private val edges = get(
    TemporalStudyInput.of(
      input,
      Vector(
        epoch(a),
        epoch(b, until = 1500L),
        epoch(ar),
        epoch(br, anchor = Long.MaxValue - 100L)
      )
    )
  )
  private val clean = get(TemporalStudyInput.of(input, rows.map(t => epoch(t.key))))

  private def window(name: String, from: Long, until: Long) =
    get(StudyWindow.of(name, get(Window.of(Span.micros(from), Span.micros(until)))))

  private def temporalPlan(source: TemporalStudyInput[StudyKey, Px]) =
    get(
      TemporalStudyPlan.of(
        get(
          StudyPlan.of(
            source.study.reference,
            StudyKey.layout(DefinitionId.studyLayout),
            grid,
            "recall",
            "encode",
            Weight.Duration,
            Vector(StudyEstimate.Binned()),
            FailurePolicy.RequireAll,
            cosine,
            ()
          )
        ),
        source.reference,
        Vector(window("early", 0, 1500), window("late", 1500, 3000)),
        Vector(
          get(RepetitionContrast.withinParticipant("recall", "recall", "encode")),
          get(RepetitionContrast.withinParticipant("reversed", "encode", "recall"))
        ),
        FixationBoundary.ClipDuration
      )
    )

  private type TemporalC = TemporalCursor[StudyKey, Px, Unit, Similarity, SignedDifference]
  private type TemporalR = TemporalStudyResult[StudyKey, Px, Unit, Similarity, SignedDifference]

  private type PreparedTemporal =
    PreparedTemporalStudy[StudyKey, Px, Unit, Similarity, SignedDifference]

  private val temporalPrepared: Vector[(String, PreparedTemporal)] =
    Vector("clean" -> clean, "edges" -> edges).map((n, i) =>
      n -> get(temporalPlan(i).prepare(i))
    )

  private def temporalFamily(work: PreparedTemporal) = new ExecutionLaws.Family[
    TemporalC,
    TemporalStage,
    TemporalSegment,
    TemporalStudyError,
    TemporalR
  ](
    TemporalSegment.of,
    TemporalSegment.total(work, _, _),
    _ => work.run,
    sameTemporal,
    _ == _,
    20_000
  )

  temporalPrepared.foreach { (name, work) =>
    checkAll(
      s"temporal.$name",
      ExecutionLaws.conformance(temporalFamily(work), Gen.const(get(work.work())), quanta)
    )
  }

  // -------------------------------------------------------------------------
  // Runs that fail lawfully: a comparison budget below the grid's four cells
  // is refused when a cell's study cursor begins, after that cell's trials are
  // prepared. With five trials the failure cuts the Preparing block short
  // after four steps; with one trial the very first advance fails. The
  // reference is the literal expected error, not a run of the cursor.
  // -------------------------------------------------------------------------

  private val refusal = TemporalStudyError.Input(
    PlanError.ComparisonWork(ComparisonWorkError.WorkBudget("cosine", 4, 3))
  )
  private val single = get(TemporalStudyInput.of(alone, Vector(epoch(a))))

  private val failingTemporal: Vector[(String, PreparedTemporal)] = Vector(
    "budget-after-preparation" -> get(temporalPlan(clean).prepare(clean)),
    "budget-on-first-advance"  -> get(temporalPlan(single).prepare(single))
  )

  private def refusedFamily(work: PreparedTemporal) = new ExecutionLaws.Family[
    TemporalC,
    TemporalStage,
    TemporalSegment,
    TemporalStudyError,
    TemporalR
  ](
    TemporalSegment.of,
    TemporalSegment.total(work, _, _),
    _ => Left(refusal),
    sameTemporal,
    _ == _,
    20_000
  )

  failingTemporal.foreach { (name, work) =>
    checkAll(
      s"temporal.$name",
      ExecutionLaws.conformance(
        refusedFamily(work),
        Gen.const(get(work.work(get(ComparisonBudget.of(3))))),
        quanta
      )
    )
  }

  test("segment totals are total: a segment outside the work claims nothing") {
    val (_, work) = temporalPrepared.head
    assertEquals(StudySegment.total(binned, StudySegment.Estimating(7)), SegmentTotal.Unknown)
    assertEquals(
      StudySegment
        .total(binned, StudySegment.Reducing(7, StudyDesign.Matched), get(binned.work())),
      SegmentTotal.Unknown
    )
    assertEquals(
      TemporalSegment.total(work, TemporalSegment.Preparing(9, 0)),
      SegmentTotal.Unknown
    )
    assertEquals(
      TemporalSegment.total(work, TemporalSegment.Preparing(0, 9)),
      SegmentTotal.Unknown
    )
    assertEquals(
      TemporalSegment.total(work, TemporalSegment.Studying(-1, 0, StudySegment.Estimating(0))),
      SegmentTotal.Unknown
    )
    assertEquals(
      TemporalSegment.total(work, TemporalSegment.Preparing(1, 1)),
      SegmentTotal.Exact(rows.size.toLong)
    )
  }

  test("the failing fixtures fail where the laws must tolerate it") {
    val (_, after) = failingTemporal(0)
    val cut        = ExecutionLaws.trace(
      refusedFamily(after),
      get(after.work(get(ComparisonBudget.of(3)))),
      _ => ExecutionLaws.finest
    )
    assertEquals(cut.end, Some(Left(refusal)))
    assertEquals(
      cut.blocks.map((segment, units, totals) => (segment, units, totals.distinct)),
      Vector(
        (TemporalSegment.Preparing(0, 0), rows.size - 1L, Vector(SegmentTotal.Exact(rows.size)))
      )
    )
    val (_, first) = failingTemporal(1)
    val none       = ExecutionLaws.trace(
      refusedFamily(first),
      get(first.work(get(ComparisonBudget.of(3)))),
      _ => ExecutionLaws.finest
    )
    assertEquals((none.steps, none.end), (Vector.empty, Some(Left(refusal))))
  }

  // -------------------------------------------------------------------------
  // An independent oracle: the R-pinned matched/control fixture (rational
  // cosine scores, generated by tools/r-parity) states every recall key's
  // matched mean, control mean and difference without running a cursor.
  // -------------------------------------------------------------------------

  /** Four-cell dot products and two-value means need only rounding allowance. */
  private val ReferenceTolerance = Tolerance(absolute = 1e-12, relative = 0.0)

  private val pinnedFrame = get(Frame.screen("execution-laws-pinned", 2, 2))
  private val pinnedInput = StudyInput(
    Trials(
      MatchedControlFixtures.fixations
        .groupBy(row => StudyKey(row.participant, row.image, row.phase))
        .toVector
        .sortBy((key, _) => (key.participant, key.stimulus, key.phase))
        .map { (key, fixations) =>
          val trialClock = clock(key)
          Trial(
            key,
            (),
            get(
              Scanpath.of(
                pinnedFrame,
                trialClock,
                IArray.from(fixations.sortBy(_.ordinal).map { row =>
                  get(
                    Event.Fixation.withoutDispersion(
                      get(
                        Interval.of(
                          trialClock,
                          Instant.micros(row.onsetMicros),
                          Instant.micros(row.onsetMicros + row.durationMicros)
                        )
                      ),
                      Pt[Px](row.x, row.y),
                      row.sampleCount
                    )
                  )
                })
              )
            )
          )
        }
    )
  )
  private val pinned: Prepared = get(
    StudyPlan
      .of(
        pinnedInput.reference,
        StudyKey.layout(DefinitionId.studyLayout),
        get(Grid.over(pinnedFrame, 2, 2)),
        "recall",
        "encode",
        Weight.Duration,
        Vector(StudyEstimate.Binned()),
        FailurePolicy.RequireAll,
        cosine,
        ()
      )
      .flatMap(_.prepare(pinnedInput))
  )

  private def label(key: StudyKey): String = s"${key.participant}/${key.stimulus}/${key.phase}"

  /** The pinned reductions, checked against a run's end without consulting any cursor. */
  private def pinnedReductions(end: Either[PlanError, Result]): Prop = end match
    case Left(error)   => Prop.falsified :| s"the pinned study failed: $error"
    case Right(result) =>
      result.scales.map(_.contrast) match
        case Vector(Right(contrast)) =>
          val byKey = contrast.rows.map(row => label(row.key) -> row).toMap
          (byKey.size == MatchedControlFixtures.reductions.size) :| s"${byKey.size} rows" &&
          Prop.all(MatchedControlFixtures.reductions.map { expected =>
            val row      = byKey.get(expected.id)
            val observed = Vector(
              row.flatMap(_.matched).flatMap(_.result.toOption).map(_.value),
              row.flatMap(_.control).flatMap(_.result.toOption).map(_.value),
              row.flatMap(_.difference.toOption).map(_.value)
            )
            observed
              .zip(Vector(expected.matched, expected.control, expected.difference))
              .forall((value, want) =>
                value.exists(ReferenceTolerance.approxEquals(_, want))
              ) :|
              s"${expected.id}: $observed, pinned ${expected.matched}, ${expected.control}, ${expected.difference}"
          }*)
        case other => Prop.falsified :| s"expected one binned contrast, got $other"

  checkAll(
    "study.pinned",
    ExecutionLaws.conformance(
      studyFamily(pinned),
      Gen.const(get(pinned.work())),
      quanta,
      Some(pinnedReductions)
    )
  )

  test("the plans' own runs are the shipped instances driven at the default quanta") {
    prepared.foreach { (name, work) =>
      assert(
        sameStudy(get(Stepwise.complete(get(work.work()), WorkQuanta.default)), get(work.run)),
        name
      )
    }
    temporalPrepared.foreach { (name, work) =>
      assert(
        sameTemporal(
          get(Stepwise.complete(get(work.work()), WorkQuanta.default)),
          get(work.run)
        ),
        name
      )
    }
    recordingPlans.foreach { (name, plan) =>
      assert(
        sameRecording(
          get(Stepwise.complete(get(plan.work(recording)), WorkQuanta.default)),
          get(plan.run(recording))
        ),
        name
      )
    }
  }

  // -------------------------------------------------------------------------
  // Deliberate mutants
  // -------------------------------------------------------------------------

  private type Advance = Either[PlanError, WorkStep[StudyStage, Cursor, Result]]

  /** A stepwise instance for the study cursor with one injected fault. */
  private def mutant(fault: (Cursor, WorkQuanta) => Advance): Study =
    new Stepwise[Cursor, StudyStage, PlanError, Result]:
      def stage(cursor: Cursor): StudyStage                    = cursor.stage
      def advance(cursor: Cursor, quanta: WorkQuanta): Advance = fault(cursor, quanta)

  private val shipped: Study = Stepwise.study

  private def isFirstEstimate(cursor: Cursor): Boolean = cursor.stage match
    case StudyStage.Estimating(_, 0) => true
    case _                           => false

  private def isComparing(cursor: Cursor): Boolean = cursor.stage match
    case StudyStage.Comparing(_, _) => true
    case _                          => false

  private val unitsDropped = mutant((c, q) =>
    shipped.advance(c, q).map {
      case WorkStep.More(stage, _, next) => WorkStep.More(stage, 0, next)
      case WorkStep.Done(_, result)      => WorkStep.Done(0, result)
    }
  )

  private val trialSkipped = mutant((c, q) =>
    if isFirstEstimate(c) then
      shipped.advance(c, q).flatMap {
        case WorkStep.More(_, _, next) => shipped.advance(next, q)
        case done                      => Right(done)
      }
    else shipped.advance(c, q)
  )

  private val unitsDrift =
    var calls = 0
    mutant((c, q) =>
      calls += 1
      shipped.advance(c, q).map {
        case WorkStep.More(stage, units, next) => WorkStep.More(stage, units + calls, next)
        case done                              => done
      }
    )

  private val prematureDone = mutant((c, q) =>
    if isFirstEstimate(c) then Right(WorkStep.Done(1, decoyResult)) else shipped.advance(c, q)
  )

  private val finestSwapped = mutant((c, q) =>
    shipped.advance(c, q).map {
      case WorkStep.Done(units, _) if q.pairs.value == 1 => WorkStep.Done(units, decoyResult)
      case step                                          => step
    }
  )

  private val looping = mutant((c, q) =>
    shipped.advance(c, q).map {
      case WorkStep.Done(units, _) => WorkStep.More(c.stage, units, c)
      case step                    => step
    }
  )

  private val cutCharged = mutant((c, q) =>
    shipped.advance(c, q).map {
      case WorkStep.More(stage, units, next)
          if isComparing(c) && q.pairs.value == 1 && q.comparison.value == 1 =>
        WorkStep.More(stage, units + 1, next)
      case step => step
    }
  )

  private val segmentRevisited: StudyStage => StudySegment =
    case StudyStage.Estimating(scale, trial) if trial % 2 == 1 =>
      StudySegment.Contrasting(scale)
    case stage => StudySegment.of(stage)

  /** The mutants are checked over the binned fixture. */
  private val binned: Prepared = prepared.head._2

  private def binnedTotal(segment: StudySegment, cursor: Cursor): SegmentTotal =
    StudySegment.total(binned, segment, cursor)

  private val exactOverclaimed: (StudySegment, Cursor) => SegmentTotal = (segment, cursor) =>
    segment match
      case StudySegment.Estimating(_) =>
        binnedTotal(segment, cursor) match
          case SegmentTotal.Exact(n) => SegmentTotal.Exact(n + 1)
          case other                 => other
      case _ => binnedTotal(segment, cursor)

  private val atMostUnderstated: (StudySegment, Cursor) => SegmentTotal = (segment, cursor) =>
    segment match
      case StudySegment.Comparing(_, _) => SegmentTotal.AtMost(0L)
      case _                            => binnedTotal(segment, cursor)

  private val waveringTotal: (StudySegment, Cursor) => SegmentTotal = (segment, cursor) =>
    segment match
      case StudySegment.Estimating(_) if isFirstEstimate(cursor) => SegmentTotal.Unknown
      case _                                                     => binnedTotal(segment, cursor)

  private val determinism =
    "the same cursor at the same quanta yields the same steps and the same end"
  private val terminal =
    "a run ends in exactly one terminal step within the family's step budget"
  private val completion    = "completion at any quanta is the reference run"
  private val cutInvariance = "every sequence of quanta yields the reference run"
  private val contiguity    =
    "units are non-negative and each segment is visited in one contiguous block"
  private val statedOnce =
    "a segment's total is position-independent: every step of the segment states the same one"
  private val totals     = "an Exact total is met and an AtMost total is never exceeded"
  private val accounting =
    "the work each segment charges is a property of the cursor, not of its cuts"
  private val independentLaw =
    "completion at any sequence of quanta satisfies the independent oracle"

  private type StudyFamily =
    ExecutionLaws.Family[Cursor, StudyStage, StudySegment, PlanError, Result]

  /** Killed means falsified: a law that throws or gives up has not caught the mutant. */
  private def falsified(prop: Prop): Boolean =
    Test.check(killParameters, prop).status match
      case _: Test.Failed => true
      case _              => false

  /** The laws a family fails over the binned cursor, each run alone under the kill parameters. */
  private def failing(family: StudyFamily): Set[String] =
    ExecutionLaws
      .laws(family, Gen.const(get(binned.work())), quanta)
      .collect { case (name, prop) if falsified(prop) => name }
      .toSet

  private val mutants: Vector[(String, StudyFamily, Set[String])] =
    Vector(
      ("units-dropped", studyFamily(binned, stepwise = unitsDropped), Set(totals)),
      ("trial-skipped", studyFamily(binned, stepwise = trialSkipped), Set(totals)),
      (
        "units-drift",
        studyFamily(binned, stepwise = unitsDrift),
        Set(determinism, totals, accounting)
      ),
      (
        "premature-done",
        studyFamily(binned, stepwise = prematureDone),
        Set(completion, cutInvariance, totals)
      ),
      (
        "finest-swapped",
        studyFamily(binned, stepwise = finestSwapped),
        Set(completion, cutInvariance)
      ),
      (
        "looping",
        studyFamily(binned, stepwise = looping),
        Set(terminal, completion, cutInvariance, totals, accounting)
      ),
      ("cut-charged", studyFamily(binned, stepwise = cutCharged), Set(accounting)),
      (
        "segment-revisited",
        studyFamily(binned, segment = segmentRevisited),
        Set(contiguity, totals)
      ),
      ("exact-overclaimed", studyFamily(binned, total = Some(exactOverclaimed)), Set(totals)),
      ("atmost-understated", studyFamily(binned, total = Some(atMostUnderstated)), Set(totals)),
      ("wavering-total", studyFamily(binned, total = Some(waveringTotal)), Set(statedOnce))
    )

  test(
    "the shipped study instance fails no law alone and every mutant fails the laws that name it"
  ) {
    assertEquals(failing(studyFamily(binned)), Set.empty[String])
    mutants.foreach { (name, family, killers) =>
      val observed = failing(family)
      assert(
        killers.subsetOf(observed),
        s"$name: expected $killers to fail, observed $observed"
      )
      assertEquals(observed, killers, s"$name fails laws the receipts do not list")
    }
  }

  test(
    "a self-vouching instance passes every cut law and only the independent oracle kills it"
  ) {
    // Every Done carries the small binned fixture's result, and the family's
    // reference is this same instance at the default quanta: the cut laws are
    // satisfied by construction, which is why an independent oracle is needed.
    val selfVouching = mutant((c, q) =>
      shipped.advance(c, q).map {
        case WorkStep.Done(units, _) => WorkStep.Done(units, decoyResult)
        case step                    => step
      }
    )
    val family =
      new ExecutionLaws.Family[Cursor, StudyStage, StudySegment, PlanError, Result](
        StudySegment.of,
        StudySegment.total(pinned, _, _),
        cursor => Stepwise.complete(cursor, WorkQuanta.default)(using selfVouching),
        sameStudy,
        _ == _,
        stepBudget = 20_000
      )(using selfVouching)
    def killedBy(family: StudyFamily): Set[String] =
      ExecutionLaws
        .laws(family, Gen.const(get(pinned.work())), quanta, Some(pinnedReductions))
        .collect { case (name, prop) if falsified(prop) => name }
        .toSet
    assertEquals(killedBy(studyFamily(pinned)), Set.empty[String])
    assertEquals(killedBy(family), Set(independentLaw))
  }
