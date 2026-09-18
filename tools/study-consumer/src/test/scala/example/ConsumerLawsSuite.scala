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

import eyes4s.codec.*
import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.laws.*
import eyes4s.plan.*
import org.scalacheck.{Gen, Prop, Test}

/** UI-G1: the published execution and manifest laws, run by the consumer
  * over its own routes and types, on the JVM and Scala.js.
  *
  * `ExecutionLaws` drives each route's cursor through `Stepwise` at generated
  * quanta and sequences of quanta, and holds every cut to the route's own
  * reference run and to an independent oracle: the rational and decimal
  * cosine targets for the fixation routes, the I-VT conformance events for
  * the recording routes, and the integer-overlap ledgers and decimal cosine
  * targets for the temporal routes. `ManifestLaws` holds the consumer's own
  * writers (`FixationJourney.save`, `RecordingJourney.save`,
  * `TemporalJourney.save`) and registrations to verified resolution. Both
  * law sets are shown to have teeth here with one deliberate mutant each.
  */
class ConsumerLawsSuite extends munit.DisciplineSuite:
  import JourneySetup.{comparison, gaussian, get, pairs}

  /** The generator varies only the quanta and the stored graph, so a modest
    * count covers every drawn value; the seed makes a failure reproducible.
    */
  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(20).withInitialSeed(0x47314c41L)

  private val Oracle = Tolerance(absolute = 1e-12, relative = 0)

  private val quanta =
    ExecutionLaws.quanta(
      pairs = Seq(1, 2, 5),
      comparison = Seq(1, 2, 3),
      samples = Seq(1, 2, 3, 7)
    )

  // ---------------------------------------------------------------------------
  // Fixation routes: the journey table at the binned and sigma 1 scales
  // ---------------------------------------------------------------------------

  private def fixation[K, P, S, D](j: Journey[K, P, S, D]): Unit =
    val name  = j.c.name
    val plan  = j.plan(j.input, Vector(StudyEstimate.Binned[Px](), gaussian(1.0)))
    val work  = get(plan.preflight(Some(j.input), pairs).prepare(plan, j.input, pairs))
    val m     = j.c.multiplier
    val sigma = JourneyFixtures.gaussian.find(_._1 == 1.0).map(_._2).getOrElse(Map.empty)
    def labelled(result: StudyResult[K, Px, S, D], scale: Int) =
      result.scales(scale).contrast.toOption.toVector.flatMap(_.rows).map { row =>
        j.c.label(row.key) -> row.difference.toOption.map(
          j.schema.difference(_).components.head.value
        )
      }
    val independent = (outcome: Either[PlanError, StudyResult[K, Px, S, D]]) =>
      outcome match
        case Left(error)   => Prop.falsified :| s"the run failed: $error"
        case Right(result) =>
          val binned   = labelled(result, 0)
          val gaussian = labelled(result, 1)
          Prop(
            binned.map(_._1) == JourneyFixtures.binned.map(_._1) &&
              binned.zip(JourneyFixtures.binned).forall { case ((_, value), (_, (_, _, d))) =>
                value.exists(v => Oracle.approxEquals(v, m * d.value))
              } &&
              gaussian.size == sigma.size &&
              gaussian.forall((label, value) =>
                value.exists(v => Oracle.approxEquals(v, m * sigma(label)))
              )
          ) :| "the rational and decimal cosine oracles"
    checkAll(
      s"execution.fixation.$name",
      ExecutionLaws.conformance(
        new ExecutionLaws.Family[
          StudyCursor[K, Px, S, D],
          StudyStage,
          StudySegment,
          PlanError,
          StudyResult[K, Px, S, D]
        ](
          StudySegment.of,
          StudySegment.total(work, _, _),
          _ => work.run,
          j.same,
          _ == _,
          20_000
        ),
        Gen.const(get(work.work(comparison))),
        quanta,
        Some(independent)
      )
    )
    // Two saved studies of this route: duration and uniform weighting.
    lazy val runs = Vector(Weight.Duration, Weight.Uniform).map { w =>
      val study = j.plan(j.input, weighting = w)
      w -> (study, get(study.run(j.input)))
    }.toMap
    checkAll(
      s"manifest.fixation.$name",
      ManifestLaws.verifiedResolution(
        Gen.oneOf(Weight.Duration, Weight.Uniform),
        (w: Weight) =>
          val (study, result) = runs(w)
          FixationJourney.save(j.route, study, j.input, j.ledger, result).map(StoredGraph.of)
        ,
        get(j.route.decoders),
        (w: Weight, resolved: ResolvedManifest[K, Px]) =>
          resolved.results.map(_._2.encode) ==
            Vector(j.route.results.codec.encode(runs(w)._2))
      )
    )

  fixation(Journey(JourneyCases.cosine))
  fixation(Journey(JourneyCases.scaled))

  // ---------------------------------------------------------------------------
  // Recording routes: the I-VT conformance fixture through the angular warp
  // ---------------------------------------------------------------------------

  private def recordingFamily[P](
      run: RecordingCase[P],
      plan: RecordingPlan[P],
      total: (RecordingSegment, RecordingCursor[P]) => SegmentTotal
  ) = new ExecutionLaws.Family[
    RecordingCursor[P],
    RecordingStage,
    RecordingSegment,
    RecordingPlanError,
    RecordingAnalysis[P]
  ](
    RecordingSegment.of,
    total,
    _ => plan.run(RecordingFixtures.recording),
    run.same,
    _ == _,
    20_000
  )

  private def recordingLaws[P](run: RecordingCase[P]): Unit =
    import RecordingFixtures.{angular, areas, input, recording}
    val name = run.route.name
    checkAll(
      s"execution.recording.$name",
      ExecutionLaws.conformance(
        recordingFamily(run, run.plan, (s, c) => RecordingSegment.total(c.samples, s)),
        Gen.const(get(run.plan.work(recording))),
        quanta,
        Some((outcome: Either[RecordingPlanError, RecordingAnalysis[P]]) =>
          Prop(outcome.exists(run.matchesOracle(_, 1e-9))) :| "the I-VT conformance events"
        )
      )
    )
    // Saved analyses of this route at three interpolation gaps.
    val gaps      = Vector(0L, 5000L, 20000L)
    lazy val runs = gaps.map { micros =>
      val plan = get(
        run.route.plan(
          input,
          angular,
          get(RecipeParameters.interpolationGap.parse(Span.micros(micros))),
          areas
        )
      )
      micros -> (plan, get(plan.run(recording)))
    }.toMap
    checkAll(
      s"manifest.recording.$name",
      ManifestLaws.verifiedResolution(
        Gen.oneOf(gaps),
        (micros: Long) =>
          val (plan, analysis) = runs(micros)
          RecordingJourney.save(run.route, input, plan, analysis).map(StoredGraph.of)
        ,
        get(run.route.decoders),
        (micros: Long, resolved: ResolvedManifest[StudyKey, Px]) =>
          resolved.recordingResults.map(_._2.encode) ==
            Vector(run.route.results.codec.encode(runs(micros)._2))
      )
    )

  recordingLaws(RecordingRun.ivt)
  recordingLaws(RecordingRun.lab)

  // ---------------------------------------------------------------------------
  // Temporal routes: one repetition over an observed and an unobserved window
  // ---------------------------------------------------------------------------

  private def temporal[K, P, S, D](t: TemporalCase[K, P, S, D]): Unit =
    val name    = t.c.name
    val windows = t.windows.filter(w => w.name == "early" || w.name == "outside")
    def plan(boundary: FixationBoundary) =
      t.plan(
        t.input,
        scales = Vector(StudyEstimate.Binned[Px]()),
        cells = windows,
        repeats = t.repetitions.take(1),
        bound = boundary
      )
    val study   = plan(t.boundary)
    val work    = t.prepare(study, t.input)
    val ledgers = TemporalConsumerFixtures.ledgers.map {
      (key, window, retained, observed, missing) =>
        (key, window) -> (retained, observed, missing)
    }.toMap
    val targets = TemporalConsumerFixtures.targets.collect {
      case target
          if target.sigma.isEmpty && target.repetition == "recall-encode" &&
            (target.window == "early" || target.window == "outside") =>
        (target.repetition, target.key, target.window, target.sigma) -> target.difference
    }.toMap
    val independent =
      (outcome: Either[TemporalStudyError, TemporalStudyResult[K, Px, P, S, D]]) =>
        outcome match
          case Left(error)   => Prop.falsified :| s"the run failed: $error"
          case Right(result) =>
            val found     = t.ledgers(result)
            val contrasts = t.contrasts(result)
            Prop(
              found.size == 2 * 18 && found.forall((at, ledger) =>
                ledgers.get(at).contains(ledger)
              )
            ) :| "the integer-overlap ledgers" &&
            Prop(
              contrasts.keySet == targets.keySet && targets.forall { (at, expected) =>
                (contrasts(at), expected) match
                  case (Some(v), Some(e)) => Oracle.approxEquals(v, t.c.multiplier * e)
                  case (v, e)             => v == e
              }
            ) :| "the decimal cosine targets"
    checkAll(
      s"execution.temporal.$name",
      ExecutionLaws.conformance(
        new ExecutionLaws.Family[
          TemporalCursor[K, Px, P, S, D],
          TemporalStage,
          TemporalSegment,
          TemporalStudyError,
          TemporalStudyResult[K, Px, P, S, D]
        ](
          TemporalSegment.of,
          TemporalSegment.total(work, _, _),
          _ => work.run,
          t.same,
          _ == _,
          20_000
        ),
        Gen.const(get(work.work(comparison))),
        quanta,
        Some(independent)
      )
    )
    val boundaries = Vector(FixationBoundary.ClipDuration, FixationBoundary.FullyContained)
    lazy val runs  = boundaries.map { b =>
      val p = plan(b)
      b -> (p, get(t.prepare(p, t.input).run))
    }.toMap
    checkAll(
      s"manifest.temporal.$name",
      ManifestLaws.verifiedResolution(
        Gen.oneOf(boundaries),
        (b: FixationBoundary) =>
          val (p, result) = runs(b)
          TemporalJourney.save(t.route, t.input, t.ledger, p, result).map(StoredGraph.of)
        ,
        get(t.route.decoders),
        (b: FixationBoundary, resolved: ResolvedManifest[K, Px]) =>
          resolved.temporalResults.map(_._2.encode) ==
            Vector(t.route.results.codec.encode(runs(b)._2))
      )
    )

  temporal(TemporalRun.cosine)
  temporal(TemporalRun.scaled)

  // ---------------------------------------------------------------------------
  // The laws have teeth over these routes
  // ---------------------------------------------------------------------------

  private val params =
    Test.Parameters.default.withMinSuccessfulTests(20).withInitialSeed(0x4d555441L)

  private def falsified(props: Seq[(String, Prop)]): Vector[String] =
    props.collect { case (name, prop) if !Test.check(params, prop).passed => name }.toVector

  test("a detection total claimed one sample too many is killed by the totals law") {
    val run    = RecordingRun.lab
    val mutant = recordingFamily(
      run,
      run.plan,
      (segment, cursor) =>
        segment match
          case RecordingSegment.Detecting => SegmentTotal.Exact(cursor.samples.toLong + 1)
          case other                      => RecordingSegment.total(cursor.samples, other)
    )
    assertEquals(
      falsified(
        ExecutionLaws
          .laws(mutant, Gen.const(get(run.plan.work(RecordingFixtures.recording))), quanta)
      ),
      Vector("an Exact total is met and an AtMost total is never exceeded")
    )
  }

  test("a writer that drops a packed payload is killed by verified resolution") {
    val run     = RecordingRun.lab
    val dropped = (_: Unit) =>
      RecordingJourney
        .save(run.route, RecordingFixtures.input, run.plan, run.pure)
        .map(StoredGraph.of)
        .map(graph =>
          graph.copy(entries = graph.entries.filterNot(_._1.value.endsWith(".values")))
        )
    val laws = ManifestLaws.verifiedResolution(
      Gen.const(()),
      dropped,
      get(run.route.decoders),
      (_: Unit, _: ResolvedManifest[StudyKey, Px]) => true
    )
    // The dropped payload is missing when the graph resolves, and beside any
    // corrupted byte, so the refusal is never the single digest the law expects.
    assertEquals(
      falsified(laws.props),
      Vector(
        "a written graph resolves by address, reading each artifact once",
        "any single corrupted byte is refused by its digest before anything is decoded"
      )
    )
  }
