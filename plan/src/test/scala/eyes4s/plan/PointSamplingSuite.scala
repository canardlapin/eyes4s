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

package eyes4s.pointconsumer

import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import eyes4s.surface.*
import scala.compiletime.testing.typeCheckErrors

class PointSamplingSuite extends munit.FunSuite:
  private def get[E, A](v: Either[E, A]): A = v.fold(e => fail(e.toString), identity)
  private val OracleTolerance               = 1e-12
  private val clock                         = ClockId("point-onsets")
  private val frame                         = get(Frame.screen("point-map", 2, 2))
  private val grid                          = get(Grid.over(frame, 2, 2))
  private val layout = StudyKey.layout(get(DefinitionId.of("example.point-layout", 1)))
  private val policy = FailurePolicy.SuccessfulOnly(get(MinimumSuccessful.of(1)))
  private val input  = StudyInput(Trials(PointSamplingReference.sources.map { s =>
    val fixes = s.onsets.indices.map(i =>
      get(
        Event.Fixation.withoutDispersion(
          get(
            Interval.of(
              clock,
              Instant.micros(s.onsets(i)),
              Instant.micros(s.onsets(i) + s.durations(i))
            )
          ),
          Pt[Px](s.x(i), s.y(i)),
          1
        )
      )
    )
    Trial(
      StudyKey(s.stratum, s.matched, s.key),
      (),
      get(Scanpath.of(frame, clock, IArray.from(fixes)))
    )
  }))
  private val templates = Trials(PointSamplingReference.templates.map { t =>
    val values = IArray.from(t.values)
    Trial(
      StudyKey(t.stratum, t.key, "template-" + t.key),
      (),
      get(Surface.signed(grid, values, Provenance.raw(ContentHash.of(values))))
    )
  })
  private def spec(
      normalization: DensityNormalization = DensityNormalization.None,
      controls: PointControlSelection = PointControlSelection.Candidates(Selection.All),
      times: Vector[Long] = PointSamplingReference.queries,
      boundaries: Vector[Long] = PointSamplingReference.boundaries,
      endpoint: PointBinEndpoint = PointBinEndpoint.IncludeFinalEndpoint,
      queryClock: ClockId = clock,
      lookup: DensityLookupPolicy = DensityLookupPolicy.NearestClampedRIndex,
      binPolicy: FailurePolicy = policy
  ) = get(
    PointSamplingSpec.of(
      queryClock,
      times.map(Instant.micros),
      Some(get(PointBins.of(queryClock, boundaries.map(Instant.micros), endpoint))),
      normalization,
      lookup,
      TrajectoryEndpoint.OnsetRange,
      controls,
      policy,
      binPolicy
    )
  )
  private def run(
      s: PointSamplingSpec = spec(),
      source: StudyInput[StudyKey, Px] = input,
      refs: Trials[StudyKey, Unit, Signed[Px]] = templates
  ) = PointSamplingPlan.of(layout, source, refs, s).run
  private def same(actual: Vector[Option[Double]], expected: Vector[Option[Double]]): Unit =
    assertEquals(actual.size, expected.size)
    actual.zip(expected).foreach { (a, b) =>
      (a, b) match
        case (Some(x), Some(y)) => assertEqualsDouble(x, y, OracleTolerance)
        case _                  => assertEquals(a, b)
    }

  test(
    "public static workflow agrees with independently checked R values for every normalization and exhaustive/disabled controls"
  ) {
    val modes = Vector(
      "none"   -> DensityNormalization.None,
      "max"    -> DensityNormalization.Maximum,
      "sum"    -> DensityNormalization.Sum,
      "zscore" -> DensityNormalization.SampleZScore
    )
    for (name, n) <- modes; cap <- Vector(0, 20) do
      val result = run(
        spec(
          n,
          if cap == 0 then PointControlSelection.Disabled
          else PointControlSelection.Candidates(Selection.All)
        )
      )
      assertEquals(result.rows.map(_.key), input.trials.rows.map(_.key));
      assertEquals(result.unbinned, Vector(5, 6))
      result.rows.foreach { row =>
        val expected = PointSamplingReference.results
          .find(r => r.normalization == name && r.cap == cap && r.key == row.key.phase)
          .get
        assertEquals(row.points.map(_.time.toMicros), PointSamplingReference.queries)
        same(row.points.map(_.value.toOption), expected.values)
        same(row.bins.map(_.observed.result.toOption), expected.bins)
        expected.controls match
          case None =>
            assert(row.points.forall(_.control.isEmpty));
            assert(row.bins.forall(_.control.isEmpty))
          case Some(values) =>
            same(row.points.map(_.control.flatMap(_.result.toOption)), values)
            same(row.bins.map(_.control.flatMap(_.result.toOption)), expected.controlBins.get)
        assertEquals(row.bins.map(_.queries), Vector(Vector(1, 3), Vector(0, 2, 4)))
        assertEquals(row.bins.map(_.observed.requested), Vector(2, 3))
        assertEquals(row.bins.map(_.observed.contributing), Vector(2, 3))
      }
  }
  test(
    "control population preserves source occurrences, excludes all true copies and uses the focal path"
  ) {
    val rows = run().rows
    assertEquals(rows.map(_.eligibleControls), Vector(2, 2, 3, 3, 0))
    assertEquals(rows(0).controls.map(_.occurrence.phase).toSet, Set("b1", "c1"))
    assertEquals(rows(2).controls.map(_.occurrence.phase).toSet, Set("a1", "a2", "c1"))
    assertEquals(rows(2).controls.count(_.template.stimulus == "A"), 2)
    assertEquals(
      rows(0).controls.find(_.occurrence.phase == "b1").get.values.take(3),
      Vector(Right(6.0), Right(2.0), Right(9.0))
    )
    assertEquals(rows(4).points.head.control.get.requested, 0)
    assert(rows(4).points.head.control.get.result.isLeft)
    assertEquals(rows(0).points(5).control.get.successful, 0)
    assertEquals(rows(0).points(5).control.get.requested, 2)
  }
  test(
    "finite keyed controls are deterministic and order invariant with exact occurrence identities"
  ) {
    def selection(cap: Int) = PointControlSelection.Candidates(
      Selection.BottomK(get(PairLimit.of(cap)), Seed(Long.MinValue + 7), SampleId("points"))
    )
    def edges(r: PointSamplingResult[StudyKey, Px]) =
      r.rows.flatMap(row => row.controls.map(c => row.key -> c.occurrence)).toSet
    val one = run(spec(controls = selection(1)))
    assertEquals(one.rows.map(_.controls.size), Vector(1, 1, 1, 1, 0))
    assertEquals(
      edges(one),
      edges(run(spec(controls = selection(1)), StudyInput(Trials(input.trials.rows.reverse))))
    )
    assert(edges(one).subsetOf(edges(run(spec(controls = selection(2))))))
    assertEquals(one.controlPairing.get.selectedPairCount, 4)
  }
  test("final endpoint is explicit; empty and all-missing bins keep their counts") {
    assertEquals(
      run(spec(endpoint = PointBinEndpoint.HalfOpen)).rows.head.bins.map(_.queries),
      Vector(Vector(1, 3), Vector(2))
    )
    val r = run(spec(boundaries = Vector(-10000, 0, 10000, 20000, 30000, 40000))).rows.head
    assertEquals(r.bins.map(_.observed.requested), Vector(1, 2, 1, 3, 0))
    assertEquals(r.bins.map(_.observed.successful), Vector(0, 2, 1, 2, 0))
    assert(r.bins.head.observed.result.isLeft); assert(r.bins.last.observed.result.isLeft)
    val strict = run(
      spec(
        boundaries = Vector(-10000, 0, 10000, 20000, 30000, 40000),
        binPolicy = FailurePolicy.RequireAll
      )
    ).rows.head
    assertEquals(strict.bins(3).observed.successful, 2);
    assertEquals(strict.bins(3).observed.contributing, 0)
    assertEquals(run(spec(times = Vector.empty)).rows.head.points, Vector.empty)
    assert(run(spec(times = Vector.empty)).rows.head.bins.forall(_.observed.result.isLeft))
  }
  test("matching failures and duplicate source keys retain every row and query") {
    val a       = input.trials.rows.head
    val absent  = Trial(a.key.copy(stimulus = "absent"), (), a.value)
    val missing = run(source = StudyInput(Trials(input.trials.rows :+ absent)))
    assertEquals(missing.rows.size, 6);
    assert(
      missing.rows.last.template.left.toOption
        .exists(_.isInstanceOf[PointSamplingError.MissingTemplate])
    )
    assertEquals(missing.rows.last.points.size, 7);
    assert(missing.rows.last.points.forall(_.value.isLeft))
    val ambiguous = run(refs =
      Trials(
        templates.rows :+ templates.rows.head
          .copy(key = templates.rows.head.key.copy(phase = "duplicate"))
      )
    )
    assertEquals(
      ambiguous.rows.take(2).map(_.template.left.toOption),
      Vector.fill(2)(Some(PointSamplingError.AmbiguousTemplate("p1", "A", Vector(0, 4))))
    )
    val duplicate = run(source = StudyInput(Trials(input.trials.rows :+ a)))
    assertEquals(
      duplicate.rows.head.template,
      Left(PointSamplingError.DuplicateSource(Vector(0, 5)))
    )
    assertEquals(duplicate.rows.last.template, duplicate.rows.head.template)
    assertEquals(duplicate.rows(2).eligibleControls, 2)
    assertEquals(
      run(source = StudyInput(Trials(Vector.empty[Trial[StudyKey, Unit, Scanpath[Px]]]))).rows,
      Vector.empty
    )
  }
  test(
    "clock and frame disagreements are operand-bearing failures, including no containing cell"
  ) {
    val wrong = run(spec(queryClock = ClockId("different")))
    assert(
      wrong.rows.forall(
        _.points.forall(_.value.left.toOption.exists(_.isInstanceOf[PointSamplingError.Clock]))
      )
    )
    val foreign = get(Grid.over(get(Frame.screen("foreign", 2, 2)), 2, 2))
    val values  = IArray(1.0, 2.0, 3.0, 4.0)
    val refs    = Trials(
      templates.rows.map(t =>
        t.copy(value =
          get(Surface.signed(foreign, values, Provenance.raw(ContentHash.of(values))))
        )
      )
    )
    assert(
      run(refs = refs).rows.forall(
        _.points.forall(_.value.left.toOption.exists(_.isInstanceOf[PointSamplingError.Frames]))
      )
    )
    assert(
      run(spec(lookup = DensityLookupPolicy.ContainingCell)).rows.head.points.head.value.isLeft
    )
    val bins = get(
      PointBins.of(
        clock,
        Vector(Instant.micros(0), Instant.micros(1)),
        PointBinEndpoint.HalfOpen
      )
    )
    assert(
      PointSamplingSpec
        .of(
          ClockId("different"),
          Vector.empty,
          Some(bins),
          DensityNormalization.None,
          DensityLookupPolicy.ContainingCell,
          TrajectoryEndpoint.OnsetRange,
          PointControlSelection.Disabled,
          policy,
          policy
        )
        .isLeft
    )
  }
  test(
    "point bins preserve adjacent microseconds above the double integer limit and reject invalid boundaries"
  ) {
    val base = 9007199254740992L
    val b    = get(
      PointBins.of(
        clock,
        Vector(base, base + 1, base + 2).map(Instant.micros),
        PointBinEndpoint.IncludeFinalEndpoint
      )
    )
    assertEquals(
      Vector(base - 1, base, base + 1, base + 2, base + 3).map(x => b.index(Instant.micros(x))),
      Vector(None, Some(0), Some(1), Some(1), None)
    )
    for v <- Vector(Vector.empty[Long], Vector(0L), Vector(0L, 0L), Vector(1L, 0L)) do
      assert(PointBins.of(clock, v.map(Instant.micros), PointBinEndpoint.HalfOpen).isLeft)
  }
  test(
    "mean of control-time means differs from pooled control values and preserves failure counts"
  ) {
    val missing  = Left(PointSamplingError.MissingTemplate("p", "s"))
    val controls = Vector(
      Vector(missing, Right(3.0)),
      Vector(Right(9.0), Right(1.0)),
      Vector(missing, Right(3.0))
    )
    val times = controls.map(v => PointSamplingMean("time", v, policy))
    assertEquals(times.map(_.successful), Vector(1, 2, 1))
    val nested = PointSamplingMean("bin", times.map(_.result), policy)
    val pooled = PointSamplingMean("pooled", controls.flatten, policy)
    assertEqualsDouble(get(nested.result), 11.0 / 3, OracleTolerance)
    assertEqualsDouble(get(pooled.result), 4.0, OracleTolerance)
    assertEquals(
      PointSamplingMean("strict", controls.head, FailurePolicy.RequireAll).contributing,
      0
    )
    val nonfinite =
      PointSamplingMean("nonfinite", Vector(missing, Right(Double.NaN), Right(1.0)), policy)
    assertEquals(nonfinite.successful, 1)
    nonfinite.result match
      case Left(PointSamplingError.NonFinite("nonfinite", 1, v)) => assert(v.isNaN)
      case other                                                 => fail(other.toString)
    assert(PointSamplingMean("empty", Vector.empty, policy).result.isLeft)
  }
  test("checked plan components cannot be fabricated by an external consumer") {
    assert(
      typeCheckErrors(
        "new eyes4s.plan.PointBins(null,Vector.empty,eyes4s.plan.PointBinEndpoint.HalfOpen)"
      ).nonEmpty
    )
    assert(
      typeCheckErrors(
        "new eyes4s.plan.PointMean(0,0,eyes4s.design.FailurePolicy.RequireAll,Right(1.0))"
      ).nonEmpty
    )
  }
