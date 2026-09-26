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

package eyes4s.scanpathconsumer

import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import scala.compiletime.testing.typeCheckErrors

class ScanpathReferenceSuite extends munit.FunSuite:
  private def get[E, A](x: Either[E, A]): A = x.fold(e => fail(e.toString), identity)
  private val AnalyticTolerance             = 1e-12
  // T4transport's stopping tolerance is floored at sqrt(machine epsilon).
  private val BackendTolerance      = 1e-7
  private val MarginalCostTolerance = 1e-9
  private val frame                 = get(Frame.screen("scanpath-reference", 800, 400))
  private val clock                 = ClockId("scanpath-reference-milliseconds")
  private def path(p: ScanpathReference.Path): Scanpath[Px] =
    get(
      Scanpath.of(
        frame,
        clock,
        IArray.from(p.x.indices.map { i =>
          get(
            Event.Fixation.of(
              get(
                Interval.of(
                  clock,
                  Instant.millis(p.onset(i).toLong),
                  Instant.millis((p.onset(i) + p.duration(i)).toLong)
                )
              ),
              Pt[Px](p.x(i), p.y(i)),
              0,
              DispersionMethod.RmsRadius,
              1
            )
          )
        })
      )
    )
  private val base       = path(ScanpathReference.paths.head)
  private val translated = path(ScanpathReference.paths(1))
  private def singleton(x: Double, y: Double, time: Long, duration: Long = 10): Scanpath[Px] =
    get(
      Scanpath.of(
        frame,
        clock,
        IArray(
          get(
            Event.Fixation.of(
              get(Interval.of(clock, Instant.millis(time), Instant.millis(time + duration))),
              Pt[Px](x, y),
              0,
              DispersionMethod.RmsRadius,
              1
            )
          )
        )
      )
    )
  private def config(limit: Int) =
    get(
      FixationTransportConfig.of[Px](1000, 1000, Span.millis(3000), .8, .1, 10000, 1e-10, limit)
    )

  ScanpathReference.paths.foreach { p =>
    test(
      s"${p.name}: five portable components match exported R calls; sixth availability is retained"
    ) {
      val result = ScanpathComparison.baseline(base, path(p))
      assertEquals(result.values.map(_._1), ScanpathComponent.values.toVector)
      result.values.take(5).zip(p.expected).foreach { case ((_, score), expected) =>
        assertEqualsDouble(get(score).value, expected, AnalyticTolerance)
      }
      assert(result.values.last._2.isLeft)
    }
  }
  test(
    "analytic translation, duration, tied alignment and sixth backend value remain separate"
  ) {
    val scores =
      ScanpathComparison.baseline(base, translated).values.take(5).map(x => get(x._2).value)
    assertEqualsDouble(scores(3), 1 - 50 / math.hypot(800, 400), AnalyticTolerance)
    assertEqualsDouble(ScanpathReference.paths(1).expected(5), scores(3), AnalyticTolerance)
    val t = path(ScanpathReference.paths.find(_.name == "tie").get)
    ScanpathComparison.baseline(t, t).values.take(5).zip(ScanpathReference.tieSelf).foreach {
      case ((_, score), expected) =>
        assertEqualsDouble(get(score).value, expected, AnalyticTolerance)
    }
    val duration = ScanpathComparison.baseline(base, path(ScanpathReference.paths(2)))
    assertEqualsDouble(get(duration.values(4)._2).value, .5, AnalyticTolerance)
    // Pinned sixth component on changed duration masses returns 1; it is not an exact OT oracle.
    assertEquals(ScanpathReference.paths(2).expected(5), 1.0)
  }
  test("minimum three and half-open onset windows preserve component failures") {
    val two = get(
      base.within(
        get(Window.of(Span.zero, Span.millis(200))),
        Instant.millis(0),
        Overlap.OnsetInside
      )
    )
    assertEquals(two.n, 2)
    assert(ScanpathComparison.baseline(two, base).values.forall(_._2.isLeft))
    assert(MultiMatch[Px].compare(two, two).isRight)
    assert(
      base
        .within(
          get(Window.of(Span.millis(300), Span.millis(400))),
          Instant.millis(0),
          Overlap.OnsetInside
        )
        .isLeft
    )
    assert(ScanpathComparison.baseline(singleton(1, 1, 0), base).values.forall(_._2.isLeft))
    val all = get(
      base.within(
        get(Window.of(Span.zero, Span.millis(201))),
        Instant.millis(0),
        Overlap.OnsetInside
      )
    )
    assertEquals(all.n, 3)
    assert(
      Event.Fixation
        .of(
          get(Interval.of(clock, Instant.millis(0), Instant.millis(0))),
          Pt[Px](1, 1),
          0,
          DispersionMethod.RmsRadius,
          1
        )
        .isLeft
    )
    assert(Scanpath.of(frame, clock, IArray(base.first, base.first, base.last)).isLeft)
  }
  test(
    "strict Euclidean and Manhattan overlap matches R with every requested query in the denominator"
  ) {
    val queries = Vector(-20L, 0L, 50L, 100L, 200L, 220L).map(Instant.millis)
    ScanpathReference.overlap.foreach { (method, threshold, count, score) =>
      val distance = if method == "euclidean" then FixationGroundDistance.Euclidean
      else FixationGroundDistance.Manhattan
      val r = get(
        FixationOverlap.compare(
          FixationTrajectory.fromScanpath(base),
          FixationTrajectory.fromScanpath(translated),
          clock,
          queries,
          get(OverlapThreshold.of[Px](threshold)),
          distance,
          // eyesim holds each path's final fixation after its onset.
          TrajectoryEndpoint.HoldLastOnset,
          MissingOverlapPolicy.CountAsNonOverlap
        )
      )
      assertEquals(r.requested, 6); assertEquals(r.contributing, 5);
      assertEquals(r.overlaps, count)
      assertEqualsDouble(get(r.similarity).value, score, AnalyticTolerance)
      assertEquals(r.rows.map(_.time), queries)
    }
  }
  test(
    "overlap missing support, empty query, threshold, identity and endpoint policies are values"
  ) {
    def run(
        a: FixationTrajectory[Px],
        b: FixationTrajectory[Px],
        queries: Vector[Instant],
        policy: MissingOverlapPolicy,
        endpoint: TrajectoryEndpoint = TrajectoryEndpoint.OnsetRange,
        c: ClockId = clock
    ) =
      FixationOverlap.compare(
        a,
        b,
        c,
        queries,
        get(OverlapThreshold.of[Px](60)),
        FixationGroundDistance.Euclidean,
        endpoint,
        policy
      )
    val a       = FixationTrajectory.fromScanpath(base);
    val b       = FixationTrajectory.fromScanpath(translated)
    val queries = Vector(Instant.millis(0), Instant.millis(220))
    assert(get(run(a, b, queries, MissingOverlapPolicy.RequireComplete)).similarity.isLeft)
    assertEqualsDouble(
      get(get(run(a, b, queries, MissingOverlapPolicy.CountAsNonOverlap)).similarity).value,
      .5,
      AnalyticTolerance
    )
    assertEqualsDouble(
      get(
        get(
          run(
            a,
            b,
            queries,
            MissingOverlapPolicy.RequireComplete,
            TrajectoryEndpoint.HoldLastOnset
          )
        ).similarity
      ).value,
      1,
      AnalyticTolerance
    )
    assertEquals(
      get(
        run(
          a,
          FixationTrajectory.empty(frame, clock),
          queries,
          MissingOverlapPolicy.CountAsNonOverlap
        )
      ).contributing,
      0
    )
    assert(run(a, b, Vector.empty, MissingOverlapPolicy.CountAsNonOverlap).isLeft)
    assert(
      run(a, b, queries, MissingOverlapPolicy.CountAsNonOverlap, c = ClockId("foreign")).isLeft
    )
    assert(OverlapThreshold.of[Px](-1).isLeft);
    assert(OverlapThreshold.of[Px](Double.NaN).isLeft)
    val foreign = FixationTrajectory.empty(get(Frame.screen("foreign", 800, 400)), clock)
    assert(run(a, foreign, queries, MissingOverlapPolicy.CountAsNonOverlap).isLeft)
    assert(
      typeCheckErrors(
        "new eyes4s.compare.OverlapThreshold[eyes4s.kernel.Unit2D.Px](1)"
      ).nonEmpty
    )
    assert(
      typeCheckErrors(
        "new eyes4s.compare.FixationTransportConfig[eyes4s.kernel.Unit2D.Px](1,1,eyes4s.kernel.Span.millis(1),1,1,1,1,1)"
      ).nonEmpty
    )
  }
  test("singleton transport separates spatial, onset, squared cost, root cost and similarity") {
    val c      = get(FixationTransportConfig.of[Px](10, 10, Span.millis(100), 1, .1))
    val result =
      get(FixationTransport.compare(singleton(0, 0, 0, 2), singleton(3, 4, 100, 7), c))
    assertEqualsDouble(result.squaredCost, 1.25, AnalyticTolerance)
    assertEqualsDouble(result.rootCost, math.sqrt(1.25), AnalyticTolerance)
    assertEqualsDouble(
      get(result.similarity).value,
      1 / (1 + math.sqrt(1.25)),
      AnalyticTolerance
    )
    val spatial = get(FixationTransportConfig.of[Px](10, 10, Span.millis(100), 0, .1))
    assertEqualsDouble(
      get(
        FixationTransport.compare(singleton(0, 0, 0), singleton(3, 4, 100), spatial)
      ).rootCost,
      .5,
      AnalyticTolerance
    )
  }
  test(
    "transport agrees with independent primal oracle and retains failed default-backend conformance"
  ) {
    ScanpathReference.transport.foreach { (lambda, expected, reference, referencePass) =>
      val tight = get(
        FixationTransportConfig
          .of[Px](1000, 1000, Span.millis(3000), .8, lambda, marginalTolerance = 1e-13)
      )
      val result = get(FixationTransport.compare(base, translated, tight))
      assert(result.marginalResidual <= 1e-10)
      assertEquals(result.leftCount, 3); assertEquals(result.rightCount, 3)
      assertEqualsDouble(get(result.similarity).value, expected, AnalyticTolerance)
      assertEquals(math.abs(reference - expected) <= BackendTolerance, referencePass)
      if lambda == .01 then assert(!referencePass) else assert(referencePass)
    }
  }
  test("two-point analytic entropic bias, duration weighting and convergence refusal") {
    def two(d1: Long, d2: Long) = get(
      Scanpath.of(
        frame,
        clock,
        IArray(singleton(0, 0, 0, d1).first, singleton(1, 0, 100, d2).first)
      )
    )
    val equal = two(50, 50)
    val c     = get(FixationTransportConfig.of[Px](1, 1, Span.millis(1), 0, .5))
    val r     = get(FixationTransport.compare(equal, equal, c))
    assertEqualsDouble(r.squaredCost, 1 / (1 + math.exp(2)), AnalyticTolerance)
    assert(r.rootCost > 0); assert(get(r.similarity).value < 1) // no debiasing
    val one =
      get(FixationTransportConfig.of[Px](1, 1, Span.millis(1), 0, .5, maximumIterations = 1))
    assert(FixationTransport.compare(two(80, 20), two(30, 70), one).isLeft)
    val weighted = get(FixationTransport.compare(two(80, 20), two(30, 70), c))
    // Independent 2x2 marginal equation: t(t-.1)=exp(4)(.8-t)(.3-t), cost=1.1-2t.
    assertEqualsDouble(weighted.squaredCost, 0.5042988748479535, MarginalCostTolerance)
    assert(FixationTransport.compare(base, translated, config(limit = 8)).isLeft)
    assert(FixationTransportConfig.of[Px](0, 1, Span.millis(1), 0, .1).isLeft)
    assert(FixationTransportConfig.of[Px](1, 1, Span.zero, 0, .1).isLeft)
    assert(FixationTransportConfig.of[Px](1, 1, Span.millis(1), -1, .1).isLeft)
    assert(FixationTransportConfig.of[Px](1, 1, Span.millis(1), 0, 0).isLeft)
    assert(
      FixationTransportConfig.of[Px](1, 1, Span.millis(1), 0, .1, maximumIterations = 0).isLeft
    )
  }

  test(
    "transport retains exact relative microseconds beyond double integer precision and checks identities"
  ) {
    val origin       = 9007199254740992L
    def at(us: Long) = get(
      Scanpath.of(
        frame,
        clock,
        IArray(
          get(
            Event.Fixation.of(
              get(Interval.of(clock, Instant.micros(us), Instant.micros(us + 1))),
              Pt[Px](0, 0),
              0,
              DispersionMethod.RmsRadius,
              1
            )
          )
        )
      )
    )
    val c = get(FixationTransportConfig.of[Px](1, 1, Span.micros(1), 1, .1))
    assertEqualsDouble(
      get(FixationTransport.compare(at(origin), at(origin + 1), c)).rootCost,
      1,
      AnalyticTolerance
    )
    val otherClock = ClockId("foreign")
    val wrong      = get(
      Scanpath.of(
        frame,
        otherClock,
        IArray(
          get(
            Event.Fixation.of(
              get(Interval.of(otherClock, Instant.millis(0), Instant.millis(1))),
              Pt[Px](0, 0),
              0,
              DispersionMethod.RmsRadius,
              1
            )
          )
        )
      )
    )
    assert(FixationTransport.compare(base, wrong, c).isLeft)
    val otherFrame = get(Frame.screen("foreign", 800, 400))
    val foreign    = get(Scanpath.of(otherFrame, clock, base.fixations))
    assert(FixationTransport.compare(base, foreign, c).isLeft)
    val tiny =
      get(FixationTransportConfig.of[Px](java.lang.Double.MIN_VALUE, 1, Span.millis(1), 0, .1))
    assert(FixationTransport.compare(base, translated, tiny).isLeft)
    assert(typeCheckErrors("""
      def run(p: eyes4s.core.Scanpath[eyes4s.kernel.Unit2D.Px],c: eyes4s.compare.FixationTransportConfig[eyes4s.kernel.Unit2D.Deg]) =
        eyes4s.compare.FixationTransport.compare(p,p,c)
    """).nonEmpty)
  }

  test(
    "explicit direct-default grids span both paths' onsets and distance overflow stays located"
  ) {
    // eyesim's direct default grid runs from 0 to the later of both paths' final onsets by 20.
    def grid(last: Long) = (0L to last by 20L).map(Instant.millis).toVector
    def run(a: Scanpath[Px], b: Scanpath[Px], times: Vector[Instant]) = get(
      FixationOverlap.compare(
        FixationTrajectory.fromScanpath(a),
        FixationTrajectory.fromScanpath(b),
        clock,
        times,
        get(OverlapThreshold.of[Px](60)),
        FixationGroundDistance.Euclidean,
        TrajectoryEndpoint.HoldLastOnset,
        MissingOverlapPolicy.CountAsNonOverlap
      )
    )
    assertEquals(run(base, translated, grid(200)).overlaps, 11)
    val longer     = path(ScanpathReference.paths(1).copy(onset = Vector(0, 100, 400)))
    val asymmetric = run(base, longer, grid(400))
    assertEquals(asymmetric.overlaps, 11); assertEquals(asymmetric.requested, 21)
    assertEqualsDouble(get(asymmetric.similarity).value, 11.0 / 21, AnalyticTolerance)
    // The grid is symmetric in its arguments, so swapping them cannot change the score.
    assertEquals(run(longer, base, grid(400)).overlaps, 11)
    val overflow = run(
      singleton(Double.MaxValue, 0, 0),
      singleton(-Double.MaxValue, 0, 0),
      Vector(Instant.millis(0))
    )
    assert(overflow.rows.head.distance.isLeft); assertEquals(overflow.contributing, 0)
  }
