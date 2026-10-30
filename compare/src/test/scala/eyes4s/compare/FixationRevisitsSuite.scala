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

package eyes4s.compare

import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import scala.compiletime.testing.typeCheckErrors

class FixationRevisitsSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(e => fail(e.toString), identity)
  private val frame                         = get(Frame.screen("revisit", 1000, 1000))
  private val clock                         = ClockId("revisit")
  private def path(points: (Double, Double)*): Scanpath[Px] =
    get(
      Scanpath.of(
        frame,
        clock,
        IArray.from(points.zipWithIndex.map { case ((x, y), i) =>
          get(
            Event.Fixation.withoutDispersion(
              get(
                Interval
                  .of(clock, Instant.micros(i.toLong * 20), Instant.micros(i.toLong * 20 + 10))
              ),
              Pt[Px](x, y),
              1
            )
          )
        })
      )
    )
  private def config(
      radius: Double = 10,
      minimum: Int = 2,
      maximum: Option[Int] = None,
      departure: RevisitDeparture = RevisitDeparture.IncomingStepAtLeastRadius,
      distance: FixationGroundDistance = FixationGroundDistance.Euclidean,
      work: Long = 50000000L
  ): RevisitConfig[Px] = get(
    RevisitConfig.of(get(Distance.px(radius)), minimum, maximum, departure, distance, work)
  )
  private def reference(points: (Double, Double, Double)*): PointMeasure[Px] =
    get(
      PointMeasure.of(
        frame,
        IArray.from(points.map(p => Pt[Px](p._1, p._2))),
        IArray.from(points.map(_._3))
      )
    )
  private def available(e: RevisitExpectation): RevisitExpectation.Available = e match
    case a: RevisitExpectation.Available => a
    case other                           => fail(s"not available: $other")
  private def summary(e: RevisitExpectedSummary): RevisitExpectedSummary.Available = e match
    case a: RevisitExpectedSummary.Available => a
    case other                               => fail(s"not available: $other")

  test("ABCA retains every row and the source configuration") {
    val p = path((0, 0), (100, 0), (100, 100), (1, 0))
    val c = config()
    val r = get(FixationRevisits.analyze(p, c))
    assert(r.scanpath eq p)
    assert(r.config eq c)
    assertEquals(r.reference, None)
    assertEquals(r.rows.map(_.index), Vector(0, 1, 2, 3))
    assertEquals(r.rows.map(_.onset), p.fixations.toVector.map(_.span.onset))
    assertEquals(r.rows.map(_.opportunity), Vector(false, false, true, true))
    assertEquals(r.rows.map(_.revisit), Vector(false, false, false, true))
    val hit = r.rows(3).matched.get
    assertEquals((hit.index, hit.lag, hit.distance.value), (0, 3, 1.0))
    assertEquals(
      (r.summary.fixationCount, r.summary.opportunities, r.summary.revisits),
      (4, 2, 1)
    )
    assertEquals(r.summary.rate, Some(0.5))
    assert(r.rows.forall(_.expectation == RevisitExpectation.NotRequested))
    assertEquals(r.summary.expectation, RevisitExpectedSummary.NotRequested)
  }

  test("singleton and too-large lag retain rows with no opportunities") {
    val r = get(FixationRevisits.analyze(path((1, 1)), config(), Some(reference())))
    assertEquals(r.summary.rate, None)
    assertEquals(r.summary.expectation, RevisitExpectedSummary.NoOpportunities)
    assertEquals(r.rows.head.eligibility, RevisitEligibility.InsufficientHistory)
    assertEquals(r.rows.head.expectation, RevisitExpectation.NotAnOpportunity)
    val p = path((0, 0), (100, 0), (0, 0))
    assertEquals(
      get(FixationRevisits.analyze(p, config(minimum = Int.MaxValue))).rows.size,
      p.n
    )
  }

  test("arrival rule removes dwell clusters and can be explicitly disabled") {
    val p      = path((0, 0), (1, 0), (2, 0))
    val strict = get(FixationRevisits.analyze(p, config()))
    assertEquals(strict.rows.last.eligibility, RevisitEligibility.IncomingStepTooShort)
    assertEquals(strict.summary.opportunities, 0)
    val loose = get(
      FixationRevisits.analyze(
        p,
        config(minimum = 1, departure = RevisitDeparture.Unrestricted)
      )
    )
    assertEquals(loose.rows.map(_.matched.map(_.lag)), Vector(None, Some(1), Some(1)))
    assertEquals(loose.summary.rate, Some(1.0))
  }

  test("arrival rule makes no claim of exit from the matched region") {
    assert(
      get(FixationRevisits.analyze(path((0, 0), (-9, 0), (9, 0)), config())).rows.last.revisit
    )
  }

  test("open match radius and inclusive departure boundary are exact") {
    val miss = get(FixationRevisits.analyze(path((0, 0), (100, 0), (10, 0)), config()))
    assert(miss.rows.last.opportunity)
    assert(!miss.rows.last.revisit)
    val hit = get(FixationRevisits.analyze(path((0, 0), (10, 0), (0, 0)), config()))
    assert(hit.rows.last.revisit)
    val p = get(
      FixationRevisits.analyze(
        path((0, 0), (10, 0), (0, 0)),
        config(),
        Some(reference((0, 0, 1)))
      )
    )
    assertEquals(available(p.rows.last.expectation).probability, 1.0)
  }

  test("most recent match and nearest eligible distance are distinct") {
    val r =
      get(FixationRevisits.analyze(path((0, 0), (100, 0), (8, 0), (100, 0), (1, 0)), config()))
    assertEquals(r.rows.last.nearestEligibleDistance.map(_.value), Some(1.0))
    val matched = r.rows.last.matched.get
    assertEquals((matched.index, matched.lag, matched.distance.value), (2, 2, 7.0))
  }

  test("maximum lag expires old matches and accepts the largest integer") {
    val p = path((0, 0), (100, 0), (200, 0), (0, 0))
    assert(!get(FixationRevisits.analyze(p, config(maximum = Some(2)))).rows.last.revisit)
    assert(get(FixationRevisits.analyze(p, config(maximum = Some(3)))).rows.last.revisit)
    assert(
      get(FixationRevisits.analyze(p, config(maximum = Some(Int.MaxValue)))).rows.last.revisit
    )
  }

  test("Manhattan and Euclidean conventions are carried and distinguish a diagonal") {
    val p = path((0, 0), (100, 0), (3, 4))
    assert(get(FixationRevisits.analyze(p, config(radius = 6))).rows.last.revisit)
    val c = config(radius = 6, distance = FixationGroundDistance.Manhattan)
    val r = get(FixationRevisits.analyze(p, c))
    assert(!r.rows.last.revisit)
    assertEquals(r.config.distance.normConvention, FixationNormConvention.AbsoluteAxisSumV1)
  }

  test("conditional reference probabilities and complete summary match an analytic example") {
    val p   = path((0, 0), (100, 0), (100, 100), (0, 0))
    val ref = reference((0, 0, 1), (100, 0, 1), (100, 100, 1), (800, 800, 1))
    val r   = get(FixationRevisits.analyze(p, config(), Some(ref)))
    assert(r.reference.contains(ref))
    val a = available(r.rows(2).expectation)
    val b = available(r.rows(3).expectation)
    assertEquals((a.matchedMass, a.eligibleMass, a.eligiblePoints), (1.0, 3.0, 3))
    assertEquals((a.probability, b.probability), (1.0 / 3.0, 2.0 / 3.0))
    val s = summary(r.summary.expectation)
    assertEquals(
      (s.expectedRevisits, s.expectedRate, s.excessRevisits, s.excessRate),
      (1.0, 0.5, 0.0, 0.0)
    )
  }

  test("reference history counts a union and removes only expiring coverage") {
    val p = path((0, 0), (0, 0), (100, 0), (200, 0), (100, 0), (0, 0))
    val r = get(
      FixationRevisits.analyze(
        p,
        config(maximum = Some(3), departure = RevisitDeparture.Unrestricted),
        Some(reference((0, 0, 1), (100, 0, 1)))
      )
    )
    assertEquals(
      r.rows.drop(2).map(row => available(row.expectation).probability),
      Vector(0.5, 0.5, 1.0, 0.5)
    )
  }

  test("weighted reference uses supplied mass and direct conditional sums") {
    val p = path((0, 0), (100, 0), (0, 0))
    val r = get(
      FixationRevisits.analyze(
        p,
        config(),
        Some(reference((0, 0, 1), (200, 0, 3), (100, 0, 1e20)))
      )
    )
    val a = available(r.rows.last.expectation)
    assertEquals((a.matchedMass, a.eligibleMass, a.probability), (1.0, 4.0, 0.25))
    val tiny = get(FixationRevisits.analyze(p, config(), Some(reference((0, 0, 1e-250)))))
    assertEquals(available(tiny.rows.last.expectation).probability, 1.0)
  }

  test("no reference, empty reference, zero mass and no departure support stay distinct") {
    val p = path((0, 0), (100, 0), (0, 0))
    for (ref, reason) <- Vector(
        reference()            -> RevisitUnavailable.EmptyReference,
        reference((0, 0, 0))   -> RevisitUnavailable.ZeroReferenceMass,
        reference((100, 0, 1)) -> RevisitUnavailable.NoDepartureSupport
      )
    do
      val r = get(FixationRevisits.analyze(p, config(), Some(ref)))
      assertEquals(r.rows.last.expectation, RevisitExpectation.Unavailable(reason))
      r.summary.expectation match
        case s: RevisitExpectedSummary.Incomplete =>
          assertEquals(s.unavailableIndices, Vector(2))
        case other => fail(s"unexpected $other")
  }

  test("one missing expectation refuses an apparently complete expected total") {
    val r = get(
      FixationRevisits.analyze(
        path((0, 0), (100, 0), (200, 0), (0, 0)),
        config(),
        Some(reference((100, 0, 1)))
      )
    )
    assertEquals(available(r.rows.last.expectation).probability, 1.0)
    r.summary.expectation match
      case s: RevisitExpectedSummary.Incomplete => assertEquals(s.unavailableIndices, Vector(2))
      case other                                => fail(s"unexpected $other")
    assertEquals(
      (r.summary.opportunities, r.summary.revisits, r.summary.rate),
      (2, 1, Some(0.5))
    )
  }

  test("configuration rejects invalid bounds and zero radius") {
    assertEquals(RevisitConfig.of(get(Distance.px(0))), Left(RevisitError.NonPositiveRadius(0)))
    assertEquals(
      RevisitConfig.of(get(Distance.px(1)), 0),
      Left(RevisitError.InvalidLagRange(0, None))
    )
    assertEquals(
      RevisitConfig.of(get(Distance.px(1)), 3, Some(2)),
      Left(RevisitError.InvalidLagRange(3, Some(2)))
    )
    assertEquals(
      RevisitConfig.of(get(Distance.px(1)), maximumWork = 0),
      Left(RevisitError.InvalidMaximumWork(0))
    )
  }

  test("work is refused before evaluation without narrowing arithmetic") {
    val p = path((0, 0), (100, 0), (0, 0))
    FixationRevisits.analyze(p, config(work = 1)) match
      case Left(RevisitError.WorkLimit(3, 0, required, 1)) => assertEquals(required, BigInt(2))
      case other                                           => fail(s"unexpected $other")
  }

  test("reference geometry and invalid points are checked even for a singleton") {
    val p     = path((0, 0))
    val other = get(Frame.screen("other", 1000, 1000))
    val wrong = get(PointMeasure.of(other, IArray(Pt[Px](0, 0)), IArray(1.0)))
    assert(FixationRevisits.analyze(p, config(), Some(wrong)).swap.toOption.exists {
      case RevisitError.Frames(_) => true
      case _                      => false
    })
    val bad = reference((Double.PositiveInfinity, 0, 1))
    assertEquals(
      FixationRevisits.analyze(p, config(), Some(bad)),
      Left(RevisitError.NonFiniteReferencePoint(0, Double.PositiveInfinity, 0))
    )
    assert(
      FixationRevisits
        .analyze(p, config(), Some(reference((0, 0, Double.MaxValue), (1, 0, Double.MaxValue))))
        .isLeft
    )
  }

  test("scaled hypot avoids intermediate squaring overflow") {
    val r = get(
      FixationRevisits.analyze(path((0, 0), (1e156, 0), (1e155, 0)), config(radius = 2e155))
    )
    assert(r.rows.last.revisit)
    assertEquals(r.rows.last.matched.get.distance.value, 1e155)
  }

  test("unrepresentable distances are named errors rather than false misses") {
    val p   = path((-1e308, 0), (1e308, 0), (0, 0))
    val ref = reference((1e308, 0, 1))
    FixationRevisits.analyze(p, config(), Some(ref)) match
      case Left(RevisitError.NonFiniteDistance(0, 0, true, value)) => assert(!value.isFinite)
      case other                                                   => fail(s"unexpected $other")
  }

  test("units and fractional lag rejection are compile-time guarantees") {
    assert(typeCheckErrors("""import eyes4s.compare.*; import eyes4s.kernel.*
      val c: Either[RevisitError, RevisitConfig[Unit2D.Px]] = RevisitConfig.of(Distance.deg(2).toOption.get)
    """).nonEmpty)
    assert(typeCheckErrors("""import eyes4s.compare.*; import eyes4s.kernel.*
      RevisitConfig.of(Distance.px(2).toOption.get, maximumLag = Some(2.9))
    """).nonEmpty)
  }

  RevisitReference.cases.foreach { c =>
    test(s"independent exact fixture: ${c.name}") {
      val p   = path(c.points*)
      val cfg = config(
        c.radius,
        c.minimum,
        c.maximum,
        if c.departure then RevisitDeparture.IncomingStepAtLeastRadius
        else RevisitDeparture.Unrestricted,
        if c.manhattan then FixationGroundDistance.Manhattan
        else FixationGroundDistance.Euclidean
      )
      val r = get(FixationRevisits.analyze(p, cfg, c.reference.map(ps => reference(ps*))))
      assertEquals(r.rows.size, c.rows.size)
      r.rows.zip(c.rows).foreach { (actual, expected) =>
        assertEquals(actual.opportunity, expected.opportunity)
        assertEquals(actual.revisit, expected.revisit)
        assertEquals(actual.matched.map(_.index), expected.matched)
        assertEquals(actual.matched.map(_.lag), expected.lag)
        assertEquals(actual.nearestEligibleDistance.map(_.value), expected.distance)
        val probability = actual.expectation match
          case a: RevisitExpectation.Available => Some(a.probability)
          case _                               => None
        assertEquals(probability, expected.expected)
      }
      assertEquals(r.summary.rate, c.rate)
      val expectedTotal = r.summary.expectation match
        case a: RevisitExpectedSummary.Available => Some(a.expectedRevisits)
        case _                                   => None
      assertEquals(expectedTotal, c.expected)
    }
  }
