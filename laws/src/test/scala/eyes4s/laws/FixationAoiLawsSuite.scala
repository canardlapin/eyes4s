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

import eyes4s.aoi.*
import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px

import org.scalacheck.Gen

class FixationAoiLawsSuite extends munit.DisciplineSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val frame               = get(Frame.screen("fixation-aoi-laws", 100, 100))
  private val clock               = ClockId("fixation-aoi-laws")
  private val proportionTolerance = Tolerance(absolute = 1e-15, relative = 1e-14)
  private def rectangle(id: String, lo: Double, hi: Double): Aoi[Px] =
    get(Aoi.of(id, id, frame, get(Region.rect(Pt[Px](lo, 0), Pt[Px](hi, 60)))))
  private val overlapping = get(
    AoiSet.of(
      Vector(
        rectangle("wide", 0, 80),
        rectangle("middle", 20, 70),
        rectangle("small", 40, 60)
      )
    )
  )
  private val disjoint = get(
    AoiSet.of(Vector(rectangle("left", 0, 50), rectangle("right", 50, 100)))
  )
  private val positions = Vector[Pt[Px]](
    Pt(10, 10),
    Pt(30, 10),
    Pt(50, 10),
    Pt(60, 10),
    Pt(90, 90),
    Pt(110, 10),
    Pt(-10, 10)
  )

  private def scanpath(
      points: Vector[Pt[Px]],
      durations: Vector[Long],
      gaps: Vector[Long]
  ): Scanpath[Px] =
    var onset     = 1000L
    val fixations = points.indices.map { i =>
      val end      = onset + durations(i)
      val fixation = get(
        Event.Fixation.withoutDispersion(
          get(Interval.of(clock, Instant.micros(onset), Instant.micros(end))),
          points(i),
          1
        )
      )
      onset = end + gaps.lift(i).getOrElse(0L)
      fixation
    }
    get(Scanpath.of(frame, clock, IArray.from(fixations)))

  private val assignments = for
    points <- Gen.frequency(
      1 -> Gen.const(Vector[Pt[Px]](Pt(10, 10), Pt(20, 10), Pt(10, 10))),
      4 -> Gen.nonEmptyListOf(Gen.oneOf(positions)).map(_.take(30).toVector)
    )
    durations <- Gen.listOfN(points.size, Gen.choose(1L, 10000L)).map(_.toVector)
    gaps      <- Gen.listOfN(points.size - 1, Gen.choose(0L, 10000L)).map(_.toVector)
    policy    <- Gen.oneOf(
      MembershipPolicy.Multiple,
      MembershipPolicy.ExclusiveByPriority,
      MembershipPolicy.SmallestContaining(get(AoiResolution.of(10, 10))),
      MembershipPolicy.RejectOverlap
    )
  yield get(
    (if policy == MembershipPolicy.RejectOverlap then disjoint else overlapping)
      .assign(scanpath(points, durations, gaps), policy)
  )

  checkAll("fixation AOIs", FixationAoiLaws.accounting(assignments, proportionTolerance))

  test("the all-contained generator witness has one visit across positive gaps") {
    val path = scanpath(
      Vector[Pt[Px]](Pt(10, 10), Pt(20, 10), Pt(10, 10)),
      Vector(10L, 20L, 30L),
      Vector(100L, 200L)
    )
    val assignment = get(disjoint.assign(path, MembershipPolicy.RejectOverlap))
    val left       = assignment.measure.areas.head
    assertEquals(left.visitCount, 1)
    assertEquals(left.entryCount, 1)
    assertEquals(left.fixationCount, 3)
    assertEquals(left.dwell, Span.micros(60))
    assertEquals(left.dwellProportion, Some(1.0))
    assertEquals(assignment.report.interFixationTime, Span.micros(300))
  }

  test("a background fixation ends a visit while its gaps remain outside the denominator") {
    val path = scanpath(
      Vector[Pt[Px]](Pt(10, 10), Pt(90, 90), Pt(20, 10)),
      Vector(10L, 20L, 30L),
      Vector(100L, 200L)
    )
    val assignment = get(disjoint.assign(path, MembershipPolicy.Multiple))
    val left       = assignment.measure.areas.head
    assertEquals(left.visitCount, 2)
    assertEquals(left.entryCount, 2)
    assertEqualsDouble(left.dwellProportion.get, 2.0 / 3.0, 0.0)
    assertEquals(assignment.report.analysableTime, Span.micros(60))
    assertEquals(assignment.report.accountedTime, Span.micros(360))
  }
