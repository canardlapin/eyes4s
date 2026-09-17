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

class AoiLawsSuite extends munit.DisciplineSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val frame                             = get(Frame.screen("aoi-laws", 100, 100))
  private val clock                             = ClockId("aoi-laws")
  private val proportionTolerance               = Tolerance(absolute = 1e-15, relative = 1e-14)

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
  private val gazes: Vector[Gaze[Px]] = Vector(
    Gaze.Tracked(Pt(10, 10), None),
    Gaze.Tracked(Pt(30, 10), None),
    Gaze.Tracked(Pt(50, 10), None), // three memberships, two duplicate contributions
    Gaze.Tracked(Pt(60, 10), None), // exact half-open edge of the small AOI
    Gaze.Tracked(Pt(90, 90), None),
    Gaze.Blink(),
    Gaze.Lost(),
    Gaze.OffScreen(Pt(110, 10))
  )

  private def recording(states: Vector[Gaze[Px]], step: Long): Recording[Px] =
    get(
      Recording.of(
        frame,
        clock,
        Rate.Irregular,
        Eye.Left,
        None,
        IArray.from(states.zipWithIndex.map { (gaze, i) =>
          Sample(Instant.micros(i.toLong * step), gaze)
        })
      )
    )

  private val assignments = for
    step    <- Gen.choose(2L, 10000L)
    extras  <- Gen.listOf(Gen.oneOf(gazes))
    states  <- Gen.pick(gazes.size + extras.size, gazes ++ extras).map(_.toVector)
    support <- Gen.oneOf(
      get(TemporalSupport.fixed(Span.micros(step))),
      TemporalSupport.ForwardHold(
        get(MaximumSupportGap.atMost(Span.micros(step / 2))),
        EdgeSupport.Censored
      ),
      TemporalSupport.Voronoi(
        get(MaximumSupportGap.atMost(Span.micros(step / 2))),
        EdgeSupport.PreviousInterval
      )
    )
    policy <- Gen.oneOf(
      MembershipPolicy.Multiple,
      MembershipPolicy.ExclusiveByPriority,
      MembershipPolicy.SmallestContaining(get(AoiResolution.of(10, 10))),
      MembershipPolicy.RejectOverlap
    )
  yield get(
    (if policy == MembershipPolicy.RejectOverlap then disjoint else overlapping)
      .assign(recording(states, step), policy, support)
  )

  checkAll("static AOIs", AoiLaws.accounting(assignments, proportionTolerance))

  private val noAnalysable = get(
    overlapping.assign(
      recording(Vector(Gaze.Blink(), Gaze.Lost(), Gaze.OffScreen(Pt(110, 0))), 100),
      MembershipPolicy.Multiple,
      get(TemporalSupport.fixed(Span.micros(100)))
    )
  )
  private val noSupport = get(
    overlapping.assign(
      recording(gazes.take(1), 100),
      MembershipPolicy.Multiple,
      TemporalSupport.ForwardHold(MaximumSupportGap.Unlimited, EdgeSupport.Censored)
    )
  )
  checkAll(
    "zero analysable time",
    AoiLaws.accounting(Gen.oneOf(noAnalysable, noSupport), proportionTolerance)
  )

  test("analytic three-way overlap distinguishes replicated mass from visible union") {
    val a = get(
      overlapping.assign(
        recording(gazes, 100),
        MembershipPolicy.Multiple,
        get(TemporalSupport.fixed(Span.micros(100)))
      )
    )
    assertEquals(
      a.report,
      AoiAssignmentReport(
        Span.micros(400),
        Span.micros(100),
        Span.micros(300),
        Span.zero,
        Span.micros(400)
      )
    )
    assertEquals(a.measure.areas.map(_.dwell.toMicros), Vector(400L, 300L, 100L))
    assertEquals(
      a.measure.areas.map(_.dwellProportion),
      Vector(Some(0.8), Some(0.6), Some(0.2))
    )
  }
