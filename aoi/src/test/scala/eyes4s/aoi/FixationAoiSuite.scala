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

package eyes4s.aoi

import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px

import scala.compiletime.testing.typeCheckErrors

class FixationAoiSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val frame                             = get(Frame.screen("fixation-aoi", 100, 100))
  private val clock                             = ClockId("fixation-aoi")
  private def area(id: String, lo: Double, hi: Double): Aoi[Px] =
    get(Aoi.of(id, id, frame, get(Region.rect(Pt[Px](lo, 0), Pt[Px](hi, 60)))))
  private val areas = get(AoiSet.of(Vector(area("left", 0, 50), area("right", 50, 100))))
  private def fixation(
      start: Long,
      duration: Long,
      x: Double,
      y: Double = 10
  ): Event.Fixation[Px] =
    get(
      Event.Fixation.withoutDispersion(
        get(Interval.of(clock, Instant.micros(start), Instant.micros(start + duration))),
        Pt[Px](x, y),
        1
      )
    )
  private def path(fixations: Event.Fixation[Px]*): Scanpath[Px] =
    get(Scanpath.of(frame, clock, IArray.from(fixations)))

  test("visits survive saccades and blinks; only fixation time contributes dwell") {
    val first                     = fixation(100, 20, 10)
    val second                    = fixation(170, 30, 20)
    val third                     = fixation(300, 40, 30)
    val events: Vector[Event[Px]] = Vector(
      first,
      get(
        Event.Saccade.of(
          get(Interval.of(clock, Instant.micros(120), Instant.micros(170))),
          first.centre,
          second.centre,
          None
        )
      ),
      second,
      get(
        Event.Blink.of[Px](get(Interval.of(clock, Instant.micros(200), Instant.micros(300))))
      ),
      third
    )
    val assignment = get(
      areas.assign(get(Scanpath.fromEvents(frame, clock, events)), MembershipPolicy.Multiple)
    )
    val left = assignment.measure.areas.head
    assertEquals(assignment.size, 3)
    assertEquals(left.dwell, Span.micros(90))
    assertEquals(left.fixationCount, 3)
    assertEquals(left.visitCount, 1)
    assertEquals(left.entryCount, 1)
    assertEquals(left.firstEntryLatency, Some(Span.zero))
    assertEquals(left.dwellProportion, Some(1.0))
    assertEquals(assignment.report.interFixationTime, Span.micros(150))
    assertEquals(assignment.report.accountedTime, Span.micros(240))
    assert(assignment.accountingHolds)
    assertEquals(assignment.get(-1), None)
    assertEquals(assignment.get(3), None)
  }

  test("outside fixations interrupt visits and first entry is relative to the scanpath") {
    val scanpath = path(
      fixation(1000, 10, 90, 90), // background
      fixation(1020, 20, 10),
      fixation(1100, 30, 20),
      fixation(1200, 40, 70),
      fixation(1300, 50, 10),
      fixation(1400, 60, 110), // off surface
      fixation(1500, 70, 10)
    )
    val assignment = get(areas.assign(scanpath, MembershipPolicy.ExclusiveByPriority))
    val measured   = assignment.measure
    val left       = measured.areas.head
    assertEquals(left.dwell, Span.micros(170))
    assertEquals(left.fixationCount, 4)
    assertEquals(left.visitCount, 3)
    assertEquals(left.entryCount, 3)
    assertEquals(left.firstEntryLatency, Some(Span.micros(20)))
    assertEqualsDouble(left.dwellProportion.get, 170.0 / 220.0, 0.0)
    assertEquals(assignment.get(0), Some(FixationMembership.Background))
    assertEquals(assignment.get(5), Some(FixationMembership.OffSurface))
    assertEquals(assignment.report.backgroundTime, Span.micros(10))
    assertEquals(assignment.report.offSurfaceTime, Span.micros(60))
    assertEquals(assignment.report.fixationTime, Span.micros(280))
    assertEquals(assignment.report.interFixationTime, Span.micros(290))
    assert(assignment.accountingHolds)
    assertEquals(
      measured.transitions.map(t => (t.from.value, t.to.value, t.count)),
      Vector(("left", "right", 1), ("right", "left", 1))
    )
  }

  test(
    "overlap policies preserve ordered membership and account for every duplicate duration"
  ) {
    val overlap =
      get(AoiSet.of(Vector(area("wide", 0, 80), area("middle", 20, 70), area("small", 40, 60))))
    val scanpath = path(fixation(0, 10, 50), fixation(100, 20, 60))
    val multiple = get(overlap.assign(scanpath, MembershipPolicy.Multiple))
    assertEquals(
      multiple.toVector.collect { case FixationMembership.Areas(ids) => ids.map(_.value) },
      Vector(Vector("wide", "middle", "small"), Vector("wide", "middle"))
    )
    assertEquals(multiple.report.aoiUnionTime, Span.micros(30))
    assertEquals(multiple.report.duplicatedAoiTime, Span.micros(40))
    assertEquals(multiple.measure.areas.map(_.dwell.toMicros).sum, 70L)
    assertEquals(multiple.measure.areas.map(_.visitCount), Vector(1, 1, 1))
    val exclusive = get(overlap.assign(scanpath, MembershipPolicy.ExclusiveByPriority))
    assertEquals(exclusive.measure.areas.map(_.dwell.toMicros), Vector(30L, 0L, 0L))
    val smallest = get(
      overlap.assign(
        scanpath,
        MembershipPolicy.SmallestContaining(get(AoiResolution.of(100, 100)))
      )
    )
    assertEquals(smallest.measure.areas.map(_.dwell.toMicros), Vector(0L, 20L, 10L))
    assertEquals(smallest.report.duplicatedAoiTime, Span.zero)
    assertEquals(
      overlap.assign(scanpath, MembershipPolicy.RejectOverlap),
      Left(AoiError.ObservedFixationOverlap(frame.id, 0, 50, 10, overlap.ids))
    )
    val error = overlap.assign(scanpath, MembershipPolicy.RejectOverlap).left.toOption.get
    assert(error.message.contains("fixation[0]"))
    assert(error.message.contains("fixation-aoi"))
  }

  test("an all-off-surface path retains every fixation and has no analysable denominator") {
    val assignment = get(
      areas.assign(
        path(fixation(10, 20, 110), fixation(50, 30, -10)),
        MembershipPolicy.Multiple
      )
    )
    assertEquals(assignment.toVector, Vector.fill(2)(FixationMembership.OffSurface))
    assertEquals(assignment.report.offSurfaceTime, Span.micros(50))
    assertEquals(assignment.report.interFixationTime, Span.micros(20))
    assert(assignment.accountingHolds)
    assignment.measure.areas.foreach { metric =>
      assertEquals(metric.dwell, Span.zero)
      assertEquals(metric.dwellProportion, None)
      assertEquals(metric.firstEntryLatency, None)
      assertEquals(metric.fixationCount, 0)
      assertEquals(metric.entryCount, 0)
      assertEquals(metric.visitCount, 0)
    }
  }

  test("smallest-area ties preserve set priority and a singleton starts one visit") {
    val tied       = get(AoiSet.of(Vector(area("first", 0, 50), area("second", 0, 50))))
    val assignment = get(
      tied.assign(
        path(fixation(20, 10, 10)),
        MembershipPolicy.SmallestContaining(get(AoiResolution.of(10, 10)))
      )
    )
    assertEquals(assignment.measure.areas.map(_.visitCount), Vector(1, 0))
    assertEquals(assignment.report.interFixationTime, Span.zero)
    assert(assignment.accountingHolds)
    val member = assignment.toVector.head.asInstanceOf[FixationMembership.Areas]
    val second = get(tied.assign(assignment.scanpath, assignment.policy)).toVector.head
    assertEquals(member, second)
    assertEquals(member.hashCode, second.hashCode)
    assert(member.toString.contains("first"))
  }

  test("assignment rejects nominal and structural frame conflicts") {
    val scanpath = path(fixation(0, 10, 10))
    Vector(get(Frame.screen("other", 100, 100)), get(Frame.screen("fixation-aoi", 120, 100)))
      .foreach { other =>
        val wrong = get(Scanpath.of(other, clock, scanpath.fixations))
        assert(areas.assign(wrong, MembershipPolicy.Multiple).left.exists {
          case AoiError.FrameConflict(_) => true
          case _                         => false
        })
      }
  }

  test("membership and metrics cannot bypass assignment with constructors or Product mirrors") {
    assert(typeCheckErrors("""
      import eyes4s.aoi.*
      new FixationMembership.Areas(Vector.empty)
    """).nonEmpty)
    assert(typeCheckErrors("""
      import eyes4s.aoi.*
      import scala.deriving.Mirror
      summon[Mirror.ProductOf[FixationMembership.Areas]]
    """).nonEmpty)
    assert(typeCheckErrors("""
      import eyes4s.aoi.*
      import scala.deriving.Mirror
      summon[Mirror.ProductOf[FixationAoiMetric]]
    """).nonEmpty)
  }
