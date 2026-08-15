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

import scala.compiletime.testing.typeCheckErrors

class AoiSuite extends munit.FunSuite:

  private val frame = Frame.screen("stimulus", 100, 100).toOption.get
  private val clock = ClockId("stimulus-clock")
  private val rate  = Rate.Fixed(Hz(10.0).toOption.get)

  private def rectangle(
      id: String,
      label: String,
      lo: Pt[Unit2D.Px],
      hi: Pt[Unit2D.Px],
      attributes: Map[String, String] = Map.empty
  ): Aoi[Unit2D.Px] =
    val region = Region.rect(lo, hi).toOption.get
    Aoi.of(id, label, frame, region, attributes).toOption.get

  private val a = rectangle(
    "a",
    "Left target",
    Pt(0.0, 0.0),
    Pt(70.0, 60.0),
    Map("condition" -> "left")
  )
  private val b     = rectangle("b", "Right target", Pt(40.0, 0.0), Pt(100.0, 60.0))
  private val c     = rectangle("c", "Lower target", Pt(60.0, 60.0), Pt(100.0, 100.0))
  private val areas = AoiSet.of(Vector(a, b, c)).toOption.get

  private val samples: IArray[Sample[Unit2D.Px]] = IArray(
    Sample(Instant.millis(0), Gaze.Tracked(Pt(10.0, 10.0), None)),
    Sample(Instant.millis(100), Gaze.Tracked(Pt(50.0, 10.0), None)),
    Sample(Instant.millis(200), Gaze.Tracked(Pt(70.0, 10.0), None)),
    Sample(Instant.millis(300), Gaze.Tracked(Pt(90.0, 90.0), None)),
    Sample(Instant.millis(400), Gaze.Tracked(Pt(90.0, 90.0), None)),
    Sample(Instant.millis(500), Gaze.Blink()),
    Sample(Instant.millis(600), Gaze.Lost()),
    Sample(Instant.millis(700), Gaze.OffScreen(Pt(110.0, 10.0))),
    Sample(Instant.millis(800), Gaze.Tracked(Pt(30.0, 90.0), None)),
    Sample(Instant.millis(900), Gaze.Tracked(Pt(10.0, 10.0), None)),
    Sample(Instant.millis(1000), Gaze.Tracked(Pt(70.0, 10.0), None))
  )
  private val recording = Recording
    .of(frame, clock, rate, Eye.Left, None, samples)
    .toOption
    .get

  private def id(value: String): AoiId = AoiId.of(value).toOption.get

  test("AOIs retain validated identity, full frame specification, geometry, and attributes") {
    assertEquals(a.id.value, "a")
    assertEquals(a.label, "Left target")
    assertEquals(a.frame.spec, frame.spec)
    assert(a.region.contains(Pt(10.0, 10.0)))
    assertEquals(a.attributes, Map("condition" -> "left"))
    assertEquals(areas.ids.map(_.value), Vector("a", "b", "c"))
  }

  test("multiple membership accounts for duplicated AOI mass without duplicating union time") {
    val assignment = areas.assign(recording, MembershipPolicy.Multiple).toOption.get
    val measured   = assignment.measure

    assertEquals(
      assignment.get(1) match
        case Some(SampleMembership.Areas(ids)) => ids.map(_.value)
        case _                                 => Vector.empty,
      Vector("a", "b")
    )
    assertEquals(assignment.get(5), Some(SampleMembership.Excluded(ExclusionReason.Blink)))
    assertEquals(
      assignment.get(6),
      Some(SampleMembership.Excluded(ExclusionReason.SignalLoss))
    )
    assertEquals(
      assignment.get(7),
      Some(SampleMembership.Excluded(ExclusionReason.OffSurface))
    )
    assertEquals(assignment.get(8), Some(SampleMembership.Background))
    assertEquals(assignment.get(-1), None)
    assertEquals(assignment.get(assignment.size), None)
    assertEquals(assignment.report.aoiUnionTime, Span.millis(700))
    assertEquals(assignment.report.backgroundTime, Span.millis(100))
    assertEquals(assignment.report.excludedTime, Span.millis(300))
    assertEquals(assignment.report.policyCensoredTime, Span.zero)
    assertEquals(assignment.report.duplicatedAoiTime, Span.millis(100))
    assertEquals(assignment.support.representedTime, Span.millis(1100))
    assert(assignment.accountingHolds)

    val byId = measured.areas.map(metric => metric.id.value -> metric).toMap
    assertEquals(byId("a").dwell, Span.millis(300))
    assertEquals(byId("b").dwell, Span.millis(300))
    assertEquals(byId("c").dwell, Span.millis(200))
    assertEqualsDouble(byId("a").dwellProportion.get, 0.375, 0.0)
    assertEquals(byId("b").firstEntryLatency, Some(Span.millis(100)))
    assertEquals(byId("c").firstEntryLatency, Some(Span.millis(300)))
    assertEquals(byId("a").runCount, 2)
    assertEquals(byId("b").runCount, 2)
    assertEquals(byId("c").runCount, 1)
    assertEquals(
      measured.transitions,
      Vector(AoiTransition(id("a"), id("b"), 3), AoiTransition(id("b"), id("c"), 1))
    )
  }

  test("exclusive priority allocates overlap once and preserves half-open boundaries") {
    val assignment = areas.assign(recording, MembershipPolicy.ExclusiveByPriority).toOption.get
    val measured   = assignment.measure

    assertEquals(
      assignment.get(1) match
        case Some(SampleMembership.Areas(ids)) => ids.map(_.value)
        case _                                 => Vector.empty,
      Vector("a")
    )
    assertEquals(
      assignment.get(2) match
        case Some(SampleMembership.Areas(ids)) => ids.map(_.value)
        case _                                 => Vector.empty,
      Vector("b")
    )
    assertEquals(assignment.report.duplicatedAoiTime, Span.zero)
    val dwell = measured.areas.map(metric => metric.id.value -> metric.dwell).toMap
    assertEquals(
      dwell,
      Map("a" -> Span.millis(300), "b" -> Span.millis(200), "c" -> Span.millis(200))
    )
    assertEquals(
      measured.transitions,
      Vector(AoiTransition(id("a"), id("b"), 2), AoiTransition(id("b"), id("c"), 1))
    )
  }

  test("smallest-containing uses an explicit validated area resolution") {
    val resolution = AoiResolution.of(100, 100).toOption.get
    val assignment = areas
      .assign(recording, MembershipPolicy.SmallestContaining(resolution))
      .toOption
      .get

    assertEquals(
      assignment.get(1) match
        case Some(SampleMembership.Areas(ids)) => ids.map(_.value)
        case _                                 => Vector.empty,
      Vector("b")
    )
    val dwell = assignment.measure.areas.map(metric => metric.id.value -> metric.dwell).toMap
    assertEquals(dwell("a"), Span.millis(200))
    assertEquals(dwell("b"), Span.millis(300))
    assertEquals(assignment.report.duplicatedAoiTime, Span.zero)
  }

  test("reject-overlap returns the first observed point and every competing AOI") {
    val result = areas.assign(recording, MembershipPolicy.RejectOverlap)
    assert(
      result.left.exists {
        case AoiError.ObservedOverlap(frameId, 1, 50.0, 10.0, overlapping) =>
          frameId == frame.id && overlapping.map(_.value) == Vector("a", "b")
        case _ => false
      }
    )
  }

  test("AOI sets reject duplicate identity and nominal or structural frame conflicts") {
    val duplicate = rectangle("a", "Duplicate", Pt(0.0, 60.0), Pt(40.0, 100.0))
    assertEquals(
      AoiSet.of(Vector(a, duplicate)).left.toOption,
      Some(AoiError.DuplicateId(id("a"), 0, 1))
    )

    val otherFrame = Frame.screen("other", 100, 100).toOption.get
    val other      = Aoi
      .of("other", "Other", otherFrame, Region.fromBounds(otherFrame.bounds))
      .toOption
      .get
    assert(
      AoiSet.of(Vector(a, other)).left.exists {
        case AoiError.FrameConflict(GeometryError.FrameMismatch(left, right)) =>
          left == frame.id && right == otherFrame.id
        case _ => false
      }
    )

    val corruptFrame = Frame.screen("stimulus", 120, 100).toOption.get
    val corrupt      = Aoi
      .of("corrupt", "Corrupt", corruptFrame, Region.fromBounds(corruptFrame.bounds))
      .toOption
      .get
    assert(
      AoiSet.of(Vector(a, corrupt)).left.exists {
        case AoiError.FrameConflict(GeometryError.FrameIdentityConflict(id, left, right)) =>
          id == frame.id && left == frame.spec && right == corruptFrame.spec
        case _ => false
      }
    )
  }

  test("assignment rejects a recording with conflicting frame metadata") {
    val conflictingFrame     = Frame.screen("stimulus", 120, 100).toOption.get
    val conflictingRecording = Recording
      .of(
        conflictingFrame,
        clock,
        rate,
        Eye.Left,
        None,
        IArray(Sample(Instant.epoch, Gaze.Tracked(Pt(10.0, 10.0), None)))
      )
      .toOption
      .get

    assert(
      areas.assign(conflictingRecording, MembershipPolicy.Multiple).left.exists {
        case AoiError.FrameConflict(GeometryError.FrameIdentityConflict(id, _, _)) =>
          id == frame.id
        case _ => false
      }
    )
  }

  test(
    "trusted recordings reject false Tracked state and explicit off-surface support is kept"
  ) {
    val falseTracked = Recording.of(
      frame,
      clock,
      rate,
      Eye.Left,
      None,
      IArray[Sample[Unit2D.Px]](
        Sample(
          Instant.epoch,
          Gaze.Tracked(Pt[Unit2D.Px](110.0, 10.0), None)
        )
      )
    )
    assert(
      falseTracked.left.exists(_.isInstanceOf[RecordingError.TrackedOutsideFrame])
    )

    val offSurface = Recording
      .of(
        frame,
        clock,
        rate,
        Eye.Left,
        None,
        IArray[Sample[Unit2D.Px]](
          Sample(
            Instant.epoch,
            Gaze.OffScreen(Pt[Unit2D.Px](110.0, 10.0))
          )
        )
      )
      .toOption
      .get
    val assignment = areas.assign(offSurface, MembershipPolicy.Multiple).toOption.get

    assertEquals(
      assignment.get(0),
      Some(SampleMembership.Excluded(ExclusionReason.OffSurface))
    )
    assertEquals(assignment.report.excludedTime, Span.millis(100))
    assert(assignment.accountingHolds)
  }

  test("the shared support ledger agrees exactly with occupancy accounting") {
    val support   = recording.representedSupport
    val occupancy = recording.occupancy.toOption.get

    assertEquals(support.size, recording.size)
    assertEquals(support.toVector, Vector.fill(recording.size)(Span.millis(100)))
    assertEquals(support.get(-1), None)
    assertEquals(support.get(recording.size), None)
    assert(support.isNonNegative)
    assertEquals(support.assignedTime, Span.millis(1100))
    assertEquals(
      (occupancy.analysableTime + occupancy.censoredTime).toMicros,
      support.representedTime.toMicros
    )
  }

  test("constructors reject empty scalar invariants and cannot be bypassed") {
    assert(AoiId.of(" ").left.exists(_.isInstanceOf[AoiError.BlankId]))
    assert(AoiResolution.of(0, 10).left.exists(_.isInstanceOf[AoiError.NonPositiveResolution]))
    assertEquals(AoiSet.of[Unit2D.Px](Vector.empty), Left(AoiError.EmptySet))
    assert(
      Aoi
        .of("x", " ", frame, Region.fromBounds(frame.bounds))
        .left
        .exists(_.isInstanceOf[AoiError.BlankLabel])
    )

    val errors = typeCheckErrors("""
      import eyes4s.aoi.*
      import eyes4s.kernel.*
      val frame = Frame.screen("x", 10, 10).toOption.get
      val area = new Aoi[Unit2D.Px](
        AoiId.of("x").toOption.get,
        "",
        frame,
        Region.fromBounds(frame.bounds),
        Map.empty
      )
      val set = new AoiSet(frame, Vector(area))
    """)
    assert(errors.nonEmpty)
  }

end AoiSuite
