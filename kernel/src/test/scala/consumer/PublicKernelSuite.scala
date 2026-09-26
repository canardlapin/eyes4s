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

package consumer

import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.*
import cats.kernel.Order

class PublicKernelSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val NumericTolerance                  = 1e-12
  private val frame                             = get(Frame.screen("public-kernel", 4, 3))
  private val grid                              = get(Grid.over(frame, 2, 1))
  private val provenance = Provenance.raw(ContentHash.of(IArray(1.0, 3.0)))

  test("positions extents vectors and their renderers preserve units and elementary geometry") {
    val p = Pt.origin[Px]
    val v = Vec2[Px](3.0, 4.0)
    assertEquals(p + v, Pt[Px](3.0, 4.0))
    assertEquals(v + (-v), Vec2.zero[Px])
    assertEquals(v - v, Vec2.zero[Px])
    assertEquals(v * 2.0, Vec2[Px](6.0, 8.0))
    assertEquals(v.normSquared, 25.0)
    assert(v.isFinite)
    assert(!Vec2[Px](Double.NaN, 0).isFinite)
    assert(p.render.endsWith("px")); assert(v.render.endsWith("px"))
    val extent = frame.bounds.extent
    assertEquals(extent.area, 12.0); assertEquals(extent.diagonal, 5.0)
    assert(extent.render.endsWith("px")); assert(frame.bounds.render.endsWith("px"))
    assert(frame.render.contains("public-kernel"));
    assert(grid.render.contains("public-kernel"))
    assertEquals(summon[UnitLabel[Mm]].symbol, "mm")
  }

  test("checked physical quantities expose conversions ordering and operand-bearing failures") {
    val length = get(Length.mm(1500))
    assertEquals(length.toCm, 150.0); assertEquals(length.toM, 1.5)
    assert(summon[Order[Length]].gt(length, Length.zero))
    val sigma = get(Sigma.norm(0.25))
    assert(sigma.render.contains("0.25"));
    assert(summon[Order[Sigma[Norm]]].lt(sigma, get(Sigma.norm(0.5))))
    assert(Sigma.norm(0).isLeft); assert(Sigma.norm(Double.NaN).isLeft)
    val distance = get(Distance.px(5))
    assert(get(Distance.px(2)) < distance)
    assert(summon[Order[Distance[Px]]].lt(get(Distance.px(0)), distance))
    assertEquals(Distance.zero.value, 0.0); assert(distance.render.endsWith("px"))
    val speed = get(Velocity.perSecond[Px](5))
    assert(summon[Order[Velocity[Px]]].gt(speed, Velocity.zero[Px]))
    assert(speed.render.endsWith("px/s"))
    assert(summon[Order[Hz]].lt(get(Hz(100)), get(Hz(1000))))
    assert(summon[Order[ContentHash]].compare(provenance.digest, provenance.digest) == 0)
  }

  test("exact time conveniences preserve half-open extents and checked accounting overflow") {
    val clock    = ClockId("public-clock")
    val interval = get(Interval.lasting(clock, Instant.seconds(2), Span.millis(10)))
    assertEquals(interval.onset.toSeconds, 2.0)
    assertEquals(interval.shift(Span.millis(3)).onset, Instant.millis(2003))
    assert(Interval.lasting(clock, Instant.epoch, Span.micros(-1)).isLeft)
    val window = get(Window.lasting(Span.millis(10))).shift(Span.millis(2))
    assertEquals(window.from, Span.millis(2)); assert(window.render.contains("relative"))
    assert(Window.lasting(Span.micros(-1)).isLeft)
    assertEquals((-Span.millis(2)).toMicros, -2000L)
    assertEquals((Span.millis(2) * 1.5).toMicros, 3000L)
    val nonnegative = get(NonNegativeSpan.of(Span.millis(2)))
    val positive    = get(PositiveSpan.of(Span.millis(3)))
    assertEquals(nonnegative.span, Span.millis(2)); assertEquals(nonnegative.toMillis, 2.0)
    assertEquals(positive.span, Span.millis(3)); assertEquals(positive.toMillis, 3.0)
    assert(nonnegative.render.contains("2")); assert(positive.render.contains("3"))
    assertEquals(NonNegativeSpan.zero.toMicros, 0L)
    val maximum = get(NonNegativeLong.of(Long.MaxValue))
    val one     = get(NonNegativeLong.of(1))
    assertEquals(get(one.plus(one)).toLong, 2L)
    assert(maximum.plus(one).left.exists(_.message.contains(Long.MaxValue.toString)))
    assertEquals(get(Sync.identity(clock)(interval)), interval)
  }

  test("planned and observed timeline mapping preserves clocks order and equality hashing") {
    val clock    = ClockId("messages")
    val marks    = Vector(Mark(Instant.millis(2), "b"), Mark(Instant.millis(1), "a"))
    val timeline = get(Timeline.of(clock, marks))
    assert(timeline.nonEmpty); assert(!timeline.isEmpty)
    assert(get(Timeline.empty[String](clock)).isEmpty)
    assert(timeline.toString.contains("messages"))
    assertEquals(timeline.hashCode, get(Timeline.of(clock, marks.reverse)).hashCode)
    val planned  = get(PlannedTimeline.of(clock, marks)).map(_.toUpperCase)
    val observed = get(ObservedTimeline.of(clock, marks)).map(_.toUpperCase)
    assertEquals(planned.marks.map(_.value), Vector("A", "B"))
    assertEquals(observed.marks, planned.marks)
    assertEquals(planned.hashCode, PlannedTimeline.from(timeline.map(_.toUpperCase)).hashCode)
    assertEquals(observed.hashCode, ObservedTimeline.from(timeline.map(_.toUpperCase)).hashCode)
    assert(Timeline.of(ClockId(" "), marks).left.exists(_.message.contains("clock=' '")))
  }

  test("angle algebra matrix composition and inverse warps agree on independent landmarks") {
    val quarter = Angle.degrees(90)
    assertEqualsDouble((quarter + quarter).toDegrees, 180.0, NumericTolerance)
    assertEqualsDouble((quarter - quarter).toDegrees, Angle.zero.toDegrees, NumericTolerance)
    assert(quarter.render.contains("90"))
    val rotation = Mat3.rotation(quarter)
    assertEquals(Mat3.identity * rotation, rotation)
    val point = rotation.applyTo[Px, Px](Pt[Px](1, 0)).get
    assertEqualsDouble(point.x, 0.0, NumericTolerance);
    assertEqualsDouble(point.y, 1.0, NumericTolerance)
    val id = Warp.id(frame)
    assertEquals(id.inverse.flatMap(_(Pt[Px](1, 2))), Some(Pt[Px](1, 2)))
    assert(id.render.contains(frame.id.name))
    val homography = Warp.homography(frame, frame, Mat3.translation(2, 3))
    assertEquals(homography.inverse.flatMap(_(Pt[Px](3, 5))), Some(Pt[Px](1, 2)))
    assert(homography.render.contains(frame.id.name))
  }

  test("region algebra and diagnostics retain shapes with analytic empty and circular areas") {
    val rect     = get(Region.rect(Pt[Px](0, 0), Pt[Px](2, 2)))
    val circle   = get(Region.circle(Pt[Px](1, 1), 1))
    val triangle = get(Region.polygon(Vector(Pt[Px](0, 0), Pt[Px](2, 0), Pt[Px](0, 2))))
    assertEqualsDouble(circle.area(grid), math.Pi, NumericTolerance)
    assertEquals(Region.empty[Px].area(grid), 0.0)
    assert(Region.circle(Pt[Px](1, 1), -1).isLeft)
    assert(Region.circle(Pt[Px](Double.NaN, 1), 1).isLeft)
    val shapes = Vector(
      rect,
      circle,
      triangle,
      rect || circle,
      rect && circle,
      !rect,
      Region.empty[Px],
      Region.everything[Px]
    )
    assertEquals(shapes.map(_.render).distinct.size, shapes.size)
    assert(shapes.forall(_.render.nonEmpty))
  }

  test(
    "surface scaling subtraction entropy and empty point measures retain scientific meaning"
  ) {
    val intensity = get(Surface.intensity(grid, IArray(1.0, 3.0), provenance))
    val scaled    = get(intensity.scaled(2))
    assertEquals(scaled.values.toVector, Vector(2.0, 6.0))
    assert(intensity.scaled(-1).isLeft); assert(intensity.scaled(Double.NaN).isLeft)
    val mass   = get(intensity.normalised)
    val signed = get(Surface.signed(grid, IArray(1.0, -1.0), provenance))
    assertEquals(grid.signedModule.minus(signed, signed).values.toVector, Vector(0.0, 0.0))
    assert(intensity.render.startsWith("intensity(")); assert(mass.render.startsWith("mass("));
    assert(signed.render.startsWith("signed("))
    assert(mass.entropy(LogBase.Two).render.endsWith("bits"))
    assertEquals(LogBase.E.unitName, "nats")
    val empty = PointMeasure.empty(frame)
    assert(empty.isEmpty); assert(empty.render.contains("public-kernel"))
    assert(empty.normalised.isLeft)
  }

  test("synchronization evidence maps intervals only on its declared source clock") {
    val source = ClockId("source")
    val target = ClockId("target")
    val mark   = get(SyncMark.of("trigger", Instant.millis(1), Instant.millis(11)))
    assertEquals(
      mark.hashCode,
      get(SyncMark.of("trigger", Instant.millis(1), Instant.millis(11))).hashCode
    )
    val evidence =
      get(SyncEvidence.fromCommonEvents(source, target, SyncFitMode.OffsetOnly, Vector(mark)))
    val interval = get(Interval.lasting(source, Instant.millis(1), Span.millis(2)))
    assertEquals(
      get(evidence(interval)),
      get(Interval.lasting(target, Instant.millis(11), Span.millis(2)))
    )
    assert(
      evidence(get(Interval.lasting(ClockId("wrong"), Instant.epoch, Span.millis(2)))).isLeft
    )
    val moving = get(Moving.of(Vector(Moving.Segment(interval, Warp.id(frame))), Interp.Hold))
    assert(moving.render.contains("public-kernel"));
    assert(moving.render.contains("1 segments"))
  }

  test(
    "checked bounds reject overflowing extents and retain finite extreme-coordinate centres"
  ) {
    val rejected = Bounds.of[Px](-Double.MaxValue, 0, Double.MaxValue, 1)
    assert(rejected.isLeft)
    val extreme = get(Bounds.of[Px](1e308, -1e308, 1.5e308, -5e307))
    assert(extreme.extent.width.isFinite)
    assertEquals(extreme.centre, Pt[Px](1.25e308, -7.5e307))
  }
