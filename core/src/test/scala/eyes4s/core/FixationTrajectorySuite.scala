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

package eyes4s.core

import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import scala.compiletime.testing.typeCheckErrors

class FixationTrajectorySuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val frame                             = get(Frame.screen("sampling", 100, 100))
  private val clock = ClockId("seconds converted explicitly to microseconds")
  private def fix(onset: Long, duration: Long, x: Double, y: Double) = get(
    Event.Fixation.of(
      get(Interval.of(clock, Instant.micros(onset), Instant.micros(onset + duration))),
      Pt[Px](x, y),
      dispersion = 0.0,
      method = DispersionMethod.RmsRadius,
      sampleCount = 1
    )
  )
  private def path(c: SamplingReference.Case) = Scanpath.of(
    frame,
    clock,
    IArray.from(c.onsets.indices.map(i => fix(c.onsets(i), c.durations(i), c.x(i), c.y(i))))
  )
  private def points(rows: Vector[TrajectorySample[Px]]) =
    rows.map(_.location.toOption.map(l => (l.point.x, l.point.y)))

  SamplingReference.cases.filter(c => c.name == "ordered" || c.name == "singleton").foreach {
    c =>
      test(
        s"${c.name}: exact native onset policies preserve reference positions, order and missingness"
      ) {
        val trajectory = FixationTrajectory.fromScanpath(get(path(c)))
        val times      = c.queries.map(Instant.micros)
        val fast       = get(trajectory.sample(clock, times, TrajectoryEndpoint.OnsetRange))
        val slow       = get(trajectory.sample(clock, times, TrajectoryEndpoint.HoldLastOnset))
        assertEquals(fast.rows.map(_.time.toMicros), c.queries)
        assertEquals(slow.rows.map(_.time.toMicros), c.queries)
        // Both eyesim paths hold the final fixation after its onset, as HoldLastOnset does.
        c.fast.foreach(expected => assertEquals(points(slow.rows), expected))
        c.slow.foreach(expected => assertEquals(points(slow.rows), expected))
        // OnsetRange is the native alternative: identical up to the last onset, missing after it.
        val last = c.onsets.max
        points(fast.rows).zip(points(slow.rows)).zip(c.queries).foreach { case ((f, s), t) =>
          assertEquals(f, if t > last then None else s)
        }
        assertEquals(fast.frame, frame); assertEquals(fast.clock, clock)
        assert(trajectory.sample(ClockId("other"), times, TrajectoryEndpoint.OnsetRange).isLeft)
        assert(
          get(
            trajectory.sample(clock, Vector.empty, TrajectoryEndpoint.OnsetRange)
          ).rows.isEmpty
        )
      }
  }

  test("offsets do not censor gaps; onset endpoints are exact to one microsecond") {
    val c          = SamplingReference.cases.find(_.name == "ordered").get
    val trajectory = FixationTrajectory.fromScanpath(get(path(c)))
    val times      =
      Vector(99999L, 100000L, 199999L, 200000L, 499999L, 500000L, 500001L).map(Instant.micros)
    val fast = get(trajectory.sample(clock, times, TrajectoryEndpoint.OnsetRange))
    assertEquals(
      fast.rows.map(_.location.toOption.map(_.index)),
      Vector(None, Some(0), Some(0), Some(1), Some(1), Some(2), None)
    )
    val slow = get(trajectory.sample(clock, times, TrajectoryEndpoint.HoldLastOnset))
    assertEquals(slow.rows.last.location.toOption.map(_.index), Some(2))
  }

  test("empty support is cardinality preserving and duplicate onsets do not weaken Scanpath") {
    val times = Vector(Instant.micros(Long.MinValue), Instant.micros(Long.MaxValue))
    val empty = FixationTrajectory.empty(frame, clock)
    TrajectoryEndpoint.values.foreach { endpoint =>
      val result = get(empty.sample(clock, times, endpoint))
      assertEquals(result.rows.map(_.time), times)
      assert(result.rows.forall(_.location == Left(TrajectoryMissing.Empty)))
    }
    assert(path(SamplingReference.cases.find(_.name == "duplicate").get).isLeft)
    assert(path(SamplingReference.cases.find(_.name == "empty").get).isLeft)
  }

  test(
    "duration replication is exact, minimum one, bounded and never fabricates Scanpath ordering"
  ) {
    val period    = get(ReplicationPeriod.of(Span.micros(10000)))
    val durations = Vector(290000L, 1000L, 0L, 10000L, 39999L).map(Span.micros)
    assertEquals(get(DurationReplication.counts(durations, period, 35)), Vector(29, 1, 1, 1, 3))
    assert(DurationReplication.counts(durations, period, 34).isLeft)
    assertEquals(get(DurationReplication.counts(Vector.empty, period, 0)), Vector.empty)
    assert(DurationReplication.counts(Vector(Span.zero), period, 0).isLeft)
    assert(DurationReplication.counts(Vector(Span.micros(-1)), period, 10).isLeft)
    assert(DurationReplication.counts(Vector.empty, period, -1).isLeft)
    assert(ReplicationPeriod.of(Span.zero).isLeft)
    assert(ReplicationPeriod.of(Span.micros(-1)).isLeft)
    val p    = get(path(SamplingReference.cases.find(_.name == "ordered").get))
    val rows = get(DurationReplication.replicate(p, period, 9))
    assertEquals(rows.map(_.sourceIndex), Vector(0, 0, 0, 0, 0, 1, 1, 1, 2))
    assertEquals(
      rows.map(_.fixation.span.onset.toMicros),
      Vector.fill(5)(100000L) ++ Vector.fill(3)(200000L) ++ Vector(500000L)
    )
    assert(DurationReplication.replicate(p, period, 8).isLeft)
  }

  test("replication cardinality cannot overflow Long or narrow into an Int sentinel") {
    val period = get(ReplicationPeriod.of(Span.micros(1)))
    val result = DurationReplication.counts(
      Vector.fill(2)(Span.micros(Long.MaxValue)),
      period,
      Int.MaxValue
    )
    assertEquals(
      result,
      Left(ReplicationError.Cardinality(BigInt(Long.MaxValue) * 2, Int.MaxValue))
    )
    result.left.foreach(e => assert(e.message.contains("maximumRows")))
    assert(
      typeCheckErrors("""new eyes4s.core.ReplicationPeriod(eyes4s.kernel.Span.zero)""").nonEmpty
    )
  }
