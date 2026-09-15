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

class WindowOccupancySuite extends munit.FunSuite:
  private def get[E, A](x: Either[E, A]): A = x.fold(e => fail(s"$e"), identity)
  private val NumericTolerance              = 1e-15
  private val clock                         = ClockId("observed")
  private val frame                         = get(Frame.screen("screen", 2, 2))
  private def interval(a: Long, b: Long, c: ClockId = clock) = get(
    Interval.of(c, Instant.micros(a), Instant.micros(b))
  )
  private val path = get(
    Scanpath.of(
      frame,
      clock,
      IArray(
        get(Event.Fixation.withoutDispersion(interval(0, 10), Pt[Px](0.5, 0.5), 1)),
        get(Event.Fixation.withoutDispersion(interval(10, 30), Pt[Px](1.5, 0.5), 2)),
        get(Event.Fixation.withoutDispersion(interval(40, 60), Pt[Px](0.5, 0.5), 2))
      )
    )
  )
  private val coverage = get(
    ObservedCoverage.of(clock, Vector(interval(0, 20), interval(25, 50)))
  )
  test(
    "clipping intersects both the window and observed support, retaining a complete ledger"
  ) {
    val result =
      get(WindowOccupancy(path, interval(5, 45), coverage, FixationBoundary.ClipDuration))
    assertEquals(result.fixationTimes.map(_.retainedMicros), Vector(5L, 15L, 5L))
    assertEquals(result.retainedMicros, 25L)
    assertEquals(result.observedMicros, 35L)
    assertEquals(result.missingMicros, 5L)
    assertEqualsDouble(result.measure.total, 25e-6, NumericTolerance)
    assertEquals(
      result.fixationTimes.map(_.originalMicros),
      Vector(BigInt(10), BigInt(20), BigInt(20))
    )
  }
  test(
    "whole-fixation policy excludes straddlers including a fixation crossing a coverage gap"
  ) {
    val result =
      get(WindowOccupancy(path, interval(5, 45), coverage, FixationBoundary.FullyContained))
    assertEquals(result.excludedFixations, Vector(0, 1, 2))
    assertEquals(result.measure.size, 0)
    assertEquals(result.retainedMicros, 0L)
  }
  test("half-open boundaries never duplicate duration between adjacent windows") {
    val observed = get(ObservedCoverage.of(clock, Vector(interval(0, 60))))
    val pieces   = Vector((0L, 10L), (10L, 30L), (30L, 40L), (40L, 60L)).map { case (a, b) =>
      get(WindowOccupancy(path, interval(a, b), observed, FixationBoundary.ClipDuration))
    }
    assertEquals(pieces.map(_.retainedMicros), Vector(10L, 20L, 0L, 20L))
    assertEquals(pieces.map(_.retainedMicros).sum, path.dwellTotal.toMicros)
    assertEquals(pieces(1).excludedFixations, Vector(0, 2))
  }
  test("unobserved and empty-fixation windows are distinct, and neither invents samples") {
    val gap =
      get(WindowOccupancy(path, interval(30, 40), coverage, FixationBoundary.ClipDuration))
    val absent =
      get(WindowOccupancy(path, interval(60, 70), coverage, FixationBoundary.ClipDuration))
    assertEquals(gap.observedMicros, 10L)
    assertEquals(absent.missingMicros, 10L)
    assertEquals(gap.measure.size, 0)
    assertEquals(absent.fixationTimes.size, path.n)
  }
  test(
    "invalid clock, overlapping coverage, empty windows and overflowing widths are rejected"
  ) {
    assert(ObservedCoverage.of(clock, Vector(interval(0, 20), interval(10, 30))).isLeft)
    assert(ObservedCoverage.of(clock, Vector(interval(0, 0))).isLeft)
    assert(
      WindowOccupancy(
        path,
        interval(0, 10, ClockId("wrong")),
        coverage,
        FixationBoundary.ClipDuration
      ).isLeft
    )
    assert(
      WindowOccupancy(path, interval(0, 0), coverage, FixationBoundary.ClipDuration).isLeft
    )
    assert(
      WindowOccupancy(
        path,
        interval(Long.MinValue, Long.MaxValue),
        coverage,
        FixationBoundary.ClipDuration
      ).isLeft
    )
  }
