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

package eyes4s.plan

import scala.compiletime.testing.typeCheckErrors

class StageMeterSuite extends munit.FunSuite:
  private val kind                                         = StageKind.Comparing
  private val unit                                         = CountUnit.Pairs
  private def get[A](value: Either[StageMeterError, A]): A =
    value.fold(e => fail(e.message), identity)

  test("counting is present, distinct from unknown and exact zero") {
    val counting = get(StageMeter.of(kind, unit, 0L, SegmentTotal.Counting))
    assertEquals(counting.total, SegmentTotal.Counting)
    val resolved = get(counting.advanceTo(0L, SegmentTotal.Exact(0L)))
    assertEquals(resolved.total, SegmentTotal.Exact(0L))
    assertEquals(
      get(StageMeter.of(kind, unit, 0L, SegmentTotal.Unknown)).total,
      SegmentTotal.Unknown
    )
  }

  test("negative counts and bounds fail with their operands") {
    assertEquals(
      StageMeter.of(kind, unit, -1L, SegmentTotal.Counting).left.toOption,
      Some(StageMeterError.NegativeDone(kind, unit, -1L))
    )
    Vector(SegmentTotal.Exact(-1L), SegmentTotal.AtMost(-1L)).foreach { total =>
      assertEquals(
        StageMeter.of(kind, unit, 0L, total).left.toOption,
        Some(StageMeterError.NegativeTotal(kind, unit, total))
      )
    }
  }

  test("counts cannot exceed an exact total or upper bound") {
    Vector(SegmentTotal.Exact(4L), SegmentTotal.AtMost(4L)).foreach { total =>
      assertEquals(get(StageMeter.of(kind, unit, 4L, total)).done, 4L)
      assertEquals(
        StageMeter.of(kind, unit, 5L, total).left.toOption,
        Some(StageMeterError.BeyondTotal(kind, unit, 5L, total))
      )
    }
  }

  test("a resolved total cannot contradict completed work and progress cannot regress") {
    val meter = get(StageMeter.of(kind, unit, 5L, SegmentTotal.Counting))
    assertEquals(
      meter.advanceTo(4L).left.toOption,
      Some(StageMeterError.Regressed(kind, unit, 5L, 4L))
    )
    assertEquals(
      meter.advanceTo(5L, SegmentTotal.Exact(4L)).left.toOption,
      Some(StageMeterError.BeyondTotal(kind, unit, 5L, SegmentTotal.Exact(4L)))
    )
    assertEquals(
      get(meter.advanceTo(Long.MaxValue, SegmentTotal.Exact(Long.MaxValue))).done,
      Long.MaxValue
    )
  }

  test("record units are expressible and construction cannot bypass validation") {
    assertEquals(
      get(StageMeter.of(StageKind.Estimating, CountUnit.Rows, 2L, SegmentTotal.Exact(3L))).unit,
      CountUnit.Rows
    )
    assert(typeCheckErrors("""
      new eyes4s.plan.StageMeter(eyes4s.plan.StageKind.Comparing,
        eyes4s.plan.CountUnit.Pairs, -1L, eyes4s.plan.SegmentTotal.Counting)
    """).nonEmpty)
  }
