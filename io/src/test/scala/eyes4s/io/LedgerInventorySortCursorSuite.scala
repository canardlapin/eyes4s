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

package eyes4s.io

import eyes4s.design.SampleQuantum
import eyes4s.plan.*

class LedgerInventorySortCursorSuite extends munit.FunSuite:
  import LedgerExecutionLimitFixtures.*
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private def rows(keys: Vector[(String, String, String)]): Vector[TrialInventory.Row] =
    keys.zipWithIndex.map { case ((p, f, t), index) =>
      TrialInventory.Row(
        index + 2,
        get(TrialIdentity.of(p, f, t, get(TrialOccurrence.of(index % 2 + 1)))),
        Some(s"item-$index"),
        Attributes.empty
      )
    }
  private def start(
      values: Vector[TrialInventory.Row],
      envelope: LedgerExecutionLimits,
      before: Long
  ) =
    get(LedgerInventorySortCursor.start(values, envelope, before))
  private def drain(
      initial: LedgerInventorySortCursor,
      budget: Int
  ): Either[LedgerResourceError, (Vector[Int], Long, Long, Long)] =
    val quantum = get(SampleQuantum.of(budget))
    @annotation.tailrec
    def loop(
        cursor: LedgerInventorySortCursor,
        units: Long,
        peak: Long
    ): Either[LedgerResourceError, (Vector[Int], Long, Long, Long)] =
      cursor.advance(quantum) match
        case Left(error)                                                  => Left(error)
        case Right(LedgerInventorySortStep.More(stage, used, kept, next)) =>
          assert(used > 0 && used <= budget)
          assertEquals(stage, cursor.stage)
          assertEquals(
            BigInt(kept),
            BigInt(initial.scope.before) + next.order.size + next.output.size
          )
          loop(next, units + used, math.max(peak, kept))
        case Right(LedgerInventorySortStep.Done(used, kept, indices)) =>
          assert(used > 0 && used <= budget)
          assertEquals(BigInt(kept), BigInt(initial.scope.before) + indices.size)
          Right((indices, units + used, math.max(peak, kept), kept))
    loop(initial, 0L, initial.scope.before)

  private def oracle(values: Vector[TrialInventory.Row]): Vector[Int] =
    values.indices.toVector.sortBy(i => TrialInventory.label(values(i).identity))

  test("equal-label runs preserve canonical inventory groups and first-record presentation") {
    val values = rows(
      Vector(
        ("p2", "f", "t"),
        ("p1", "f2", "t"),
        ("p1", "f", "t"),
        ("p2", "f", "t"),
        ("p1", "f", "t"),
        ("p1", "f2", "t"),
        ("a", "bc", "d"),
        ("ab", "c", "d"),
        ("a", "bc", "d")
      )
    )
    val sorted = get(drain(start(values, limits(), 0L), 1))._1
    assertEquals(sorted, oracle(values))
    val contiguous =
      sorted.map(values.apply).foldLeft(Vector.empty[Vector[TrialInventory.Row]]) {
        (groups, row) =>
          if groups.lastOption.exists(g =>
              TrialInventory.label(g.head.identity) == TrialInventory.label(row.identity)
            )
          then groups.updated(groups.size - 1, groups.last :+ row)
          else groups :+ Vector(row)
      }
    // This is the actual synchronous grouping/presentation contract; the
    // preparatory sort must not substitute lexicographic group presentation.
    val expected = values
      .groupBy(r => TrialInventory.label(r.identity))
      .values
      .toVector
      .sortBy(_.head.number)
    assertEquals(contiguous.sortBy(_.head.number), expected)
    assertEquals(contiguous.map(_.map(_.number)).flatten.sorted, values.map(_.number))
  }

  test("odd/even runs, ties and UTF16 labels have quantum-independent work and storage") {
    val keys = Vector(
      ("a", "z", "t"),
      ("b", "f", "t"),
      ("a", "f", "x"),
      ("a", "f", "t"),
      ("a\u0000", "f", "t"),
      ("\u03b1", "f", "t"),
      ("\ud83d\ude42", "f", "t"),
      ("\ud800", "f", "t")
    )
    Vector(0, 1, 2, 3, 5, 6, 7, 16, 31, 64).foreach { count =>
      val values   = rows(Vector.tabulate(count)(i => keys((i * 7 + count) % keys.size)))
      val initial  = start(values, limits(), 11L)
      val expected = get(drain(initial, 1))
      assertEquals(expected._1, oracle(values))
      assertEquals(expected._3, 11L + (if count <= 1 then count.toLong else count.toLong * 2))
      assertEquals(expected._4, 11L + count)
      Vector(7, 1024).foreach(b => assertEquals(drain(initial, b), Right(expected)))
    }
  }

  test("tiny runs pin seed, copy, comparison, pass and terminal operation counts") {
    val empty = get(drain(start(rows(Vector.empty), limits(), 0L), 1))
    assertEquals(empty, (Vector.empty[Int], 1L, 0L, 0L))
    val one = get(drain(start(rows(Vector(("a", "f", "t"))), limits(), 0L), 1))
    assertEquals(one, (Vector(0), 2L, 1L, 1L))
    val two = get(drain(start(rows(Vector(("a", "f", "t"), ("b", "f", "t"))), limits(), 0L), 1))
    assertEquals(two, (Vector(0, 1), 10L, 4L, 2L))
    val tied = get(drain(start(rows(Vector.fill(2)(("a", "f", "t"))), limits(), 0L), 1))
    assertEquals(tied, (Vector(0, 1), 15L, 4L, 2L))
  }

  test("long common prefixes yield during comparison and preserve immutable continuations") {
    val prefix   = "q" * 20000
    val values   = rows(Vector((prefix + "b", "f", "t"), (prefix + "a", "f", "t")))
    val initial  = start(values, limits(), 0L)
    val expected = (Vector(1, 0), 20010L, 4L, 2L)
    Vector(1, 7, 1024).foreach(b => assertEquals(drain(initial, b), Right(expected)))
    val q = get(SampleQuantum.of(1))
    @annotation.tailrec
    def comparison(cursor: LedgerInventorySortCursor): LedgerInventorySortCursor =
      if cursor.stage == LedgerInventorySortStage.Compare then cursor
      else
        get(cursor.advance(q)) match
          case LedgerInventorySortStep.More(_, _, _, next) => comparison(next)
          case _ => fail("sort completed before comparing")
    val snapshot = comparison(initial)
    assertEquals(snapshot.advance(q), snapshot.advance(q))
    assertEquals(drain(snapshot, 7), drain(snapshot, 1))
    assertEquals(initial.order, Vector.empty)
    assertEquals(initial.output, Vector.empty)
  }

  test("combined buffers refuse before append at the caller cap, including Long overflow") {
    val values = rows(Vector("a", "b", "c", "d", "e").map(p => (p, "f", "t")))
    val before = 17L
    val cap    = before + 10L
    Vector(1, 7, 1024).foreach { b =>
      val result = get(
        drain(start(values, limits(LedgerResource.RetainedEvidenceUnits -> cap), before), b)
      )
      assertEquals(result._3, cap)
      assertEquals(result._4, before + 5L)
      assert(
        drain(
          start(values, limits(LedgerResource.RetainedEvidenceUnits -> (cap + 1)), before),
          b
        ).isRight
      )
      assertEquals(
        drain(
          start(values, limits(LedgerResource.RetainedEvidenceUnits -> (cap - 1)), before),
          b
        ),
        Left(
          LedgerResourceError.Exceeded(
            LedgerResource.RetainedEvidenceUnits,
            cap - 1,
            BigInt(cap),
            LedgerResourceLocation(LedgerResourceSource.Inventory, Some(6))
          )
        )
      )
      assertEquals(
        drain(
          start(
            values,
            limits(LedgerResource.RetainedEvidenceUnits -> Long.MaxValue),
            Long.MaxValue - 9
          ),
          b
        ),
        Left(
          LedgerResourceError.Exceeded(
            LedgerResource.RetainedEvidenceUnits,
            Long.MaxValue,
            BigInt(Long.MaxValue) + 1,
            LedgerResourceLocation(LedgerResourceSource.Inventory, Some(6))
          )
        )
      )
    }
    val initial = start(values, limits(LedgerResource.RetainedEvidenceUnits -> before), before)
    assert(initial.advance(get(SampleQuantum.of(1))).isLeft)
    assertEquals(initial.order, Vector.empty)
  }

  test("all three label fields are checked during seeding even for a singleton") {
    val huge = "x" * 20
    val keys = Vector((huge, "f", "t"), ("p", huge, "t"), ("p", "f", huge))
    keys.foreach { key =>
      assertEquals(
        drain(start(rows(Vector(key)), limits(LedgerResource.FieldCodeUnits -> 10L), 0L), 7),
        Left(
          LedgerResourceError.Exceeded(
            LedgerResource.FieldCodeUnits,
            10L,
            BigInt(20),
            LedgerResourceLocation(LedgerResourceSource.Inventory, Some(2))
          )
        )
      )
    }
  }
