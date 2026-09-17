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

package eyes4s.design

import munit.FunSuite

class WithinPairScheduleSuite extends FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val all                               = Relation.all[Int, Int]
  private val one                               = get(PairQuantum.of(1))

  private def drain(
      schedule: WithinPairSchedule[Int]
  ): (Vector[(Int, Int)], PairingReport[Int, Int]) =
    val pairs = Vector.newBuilder[(Int, Int)]
    @annotation.tailrec
    def loop(cursor: PairCursor[Int, Int]): PairingReport[Int, Int] =
      get(cursor.advance(one)) match
        case PairPage.More(page, work, next) =>
          assertEquals(work, 1)
          pairs ++= page.map(p => p.leftIndex -> p.rightIndex)
          loop(next)
        case PairPage.Done(page, work, report) =>
          assert(work >= 0 && work <= 1)
          pairs ++= page.map(p => p.leftIndex -> p.rightIndex)
          report
    val report = loop(schedule.start)
    pairs.result() -> report

  test("canonical undirected keeps incoming endpoints matched, including the last key") {
    val schedule =
      get(WithinPairSchedule.canonicalUndirected(Vector(9, 3, 7), all, SelfPolicy.Exclude))
    val (pairs, report) = drain(schedule)
    assertEquals(pairs, Vector(0 -> 1, 0 -> 2, 1 -> 2))
    assertEquals(report.unmatchedLeft, Vector.empty)
    assertEquals(report.unmatchedRight, Vector.empty)
    assertEquals(report.pairSpace, PairSpace.WithinUndirected("all", SelfPolicy.Exclude))
  }

  test("source positions and duplicate diagnostics survive exclusion") {
    val schedule = get(WithinPairSchedule.directed(Vector(9, 3, 9, 7), all, SelfPolicy.Exclude))
    val (pairs, report) = drain(schedule)
    assertEquals(pairs, Vector(1 -> 3, 3 -> 1))
    assertEquals(
      report.ambiguous,
      Vector(
        PairingAmbiguity.DuplicateLeft[Int, Int](9, Vector(0, 2)),
        PairingAmbiguity.DuplicateRight[Int, Int](9, Vector(0, 2))
      )
    )
  }

  test("empty and singleton schedules finish with explicit self policy") {
    for undirected <- Vector(false, true); self <- SelfPolicy.values do
      def schedule(keys: Vector[Int]) =
        get(if undirected then WithinPairSchedule.canonicalUndirected(keys, all, self)
        else WithinPairSchedule.directed(keys, all, self))
      assertEquals(drain(schedule(Vector.empty))._1, Vector.empty)
      val (pairs, report) = drain(schedule(Vector(5)))
      assertEquals(pairs, if self == SelfPolicy.Include then Vector(0 -> 0) else Vector.empty)
      assertEquals(
        report.unmatchedLeft,
        if self == SelfPolicy.Include then Vector.empty else Vector(5)
      )
      assertEquals(report.unmatchedRight, report.unmatchedLeft)
  }

  test("preparation checks raw counts and selected budget fails at the exact next edge") {
    val sourceCap = get(PairScheduleBudget.of(3, 100, 100))
    assertEquals(
      WithinPairSchedule
        .directed(Vector(1, 1), all, SelfPolicy.Exclude, sourceCap)
        .left
        .toOption,
      Some(PairScheduleError.SourceBudget(2, 2, 3))
    )
    val candidateCap = get(PairScheduleBudget.of(100, 3, 100))
    assertEquals(
      WithinPairSchedule
        .canonicalUndirected(Vector(1, 2), all, SelfPolicy.Exclude, candidateCap)
        .left
        .toOption,
      Some(PairScheduleError.CandidateBudget(2, 2, 3))
    )
    val selectedCap = get(PairScheduleBudget.of(100, 100, 1))
    for undirected <- Vector(false, true) do
      val schedule = get(if undirected then
        WithinPairSchedule
          .canonicalUndirected(Vector(1, 2, 3), all, SelfPolicy.Exclude, selectedCap)
      else WithinPairSchedule.directed(Vector(1, 2, 3), all, SelfPolicy.Exclude, selectedCap))
      @annotation.tailrec
      def overflow(cursor: PairCursor[Int, Int]): PairScheduleError = cursor.advance(one) match
        case Left(error)                      => error
        case Right(PairPage.More(_, _, next)) => overflow(next)
        case Right(PairPage.Done(_, _, _))    => fail("selected budget was ignored")
      assertEquals(overflow(schedule.start), PairScheduleError.SelectedBudget("all", 2, 1))
  }

  test("re-reading a cursor does not mutate or consume its schedule") {
    val schedule = get(WithinPairSchedule.directed(Vector(1, 2), all, SelfPolicy.Include))
    val cursor   = schedule.start
    def head     = get(cursor.advance(one)) match
      case PairPage.More(pairs, work, _) => pairs -> work
      case PairPage.Done(_, _, _)        => fail("expected remaining work")
    assertEquals(head, head)
    assertEquals(head._1.map(p => p.left -> p.right), Vector(1 -> 1))
    assertEquals(drain(schedule)._1, Vector(0 -> 0, 0 -> 1, 1 -> 0, 1 -> 1))
  }
