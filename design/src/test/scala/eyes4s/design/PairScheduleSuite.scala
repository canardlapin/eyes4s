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

import eyes4s.kernel.ContentHash
import scala.compiletime.testing.typeCheckErrors

class PairScheduleSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(e => fail(s"$e"), identity)
  final case class Key(subject: String, item: String, occurrence: Int = 0):
    override def toString: String = "same display label"
  private given KeyDigest[Key] = KeyDigest.derived[Key]
  private val subject          = Projection.named[Key, String]("subject")(_.subject)
  private val item             = Projection.named[Key, String]("item")(_.item)
  private val matched          =
    Pairing.between[Key, Key].sameOn(subject, subject).sameOn(item, item).all
  private val controls =
    Pairing.between[Key, Key].sameOn(subject, subject).differentOn(item, item).all

  private def drain[K, L](
      schedule: DirectedPairSchedule[K, L],
      quantum: Int
  ): (Vector[ScheduledPair[K, L]], PairingReport[K, L]) =
    val out = Vector.newBuilder[ScheduledPair[K, L]]
    val q   = get(PairQuantum.of(quantum))
    @annotation.tailrec
    def loop(cursor: PairCursor[K, L]): PairingReport[K, L] =
      get(cursor.advance(q)) match
        case PairPage.More(pairs, work, next) =>
          assert(work > 0 && work <= quantum)
          assert(pairs.size <= work)
          out ++= pairs
          loop(next)
        case PairPage.Done(pairs, work, report) =>
          assert(work >= 0 && work <= quantum)
          assert(pairs.size <= work)
          out ++= pairs
          report
    val report = loop(schedule.start)
    out.result() -> report

  test(
    "manual two-participant schedule retains missing and ambiguous operands at every quantum"
  ) {
    val left = Vector(
      Key("p1", "a"),
      Key("p1", "b"),
      Key("p1", "c"),
      Key("p2", "a"),
      Key("p2", "b"),
      Key("p2", "c")
    )
    val right =
      Vector(Key("p1", "a"), Key("p1", "b"), Key("p1", "b"), Key("p2", "a"), Key("p2", "c"))
    val expectedMatched  = Vector(0 -> 0, 3 -> 3, 5 -> 4)
    val expectedControls = Vector(1 -> 0, 2 -> 0, 3 -> 4, 4 -> 3, 4 -> 4, 5 -> 3)
    Vector(1, 2, 7, 1024).foreach { quantum =>
      val (m, mr) =
        drain(get(DirectedPairSchedule.exhaustive(left, right, matched.relation)), quantum)
      val (c, cr) =
        drain(get(DirectedPairSchedule.exhaustive(left, right, controls.relation)), quantum)
      assertEquals(m.map(p => p.leftIndex -> p.rightIndex), expectedMatched)
      assertEquals(c.map(p => p.leftIndex -> p.rightIndex), expectedControls)
      assertEquals(mr.unmatchedLeft, Vector(left(1), left(2), left(4)))
      assertEquals(cr.unmatchedLeft, Vector(left(0)))
      assertEquals(mr.unmatchedRight, Vector.empty)
      assertEquals(
        mr.ambiguous,
        Vector(PairingAmbiguity.DuplicateRight[Key, Key](right(1), Vector(1, 2)))
      )
      assertEquals(cr.ambiguous, mr.ambiguous)
      assertEquals(mr.selectedPairCount, 3)
      assertEquals(cr.eligiblePairCount, 6L)
    }
  }

  test("pages preserve source ordering under permutation and distinct occurrence keys") {
    val left         = Vector(Key("p", "a", 0), Key("p", "a", 1), Key("p", "b", 0))
    val right        = Vector(Key("p", "b", 8), Key("p", "a", 9))
    val (forward, _) =
      drain(get(DirectedPairSchedule.exhaustive(left, right, controls.relation)), 1)
    val (reverse, _) = drain(
      get(DirectedPairSchedule.exhaustive(left.reverse, right.reverse, controls.relation)),
      3
    )
    assertEquals(
      forward.map(p => p.left -> p.right),
      Vector(left(0) -> right(0), left(1) -> right(0), left(2) -> right(1))
    )
    assertEquals(
      reverse.map(p => p.left -> p.right),
      Vector(left(2) -> right(1), left(1) -> right(0), left(0) -> right(0))
    )
    assertEquals(
      forward.map(p => p.left -> p.right).toSet,
      reverse.map(p => p.left -> p.right).toSet
    )
  }

  test(
    "empty sides and duplicate focal keys retain complete reports with bounded finalization"
  ) {
    val a              = Key("p", "a")
    val b              = Key("p", "b")
    val (_, emptyLeft) = drain(
      get(DirectedPairSchedule.exhaustive(Vector.empty[Key], Vector(a, b), matched.relation)),
      1
    )
    assertEquals(emptyLeft.unmatchedRight, Vector(a, b))
    val (_, emptyRight) = drain(
      get(DirectedPairSchedule.exhaustive(Vector(a, b), Vector.empty[Key], matched.relation)),
      1
    )
    assertEquals(emptyRight.unmatchedLeft, Vector(a, b))
    val (_, duplicates) = drain(
      get(DirectedPairSchedule.exhaustive(Vector(a, b, a), Vector(a), matched.relation)),
      1
    )
    assertEquals(duplicates.unmatchedLeft, Vector(b))
    assertEquals(duplicates.unmatchedRight, Vector(a))
    assertEquals(
      duplicates.ambiguous,
      Vector(PairingAmbiguity.DuplicateLeft[Key, Key](a, Vector(0, 2)))
    )
  }

  test("source and candidate budgets reject before predicate calls or pair allocation") {
    var calls    = 0
    val relation = Relation.SameOn(
      Projection.named[Int, Int]("counted") { x => calls += 1; x },
      Projection.named[Int, Int]("right")(identity)
    )
    val budget = get(PairScheduleBudget.of(200000, Int.MaxValue.toLong, 10))
    assertEquals(
      budget.checkCounts(65536, 65536),
      Left(PairScheduleError.CandidateBudget(65536, 65536, Int.MaxValue.toLong))
    )
    val result = DirectedPairSchedule.exhaustive(
      Vector.range(0, 65536),
      Vector.range(0, 65536),
      relation,
      budget
    )
    assertEquals(
      result.left.toOption,
      Some(PairScheduleError.CandidateBudget(65536, 65536, Int.MaxValue.toLong))
    )
    assertEquals(calls, 0)
    assert(PairScheduleBudget.default.checkCounts(Long.MaxValue, Long.MaxValue).isLeft)
    assert(PairScheduleBudget.default.checkCounts(-1, 0).isLeft)
    assert(PairQuantum.of(0).isLeft)
    assert(PairScheduleBudget.of(-1, 1, 1).isLeft)
  }

  test(
    "selected-pair budget returns an explicit failure instead of a truncated completed report"
  ) {
    val schedule = get(
      DirectedPairSchedule.exhaustive(
        Vector(1, 2),
        Vector(1, 2),
        Relation.all[Int, Int],
        get(PairScheduleBudget.of(4, 4, 1))
      )
    )
    val first = get(schedule.start.advance(get(PairQuantum.of(1))))
    first match
      case PairPage.More(pairs, _, next) =>
        assertEquals(pairs.size, 1)
        assertEquals(
          next.advance(get(PairQuantum.of(1))).left.toOption,
          Some(PairScheduleError.SelectedBudget(schedule.pairSpace.relation, 2, 1))
        )
      case _ => fail("schedule completed before all candidates")
  }

  test(
    "scheduled evaluation exactly retains legacy diagnostics, provenance and failure reductions"
  ) {
    val l = Trials(
      Vector(
        Trial(Key("p", "a"), (), 10),
        Trial(Key("p", "b"), (), 20),
        Trial(Key("q", "a"), (), 30)
      )
    )
    val r    = Trials(Vector(Trial(Key("p", "a"), (), 11), Trial(Key("p", "b"), (), 22)))
    val info = EvaluationInfo("test", EvaluationScale.Unitless, None)
    def compare(a: Int, b: Int): Either[String, Double] =
      if b == 22 then Left("failed right") else Right((a - b).toDouble)
    val legacy   = evaluatePairs(pair(l, r, controls), ContentHash.empty, info)(compare)
    val schedule = get(
      DirectedPairSchedule.exhaustive(l.rows.map(_.key), r.rows.map(_.key), controls.relation)
    )
    Vector(1, 3, 1024).foreach { q =>
      val evaluated = get(
        evaluateScheduled(schedule, ContentHash.empty, info, get(PairQuantum.of(q)))(p =>
          compare(l.rows(p.leftIndex).value, r.rows(p.rightIndex).value)
        )
      )
      assertEquals(evaluated, legacy)
      Vector(FailurePolicy.RequireAll, get(FailurePolicy.successfulOnly(1))).foreach { policy =>
        assertEquals(evaluated.meanByLeft(policy).entries, legacy.meanByLeft(policy).entries)
        assertEquals(
          evaluated.meanByLeft(policy).diagnostics,
          legacy.meanByLeft(policy).diagnostics
        )
      }
    }
  }

  test("a page quantum cannot be constructed without its positive-value check") {
    assert(typeCheckErrors("""
      import eyes4s.design.*
      new PairQuantum(0)
    """).nonEmpty)
  }
