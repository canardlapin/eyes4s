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

class RelationJoinSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  final case class Key(group: Int, item: Int) derives CanEqual
  private given KeyDigest[Key]            = KeyDigest.derived[Key]
  private val group                       = Projection.named[Key, Int]("group")(_.group)
  private val item                        = Projection.named[Key, Int]("item")(_.item)
  private val keys                        = Vector.tabulate(24)(i => Key(i % 4, i / 4))
  private def trials(values: Vector[Key]) = Trials(values.map(k => Trial(k, (), k.item)))

  test(
    "all join strategies agree with an independent Cartesian oracle, including conjunction order"
  ) {
    val same      = Relation.sameOn(group)
    val different = Relation.differentOn(item)
    val relations = Vector(
      Relation.all[Key, Key],
      same,
      different,
      same.and(different),
      different.and(same),
      same.and(Relation.sameOn(item))
    )
    val right = keys.reverse :+ Key(99, 99)
    relations.foreach { relation =>
      val expected = for l <- keys; r <- right if relation.accepts(l, r) yield l -> r
      val result   =
        pair(trials(keys), trials(right), PairDesign.BetweenDirected(relation, Selection.All))
      assertEquals(result.pairs.map((l, r) => l.key -> r.key), expected)
      assertEquals(result.eligiblePairCount, expected.size.toLong)
      assertEquals(result.unmatchedLeft, keys.filterNot(k => expected.exists(_._1 == k)))
      assertEquals(result.unmatchedRight, right.filterNot(k => expected.exists(_._2 == k)))
      Vector(SelfPolicy.Include, SelfPolicy.Exclude).foreach { self =>
        val directedExpected = for
          l <- keys; r <- keys
          if (self == SelfPolicy.Include || l != r) && relation.accepts(l, r)
        yield l -> r
        val directed =
          pair(trials(keys), PairDesign.WithinDirected(relation, self, Selection.All))
        assertEquals(directed.pairs.map((l, r) => l.key -> r.key), directedExpected)
        val undirectedExpected = for
          (l, i) <- keys.zipWithIndex
          (r, j) <- keys.zipWithIndex
          if (j > i || (j == i && self == SelfPolicy.Include)) && relation.accepts(l, r)
        yield l -> r
        val undirected = pair(trials(keys), PairDesign.WithinUndirected(relation, self))
        assertEquals(undirected.pairs.map((l, r) => l.key -> r.key), undirectedExpected)
      }
    }
  }

  test("bottom-k uses exactly the exhaustive eligible set and unchanged keyed priorities") {
    val relation  = Relation.sameOn(group).and(Relation.differentOn(item))
    val selection = Selection.BottomK(get(PairLimit.of(2)), Seed(42), SampleId("join"))
    val expected  = keys.flatMap { l =>
      keys
        .filter(r => l.group == r.group && l.item != r.item)
        .sortBy(r => Selection.priority(Seed(42), SampleId("join"), l, r))
        .take(2)
        .map(l -> _)
    }
    val between =
      pair(trials(keys), trials(keys), PairDesign.BetweenDirected(relation, selection))
    val within =
      pair(trials(keys), PairDesign.WithinDirected(relation, SelfPolicy.Exclude, selection))
    assertEquals(between.pairs.map((l, r) => l.key -> r.key), expected)
    assertEquals(within.pairs.map((l, r) => l.key -> r.key), expected)
    assertEquals(between.eligiblePairCount, 120L)
  }

  test("equality candidate visits grow linearly, not as the Cartesian product") {
    var projections = 0
    val counted     = Projection.named[Int, Int]("counted") { k => projections += 1; k }
    val input       = Vector.range(0, 1000)
    val schedule    =
      get(DirectedPairSchedule.exhaustive(input, input.reverse, Relation.sameOn(counted)))
    // The budget's conservative cross product is intentionally unchanged.
    assertEquals(schedule.candidatePairCount, 1000000L)
    var work  = 0
    var count = 0
    @annotation.tailrec
    def drain(cursor: PairCursor[Int, Int]): PairingReport[Int, Int] =
      get(cursor.advance(get(PairQuantum.of(7)))) match
        case PairPage.More(pairs, units, next) =>
          work += units; count += pairs.size; drain(next)
        case PairPage.Done(pairs, units, report) =>
          work += units; count += pairs.size; report
    val report = drain(schedule.start)
    assertEquals(count, 1000)
    assertEquals(work, 2000) // one matched candidate plus one right-ledger visit each
    assertEquals(report.unmatchedRight, Vector.empty)
    assert(projections <= 5000, s"$projections projections indicates Cartesian execution")
  }

  test("same hash does not imply equal join values") {
    // These strings deliberately share String.hashCode; equality must still decide.
    assertEquals("Aa".hashCode, "BB".hashCode)
    val values = Vector("Aa", "BB")
    val data   = Trials(values.map(k => Trial(k, (), ())))
    val result = pair(data, data, Pairing.matched[String])
    assertEquals(result.pairs.map((l, r) => l.key -> r.key), values.map(k => k -> k))
  }
