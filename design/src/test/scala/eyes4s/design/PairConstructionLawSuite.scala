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

import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll

/** Laws for the shared exhaustive pairing path.
  *
  * The oracle here is a deliberately naive brute force over keys: it shares no
  * code with [[PairConstruction]] or [[DirectedPairSchedule]]. Generated
  * operands contain duplicate keys, references with no eligible partner and
  * same-stimulus exclusions.
  */
class PairConstructionLawSuite extends ScalaCheckSuite:
  final case class Key(subject: String, item: String, occurrence: Int):
    override def toString: String = "same display label"
  private given KeyDigest[Key] = KeyDigest.derived[Key]
  private val subject          = Projection.named[Key, String]("subject")(_.subject)
  private val item             = Projection.named[Key, String]("item")(_.item)
  private val matched          =
    Pairing.between[Key, Key].sameOn(subject, subject).sameOn(item, item).all
  private val controls =
    Pairing.between[Key, Key].sameOn(subject, subject).differentOn(item, item).all
  private def get[E, A](e: Either[E, A]): A = e.fold(e => fail(s"$e"), identity)

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(200).withInitialSeed(0x5041495253L)

  private val genKey: Gen[Key] =
    for
      s <- Gen.oneOf("p", "q", "r")
      i <- Gen.oneOf("a", "b", "c", "d")
      o <- Gen.frequency(3 -> Gen.const(0), 1 -> Gen.const(1))
    yield Key(s, i, o)

  private val genTrials: Gen[Trials[Key, Unit, Int]] =
    Gen
      .choose(0, 9)
      .flatMap(n => Gen.listOfN(n, Gen.zip(genKey, Gen.choose(0, 1000))))
      .map(rows => Trials(rows.toVector.map { case (k, v) => Trial(k, (), v) }))

  private val genDesign: Gen[PairDesign.BetweenDirected[Key, Key]] =
    Gen.oneOf(matched, controls)

  /** Brute-force expectation: pairs in left-major source order, unmatched
    * operands in source order and every duplicate occurrence excluded.
    */
  private final case class Expected(
      pairs: Vector[(Trial[Key, Unit, Int], Trial[Key, Unit, Int])],
      unmatchedLeft: Vector[Key],
      unmatchedRight: Vector[Key],
      ambiguous: Vector[PairingAmbiguity[Key, Key]]
  )

  private def oracle(
      left: Trials[Key, Unit, Int],
      right: Trials[Key, Unit, Int],
      relation: Relation[Key, Key]
  ): Expected =
    def duplicated(rows: Vector[Trial[Key, Unit, Int]]): Vector[(Key, Vector[Int])] =
      rows
        .map(_.key)
        .distinct
        .map(key => key -> rows.indices.filter(i => rows(i).key == key).toVector)
        .filter(_._2.size > 1)
    val leftDuplicates  = duplicated(left.rows)
    val rightDuplicates = duplicated(right.rows)
    val usableLeft      =
      left.rows.zipWithIndex
        .filterNot(r => leftDuplicates.exists(_._2.contains(r._2)))
        .map(_._1)
    val usableRight =
      right.rows.zipWithIndex
        .filterNot(r => rightDuplicates.exists(_._2.contains(r._2)))
        .map(_._1)
    val pairs = for
      l <- usableLeft
      r <- usableRight
      if relation.accepts(l.key, r.key)
    yield l -> r
    Expected(
      pairs,
      usableLeft.collect {
        case l if !usableRight.exists(r => relation.accepts(l.key, r.key)) => l.key
      },
      usableRight.collect {
        case r if !usableLeft.exists(l => relation.accepts(l.key, r.key)) => r.key
      },
      leftDuplicates.map { case (k, i) => PairingAmbiguity.DuplicateLeft[Key, Key](k, i) } ++
        rightDuplicates.map { case (k, i) => PairingAmbiguity.DuplicateRight[Key, Key](k, i) }
    )

  property("pair under Selection.All equals the brute-force oracle") {
    forAll(genTrials, genTrials, genDesign) { (left, right, design) =>
      val expected = oracle(left, right, design.relation)
      val paired   = pair(left, right, design)
      Prop(paired.pairs == expected.pairs) :| "pairs" &&
      Prop(paired.eligiblePairCount == expected.pairs.size.toLong) :| "eligible" &&
      Prop(paired.selectedPairCount == expected.pairs.size) :| "selected" &&
      Prop(paired.unmatchedLeft == expected.unmatchedLeft) :| "unmatchedLeft" &&
      Prop(paired.unmatchedRight == expected.unmatchedRight) :| "unmatchedRight" &&
      Prop(paired.diagnostics.ambiguous == expected.ambiguous) :| "ambiguous" &&
      Prop(
        paired.pairSpace == PairSpace.BetweenDirected(design.relation.render, Selection.All)
      ) :| "pairSpace"
    }
  }

  property("paged schedules at every quantum reproduce pair exactly") {
    forAll(genTrials, genTrials, genDesign, Gen.oneOf(1, 2, 3, 7, 1024)) {
      (left, right, design, quantum) =>
        val paired   = pair(left, right, design)
        val schedule = get(
          DirectedPairSchedule.exhaustive(
            left.rows.map(_.key),
            right.rows.map(_.key),
            design.relation
          )
        )
        val q     = get(PairQuantum.of(quantum))
        val pages = Vector.newBuilder[ScheduledPair[Key, Key]]
        @annotation.tailrec
        def loop(cursor: PairCursor[Key, Key]): PairingReport[Key, Key] =
          get(cursor.advance(q)) match
            case PairPage.More(pairs, _, next)   => pages ++= pairs; loop(next)
            case PairPage.Done(pairs, _, report) => pages ++= pairs; report
        val report    = loop(schedule.start)
        val scheduled =
          pages.result().map(p => left.rows(p.leftIndex) -> right.rows(p.rightIndex))
        Prop(scheduled == paired.pairs) :| "pairs" &&
        Prop(report == paired.diagnostics) :| "report" &&
        Prop(schedule.ambiguities == paired.diagnostics.ambiguous) :| "ambiguities"
    }
  }

  property("bottom-k sampling keeps the All diagnostics and selects min(cap, eligible)") {
    forAll(genTrials, genTrials, genDesign, Gen.choose(1, 3), Gen.long) {
      (left, right, design, cap, seedValue) =>
        val all     = pair(left, right, design)
        val sampled = pair(
          left,
          right,
          get(
            Pairing
              .Between(design.relation)
              .bottomK(cap, Seed(seedValue), SampleId("law"))
          )
        )
        val eligibleByLeft = all.pairs.groupBy(_._1).view.mapValues(_.size).toMap
        val selectedByLeft = sampled.pairs.groupBy(_._1).view.mapValues(_.size).toMap
        Prop(sampled.eligiblePairCount == all.eligiblePairCount) :| "eligible" &&
        Prop(sampled.unmatchedLeft == all.unmatchedLeft) :| "unmatchedLeft" &&
        Prop(sampled.unmatchedRight == all.unmatchedRight) :| "unmatchedRight" &&
        Prop(sampled.ambiguous == all.ambiguous) :| "ambiguous" &&
        Prop(sampled.pairs.forall(all.pairs.contains)) :| "subset" &&
        Prop(sampled.pairs.map(_._1).distinct == all.pairs.map(_._1).distinct) :| "leftOrder" &&
        Prop(eligibleByLeft.forall { case (l, n) =>
          selectedByLeft.getOrElse(l, 0) == math.min(cap, n)
        }) :|
          "counts"
    }
  }

  private val genWithinRelation: Gen[Relation[Key, Key]] = Gen.oneOf(
    matched.relation,
    controls.relation,
    Relation.all[Key, Key],
    Relation.SameOn(
      Projection.named[Key, Int]("occurrence")(_.occurrence),
      Projection.named[Key, Int]("nextOccurrence")(_.occurrence + 1)
    )
  )

  property("within schedules and pure pairing match an independent source-position oracle") {
    forAll(
      genTrials,
      genWithinRelation,
      Gen.oneOf(SelfPolicy.values.toSeq),
      Gen.oneOf(true, false),
      Gen.oneOf(1, 2, 3, 7, 1024)
    ) { (trials, relation, self, undirected, quantum) =>
      // No production grouping, eligibility enumeration or diagnostic helpers.
      val rows     = trials.rows
      val usable   = rows.indices.filter(i => rows.count(_.key == rows(i).key) == 1).toVector
      val expected = for
        l <- usable
        r <- usable
        if (if undirected then l < r || (self == SelfPolicy.Include && l == r)
            else self == SelfPolicy.Include || l != r)
        if relation.accepts(rows(l).key, rows(r).key)
      yield l -> r
      val unmatchedLeft = usable
        .filterNot(i => expected.exists { case (l, r) => l == i || (undirected && r == i) })
        .map(rows(_).key)
      val unmatchedRight = usable
        .filterNot(i => expected.exists { case (l, r) => r == i || (undirected && l == i) })
        .map(rows(_).key)
      val duplicated = rows.map(_.key).distinct.flatMap { key =>
        val indices = rows.indices.filter(i => rows(i).key == key).toVector
        Option.when(indices.size > 1)(key -> indices)
      }
      val ambiguities = duplicated.map { case (key, indices) =>
        PairingAmbiguity.DuplicateLeft[Key, Key](key, indices)
      } ++ duplicated.map { case (key, indices) =>
        PairingAmbiguity.DuplicateRight[Key, Key](key, indices)
      }
      val paired = if undirected then pair(trials, PairDesign.WithinUndirected(relation, self))
      else pair(trials, PairDesign.WithinDirected(relation, self, Selection.All))
      val schedule = get(if undirected then
        WithinPairSchedule.canonicalUndirected(rows.map(_.key), relation, self)
      else WithinPairSchedule.directed(rows.map(_.key), relation, self))
      val pages = Vector.newBuilder[ScheduledPair[Key, Key]]
      val q     = get(PairQuantum.of(quantum))
      @annotation.tailrec
      def drain(cursor: PairCursor[Key, Key]): PairingReport[Key, Key] =
        get(cursor.advance(q)) match
          case PairPage.More(pairs, work, next) =>
            assert(work > 0 && work <= quantum)
            pages ++= pairs
            drain(next)
          case PairPage.Done(pairs, work, report) =>
            assert(work >= 0 && work <= quantum)
            pages ++= pairs
            report
      val report = drain(schedule.start)
      Prop(pages.result().map(p => p.leftIndex -> p.rightIndex) == expected) :| "positions" &&
      Prop(paired.pairs == expected.map { case (l, r) =>
        rows(l) -> rows(r)
      }) :| "pure pairs" &&
      Prop(report == paired.diagnostics) :| "same report" &&
      Prop(report.unmatchedLeft == unmatchedLeft) :| "unmatched left" &&
      Prop(report.unmatchedRight == unmatchedRight) :| "unmatched right" &&
      Prop(report.ambiguous == ambiguities) :| "all duplicate occurrences" &&
      Prop(report.eligiblePairCount == expected.size.toLong) :| "eligible count" &&
      Prop(report.selectedPairCount == expected.size) :| "selected count"
    }
  }
