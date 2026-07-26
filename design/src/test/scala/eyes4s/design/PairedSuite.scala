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
import eyes4s.kernel.Provenance

class PairedSuite extends munit.FunSuite:

  final case class Key(subject: String, item: String) derives CanEqual
  final case class Meta(label: String) derives CanEqual

  private given KeyDigest[Key] = KeyDigest.derived[Key]

  private val subject = Projection.named[Key, String]("subject")(_.subject)
  private val item    = Projection.named[Key, String]("item")(_.item)

  private def trial(subject: String, item: String, value: Int): Trial[Key, Meta, Int] =
    Trial(Key(subject, item), Meta(s"$subject-$item"), value)

  test("between pairing retains eligible pairs and both unmatched sides") {
    val left = Trials(
      Vector(
        trial("s1", "a", 1),
        trial("s2", "b", 2),
        trial("s3", "missing-left", 3)
      )
    )
    val right = Trials(
      Vector(
        trial("s1", "a", 10),
        trial("s2", "b", 20),
        trial("s4", "missing-right", 40)
      )
    )
    val design =
      Pairing.between[Key, Key].sameOn(subject, subject).sameOn(item, item).all

    val result = pair(left, right, design)

    assertEquals(
      result.pairs.map { case (l, r) => l.key -> r.key },
      Vector(
        Key("s1", "a") -> Key("s1", "a"),
        Key("s2", "b") -> Key("s2", "b")
      )
    )
    assertEquals(result.unmatchedLeft, Vector(Key("s3", "missing-left")))
    assertEquals(result.unmatchedRight, Vector(Key("s4", "missing-right")))
    assertEquals(result.eligiblePairCount, 2L)
    assertEquals(result.selectedPairCount, 2)
    assertEquals(result.storage, PairStorage.BetweenDirected)
    assert(result.ambiguous.isEmpty)
  }

  test("directed bottom-k records eligible and selected counts separately") {
    val left  = Trials.one(Key("s1", "target"), Meta("target"), 1)
    val right = Trials(
      Vector.tabulate(12)(index => trial("s1", s"control-$index", index))
    )
    val design =
      Pairing
        .between[Key, Key]
        .sameOn(subject, subject)
        .bottomK(4, Seed(7L), SampleId("controls"))
        .toOption
        .get

    val result = pair(left, right, design)

    assertEquals(result.eligiblePairCount, 12L)
    assertEquals(result.selectedPairCount, 4)
    assertEquals(result.unmatchedLeft, Vector.empty)
    assertEquals(result.unmatchedRight, Vector.empty)
  }

  test("bottom-k pairing is stable under candidate row reordering") {
    val left   = Trials.one(Key("s1", "target"), Meta("target"), 1)
    val rows   = Vector.tabulate(20)(index => trial("s1", s"control-$index", index))
    val design =
      Pairing
        .between[Key, Key]
        .sameOn(subject, subject)
        .bottomK(7, Seed(11L), SampleId("controls"))
        .toOption
        .get

    val forward = pair(left, Trials(rows), design).pairs.map(_._2.key).toSet
    val reverse = pair(left, Trials(rows.reverse), design).pairs.map(_._2.key).toSet

    assertEquals(reverse, forward)
  }

  test("duplicate keys are returned in full and never resolved to the first row") {
    val duplicateA = trial("s1", "a", 10)
    val duplicateB = trial("s1", "a", 11)
    val left       = Trials.one(Key("s1", "a"), Meta("left"), 1)
    val right      = Trials(Vector(duplicateA, duplicateB, trial("s2", "b", 20)))
    val design     = Pairing.matched[Key]

    val result = pair(left, right, design)

    assertEquals(result.pairs, Vector.empty)
    assertEquals(result.unmatchedLeft, Vector(left.rows.head.key))
    assertEquals(result.unmatchedRight, Vector(Key("s2", "b")))
    assert(result.ambiguous.nonEmpty)
    assertEquals(result.ambiguous.right.size, 1)
    assertEquals(result.ambiguous.right.head.key, Key("s1", "a"))
    assertEquals(
      result.ambiguous.right.head.occurrences.map(_.trial),
      Vector(duplicateA, duplicateB)
    )
    assertEquals(
      result.ambiguous.right.head.occurrences.map(_.index),
      Vector(0, 1)
    )
  }

  test("within-directed pairing applies self exclusion before eligibility") {
    val trials = Trials(
      Vector(
        trial("s1", "a", 1),
        trial("s1", "b", 2),
        trial("s2", "c", 3)
      )
    )
    val design = Pairing.mismatchedWithin(subject)

    val result = pair(trials, design)

    assertEquals(
      result.pairs.map { case (l, r) => l.key -> r.key }.toSet,
      Set(
        Key("s1", "a") -> Key("s1", "b"),
        Key("s1", "b") -> Key("s1", "a")
      )
    )
    assertEquals(result.unmatchedLeft, Vector(Key("s2", "c")))
    assertEquals(result.unmatchedRight, Vector(Key("s2", "c")))
    assertEquals(result.eligiblePairCount, 2L)
    assertEquals(result.storage, PairStorage.WithinDirected)
  }

  test("canonical-undirected pairing stores each eligible edge once") {
    val trials = Trials(
      Vector(
        trial("s1", "a", 1),
        trial("s1", "b", 2),
        trial("s1", "c", 3)
      )
    )
    val design =
      Pairing.within[Key].sameOn(subject).excludingSelf.canonicalUndirected

    val result = pair(trials, design)

    assertEquals(
      result.pairs.map { case (l, r) => l.key.item -> r.key.item },
      Vector("a" -> "b", "a" -> "c", "b" -> "c")
    )
    assertEquals(result.eligiblePairCount, 3L)
    assertEquals(result.selectedPairCount, 3)
    assertEquals(result.unmatchedLeft, Vector.empty)
    assertEquals(result.unmatchedRight, Vector.empty)
    assertEquals(result.storage, PairStorage.WithinUndirected)
  }

  test("canonical-undirected self inclusion stores one loop per row") {
    val trials = Trials(Vector(trial("s1", "a", 1), trial("s1", "b", 2)))
    val design =
      Pairing.within[Key].sameOn(subject).includingSelf.canonicalUndirected

    val result = pair(trials, design)

    assertEquals(
      result.pairs.map { case (l, r) => l.key.item -> r.key.item },
      Vector("a" -> "a", "a" -> "b", "b" -> "b")
    )
  }

  test("PairScore retains both operands when evaluation fails") {
    val failed =
      PairScore(Key("s1", "a"), Key("s2", "b"), Left("incompatible frames"))

    assertEquals(failed.left, Key("s1", "a"))
    assertEquals(failed.right, Key("s2", "b"))
    assertEquals(failed.result, Left("incompatible frames"))
  }

  test("PairwiseAnalysis retains pairing diagnostics and provenance") {
    val diagnostics = PairingReport[Key, Key](
      PairSpace.BetweenDirected("all", Selection.All),
      eligiblePairCount = 1L,
      selectedPairCount = 1,
      unmatchedLeft = Vector.empty,
      unmatchedRight = Vector.empty,
      ambiguous = Vector.empty
    )
    val provenance = Provenance.raw(ContentHash.ofString("paired-suite"))
    val result     = DirectedPairwiseAnalysis[Key, Key, String, Double](
      Vector(PairScore(Key("s1", "a"), Key("s1", "a"), Right(0.5))),
      diagnostics,
      provenance
    )

    assertEquals(result.rows.head.left, Key("s1", "a"))
    assertEquals(result.rows.head.right, Key("s1", "a"))
    assertEquals(result.diagnostics, diagnostics)
    assertEquals(result.provenance, provenance)
  }

end PairedSuite
