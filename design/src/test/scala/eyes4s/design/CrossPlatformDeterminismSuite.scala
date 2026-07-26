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

/** Shared JVM/Scala.js executable specification for PRD V-7.
  *
  * Exact values pin platform agreement; the metamorphic checks state why the
  * keyed sample remains stable as a study is reordered or extended.
  */
class CrossPlatformDeterminismSuite extends munit.FunSuite:

  final case class StudyKey(
      participant: String,
      item: Int,
      visit: Long,
      accepted: Boolean,
      weight: Double
  ) derives CanEqual

  final case class PairKey(participant: String, item: String) derives CanEqual

  private given KeyDigest[PairKey] = KeyDigest.derived[PairKey]

  private val digest     = KeyDigest.derived[StudyKey]
  private val seed       = Seed(20260726L)
  private val field      = SampleId("cross-platform-controls")
  private val focal      = PairKey("p00", "target")
  private val candidates =
    Vector.tabulate(100)(index => PairKey(f"p${index % 7}%02d", f"item-$index%03d"))

  private def pick(cap: Int, rows: Vector[PairKey] = candidates, sampleId: SampleId = field) =
    Selection.bottomK(focal, rows, cap, seed, sampleId)

  test("derived KeyDigest values and Seed.derive are pinned bit-for-bit") {
    val first  = digest.digest(StudyKey("p01", 17, 3L, accepted = true, -0.0)).value
    val second = digest
      .digest(
        StudyKey("participant-with-unicode-\u03bb", -9, Long.MaxValue, accepted = false, 1.25)
      )
      .value
    val derivedA = seed.derive("stratum/a").value
    val derivedB = seed.derive("stratum/b").value

    assertEquals(first, -6037774809768149520L)
    assertEquals(second, 4218499480289629398L)
    assertEquals(derivedA, 8128713585455657720L)
    assertEquals(derivedB, 8463378521954091055L)
  }

  test("keyed pair priorities and the bottom-k sample are pinned bit-for-bit") {
    val priorities = candidates.take(4).map(Selection.priority(seed, field, focal, _))
    val selected   = pick(12).map(key => s"${key.participant}/${key.item}")

    assertEquals(
      priorities,
      Vector(
        -7502315195422451911L,
        -4119770224345496645L,
        5181245983655189340L,
        7853309270973273171L
      )
    )
    assertEquals(
      selected,
      Vector(
        "p01/item-071",
        "p06/item-062",
        "p05/item-005",
        "p00/item-049",
        "p03/item-045",
        "p02/item-023",
        "p03/item-066",
        "p00/item-000",
        "p05/item-054",
        "p06/item-006",
        "p00/item-021",
        "p00/item-084"
      )
    )
  }

  test("row order does not change a keyed sample") {
    val expected = pick(25)
    val rotated  = candidates.drop(37) ++ candidates.take(37)

    assertEquals(pick(25, candidates.reverse).toSet, expected.toSet)
    assertEquals(pick(25, rotated).toSet, expected.toSet)
  }

  test("sampling an unrelated stratum does not advance or perturb this stratum") {
    val before = pick(20)
    val _      = Selection.bottomK(
      PairKey("other", "target"),
      candidates.map(key => key.copy(participant = "unrelated-" + key.participant)),
      40,
      seed,
      SampleId("unrelated-field")
    )
    assertEquals(pick(20), before)
  }

  test("distinct SampleId values request independent priority fields") {
    val first  = pick(20, sampleId = SampleId("field-a")).toSet
    val second = pick(20, sampleId = SampleId("field-b")).toSet

    assertNotEquals(first, second)
    assert(first.intersect(second).size < first.size)
  }

  test("bottom-50 is a subset and prefix of bottom-60") {
    val bottom50 = pick(50)
    val bottom60 = pick(60)

    assertEquals(bottom60.take(50), bottom50)
    assert(bottom50.toSet.subsetOf(bottom60.toSet))
  }

end CrossPlatformDeterminismSuite
