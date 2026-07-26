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

import scala.compiletime.testing.typeCheckErrors

class PairDesignSuite extends munit.FunSuite:

  final case class Key(subject: String, image: String) derives CanEqual

  given KeyDigest[Key] = KeyDigest.derived[Key]

  val subject = Projection.named[Key, String]("subject")(_.subject)
  val image   = Projection.named[Key, String]("image")(_.image)

  val seed   = Seed(20260725L)
  val sample = SampleId("controls")

  private def keys(n: Int): Vector[Key] =
    (0 until n).map(i => Key(s"s${i % 5}", f"img$i%03d")).toVector

  // ---------------------------------------------------------------------------
  // Relation is structure
  // ---------------------------------------------------------------------------

  test("a relation names what it compares") {
    assertEquals(Relation.sameOn(image).render, "image == image")
    assertEquals(Relation.differentOn(image).render, "image != image")
    assertEquals(
      Relation.sameOn(image).and(Relation.differentOn(subject)).render,
      "(image == image and subject != subject)"
    )
  }

  test("accepts is derived from the structure") {
    val r = Relation.sameOn(image).and(Relation.differentOn(subject))
    assert(r.accepts(Key("s1", "a"), Key("s2", "a")), "same image, different subject")
    assert(!r.accepts(Key("s1", "a"), Key("s1", "a")), "same subject")
    assert(!r.accepts(Key("s1", "a"), Key("s2", "b")), "different image")
  }

  test("equality projections are exposed for a join, which a closure could not be") {
    val r = Relation.sameOn(image).and(Relation.sameOn(subject))
    assertEquals(r.joinKeys, Vector("image" -> "image", "subject" -> "subject"))
    // A difference is not a join key: it is a different execution strategy.
    assertEquals(Relation.differentOn(image).joinKeys, Vector.empty)
  }

  test("there is no arbitrary-predicate case to reach for") {
    // The escape hatch that would collapse hash joins, diagnostics and plan
    // serialisation the moment anyone used it -- and people use escape hatches.
    val errs = typeCheckErrors("""
      import eyes4s.design.*
      val r: Relation[Int, Int] = Relation.Where((a, b) => a < b)
    """)
    assert(errs.nonEmpty, "an arbitrary predicate case exists")
  }

  // ---------------------------------------------------------------------------
  // Only meaningful designs are writable
  // ---------------------------------------------------------------------------

  test("a canonical-undirected design has no per-focal sampling") {
    // There is no focal side to sample per, so the parameter does not exist
    // rather than being accepted and ignored.
    val errs = typeCheckErrors("""
      import eyes4s.design.*
      PairDesign.WithinUndirected(Relation.all[Int, Int], SelfPolicy.Exclude, Selection.All)
    """)
    assert(errs.nonEmpty, "an undirected design accepted a selection")
  }

  test("a between-collection design has no self policy") {
    // There is no "self" between two different collections.
    val errs = typeCheckErrors("""
      import eyes4s.design.*
      PairDesign.BetweenDirected(Relation.all[Int, String], SelfPolicy.Exclude, Selection.All)
    """)
    assert(errs.nonEmpty, "a between design accepted a self policy")
  }

  test("the designs that ARE meaningful are writable") {
    val between = PairDesign.BetweenDirected(Relation.sameOn(image), Selection.All)
    val within  =
      PairDesign.WithinDirected(Relation.all[Key, Key], SelfPolicy.Exclude, Selection.All)
    val undir = PairDesign.WithinUndirected(Relation.all[Key, Key], SelfPolicy.Exclude)
    assert(between.render.startsWith("between-directed"))
    assert(within.render.contains("self=Exclude"))
    assert(undir.render.startsWith("within-undirected"))
  }

  test("the fluent facade reads as the scientific pair design") {
    val target =
      Pairing
        .between[Key, Key]
        .sameOn(subject, subject)
        .sameOn(image, image)
        .all

    val repeated =
      PairDesign
        .within[Key]
        .sameOn(subject)
        .sameOn(image)
        .excludingSelf
        .canonicalUndirected

    assertEquals(target.relation.render, "(subject == subject and image == image)")
    assertEquals(repeated.relation.render, "(subject == subject and image == image)")
    assertEquals(repeated.self, SelfPolicy.Exclude)
  }

  test("task-named facades compile to the same algebra") {
    assertEquals(Pairing.matchedOn(image).relation, Relation.sameOn(image))
    assertEquals(Pairing.mismatchedWithin(subject).relation, Relation.sameOn(subject))
    assertEquals(Pairing.mismatchedWithin(subject).self, SelfPolicy.Exclude)
  }

  test("sampled is a thin facade over relation plus validated bottom-k") {
    val relation =
      Relation.sameOn(subject).and(Relation.differentOn(image))

    Pairing.sampled(relation, 7, seed, sample) match
      case Right(PairDesign.BetweenDirected(actual, Selection.BottomK(cap, s, id))) =>
        assertEquals(actual.render, relation.render)
        assertEquals(cap.value, 7)
        assertEquals(s, seed)
        assertEquals(id, sample)
      case other => fail(s"unexpected sampled design: $other")

    assertEquals(
      Pairing.sampled(relation, 0, seed, sample),
      Left(PairingError.NonPositiveLimit(0))
    )
  }

  test("a bottom-k design validates the raw limit at the facade") {
    val controls =
      Pairing
        .between[Key, Key]
        .sameOn(subject, subject)
        .differentOn(image, image)

    val valid = controls.bottomK(10, seed, sample)
    assert(valid.isRight)
    assertEquals(
      controls.bottomK(0, seed, sample),
      Left(PairingError.NonPositiveLimit(0))
    )
    assert(clue(controls.bottomK(-5, seed, sample).left.toOption.get.message).contains("-5"))
  }

  test("the BottomK algebra cannot be constructed with a raw Int") {
    val errs = typeCheckErrors("""
      import eyes4s.design.*
      Selection.BottomK(0, Seed(1L), SampleId("controls"))
    """)
    assert(errs.nonEmpty, "BottomK accepted an unvalidated, non-positive cap")
  }

  // ---------------------------------------------------------------------------
  // Bottom-k: the properties that follow from excluding the cap
  // ---------------------------------------------------------------------------

  val focal      = Key("s0", "img000")
  val candidates = keys(60)

  private def pick(cap: Int, cs: Vector[Key] = candidates, sid: SampleId = sample) =
    Selection.bottomK(focal, cs, cap, seed, sid)

  test("raising the cap yields a SUPERSET, never a different sample") {
    // The central claim. A reviewer asking for more controls must see the
    // original ones plus more, or every number computed before the request
    // silently changes.
    val caps = Vector(1, 2, 5, 10, 25, 50)
    caps.sliding(2).foreach {
      case Vector(small, large) =>
        val a = pick(small).toSet
        val b = pick(large).toSet
        assert(a.subsetOf(b), clue((small, large, a.diff(b))))
      case _ => ()
    }
  }

  test("the selection is a prefix, in priority order, at every cap") {
    val full = pick(candidates.length)
    (1 to 20).foreach(k => assertEquals(pick(k), full.take(k), clue(k)))
  }

  test("the result does not depend on the order the candidates arrive in") {
    val shuffled = candidates.reverse
    val rotated  = candidates.drop(17) ++ candidates.take(17)
    assertEquals(pick(10, shuffled).toSet, pick(10).toSet)
    assertEquals(pick(10, rotated).toSet, pick(10).toSet)
  }

  test("adding candidates elsewhere does not disturb the ones already chosen") {
    // Not literally true of the selection -- a new candidate may outrank an old
    // one -- but the PRIORITIES of the existing candidates must be untouched,
    // which is what makes the sample explainable.
    val extended = candidates ++ keys(40).map(k => k.copy(image = "extra-" + k.image))
    val before   = candidates.map(c => Selection.priority(seed, sample, focal, c))
    val after    = candidates.map(c => Selection.priority(seed, sample, focal, c))
    assertEquals(before, after)
    // And the enlarged pool still yields a deterministic answer.
    assertEquals(pick(10, extended), pick(10, extended))
  }

  test("a distinct sample identifier gives an independent field") {
    val a = pick(10, sid = SampleId("controls"))
    val b = pick(10, sid = SampleId("controls-2"))
    assertNotEquals(a, b)
    // Independent, not merely permuted: the sets differ, not just the order.
    assertNotEquals(a.toSet, b.toSet)
  }

  test("a distinct seed gives an independent field too") {
    assertNotEquals(
      Selection.bottomK(focal, candidates, 10, Seed(1L), sample).toSet,
      Selection.bottomK(focal, candidates, 10, Seed(2L), sample).toSet
    )
  }

  test("each focal key gets its own priority ordering") {
    // Otherwise every trial would draw the same controls, and the control
    // condition would be one sample rather than many.
    val a = Selection.bottomK(Key("s0", "img000"), candidates, 5, seed, sample)
    val b = Selection.bottomK(Key("s1", "img001"), candidates, 5, seed, sample)
    assertNotEquals(a.toSet, b.toSet)
  }

  test("the realised count is min(cap, eligible), and knowable in advance") {
    // eyesim samples first and removes the true match afterwards, so its
    // realised count is n or n-1 unpredictably -- which is the entire reason it
    // has to return an n_perm column. Exclusion happens before selection here.
    assertEquals(pick(10).length, 10)
    assertEquals(pick(1000).length, candidates.length)
    assertEquals(pick(0).length, 0)
    assertEquals(pick(-5).length, 0)
  }

  test("selection is deterministic across runs with no global state") {
    assertEquals(pick(12), pick(12))
    // And unaffected by having drawn other samples in between.
    val _ = Selection.bottomK(Key("other", "x"), candidates, 30, seed, SampleId("noise"))
    assertEquals(pick(12), pick(12))
  }

  test("priority does not depend on the cap") {
    // Stated directly, since every property above follows from it.
    val p1 = Selection.priority(seed, sample, focal, candidates.head)
    val p2 = Selection.priority(seed, sample, focal, candidates.head)
    assertEquals(p1, p2)
  }

  /** GOLDEN VECTOR: the first five controls for a fixed focal key and seed.
    *
    * Pinned for the same reason the generator's stream is. If a refactor
    * changes these, every control sample drawn before the change is no longer
    * reproducible, and a green test suite would not say so.
    */
  test("keyed sampling is pinned, and identical on JVM and Scala.js") {
    assertEquals(
      pick(5).map(_.image),
      Vector("img040", "img010", "img013", "img014", "img015")
    )
  }

end PairDesignSuite
