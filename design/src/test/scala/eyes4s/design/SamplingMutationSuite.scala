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
import munit.ScalaCheckSuite
import org.scalacheck.Gen
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll
import org.scalacheck.Test

/** Mutation suite for keyed bottom-k sampling (PRD X-9, V-9).
  *
  * Every mutant below is a complete alternative sampler with the shipped
  * signature, written inside this file. Each named property is a function of
  * a sampler; the shipped [[Selection.bottomK]] must satisfy every property
  * and each mutant must be observed to violate the property that names it.
  * A property that no mutant violates would be decoration, and a mutant that
  * no property kills would be an unguarded regression.
  *
  * ==Mutation execution receipts==
  *
  * Observed on both JVM and Scala.js (`designJVM/test`, `designJS/test`).
  *
  * {{{
  * | mutant              | changed expression                                   | killed by         |
  * |---------------------|------------------------------------------------------|-------------------|
  * | focal-blind         | priority omits the focal key digest                  | P-focal           |
  * | stream-sequenced    | priorities drawn from one advancing generator        | P-order, P-focal  |
  * | arrival-order       | candidates.take(cap), no priority at all             | P-order, P-pin    |
  * | cap-short           | take(cap - 1)                                        | P-count           |
  * | cap-long            | take(cap + 1)                                        | P-count           |
  * | cap-in-priority     | cap mixed into the priority digest                   | P-prefix          |
  * | tie-arrival         | 3-bit priority; ties resolved by arrival position    | P-order           |
  * | tie-random          | 3-bit priority; ties resolved by an unseeded Random  | P-repeat          |
  * | render-digest       | priority from toString rather than KeyDigest         | P-pin, P-drift    |
  * | match-after-sample  | true match removed after taking the bottom k         | P-count           |
  * }}}
  *
  * The `render-digest` mutant is the cross-platform one. `Double.toString`
  * renders `1.0` as `"1.0"` on the JVM and `"1"` on Scala.js, so a digest of
  * the rendering selects different controls on the two platforms. P-drift
  * feeds both renderings to the mutant on whichever platform is running and
  * shows they disagree; P-pin shows the shipped sampler agrees with one
  * literal on both.
  */
class SamplingMutationSuite extends ScalaCheckSuite:

  /** A key whose `Double` field renders differently on the JVM and Scala.js. */
  final case class Key(participant: String, item: Int, weight: Double) derives CanEqual

  private given KeyDigest[Key] = KeyDigest.derived[Key]

  /** The shipped signature, so every mutant is a drop-in alternative. */
  type Sampler = (Key, Vector[Key], Int, Seed, SampleId) => Vector[Key]

  override def scalaCheckTestParameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(120).withInitialSeed(0x53414d504cL)

  private val killParameters =
    Test.Parameters.default.withMinSuccessfulTests(120).withInitialSeed(0x4b494c4cL)

  private def killed(prop: Prop): Boolean = !Test.check(killParameters, prop).passed

  // ---------------------------------------------------------------------------
  // Fixture and generators
  // ---------------------------------------------------------------------------

  private val seed   = Seed(20260917L)
  private val sample = SampleId("mutation-controls")

  /** The focal key is outside the pool, and its whole-valued weight renders
    * differently on the two platforms, as do fifteen of the pool's weights.
    */
  private val focal = Key("focal", -1, 1.0)
  private val pool  =
    Vector.tabulate(60)(i => Key(s"p${i % 5}", i, i.toDouble / 4.0))

  private val genKey: Gen[Key] =
    for
      p <- Gen.choose(0, 6)
      i <- Gen.choose(0, 200)
      w <- Gen.oneOf(0.0, 0.25, 0.5, 1.0, 2.0, 3.5, 7.0)
    yield Key(s"p$p", i, w)

  private val genPool: Gen[Vector[Key]] =
    Gen.choose(30, 60).flatMap(n => Gen.listOfN(n, genKey)).map(_.toVector.distinct)

  private val genCap: Gen[Int] = Gen.choose(1, 12)

  private val genShuffle: Gen[Vector[Key] => Vector[Key]] =
    Gen.oneOf(
      Gen.const((v: Vector[Key]) => v.reverse),
      Gen.choose(1, 29).map(k => (v: Vector[Key]) => v.drop(k) ++ v.take(k)),
      Gen.long.map(s => (v: Vector[Key]) => new scala.util.Random(s).shuffle(v))
    )

  // ---------------------------------------------------------------------------
  // Samplers: the shipped one and every mutant
  // ---------------------------------------------------------------------------

  val shipped: Sampler = (f, cs, cap, s, id) => Selection.bottomK(f, cs, cap, s, id)

  private def digest(k: Key): ContentHash = summon[KeyDigest[Key]].digest(k)

  private def mixed(s: Seed, id: SampleId, parts: ContentHash*): Long =
    Seed.mix(
      s.value ^ ContentHash
        .combineAll(
          ContentHash.ofString("bottomk") +: ContentHash.ofString(id.value) +: parts.toVector
        )
        .value
    )

  /** Per-focal seed not derived from the key: every focal draws the same set. */
  val focalBlind: Sampler = (_, cs, cap, s, id) =>
    cs.map(c => mixed(s, id, digest(c)) -> c).sortBy(_._1).take(cap).map(_._2)

  /** One generator advanced across candidates in arrival order. */
  val streamSequenced: Sampler = (_, cs, cap, s, _) =>
    val rng = s.generator
    cs.map(c => rng.nextLong() -> c).sortBy(_._1).take(cap).map(_._2)

  /** Selection by insertion order instead of digest order. */
  val arrivalOrder: Sampler = (_, cs, cap, _, _) => cs.take(cap)

  private def ranked(f: Key, cs: Vector[Key], s: Seed, id: SampleId): Vector[Key] =
    cs.map(c => mixed(s, id, digest(f), digest(c)) -> c).sortBy(_._1).map(_._2)

  val capShort: Sampler = (f, cs, cap, s, id) => ranked(f, cs, s, id).take(cap - 1)
  val capLong: Sampler  = (f, cs, cap, s, id) => ranked(f, cs, s, id).take(cap + 1)

  /** The cap participates in the priority, so raising it reshuffles. */
  val capInPriority: Sampler = (f, cs, cap, s, id) =>
    cs.map(c => mixed(s, id, digest(f), digest(c), ContentHash.ofString(cap.toString)) -> c)
      .sortBy(_._1)
      .take(cap)
      .map(_._2)

  /** A 3-bit priority guarantees ties among 60 candidates. */
  private def coarse(f: Key, c: Key, s: Seed, id: SampleId): Long =
    mixed(s, id, digest(f), digest(c)) >>> 61

  /** Ties resolved by arrival position (a stable sort on a coarse key). */
  val tieArrival: Sampler = (f, cs, cap, s, id) =>
    cs.map(c => coarse(f, c, s, id) -> c).sortBy(_._1).take(cap).map(_._2)

  /** Ties resolved by an unseeded generator: not a function of its inputs. */
  val tieRandom: Sampler = (f, cs, cap, s, id) =>
    val rnd = new scala.util.Random()
    cs.map(c => (coarse(f, c, s, id), rnd.nextLong()) -> c).sortBy(_._1).take(cap).map(_._2)

  /** Priority from a rendering of the key rather than its digest. */
  private def renderDigest(render: Key => String): Sampler = (f, cs, cap, s, id) =>
    cs.map(c =>
      mixed(s, id, ContentHash.ofString(render(f)), ContentHash.ofString(render(c))) -> c
    ).sortBy(_._1)
      .take(cap)
      .map(_._2)

  val renderDigestNative: Sampler = renderDigest(_.toString)

  /** The two platforms' renderings of a whole `Double`, made explicit. */
  private def jvmStyle(d: Double): String =
    if d == d.floor then s"${d.toLong}.0" else d.toString
  private def jsStyle(d: Double): String = if d == d.floor then s"${d.toLong}" else d.toString

  private def renderWith(fmt: Double => String)(k: Key): String =
    s"Key(${k.participant},${k.item},${fmt(k.weight)})"

  val renderDigestJvm: Sampler = renderDigest(renderWith(jvmStyle))
  val renderDigestJs: Sampler  = renderDigest(renderWith(jsStyle))

  /** eyesim's procedure: sample from a pool that still contains the true
    * match, then remove it, so the realised count is `n` or `n - 1`.
    */
  val matchAfterSample: Sampler = (f, cs, cap, s, id) =>
    ranked(f, f +: cs, s, id).take(cap).filterNot(_ == f)

  // ---------------------------------------------------------------------------
  // Properties, each a function of a sampler
  // ---------------------------------------------------------------------------

  /** P-count: the realised count is min(cap, eligible), knowable in advance. */
  def pCount(sampler: Sampler): Prop =
    forAll(genKey, genPool, genCap) { (f, cs, cap) =>
      val n = sampler(f, cs, cap, seed, sample).size
      Prop(n == math.min(cap, cs.size)) :| s"realised $n, expected ${math.min(cap, cs.size)}"
    }

  /** P-prefix: raising the cap extends the selection; it never reshuffles. */
  def pPrefix(sampler: Sampler): Prop =
    forAll(genKey, genPool, genCap) { (f, cs, cap) =>
      val small = sampler(f, cs, cap, seed, sample)
      val large = sampler(f, cs, cap + 3, seed, sample)
      Prop(large.take(small.size) == small) :| "prefix"
    }

  /** P-order: the selection is a function of the keys, not of arrival order. */
  def pOrder(sampler: Sampler): Prop =
    forAll(genKey, genPool, genCap, genShuffle) { (f, cs, cap, shuffle) =>
      val a = sampler(f, cs, cap, seed, sample)
      val b = sampler(f, shuffle(cs), cap, seed, sample)
      Prop(a == b) :| s"original $a, shuffled $b"
    }

  /** P-focal: distinct focal keys draw distinct samples from the same pool. */
  def pFocal(sampler: Sampler): Prop =
    forAll(genPool, genKey, genKey) { (cs, f1, f2) =>
      val cap = 8
      Prop(f1 == f2 || cs.size <= cap) ||
      Prop(sampler(f1, cs, cap, seed, sample) != sampler(f2, cs, cap, seed, sample)) :|
        s"$f1 and $f2 drew the same sample"
    }

  /** P-repeat: the same inputs give the same output, with no hidden state. */
  def pRepeat(sampler: Sampler): Prop =
    forAll(genKey, genPool, genCap) { (f, cs, cap) =>
      val a = sampler(f, cs, cap, seed, sample)
      val b = sampler(f, cs, cap, seed, sample)
      Prop(a == b) :| s"first $a, second $b"
    }

  /** P-pin: the literal selection for a fixed seed, identical on JVM and JS.
    *
    * GOLDEN VECTOR. If a refactor changes this, every control sample drawn
    * before the change is no longer reproducible.
    */
  val pinned: Vector[Int] = Vector(23, 39, 58, 55, 54, 45, 21, 2, 29, 51)

  def pPin(sampler: Sampler): Boolean =
    sampler(focal, pool, 10, seed, sample).map(_.item) == pinned

  /** P-drift: the selection does not depend on how a Double is rendered. */
  def pDrift(jvm: Sampler, js: Sampler): Boolean =
    jvm(focal, pool, 10, seed, sample) == js(focal, pool, 10, seed, sample)

  // ---------------------------------------------------------------------------
  // The shipped sampler satisfies every property
  // ---------------------------------------------------------------------------

  property("shipped: P-count")(pCount(shipped))
  property("shipped: P-prefix")(pPrefix(shipped))
  property("shipped: P-order")(pOrder(shipped))
  property("shipped: P-focal")(pFocal(shipped))
  property("shipped: P-repeat")(pRepeat(shipped))

  test("shipped: P-pin, identical on JVM and Scala.js") {
    assertEquals(shipped(focal, pool, 10, seed, sample).map(_.item), pinned)
    // The pin is a prefix of a larger draw, so it is a fact about priorities.
    assertEquals(shipped(focal, pool, 25, seed, sample).map(_.item).take(10), pinned)
  }

  /** GOLDEN VECTORS for the digest path itself: the focal key's KeyDigest and
    * its priority against the first pool candidate. A change in KeyDigest or
    * in the priority mix is caught here directly, before it reaches a sample.
    */
  val pinnedFocalDigest: Long   = -452002883083538310L
  val pinnedFirstPriority: Long = 3119845174082442710L

  test("shipped: P-drift, the digest is of bits and not of a rendering") {
    // The fixture contains whole-valued weights (0.0, 1.0, 2.0, ...) whose
    // toString differs between platforms.
    assert(!pool.contains(focal))
    assert(jvmStyle(focal.weight) != jsStyle(focal.weight))
    assertEquals(pool.count(k => jvmStyle(k.weight) != jsStyle(k.weight)), 15)
    // The shipped selection is neither platform's rendering-derived one.
    val selected = shipped(focal, pool, 10, seed, sample)
    assertNotEquals(selected, renderDigestJvm(focal, pool, 10, seed, sample))
    assertNotEquals(selected, renderDigestJs(focal, pool, 10, seed, sample))
    // And the digest the selection is built from is pinned bit-for-bit.
    assertEquals(digest(focal).value, pinnedFocalDigest)
    assertEquals(Selection.priority(seed, sample, focal, pool.head), pinnedFirstPriority)
  }

  // ---------------------------------------------------------------------------
  // Every mutant is killed by the property that names it
  // ---------------------------------------------------------------------------

  test("mutant focal-blind: killed by P-focal") {
    assert(killed(pFocal(focalBlind)))
    // And it survives what it should, which is why P-focal is needed at all.
    assert(!killed(pCount(focalBlind)))
    assert(!killed(pOrder(focalBlind)))
  }

  test("mutant stream-sequenced: killed by P-order and P-focal") {
    assert(killed(pOrder(streamSequenced)))
    assert(killed(pFocal(streamSequenced)))
    assert(!killed(pCount(streamSequenced)))
    assert(!killed(pRepeat(streamSequenced)))
  }

  test("mutant arrival-order: killed by P-order and P-pin") {
    assert(killed(pOrder(arrivalOrder)))
    assert(!pPin(arrivalOrder))
    assert(!killed(pCount(arrivalOrder)))
    assert(!killed(pPrefix(arrivalOrder)))
  }

  test("mutant cap-short: killed by P-count") {
    assert(killed(pCount(capShort)))
    assert(!killed(pOrder(capShort)))
    assert(!killed(pPrefix(capShort)))
  }

  test("mutant cap-long: killed by P-count") {
    assert(killed(pCount(capLong)))
    assert(!killed(pOrder(capLong)))
    assert(!killed(pPrefix(capLong)))
  }

  test("mutant cap-in-priority: killed by P-prefix") {
    assert(killed(pPrefix(capInPriority)))
    assert(!killed(pCount(capInPriority)))
    assert(!killed(pOrder(capInPriority)))
    assert(!killed(pFocal(capInPriority)))
  }

  test("mutant tie-arrival: killed by P-order") {
    assert(killed(pOrder(tieArrival)))
    assert(!killed(pRepeat(tieArrival)))
    assert(!killed(pCount(tieArrival)))
  }

  test("mutant tie-random: killed by P-repeat") {
    assert(killed(pRepeat(tieRandom)))
    assert(!killed(pCount(tieRandom)))
  }

  test("mutant render-digest: killed by P-pin and P-drift") {
    assert(!pPin(renderDigestNative))
    // The same mutant fed the two platforms' renderings of the same keys
    // selects different controls: this is the drift P-pin exists to catch.
    assert(!pDrift(renderDigestJvm, renderDigestJs))
    // On whichever platform is running, the native rendering is one of the
    // two, so the mutant's answer here differs from its answer elsewhere.
    val native = renderDigestNative(focal, pool, 10, seed, sample)
    val jvm    = renderDigestJvm(focal, pool, 10, seed, sample)
    val js     = renderDigestJs(focal, pool, 10, seed, sample)
    assert(native == jvm || native == js, clue((native, jvm, js)))
    // It is otherwise a well-behaved sampler, so nothing but the pin sees it.
    assert(!killed(pCount(renderDigestNative)))
    assert(!killed(pOrder(renderDigestNative)))
    assert(!killed(pPrefix(renderDigestNative)))
  }

  test("mutant match-after-sample: killed by P-count") {
    assert(killed(pCount(matchAfterSample)))
    // eyesim's n-or-(n-1): the count is never MORE than the cap, only short.
    // Under the fixed seed the focal ranks within the first 60 of the 61-key
    // pool it was prepended to, so some cap in 1..60 is short by one and the
    // `exists` below is deterministic rather than probabilistic.
    val counts = (1 to 60).map(cap => matchAfterSample(focal, pool, cap, seed, sample).size)
    assert(counts.zipWithIndex.forall { case (n, i) => n == i + 1 || n == i })
    assert(counts.zipWithIndex.exists { case (n, i) => n == i })
  }

  // ---------------------------------------------------------------------------
  // The pairing path uses the same sampler, so the pin holds end to end
  // ---------------------------------------------------------------------------

  test("bottom-k through pair selects the pinned candidates per focal") {
    val odd    = Projection.named[Key, Boolean]("odd")(k => k.item % 2 != 0)
    val left   = Trials(Vector(Trial(focal, (), 0)))
    val right  = Trials(pool.map(k => Trial(k, (), k.item)))
    val design = Pairing
      .between[Key, Key]
      .bottomK(10, seed, sample)
      .fold(e => fail(e.message), identity)
    val paired = pair(left, right, design)
    assertEquals(paired.pairs.map(_._2.key.item), pinned)
    assertEquals(paired.eligiblePairCount, 60L)
    // Restricting the relation restricts the pool BEFORE sampling (the focal's
    // item is -1, so it is odd), and the survivors keep their priority order.
    val restricted = pair(
      left,
      right,
      PairDesign.BetweenDirected(Relation.SameOn(odd, odd), design.selection)
    )
    val oddPinned = pinned.filter(_ % 2 != 0)
    assertEquals(oddPinned.size, 7)
    assertEquals(restricted.pairs.map(_._2.key.item).take(7), oddPinned)
    assertEquals(restricted.pairs.size, 10)
    assertEquals(restricted.eligiblePairCount, 30L)
  }

end SamplingMutationSuite
