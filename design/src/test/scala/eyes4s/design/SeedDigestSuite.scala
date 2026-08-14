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

class SeedDigestSuite extends munit.FunSuite:

  // ---------------------------------------------------------------------------
  // Seed and the generator
  // ---------------------------------------------------------------------------

  test("the same seed gives the same stream") {
    val a  = Seed(42L).generator
    val b  = Seed(42L).generator
    val xs = Vector.fill(20)(a.nextLong())
    val ys = Vector.fill(20)(b.nextLong())
    assertEquals(xs, ys)
  }

  test("different seeds give different streams") {
    val a = Vector.fill(20)(Seed(1L).generator.nextLong())
    val b = Vector.fill(20)(Seed(2L).generator.nextLong())
    assertNotEquals(a, b)
  }

  /** GOLDEN VECTOR.
    *
    * These exact values must appear on every platform and in every future
    * release. If this test fails after a refactor, control samples drawn before
    * the change are no longer reproducible -- which is a far worse outcome than
    * a failing test, and the reason to pin the numbers rather than a property.
    */
  test("the generator's stream is pinned, bit for bit") {
    val g = Seed(0L).generator
    assertEquals(
      Vector.fill(4)(g.nextLong()),
      Vector(
        -2152535657050944081L,
        7960286522194355700L,
        487617019471545679L,
        -537132696929009172L
      )
    )
  }

  test("a derived seed's stream is pinned too") {
    // Derivation feeds sampling identity, so it needs the same protection: a
    // refactor that changes it silently invalidates every control sample ever
    // drawn.
    val g     = Seed(1L).derive("stratum-a").generator
    val first = g.nextLong()
    assertEquals(Seed(1L).derive("stratum-a").generator.nextLong(), first)
    assertNotEquals(Seed(1L).derive("stratum-b").generator.nextLong(), first)
  }

  test("uniform draws lie in the unit interval") {
    val g  = Seed(7L).generator
    val xs = Vector.fill(500)(g.nextDouble())
    assert(xs.forall(x => x >= 0.0 && x < 1.0), clue(xs.filter(x => x < 0.0 || x >= 1.0)))
    // Not a distribution test, just a sanity check that it is not constant.
    assert(xs.distinct.length > 400)
  }

  test("bounded integers stay in range") {
    val g     = Seed(3L).generator
    val draws = (0 until 1000).map(_ => g.nextInt(10).toOption.get)
    assert(draws.forall(i => i >= 0 && i < 10))
  }

  test("an invalid random bound is a named failure, not the sentinel zero") {
    val g = Seed(3L).generator
    assertEquals(g.nextInt(0), Left(RngError.NonPositiveBound(0)))
    assert(clue(g.nextInt(-2).left.toOption.get.message).contains("-2"))
  }

  test("derivation is by label, so adding a stratum does not reshuffle the others") {
    val base = Seed(99L)
    val a1   = base.derive("participant-01")
    val a2   = base.derive("participant-01")
    val b    = base.derive("participant-02")
    assertEquals(a1.value, a2.value)
    assertNotEquals(a1.value, b.value)
  }

  test("a derived seed is independent of derivation order") {
    // The property that makes results independent of how many strata were
    // processed first -- which a shared, advancing generator cannot give.
    val base = Seed(5L)
    val x    = base.derive("a")
    val _    = base.derive("b")
    val _    = base.derive("c")
    assertEquals(base.derive("a").value, x.value)
  }

  // ---------------------------------------------------------------------------
  // KeyDigest
  // ---------------------------------------------------------------------------

  final case class TrialKey(subject: String, image: String) derives CanEqual
  final case class Nested(key: TrialKey, block: Int) derives CanEqual

  test("equal keys digest equally") {
    val d = KeyDigest.derived[TrialKey]
    assertEquals(d.digest(TrialKey("s1", "img")), d.digest(TrialKey("s1", "img")))
  }

  test("different keys digest differently") {
    val d = KeyDigest.derived[TrialKey]
    assertNotEquals(d.digest(TrialKey("s1", "a")), d.digest(TrialKey("s1", "b")))
    assertNotEquals(d.digest(TrialKey("s1", "a")), d.digest(TrialKey("s2", "a")))
  }

  test("field boundaries are respected, so a shift does not collide") {
    // The classic failure of concatenating without separators: ("ab","c") and
    // ("a","bc") would hash identically.
    val d = KeyDigest.derived[TrialKey]
    assertNotEquals(d.digest(TrialKey("ab", "c")), d.digest(TrialKey("a", "bc")))
  }

  test("field POSITION is respected, so swapping roles does not collide") {
    val d = KeyDigest.derived[TrialKey]
    assertNotEquals(d.digest(TrialKey("x", "y")), d.digest(TrialKey("y", "x")))
  }

  test("the domain tag separates types that share a representation") {
    assertNotEquals(KeyDigest[Int].digest(1), KeyDigest[Long].digest(1L))
    assertNotEquals(KeyDigest[String].digest("1"), KeyDigest[Int].digest(1))
    assertNotEquals(KeyDigest[Boolean].digest(true), KeyDigest[String].digest("1"))
  }

  test("an absent option differs from a present one") {
    val d = summon[KeyDigest[Option[String]]]
    assertNotEquals(d.digest(None), d.digest(Some("")))
  }

  test("nested products digest structurally") {
    val d = KeyDigest.derived[Nested]
    assertEquals(
      d.digest(Nested(TrialKey("s", "i"), 1)),
      d.digest(Nested(TrialKey("s", "i"), 1))
    )
    assertNotEquals(
      d.digest(Nested(TrialKey("s", "i"), 1)),
      d.digest(Nested(TrialKey("s", "i"), 2))
    )
  }

  test("doubles digest by bit pattern, not by rendering") {
    // Double.toString differs between the JVM and Scala.js. A digest built from
    // it would make a control sample non-reproducible across platforms.
    assertEquals(KeyDigest[Double].digest(4.0), KeyDigest[Double].digest(4.0))
    assertNotEquals(KeyDigest[Double].digest(4.0), KeyDigest[Double].digest(4.5))
  }

  test("an unsupported field type is rejected at compile time") {
    val errors = typeCheckErrors("""
      import eyes4s.design.*
      final case class Bad(x: java.util.Date)
      KeyDigest.derived[Bad]
    """)
    assert(errors.nonEmpty, "unsupported key fields reached the pure digest path")
    assert(
      clue(errors.map(_.message).mkString("\n")).contains("KeyDigest[java.util.Date]"),
      "the compile error should name the missing field instance"
    )
  }

end SeedDigestSuite
