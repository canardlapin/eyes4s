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

package eyes4s.laws

import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Norm

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Prop}
import org.typelevel.discipline.Laws

/** Law suite for [[Warp]] as a **partial** category.
  *
  * ==Why partial, and what that changes==
  *
  * Cats' `CategoryTests` cannot be reused here, because `andThen` is not total:
  * it returns `Either` and refuses when the frames do not meet. The category
  * laws hold on the subcategory where frame identities align, so that is where
  * they are stated. Reusing the total bundle would require a composition that
  * always succeeds, which would mean deleting the check these types exist for.
  *
  * ==Equality is extensional==
  *
  * Two warps are equal when they map the same positions to the same results.
  * There is no structural equality to test against: `id.andThen(f)` builds a
  * `Then(Id, f)` node, which is a *different value* from `f` and must remain so
  * -- the whole point of retaining structure is that the tree records how a
  * transform was assembled. So the laws compare behaviour over sampled points,
  * with an explicit [[Tolerance]].
  */
trait WarpLaws extends Laws:

  import Generators.given
  import Generators.*

  /** Number of positions sampled when comparing two warps extensionally. */
  def samplesPerCase: Int = 16

  private def sameOn[U <: Unit2D, V <: Unit2D](
      f: Warp[U, V],
      g: Warp[U, V],
      tol: Tolerance
  ): Gen[Prop] =
    Gen.listOfN(samplesPerCase, genPtIn(f.from)).map { pts =>
      Prop.all(pts.map { p =>
        (f(p), g(p)) match
          case (Some(a), Some(b)) => Prop(tol.approxEquals(a, b))
          case (None, None)       => Prop(true)
          case _                  => Prop(false)
      }*)
    }

  /** The category laws, on the subcategory where frames align. */
  def category(tol: Tolerance = Tolerance.exactish): RuleSet =
    new SimpleRuleSet(
      "warp.partialCategory",
      "left identity" -> forAll { (ch: Chain[Norm]) =>
        val composed = Warp.id(ch.a).andThen(ch.f)
        composed.isRight && Prop.forAll(sameOn(composed.toOption.get, ch.f, tol))(identity)
      },
      "right identity" -> forAll { (ch: Chain[Norm]) =>
        val composed = ch.f.andThen(Warp.id(ch.b))
        composed.isRight && Prop.forAll(sameOn(composed.toOption.get, ch.f, tol))(identity)
      },
      "associativity" -> forAll { (ch: Chain[Norm]) =>
        val left  = ch.f.andThen(ch.g).flatMap(_.andThen(ch.h))
        val right = ch.g.andThen(ch.h).flatMap(ch.f.andThen)
        (left, right) match
          case (Right(l), Right(r)) => Prop.forAll(sameOn(l, r, tol))(identity)
          case _                    => Prop(false) :| "a constructed chain failed to compose"
      },
      "composition preserves endpoints" -> forAll { (ch: Chain[Norm]) =>
        val composed = ch.f.andThen(ch.g).toOption.get
        Prop(composed.from.id == ch.a.id) && Prop(composed.to.id == ch.c.id)
      }
    )

  /** Composition must refuse whenever the frames do not meet.
    *
    * The negative half of the law. Without it, a `Warp` whose `andThen` always
    * returned `Right` would satisfy every rule above.
    */
  def compositionRefusesMismatch: RuleSet =
    new SimpleRuleSet(
      "warp.frameCheck",
      "refuses when the frames do not meet" -> forAll {
        (f: Frame[Norm], g: Frame[Norm], h: Frame[Norm]) =>
          val w1 = Warp.rescale(f, g).toOption.get
          val w2 = Warp.rescale(h, f).toOption.get
          val r  = w1.andThen(w2)
          // Composable exactly when w1's target frame is w2's source frame.
          Agreement.frames(g, h) match
            case Right(_) =>
              Prop(r.isRight) :| s"refused a valid composition: ${g.id}"
            case Left(expected) =>
              Prop(r == Left(expected)) :|
                s"accepted incompatible frames ${g.id} -> ${h.id}: ${expected.message}"
      }
    )

  /** Inverses, where they exist, undo the transform. */
  def inverses(tol: Tolerance = Tolerance.exactish): RuleSet =
    new SimpleRuleSet(
      "warp.inverse",
      "inverse round-trips" -> forAll { (ch: Chain[Norm]) =>
        ch.f.inverse match
          case None       => Prop(true) :| "no inverse to test"
          case Some(back) =>
            val there = ch.f.andThen(back)
            Prop(there.isRight) && Prop.forAll(
              sameOn(there.toOption.get, Warp.id(ch.a), tol)
            )(identity)
      },
      "inverse swaps the endpoints" -> forAll { (ch: Chain[Norm]) =>
        ch.f.inverse match
          case None       => Prop(true)
          case Some(back) => Prop(back.from.id == ch.b.id) && Prop(back.to.id == ch.a.id)
      },
      "a composite inverts in reverse order" -> forAll { (ch: Chain[Norm]) =>
        val composed = ch.f.andThen(ch.g).toOption.get
        composed.inverse match
          case None       => Prop(true)
          case Some(back) =>
            Prop(back.from.id == ch.c.id) && Prop(back.to.id == ch.a.id)
      }
    )

  /** The projection between a linear unit and an angular one round-trips.
    *
    * This is the acceptance criterion named for milestone v0.1, stated as a
    * property over generated display geometries rather than as one example.
    */
  def tangentRoundTrip(tol: Tolerance = Tolerance.roundTrip): RuleSet =
    new SimpleRuleSet(
      "warp.tangent",
      "linear -> angular -> linear" -> forAll { (s: TangentSetup) =>
        val back = s.warp.inverse.get
        Prop.forAll(Gen.listOfN(samplesPerCase, genPtIn(s.linear))) { pts =>
          Prop.all(pts.map { p =>
            s.warp(p).flatMap(back.apply) match
              case Some(r) => Prop(tol.approxEquals(r, p)) :| s"$p -> $r"
              case None    => Prop(false) :| s"round trip undefined at $p"
          }*)
        }
      },
      "the centre has zero eccentricity" -> forAll { (s: TangentSetup) =>
        s.warp(s.linear.centre) match
          case Some(c) => Prop(tol.approxEquals(c, s.angular.centre))
          case None    => Prop(false)
      },
      "eccentricity grows monotonically with offset" -> forAll { (s: TangentSetup) =>
        val c  = s.linear.centre
        val q1 = Pt[Unit2D.Px](c.x + s.linear.width * 0.1, c.y)
        val q2 = Pt[Unit2D.Px](c.x + s.linear.width * 0.3, c.y)
        (s.warp(q1), s.warp(q2)) match
          case (Some(a), Some(b)) => Prop(math.abs(b.x) > math.abs(a.x))
          case _                  => Prop(false)
      }
    )

end WarpLaws

object WarpLaws extends WarpLaws

/** Law suite for [[Machine]] composition.
  *
  * ==Observational, and this one could not be otherwise==
  *
  * Composition pairs state types, so `id.andThen(f)` has state `(Unit, S)`
  * where `f` has `S`. Those are isomorphic and never equal, and making them
  * equal would mean erasing the composition the type records. A suite comparing
  * machines structurally would therefore fail for a correct implementation, and
  * the temptation would be to weaken it until it passed.
  *
  * So the laws are stated over the output sequences `runAll` produces, which is
  * both true and the thing a caller actually depends on.
  */
trait MachineLaws extends Laws:

  /** @param gen  inputs to drive the machines with
    * @param mk   machines to compose; supplied by the caller so a downstream
    *             author can test their own detectors under these laws
    */
  def category[A](
      gen: Gen[List[A]],
      mk: Gen[Machine[A, A]]
  ): RuleSet =
    new SimpleRuleSet(
      "machine.category",
      "left identity" -> forAll(gen, mk) { (in, f) =>
        Prop(Machine.identity[A].andThen(f).runAll(in) == f.runAll(in))
      },
      "right identity" -> forAll(gen, mk) { (in, f) =>
        Prop(f.andThen(Machine.identity[A]).runAll(in) == f.runAll(in))
      },
      "associativity" -> forAll(gen, mk, mk, mk) { (in, f, g, h) =>
        Prop(f.andThen(g).andThen(h).runAll(in) == f.andThen(g.andThen(h)).runAll(in))
      },
      "the Category instance agrees with andThen" -> forAll(gen, mk, mk) { (in, f, g) =>
        val C = summon[cats.arrow.Category[Machine]]
        Prop(C.compose(g, f).runAll(in) == f.andThen(g).runAll(in))
      },
      "a composite is a different value from its parts" -> forAll(mk) { f =>
        // The reason these laws are observational rather than structural.
        Prop(!Machine.identity[A].andThen(f).equals(f))
      }
    )

  /** Running is deterministic: the same machine over the same input twice. */
  def deterministic[A, B](gen: Gen[List[A]], mk: Gen[Machine[A, B]]): RuleSet =
    new SimpleRuleSet(
      "machine.deterministic",
      "the same input yields the same output" -> forAll(gen, mk) { (in, f) =>
        Prop(f.runAll(in) == f.runAll(in))
      },
      "a fresh machine is not affected by a previous run" -> forAll(gen, gen, mk) { (a, b, f) =>
        val first = f.runAll(a)
        val _     = f.runAll(b)
        Prop(f.runAll(a) == first)
      }
    )

end MachineLaws

object MachineLaws extends MachineLaws
