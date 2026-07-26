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

import eyes4s.compare.*

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Prop}
import org.typelevel.discipline.Laws

/** Law suites for the [[Compare]] hierarchy.
  *
  * ==The interface is a claim; this is the audit==
  *
  * A measure that extends [[Metric]] is asserting three things about itself,
  * and nothing in the type system checks them. These rule sets do. Publishing
  * them main-scope means an author adding a measure -- theirs or ours -- runs
  * the same audit rather than writing three ad-hoc assertions and hoping they
  * chose representative inputs.
  *
  * ==Separation is not optional, and omitting it made the suite vacuous==
  *
  * A full metric also requires `d(x, y) > 0` whenever `x != y`. The first
  * version of this file left that out, reasoning that distinctness for a
  * surface is a tolerance question rather than a structural one. Mutation
  * testing then showed what the omission cost: deleting the absolute value from
  * total variation makes it return exactly zero for every pair -- both masses
  * sum to one, so the signed difference cancels -- and the constant-zero
  * function satisfies identity, non-negativity, symmetry AND the triangle
  * inequality. Every law passed for a measure that had stopped measuring.
  *
  * Separation is therefore a required argument: the caller supplies a pair it
  * knows to be genuinely different, and the law asserts the measure puts real
  * daylight between them.
  *
  * A weaker form was tried first -- assert the measure produces more than one
  * distinct value across a sample -- and it also failed to catch the mutant.
  * Removing the absolute value does not give exactly zero in floating point; it
  * gives a scatter of values around 1e-17, which are all distinct. Testing for
  * distinctness rather than magnitude let rounding dust masquerade as signal,
  * the same failure this project has now hit three times.
  */
trait MeasureLaws extends Laws:

  private def value(d: Distance0): Double = d.value

  /** Identity, symmetry, non-negativity, triangle inequality. */
  def metric[A](
      m: Metric[A],
      gen: Gen[A],
      distinct: (A, A),
      minSeparation: Double = 1e-6,
      tol: Tolerance = Tolerance.roundTrip
  ): RuleSet =
    new SimpleRuleSet(
      s"metric.${m.info.name}",
      "zero on identical inputs" -> forAll(gen) { a =>
        m.compare(a, a) match
          case Right(d) => Prop(tol.approxEquals(value(d), 0.0)) :| s"d(x,x) = ${value(d)}"
          case Left(e)  => Prop(false) :| e.message
      },
      "non-negative" -> forAll(gen, gen) { (a, b) =>
        m.compare(a, b) match
          case Right(d) => Prop(value(d) >= -1e-12) :| s"d = ${value(d)}"
          case Left(_)  => Prop(true) :| "undefined pairs are not a law violation"
      },
      "symmetric" -> forAll(gen, gen) { (a, b) =>
        (m.compare(a, b), m.compare(b, a)) match
          case (Right(x), Right(y)) => Prop(tol.approxEquals(value(x), value(y)))
          case (Left(_), Left(_))   => Prop(true)
          case _                    => Prop(false) :| "defined in one direction only"
      },
      "triangle inequality" -> forAll(gen, gen, gen) { (a, b, c) =>
        (m.compare(a, c), m.compare(a, b), m.compare(b, c)) match
          case (Right(ac), Right(ab), Right(bc)) =>
            Prop(value(ac) <= value(ab) + value(bc) + 1e-9) :|
              s"${value(ac)} > ${value(ab)} + ${value(bc)}"
          case _ => Prop(true)
      },
      "separates inputs the caller knows to be different" ->
        separates(m.compare, distinct, minSeparation)
    )

  /** The measure puts real daylight between two genuinely different inputs.
    *
    * The guard against a suite passing for a measure that has stopped
    * measuring. Every other axiom is satisfied by the zero function -- and, as
    * it turns out, by a function returning rounding noise -- so the threshold
    * is on MAGNITUDE, not on distinctness.
    */
  private def separates[A](
      f: (A, A) => Either[CompareError, Distance0],
      distinct: (A, A),
      minSeparation: Double
  ): Prop =
    f(distinct._1, distinct._2) match
      case Right(d) =>
        Prop(value(d) > minSeparation) :|
          s"distinct inputs gave ${value(d)}, below the $minSeparation floor -- " +
          "the measure may have stopped measuring"
      case Left(e) => Prop(false) :| e.message

  /** A semimetric drops only the triangle inequality.
    *
    * Stated as its own rule set rather than as "metric minus one property", so
    * that a measure declaring itself a semimetric is still audited for the
    * three properties it does claim.
    */
  def semimetric[A](
      m: Semimetric[A],
      gen: Gen[A],
      distinct: (A, A),
      minSeparation: Double = 1e-6,
      tol: Tolerance = Tolerance.roundTrip
  ): RuleSet =
    new SimpleRuleSet(
      s"semimetric.${m.info.name}",
      "zero on identical inputs" -> forAll(gen) { a =>
        m.compare(a, a) match
          case Right(d) => Prop(tol.approxEquals(value(d), 0.0))
          case Left(e)  => Prop(false) :| e.message
      },
      "non-negative" -> forAll(gen, gen) { (a, b) =>
        m.compare(a, b).forall(d => value(d) >= -1e-12)
      },
      "symmetric" -> forAll(gen, gen) { (a, b) =>
        (m.compare(a, b), m.compare(b, a)) match
          case (Right(x), Right(y)) => Prop(tol.approxEquals(value(x), value(y)))
          case (Left(_), Left(_))   => Prop(true)
          case _                    => Prop(false)
      },
      "separates inputs the caller knows to be different" ->
        separates(m.compare, distinct, minSeparation)
    )

  /** The one law a [[SymmetricCompare]] promises, for any score type.
    *
    * Required by PRD C-9 for every instance: an unordered pair evaluation
    * depends on this and nothing else.
    */
  def symmetry[A, S](
      c: SymmetricCompare[A, S],
      gen: Gen[A],
      same: (S, S) => Boolean
  ): RuleSet =
    new SimpleRuleSet(
      s"symmetric.${c.info.name}",
      "the order of the arguments does not matter" -> forAll(gen, gen) { (a, b) =>
        (c.compare(a, b), c.compare(b, a)) match
          case (Right(x), Right(y)) => Prop(same(x, y)) :| s"$x vs $y"
          case (Left(_), Left(_))   => Prop(true)
          case _                    => Prop(false) :| "defined in one direction only"
      },
      "not degenerate" -> Prop.forAll(Gen.listOfN(8, gen)) { xs =>
        val vs = for
          a <- xs
          b <- xs
          v <- c.compare(a, b).toOption
        yield v
        Prop(vs.exists(v => !same(v, vs.head))) :| "every comparison gave the same value"
      }
    )

  /** A divergence promises identity and non-negativity, and NOT symmetry.
    *
    * Symmetry is deliberately not asserted either way. Some divergences happen
    * to be symmetric on some inputs, and a law forbidding it would be as wrong
    * as one requiring it. What matters is that the type does not advertise it.
    */
  def divergence[A](
      d: Divergence[A],
      gen: Gen[A],
      distinct: (A, A),
      minSeparation: Double = 1e-6,
      tol: Tolerance = Tolerance.roundTrip
  ): RuleSet =
    new SimpleRuleSet(
      s"divergence.${d.info.name}",
      "zero on identical inputs" -> forAll(gen) { a =>
        d.compare(a, a) match
          case Right(v) => Prop(tol.approxEquals(value(v), 0.0)) :| s"D(x,x) = ${value(v)}"
          case Left(e)  => Prop(false) :| e.message
      },
      "non-negative" -> forAll(gen, gen) { (a, b) =>
        d.compare(a, b).forall(v => value(v) >= -1e-12)
      },
      "separates inputs the caller knows to be different" ->
        separates(d.compare, distinct, minSeparation)
    )

  /** Every measure can describe itself well enough for an application to
    * present it (PRD APP-15).
    */
  def described(c: Compare[?, ?, ?]): RuleSet =
    new SimpleRuleSet(
      s"info.${c.info.name}",
      "has a name"       -> Prop(c.info.name.nonEmpty),
      "has a summary"    -> Prop(c.info.summary.nonEmpty),
      "renders"          -> Prop(c.info.render.nonEmpty),
      "declares a scale" -> Prop(c.info.scale.render.nonEmpty)
    )

end MeasureLaws

object MeasureLaws extends MeasureLaws
