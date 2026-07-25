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

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Prop}
import org.typelevel.discipline.Laws

/** Law suite for [[Region]] as a Boolean algebra.
  *
  * ==Observational, necessarily==
  *
  * `Union(a, a)` is a different *value* from `a` and must remain so: the tree
  * records how the region was built, which is what lets an application show and
  * edit it. So the laws compare **membership** over sampled positions rather
  * than comparing regions structurally. Testing structural equality here would
  * be testing that the library throws away the information it exists to keep.
  *
  * Sampling is at the grid's cell centres, which makes coverage a property of
  * the caller's grid rather than a hidden constant.
  */
trait RegionLaws extends Laws:

  import Generators.*

  private def sameMembership[U <: Unit2D](
      g: Grid[U],
      a: Region[U],
      b: Region[U]
  ): Prop =
    Prop.all((0 until g.size).map { i =>
      val p = g.cellCentre(i)
      Prop(a.contains(p) == b.contains(p)) :| s"differ at cell $i"
    }*)

  def lattice[U <: Unit2D](g: Grid[U]): RuleSet =
    val gen = genRegion(g.frame)

    new SimpleRuleSet(
      "region.booleanAlgebra",
      "union is commutative" -> forAll(gen, gen) { (a, b) =>
        sameMembership(g, a || b, b || a)
      },
      "intersection is commutative" -> forAll(gen, gen) { (a, b) =>
        sameMembership(g, a && b, b && a)
      },
      "union is associative" -> forAll(gen, gen, gen) { (a, b, c) =>
        sameMembership(g, (a || b) || c, a || (b || c))
      },
      "intersection is associative" -> forAll(gen, gen, gen) { (a, b, c) =>
        sameMembership(g, (a && b) && c, a && (b && c))
      },
      "union is idempotent"        -> forAll(gen)(a => sameMembership(g, a || a, a)),
      "intersection is idempotent" -> forAll(gen)(a => sameMembership(g, a && a, a)),
      "absorption"                 -> forAll(gen, gen) { (a, b) =>
        sameMembership(g, a || (a && b), a) && sameMembership(g, a && (a || b), a)
      },
      "distributivity" -> forAll(gen, gen, gen) { (a, b, c) =>
        sameMembership(g, a && (b || c), (a && b) || (a && c)) &&
        sameMembership(g, a || (b && c), (a || b) && (a || c))
      },
      "empty is the unit of union" -> forAll(gen) { a =>
        sameMembership(g, a || Region.empty[U], a)
      },
      "everything is the unit of intersection" -> forAll(gen) { a =>
        sameMembership(g, a && Region.everything[U], a)
      },
      "complement" -> forAll(gen) { a =>
        sameMembership(g, a || !a, Region.everything[U]) &&
        sameMembership(g, a && !a, Region.empty[U])
      },
      "double negation" -> forAll(gen)(a => sameMembership(g, !(!a), a)),
      "de Morgan"       -> forAll(gen, gen) { (a, b) =>
        sameMembership(g, !(a || b), (!a) && (!b)) &&
        sameMembership(g, !(a && b), (!a) || (!b))
      },
      "difference is intersection with the complement" -> forAll(gen, gen) { (a, b) =>
        sameMembership(g, a \ b, a && (!b))
      },
      "rasterise agrees with contains" -> forAll(gen) { a =>
        val bits = a.rasterise(g)
        Prop.all((0 until g.size).map { i =>
          Prop(bits(i) == a.contains(g.cellCentre(i)))
        }*)
      }
    )

end RegionLaws

object RegionLaws extends RegionLaws

/** Law suites for the occupancy layer: the module on signed surfaces, the
  * invariants a [[Mass]] claims, and the identity that makes [[PointMeasure]]
  * a measure rather than a bag of numbers.
  */
trait SurfaceLaws extends Laws:

  import Generators.*

  private def close(a: Double, b: Double, tol: Tolerance): Boolean =
    tol.approxEquals(a, b)

  private def sameValues[U <: Unit2D](
      a: Surface[U],
      b: Surface[U],
      tol: Tolerance
  ): Prop =
    Prop(a.size == b.size) :| "sizes differ" && Prop.all(
      (0 until a.size).map(i => Prop(close(a.at(i), b.at(i), tol)) :| s"differ at $i")*
    )

  /** The module laws, on a fixed grid.
    *
    * Fixed rather than generated because the module *belongs to* a grid: its
    * zero is dimensioned by that grid. Quantifying over grids inside the law
    * would be quantifying over which module is being tested.
    */
  def module[U <: Unit2D](g: Grid[U], tol: Tolerance = Tolerance.exactish): RuleSet =
    val M   = g.signedModule
    val gen = genSigned(g)

    new SimpleRuleSet(
      "surface.module",
      "addition is associative" -> forAll(gen, gen, gen) { (a, b, c) =>
        sameValues(M.plus(M.plus(a, b), c), M.plus(a, M.plus(b, c)), tol)
      },
      "addition is commutative" -> forAll(gen, gen) { (a, b) =>
        sameValues(M.plus(a, b), M.plus(b, a), tol)
      },
      "zero is the additive unit" -> forAll(gen) { a =>
        sameValues(M.plus(a, M.zero), a, tol) && sameValues(M.plus(M.zero, a), a, tol)
      },
      "negate is the additive inverse" -> forAll(gen) { a =>
        sameValues(M.plus(a, M.negate(a)), M.zero, tol)
      },
      "scaling by one is the identity" -> forAll(gen) { a =>
        sameValues(M.scale(1.0, a), a, tol)
      },
      "scaling by zero gives zero" -> forAll(gen) { a =>
        sameValues(M.scale(0.0, a), M.zero, tol)
      },
      "scaling distributes over addition" -> forAll(gen, gen, Gen.choose(-5.0, 5.0)) {
        (a, b, k) =>
          sameValues(M.scale(k, M.plus(a, b)), M.plus(M.scale(k, a), M.scale(k, b)), tol)
      },
      "scaling distributes over scalar addition" -> forAll(
        gen,
        Gen.choose(-5.0, 5.0),
        Gen.choose(-5.0, 5.0)
      ) { (a, j, k) =>
        sameValues(M.scale(j + k, a), M.plus(M.scale(j, a), M.scale(k, a)), tol)
      },
      "scaling is associative" -> forAll(gen, Gen.choose(-5.0, 5.0), Gen.choose(-5.0, 5.0)) {
        (a, j, k) => sameValues(M.scale(j, M.scale(k, a)), M.scale(j * k, a), tol)
      },
      "zero is dimensioned by its grid" -> Prop(M.zero.size == g.size)
    )

  /** What a [[Mass]] guarantees, and what the operations on it preserve. */
  def mass[U <: Unit2D](g: Grid[U], tol: Tolerance = Tolerance.roundTrip): RuleSet =
    val gm = genMass(g)
    val gi = genIntensity(g)

    new SimpleRuleSet(
      "surface.mass",
      "normalising an intensity yields unit total" -> forAll(gi) { i =>
        i.normalised match
          case Left(e)  => Prop(false) :| e.message
          case Right(m) => Prop(close(m.sum, 1.0, tol)) :| s"sum was ${m.sum}"
      },
      "normalising preserves relative magnitudes" -> forAll(gi) { i =>
        val m = i.normalised.toOption.get
        val t = i.sum
        Prop.all((0 until g.size).map(k => Prop(close(m.at(k), i.at(k) / t, tol)))*)
      },
      "normalising is idempotent in effect" -> forAll(gi) { i =>
        val m = i.normalised.toOption.get
        // A Mass cannot be normalised again -- that does not compile -- so the
        // property is that re-normalising the same VALUES changes nothing.
        val again = Surface
          .intensity(g, m.values, m.provenance)
          .flatMap(_.normalised)
          .toOption
          .get
        sameValues(again, m, tol)
      },
      "every mass is non-negative" -> forAll(gm) { m =>
        Prop.all((0 until g.size).map(k => Prop(m.at(k) >= 0.0))*)
      },
      "the difference of two masses sums to zero" -> forAll(gm, gm) { (a, b) =>
        a.difference(b) match
          case Left(e)  => Prop(false) :| e.message
          case Right(d) => Prop(close(d.sum, 0.0, tol)) :| s"sum was ${d.sum}"
      },
      "a mass differs from itself by nothing" -> forAll(gm) { m =>
        val d = m.difference(m).toOption.get
        Prop.all((0 until g.size).map(k => Prop(close(d.at(k), 0.0, tol)))*)
      },
      "log ratio against itself is zero" -> forAll(gm) { m =>
        val r = m.logRatio(m).toOption.get
        Prop.all((0 until g.size).map(k => Prop(close(r.at(k), 0.0, tol)))*)
      },
      "the mean of masses is a mass" -> forAll(gm, gm) { (a, b) =>
        Mass.mean(Seq(a, b)) match
          case Left(e)  => Prop(false) :| e.message
          case Right(m) => Prop(close(m.sum, 1.0, tol))
      },
      "the mean of one mass is that mass" -> forAll(gm) { m =>
        sameValues(Mass.mean(Seq(m)).toOption.get, m, tol)
      },
      "weighted mean with equal weights is the plain mean" -> forAll(gm, gm) { (a, b) =>
        val plain    = Mass.mean(Seq(a, b)).toOption.get
        val weighted = Mass.weightedMean(Seq((2.0, a), (2.0, b))).toOption.get
        sameValues(weighted, plain, tol)
      },
      "entropy is bounded below by zero and above by log of the cell count" -> forAll(gm) { m =>
        val h   = m.entropy().value
        val max = math.log(g.size.toDouble)
        Prop(h >= -1e-12) :| s"entropy $h" && Prop(h <= max + 1e-9) :| s"entropy $h > $max"
      },
      "relative entropy lies in the unit interval" -> forAll(gm) { m =>
        val r = m.relativeEntropy()
        Prop(r >= -1e-12 && r <= 1.0 + 1e-9) :| s"relative entropy $r"
      },
      "entropy does not depend on the base beyond a constant factor" -> forAll(gm) { m =>
        val nats = m.entropy(LogBase.E).value
        val bits = m.entropy(LogBase.Two).value
        Prop(close(bits * math.log(2.0), nats, tol))
      }
    )

  /** The identity that makes a [[PointMeasure]] a measure. */
  def measure[U <: Unit2D](g: Grid[U], tol: Tolerance = Tolerance.roundTrip): RuleSet =
    val gm = genPointMeasure(g.frame)
    val gr = genRegion(g.frame)

    new SimpleRuleSet(
      "measure.integrate",
      "mass in a region is the integral of its indicator" -> forAll(gm, gr) { (mu, r) =>
        val viaIndicator = mu.integrate(p => if r.contains(p) then 1.0 else 0.0)
        Prop(close(mu.massIn(r), viaIndicator, tol))
      },
      "integration is additive over the integrand" -> forAll(gm, gr, gr) { (mu, a, b) =>
        def ind(r: Region[U])(p: Pt[U]): Double = if r.contains(p) then 1.0 else 0.0
        val separate                            = mu.integrate(ind(a)) + mu.integrate(ind(b))
        val together                            = mu.integrate(p => ind(a)(p) + ind(b)(p))
        Prop(close(separate, together, tol))
      },
      "inclusion and exclusion holds" -> forAll(gm, gr, gr) { (mu, a, b) =>
        val lhs = mu.massIn(a || b)
        val rhs = mu.massIn(a) + mu.massIn(b) - mu.massIn(a && b)
        Prop(close(lhs, rhs, tol))
      },
      "the whole plane carries the total mass" -> forAll(gm) { mu =>
        Prop(close(mu.massIn(Region.everything[U]), mu.total, tol))
      },
      "nothing carries no mass" -> forAll(gm) { mu =>
        Prop(close(mu.massIn(Region.empty[U]), 0.0, tol))
      },
      "a region and its complement partition the total" -> forAll(gm, gr) { (mu, r) =>
        Prop(close(mu.massIn(r) + mu.massIn(!r), mu.total, tol))
      },
      "normalising a measure gives unit total" -> forAll(gm) { mu =>
        mu.normalised match
          case Left(_)  => Prop(mu.total <= 0.0) :| "only a massless measure may fail"
          case Right(n) => Prop(close(n.total, 1.0, tol))
      },
      "binning conserves the mass that lies inside the frame" -> forAll(gm) { mu =>
        val inside = mu.withinFrame.total
        mu.binned(g) match
          case Left(e)     => Prop(false) :| e.message
          case Right(bins) => Prop(close(bins.sum, inside, tol))
      }
    )

end SurfaceLaws

object SurfaceLaws extends SurfaceLaws
