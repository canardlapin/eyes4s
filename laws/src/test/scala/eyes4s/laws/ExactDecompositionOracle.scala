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

/** Exact rational arithmetic, independent of every floating-point path. */
final case class Q private (n: BigInt, d: BigInt):
  def +(o: Q): Q         = Q(n * o.d + o.n * d, d * o.d)
  def -(o: Q): Q         = Q(n * o.d - o.n * d, d * o.d)
  def *(o: Q): Q         = Q(n * o.n, d * o.d)
  def /(o: Q): Q         = Q(n * o.d, d * o.n)
  def unary_- : Q        = Q(-n, d)
  def signum: Int        = n.signum
  def compare(o: Q): Int = (this - o).signum
  def toDouble: Double   =
    (BigDecimal(n) / BigDecimal(d)).toDouble
object Q:
  def apply(n: BigInt, d: BigInt): Q =
    require(d != 0)
    val g = n.gcd(d)
    val s = if d < 0 then -1 else 1
    new Q(s * n / g, s * d / g)
  def int(n: Int): Q          = Q(BigInt(n), BigInt(1))
  val zero: Q                 = int(0)
  val one: Q                  = int(1)
  def sum(qs: Iterable[Q]): Q = qs.foldLeft(zero)(_ + _)

  /** The exact value of a finite double. */
  def of(x: Double): Q =
    val b = BigDecimal(new java.math.BigDecimal(x))
    if b.scale >= 0 then Q(BigInt(b.underlying.unscaledValue), BigInt(10).pow(b.scale))
    else Q(BigInt(b.underlying.unscaledValue) * BigInt(10).pow(-b.scale), BigInt(1))

/** Exhaustive exact oracles for the constrained surface fits and partial
  * association. Each enumerates every active set, solves its first-order
  * conditions exactly by Gauss-Jordan elimination, and returns the unique set
  * satisfying every Karush-Kuhn-Tucker condition. Nothing here shares code
  * with the Householder or Lawson-Hanson paths it checks.
  */
object ExactDecompositionOracle:
  type Column = Vector[Q]

  def dot(a: Column, b: Column): Q = Q.sum(a.indices.map(i => a(i) * b(i)))

  /** Solve a square system, or `None` when it is singular. */
  def solve(a: Vector[Vector[Q]], b: Vector[Q]): Option[Vector[Q]] =
    val p  = b.size
    var m  = a.indices.map(i => a(i) :+ b(i)).toVector
    var k  = 0
    var ok = true
    while ok && k < p do
      m.indices.drop(k).find(i => m(i)(k).signum != 0) match
        case None        => ok = false
        case Some(pivot) =>
          m = m.updated(k, m(pivot)).updated(pivot, m(k))
          val row = m(k).map(_ / m(k)(k))
          m = m.updated(k, row)
          m = m.indices.toVector.map(i =>
            if i == k then m(i) else m(i).zip(row).map((v, w) => v - m(i)(k) * w)
          )
          k += 1
    Option.when(ok)(m.map(_.last))

  private def subsets(p: Int): Vector[Vector[Int]] =
    (0 until (1 << p)).toVector.map(mask =>
      (0 until p).toVector.filter(j => (mask >> j & 1) == 1)
    )

  private def residual(columns: Vector[Column], y: Column, w: Vector[Q]): Column =
    y.indices.toVector.map(i => y(i) - Q.sum(columns.indices.map(j => columns(j)(i) * w(j))))

  /** argmin |y - X w| subject to w >= 0. */
  def nonNegative(columns: Vector[Column], y: Column): Vector[Q] =
    val p         = columns.size
    val solutions = subsets(p).flatMap { active =>
      val gram = active.map(i => active.map(j => dot(columns(i), columns(j))))
      solve(gram, active.map(i => dot(columns(i), y))).flatMap { z =>
        val w = Vector.tabulate(p)(j =>
          active.indexOf(j) match
            case -1 => Q.zero
            case k  => z(k)
        )
        val r = residual(columns, y, w)
        Option.when(
          z.forall(_.signum > 0) &&
            (0 until p).filterNot(active.contains).forall(j => dot(columns(j), r).signum <= 0)
        )(w)
      }
    }
    assert(solutions.size == 1, s"exact NNLS KKT solutions=${solutions.size}")
    solutions.head

  /** argmin |y - X w| subject to w >= 0 and sum(w) = 1. */
  def simplex(columns: Vector[Column], y: Column): Vector[Q] =
    val p         = columns.size
    val solutions = subsets(p).filter(_.nonEmpty).flatMap { active =>
      val s    = active.size
      val gram = active.map(i => active.map(j => dot(columns(i), columns(j))) :+ Q.one) :+
        (Vector.fill(s)(Q.one) :+ Q.zero)
      solve(gram, active.map(i => dot(columns(i), y)) :+ Q.one).flatMap { z =>
        val w = Vector.tabulate(p)(j =>
          active.indexOf(j) match
            case -1 => Q.zero
            case k  => z(k)
        )
        val r  = residual(columns, y, w)
        val nu = dot(columns(active.head), r)
        Option.when(
          z.take(s).forall(_.signum > 0) &&
            (0 until p)
              .filterNot(active.contains)
              .forall(j => dot(columns(j), r).compare(nu) <= 0)
        )(w)
      }
    }
    assert(solutions.size == 1, s"exact simplex KKT solutions=${solutions.size}")
    solutions.head

  /** Exact average ranks, one-based. */
  def ranks(values: Column): Column =
    values.map { v =>
      val below = values.count(_.compare(v) < 0)
      val tied  = values.count(_.compare(v) == 0)
      Q.int(2 * below + tied + 1) / Q.int(2)
    }

  /** Partial correlation of columns 0 and 1 given the rest, through the
    * inverse covariance matrix (the route ppcor takes), not through residuals.
    * Returns the exact sign and squared value, or `None` when the covariance is
    * singular.
    */
  def partial(columns: Vector[Column]): Option[(Int, Q)] =
    val n        = columns.head.size
    val centered = columns.map { c =>
      val mean = Q.sum(c) / Q.int(n)
      c.map(_ - mean)
    }
    val k   = columns.size
    val cov = centered.map(a => centered.map(b => dot(a, b)))
    // Columns 0 and 1 of the inverse: solve cov * e = unit vector.
    for
      e0 <- solve(cov, Vector.tabulate(k)(i => if i == 0 then Q.one else Q.zero))
      e1 <- solve(cov, Vector.tabulate(k)(i => if i == 1 then Q.one else Q.zero))
    yield (-e0(1).signum, e0(1) * e0(1) / (e0(0) * e1(1)))
