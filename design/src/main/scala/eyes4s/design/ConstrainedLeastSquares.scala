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

/** Relative Karush-Kuhn-Tucker threshold for the active-set solvers. A column
  * outside the active set enters only while its gradient exceeds this fraction
  * of the product of its own Euclidean norm and the response's.
  */
final class RelativeDualTolerance private (val value: Double)
object RelativeDualTolerance:
  val default: RelativeDualTolerance = new RelativeDualTolerance(1e-10)
  def of(value: Double): Either[LeastSquaresError, RelativeDualTolerance] =
    if value.isFinite && value > 0.0 && value < 1.0 then Right(new RelativeDualTolerance(value))
    else Left(LeastSquaresError.DualTolerance(value))

final class ConstrainedFit private[design] (
    /** Non-negative; exactly zero outside the active set. */
    val coefficients: Vector[Double],
    val fitted: Vector[Double],
    val residuals: Vector[Double],
    /** Full column rank of the whole design, checked before the active set runs. */
    val rank: Int,
    /** Columns with a strictly positive coefficient. */
    val active: Int,
    /** Columns entered plus interpolation steps taken. */
    val iterations: Int,
    /** Largest relative KKT violation left outside the active set; zero when none. */
    val dualViolation: Double,
    /** The whole design's scaled R diagonal ratio; not a condition number. */
    val scaledDiagonalRatio: Double
)

/** Active-set least squares under sign constraints, after Lawson and Hanson
  * (1974, ch. 23). Every subproblem is the native scaled Householder
  * [[LeastSquares.fit]] on the active columns, so no normal equations are
  * formed. The whole design must have full column rank under the given
  * [[RelativeRankTolerance]]; the solution is then unique, and each active
  * subproblem, whose columns keep their order, has pivots no smaller than the
  * whole design's.
  *
  * A column whose own least-squares coefficient is not positive on the step
  * that entered it is set aside until the active set next changes: its
  * gradient was rounding, and admitting it again would cycle. Termination is
  * reported in [[ConstrainedFit.dualViolation]], never assumed.
  */
object ConstrainedLeastSquares:

  /** A safety net, not an expected bound: Lawson-Hanson terminates in finitely
    * many steps, usually fewer than twice the column count.
    */
  def iterationLimit(columns: Int): Int = 30 * columns

  /** Minimise the residual norm subject to every coefficient being non-negative. */
  def nonNegative(
      rows: Vector[Vector[Double]],
      response: Vector[Double],
      rankTolerance: RelativeRankTolerance = RelativeRankTolerance.default,
      dualTolerance: RelativeDualTolerance = RelativeDualTolerance.default
  ): Either[LeastSquaresError, ConstrainedFit] =
    run(rows, response, rankTolerance, dualTolerance, sumToOne = false)

  /** Minimise the residual norm over the probability simplex: non-negative
    * coefficients summing to one. The sum is eliminated exactly through a
    * reference active column, not imposed by a weighted penalty row, so it
    * holds to rounding whatever the scale of the design.
    */
  def simplex(
      rows: Vector[Vector[Double]],
      response: Vector[Double],
      rankTolerance: RelativeRankTolerance = RelativeRankTolerance.default,
      dualTolerance: RelativeDualTolerance = RelativeDualTolerance.default
  ): Either[LeastSquaresError, ConstrainedFit] =
    run(rows, response, rankTolerance, dualTolerance, sumToOne = true)

  private def run(
      rows: Vector[Vector[Double]],
      response: Vector[Double],
      rankTolerance: RelativeRankTolerance,
      dualTolerance: RelativeDualTolerance,
      sumToOne: Boolean
  ): Either[LeastSquaresError, ConstrainedFit] =
    LeastSquares.fit(rows, response, rankTolerance).flatMap { full =>
      val n     = rows.size
      val p     = rows.head.size
      val limit = iterationLimit(p)
      val y     = response.toArray
      val cols  = Array.tabulate(p, n)((j, i) => rows(i)(j))
      val norms = cols.map(c => c.foldLeft(0.0)(math.hypot))
      val yNorm = y.foldLeft(0.0)(math.hypot)

      def solve(passive: Vector[Int]): Either[LeastSquaresError, Map[Int, Double]] =
        if !sumToOne then
          LeastSquares
            .fit(rows.map(r => passive.map(r)), response, rankTolerance)
            .map(f => passive.zip(f.coefficients).toMap)
        else if passive.size == 1 then Right(Map(passive.head -> 1.0))
        else
          // Substitute w(r) = 1 - sum(others): y - x(r) = sum_j w(j) (x(j) - x(r)).
          val r = passive.head
          LeastSquares
            .fit(
              rows.map(row => passive.tail.map(j => row(j) - row(r))),
              response.indices.toVector.map(i => response(i) - rows(i)(r)),
              rankTolerance
            )
            .map { f =>
              val others = passive.tail.zip(f.coefficients)
              others.toMap.updated(r, 1.0 - others.foldLeft(0.0)(_ + _._2))
            }

      def violations(x: Array[Double], passive: Vector[Int]): Array[Double] =
        val residual = Array.tabulate(n) { i =>
          var s = y(i)
          var j = 0
          while j < p do
            s -= cols(j)(i) * x(j)
            j += 1
          s
        }
        val dual = cols.map(c => c.indices.foldLeft(0.0)((s, i) => s + c(i) * residual(i)))
        val nu   =
          if sumToOne && passive.nonEmpty then passive.map(dual).sum / passive.size else 0.0
        Array.tabulate(p) { j =>
          val scale = norms(j) * yNorm
          if scale == 0.0 then 0.0 else (dual(j) - nu) / scale
        }

      val x                    = Array.fill(p)(0.0)
      var passive: Vector[Int] =
        if !sumToOne then Vector.empty
        else
          // Start at the best vertex: all weight on the closest single column.
          val distance = (0 until p).map(j =>
            (0 until n).foldLeft(0.0)((s, i) => s + (y(i) - cols(j)(i)) * (y(i) - cols(j)(i)))
          )
          Vector(distance.indexOf(distance.min))
      passive.foreach(j => x(j) = 1.0)
      var blocked                                          = Set.empty[Int]
      var iterations                                       = 0
      var outcome: Option[Either[LeastSquaresError, Unit]] = None

      def step(): Unit =
        iterations += 1
        if iterations > limit then
          val v = violations(x, passive)
          outcome = Some(
            Left(
              LeastSquaresError.NotConverged(
                iterations - 1,
                limit,
                (0 until p).filterNot(passive.contains).map(v).maxOption.getOrElse(0.0)
              )
            )
          )

      while outcome.isEmpty do
        val v         = violations(x, passive)
        val candidate = (0 until p)
          .filter(j => !passive.contains(j) && !blocked(j) && v(j) > dualTolerance.value)
          .maxByOption(v)
        candidate match
          case None        => outcome = Some(Right(()))
          case Some(enter) =>
            step()
            passive = (passive :+ enter).sorted
            var inner = true
            var first = true
            while inner && outcome.isEmpty do
              if passive.isEmpty then inner = false
              else
                solve(passive) match
                  case Left(e)  => outcome = Some(Left(e))
                  case Right(z) =>
                    if passive.forall(j => z(j) > 0.0) then
                      passive.foreach(j => x(j) = z(j))
                      blocked = Set.empty
                      inner = false
                    else if first && z(enter) <= 0.0 then
                      passive = passive.filterNot(_ == enter)
                      blocked += enter
                      inner = false
                    else
                      // Move toward z until the first coefficient reaches zero.
                      val leaving = passive.filter(j => z(j) <= 0.0)
                      val alpha   = leaving.map(j => x(j) / (x(j) - z(j))).min
                      val stopper = leaving.minBy(j => x(j) / (x(j) - z(j)))
                      passive.foreach(j => x(j) += alpha * (z(j) - x(j)))
                      x(stopper) = 0.0
                      passive.filter(j => x(j) <= 0.0).foreach(j => x(j) = 0.0)
                      passive = passive.filter(j => x(j) > 0.0)
                      first = false
                      step()

      outcome.get.flatMap { _ =>
        val coefficients = x.toVector
        val fitted       =
          rows.map(row => row.indices.foldLeft(0.0)((s, j) => s + row(j) * coefficients(j)))
        val residuals = response.zip(fitted).map(_ - _)
        val v         = violations(x, passive)
        val violation =
          (0 until p).filterNot(passive.contains).map(v).maxOption.fold(0.0)(math.max(0.0, _))
        fitted.indices.find(i => !fitted(i).isFinite || !residuals(i).isFinite) match
          case Some(i) => Left(LeastSquaresError.RowArithmetic("prediction/residual", i))
          case None    =>
            Right(
              new ConstrainedFit(
                coefficients,
                fitted,
                residuals,
                full.rank,
                passive.size,
                iterations,
                violation,
                full.scaledDiagonalRatio
              )
            )
      }
    }
