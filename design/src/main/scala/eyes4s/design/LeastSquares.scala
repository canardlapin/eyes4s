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

/** Relative pivot threshold after every input column has unit Euclidean norm. */
final class RelativeRankTolerance private (val value: Double)
object RelativeRankTolerance:
  val default: RelativeRankTolerance = new RelativeRankTolerance(1e-12)
  def of(value: Double): Either[LeastSquaresError, RelativeRankTolerance] =
    if value.isFinite && value > 0.0 && value < 1.0 then Right(new RelativeRankTolerance(value))
    else Left(LeastSquaresError.RankTolerance(value))

enum LeastSquaresError derives CanEqual:
  case Shape(rows: Int, columns: Int, response: Int, rowWidths: Vector[Int])
  case NonFinite(row: Int, column: Option[Int], value: Double)
  case RankTolerance(value: Double)
  case RankDeficient(column: Int, pivot: Double, threshold: Double)
  case Arithmetic(operation: String, row: Int, column: Int)
  def message: String = this match
    case Shape(n, p, y, widths) =>
      s"Least squares rows=$n columns=$p responseLength=$y rowWidths=$widths require a nonempty rectangular matrix with rows >= columns."
    case NonFinite(r, c, v) => s"Least squares row=$r column=$c contains nonfinite value=$v."
    case RankTolerance(v)   =>
      s"Relative QR rank tolerance=$v must be finite and strictly between zero and one."
    case RankDeficient(c, p, t) =>
      s"Scaled QR column=$c has pivot=$p at or below relative threshold=$t."
    case Arithmetic(op, r, c) => s"Least squares $op is nonfinite at row=$r column=$c."

final class LeastSquaresFit private[design] (
    val coefficients: Vector[Double],
    val fitted: Vector[Double],
    val residuals: Vector[Double],
    val rank: Int,
    /** Ratio of largest to smallest scaled R diagonal; not a condition number. */
    val scaledDiagonalRatio: Double
)

/** Native full-column-rank OLS. Column scaling and Householder reflections avoid
  * forming normal equations. A deficient pivot is an error, never a dropped column.
  */
object LeastSquares:
  def fit(
      rows: Vector[Vector[Double]],
      response: Vector[Double],
      tolerance: RelativeRankTolerance = RelativeRankTolerance.default
  ): Either[LeastSquaresError, LeastSquaresFit] =
    val n = rows.size
    val p = rows.headOption.fold(0)(_.size)
    if n == 0 || p == 0 || n < p || response.size != n || rows.exists(_.size != p) then
      Left(LeastSquaresError.Shape(n, p, response.size, rows.map(_.size)))
    else
      val invalidX = rows.indices.iterator
        .flatMap(i =>
          rows(i).indices.iterator
            .filter(j => !rows(i)(j).isFinite)
            .map(j => LeastSquaresError.NonFinite(i, Some(j), rows(i)(j)))
        )
        .nextOption()
      val invalidY = response.indices
        .find(i => !response(i).isFinite)
        .map(i => LeastSquaresError.NonFinite(i, None, response(i)))
      invalidX.orElse(invalidY) match
        case Some(error) => Left(error)
        case None        => solve(rows, response, tolerance)

  private def solve(
      rows: Vector[Vector[Double]],
      response: Vector[Double],
      tolerance: RelativeRankTolerance
  ): Either[LeastSquaresError, LeastSquaresFit] =
    val n                                = rows.size
    val p                                = rows.head.size
    val a                                = rows.map(_.toArray).toArray
    val y                                = response.toArray
    val scales                           = Array.fill(p)(0.0)
    var error: Option[LeastSquaresError] = None
    var j                                = 0
    while j < p && error.isEmpty do
      var i = 0
      while i < n do
        scales(j) = math.hypot(scales(j), a(i)(j))
        i += 1
      if !scales(j).isFinite then
        error = Some(LeastSquaresError.Arithmetic("column norm", 0, j))
      else if scales(j) == 0.0 then
        error = Some(LeastSquaresError.RankDeficient(j, 0.0, tolerance.value))
      else
        i = 0
        while i < n do
          a(i)(j) /= scales(j)
          i += 1
      j += 1
    var k = 0
    while k < p && error.isEmpty do
      var norm = 0.0
      var i    = k
      while i < n do
        norm = math.hypot(norm, a(i)(k))
        i += 1
      if !norm.isFinite then error = Some(LeastSquaresError.Arithmetic("reflector norm", k, k))
      else if norm <= tolerance.value then
        error = Some(LeastSquaresError.RankDeficient(k, norm, tolerance.value))
      else
        val alpha = if a(k)(k) >= 0.0 then -norm else norm
        val first = a(k)(k) - alpha
        val tau   = (alpha - a(k)(k)) / alpha
        i = k + 1
        while i < n do
          a(i)(k) /= first
          i += 1
        a(k)(k) = alpha
        j = k + 1
        while j < p do
          var dot = a(k)(j)
          i = k + 1
          while i < n do
            dot += a(i)(k) * a(i)(j)
            i += 1
          dot *= tau
          a(k)(j) -= dot
          i = k + 1
          while i < n do
            a(i)(j) -= a(i)(k) * dot
            i += 1
          j += 1
        var dot = y(k)
        i = k + 1
        while i < n do
          dot += a(i)(k) * y(i)
          i += 1
        dot *= tau
        y(k) -= dot
        i = k + 1
        while i < n do
          y(i) -= a(i)(k) * dot
          i += 1
      k += 1
    error match
      case Some(e) => Left(e)
      case None    =>
        val beta = Array.fill(p)(0.0)
        k = p - 1
        while k >= 0 do
          var rhs = y(k)
          j = k + 1
          while j < p do
            rhs -= a(k)(j) * beta(j)
            j += 1
          beta(k) = rhs / a(k)(k)
          k -= 1
        val coefficients = beta.indices.map(j => beta(j) / scales(j)).toVector
        val fitted       =
          rows.map(row => row.indices.foldLeft(0.0)((s, j) => s + row(j) * coefficients(j)))
        val residuals = response.zip(fitted).map(_ - _)
        val bad       = coefficients.indexWhere(v => !v.isFinite)
        val badRow    = fitted.indices.find(i => !fitted(i).isFinite || !residuals(i).isFinite)
        if bad >= 0 then Left(LeastSquaresError.Arithmetic("coefficient", 0, bad))
        else
          badRow match
            case Some(i) => Left(LeastSquaresError.Arithmetic("prediction/residual", i, 0))
            case None    =>
              val diagonal = (0 until p).map(j => math.abs(a(j)(j)))
              Right(
                new LeastSquaresFit(
                  coefficients,
                  fitted,
                  residuals,
                  p,
                  diagonal.max / diagonal.min
                )
              )
