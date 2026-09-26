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

package eyes4s.compare

import eyes4s.core.*
import eyes4s.kernel.*

enum FixationComparisonError derives CanEqual:
  case Parameter(name: String, value: Double)
  case Frames(error: GeometryError)
  case Clock(error: TimeError)
  case EmptyQueries
  case MissingSupport(requested: Int, contributing: Int)
  case MatrixSize(left: Int, right: Int, limit: Int)
  case Numerical(left: Int, right: Int, value: Double)
  case NonConvergence(
      left: Int,
      right: Int,
      iterations: Int,
      residual: Double,
      tolerance: Double
  )
  case SolverNumerical(left: Int, right: Int, cost: Double, residual: Double)
  case Score(error: ComparisonValueError)
  def message: String = this match
    case Parameter(n, v)      => s"Fixation comparison parameter $n=$v is invalid."
    case Frames(e)            => e.message
    case Clock(e)             => e.message
    case EmptyQueries         => "Fixation overlap requires at least one requested query."
    case MissingSupport(n, k) => s"Fixation overlap has support for $k of $n requested queries."
    case MatrixSize(a, b, n)  =>
      s"Fixation transport requests $a by $b costs, exceeding limit=$n."
    case Numerical(a, b, v) =>
      s"Fixation transport left[$a], right[$b] produced nonfinite value=$v."
    case NonConvergence(a, b, n, r, t) =>
      s"Fixation transport leftCount=$a, rightCount=$b: residual=$r exceeds tolerance=$t after $n iterations."
    case SolverNumerical(a, b, c, r) =>
      s"Fixation transport leftCount=$a, rightCount=$b produced cost=$c, residual=$r."
    case Score(e) => e.message

final class OverlapThreshold[U <: Unit2D] private (val value: Double)
object OverlapThreshold:
  def of[U <: Unit2D](value: Double): Either[FixationComparisonError, OverlapThreshold[U]] =
    Either.cond(
      value.isFinite && value >= 0,
      new OverlapThreshold(value),
      FixationComparisonError.Parameter("threshold", value)
    )

enum FixationGroundDistance derives CanEqual:
  case Euclidean, Manhattan

enum MissingOverlapPolicy derives CanEqual:
  /** Pinned reference denominator: every requested query, with missing support a non-hit. */
  case CountAsNonOverlap
  case RequireComplete

enum OverlapFailure derives CanEqual:
  case Missing(left: Option[TrajectoryMissing], right: Option[TrajectoryMissing])
  case NonfiniteDistance(leftX: Double, leftY: Double, rightX: Double, rightY: Double)
  def message: String = this match
    case Missing(l, r) => s"Fixation overlap missing support: left=$l, right=$r."
    case NonfiniteDistance(x, y, u, v) =>
      s"Fixation overlap distance overflow between ($x,$y) and ($u,$v)."
final case class OverlapQuery(
    time: Instant,
    distance: Either[OverlapFailure, Double],
    hit: Boolean
)
final class FixationOverlapResult private[compare] (
    val rows: Vector[OverlapQuery],
    val policy: MissingOverlapPolicy
):
  def requested: Int                                          = rows.size
  def contributing: Int                                       = rows.count(_.distance.isRight)
  def overlaps: Int                                           = rows.count(_.hit)
  def similarity: Either[FixationComparisonError, Similarity] =
    if policy == MissingOverlapPolicy.RequireComplete && contributing != requested then
      Left(FixationComparisonError.MissingSupport(requested, contributing))
    else
      Similarity.of(overlaps.toDouble / requested).left.map(FixationComparisonError.Score.apply)

object FixationOverlap:
  /** Explicit query grid and endpoint convention; no left-dependent implicit time range. */
  def compare[U <: Unit2D](
      left: FixationTrajectory[U],
      right: FixationTrajectory[U],
      clock: ClockId,
      queries: Vector[Instant],
      threshold: OverlapThreshold[U],
      distance: FixationGroundDistance,
      endpoint: TrajectoryEndpoint,
      missing: MissingOverlapPolicy
  ): Either[FixationComparisonError, FixationOverlapResult] =
    for
      _ <- Either.cond(queries.nonEmpty, (), FixationComparisonError.EmptyQueries)
      _ <- Agreement
        .frames(left.frame, right.frame)
        .left
        .map(FixationComparisonError.Frames.apply)
      a <- left.sample(clock, queries, endpoint).left.map(FixationComparisonError.Clock.apply)
      b <- right.sample(clock, queries, endpoint).left.map(FixationComparisonError.Clock.apply)
    yield new FixationOverlapResult(
      a.rows.zip(b.rows).map { (x, y) =>
        val d = (x.location, y.location) match
          case (Right(p), Right(q)) =>
            val dx = math.abs(p.point.x - q.point.x); val dy = math.abs(p.point.y - q.point.y)
            val value = distance match
              case FixationGroundDistance.Euclidean => math.hypot(dx, dy)
              case FixationGroundDistance.Manhattan => dx + dy
            Either.cond(
              value.isFinite,
              value,
              OverlapFailure.NonfiniteDistance(p.point.x, p.point.y, q.point.x, q.point.y)
            )
          case _ =>
            Left(OverlapFailure.Missing(x.location.swap.toOption, y.location.swap.toOption))
        OverlapQuery(x.time, d, d.exists(_ < threshold.value))
      },
      missing
    )

/** Dimensionless squared ground cost after explicit spatial and exact-time scaling.
  * Lambda is the entropy regularisation, not its inverse.
  */
final class FixationTransportConfig[U <: Unit2D] private (
    val xScale: Double,
    val yScale: Double,
    val timeScale: Span,
    val timeWeight: Double,
    val lambda: Double,
    val maximumIterations: Int,
    val marginalTolerance: Double,
    val maximumCosts: Int
)
object FixationTransportConfig:
  def of[U <: Unit2D](
      xScale: Double,
      yScale: Double,
      timeScale: Span,
      timeWeight: Double,
      lambda: Double,
      maximumIterations: Int = 10000,
      marginalTolerance: Double = 1e-10,
      maximumCosts: Int = 1000000
  ): Either[FixationComparisonError, FixationTransportConfig[U]] =
    val positive = Vector(
      "xScale"            -> xScale,
      "yScale"            -> yScale,
      "timeScaleMicros"   -> timeScale.toMicros.toDouble,
      "lambda"            -> lambda,
      "maximumIterations" -> maximumIterations.toDouble,
      "marginalTolerance" -> marginalTolerance,
      "maximumCosts"      -> maximumCosts.toDouble
    )
    positive.find((_, v) => !v.isFinite || v <= 0) match
      case Some((n, v)) => Left(FixationComparisonError.Parameter(n, v))
      case None if !timeWeight.isFinite || timeWeight < 0 =>
        Left(FixationComparisonError.Parameter("timeWeight", timeWeight))
      case None =>
        Right(
          new FixationTransportConfig(
            xScale,
            yScale,
            timeScale,
            timeWeight,
            lambda,
            maximumIterations,
            marginalTolerance,
            maximumCosts
          )
        )

final class FixationTransportResult private[compare] (
    val squaredCost: Double,
    val iterations: Int,
    val marginalResidual: Double,
    val leftCount: Int,
    val rightCount: Int
):
  /** T4transport p=2 convention; not a debiased Sinkhorn divergence. */
  def rootCost: Double                                        = math.sqrt(squaredCost)
  def similarity: Either[FixationComparisonError, Similarity] =
    Similarity.of(1 / (1 + rootCost)).left.map(FixationComparisonError.Score.apply)

object FixationTransport:
  /** Log-domain alternating scaling with an explicit convergence admission gate.
    * Finite iteration is not promoted to symmetry or a Metric contract.
    */
  def compare[U <: Unit2D](
      left: Scanpath[U],
      right: Scanpath[U],
      config: FixationTransportConfig[U]
  ): Either[FixationComparisonError, FixationTransportResult] =
    for
      _ <- Agreement
        .frames(left.frame, right.frame)
        .left
        .map(FixationComparisonError.Frames.apply)
      _ <- Agreement
        .clocks(left.clock, right.clock)
        .left
        .map(FixationComparisonError.Clock.apply)
      _ <- Either.cond(
        left.n.toLong * right.n <= config.maximumCosts,
        (),
        FixationComparisonError.MatrixSize(left.n, right.n, config.maximumCosts)
      )
      result <- solve(left, right, config)
    yield result

  private def solve[U <: Unit2D](
      a: Scanpath[U],
      b: Scanpath[U],
      c: FixationTransportConfig[U]
  ): Either[FixationComparisonError, FixationTransportResult] =
    val n                                    = a.n; val m = b.n
    val cost                                 = Array.ofDim[Double](n, m)
    var bad: Option[FixationComparisonError] = None
    var i                                    = 0
    while i < n do
      var j = 0
      while j < m do
        val x  = a.fixations(i); val y = b.fixations(j)
        val dx = (x.centre.x - y.centre.x) / c.xScale;
        val dy = (x.centre.y - y.centre.y) / c.yScale
        val dt = (BigInt(x.span.onset.toMicros) - BigInt(
          y.span.onset.toMicros
        )).toDouble / c.timeScale.toMicros * c.timeWeight
        val d = dx * dx + dy * dy + dt * dt
        cost(i)(j) = d
        if !d.isFinite || !(d / c.lambda).isFinite then
          bad = Some(FixationComparisonError.Numerical(i, j, d))
        j += 1
      i += 1
    bad match
      case Some(error) => Left(error)
      case None        =>
        // Fixations have strictly positive duration by construction. Scale before summing.
        def weights(path: Scanpath[U]): Vector[Double] =
          val durations = path.fixations.toVector.map(f =>
            (BigInt(f.span.offset.toMicros) - BigInt(f.span.onset.toMicros)).toDouble
          )
          val maximum = durations.max; val scaled = durations.map(_ / maximum);
          scaled.map(_ / scaled.sum)
        val wa = weights(a); val wb        = weights(b)
        val u  = Array.fill(n)(0.0); val v = Array.fill(m)(0.0)
        def logSum(count: Int, f: Int => Double): Double =
          var k = 0; var maximum = Double.NegativeInfinity
          while k < count do
            maximum = math.max(maximum, f(k)); k += 1
          k = 0; var sum = 0.0
          while k < count do
            sum += math.exp(f(k) - maximum); k += 1
          maximum + math.log(sum)
        var iteration = 0; var residual = Double.PositiveInfinity; var total = 0.0
        while iteration < c.maximumIterations && residual > c.marginalTolerance do
          i = 0
          while i < n do
            val row = i
            u(i) = math.log(wa(i)) - logSum(m, j => v(j) - cost(row)(j) / c.lambda); i += 1
          var j = 0
          while j < m do
            val col = j
            v(j) = math.log(wb(j)) - logSum(n, k => u(k) - cost(k)(col) / c.lambda); j += 1
          val rows = Array.fill(n)(0.0); val cols = Array.fill(m)(0.0); total = 0.0; i = 0
          while i < n do
            j = 0
            while j < m do
              val p = math.exp(u(i) + v(j) - cost(i)(j) / c.lambda)
              rows(i) += p; cols(j) += p; total += p * cost(i)(j); j += 1
            i += 1
          residual = math.max(
            rows.indices.map(k => math.abs(rows(k) - wa(k))).max,
            cols.indices.map(k => math.abs(cols(k) - wb(k))).max
          )
          iteration += 1
        if !total.isFinite || !residual.isFinite then
          Left(FixationComparisonError.SolverNumerical(n, m, total, residual))
        else if residual > c.marginalTolerance then
          Left(
            FixationComparisonError
              .NonConvergence(n, m, iteration, residual, c.marginalTolerance)
          )
        else Right(new FixationTransportResult(total, iteration, residual, n, m))
