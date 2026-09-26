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

package eyes4s.surface

import eyes4s.core.{TrajectoryMissing, TrajectorySamples}
import eyes4s.kernel.*

/** Normalization is performed on all cells before any point is looked up. */
enum DensityNormalization derives CanEqual:
  case None, Maximum, Sum, SampleZScore

enum DensityLookupPolicy derives CanEqual:
  /** Existing Surface.sampleAt semantics: half-open containing cell, missing outside. */
  case ContainingCell

  /** Nearest cell centre, clamped outside; half ties round the one-based index to even. */
  case NearestClampedRIndex

enum DensityLookupError derives CanEqual:
  case Arithmetic(normalization: DensityNormalization, operand: String, value: Double)
  case Surface(underlying: SurfaceError)
  def message: String = this match
    case Arithmetic(n, o, v) =>
      s"Density normalization=$n operand=$o produced nonfinite value=$v."
    case Surface(e) => e.message

enum DensityPointFailure derives CanEqual:
  case NonFinitePoint(x: Double, y: Double)
  case OutsideGrid(grid: GridId, x: Double, y: Double)
  case Trajectory(reason: TrajectoryMissing)
  def message: String = this match
    case NonFinitePoint(x, y) => s"Density query ($x,$y) must be finite."
    case OutsideGrid(g, x, y) => s"Density query ($x,$y) is outside grid=$g."
    case Trajectory(r)        => s"Density query has no trajectory position: $r."

final case class DensityPointSample[U <: Unit2D](
    point: Pt[U],
    value: Either[DensityPointFailure, Double]
)
final case class DensityTimeSample[U <: Unit2D](
    time: Instant,
    point: Option[Pt[U]],
    value: Either[DensityPointFailure, Double]
)

/** A normalized signed field, never mislabeled as probability mass after z-scoring. */
final class PreparedDensityLookup[U <: Unit2D] private[surface] (
    val field: Signed[U],
    val normalization: DensityNormalization
):
  def sample(
      frame: Frame[U],
      points: Vector[Pt[U]],
      policy: DensityLookupPolicy
  ): Either[GeometryError, Vector[DensityPointSample[U]]] =
    Agreement
      .frames(field.grid.frame, frame)
      .map(_ => points.map(p => DensityPointSample(p, at(p, policy))))

  /** Consume the shared trajectory verbatim; missing positions keep their query times. */
  def along(
      trajectory: TrajectorySamples[U],
      policy: DensityLookupPolicy
  ): Either[GeometryError, Vector[DensityTimeSample[U]]] =
    Agreement.frames(field.grid.frame, trajectory.frame).map { _ =>
      trajectory.rows.map { row =>
        row.location match
          case Left(reason) =>
            DensityTimeSample(row.time, None, Left(DensityPointFailure.Trajectory(reason)))
          case Right(location) =>
            DensityTimeSample(row.time, Some(location.point), at(location.point, policy))
      }
    }

  private def at(
      point: Pt[U],
      policy: DensityLookupPolicy
  ): Either[DensityPointFailure, Double] =
    if !point.x.isFinite || !point.y.isFinite then
      Left(DensityPointFailure.NonFinitePoint(point.x, point.y))
    else
      val grid = field.grid
      policy match
        case DensityLookupPolicy.ContainingCell =>
          field
            .sampleAt(point)
            .toRight(DensityPointFailure.OutsideGrid(grid.id, point.x, point.y))
        case DensityLookupPolicy.NearestClampedRIndex =>
          def axis(value: Double, minimum: Double, step: Double, size: Int): Int =
            val first = minimum + step / 2
            val last  = minimum + step * (size - 0.5)
            if value <= first then 0
            else if value >= last then size - 1
            else
              math.max(0, math.min(size - 1, math.rint((value - first) / step + 1.0).toInt - 1))
          val x = axis(point.x, grid.frame.bounds.xMin, grid.cellWidth, grid.nx)
          val y = axis(point.y, grid.frame.bounds.yMin, grid.cellHeight, grid.ny)
          Right(field.values(y * grid.nx + x))

object DensityLookup:
  def prepare[U <: Unit2D](
      surface: Surface[U],
      normalization: DensityNormalization
  ): Either[DensityLookupError, PreparedDensityLookup[U]] =
    val values                                                  = surface.values.toVector
    val transformed: Either[DensityLookupError, Vector[Double]] = normalization match
      case DensityNormalization.None    => Right(values)
      case DensityNormalization.Maximum =>
        val maximum = values.max
        Right(if maximum > 0 then values.map(_ / maximum) else values)
      case DensityNormalization.Sum =>
        val sum = values.sum
        if !sum.isFinite then Left(DensityLookupError.Arithmetic(normalization, "sum", sum))
        else Right(if sum > 0 then values.map(_ / sum) else values)
      case DensityNormalization.SampleZScore =>
        if values.forall(_ == values.head) then Right(values)
        else
          val mean     = values.map(_ / values.size).sum
          val variance = if values.size == 1 then 0.0
          else values.map(v => (v - mean) * (v - mean) / (values.size - 1)).sum
          if !mean.isFinite then
            Left(DensityLookupError.Arithmetic(normalization, "mean", mean))
          else if !variance.isFinite then
            Left(DensityLookupError.Arithmetic(normalization, "sample variance", variance))
          else
            Right(
              if variance > 0 then values.map(v => (v - mean) / math.sqrt(variance)) else values
            )
    transformed.flatMap { v =>
      val provenance = Provenance(
        surface.provenance.digest,
        Vector(
          Provenance.Step(
            "density-lookup-normalization",
            Vector("mode" -> Provenance.Param.Text(normalization.toString))
          )
        )
      )
      Surface
        .signed(surface.grid, IArray.from(v), provenance)
        .left
        .map(DensityLookupError.Surface.apply)
        .map(field => new PreparedDensityLookup(field, normalization))
    }
