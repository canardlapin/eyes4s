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

import eyes4s.kernel.*

/** Closed supported map-similarity vocabulary; exact EMD has no native alias. */
enum MapSimilarityMethod derives CanEqual:
  case Pearson, Spearman, FisherZMachineEpsilon, FisherZLegacy, Cosine, L1Similarity,
    ExtendedJaccard, DistanceCorrelation
  def instance[U <: Unit2D]: SymmetricCompare[Mass[U], Similarity] = this match
    case Pearson               => Distribution.pearson
    case Spearman              => Distribution.spearman
    case FisherZMachineEpsilon => Distribution.fisherZMachineEpsilon
    case FisherZLegacy         => Distribution.fisherZ
    case Cosine                => Distribution.cosine
    case L1Similarity          => Distribution.l1Similarity
    case ExtendedJaccard       => Distribution.extendedJaccard
    case DistanceCorrelation   => Distribution.distanceCorrelation

object MapSimilarityMethod:
  def fromReference(value: String): Either[MapComparisonError, MapSimilarityMethod] =
    value match
      case "pearson"  => Right(Pearson)
      case "spearman" => Right(Spearman)
      case "fisherz"  => Right(FisherZMachineEpsilon)
      case "cosine"   => Right(Cosine)
      case "l1"       => Right(L1Similarity)
      case "jaccard"  => Right(ExtendedJaccard)
      case "dcov"     => Right(DistanceCorrelation)
      case other      => Left(MapComparisonError.UnsupportedMethod(other))

enum MapScaleFailure derives CanEqual:
  case MissingLeft, MissingRight
  case Comparison(underlying: CompareError)
  def message: String = this match
    case MissingLeft   => "scale missing from left input"
    case MissingRight  => "scale missing from right input"
    case Comparison(e) => e.message

enum MapComparisonError derives CanEqual:
  case UnsupportedMethod(value: String)
  case Scales(values: Vector[Double])
  case Grid(scale: Double, underlying: SurfaceError)
  case Incomplete(requested: Int, failures: Vector[(Double, MapScaleFailure)])
  case Score(underlying: ComparisonValueError)
  def message: String = this match
    case UnsupportedMethod(v) =>
      s"Map similarity method '$v' has no supported native dispatch; exact EMD is not an alias for an approximation."
    case Scales(v)        => s"Map scales $v must be nonempty and unique."
    case Grid(s, e)       => s"Map scale=$s: ${e.message}"
    case Incomplete(n, f) =>
      s"Cannot average $n requested scales; failures=" + f
        .map((s, e) => s"$s: ${e.message}")
        .mkString("; ")
    case Score(e) => e.message

/** Checked supplied maps, with unique scales stored in ascending order. */
final class MapScales[U <: Unit2D] private (val levels: Vector[(Sigma[U], Mass[U])])
object MapScales:
  def of[U <: Unit2D](
      levels: Vector[(Sigma[U], Mass[U])]
  ): Either[MapComparisonError, MapScales[U]] =
    val values = levels.map(_._1.value)
    if values.isEmpty || values.distinct.size != values.size then
      Left(MapComparisonError.Scales(values))
    else
      levels
        .foldLeft[Either[MapComparisonError, Unit]](Right(())) {
          case (checked, (sigma, mass)) =>
            checked.flatMap(_ =>
              Agreement
                .grids(levels.head._2.grid, mass.grid)
                .left
                .map(MapComparisonError.Grid(sigma.value, _))
                .map(_ => ())
            )
        }
        .map(_ => new MapScales(levels.sortBy(_._1.value)))

final class MapScaleComparison[U <: Unit2D] private[compare] (
    val method: MapSimilarityMethod,
    val rows: Vector[(Sigma[U], Either[MapScaleFailure, Similarity])]
):
  def requested: Int    = rows.size
  def contributing: Int = rows.count(_._2.isRight)

  /** Strict mean over every requested scale. Failures never disappear into na.rm. */
  def mean: Either[MapComparisonError, Similarity] =
    val failures = rows.collect { case (sigma, Left(error)) => sigma.value -> error }
    if failures.nonEmpty then Left(MapComparisonError.Incomplete(requested, failures))
    else
      Similarity
        .of(rows.flatMap(_._2.toOption).map(_.value / requested).sum)
        .left
        .map(MapComparisonError.Score.apply)

object MapComparison:
  def scales[U <: Unit2D](
      left: MapScales[U],
      right: MapScales[U],
      method: MapSimilarityMethod
  ): MapScaleComparison[U] =
    val sigmas = (left.levels.map(_._1) ++ right.levels
      .map(_._1)).groupBy(_.value).values.map(_.head).toVector.sortBy(_.value)
    new MapScaleComparison(
      method,
      sigmas.map { sigma =>
        val result = (
          left.levels.find(_._1.value == sigma.value),
          right.levels.find(_._1.value == sigma.value)
        ) match
          case (None, _)                    => Left(MapScaleFailure.MissingLeft)
          case (_, None)                    => Left(MapScaleFailure.MissingRight)
          case (Some((_, a)), Some((_, b))) =>
            method.instance[U].compare(a, b).left.map(MapScaleFailure.Comparison.apply)
        sigma -> result
      }
    )
