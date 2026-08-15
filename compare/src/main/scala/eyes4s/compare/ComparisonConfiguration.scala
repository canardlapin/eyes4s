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

/** Invalid construction of a comparison algorithm.
  *
  * These failures occur before a comparison exists. Keeping them separate from
  * [[CompareError]] means a successfully constructed value satisfies its local
  * numerical invariants for every later input pair.
  */
enum ComparisonConfigurationError derives CanEqual:
  case InvalidProjectionDirections(algorithm: String, supplied: Int)
  case InvalidRegularisation(algorithm: String, supplied: Double)
  case InvalidIterationCount(algorithm: String, supplied: Int)
  case InvalidCellLimit(algorithm: String, supplied: Int)
  case InvalidProbabilityFloor(algorithm: String, supplied: Double)
  case InvalidGapCost(algorithm: String, supplied: Double)

  def message: String = this match
    case InvalidProjectionDirections(algorithm, supplied) =>
      s"$algorithm needs at least one projection direction, got $supplied."
    case InvalidRegularisation(algorithm, supplied) =>
      s"$algorithm needs finite positive regularisation, got $supplied."
    case InvalidIterationCount(algorithm, supplied) =>
      s"$algorithm needs at least one iteration, got $supplied."
    case InvalidCellLimit(algorithm, supplied) =>
      s"$algorithm needs a positive cell limit, got $supplied."
    case InvalidProbabilityFloor(algorithm, supplied) =>
      s"$algorithm needs a finite probability floor in (0, 1], got $supplied."
    case InvalidGapCost(algorithm, supplied) =>
      s"$algorithm needs a finite positive gap cost, got $supplied."

end ComparisonConfigurationError

opaque type ProjectionDirections = Int

object ProjectionDirections:
  val default: ProjectionDirections = 16

  def of(supplied: Int): Either[ComparisonConfigurationError, ProjectionDirections] =
    if supplied > 0 then Right(supplied)
    else
      Left(
        ComparisonConfigurationError.InvalidProjectionDirections(
          "sliced Wasserstein-1",
          supplied
        )
      )

  extension (directions: ProjectionDirections) def value: Int = directions

opaque type SinkhornRegularisation = Double

object SinkhornRegularisation:
  val default: SinkhornRegularisation = 1.0

  def of(supplied: Double): Either[ComparisonConfigurationError, SinkhornRegularisation] =
    if supplied.isFinite && supplied > 0.0 then Right(supplied)
    else
      Left(
        ComparisonConfigurationError.InvalidRegularisation("Sinkhorn", supplied)
      )

  extension (regularisation: SinkhornRegularisation) def value: Double = regularisation

opaque type SinkhornIterations = Int

object SinkhornIterations:
  val default: SinkhornIterations = 100

  def of(supplied: Int): Either[ComparisonConfigurationError, SinkhornIterations] =
    if supplied > 0 then Right(supplied)
    else Left(ComparisonConfigurationError.InvalidIterationCount("Sinkhorn", supplied))

  extension (iterations: SinkhornIterations) def value: Int = iterations

opaque type SinkhornCellLimit = Int

object SinkhornCellLimit:
  val default: SinkhornCellLimit = 4096

  def of(supplied: Int): Either[ComparisonConfigurationError, SinkhornCellLimit] =
    if supplied > 0 then Right(supplied)
    else Left(ComparisonConfigurationError.InvalidCellLimit("Sinkhorn", supplied))

  extension (limit: SinkhornCellLimit) def value: Int = limit

final case class SinkhornConfig(
    regularisation: SinkhornRegularisation,
    iterations: SinkhornIterations,
    cellLimit: SinkhornCellLimit
)

object SinkhornConfig:
  val default: SinkhornConfig =
    SinkhornConfig(
      SinkhornRegularisation.default,
      SinkhornIterations.default,
      SinkhornCellLimit.default
    )

opaque type ProbabilityFloor = Double

object ProbabilityFloor:
  val default: ProbabilityFloor = 1e-12

  def of(supplied: Double): Either[ComparisonConfigurationError, ProbabilityFloor] =
    if supplied.isFinite && supplied > 0.0 && supplied <= 1.0 then Right(supplied)
    else
      Left(
        ComparisonConfigurationError.InvalidProbabilityFloor(
          "Kullback-Leibler",
          supplied
        )
      )

  extension (floor: ProbabilityFloor) def value: Double = floor

opaque type ScanMatchGap = Double

object ScanMatchGap:
  val unit: ScanMatchGap = 1.0

  def of(supplied: Double): Either[ComparisonConfigurationError, ScanMatchGap] =
    if supplied.isFinite && supplied > 0.0 then Right(supplied)
    else Left(ComparisonConfigurationError.InvalidGapCost("ScanMatch", supplied))

  extension (gap: ScanMatchGap) def value: Double = gap
