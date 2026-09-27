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

package eyes4s.kernel

/** Failures when selecting highest-density regions from a [[Mass]]. */
enum MassLevelError derives CanEqual:
  case InvalidCoverage(index: Int, coverage: Double)
  case InsufficientTotal(index: Int, coverage: Double, total: Double)

  def message: String = this match
    case InvalidCoverage(index, coverage) =>
      s"Coverage at index $index must be finite and in (0, 1]; it was $coverage."
    case InsufficientTotal(index, coverage, total) =>
      s"Coverage at index $index was $coverage, but the supplied mass totals only $total."

/** One thresholded highest-density region.
  *
  * `coveredMass` and `cells` include every cell equal to `threshold`: a
  * threshold never arbitrarily splits an exact tie.
  */
final class MassLevel private (
    val coverage: Double,
    val threshold: Double,
    val coveredMass: Double,
    val cells: Int
) derives CanEqual

object MassLevel:
  private[kernel] def make(
      coverage: Double,
      threshold: Double,
      coveredMass: Double,
      cells: Int
  ): MassLevel =
    new MassLevel(coverage, threshold, coveredMass, cells)

/** Highest-density regions of a mass over its cells.
  *
  * A requested coverage is accumulated from the largest cell values downward.
  * The selected threshold is the largest value whose thresholded region carries
  * at least that coverage. Values equal to the threshold are selected together.
  *
  * `Mass` may be accepted with a total merely close to one. This operation uses
  * its supplied values as-is: it does not renormalise, and refuses a coverage
  * above their compensated total. Accumulation uses Neumaier compensation; the
  * reported totals are still IEEE-754 approximations to the supplied values.
  */
object MassLevels:

  /** Select one highest-density region for every requested coverage.
    *
    * The returned vector has the same order and duplicates as `coverages`.
    */
  def of[U <: Unit2D](
      mass: Mass[U],
      coverages: Vector[Double]
  ): Either[MassLevelError, Vector[MassLevel]] =
    validate(coverages).flatMap { _ =>
      if coverages.isEmpty then Right(Vector.empty)
      else
        val values = mass.values.toVector.sortWith(_ > _)
        val total  = compensatedSum(values)
        coverages.zipWithIndex.foldLeft[Either[MassLevelError, Vector[MassLevel]]](
          Right(Vector.empty)
        ) {
          case (Right(levels), (coverage, index)) =>
            if total < coverage then
              Left(MassLevelError.InsufficientTotal(index, coverage, total))
            else select(values, coverage, index).map(levels :+ _)
          case (left @ Left(_), _) => left
        }
    }

  private def validate(coverages: Vector[Double]): Either[MassLevelError, Unit] =
    coverages.zipWithIndex.collectFirst {
      case (coverage, index) if !coverage.isFinite || coverage <= 0.0 || coverage > 1.0 =>
        MassLevelError.InvalidCoverage(index, coverage)
    } match
      case Some(error) => Left(error)
      case None        => Right(())

  private def select(
      values: Vector[Double],
      coverage: Double,
      coverageIndex: Int
  ): Either[MassLevelError, MassLevel] =
    var accumulator = CompensatedSum()
    var index       = 0
    while index < values.length do
      val threshold = values(index)
      while index < values.length && values(index) == threshold do
        accumulator = accumulator.add(values(index))
        index += 1
      if accumulator.value >= coverage then
        return Right(MassLevel.make(coverage, threshold, accumulator.value, index))
    Left(MassLevelError.InsufficientTotal(coverageIndex, coverage, accumulator.value))

  private def compensatedSum(values: IterableOnce[Double]): Double =
    values.iterator.foldLeft(CompensatedSum())((sum, value) => sum.add(value)).value

  /** Neumaier summation keeps the discarded low-order part separately. */
  private final case class CompensatedSum(sum: Double = 0.0, correction: Double = 0.0):
    def add(value: Double): CompensatedSum =
      val next = sum + value
      val lost =
        if math.abs(sum) >= math.abs(value) then (sum - next) + value
        else (value - next) + sum
      CompensatedSum(next, correction + lost)

    def value: Double = sum + correction
