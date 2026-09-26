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

import eyes4s.compare.*
import eyes4s.kernel.*

enum RepetitionMeanError derives CanEqual:
  case Incomplete(level: String, requested: Int, successful: Int, required: Int)
  case Mean(error: ScoreMeanError)
  def message: String = this match
    case Incomplete(level, n, k, min) =>
      s"Repetition $level mean: $k successful of $n requested; required=$min."
    case Mean(e) => e.message

/** One comparison's scale rows are retained even when its mean excludes failures. */
final class RepetitionScaleMean[U <: Unit2D] private[design] (
    val source: MapScaleComparison[U],
    val policy: FailurePolicy,
    val result: Either[RepetitionMeanError, Similarity]
):
  def requested: Int    = source.requested
  def successful: Int   = source.contributing
  def contributing: Int = if result.isRight then successful else 0

/** Equal weight per contributing comparison, never a pooled mean of all scale cells. */
final class RepetitionNestedMean[U <: Unit2D] private[design] (
    val comparisons: Vector[RepetitionScaleMean[U]],
    val policy: FailurePolicy,
    val result: Either[RepetitionMeanError, Similarity]
):
  def requested: Int        = comparisons.size
  def successful: Int       = comparisons.count(_.result.isRight)
  def contributing: Int     = if result.isRight then successful else 0
  def requestedScales: Int  = comparisons.map(_.requested).sum
  def successfulScales: Int = comparisons.map(_.successful).sum

object RepetitionAggregation:
  private def mean(
      level: String,
      requested: Int,
      values: Vector[Similarity],
      policy: FailurePolicy
  ): Either[RepetitionMeanError, Similarity] =
    val minimum = policy match
      case FailurePolicy.RequireAll            => math.max(1, requested)
      case FailurePolicy.SuccessfulOnly(count) => count.value
    if values.size < minimum then
      Left(RepetitionMeanError.Incomplete(level, requested, values.size, minimum))
    else ScoreMean[Similarity].mean(values).left.map(RepetitionMeanError.Mean.apply)

  def scales[U <: Unit2D](
      comparison: MapScaleComparison[U],
      policy: FailurePolicy
  ): RepetitionScaleMean[U] =
    new RepetitionScaleMean(
      comparison,
      policy,
      mean("scales", comparison.requested, comparison.rows.flatMap(_._2.toOption), policy)
    )

  def comparisons[U <: Unit2D](
      values: Vector[RepetitionScaleMean[U]],
      policy: FailurePolicy
  ): RepetitionNestedMean[U] =
    new RepetitionNestedMean(
      values,
      policy,
      mean("comparisons", values.size, values.flatMap(_.result.toOption), policy)
    )
