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

import eyes4s.kernel.ContentHash

/** The existing checked external-lm receipt, retained as data for saved plans.
  * Admission binds it to training data, ordered columns, rank and row count.
  * It is explicitly an imported fit, never evidence of a native refit.
  */
final class TemplateImportedFit private (
    val trainingHash: String,
    val columns: Vector[String],
    val coefficients: Vector[Double],
    val rank: Int,
    val observations: Int,
    val backend: String
):
  def bind[K](
      training: TemplateTraining[K, Vector[Double]]
  ): Either[TemplateError, FittedTemplate[K, Vector[Double]]] =
    Template.importFit(
      training,
      trainingHash,
      columns,
      coefficients,
      rank,
      observations,
      backend
    )
  def identity: ContentHash = ContentHash.combineAll(
    Vector(
      ContentHash.ofString(trainingHash),
      ContentHash.ofString(backend),
      ContentHash.of(IArray(rank.toDouble, observations.toDouble)),
      ContentHash.of(IArray.from(coefficients))
    ) ++ columns.map(ContentHash.ofString)
  )
object TemplateImportedFit:
  def of[K](
      training: TemplateTraining[K, Vector[Double]],
      trainingHash: String,
      columns: Vector[String],
      coefficients: Vector[Double],
      rank: Int,
      observations: Int,
      backend: String
  ): Either[TemplateError, TemplateImportedFit] =
    Template
      .importFit(training, trainingHash, columns, coefficients, rank, observations, backend)
      .map(_ =>
        new TemplateImportedFit(
          trainingHash,
          columns,
          coefficients,
          rank,
          observations,
          backend
        )
      )

private[design] enum TemplateValidationFit[X]:
  case Native[A]()                            extends TemplateValidationFit[A]
  case Imported(receipt: TemplateImportedFit) extends TemplateValidationFit[Vector[Double]]

  def stage: TemplateValidationStage = this match
    case Native()    => TemplateValidationStage.FittingWholeTraining
    case Imported(_) => TemplateValidationStage.CheckingWholeImportedReceipt
  def fit[K](training: TemplateTraining[K, X]): Either[TemplateError, FittedTemplate[K, X]] =
    this match
      case Native()          => Template.fit(training)
      case Imported(receipt) => receipt.bind(training)
