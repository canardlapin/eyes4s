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

import cats.syntax.all.*
import eyes4s.compare.*
import eyes4s.kernel.*

enum LearnedTemplateError derives CanEqual:
  case Observation(key: String, splitGroup: String, matchGroup: String, response: Double)
  case Definition(splitUnit: String, responseUnit: String)
  case DuplicateKey(key: String)
  case Split(
      requested: Set[String],
      available: Set[String],
      training: Int,
      heldOut: Int,
      excluded: Int
  )
  case Geometry(key: String, underlying: SurfaceError)
  case Feature(key: String, underlying: CompareError)
  case Fit(trainingHash: ContentHash, underlying: LeastSquaresError)
  case Identity(expected: ContentHash, actual: ContentHash)
  case Numerical(key: String, prediction: Double, response: Double)
  def message: String = this match
    case Observation(k, s, m, y) =>
      s"Learned template row=$k splitGroup='$s' matchGroup='$m' response=$y requires nonblank groups and a finite response."
    case Definition(s, r) =>
      s"Learned template splitUnit='$s' responseUnit='$r' must be nonblank."
    case DuplicateKey(k)      => s"Learned template row key=$k is duplicated."
    case Split(r, a, t, h, e) =>
      s"Learned template heldOut=$r available=$a training=$t heldOutRows=$h excluded=$e requires present held-out groups and nonempty training and evaluation."
    case Geometry(k, e) => s"Learned template row=$k: ${e.message}"
    case Feature(k, e)  => s"Learned template feature row=$k: ${e.message}"
    case Fit(h, e)      => s"Learned template training=${h.render}: ${e.message}"
    case Identity(e, a) =>
      s"Learned template expected training=${e.render}, received=${a.render}."
    case Numerical(k, p, y) =>
      s"Learned template row=$k prediction=$p response=$y has nonfinite prediction/residual."

/** One normalized map and response per trial; group labels are explicit scientific inputs. */
final class MapTemplateObservation[K, U <: Unit2D] private (
    val key: K,
    val splitGroup: String,
    val matchGroup: String,
    val map: Mass[U],
    val response: Double
)
object MapTemplateObservation:
  def of[K, U <: Unit2D](
      key: K,
      splitGroup: String,
      matchGroup: String,
      map: Mass[U],
      response: Double
  ): Either[LearnedTemplateError, MapTemplateObservation[K, U]] =
    Either.cond(
      splitGroup.trim.nonEmpty && matchGroup.trim.nonEmpty && response.isFinite,
      new MapTemplateObservation(key, splitGroup, matchGroup, map, response),
      LearnedTemplateError.Observation(key.toString, splitGroup, matchGroup, response)
    )

enum TemplateExclusionReason derives CanEqual:
  case HeldOutMatchGroup
final case class TemplateExcludedRow[K, U <: Unit2D](
    row: MapTemplateObservation[K, U],
    reason: TemplateExclusionReason
)

final class MapTemplateTraining[K, U <: Unit2D] private[design] (
    val rows: Vector[MapTemplateObservation[K, U]],
    val splitUnit: String,
    val responseUnit: String,
    val hash: ContentHash
)
final class MapTemplateHeldOut[K, U <: Unit2D] private[design] (
    val rows: Vector[MapTemplateObservation[K, U]],
    val trainingHash: ContentHash,
    val hash: ContentHash
)
final class MapTemplateSplit[K, U <: Unit2D] private (
    val rows: Vector[MapTemplateObservation[K, U]],
    val heldOutGroups: Set[String],
    val training: MapTemplateTraining[K, U],
    val heldOut: MapTemplateHeldOut[K, U],
    val excluded: Vector[TemplateExcludedRow[K, U]]
)
object MapTemplateSplit:
  def of[K: KeyDigest, U <: Unit2D](
      rows: Vector[MapTemplateObservation[K, U]],
      heldOutGroups: Set[String],
      splitUnit: String,
      responseUnit: String
  ): Either[LearnedTemplateError, MapTemplateSplit[K, U]] =
    val available          = rows.map(_.splitGroup).toSet
    val (held, candidates) = rows.partition(r => heldOutGroups(r.splitGroup))
    val heldMatches        = held.map(_.matchGroup).toSet
    val (excluded, train)  = candidates.partition(r => heldMatches(r.matchGroup))
    if splitUnit.trim.isEmpty || responseUnit.trim.isEmpty then
      Left(LearnedTemplateError.Definition(splitUnit, responseUnit))
    else if heldOutGroups.isEmpty || !heldOutGroups.subsetOf(
        available
      ) || train.isEmpty || held.isEmpty
    then
      Left(
        LearnedTemplateError
          .Split(heldOutGroups, available, train.size, held.size, excluded.size)
      )
    else
      rows.groupBy(_.key).collectFirst { case (k, rs) if rs.size > 1 => k } match
        case Some(key) => Left(LearnedTemplateError.DuplicateKey(key.toString))
        case None      =>
          val grid = train.head.map.grid
          rows
            .traverse_(r =>
              Agreement
                .grids(grid, r.map.grid)
                .left
                .map(LearnedTemplateError.Geometry(r.key.toString, _))
            )
            .map { _ =>
              def digest(rs: Vector[MapTemplateObservation[K, U]]): ContentHash =
                ContentHash.combineAll(
                  Vector(
                    ContentHash.ofString(LearnedTemplate.method),
                    ContentHash.ofString(splitUnit),
                    ContentHash.ofString(responseUnit),
                    ContentHash.ofString(grid.id.name),
                    ContentHash.ofString(grid.frame.id.name),
                    ContentHash.ofString(grid.frame.spec.yAxis.toString),
                    ContentHash.of(
                      IArray(
                        grid.frame.spec.xMin,
                        grid.frame.spec.xMax,
                        grid.frame.spec.yMin,
                        grid.frame.spec.yMax,
                        grid.nx.toDouble,
                        grid.ny.toDouble
                      )
                    ),
                    ContentHash.of(IArray(rs.size.toDouble))
                  ) ++ rs.flatMap(r =>
                    Vector(
                      summon[KeyDigest[K]].digest(r.key),
                      ContentHash.ofString(r.splitGroup),
                      ContentHash.ofString(r.matchGroup),
                      ContentHash.of(r.map.values),
                      ContentHash.of(IArray(r.response))
                    )
                  )
                )
              val training =
                new MapTemplateTraining(train, splitUnit, responseUnit, digest(train))
              new MapTemplateSplit(
                rows,
                heldOutGroups,
                training,
                new MapTemplateHeldOut(held, training.hash, digest(held)),
                excluded
                  .map(r => TemplateExcludedRow(r, TemplateExclusionReason.HeldOutMatchGroup))
              )
            }

final case class LearnedTemplatePrediction[K](
    key: K,
    splitGroup: String,
    matchGroup: String,
    observed: Double,
    result: Either[LearnedTemplateError, (Double, Double)]
)
final class LearnedTemplateEvaluation[K] private[design] (
    val trainingHash: ContentHash,
    val heldOutHash: ContentHash,
    val rows: Vector[LearnedTemplatePrediction[K]]
)

/** Equal-trial mean of training mass maps, cosine feature, through-origin response OLS.
  * The fit input cannot carry held-out maps. No response weighting, tuning,
  * implicit intercept, centering or learned density transform is performed.
  */
final class LearnedTemplate[K, U <: Unit2D] private (
    val training: MapTemplateTraining[K, U],
    val mean: Mass[U],
    val slope: Double,
    val trainingFeatures: Vector[(K, Double)]
):
  def evaluate(
      heldOut: MapTemplateHeldOut[K, U]
  ): Either[LearnedTemplateError, LearnedTemplateEvaluation[K]] =
    if heldOut.trainingHash != training.hash then
      Left(LearnedTemplateError.Identity(training.hash, heldOut.trainingHash))
    else
      Right(
        new LearnedTemplateEvaluation(
          training.hash,
          heldOut.hash,
          heldOut.rows.map { row =>
            val result = LearnedTemplate.feature(row, mean).flatMap { x =>
              val prediction = slope * x
              val residual   = row.response - prediction
              Either.cond(
                prediction.isFinite && residual.isFinite,
                (prediction, residual),
                LearnedTemplateError.Numerical(row.key.toString, prediction, row.response)
              )
            }
            LearnedTemplatePrediction(
              row.key,
              row.splitGroup,
              row.matchGroup,
              row.response,
              result
            )
          }
        )
      )

object LearnedTemplate:
  val method: String = "eyes4s.training-mean-cosine-response/1"
  private def feature[K, U <: Unit2D](
      row: MapTemplateObservation[K, U],
      mean: Mass[U]
  ): Either[LearnedTemplateError, Double] =
    Distribution
      .cosine[U]
      .compare(row.map, mean)
      .left
      .map(LearnedTemplateError.Feature(row.key.toString, _))
      .map(_.value)

  def fit[K, U <: Unit2D](
      training: MapTemplateTraining[K, U]
  ): Either[LearnedTemplateError, LearnedTemplate[K, U]] =
    for
      mean <- Mass
        .mean(training.rows.map(_.map))
        .left
        .map(LearnedTemplateError.Geometry("training mean", _))
      features <- training.rows.traverse(row => feature(row, mean).map(row.key -> _))
      fitted   <- LeastSquares
        .fit(features.map(x => Vector(x._2)), training.rows.map(_.response))
        .left
        .map(LearnedTemplateError.Fit(training.hash, _))
    yield new LearnedTemplate(training, mean, fitted.coefficients.head, features)
