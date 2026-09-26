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

enum TemplateFitError derives CanEqual:
  case Fit(trainingHash: ContentHash, underlying: LeastSquaresError)
  case Basis(id: String, columns: Vector[String], responseUnit: String)
  case Observation(key: String, fold: String, features: Vector[Double], response: Double)
  case Width(key: String, expected: Int, actual: Int)
  case DuplicateKey(key: String)
  case Split(requested: Set[String], available: Set[String], training: Int, heldOut: Int)
  case Receipt(expected: String, actual: String, reason: String)
  case Numerical(key: String, operation: String)
  case Evaluation(failed: Vector[String], total: Int)
  def message: String = this match
    case Fit(hash, error) => s"Template training ${hash.render}: ${error.message}"
    case Basis(i, c, u)   =>
      s"Template basis '$i' requires unique nonblank columns $c and response unit '$u'."
    case Observation(k, f, x, y) =>
      s"Template row $k in fold '$f' requires finite features $x and response $y."
    case Width(k, e, a)    => s"Template row $k has $a features; expected $e."
    case DuplicateKey(k)   => s"Template key $k occurs more than once, possibly across folds."
    case Split(r, a, t, h) =>
      s"Held-out folds $r must be present in $a and leave nonempty training/evaluation sets; got $t/$h rows."
    case Receipt(e, a, r) => s"Template receipt $a does not satisfy training request $e: $r."
    case Numerical(k, o)  => s"Template $o is not finite for key $k."
    case Evaluation(f, n) =>
      s"Cannot summarize $n held-out predictions with failed keys $f or no rows."

/** Fixed, predeclared features. The ID must change when their scientific definition changes.
  * This route performs no learned feature construction, centering or tuning.
  */
final class TemplateBasis private (
    val id: String,
    val columns: Vector[String],
    val responseUnit: String
):
  val hash: ContentHash = ContentHash.combineAll(
    Vector(
      ContentHash.ofString(id),
      ContentHash.ofString(responseUnit),
      ContentHash.of(IArray(columns.size.toDouble))
    ) ++ columns.map(ContentHash.ofString)
  )
object TemplateBasis:
  def of(
      id: String,
      columns: Vector[String],
      responseUnit: String
  ): Either[TemplateFitError, TemplateBasis] =
    Either.cond(
      id.trim.nonEmpty && responseUnit.trim.nonEmpty && columns.nonEmpty &&
        columns.forall(_.trim.nonEmpty) && columns.distinct.size == columns.size,
      new TemplateBasis(id, columns, responseUnit),
      TemplateFitError.Basis(id, columns, responseUnit)
    )

final class TemplateObservation[K] private (
    val key: K,
    val fold: String,
    val features: Vector[Double],
    val response: Double
)
object TemplateObservation:
  def of[K](
      key: K,
      fold: String,
      features: Vector[Double],
      response: Double
  ): Either[TemplateFitError, TemplateObservation[K]] =
    Either.cond(
      fold.trim.nonEmpty && features.nonEmpty && features.forall(
        _.isFinite
      ) && response.isFinite,
      new TemplateObservation(key, fold, features, response),
      TemplateFitError.Observation(key.toString, fold, features, response)
    )

/** Only this capability may be exported to a fitting backend. */
final class TemplateTraining[K] private[design] (
    val basis: TemplateBasis,
    val rows: Vector[TemplateObservation[K]],
    val hash: ContentHash
)

/** Evaluation is bound to the training payload from the validated split. */
final class TemplateHeldOut[K] private[design] (
    val rows: Vector[TemplateObservation[K]],
    val trainingHash: ContentHash,
    val hash: ContentHash
)
final class TemplateSplit[K] private (
    val basis: TemplateBasis,
    val rows: Vector[TemplateObservation[K]],
    val heldOutFolds: Set[String],
    val training: TemplateTraining[K],
    val heldOut: TemplateHeldOut[K]
)
object TemplateSplit:
  def of[K: KeyDigest](
      basis: TemplateBasis,
      rows: Vector[TemplateObservation[K]],
      heldOutFolds: Set[String]
  ): Either[TemplateFitError, TemplateSplit[K]] =
    val available     = rows.map(_.fold).toSet
    val (held, train) = rows.partition(r => heldOutFolds(r.fold))
    rows.find(_.features.size != basis.columns.size) match
      case Some(r) =>
        Left(TemplateFitError.Width(r.key.toString, basis.columns.size, r.features.size))
      case None =>
        rows.groupBy(_.key).collectFirst { case (k, rs) if rs.size > 1 => k } match
          case Some(k) => Left(TemplateFitError.DuplicateKey(k.toString))
          case None
              if heldOutFolds.isEmpty || !heldOutFolds.subsetOf(
                available
              ) || train.isEmpty || held.isEmpty =>
            Left(TemplateFitError.Split(heldOutFolds, available, train.size, held.size))
          case None =>
            def digest(rs: Vector[TemplateObservation[K]]): ContentHash =
              ContentHash.combineAll(
                Vector(basis.hash, ContentHash.of(IArray(rs.size.toDouble))) ++ rs.flatMap(r =>
                  Vector(
                    summon[KeyDigest[K]].digest(r.key),
                    ContentHash.ofString(r.fold),
                    ContentHash.of(IArray.from(r.features)),
                    ContentHash.of(IArray(r.response))
                  )
                )
              )
            val training = new TemplateTraining(basis, train, digest(train))
            Right(
              new TemplateSplit(
                basis,
                rows,
                heldOutFolds,
                training,
                new TemplateHeldOut(held, training.hash, digest(held))
              )
            )

final case class TemplatePrediction[K](
    key: K,
    fold: String,
    observed: Double,
    result: Either[TemplateFitError, (Double, Double)]
)

final class TemplateEvaluation[K] private[design] (
    val trainingHash: ContentHash,
    val heldOutHash: ContentHash,
    val rows: Vector[TemplatePrediction[K]]
):
  /** Descriptive prediction error, not a population-inference statistic. */
  def meanSquaredError: Either[TemplateFitError, Double] =
    val failures = rows.filter(_.result.isLeft).map(_.key.toString)
    if failures.nonEmpty || rows.isEmpty then
      Left(TemplateFitError.Evaluation(failures, rows.size))
    else
      val mse = rows
        .flatMap(_.result.toOption)
        .map { (_, residual) => residual * residual / rows.size }
        .sum
      Either.cond(
        mse.isFinite,
        mse,
        TemplateFitError.Numerical("held-out aggregate", "mean squared error")
      )

/** Native or imported no-intercept coefficients, bound to immutable training inputs.
  * A receipt is provenance from a trusted backend, not a cryptographic proof of fitting.
  * No coefficient p-values or standard errors are accepted or exposed.
  */
final class FittedTemplate[K] private (
    val training: TemplateTraining[K],
    val coefficients: Vector[Double],
    val backend: String,
    val methodId: String
):
  def evaluate(heldOut: TemplateHeldOut[K]): Either[TemplateFitError, TemplateEvaluation[K]] =
    if heldOut.trainingHash != training.hash then
      Left(
        TemplateFitError.Receipt(
          training.hash.render,
          heldOut.trainingHash.render,
          "evaluation belongs to another training payload"
        )
      )
    else
      Right(
        new TemplateEvaluation(
          training.hash,
          heldOut.hash,
          heldOut.rows.map { row =>
            val prediction = row.features.zip(coefficients).map(_ * _).sum
            val residual   = row.response - prediction
            val value      = Either.cond(
              prediction.isFinite && residual.isFinite,
              (prediction, residual),
              TemplateFitError.Numerical(row.key.toString, "prediction/residual")
            )
            TemplatePrediction(row.key, row.fold, row.response, value)
          }
        )
      )

object FittedTemplate:
  /** Full-rank scaled Householder QR, with no implicit intercept or preprocessing. */
  val nativeMethod: String        = "eyes4s.no-intercept-scaled-householder-qr/1"
  val nativeRankTolerance: Double = RelativeRankTolerance.default.value

  def fitNoIntercept[K](
      training: TemplateTraining[K]
  ): Either[TemplateFitError, FittedTemplate[K]] =
    LeastSquares
      .fit(training.rows.map(_.features), training.rows.map(_.response))
      .left
      .map(TemplateFitError.Fit(training.hash, _))
      .map(fit =>
        new FittedTemplate(training, fit.coefficients, "eyes4s native Scala", nativeMethod)
      )

  val method: String        = "eyes4s.no-intercept-r-lm-qr/1"
  val rankTolerance: Double = 1e-7

  /** Import only a full-rank fit with exactly the requested ordered feature names. */
  def importNoIntercept[K](
      training: TemplateTraining[K],
      trainingHash: String,
      columns: Vector[String],
      coefficients: Vector[Double],
      rank: Int,
      observations: Int,
      backend: String
  ): Either[TemplateFitError, FittedTemplate[K]] =
    val valid = trainingHash == training.hash.render && columns == training.basis.columns &&
      coefficients.size == columns.size && coefficients.forall(_.isFinite) &&
      rank == columns.size && observations == training.rows.size && observations >= rank && backend.trim.nonEmpty
    Either.cond(
      valid,
      new FittedTemplate(training, coefficients, backend, method),
      TemplateFitError.Receipt(
        training.hash.render,
        trainingHash,
        s"columns=$columns coefficients=$coefficients rank=$rank observations=$observations backend='$backend'"
      )
    )
