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

/** Failures of the template family: design, admission, split, fit and
  * evaluation. Every case names the row, group, identity or operand that
  * failed.
  */
enum TemplateError derives CanEqual:
  case Basis(id: String, columns: Vector[String], responseUnit: String)
  case Definition(splitUnit: String, responseUnit: String)
  case Observation(
      key: String,
      splitGroup: String,
      matchGroup: Option[String],
      response: Double
  )
  case Features(key: String, features: Vector[Double])
  case Width(key: String, expected: Int, actual: Int)
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
  case Route(method: String, route: String)
  case Identity(expected: ContentHash, actual: ContentHash)
  case Receipt(expected: String, actual: String, reason: String)
  case Numerical(key: String, prediction: Double, response: Double)
  case Evaluation(failed: Vector[String], total: Int)
  case Aggregate(operation: String, value: Double)
  def message: String = this match
    case Basis(i, c, u) =>
      s"Template basis '$i' requires unique nonblank columns $c and response unit '$u'."
    case Definition(s, r) =>
      s"Template splitUnit='$s' responseUnit='$r' must be nonblank."
    case Observation(k, s, m, y) =>
      s"Template row=$k splitGroup='$s' matchGroup=${m.fold("none")(g => s"'$g'")} " +
        s"response=$y requires nonblank groups and a finite response."
    case Features(k, x)  => s"Template row=$k requires nonempty finite features $x."
    case Width(k, e, a)  => s"Template row=$k has $a features; expected $e."
    case DuplicateKey(k) =>
      s"Template row key=$k occurs more than once, possibly across groups."
    case Split(r, a, t, h, e) =>
      s"Template heldOut=$r available=$a training=$t heldOutRows=$h excluded=$e requires " +
        "present held-out groups and nonempty training and evaluation."
    case Geometry(k, e) => s"Template row=$k: ${e.message}"
    case Feature(k, e)  => s"Template feature row=$k: ${e.message}"
    case Fit(h, e)      => s"Template training=${h.render}: ${e.message}"
    case Route(m, r)    => s"Template recipe method=$m cannot be fitted by route=$r."
    case Identity(e, a) =>
      s"Template expected training=${e.render}, received=${a.render}."
    case Receipt(e, a, r)   => s"Template receipt $a does not satisfy training request $e: $r."
    case Numerical(k, p, y) =>
      s"Template row=$k prediction=$p response=$y has nonfinite prediction/residual."
    case Evaluation(f, n) =>
      s"Cannot summarize $n held-out predictions with failed keys $f or no rows."
    case Aggregate(o, v) => s"Template $o is not finite: $v."

/** Fixed, predeclared features. The ID must change when their scientific definition changes.
  * No learned feature construction, centering or tuning is performed.
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
  ): Either[TemplateError, TemplateBasis] =
    Either.cond(
      id.trim.nonEmpty && responseUnit.trim.nonEmpty && columns.nonEmpty &&
        columns.forall(_.trim.nonEmpty) && columns.distinct.size == columns.size,
      new TemplateBasis(id, columns, responseUnit),
      TemplateError.Basis(id, columns, responseUnit)
    )

/** How a template model turns one trial's input `X` into regression features.
  *
  * The response model is always `response = features . beta`, fitted through
  * the origin on training trials only by the shared scaled Householder QR
  * ([[LeastSquares]]); there is no implicit intercept, centering or tuning.
  * A design decides the features:
  *
  *   - [[TemplateDesign.Fixed]]: predeclared feature vectors, `X = Vector[Double]`;
  *   - [[TemplateDesign.MeanMap]]: the cosine of each trial's map with the
  *     equal-trial mean of the training maps, `X = Mass[U]`.
  *
  * The `method` string is the recipe's stable scientific identity.
  */
sealed trait TemplateDesign[X]:
  /** The recipe's stable method identity. */
  def method: String

  /** Unit of the response. */
  def responseUnit: String

  /** Names of the regression features, in coefficient order. */
  def featureNames: Vector[String]

  /** Unit of one feature, for a coefficient's unit `response / feature`. */
  def featureUnit: String

  private[design] def admit(key: String, input: X): Either[TemplateError, Unit]
  private[design] def digest[K: KeyDigest](rows: Vector[TemplateObservation[K, X]]): ContentHash

  /** Whether every row must name a match group. */
  private[design] def requiresMatchGroup: Boolean

  /** The template learned from training rows, for a design that learns one. */
  private[design] def learn[K](
      rows: Vector[TemplateObservation[K, X]]
  ): Either[TemplateError, Option[X]]

  /** Inputs that must share geometry agree with the first training input. */
  private[design] def agree[K](
      first: TemplateObservation[K, X],
      rows: Vector[TemplateObservation[K, X]]
  ): Either[TemplateError, Unit]
  private[design] def features(
      key: String,
      input: X,
      learned: Option[X]
  ): Either[TemplateError, Vector[Double]]

object TemplateDesign:
  /** The native fixed-feature method: scaled Householder QR at relative rank
    * tolerance `1e-7`. Revision 1 used `1e-12`; its recipes are refused.
    */
  val nativeMethod: String        = "eyes4s.no-intercept-scaled-householder-qr/2"
  val nativeRankTolerance: Double = RelativeRankTolerance.default.value

  /** The historical fixed-feature method fitted by R's `lm` and imported
    * through a receipt ([[Template.importFit]]).
    */
  val importedLmMethod: String        = "eyes4s.no-intercept-r-lm-qr/1"
  val importedLmRankTolerance: Double = 1e-7

  /** The training-mean map, cosine feature, through-origin response method. */
  val meanMapMethod: String = "eyes4s.training-mean-cosine-response/1"

  /** Which route fits a fixed-feature design. */
  enum FixedRoute derives CanEqual:
    /** [[Template.fit]]: the native solver. */
    case Native

    /** [[Template.importFit]]: a receipt from R's `lm`. */
    case ImportedLm

  /** Predeclared features. Each row's features must match the basis width. */
  final class Fixed private[design] (val basis: TemplateBasis, val route: FixedRoute)
      extends TemplateDesign[Vector[Double]]:
    def method: String = route match
      case FixedRoute.Native     => nativeMethod
      case FixedRoute.ImportedLm => importedLmMethod
    def responseUnit: String         = basis.responseUnit
    def featureNames: Vector[String] = basis.columns
    def featureUnit: String          = "feature unit"

    private[design] def admit(key: String, input: Vector[Double]): Either[TemplateError, Unit] =
      if input.isEmpty || !input.forall(_.isFinite) then
        Left(TemplateError.Features(key, input))
      else
        Either.cond(
          input.size == basis.columns.size,
          (),
          TemplateError.Width(key, basis.columns.size, input.size)
        )

    /** Rows without match groups hash as fixed-feature recipes always have;
      * rows with match groups add a marker and every row's group.
      */
    private[design] def digest[K: KeyDigest](
        rows: Vector[TemplateObservation[K, Vector[Double]]]
    ): ContentHash =
      val grouped = rows.exists(_.matchGroup.isDefined)
      ContentHash.combineAll(
        Vector(basis.hash, ContentHash.of(IArray(rows.size.toDouble))) ++
          Option.when(grouped)(ContentHash.ofString("match-groups")).toVector ++
          rows.flatMap(r =>
            Vector(
              summon[KeyDigest[K]].digest(r.key),
              ContentHash.ofString(r.splitGroup),
              ContentHash.of(IArray.from(r.input)),
              ContentHash.of(IArray(r.response))
            ) ++ Option.when(grouped)(ContentHash.ofString(r.matchGroup.getOrElse(""))).toVector
          )
      )

    private[design] def features(
        key: String,
        input: Vector[Double],
        learned: Option[Vector[Double]]
    ): Either[TemplateError, Vector[Double]] = Right(input)

    private[design] def requiresMatchGroup: Boolean = false

    private[design] def learn[K](
        rows: Vector[TemplateObservation[K, Vector[Double]]]
    ): Either[TemplateError, Option[Vector[Double]]] = Right(None)

    private[design] def agree[K](
        first: TemplateObservation[K, Vector[Double]],
        rows: Vector[TemplateObservation[K, Vector[Double]]]
    ): Either[TemplateError, Unit] = Right(())

  /** The training-mean map design: the equal-trial mean of the training maps
    * is the template, and a trial's feature is its map's cosine with it.
    */
  final class MeanMap[U <: Unit2D] private[design] (
      val splitUnit: String,
      val responseUnit: String
  )(using unit: UnitLabel[U])
      extends TemplateDesign[Mass[U]]:
    def method: String               = meanMapMethod
    def featureNames: Vector[String] = Vector("training-mean cosine")
    def featureUnit: String          = "cosine similarity"
    def unitSymbol: String           = unit.symbol

    private[design] def admit(key: String, input: Mass[U]): Either[TemplateError, Unit] =
      Right(())

    private[design] def digest[K: KeyDigest](
        rows: Vector[TemplateObservation[K, Mass[U]]]
    ): ContentHash = rows.headOption.fold(ContentHash.empty) { first =>
      val grid = first.input.grid
      ContentHash.combineAll(
        Vector(
          ContentHash.ofString(meanMapMethod),
          ContentHash.ofString(splitUnit),
          ContentHash.ofString(responseUnit),
          ContentHash.ofString(unit.symbol),
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
          ContentHash.of(IArray(rows.size.toDouble))
        ) ++ rows.flatMap(r =>
          Vector(
            summon[KeyDigest[K]].digest(r.key),
            ContentHash.ofString(r.splitGroup),
            ContentHash.ofString(r.matchGroup.getOrElse("")),
            ContentHash.of(r.input.values),
            ContentHash.of(IArray(r.response))
          )
        )
      )
    }

    private[design] def requiresMatchGroup: Boolean = true

    private[design] def agree[K](
        first: TemplateObservation[K, Mass[U]],
        rows: Vector[TemplateObservation[K, Mass[U]]]
    ): Either[TemplateError, Unit] =
      rows.traverse_(r =>
        Agreement
          .grids(first.input.grid, r.input.grid)
          .left
          .map(TemplateError.Geometry(r.key.toString, _))
      )

    private[design] def learn[K](
        rows: Vector[TemplateObservation[K, Mass[U]]]
    ): Either[TemplateError, Option[Mass[U]]] =
      Mass
        .mean(rows.map(_.input))
        .left
        .map(TemplateError.Geometry("training mean", _))
        .map(Some(_))

    private[design] def features(
        key: String,
        input: Mass[U],
        learned: Option[Mass[U]]
    ): Either[TemplateError, Vector[Double]] =
      learned
        .toRight(TemplateError.Feature(key, CompareError.TooShort("training mean", 0, 1)))
        .flatMap(mean =>
          Distribution
            .cosine[U]
            .compare(input, mean)
            .left
            .map(TemplateError.Feature(key, _))
            .map(x => Vector(x.value))
        )

  /** Predeclared features fitted natively. */
  def fixed(basis: TemplateBasis): Fixed =
    new Fixed(basis, FixedRoute.Native)

  /** Predeclared features whose fit is imported from R's `lm`. */
  def importedLm(basis: TemplateBasis): Fixed =
    new Fixed(basis, FixedRoute.ImportedLm)

  /** The training-mean map design, with the scientific units of the split
    * groups (for example participant) and of the response.
    */
  def meanMap[U <: Unit2D: UnitLabel](
      splitUnit: String,
      responseUnit: String
  ): Either[TemplateError, MeanMap[U]] =
    Either.cond(
      splitUnit.trim.nonEmpty && responseUnit.trim.nonEmpty,
      new MeanMap[U](splitUnit, responseUnit),
      TemplateError.Definition(splitUnit, responseUnit)
    )
end TemplateDesign

/** One trial: a typed key, the split group that assigns it to training or
  * held-out data, an optional match group, its input and a finite response.
  *
  * A training row whose match group occurs among the held-out rows is
  * excluded from training (and retained as excluded), so the same item never
  * informs both sides. Fixed features may omit match groups; the mean-map
  * design requires them.
  */
final class TemplateObservation[K, X] private (
    val key: K,
    val splitGroup: String,
    val matchGroup: Option[String],
    val input: X,
    val response: Double
)
object TemplateObservation:
  def of[K, X](
      key: K,
      splitGroup: String,
      input: X,
      response: Double,
      matchGroup: Option[String] = None
  ): Either[TemplateError, TemplateObservation[K, X]] =
    Either.cond(
      splitGroup.trim.nonEmpty && matchGroup.forall(_.trim.nonEmpty) && response.isFinite,
      new TemplateObservation(key, splitGroup, matchGroup, input, response),
      TemplateError.Observation(key.toString, splitGroup, matchGroup, response)
    )

enum TemplateExclusionReason derives CanEqual:
  case HeldOutMatchGroup

final case class TemplateExcludedRow[K, X](
    row: TemplateObservation[K, X],
    reason: TemplateExclusionReason
)

/** Only this capability may be passed to a fitting route: it cannot carry
  * held-out rows.
  */
final class TemplateTraining[K, X] private[design] (
    val design: TemplateDesign[X],
    val rows: Vector[TemplateObservation[K, X]],
    val hash: ContentHash
)

/** Evaluation rows, bound to the training payload of the same split. */
final class TemplateHeldOut[K, X] private[design] (
    val rows: Vector[TemplateObservation[K, X]],
    val trainingHash: ContentHash,
    val hash: ContentHash
)

/** A checked partition of every row into training, held-out and excluded. */
final class TemplateSplit[K, X] private (
    val design: TemplateDesign[X],
    val rows: Vector[TemplateObservation[K, X]],
    val heldOutGroups: Set[String],
    val training: TemplateTraining[K, X],
    val heldOut: TemplateHeldOut[K, X],
    val excluded: Vector[TemplateExcludedRow[K, X]]
)
object TemplateSplit:
  /** Admit every row under `design`, then partition by the explicit held-out
    * split groups. Every requested group must exist, keys are unique across
    * the whole split, and both partitions are nonempty. The training digest
    * covers training rows only.
    */
  def of[K: KeyDigest, X](
      design: TemplateDesign[X],
      rows: Vector[TemplateObservation[K, X]],
      heldOutGroups: Set[String]
  ): Either[TemplateError, TemplateSplit[K, X]] =
    val available          = rows.map(_.splitGroup).toSet
    val (held, candidates) = rows.partition(r => heldOutGroups(r.splitGroup))
    val heldMatches        = held.flatMap(_.matchGroup).toSet
    val (excluded, train)  =
      candidates.partition(r => r.matchGroup.exists(heldMatches.contains))
    for
      _ <- rows.traverse_ { r =>
        Either
          .cond(
            !design.requiresMatchGroup || r.matchGroup.isDefined,
            (),
            TemplateError.Observation(r.key.toString, r.splitGroup, r.matchGroup, r.response)
          )
          .flatMap(_ => design.admit(r.key.toString, r.input))
      }
      _ <- rows.groupBy(_.key).collectFirst { case (k, rs) if rs.size > 1 => k } match
        case Some(key) => Left(TemplateError.DuplicateKey(key.toString))
        case None      => Right(())
      _ <- Either.cond(
        heldOutGroups.nonEmpty && heldOutGroups.subsetOf(available) && train.nonEmpty &&
          held.nonEmpty,
        (),
        TemplateError.Split(heldOutGroups, available, train.size, held.size, excluded.size)
      )
      _ <- design.agree(train.head, rows)
    yield
      val training = new TemplateTraining(design, train, design.digest(train))
      new TemplateSplit(
        design,
        rows,
        heldOutGroups,
        training,
        new TemplateHeldOut(held, training.hash, design.digest(held)),
        excluded.map(r => TemplateExcludedRow(r, TemplateExclusionReason.HeldOutMatchGroup))
      )

/** One held-out row: its key and groups, the observed response, and either
  * `(prediction, residual)` or an operand-bearing error.
  */
final case class TemplatePrediction[K](
    key: K,
    splitGroup: String,
    matchGroup: Option[String],
    observed: Double,
    result: Either[TemplateError, (Double, Double)]
)

final class TemplateEvaluation[K] private[design] (
    val trainingHash: ContentHash,
    val heldOutHash: ContentHash,
    val rows: Vector[TemplatePrediction[K]]
):
  /** Descriptive prediction error, not a population-inference statistic.
    * Refuses failed predictions; the denominator never changes quietly.
    */
  def meanSquaredError: Either[TemplateError, Double] =
    val failures = rows.filter(_.result.isLeft).map(_.key.toString)
    if failures.nonEmpty || rows.isEmpty then
      Left(TemplateError.Evaluation(failures, rows.size))
    else
      val mse = rows
        .flatMap(_.result.toOption)
        .map((_, residual) => residual * residual / rows.size)
        .sum
      Either.cond(mse.isFinite, mse, TemplateError.Aggregate("mean squared error", mse))

/** Through-origin coefficients bound to immutable training inputs, with the
  * learned template when the design learns one (the mean-map design's
  * training mean) and every training row's features.
  *
  * An imported receipt is provenance from a trusted backend, not a
  * cryptographic proof of fitting. No coefficient p-values or standard errors
  * are accepted or exposed.
  */
final class FittedTemplate[K, X] private[design] (
    val training: TemplateTraining[K, X],
    val coefficients: Vector[Double],
    val template: Option[X],
    val trainingFeatures: Vector[(K, Vector[Double])],
    val backend: String,
    val method: String
):
  def evaluate(heldOut: TemplateHeldOut[K, X]): Either[TemplateError, TemplateEvaluation[K]] =
    if heldOut.trainingHash != training.hash then
      Left(TemplateError.Identity(training.hash, heldOut.trainingHash))
    else
      Right(
        new TemplateEvaluation(
          training.hash,
          heldOut.hash,
          heldOut.rows.map { row =>
            val result = training.design
              .features(row.key.toString, row.input, template)
              .flatMap { x =>
                val prediction = x.zip(coefficients).map(_ * _).sum
                val residual   = row.response - prediction
                Either.cond(
                  prediction.isFinite && residual.isFinite,
                  (prediction, residual),
                  TemplateError.Numerical(row.key.toString, prediction, row.response)
                )
              }
            TemplatePrediction(row.key, row.splitGroup, row.matchGroup, row.response, result)
          }
        )
      )

/** A fit and its held-out evaluation, from [[Template.crossValidate]]. */
final class TemplateValidation[K, X] private[design] (
    val fitted: FittedTemplate[K, X],
    val evaluation: TemplateEvaluation[K]
)

/** The template family's entry points, one per task:
  *
  *   - [[fit]] fits a design's through-origin response model on training rows;
  *   - [[crossValidate]] fits on a split's training rows and evaluates its held-out rows;
  *   - [[decompose]] fits one response map from predictor maps across cells.
  *
  * [[importFit]] admits a fixed-feature fit computed by R's `lm` from a
  * training-only export, for the historical recipe.
  */
object Template:
  /** Fit natively on training rows only. A fixed-feature design whose route
    * is an imported `lm` fit is refused as [[TemplateError.Route]].
    */
  def fit[K, X](training: TemplateTraining[K, X]): Either[TemplateError, FittedTemplate[K, X]] =
    val design = training.design
    val native = design match
      case fixed: TemplateDesign.Fixed  => fixed.route == TemplateDesign.FixedRoute.Native
      case _: TemplateDesign.MeanMap[?] => true
    for
      _        <- Either.cond(native, (), TemplateError.Route(design.method, "native fit"))
      learned  <- design.learn(training.rows)
      features <- training.rows.traverse(row =>
        design.features(row.key.toString, row.input, learned).map(row.key -> _)
      )
      fitted <- LeastSquares
        .fit(features.map(_._2), training.rows.map(_.response))
        .left
        .map(TemplateError.Fit(training.hash, _))
    yield new FittedTemplate(
      training,
      fitted.coefficients,
      learned,
      features,
      "eyes4s native Scala",
      design.method
    )

  /** [[fit]] on the split's training rows, then evaluation of its held-out rows. */
  def crossValidate[K, X](
      split: TemplateSplit[K, X]
  ): Either[TemplateError, TemplateValidation[K, X]] =
    for
      fitted     <- fit(split.training)
      evaluation <- fitted.evaluate(split.heldOut)
    yield new TemplateValidation(fitted, evaluation)

  /** Import only a full-rank `lm` fit with exactly the requested ordered
    * feature names, for a fixed-feature design whose route is the imported fit.
    */
  def importFit[K](
      training: TemplateTraining[K, Vector[Double]],
      trainingHash: String,
      columns: Vector[String],
      coefficients: Vector[Double],
      rank: Int,
      observations: Int,
      backend: String
  ): Either[TemplateError, FittedTemplate[K, Vector[Double]]] =
    training.design match
      case fixed: TemplateDesign.Fixed if fixed.route == TemplateDesign.FixedRoute.ImportedLm =>
        val valid = trainingHash == training.hash.render && columns == fixed.basis.columns &&
          coefficients.size == columns.size && coefficients.forall(_.isFinite) &&
          rank == columns.size && observations == training.rows.size &&
          observations >= rank && backend.trim.nonEmpty
        Either.cond(
          valid,
          new FittedTemplate(
            training,
            coefficients,
            None,
            training.rows.map(r => r.key -> r.input),
            backend,
            fixed.method
          ),
          TemplateError.Receipt(
            training.hash.render,
            trainingHash,
            s"columns=$columns coefficients=$coefficients rank=$rank " +
              s"observations=$observations backend='$backend'"
          )
        )
      case design => Left(TemplateError.Route(design.method, "imported lm fit"))

  /** Cellwise OLS of one response map on predictor maps; see [[SurfaceOlsFit]]
    * for what is retained.
    */
  def decompose[U <: Unit2D](
      response: Mass[U],
      predictors: PredictorSet[U],
      intercept: Intercept,
      rankTolerance: RelativeRankTolerance = RelativeRankTolerance.default
  ): Either[DecompositionError, SurfaceOlsFit[U]] =
    SurfaceDecomposition.ols(response, predictors, intercept, rankTolerance)
