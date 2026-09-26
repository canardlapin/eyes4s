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

/** The interface a map comparison method actually satisfies (AGENTS.md rule 9).
  *
  * Every method scores a pair of maps as a [[Similarity]], but the measures
  * behind those scores differ in what they promise, and the registry keeps the
  * stronger promise instead of flattening it to [[SymmetricCompare]].
  */
enum MapMethodInterface derives CanEqual:
  /** A positive semi-definite [[Kernel]]; its law suite checks Gram matrices. */
  case Kernel

  /** A similarity derived from a true [[Metric]] by a declared transform; its
    * law suite checks the metric axioms on the distance and the transform.
    */
  case MetricDerived

  /** Symmetric and nothing stronger; its law suite checks symmetry. */
  case Symmetric

/** A registered map-similarity method: a closed vocabulary whose members keep
  * the interface their measure satisfies.
  *
  * Each member scores two checked `Mass[U]` on agreeing nominal grids as a
  * [[Similarity]] (larger is closer) through [[similarity]]. The three
  * subclasses carry the stronger typed instance where one exists: a
  * [[MapSimilarityMethod.KernelMethod]] exposes its [[Kernel]], a
  * [[MapSimilarityMethod.MetricMethod]] its [[Metric]] and the transform from
  * distance to similarity. Exact EMD has no member: no native solver exists.
  *
  * `token` is the method's stable wire identity, used by saved repetition
  * plans and their content hashes. eyesim's `method =` strings and the
  * historical bit-parity variants live in `eyes4s.compare.eyesim.EyesimCompat`,
  * not here.
  */
sealed abstract class MapSimilarityMethod private[compare] (val token: String) derives CanEqual:
  /** The interface this method's measure satisfies. */
  def interface: MapMethodInterface

  /** The method's scores as a symmetric similarity. */
  def similarity[U <: Unit2D]: SymmetricCompare[Mass[U], Similarity]

  /** As [[similarity]], with an explicit pair limit for a quadratic method.
    * Every method that is linear in grid cells ignores it.
    */
  def similarityWithin[U <: Unit2D](
      @scala.annotation.unused limit: DistanceCorrelationLimit
  ): SymmetricCompare[Mass[U], Similarity] = similarity[U]

  /** What the method measures, on which scale and in which direction. */
  final def info: MeasureInfo = similarity[Unit2D.Norm].info

  override def toString: String = token

object MapSimilarityMethod:
  /** A method whose scores are a positive semi-definite kernel. */
  abstract class KernelMethod private[compare] (token: String)
      extends MapSimilarityMethod(token):
    final def interface: MapMethodInterface = MapMethodInterface.Kernel

    /** The kernel, which also resumes in bounded quanta. */
    def kernel[U <: Unit2D]: Kernel[Mass[U]] & BoundedCompare[Mass[U], Mass[U], Similarity]
    final def similarity[U <: Unit2D]: SymmetricCompare[Mass[U], Similarity] = kernel[U]

  /** A similarity `upper - d` of a true metric `d` bounded above by `upper`.
    *
    * The metric is the measure; the similarity is its declared transform, and
    * is itself not a metric. The law suite checks both.
    */
  abstract class MetricMethod private[compare] (token: String)
      extends MapSimilarityMethod(token):
    final def interface: MapMethodInterface = MapMethodInterface.MetricDerived

    /** The metric the similarity is derived from. */
    def metric[U <: Unit2D]: Metric[Mass[U]]

    /** The metric's upper bound; the similarity is `upper - d`. */
    def upper: Double

    /** Name, summary and scale of the derived similarity. */
    protected def similarityInfo: MeasureInfo

    final def similarity[U <: Unit2D]: SymmetricCompare[Mass[U], Similarity] =
      val distance = metric[U]
      val bound    = upper
      val derived  = similarityInfo
      new SymmetricCompare[Mass[U], Similarity]:
        val info                                                              = derived
        def compare(a: Mass[U], b: Mass[U]): Either[CompareError, Similarity] =
          distance.compare(a, b).flatMap(d => Similarity.computed(info.name, bound - d.value))

  /** A method whose scores are symmetric and satisfy nothing stronger. */
  abstract class SymmetricMethod private[compare] (token: String)
      extends MapSimilarityMethod(token):
    final def interface: MapMethodInterface = MapMethodInterface.Symmetric

  /** Centered correlation of the cells, in [-1, 1]. */
  case object Pearson extends SymmetricMethod("Pearson"):
    def similarity[U <: Unit2D]: SymmetricCompare[Mass[U], Similarity] =
      Distribution.pearson[U]

  /** Pearson correlation of average tie ranks, in [-1, 1]. */
  case object Spearman extends SymmetricMethod("Spearman"):
    def similarity[U <: Unit2D]: SymmetricCompare[Mass[U], Similarity] =
      Distribution.spearman[U]

  /** atanh of the Pearson correlation with machine-epsilon endpoints
    * ([[Distribution.fisherZ]]). The token is the historical enum name.
    */
  case object FisherZ extends SymmetricMethod("FisherZMachineEpsilon"):
    def similarity[U <: Unit2D]: SymmetricCompare[Mass[U], Similarity] =
      Distribution.fisherZ[U]

  /** Normalized dot product, a kernel with scores in [0, 1] on mass. */
  case object Cosine extends KernelMethod("Cosine"):
    def kernel[U <: Unit2D]: Kernel[Mass[U]] & BoundedCompare[Mass[U], Mass[U], Similarity] =
      Distribution.cosine[U]

  /** One minus total variation. Total variation is the metric; eyesim calls
    * this similarity "l1".
    */
  case object L1Similarity extends MetricMethod("L1Similarity"):
    def metric[U <: Unit2D]: Metric[Mass[U]]  = Distribution.totalVariation[U]
    def upper: Double                         = 1.0
    protected def similarityInfo: MeasureInfo =
      MeasureInfo(
        "one minus total variation",
        "one minus total variation; larger is closer, and not itself a metric",
        MeasureScale.Bounded(0, 1),
        None
      )

  /** dot / (squared norms - dot), in [0, 1]. */
  case object ExtendedJaccard extends SymmetricMethod("ExtendedJaccard"):
    def similarity[U <: Unit2D]: SymmetricCompare[Mass[U], Similarity] =
      Distribution.extendedJaccard[U]

  /** Biased distance correlation of the cell values, in [0, 1], quadratic in cells. */
  case object DistanceCorrelation extends SymmetricMethod("DistanceCorrelation"):
    def similarity[U <: Unit2D]: SymmetricCompare[Mass[U], Similarity] =
      Distribution.distanceCorrelation[U]
    override def similarityWithin[U <: Unit2D](
        limit: DistanceCorrelationLimit
    ): SymmetricCompare[Mass[U], Similarity] = Distribution.distanceCorrelationWithin[U](limit)

  /** Every registered method, in the reference vocabulary's order. */
  val values: Vector[MapSimilarityMethod] =
    Vector(
      Pearson,
      Spearman,
      FisherZ,
      Cosine,
      L1Similarity,
      ExtendedJaccard,
      DistanceCorrelation
    )

  /** The registered method with this wire token. */
  def fromToken(token: String): Either[MapComparisonError, MapSimilarityMethod] =
    values.find(_.token == token).toRight(MapComparisonError.UnsupportedMethod(token))

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
      method: MapSimilarityMethod,
      limit: DistanceCorrelationLimit = DistanceCorrelationLimit.default
  ): MapScaleComparison[U] =
    val comparison = method.similarityWithin[U](limit)
    val sigmas     = (left.levels.map(_._1) ++ right.levels
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
            comparison.compare(a, b).left.map(MapScaleFailure.Comparison.apply)
        sigma -> result
      }
    )
