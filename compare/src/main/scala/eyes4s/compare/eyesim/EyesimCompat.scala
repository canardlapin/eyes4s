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

package eyes4s.compare.eyesim

import eyes4s.compare.*
import eyes4s.kernel.*

/** eyesim compatibility: the reference R vocabulary and the bit-parity
  * variants that exist for migration and parity tests, kept out of the
  * primary comparison API.
  *
  * Nothing here is needed to run a native analysis. [[fromReference]] maps
  * eyesim's `method =` strings to the registered [[MapSimilarityMethod]]s, and
  * [[FisherZLegacy]] is the historical 1e-12-clamped Fisher z, which remains
  * readable in saved repetition plans that name it.
  */
object EyesimCompat:

  /** The registered method behind an eyesim `similarity(method = value)` name.
    * Exact EMD (`emd`) and the other transport names have no native alias and
    * are refused, as is every unknown name.
    */
  def fromReference(value: String): Either[MapComparisonError, MapSimilarityMethod] =
    referenceNames
      .collectFirst { case (name, method) if name == value => method }
      .toRight(MapComparisonError.UnsupportedMethod(value))

  /** The eyesim `method =` name of each registered method, in registry order. */
  val referenceNames: Vector[(String, MapSimilarityMethod)] = Vector(
    "pearson"  -> MapSimilarityMethod.Pearson,
    "spearman" -> MapSimilarityMethod.Spearman,
    "fisherz"  -> MapSimilarityMethod.FisherZ,
    "cosine"   -> MapSimilarityMethod.Cosine,
    "l1"       -> MapSimilarityMethod.L1Similarity,
    "jaccard"  -> MapSimilarityMethod.ExtendedJaccard,
    "dcov"     -> MapSimilarityMethod.DistanceCorrelation
  )

  /** Fisher z with the historical symmetric clamp of r at plus/minus
    * 0.999999999999, before eyes4s adopted the machine-epsilon endpoints of
    * [[Distribution.fisherZ]], which was this function until CR2. A perfect
    * correlation gives about 14.16 here and about 18.37 there; callers choose
    * the endpoint policy explicitly.
    */
  def fisherZLegacy[U <: Unit2D]: SymmetricCompare[Mass[U], Similarity] =
    new SymmetricCompare[Mass[U], Similarity]:
      private val r = Distribution.pearson[U]
      // The measure name and summary results saved before CR2 carry for this clamp.
      val info = MeasureInfo(
        "Fisher z",
        "atanh of the Pearson correlation; unbounded, and the form to average",
        MeasureScale.FisherZ,
        None
      )
      def compare(a: Mass[U], b: Mass[U]): Either[CompareError, Similarity] =
        r.compare(a, b).flatMap { s =>
          val clamped = math.max(-0.999999999999, math.min(0.999999999999, s.value))
          Similarity
            .of(0.5 * math.log((1 + clamped) / (1 - clamped)))
            .left
            .map(CompareError.InvalidScore(info.name, _))
        }

  /** [[fisherZLegacy]] as a map method. It is not registered in
    * [[MapSimilarityMethod.values]]: saved repetition plans that name its
    * token remain readable through [[fromToken]].
    */
  case object FisherZLegacy extends MapSimilarityMethod.SymmetricMethod("FisherZLegacy"):
    def similarity[U <: Unit2D]: SymmetricCompare[Mass[U], Similarity] = fisherZLegacy[U]

  /** Every method the compatibility layer adds to the registry's vocabulary. */
  val methods: Vector[MapSimilarityMethod] = Vector(FisherZLegacy)

  /** A registered or compatibility method by its wire token. */
  def fromToken(token: String): Either[MapComparisonError, MapSimilarityMethod] =
    MapSimilarityMethod
      .fromToken(token)
      .orElse(
        methods.find(_.token == token).toRight(MapComparisonError.UnsupportedMethod(token))
      )
