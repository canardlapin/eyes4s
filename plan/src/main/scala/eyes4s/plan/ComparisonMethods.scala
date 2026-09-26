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

package eyes4s.plan

import eyes4s.compare.*
import eyes4s.design.SignedDifference
import eyes4s.kernel.*

/** Built-in identities of the registered map methods other than cosine, whose
  * identity is `DefinitionId.cosine`. Each is carried by a saved study plan
  * or result that uses the method.
  */
object ComparisonMethodDefinitions:
  val pearson: DefinitionId             = DefinitionId.builtIn("eyes4s.pearson", 1)
  val spearman: DefinitionId            = DefinitionId.builtIn("eyes4s.spearman", 1)
  val fisherZ: DefinitionId             = DefinitionId.builtIn("eyes4s.fisher-z", 1)
  val l1Similarity: DefinitionId        = DefinitionId.builtIn("eyes4s.l1-similarity", 1)
  val extendedJaccard: DefinitionId     = DefinitionId.builtIn("eyes4s.extended-jaccard", 1)
  val distanceCorrelation: DefinitionId = DefinitionId.builtIn("eyes4s.distance-correlation", 1)

/** One registered map comparison method: its typed measure, its stable
  * identity, and the descriptor an application presents it with.
  *
  * The measure keeps its interface ([[MapSimilarityMethod.interface]]); the
  * descriptor's declared properties and execution capability are derived from
  * that interface and the measure's declared scale, never from a name.
  */
final class ComparisonMethod private[plan] (
    val method: MapSimilarityMethod,
    val id: DefinitionId,
    val name: String
):
  /** The score component: the similarity on the measure's declared scale,
    * larger is closer, with the matched-minus-control difference.
    */
  val component: ScoreComponent[Similarity, SignedDifference] =
    ScoreComponent.literal(
      "value",
      s"$name; difference is matched minus control",
      ParameterUnits.Dimensionless,
      method.info.scale,
      ScoreDirection.HigherIsCloser
    )

  /** Symmetric always; bounded and non-negative as the declared scale says. */
  val properties: Set[ComparisonProperty] =
    val (bounded, nonNegative) = method.info.scale match
      case MeasureScale.Bounded(lo, _)                             => (true, lo >= 0)
      case MeasureScale.Probability                                => (true, true)
      case MeasureScale.Correlation                                => (true, false)
      case MeasureScale.DistanceLike                               => (false, true)
      case MeasureScale.FisherZ | MeasureScale.UnboundedSimilarity => (false, false)
    Set(ComparisonProperty.Symmetric) ++
      Option.when(bounded)(ComparisonProperty.Bounded) ++
      Option.when(nonNegative)(ComparisonProperty.NonNegative)

  /** A kernel resumes in bounded quanta; every other method runs whole. */
  def execution[U <: Unit2D]: MethodExecution[Unit, U, Similarity] = method match
    case kernel: MapSimilarityMethod.KernelMethod =>
      MethodExecution.Bounded(_ => kernel.kernel[U])
    case other: MapSimilarityMethod.MetricMethod =>
      MethodExecution.Synchronous(_ => other.similarity[U])
    case other: MapSimilarityMethod.SymmetricMethod =>
      MethodExecution.Synchronous(_ => other.similarity[U])

  /** The descriptor under this method's built-in identity. */
  def descriptor: MethodDescriptor[Unit, Similarity, SignedDifference] = descriptorAs(id)

  /** The descriptor under another identity, for a variant registered by a caller. */
  def descriptorAs(
      identity: DefinitionId
  ): MethodDescriptor[Unit, Similarity, SignedDifference] =
    val capability = method.interface match
      case MapMethodInterface.Kernel => ExecutionCapability.BoundedComparison
      case MapMethodInterface.MetricDerived | MapMethodInterface.Symmetric =>
        ExecutionCapability.SynchronousWholeOperation
    MethodDescriptor.of(
      identity,
      ParameterSet.empty,
      _ => method.info,
      _ => Right(Vector(component)),
      properties,
      capability
    )

  /** The study method: no hidden parameters; geometry, weighting and
    * estimation live in the study plan.
    */
  def study[U <: Unit2D]: StudyMethod[Unit, U, Similarity, SignedDifference] = studyAs[U](id)

  /** The same method under another identity, for a variant registered by a caller. */
  def studyAs[U <: Unit2D](
      identity: DefinitionId
  ): StudyMethod[Unit, U, Similarity, SignedDifference] =
    new StudyMethod[Unit, U, Similarity, SignedDifference](
      identity,
      name,
      _ => Vector.empty,
      execution[U],
      Some(descriptorAs(identity))
    )

  override def toString: String = s"${id.name}@${id.version}"

/** The comparison-method registry: every registered map method, with its
  * identity and descriptor, and the one study route per method. eyesim's
  * `template_similarity(method = X)` is `StudyPlan.similarity` with the
  * entry for X; eyesim's names map to entries through
  * `eyes4s.compare.eyesim.EyesimCompat.fromReference` and [[of]].
  */
object ComparisonMethods:
  val pearson: ComparisonMethod = new ComparisonMethod(
    MapSimilarityMethod.Pearson,
    ComparisonMethodDefinitions.pearson,
    "Pearson correlation"
  )
  val spearman: ComparisonMethod = new ComparisonMethod(
    MapSimilarityMethod.Spearman,
    ComparisonMethodDefinitions.spearman,
    "Spearman rank correlation"
  )
  val fisherZ: ComparisonMethod = new ComparisonMethod(
    MapSimilarityMethod.FisherZ,
    ComparisonMethodDefinitions.fisherZ,
    "Fisher z of the Pearson correlation"
  )
  val cosine: ComparisonMethod =
    new ComparisonMethod(MapSimilarityMethod.Cosine, DefinitionId.cosine, "Cosine similarity")
  val l1Similarity: ComparisonMethod = new ComparisonMethod(
    MapSimilarityMethod.L1Similarity,
    ComparisonMethodDefinitions.l1Similarity,
    "One minus total variation"
  )
  val extendedJaccard: ComparisonMethod = new ComparisonMethod(
    MapSimilarityMethod.ExtendedJaccard,
    ComparisonMethodDefinitions.extendedJaccard,
    "Extended Jaccard similarity"
  )
  val distanceCorrelation: ComparisonMethod = new ComparisonMethod(
    MapSimilarityMethod.DistanceCorrelation,
    ComparisonMethodDefinitions.distanceCorrelation,
    "Distance correlation"
  )

  /** Every entry, in the order of [[MapSimilarityMethod.values]]. */
  val all: Vector[ComparisonMethod] =
    Vector(
      pearson,
      spearman,
      fisherZ,
      cosine,
      l1Similarity,
      extendedJaccard,
      distanceCorrelation
    )

  /** The entry of a registered method. A compatibility-only method, which has
    * no built-in identity, is [[MapComparisonError.UnsupportedMethod]].
    */
  def of(method: MapSimilarityMethod): Either[MapComparisonError, ComparisonMethod] =
    all.find(_.method == method).toRight(MapComparisonError.UnsupportedMethod(method.token))

  /** The entry registered under `id`, if any. */
  def resolve(id: DefinitionId): Option[ComparisonMethod] = all.find(_.id == id)
