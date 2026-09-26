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

package eyes4s.laws

import eyes4s.compare.*
import eyes4s.design.SignedDifference
import eyes4s.kernel.*
import eyes4s.plan.*

import org.scalacheck.Prop
import org.scalacheck.Prop.forAll
import org.scalacheck.Gen
import org.typelevel.discipline.Laws

/** Laws for the comparison-method registry: each method satisfies the laws of
  * the interface it declares, and each registry entry's descriptor and study
  * method agree with the measure behind it.
  *
  * {{{
  * | declared interface | rule sets                                                |
  * |--------------------|----------------------------------------------------------|
  * | Kernel             | kernel (symmetry, self-similarity, PSD Gram matrices)    |
  * | MetricDerived      | metric on the distance, similarity = upper - distance    |
  * | Symmetric          | symmetry                                                 |
  * | every method       | scores within the declared scale, self-description       |
  * }}}
  */
trait ComparisonMethodLaws extends Laws:

  /** The laws of the interface `method` declares, on maps drawn from `gen`.
    * `distinct` is a pair of maps a metric must separate.
    */
  def interface[U <: Unit2D](
      method: MapSimilarityMethod,
      gen: Gen[Mass[U]],
      distinct: (Mass[U], Mass[U]),
      tol: Tolerance = Tolerance.exactish
  ): Vector[Laws#RuleSet] =
    val common = Vector[Laws#RuleSet](
      MeasureLaws.withinScale[Mass[U], Similarity](method.similarity[U], gen, _.value, tol),
      MeasureLaws.described(method.similarity[U])
    )
    val declared: Vector[Laws#RuleSet] = method match
      case kernel: MapSimilarityMethod.KernelMethod =>
        Vector(MeasureLaws.kernel(kernel.kernel[U], gen, tol = tol))
      case derived: MapSimilarityMethod.MetricMethod =>
        Vector(
          MeasureLaws.metric(derived.metric[U], gen, distinct, tol = tol),
          derivedFrom(derived.metric[U], derived.upper, derived.similarity[U], gen, tol)
        )
      case symmetric: MapSimilarityMethod.SymmetricMethod =>
        Vector(
          MeasureLaws.symmetry[Mass[U], Similarity](
            symmetric.similarity[U],
            gen,
            (a, b) => tol.approxEquals(a.value, b.value)
          )
        )
    declared ++ common

  /** A metric-derived similarity is exactly `upper - d` wherever `d` is defined,
    * and refused wherever `d` is refused.
    */
  def derivedFrom[A](
      metric: Metric[A],
      upper: Double,
      similarity: SymmetricCompare[A, Similarity],
      gen: Gen[A],
      tol: Tolerance = Tolerance.exactish
  ): RuleSet =
    new SimpleRuleSet(
      s"derived.${similarity.info.name}",
      "the similarity is the declared transform of the metric" -> forAll(gen, gen) { (a, b) =>
        (metric.compare(a, b), similarity.compare(a, b)) match
          case (Right(d), Right(s)) =>
            Prop(tol.approxEquals(s.value, upper - d.value)) :| s"$s vs $upper - $d"
          case (Left(_), Left(_)) => Prop(true)
          case _                  => Prop(false) :| "defined for only one of the two"
      }
    )

  /** A registry entry's parts agree with its measure: the descriptor carries
    * the entry's identity, the measure's info and scale, properties that
    * follow from the scale, and the execution capability the study method
    * actually has; the study method carries the same identity and descriptor.
    * `U` is the unit the study method is built for.
    */
  def registered[U <: Unit2D](
      id: DefinitionId,
      method: MapSimilarityMethod,
      descriptor: MethodDescriptor[Unit, Similarity, SignedDifference],
      study: StudyMethod[Unit, U, Similarity, SignedDifference],
      gen: Gen[Mass[U]]
  ): RuleSet =
    val scale      = method.info.scale
    val components = descriptor.components(())
    val bounded    = scale match
      case MeasureScale.Bounded(_, _) | MeasureScale.Correlation | MeasureScale.Probability =>
        true
      case MeasureScale.FisherZ | MeasureScale.UnboundedSimilarity |
          MeasureScale.DistanceLike =>
        false
    val nonNegative = scale match
      case MeasureScale.Bounded(lo, _)                          => lo >= 0
      case MeasureScale.Probability | MeasureScale.DistanceLike => true
      case MeasureScale.Correlation | MeasureScale.FisherZ | MeasureScale.UnboundedSimilarity =>
        false
    val expectedExecution = method.interface match
      case MapMethodInterface.Kernel => ExecutionCapability.BoundedComparison
      case MapMethodInterface.MetricDerived | MapMethodInterface.Symmetric =>
        ExecutionCapability.SynchronousWholeOperation
    new SimpleRuleSet(
      s"registered.${id.name}@${id.version}",
      "the descriptor names the entry's identity"  -> Prop(descriptor.id == id),
      "the descriptor presents the measure's info" -> Prop(descriptor.info(()) == method.info),
      "one component, on the measure's scale"      -> (Prop(
        components.exists(cs =>
          cs.map(_.id) == Vector("value") && cs
            .forall(c => c.range == scale && c.direction == ScoreDirection.HigherIsCloser)
        )
      ) :| s"$components"),
      "declared properties follow from the scale" -> (Prop(
        descriptor.properties ==
          (Set(ComparisonProperty.Symmetric) ++
            Option.when(bounded)(ComparisonProperty.Bounded) ++
            Option.when(nonNegative)(ComparisonProperty.NonNegative))
      ) :| s"${descriptor.properties}"),
      "the declared execution is the interface's" -> (Prop(
        descriptor.execution == expectedExecution && study.capability == expectedExecution
      ) :| s"${descriptor.execution} / ${study.capability}"),
      "the study method carries identity and descriptor" -> Prop(
        study.id == id && study.descriptor.exists(d =>
          d.id == id && d.info(()) == descriptor.info(()) &&
            d.properties == descriptor.properties && d.execution == descriptor.execution
        )
      ),
      "the study method scores as the measure does" -> forAll(gen, gen) { (a, b) =>
        Prop(
          study.comparison(()).compare(a, b).map(_.value) ==
            method.similarity[U].compare(a, b).map(_.value)
        )
      },
      "the component reads the score and the difference" -> forAll(gen, gen) { (a, b) =>
        method.similarity[U].compare(a, b) match
          case Left(_)      => Prop(true)
          case Right(score) =>
            Prop(
              components.exists(_.forall(c => c.score(score) == score.value)) &&
                SignedDifference
                  .between(score.value, 0.0)
                  .forall(d => components.exists(_.forall(c => c.difference(d) == d.value)))
            )
      }
    )

  /** The laws of a registry entry: the interface laws of its method and the
    * agreement of its descriptor and study method.
    */
  def entry(
      method: ComparisonMethod,
      gen: Gen[Mass[Unit2D.Norm]],
      distinct: (Mass[Unit2D.Norm], Mass[Unit2D.Norm]),
      tol: Tolerance = Tolerance.exactish
  ): Vector[Laws#RuleSet] =
    interface(method.method, gen, distinct, tol) :+
      registered(method.id, method.method, method.descriptor, method.study[Unit2D.Norm], gen)

end ComparisonMethodLaws

object ComparisonMethodLaws extends ComparisonMethodLaws
