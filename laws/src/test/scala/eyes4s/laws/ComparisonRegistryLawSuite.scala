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
import eyes4s.compare.eyesim.EyesimCompat
import eyes4s.design.SignedDifference
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Norm
import eyes4s.plan.*
import org.scalacheck.Test
import org.typelevel.discipline.Laws

/** Every registered comparison method satisfies the laws of the interface it
  * declares, and every registry entry's descriptor and study method agree
  * with its measure. The compatibility-only legacy Fisher z carries the
  * symmetric laws too.
  *
  * {{{
  * | rule set    | killed mutant                                              |
  * |-------------|------------------------------------------------------------|
  * | kernel      | one minus cosine declared as a kernel (indefinite Gram)    |
  * | metric      | squared total variation as the L1 metric (triangle fails)  |
  * | derivedFrom | one minus twice total variation as the L1 similarity       |
  * | withinScale | Pearson declared on the cosine scale [0, 1]                |
  * | registered  | foreign identity, widened range, dropped property, bounded |
  * |             | execution claimed for a whole-operation method             |
  * }}}
  */
class ComparisonRegistryLawSuite extends munit.DisciplineSuite:
  private val grid =
    Grid.over(Frame.unitSquare("baseline-map-laws").toOption.get, 5, 3).toOption.get
  private val massGen                      = Generators.genMass(grid)
  private def point(cell: Int): Mass[Norm] =
    val values = IArray.tabulate(grid.size)(i => if i == cell then 1.0 else 0.0)
    Surface.mass(grid, values, Provenance.raw(ContentHash.of(values))).toOption.get
  private val distinct = (point(0), point(grid.size - 1))

  ComparisonMethods.all.foreach { entry =>
    ComparisonMethodLaws
      .entry(entry, massGen, distinct)
      .foreach(rules => checkAll(s"${entry.id.name}@${entry.id.version}", rules))
  }
  EyesimCompat.methods.foreach { method =>
    ComparisonMethodLaws
      .interface(method, massGen, distinct)
      .foreach(rules => checkAll(s"eyesim-compat.${method.token}", rules))
  }

  test("the registry has exactly one entry per registered method, each with its own identity") {
    assertEquals(ComparisonMethods.all.map(_.method), MapSimilarityMethod.values)
    val ids = ComparisonMethods.all.map(_.id)
    assertEquals(ids.distinct.size, ids.size)
    assertEquals(ComparisonMethods.cosine.id, DefinitionId.cosine)
    ComparisonMethods.all.foreach { entry =>
      assertEquals(ComparisonMethods.resolve(entry.id).map(_.id), Some(entry.id))
      assertEquals(entry.toString, s"${entry.id.name}@${entry.id.version}")
      assertEquals(ComparisonMethods.of(entry.method).map(_.id), Right(entry.id))
      assert(entry.study[Norm].descriptor.isDefined, entry.id)
    }
    assertEquals(
      ComparisonMethods.of(EyesimCompat.FisherZLegacy).map(_.id),
      Left(MapComparisonError.UnsupportedMethod("FisherZLegacy"))
    )
  }

  // ---------------------------------------------------------------- mutants
  private val parameters =
    Test.Parameters.default.withMinSuccessfulTests(100).withInitialSeed(0x7265676973747279L)

  /** Whether some property of `rules` fails on the mutant it was given. */
  private def killed(rules: Vector[Laws#RuleSet]): Boolean =
    rules.exists(
      _.all.properties.exists((_, prop) =>
        Test.check(parameters, prop).status match
          case Test.Failed(_, _) | Test.PropException(_, _, _) => true
          case _                                               => false
      )
    )

  private def holds(rules: Vector[Laws#RuleSet]): Boolean =
    !killed(rules)

  test("the interface laws hold for every method and reject a kernel that is not one") {
    val cosine         = Distribution.cosine[Norm]
    val oneMinusCosine = new Kernel[Mass[Norm]]:
      val info                                  = cosine.info
      def compare(a: Mass[Norm], b: Mass[Norm]) =
        cosine
          .compare(a, b)
          .flatMap(s =>
            Similarity.of(1 - s.value).left.map(e => CompareError.InvalidScore("mutant", e))
          )
    assert(holds(Vector(MeasureLaws.kernel(cosine, massGen))))
    assert(killed(Vector(MeasureLaws.kernel(oneMinusCosine, massGen))))
  }

  test("the metric-derived laws reject a non-metric and a misdeclared transform") {
    val tv      = Distribution.totalVariation[Norm]
    val squared = new Metric[Mass[Norm]]:
      val info                                  = tv.info
      def compare(a: Mass[Norm], b: Mass[Norm]) =
        tv.compare(a, b)
          .flatMap(d =>
            MeasureDistance
              .of(d.value * d.value)
              .left
              .map(e => CompareError.InvalidScore("mutant", e))
          )
    assert(holds(Vector(MeasureLaws.metric(tv, massGen, distinct))))
    assert(killed(Vector(MeasureLaws.metric(squared, massGen, distinct))))
    val l1         = MapSimilarityMethod.L1Similarity
    val twiceAsFar = new SymmetricCompare[Mass[Norm], Similarity]:
      val info                                  = l1.info
      def compare(a: Mass[Norm], b: Mass[Norm]) =
        tv.compare(a, b)
          .flatMap(d =>
            Similarity.of(1 - 2 * d.value).left.map(e => CompareError.InvalidScore("mutant", e))
          )
    assert(
      holds(Vector(ComparisonMethodLaws.derivedFrom(tv, 1.0, l1.similarity[Norm], massGen)))
    )
    assert(killed(Vector(ComparisonMethodLaws.derivedFrom(tv, 1.0, twiceAsFar, massGen))))
  }

  test("the scale law rejects a measure outside the range it declares") {
    val pearson     = Distribution.pearson[Norm]
    val misdeclared = new SymmetricCompare[Mass[Norm], Similarity]:
      val info                                  = Distribution.cosine[Norm].info
      def compare(a: Mass[Norm], b: Mass[Norm]) = pearson.compare(a, b)
    assert(
      holds(Vector(MeasureLaws.withinScale[Mass[Norm], Similarity](pearson, massGen, _.value)))
    )
    assert(
      killed(
        Vector(MeasureLaws.withinScale[Mass[Norm], Similarity](misdeclared, massGen, _.value))
      )
    )
  }

  test("the registration law rejects a descriptor that disagrees with its measure") {
    val entry = ComparisonMethods.pearson
    val study = entry.study[Norm]
    def descriptor(
        id: DefinitionId = entry.id,
        range: MeasureScale = entry.method.info.scale,
        properties: Set[ComparisonProperty] = entry.properties,
        execution: ExecutionCapability = ExecutionCapability.SynchronousWholeOperation
    ): MethodDescriptor[Unit, Similarity, SignedDifference] =
      MethodDescriptor.of(
        id,
        ParameterSet.empty,
        _ => entry.method.info,
        _ =>
          ScoreComponent
            .of[Similarity, SignedDifference](
              "value",
              "mutant",
              Quantity.Dimensionless,
              range,
              ScoreDirection.HigherIsCloser
            )(_.value, _.value)
            .map(Vector(_)),
        properties,
        execution
      )
    def rules(d: MethodDescriptor[Unit, Similarity, SignedDifference]) =
      Vector(ComparisonMethodLaws.registered(entry.id, entry.method, d, study, massGen))
    assert(holds(rules(entry.descriptor)))
    assert(holds(rules(descriptor())))
    assert(killed(rules(descriptor(id = ComparisonMethods.spearman.id))))
    assert(killed(rules(descriptor(range = MeasureScale.Bounded(0, 1)))))
    assert(killed(rules(descriptor(properties = Set(ComparisonProperty.Symmetric)))))
    assert(killed(rules(descriptor(execution = ExecutionCapability.BoundedComparison))))
  }
