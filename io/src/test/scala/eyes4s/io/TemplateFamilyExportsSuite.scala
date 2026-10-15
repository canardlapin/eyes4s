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

package eyes4s.io

import eyes4s.codec.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

class TemplateFamilyExportsSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val keys  = VersionedCodec.string(get(DefinitionId.of("example.template-key", 1)))
  private val frame = get(Frame.screen("export-family", 3, 1))
  private val grid  = get(Grid.over(frame, 3, 1))
  private def mass(a: Double, b: Double, c: Double): Mass[Px] =
    val values = IArray(a, b, c)
    get(
      Surface
        .intensity(grid, values, Provenance.raw(ContentHash.of(values)))
        .flatMap(_.normalised)
    )
  private def numbers(table: ResultTable, name: String): Vector[Double] =
    val index = table.columns.indexWhere(_.name == name)
    table.rows.map(_(index)).collect { case ResultCell.Number(value) => value }
  private def checkContext(tables: Vector[ResultTable], saved: io.circe.Json): Unit =
    tables.foreach { table =>
      assertEquals(table.context.circe.hcursor.get[io.circe.Json]("archive"), Right(saved))
      assertEquals(get(Rfc4180.decode(table.csv.encode)).tail.size, table.rows.size)
    }

  test(
    "completed imported fixed fits export their receipt predictions without a native refit"
  ) {
    val basis  = get(TemplateBasis.of("export-template", Vector("x", "z"), "score"))
    val design = TemplateDesign.importedLm(basis)
    val rows   = Vector(
      get(TemplateObservation.of("x", "train", Vector(1.0, 0.0), 2.0, Some("x"))),
      get(TemplateObservation.of("z", "train", Vector(0.0, 1.0), 3.0, Some("z"))),
      get(TemplateObservation.of("excluded", "train", Vector(4.0, 5.0), 99.0, Some("held"))),
      get(TemplateObservation.of("held", "held", Vector(2.0, 1.0), 20.0, Some("held")))
    )
    val split   = get(TemplateSplit.of(design, rows, Set("held")))
    val receipt = get(
      TemplateImportedFit.of(
        split.training,
        split.training.hash.render,
        basis.columns,
        Vector(5.0, 7.0),
        2,
        2,
        "external export fixture"
      )
    )
    val plan    = get(TemplateFitPlan.imported(split, receipt))
    val result  = get(plan.run)
    val archive = get(TemplateFitArchive.completed(plan, result))
    val codec   = TemplateFitCodec.fixed(keys)
    val tables  = get(ResultExports.fixedTemplate(archive, codec))
    assert(archive.result eq result)
    assertEquals(tables.map(_.rows.size), Vector(2, 1, 1))
    assertEquals(numbers(tables(0), "coefficient"), Vector(5.0, 7.0))
    assertEquals(numbers(tables(1), "predicted"), Vector(17.0))
    assertEquals(numbers(tables(1), "residual"), Vector(3.0))
    checkContext(tables, get(codec.results.encode(archive)))
  }

  test(
    "completed learned templates export every prediction and exclusion with the stamped source"
  ) {
    val design = get(TemplateDesign.meanMap[Px]("participant", "score"))
    val rows   = Vector(
      get(TemplateObservation.of("a", "train", mass(1, 0, 0), 2.0, Some("a"))),
      get(TemplateObservation.of("b", "train", mass(0, 1, 0), 3.0, Some("b"))),
      get(TemplateObservation.of("excluded", "train", mass(0, 0, 1), 99.0, Some("held"))),
      get(TemplateObservation.of("held", "held", mass(1, 1, 0), 4.0, Some("held")))
    )
    val plan    = get(TemplateFitPlan.of(get(TemplateSplit.of(design, rows, Set("held")))))
    val codec   = TemplateFitCodec.meanMap[String, Px](keys)
    val archive = get(TemplateFitArchive.completed(plan, get(plan.run)))
    val tables  = get(ResultExports.learnedTemplate(archive, codec))
    assertEquals(tables.map(_.rows.size), Vector(1, 1, 1))
    assertEquals(
      numbers(tables(1), "predicted"),
      Vector(archive.validation.evaluation.rows.head.result.toOption.get._1)
    )
    checkContext(tables, get(codec.results.encode(archive)))
  }

  test(
    "all decomposition variants preserve existing table cells and add the same stamped context"
  ) {
    val predictors = get(
      PredictorSet.of(
        Vector(
          get(PredictorId.of("a")) -> mass(3, 2, 0),
          get(PredictorId.of("b")) -> mass(0, 2, 3)
        )
      )
    )
    val specs = Vector(
      DecompositionSpec.Ols(Intercept.Exclude, RelativeRankTolerance.default),
      DecompositionSpec
        .NonNegative(RelativeRankTolerance.default, RelativeDualTolerance.default),
      DecompositionSpec.Mixture(
        Intercept.Exclude,
        RelativeRankTolerance.default,
        RelativeDualTolerance.default
      )
    )
    val codec = new DecompositionCodec[Px]
    specs.foreach { spec =>
      val plan    = get(DecompositionPlan.of(mass(1, 0, 0), predictors, spec))
      val result  = get(plan.run)
      val archive = get(DecompositionArchive.completed(plan, result))
      val old     = result.fit match
        case DecompositionFit.Ols(value)         => get(ResultExports.ols(value))
        case DecompositionFit.NonNegative(value) => get(ResultExports.nnls(value))
        case DecompositionFit.Mixture(value)     => get(ResultExports.mixture(value))
      val tables = get(ResultExports.decomposition(archive, codec))
      assert(archive.result eq result)
      assertEquals(
        tables.map(t => (t.family, t.columns, t.rows)),
        old.map(t => (t.family, t.columns, t.rows))
      )
      checkContext(tables, get(codec.results.encode(archive)))
    }
  }

  test("the measured R lm receipt survives the complete family archive without refitting") {
    val split    = get(eyes4s.examples.TemplateFitGuide.historicalInput())
    val imported = get(TemplateFitCsv.importFit(split.training, TemplateFitReference.receipt))
    val receipt  = get(
      TemplateImportedFit.of(
        split.training,
        split.training.hash.render,
        split.design.featureNames,
        imported.coefficients,
        imported.coefficients.size,
        split.training.rows.size,
        imported.backend
      )
    )
    val plan     = get(TemplateFitPlan.imported(split, receipt))
    val codec    = TemplateFitCodec.fixed(keys)
    val archive  = get(TemplateFitArchive.run(plan))
    val restored = get(codec.results.decode(get(codec.results.encode(archive))))
    assertEquals(
      restored.validation.fitted.coefficients,
      Vector(1.0000000000000004, 2.0000000000000004)
    )
    assertEquals(restored.validation.fitted.backend, "R-4.5.1/stats-lm.fit")
    assertEquals(
      restored.validation.evaluation.rows,
      get(imported.evaluate(split.heldOut)).rows
    )
    val tables = get(ResultExports.fixedTemplate(restored, codec))
    assertEquals(numbers(tables.head, "coefficient"), imported.coefficients)
    checkContext(tables, get(codec.results.encode(restored)))
  }
