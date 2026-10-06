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

package eyes4s.studio.core.reports

import eyes4s.codec.ReportSources
import eyes4s.plan.{AttributeValue, Attributes}
import eyes4s.results.*
import eyes4s.studio.core.backend.{ReportAbsence, ReportRole, Response, RunId}
import eyes4s.studio.core.document.{Covariate as StudioCovariate, *}

/** Asymmetric observations in opposite categorical orders prevent accidental
  * alphabetical, first-observation, or declared-level subtraction direction.
  */
class ReportDirectionSuite extends munit.FunSuite:
  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)
  private val ArithmeticTolerance          = 1e-12
  private val name                         = ok(CovariateName.of("response"))
  private val attribute                    = ok(StudioCovariate.of("response"))
  private val run                          = RunId(7)
  private val forward                      = ok(ReportingContrast.of("Remembered", "Forgotten"))

  private def spec(
      contrast: Option[ReportingContrast] = Some(forward),
      minimum: Option[Int] = None,
      weighting: ReportingWeight = ReportingWeight.ParticipantMeans,
      filters: Vector[ReportingFilter] = Vector.empty
  ) = ok(
    ReportingSpec.of(
      ok(ReportingId.of("asymmetric")),
      "Asymmetric",
      Some(attribute),
      filters,
      minimum.map(m => ok(MinimumPerGroup.of(m))),
      weighting,
      contrast
    )
  )

  // P1 has R=0.8, F=0.2; P2 R=0.4 (two queries), F=0.1; P3 R=0.9 only.
  // First observation is Forgotten, intentionally opposite the requested direction.
  private val observations = Vector(
    ("P1", "Forgotten", 0.2, 0.6),
    ("P1", "Remembered", 0.8, 0.1),
    ("P2", "Remembered", 0.3, 0.1),
    ("P2", "Remembered", 0.5, 0.8),
    ("P2", "Forgotten", 0.1, 0.1),
    ("P3", "Remembered", 0.9, 0.1)
  )

  private def source(levels: Vector[String], reversed: Boolean) =
    val schema = ok(
      CovariateSchema.of(
        Vector(eyes4s.results.Covariate(name, CovariateType.Categorical(ok(Levels.of(levels)))))
      )
    )
    val rows = observations.zipWithIndex.map { case ((p, level, value, share), i) =>
      val scored = RoleOutcome.Scored(Vector(value))
      ok(
        Query.of(
          i,
          p,
          s"item-$i",
          "Retrieval",
          1,
          Vector(name -> Value.Present(CovariateValue.Level(level))),
          Vector.empty,
          Vector(WindowMeasure.OutsideWindowShare -> Value.Present(share)),
          scored,
          scored,
          scored
        )
      )
    }
    val table = ok(
      QueryTable.of(
        0,
        Vector(ReportEvaluation.Component),
        schema,
        if reversed then rows.reverse else rows
      )
    )
    val attributes = observations.zipWithIndex.map { case ((_, level, _, _), i) =>
      i -> ok(Attributes.of(Vector("response" -> AttributeValue.Text(level))))
    }
    val covariates = ok(CovariateTable.of(schema, attributes))
    ok(ReportSources.tables(io.circe.Encoder[Int])(Vector(table), Some(covariates)))

  private def evaluate(s: ReportingSpec, reversed: Boolean = false) =
    val levels = if reversed then Vector("Remembered", "Forgotten")
    else Vector("Forgotten", "Remembered")
    ok(
      ReportEvaluation.evaluate(run, s, 0, Map("response" -> levels), source(levels, reversed))
    )

  test(
    "explicit direction survives declaration and observation permutations; reversing operands negates it"
  ) {
    val original  = evaluate(spec())
    val reordered = evaluate(spec(), reversed = true)
    val reversed  =
      evaluate(spec(contrast = Some(ok(ReportingContrast.of("Forgotten", "Remembered")))))
    for role <- ReportRole.values do
      val a = original.contrast(role).get
      val b = reordered.contrast(role).get
      val c = reversed.contrast(role).get
      assertEquals((a.minuend, a.subtrahend), (Response.Remembered, Response.Forgotten))
      assertEqualsDouble(a.estimate.get, 0.45, ArithmeticTolerance)
      assertEquals(a.pairedN, 2)
      assertEquals(a.unpaired.map(_.participant), Vector("P3"))
      assertEqualsDouble(b.estimate.get, a.estimate.get, ArithmeticTolerance)
      assertEqualsDouble(c.estimate.get, -a.estimate.get, ArithmeticTolerance)
      assertEquals(c.pairedN, a.pairedN)
      assertNotEquals(c.ref, a.ref)
  }

  test("legacy grouping serves cells without inventing a level contrast") {
    val view = evaluate(spec(contrast = None))
    assertEquals(view.cells.size, 6)
    assertEquals(view.contrasts, Vector.empty)
  }

  test("minimum and filter decisions, and both weighting policies, remain native") {
    val plain   = evaluate(spec())
    val pooled  = evaluate(spec(weighting = ReportingWeight.PooledQueries))
    val minimum = evaluate(spec(minimum = Some(2)))
    val keep = ReportingFilter.Keep(attribute, ok(ValueSet.of(attribute, Vector("Remembered"))))
    val filtered = evaluate(spec(filters = Vector(keep)))
    val window   = ReportingFilter.OutsideWindowAtMost(ok(Share.of(0.25)))
    val windowed = evaluate(spec(filters = Vector(window)))
    for role <- ReportRole.values do
      assertEqualsDouble(
        plain.cell(Some(Response.Remembered), role).get.estimate.get,
        0.7,
        ArithmeticTolerance
      )
      assertEqualsDouble(
        pooled.cell(Some(Response.Remembered), role).get.estimate.get,
        0.625,
        ArithmeticTolerance
      )
      assertEquals(minimum.cell(Some(Response.Remembered), role).get.participants, 1)
      assertEquals(minimum.cell(Some(Response.Forgotten), role).get.estimate, None)
      assertEquals(
        filtered.cell(Some(Response.Forgotten), role).get.absence,
        Some(ReportAbsence.EmptyGroup)
      )
      assertEquals(filtered.contrast(role).get.pairedN, 0)
      assertEquals(windowed.cell(Some(Response.Forgotten), role).get.queries, 1)
      assertEqualsDouble(windowed.contrast(role).get.estimate.get, 0.2, ArithmeticTolerance)
    assertEquals(minimum.dropped.size, 4)
  }

  test("a minimum with pooled weighting is explicitly refused instead of ignored") {
    val reporting = spec(minimum = Some(3), weighting = ReportingWeight.PooledQueries)
    val result    =
      ReportEvaluation.spec(reporting, 0, Map("response" -> Vector("Remembered", "Forgotten")))
    assert(result.isLeft)
    assert(result.left.toOption.get.message.contains("3"))
    assert(result.left.toOption.get.message.contains("Pooled"))
  }

  test("undeclared contrast levels are refused by the native checked specification") {
    val wrong  = spec(contrast = Some(ok(ReportingContrast.of("Misspelled", "Forgotten"))))
    val result =
      ReportEvaluation.spec(wrong, 0, Map("response" -> Vector("Remembered", "Forgotten")))
    assert(result.isLeft)
    assert(result.left.toOption.get.message.contains("Misspelled"))
  }
