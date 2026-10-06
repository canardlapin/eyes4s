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

import eyes4s.design.AssociationMethod
import eyes4s.laws.Tolerance

class ConstrainedExportsSuite extends munit.FunSuite:
  import ConstrainedExportFixtures.*
  private val numerical                         = Tolerance(absolute = 1e-12, relative = 1e-12)
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private def numbers(table: ResultTable, column: String): Vector[Double] =
    table.rows.map(_(table.columns.indexWhere(_.name == column))).collect {
      case ResultCell.Number(n) => n
    }
  private def near(actual: Vector[Double], expected: Vector[Double]): Unit =
    assertEquals(actual.size, expected.size)
    actual.zip(expected).foreach((a, e) => assert(numerical.approxEquals(a, e), s"$a != $e"))

  test(
    "NNLS coefficients are scale factors; non-square cells retain indices and signed residuals"
  ) {
    val exported = get(ResultExports.nnls(nnls))
    assertEquals(
      exported.map(_.family),
      Vector(
        ResultFamily.NnlsCoefficients,
        ResultFamily.NnlsDiagnostics,
        ResultFamily.NnlsCells
      )
    )
    near(numbers(exported(0), "value"), Vector(7.0 / 24, 13.0 / 24))
    assertEquals(exported(0).columns.last.unit, "response mass / predictor mass")
    val cells = exported(2)
    assertEquals(
      cells.rows.map(_.take(3)),
      Vector.tabulate(6)(i =>
        Vector(
          ResultCell.Integer(i.toLong),
          ResultCell.Integer((i % 3).toLong),
          ResultCell.Integer((i / 3).toLong)
        )
      )
    )
    near(numbers(cells, "fitted"), Vector(7.0 / 24, 13.0 / 24, 0, 0, 0, 0))
    near(numbers(cells, "residual"), Vector(0, 0, 1.0 / 24, 1.0 / 24, 1.0 / 24, 1.0 / 24))
    assert(exported.forall(_.context.compact.contains("uncentered")))
    assert(exported.forall(_.context.compact.contains(nnls.fitted.grid.id.name)))
  }

  test(
    "mixture exports a uniform background as its own role, optional weights and diagnostics"
  ) {
    val exported = get(ResultExports.mixture(mixture))
    val weights  = exported(0)
    near(numbers(weights, "value"), Vector(.25, .5, .25))
    assertEquals(
      weights.rows.map(_(1)),
      Vector(
        ResultCell.Text("predictor"),
        ResultCell.Text("predictor"),
        ResultCell.Text("uniform-background")
      )
    )
    assertEquals(weights.columns.last.unit, "unitless")
    near(numbers(exported(2), "fitted"), response.values.toVector)
    near(numbers(exported(2), "residual"), Vector.fill(6)(0.0))
    val evidence = exported(1).columns.map(_.name).zip(exported(1).rows.head).toMap
    assertEquals(evidence("rank"), ResultCell.Integer(mixture.diagnostics.rank.toLong))
    assertEquals(evidence("active"), ResultCell.Integer(mixture.diagnostics.active.toLong))
    assertEquals(
      evidence("iterations"),
      ResultCell.Integer(mixture.diagnostics.iterations.toLong)
    )
    assertEquals(
      evidence("dual_violation"),
      ResultCell.Number(mixture.diagnostics.dualViolation)
    )
    assertEquals(
      evidence("residual_sum_squares"),
      ResultCell.Number(mixture.diagnostics.residualSumSquares)
    )
    assertEquals(
      evidence("scaled_diagonal_ratio"),
      ResultCell.Number(mixture.diagnostics.scaledDiagonalRatio)
    )
    val without = get(ResultExports.mixture(noBackground))
    assertEquals(without(0).rows.size, 2)
    near(
      numbers(without(2), "residual"),
      Vector(-1.0 / 12, -1.0 / 12, 1.0 / 24, 1.0 / 24, 1.0 / 24, 1.0 / 24)
    )
    val constant = get(ResultExports.mixture(uniformMixture))(1)
    assertEquals(
      constant.rows.head(constant.columns.indexWhere(_.name == "r_squared")),
      ResultCell.Missing
    )
    assertEquals(
      constant.rows.head(constant.columns.indexWhere(_.name == "r_squared_status")),
      ResultCell.Text("zero reference response variation")
    )
  }

  test(
    "partial association retains method, covariates and undefined estimates, never beta labels"
  ) {
    Vector(pearson, spearman)
      .zip(Vector(AssociationMethod.Pearson, AssociationMethod.Spearman))
      .foreach { (value, method) =>
        val table = get(ResultExports.partialAssociation(value))
        assertEquals(
          table.rows.head.take(2),
          Vector(ResultCell.Text(method.toString), ResultCell.Integer(6))
        )
        near(numbers(table, "estimate"), Vector(1.0))
        assert(table.context.compact.contains("alternating"))
        assert(table.context.compact.contains("provenance"))
      }
    val missing = get(ResultExports.partialAssociation(undefined))
    assertEquals(missing.rows.head(2), ResultCell.Missing)
    assertEquals(missing.rows.head(3), ResultCell.Text("zero residual variation"))
    assertEquals(missing.csv.rows.head(missing.csv.header.indexOf("estimate__valid")), "false")
  }

  test("every new family has a typed CSV schema, exact row cardinality and portable digest") {
    assertEquals(
      tables.map(_.family).toSet,
      Set(
        ResultFamily.NnlsCoefficients,
        ResultFamily.NnlsDiagnostics,
        ResultFamily.NnlsCells,
        ResultFamily.MixtureWeights,
        ResultFamily.MixtureDiagnostics,
        ResultFamily.MixtureCells,
        ResultFamily.PartialAssociations
      )
    )
    tables.foreach { table =>
      val rows = get(Rfc4180.decode(table.csv.encode))
      assertEquals(rows.head, table.csv.header)
      assertEquals(rows.tail, table.csv.rows)
      assertEquals(rows.tail.size, table.rows.size)
      assert(table.columns.forall(c => c.unit.nonEmpty && c.meaning.nonEmpty))
      assertEquals(
        table.metadata.circe.hcursor.get[String]("table_sha256").toOption,
        Some(table.identity.hex)
      )
      assertEquals(
        table.columns.map(_.name),
        table.family match
          case ResultFamily.NnlsCoefficients | ResultFamily.MixtureWeights =>
            Vector("predictor", "role", "value")
          case ResultFamily.NnlsDiagnostics | ResultFamily.MixtureDiagnostics =>
            Vector(
              "rank",
              "cells",
              "active",
              "iterations",
              "dual_violation",
              "residual_sum_squares",
              "r_squared",
              "r_squared_status",
              "scaled_diagonal_ratio"
            )
          case ResultFamily.NnlsCells | ResultFamily.MixtureCells =>
            Vector("cell_index", "column_index", "row_index", "fitted", "residual")
          case ResultFamily.PartialAssociations =>
            Vector("method", "cells", "estimate", "estimate_status")
          case other => fail(s"unexpected $other")
      )
    }
  }
