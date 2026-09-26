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

import eyes4s.examples.BaselineExportGuide
import io.circe.Json
import scala.compiletime.testing.typeCheckErrors

class BaselineExportsSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A       = e.fold(x => fail(x.toString), identity)
  private lazy val tables                         = get(BaselineExportGuide.tables).toMap
  private val NumericTolerance                    = 1e-12
  private def cells(t: ResultTable, name: String) =
    t.rows.map(_(t.columns.indexWhere(_.name == name)))
  private def numbers(t: ResultTable, name: String) = cells(t, name).collect {
    case ResultCell.Number(v) => v
  }

  test(
    "compiled public workflows cover every frozen result family and emit portable typed CSV"
  ) {
    assertEquals(tables.values.map(_.family).toSet, ResultFamily.values.toSet)
    tables.foreach { (name, t) =>
      val parsed = get(Rfc4180.decode(t.csv.encode))
      assertEquals(parsed.head, t.csv.header); assertEquals(parsed.tail, t.csv.rows)
      assertEquals(parsed.tail.size, t.rows.size, clue = name)
      assert(t.columns.forall(c => c.unit.nonEmpty && c.meaning.nonEmpty))
      assertEquals(
        t.metadata.hcursor.get[String]("table_sha256").toOption,
        Some(t.identity.hex)
      )
      assertEquals(
        t.metadata.hcursor.get[String]("row_count").toOption,
        Some(t.rows.size.toString)
      )
      assert(t.csv.rows.forall(_.head == t.identity.hex))
      val frozen = get(io.circe.parser.parse(BaselineExportSchemaFixture.versionOne)).hcursor
        .downField(name)
      for field <- Vector("schema", "family", "columns") do
        assertEquals(t.metadata.hcursor.downField(field).focus, frozen.downField(field).focus)

    }
  }
  test(
    "scalar and structured pair failures retain endpoints, component order and denominator evidence"
  ) {
    val p = tables("pairs"); assertEquals(p.rows.size, 9)
    assertEquals(cells(p, "status").count(_ == ResultCell.Text("failure")), 3)
    assertEquals(numbers(p, "value"), Vector(2.0, 3.0, 4.0, 3.0, 4.0, 5.0))
    assert(cells(p, "left_key_json").forall(_.text.contains("λ")))
    val m = tables("structured-pairs"); assertEquals(m.rows.size, 45)
    assertEquals(
      cells(m, "component").take(5).map(_.text),
      Vector("shape", "direction", "length", "position", "duration")
    )
    assertEquals(numbers(m, "value").take(5), Vector(.1, .2, .3, .4, .5))
    assertEquals(cells(m, "status").count(_ == ResultCell.Text("failure")), 15)
    assert(p.context.noSpaces.contains("eligiblePairCount"));
    assert(p.context.noSpaces.contains("evaluation"))
    assertEquals(tables("empty-pairs").rows.size, 0)
    assert(tables("empty-pairs").context.noSpaces.contains("example.sum"))
  }
  test(
    "reductions and contrasts retain failure counts and signed differences without fabricated denominators"
  ) {
    val r = tables("reductions")
    assertEquals(numbers(r, "value"), Vector(3.0, 4.0))
    assertEquals(cells(r, "selected"), Vector.fill(3)(ResultCell.Integer(3)))
    assertEquals(
      cells(r, "successful"),
      Vector(ResultCell.Integer(3), ResultCell.Integer(3), ResultCell.Integer(0))
    )
    assertEquals(
      cells(r, "contributing"),
      Vector(ResultCell.Integer(3), ResultCell.Integer(3), ResultCell.Integer(0))
    )
    assertEquals(
      r.context.hcursor
        .downField("source")
        .downField("pairing")
        .get[String]("eligiblePairCount")
        .toOption,
      Some("9")
    )
    val c           = tables("contrasts"); assertEquals(c.rows.size, 9)
    val differences = c.rows.filter(_(0) == ResultCell.Text("difference"))
    assert(differences.forall(_.takeRight(3) == Vector.fill(3)(ResultCell.Missing)))
  }
  test(
    "temporal exports retain exact 64-bit queries, missing support, controls and final endpoint"
  ) {
    val p = tables("point-pointsamples")
    assertEquals(p.rows.size, 30)
    assert(cells(p, "time_us").contains(ResultCell.Integer(9007199254740993L)))
    assert(cells(p, "time_us").contains(ResultCell.Integer(Long.MaxValue)))
    assertEquals(cells(p, "status").count(_ == ResultCell.Text("failure")), 12)
    val b = tables("point-pointbins")
    assertEquals(b.rows.size, 18)
    assertEquals(cells(b, "includes_final_endpoint").count(_ == ResultCell.Flag(true)), 9)
    assertEquals(tables("point-pointcontrols").rows.size, 30)
    assertEquals(tables("point-pointsources").rows.size, 3)
    assert(p.context.noSpaces.contains("archive"))
  }
  test(
    "template and OLS adapters have independent analytic numerical targets and explicit exclusions"
  ) {
    assertEquals(numbers(tables("fixed-templatecoefficients"), "coefficient"), Vector(2.0))
    assertEquals(numbers(tables("fixed-templatepredictions"), "predicted"), Vector(6.0))
    assertEquals(numbers(tables("fixed-templatepredictions"), "residual"), Vector(1.0))
    assertEqualsDouble(
      numbers(tables("learned-templatepredictions"), "predicted").head,
      1.5,
      NumericTolerance
    )
    assertEquals(tables("learned-templateexclusions").rows.size, 1)
    val ols = numbers(tables("olscoefficients"), "value")
    assertEqualsDouble(ols(0), .25, NumericTolerance);
    assertEqualsDouble(ols(1), .75, NumericTolerance)
    assertEqualsDouble(
      numbers(tables("olsdiagnostics"), "residual_sum_squares").head,
      0,
      NumericTolerance
    )
    assertEquals(tables("olscells").rows.size, 4)
    assertEquals(tables("repetition-0").rows.size, 6);
    assertEquals(tables("repetition-1").rows.size, 12)
    assertEquals(tables("study").rows.size, 2)
    assertEquals(tables("temporalcontrasts").rows.size, 2);
    assertEquals(tables("temporalcoverage").rows.size, 4)
  }
  test(
    "canonical admission refuses wrong widths, nonfinite scores, wrong scalar types and invalid labels"
  ) {
    val c = Vector(ResultColumn("value", ResultColumnType.Float64, false, "unitless", "score"))
    def build(rows: Vector[Vector[ResultCell]]) =
      ResultTable.of(ResultFamily.PairScores, c, rows, Json.obj())
    assert(build(Vector(Vector.empty)).isLeft)
    for v <- Vector(
        ResultCell.Number(Double.NaN),
        ResultCell.Number(Double.PositiveInfinity),
        ResultCell.Missing,
        ResultCell.Integer(1)
      )
    do assert(build(Vector(Vector(v))).isLeft)
    assert(ResultTable.of(ResultFamily.PairScores, c ++ c, Vector.empty, Json.obj()).isLeft)
    val labels = Vector(
      ResultColumn("status", ResultColumnType.Utf8, false, "label", "outcome", Vector("ok"))
    )
    assert(
      ResultTable
        .of(
          ResultFamily.PairScores,
          labels,
          Vector(Vector(ResultCell.Text("unknown"))),
          Json.obj()
        )
        .isLeft
    )
    assert(
      typeCheckErrors(
        "new eyes4s.io.ResultTable(null,Vector.empty,Vector.empty,null,null)"
      ).nonEmpty
    )
  }
  test(
    "CSV validity preserves missing versus empty text, quoting and negative zero without sentinels"
  ) {
    val columns = Vector(
      ResultColumn("label", ResultColumnType.Utf8, true, "text", "label"),
      ResultColumn("score", ResultColumnType.Float64, true, "unitless", "score")
    )
    val rows = Vector(
      Vector(ResultCell.Text(""), ResultCell.Number(-0.0)),
      Vector(ResultCell.Missing, ResultCell.Missing),
      Vector(ResultCell.Text("a,\"λ\"\nline"), ResultCell.Number(1.25))
    )
    val t = get(ResultTable.of(ResultFamily.PairScores, columns, rows, Json.obj()))
    assertEquals(t.csv.rows(0).drop(1), Vector("", "true", "-0", "true"))
    assertEquals(t.csv.rows(1).drop(1), Vector("", "false", "", "false"))
    assertEquals(get(Rfc4180.decode(t.csv.encode)).tail, t.csv.rows)
    val a = get(
      ResultTable.of(
        ResultFamily.PairScores,
        columns,
        rows,
        Json.obj("x" -> Json.fromDoubleOrNull(1.0))
      )
    )
    val b = get(
      ResultTable.of(ResultFamily.PairScores, columns, rows, Json.obj("x" -> Json.fromInt(1)))
    )
    assertEquals(a.identity, b.identity)
    assert(a.identity != t.identity)
  }

  test(
    "overflowing held-out prediction stays a failed row with its key and observed response"
  ) {
    import eyes4s.codec.*
    import eyes4s.design.*
    import eyes4s.plan.DefinitionId
    val basis  = get(TemplateBasis.of("overflow feature", Vector("x"), "response"))
    val train  = get(TemplateObservation.of("train", "train", Vector(1e-308), 1.0))
    val held   = get(TemplateObservation.of("held", "held", Vector(2.0), 1.0))
    val split  = get(TemplateSplit.of(basis, Vector(train, held), Set("held")))
    val keys   = VersionedCodec.string(get(DefinitionId.of("example.overflow-key", 1)))
    val result = get(
      BaselineExports.fixedTemplate(
        split,
        get(DefinitionId.of("example.overflow-recipe", 1)),
        keys
      )
    )
    val predictions = result.find(_.family == ResultFamily.TemplatePredictions).get
    assertEquals(predictions.rows.size, 1)
    assertEquals(cells(predictions, "status"), Vector(ResultCell.Text("failure")))
    assertEquals(cells(predictions, "predicted"), Vector(ResultCell.Missing))
    assertEquals(cells(predictions, "residual"), Vector(ResultCell.Missing))
    assertEquals(numbers(predictions, "observed"), Vector(1.0))
    assert(cells(predictions, "error_json").head.text.contains("held"))
  }
  test("JSON columns are canonical values and malformed JSON is refused") {
    val column = ResultColumn("key_json", ResultColumnType.JsonUtf8, false, "text", "typed key")
    val a      = get(
      ResultTable.of(
        ResultFamily.PairScores,
        Vector(column),
        Vector(Vector(ResultCell.Text("{\"b\":1.0,\"a\":2}"))),
        Json.obj()
      )
    )
    val b = get(
      ResultTable.of(
        ResultFamily.PairScores,
        Vector(column),
        Vector(Vector(ResultCell.Text("{\"a\":2,\"b\":1}"))),
        Json.obj()
      )
    )
    assertEquals(a.rows, b.rows); assertEquals(a.identity, b.identity)
    assert(
      ResultTable
        .of(
          ResultFamily.PairScores,
          Vector(column),
          Vector(Vector(ResultCell.Text("invalid"))),
          Json.obj()
        )
        .isLeft
    )
  }
