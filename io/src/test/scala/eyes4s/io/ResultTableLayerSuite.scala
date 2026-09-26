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

import eyes4s.plan.StudyKey
import eyes4s.results.*

/** The one result-table layer from io: the circe entry point agrees with the
  * results layer's own JSON reading and identity, the report families render
  * through the same CSV transport, and the deprecated facade builds the very
  * tables `ResultExports` builds.
  */
class ResultTableLayerSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private def cells(t: ResultTable, name: String) =
    t.rows.map(_(t.columns.indexWhere(_.name == name)))

  test("table JSON reads and spells documents exactly as circe does") {
    val samples = Vector(
      """{"b":1.0,"a":[1e2,-0,0.10,"\u0000\u001f\u007f\u0085 λ\"\\/"],"c":null}""",
      """[true,false,{"k":{"z":1E-7,"y":1234567890123}}]""",
      """"\ud800"""",
      """-12.5e+3""",
      """{"dup":1,"dup":2}"""
    )
    samples.foreach { text =>
      val circe = get(io.circe.parser.parse(text))
      val mine  = get(TableJson.parse(text))
      assertEquals(mine.canonical, ResultTableJson.canonical(circe), clue = text)
      // The same value; Scala.js circe reads numbers as doubles, so only the JVM keeps spellings.
      assertEquals(ResultTableJson.circe(mine), circe, clue = text)
      assertEquals(TableJson.parse(mine.canonical).map(_.canonical), Right(mine.canonical))
    }
    Vector("""{"a":01}""", "[1,]", "\"\u0001\"", "NaN", "{\"a\" 1}", "") foreach { text =>
      assert(TableJson.parse(text).isLeft, text)
      assert(io.circe.parser.parse(text).isLeft, text)
    }
  }

  test("a table identity is the SHA-256 io has always taken of the same text") {
    for text <- Vector("", "abc", "λ\ud800x", "tail\ud800", "\udc00", "😀")
    do assertEquals(TableDigest.ofUtf8(text).hex, Sha256.ofUtf8(text).hex, clue = text)
  }

  test("the report families render through the same transport, nulls and absences explicit") {
    val hex     = (c: Char) => c.toString * 64
    val binding = ReportBinding(
      get(BindingDigest.parse("plan", hex('a'))),
      get(BindingDigest.parse("input", hex('b'))),
      get(BindingDigest.parse("result", hex('c'))),
      None
    )
    def query(p: String, item: String, value: Option[Double]) = get(
      Query.of(
        StudyKey(p, item, "recall"),
        p,
        item,
        "recall",
        1,
        Vector.empty,
        Vector.empty,
        Vector.empty,
        RoleOutcome.NotStored,
        RoleOutcome.NotStored,
        value.fold(RoleOutcome.NotStored)(v => RoleOutcome.Scored(Vector(v)))
      )
    )
    val table = get(
      QueryTable.of(
        0,
        Vector("value"),
        CovariateSchema.empty,
        Vector(
          query("p1", "a", Some(0.25)),
          query("p1", "b", Some(-0.5)),
          query("p2", "a", None)
        )
      )
    )
    val item = LevelTerm.Layout(LayoutField.Item)
    val spec = get(
      ReportSpec.of(
        get(ReportId.of("items")),
        0,
        get(ReportSelection.of(Vector(Role.Difference), Vector("value"))),
        groupBy = Vector(Grouping.ByLevel(item)),
        contrast = Some(LevelContrast(item, "a", "b"))
      )
    )
    val report = get(Report.reduce(spec, table, binding))
    val all    = get(ReportTables.all(report))
    assertEquals(
      all.map(_.family),
      Vector(
        ResultFamily.ReportCells,
        ResultFamily.ReportParticipants,
        ResultFamily.ReportContrasts
      )
    )
    all.foreach { t =>
      val parsed = get(Rfc4180.decode(t.csv.encode))
      assertEquals(parsed.head, t.csv.header)
      assertEquals(parsed.tail, t.csv.rows)
      val metadata = t.metadata.circe.hcursor
      assertEquals(metadata.get[String]("schema").toOption, Some(ResultTable.schema))
      assertEquals(metadata.get[String]("table_sha256").toOption, Some(t.identity.hex))
      assertEquals(
        metadata.downField("context").downField("binding").get[String]("plan").toOption,
        Some(hex('a'))
      )
      // Rebuilding through the circe entry point keeps the identity.
      val again = get(eyes4s.io.ResultTable.of(t.family, t.columns, t.rows, t.context.circe))
      assertEquals(again.identity, t.identity)
    }
    val contrasts = all(2)
    assertEquals(cells(contrasts, "estimate"), Vector(ResultCell.Number(0.75)))
    assertEquals(cells(contrasts, "sd"), Vector(ResultCell.Missing))
    assertEquals(cells(contrasts, "sd_absence"), Vector(ResultCell.Text("undefined")))
    // p2's only query failed: it is accounted as failed, never as a zero.
    assertEquals(cells(all(1), "participant").distinct, Vector(ResultCell.Text("p1")))
    assertEquals(get(report.accountingOf(Role.Difference).toRight("books")).failed, 1)
    val emptyB = all(0).rows.find(row =>
      row(all(0).columns.indexWhere(_.name == "group_json")) ==
        ResultCell.Text("""[{"level":"b","term":"item"}]""")
    )
    assertEquals(
      emptyB.map(_(all(0).columns.indexWhere(_.name == "queries"))),
      Some(ResultCell.Integer(1))
    )
  }

  test("the deprecated BaselineExports facade builds the very tables ResultExports builds") {
    import eyes4s.codec.VersionedCodec
    import eyes4s.design.*
    import eyes4s.plan.DefinitionId
    val basis = get(TemplateBasis.of("feature", Vector("x"), "response"))
    val train = get(TemplateObservation.of("train", "train", Vector(1.0), 2.0))
    val held  = get(TemplateObservation.of("held", "held", Vector(3.0), 7.0))
    val split =
      get(TemplateSplit.of(TemplateDesign.fixed(basis), Vector(train, held), Set("held")))
    val keys   = VersionedCodec.string(get(DefinitionId.of("example.facade-key", 1)))
    val recipe = get(DefinitionId.of("example.facade-recipe", 1))
    @annotation.nowarn("cat=deprecation")
    def viaFacade = BaselineExports.fixedTemplate(split, recipe, keys)
    val facade    = get(viaFacade)
    val direct    = get(ResultExports.fixedTemplate(split, recipe, keys))
    assertEquals(facade.map(_.identity), direct.map(_.identity))
    assert(direct.nonEmpty)
    assertEquals(eyes4s.io.ResultTable.schema, eyes4s.results.ResultTable.schema)
  }
