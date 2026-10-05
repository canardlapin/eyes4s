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

package eyes4s.codec

import eyes4s.plan.{AttributeValue, Attributes, DiagnosticCode}
import eyes4s.results.*
import io.circe.Encoder

/** [[ReportSources.tables]]: a source over query tables a host holds, bound
  * to the canonical digest of exactly what it reads. The binding is stable
  * for equal tables and changes with any value the source reads; a source
  * that would read inconsistent tables is refused, naming its operands; and
  * reports evaluate over it. Shared by the JVM and Scala.js.
  */
class ReportSourcesTablesSuite extends munit.FunSuite:

  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private val memory = ok(CovariateName.of("memory"))
  private val levels = ok(Levels.of(Vector("Remembered", "Forgotten")))
  private val schema = ok(
    CovariateSchema.of(Vector(Covariate(memory, CovariateType.Categorical(levels))))
  )

  private final case class Row(
      key: String,
      participant: String = "P01",
      item: String = "beach",
      level: String = "Remembered",
      share: Option[Double] = Some(0.1),
      d: RoleOutcome = RoleOutcome.Scored(Vector(0.3)),
      components: Vector[String] = Vector("value"),
      phase: String = "retrieval",
      occurrence: Int = 1,
      measure: WindowMeasure = WindowMeasure.OutsideWindowShare
  )

  private def attributes(r: Row) =
    Attributes
      .of(Vector("memory" -> AttributeValue.Text(r.level)))
      .fold(e => fail(e.toString), identity)

  /** The row's covariates as its covariate table reads them, so a query
    * carries exactly its table's values and unparsed cells.
    */
  private def covariateRow(r: Row): CovariateRow[String] =
    ok(CovariateTable.of(schema, Vector(r.key -> attributes(r)))).row(r.key).get

  private def query(r: Row, covariates: Boolean = true): Query[String] = ok(
    Query.of(
      r.key,
      r.participant,
      r.item,
      r.phase,
      r.occurrence,
      if covariates then covariateRow(r).values else Vector.empty,
      if covariates then covariateRow(r).unparsed else Vector.empty,
      Vector(
        r.measure -> r.share.fold(
          Value.Missing(Absence.Undefined(UndefinedReason.ZeroDuration))
        )(Value.Present(_))
      ),
      RoleOutcome.Scored(Vector(0.7)),
      RoleOutcome.Scored(Vector(0.4)),
      r.d
    )
  )

  private val base = Vector(
    Row("q1"),
    Row("q2", participant = "P02", level = "Forgotten", d = RoleOutcome.Scored(Vector(0.1))),
    Row("q3", participant = "P02", item = "street")
  )

  private def table(rows: Vector[Row], scale: Int = 0): QueryTable[String] =
    ok(QueryTable.of(scale, rows.head.components, schema, rows.map(query(_))))

  private def covariates(rows: Vector[Row]): CovariateTable[String] =
    ok(
      CovariateTable.of(schema, rows.map(r => r.key -> attributes(r)))
    )

  private def source(rows: Vector[Row], scales: Int = 2, extra: Vector[Row] = Vector.empty) =
    ReportSources.tables(Encoder[String])(
      (0 until scales).toVector.map(table(rows, _)),
      Some(covariates(rows ++ extra))
    )

  private def binding(
      rows: Vector[Row],
      scales: Int = 2,
      extra: Vector[Row] = Vector.empty
  ): ReportBinding = ok(source(rows, scales, extra)).binding

  test("equal tables bind alike, whenever they are built") {
    assertEquals(binding(base), binding(base.map(identity)))
  }

  test("any value the source reads changes its binding, in the part that reads it") {
    val b                                                           = binding(base)
    def changed(rows: Vector[Row], scales: Int = 2): Vector[String] =
      val c = binding(rows, scales)
      Vector(
        Option.when(c.plan != b.plan)("plan"),
        Option.when(c.input != b.input)("input"),
        Option.when(c.result != b.result)("result"),
        Option.when(c.covariates != b.covariates)("covariates")
      ).flatten
    def at(i: Int)(f: Row => Row) = base.updated(i, f(base(i)))
    assertEquals(changed(at(1)(_.copy(d = RoleOutcome.Scored(Vector(0.11))))), Vector("result"))
    assertEquals(changed(at(1)(_.copy(d = RoleOutcome.NotStored))), Vector("result"))
    assertEquals(
      changed(
        at(1)(
          _.copy(d =
            RoleOutcome.Failed(ok(DiagnosticCode.host("study-failure", "off-window")), "x")
          )
        )
      ),
      Vector("result")
    )
    assertEquals(changed(at(0)(_.copy(share = Some(0.2)))), Vector("input"))
    assertEquals(changed(at(0)(_.copy(share = None))), Vector("input"))
    assertEquals(changed(at(2)(_.copy(item = "alley"))), Vector("input"))
    assertEquals(changed(at(2)(_.copy(participant = "P03"))), Vector("input"))
    assertEquals(changed(at(0)(_.copy(level = "Forgotten"))), Vector("covariates"))
    assertEquals(changed(base.map(_.copy(components = Vector("score")))), Vector("plan"))
    assertEquals(changed(base, scales = 3).contains("plan"), true)
    assertEquals(changed(base.reverse).nonEmpty, true)
    // Layout fields a report groups and filters by (LevelTerm.Layout).
    assertEquals(changed(at(0)(_.copy(phase = "encoding"))), Vector("input"))
    assertEquals(changed(at(0)(_.copy(occurrence = 2))), Vector("input"))
    // The window measure's name, not only its value.
    assertEquals(
      changed(at(0)(_.copy(measure = WindowMeasure.OutsideScreenShare))),
      Vector("input")
    )
    // The key identifies every row it is in.
    assertEquals(
      changed(at(0)(_.copy(key = "q9"))),
      Vector("input", "result", "covariates")
    )
  }

  test("an unparsed covariate cell is bound: it changes the report's findings") {
    // Report.reduce reads Query.unparsed for its CovariateType findings.
    val typo = base.updated(0, base(0).copy(level = "Rememberd"))
    val q1   = covariateRow(typo(0))
    assertEquals(q1.unparsed, Vector(memory -> "Rememberd"))
    val b = binding(base)
    val c = binding(typo)
    assertNotEquals(c.input, b.input)
    assertNotEquals(c.covariates, b.covariates)
    // Another unparsed spelling is another binding, though the value is
    // Missing(Unparsed) either way.
    val other = binding(base.updated(0, base(0).copy(level = "Remembred")))
    assertNotEquals(other.input, c.input)
    assertNotEquals(other.covariates, c.covariates)
  }

  test("query tables whose unparsed cells disagree with their covariate table are refused") {
    val typo    = base.updated(0, base(0).copy(level = "Rememberd"))
    val refused = ReportSources.tables(Encoder[String])(
      Vector(table(typo)),
      Some(covariates(base.updated(0, base(0).copy(level = "Remembred"))))
    )
    refused match
      case Left(CodecError.Report(ReportError.InvalidQuery("q1", reason))) =>
        assert(reason.contains("Rememberd") && reason.contains("Remembred"), reason)
      case wrong => fail(s"expected the q1 unparsed refusal, got $wrong")
  }

  test("covariate rows the source does not read are not bound") {
    val b = binding(base)
    assertEquals(binding(base, extra = Vector(Row("zz", participant = "P09"))), b)
    assertEquals(
      binding(base, extra = Vector(Row("zz", participant = "P09", level = "Forgotten"))),
      b
    )
  }

  test("the binding of fixed tables is pinned, the same on the JVM and Scala.js") {
    val b = binding(base)
    assertEquals(
      Vector(b.plan.hex, b.input.hex, b.result.hex, b.covariates.fold("none")(_.hex)),
      Vector(
        "764a21e8c0f231042c0f24cd77b1b25b87e1fbfc2046e95d7b39bfa6299ba306",
        "891d36cbfb9ed897992d41e501cc94560174c09d0264b9292f727cc664f5ebb9",
        "56faf7c7fd9b1696a323e59f6d7fda8969b66cccb42a47ab6562220e5aa97285",
        "c8f2ecee2d1b4cd51368f6b9a597a3b62fba2d23767d7c373415194abddc0ad2"
      )
    )
  }

  test("tables out of scale order, or covariates that disagree, are refused, named") {
    val shuffled = ReportSources.tables(Encoder[String])(
      Vector(table(base, 1), table(base, 0)),
      Some(covariates(base))
    )
    assertEquals(shuffled, Left(CodecError.Report(ReportError.TableOrder(0, 1))))
    assertEquals(
      ReportError.TableOrder(0, 1).message,
      "Query table 0 is of scale 1; the tables must be listed one per scale, in scale " +
        "order, so table 0 must be of scale 0."
    )
    val bare =
      ok(QueryTable.of(0, Vector("value"), CovariateSchema.empty, base.map(query(_, false))))
    val declared = ReportSources.tables(Encoder[String])(Vector(bare), Some(covariates(base)))
    assertEquals(
      declared,
      Left(CodecError.Report(ReportError.TableCovariates(Vector.empty, Vector("memory"))))
    )
    assertEquals(
      ReportError.TableCovariates(Vector.empty, Vector("memory")).message,
      "The query tables declare covariates [], but their covariate table declares [memory]."
    )
    val other    = covariates(base.updated(0, base(0).copy(level = "Forgotten")))
    val mismatch = ReportSources.tables(Encoder[String])(Vector(table(base)), Some(other))
    mismatch match
      case Left(CodecError.Report(ReportError.InvalidQuery("q1", reason))) =>
        assert(reason.contains("memory"), reason)
      case wrong => fail(s"expected the q1 covariate refusal, got $wrong")
    val unbound = ReportSources.tables(Encoder[String])(Vector(table(base)), None)
    assertEquals(
      unbound,
      Left(CodecError.Report(ReportError.UnboundCovariates(Vector("memory"))))
    )
  }

  test("a report evaluates over the source, bound to it") {
    val spec = ok(
      ReportSpec.of(
        ok(ReportId.of("by-memory")),
        0,
        ok(ReportSelection.of(Vector(Role.Difference), Vector("value"))),
        groupBy = Vector(Grouping.ByLevel(LevelTerm.Categorical(memory, levels)))
      )
    )
    val s      = ok(source(base))
    val report = ok(Report.evaluate(spec, s))
    assertEquals(report.binding, s.binding)
    assertEquals(
      report
        .cell(GroupKey(Vector("covariate:memory" -> "Forgotten")), Role.Difference, "value")
        .map(_.estimate),
      Some(Value.Present(0.1))
    )
  }
