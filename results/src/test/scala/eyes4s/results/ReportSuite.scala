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

package eyes4s.results

import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

import scala.compiletime.testing.typeCheckErrors

/** Reports over hand-built query tables, whose every estimate is worked out
  * here by hand, and over a completed study result, whose stored rows the
  * report must read without recomputing them.
  */
class ReportSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(e => fail(s"$e"), identity)

  private val hex     = (c: Char) => c.toString * 64
  private val binding = ReportBinding(
    get(BindingDigest.parse("plan", hex('a'))),
    get(BindingDigest.parse("input", hex('b'))),
    get(BindingDigest.parse("result", hex('c'))),
    Some(get(BindingDigest.parse("covariates", hex('d'))))
  )

  private val memory     = get(CovariateName.of("memory"))
  private val confidence = get(CovariateName.of("confidence"))
  private val levels     = get(Levels.of(Vector("Remembered", "Forgotten")))
  private val rating     = get(NumericUnit.of("rating"))
  private val memoryTerm = LevelTerm.Categorical(memory, levels)
  private val schema     = get(
    CovariateSchema.of(
      Vector(
        Covariate(memory, CovariateType.Categorical(levels)),
        Covariate(confidence, CovariateType.Numeric(rating))
      )
    )
  )
  private val difference = get(ReportSelection.of(Vector(Role.Difference), Vector("value")))
  private val id         = get(ReportId.of("memory"))

  private def query(
      key: StudyKey,
      value: Option[Double],
      remembered: Option[Boolean],
      share: Option[Double] = Some(0.0)
  ): Query[StudyKey] = get(
    Query.of(
      key,
      key.participant,
      key.stimulus,
      key.phase,
      1,
      Vector(
        memory -> remembered.fold(Value.Missing(Absence.NotRecorded))(r =>
          Value.Present(CovariateValue.Level(if r then "Remembered" else "Forgotten"))
        ),
        confidence -> Value.Present(CovariateValue.Number(3.0))
      ),
      Vector.empty,
      Vector(
        WindowMeasure.OutsideWindowShare -> share.fold(
          Value.Missing(Absence.Undefined(UndefinedReason.ZeroDuration))
        )(Value.Present(_))
      ),
      RoleOutcome.NotStored,
      RoleOutcome.NotStored,
      value.fold(RoleOutcome.Failed(DiagnosticCode("contrast-row", "arithmetic"), "failed"))(
        v => RoleOutcome.Scored(Vector(v))
      )
    )
  )

  private def k(p: String, i: String) = StudyKey(p, i, "recall")

  // p1 remembers a, b and forgets c; p2 remembers a only; p3 forgets d only.
  private val queries = Vector(
    query(k("p1", "a"), Some(0.1), Some(true)),
    query(k("p1", "b"), Some(0.3), Some(true)),
    query(k("p1", "c"), Some(-0.2), Some(false)),
    query(k("p2", "a"), Some(0.5), Some(true)),
    query(k("p3", "d"), Some(0.4), Some(false))
  )
  private val table = get(QueryTable.of(0, Vector("value"), schema, queries))

  private def spec(
      groupBy: Vector[Grouping] = Vector.empty,
      reduce: ReducePolicy = ReducePolicy.default,
      filter: Option[Predicate] = None,
      contrast: Option[LevelContrast] = None,
      spread: Spread = Spread.StandardDeviation
  ) = get(ReportSpec.of(id, 0, difference, filter, groupBy, reduce, contrast, spread))

  private def report(s: ReportSpec, t: QueryTable[StudyKey] = table) = get(
    Report.reduce(s, t, binding)
  )

  private def estimate(r: Report[StudyKey], group: GroupKey = GroupKey.all) =
    get(r.cell(group, Role.Difference, "value").toRight("no cell")).estimate

  private def close(v: Value[Double], expected: Double) = v match
    case Value.Present(x) => assert(math.abs(x - expected) < 1e-15, s"$x != $expected")
    case other            => fail(s"expected $expected, got $other")

  test("participant means weigh each participant once; pooled queries weigh each query") {
    // p1: mean(0.1, 0.3, -0.2) = 0.2/3; p2: 0.5; p3: 0.4.
    close(estimate(report(spec())), (0.2 / 3 + 0.5 + 0.4) / 3)
    close(
      estimate(report(spec(reduce = ReducePolicy.PooledQueries))),
      (0.1 + 0.3 - 0.2 + 0.5 + 0.4) / 5
    )
    val cell = get(report(spec()).cell(GroupKey.all, Role.Difference, "value").toRight("cell"))
    assertEquals((cell.participants, cell.queries), (3, 5))
    assertEquals(
      cell.perParticipant.map(p => p.participant -> p.queries),
      Vector("p1" -> 3, "p2" -> 1, "p3" -> 1)
    )
    assertEquals(cell.dispersion.n, 3)
    assertEquals(cell.dispersion.sem, None)
  }

  test("minimumQueries leaves a participant out of that group only, and reports it") {
    val minimum = ReducePolicy.ParticipantMeans(get(MinimumQueries.of(2)))
    val r       = report(spec(reduce = minimum))
    close(estimate(r), 0.2 / 3)
    val cell = get(r.cell(GroupKey.all, Role.Difference, "value").toRight("cell"))
    assertEquals(cell.perParticipant(1).value, Value.Missing(Absence.BelowMinimum(1, 2)))
    assertEquals(cell.participants, 1)
    assertEquals(get(r.accountingOf(Role.Difference).toRight("books")).belowMinimum, 2)
    assert(
      r.findings.contains(ReportFinding.BelowMinimum("p2", GroupKey.all, Role.Difference, 1, 2))
    )
    // Grouped by memory, p1 still has two remembered queries.
    val grouped    = report(spec(Vector(Grouping.ByLevel(memoryTerm)), minimum))
    val remembered = GroupKey(Vector("covariate:memory" -> "Remembered"))
    close(estimate(grouped, remembered), 0.2)
    val forgotten = GroupKey(Vector("covariate:memory" -> "Forgotten"))
    assertEquals(estimate(grouped, forgotten), Value.Missing(Absence.BelowMinimum(1, 2)))
  }

  test("a within-participant level contrast pairs participants and reports the unpaired") {
    val r = report(
      spec(
        Vector(Grouping.ByLevel(memoryTerm)),
        contrast = Some(LevelContrast(memoryTerm, "Remembered", "Forgotten"))
      )
    )
    val stat = get(r.contrasts.headOption.toRight("no contrast"))
    assertEquals(stat.paired.map(_.participant), Vector("p1"))
    close(stat.estimate, 0.2 - (-0.2))
    assertEquals(
      stat.unpaired,
      Vector(
        UnpairedParticipant("p2", "Remembered", "Forgotten"),
        UnpairedParticipant("p3", "Forgotten", "Remembered")
      )
    )
    assertEquals(
      stat.dispersion.sd,
      Value.Missing(Absence.Undefined(UndefinedReason.TooFewForSpread(1)))
    )
    assertEquals(
      r.findings.collect { case ReportFinding.UnpairedParticipant(p, _, _, _) => p },
      Vector("p2", "p3")
    )
  }

  test("an unknown filter excludes the query and reports the term; it is never a failure") {
    val undefined = queries.updated(0, query(k("p1", "a"), Some(0.1), Some(true), share = None))
    val t         = get(QueryTable.of(0, Vector("value"), schema, undefined))
    val shareTerm = NumericTerm.Window(WindowMeasure.OutsideWindowShare)
    val filter    = Predicate.Cmp(shareTerm, Comparison.LessOrEqual, 0.25)
    val r         = report(spec(filter = Some(filter)), t)
    val books     = get(r.accountingOf(Role.Difference).toRight("books"))
    assertEquals(
      (books.eligible, books.kept, books.unknownPredicate, books.failed),
      (5, 4, 1, 0)
    )
    assert(
      r.findings.contains(
        ReportFinding.UnknownPredicate(k("p1", "a"), "window:outside-window-share")
      )
    )
    assert(
      r.findings.contains(
        ReportFinding.UndefinedWindowShare(k("p1", "a"), WindowMeasure.OutsideWindowShare)
      )
    )
    // IsMissing decides a missing value: the excluded query is exactly the one it selects.
    val missing = report(spec(filter = Some(Predicate.IsMissing(shareTerm))), t)
    assertEquals(get(missing.accountingOf(Role.Difference).toRight("books")).kept, 1)
  }

  test(
    "a failed query is counted, a group with no query is missing, and missing is never zero"
  ) {
    val failing = queries :+ query(k("p4", "e"), None, Some(true))
    val t       = get(QueryTable.of(0, Vector("value"), schema, failing))
    val filter  = Predicate.In(memoryTerm, Vector("Remembered"))
    val r       = report(spec(Vector(Grouping.ByLevel(memoryTerm)), filter = Some(filter)), t)
    val books   = get(r.accountingOf(Role.Difference).toRight("books"))
    assertEquals((books.eligible, books.kept, books.filteredOut, books.failed), (6, 3, 2, 1))
    val forgotten = GroupKey(Vector("covariate:memory" -> "Forgotten"))
    assertEquals(estimate(r, forgotten), Value.Missing(Absence.EmptyGroup))
    assert(r.findings.contains(ReportFinding.EmptyGroup(forgotten, Role.Difference)))
    val cells = get(ReportTables.cells(r))
    val row   = cells.rows(
      cells
        .column("group_json")
        .get
        .indexWhere(
          _ == ResultCell.Text("""[{"level":"Forgotten","term":"covariate:memory"}]""")
        )
    )
    val at = (name: String) => row(cells.columns.indexWhere(_.name == name))
    assertEquals(at("estimate"), ResultCell.Missing)
    assertEquals(at("estimate_absence"), ResultCell.Text("empty-group"))
    assertEquals(at("queries"), ResultCell.Integer(0))
  }

  test("a query without a group attribute is kept but ungrouped, and accounted for") {
    val ungrouped = queries :+ query(k("p4", "e"), Some(0.9), None)
    val t         = get(QueryTable.of(0, Vector("value"), schema, ungrouped))
    val r         = report(spec(Vector(Grouping.ByLevel(memoryTerm))), t)
    val books     = get(r.accountingOf(Role.Difference).toRight("books"))
    assertEquals((books.kept, books.missingGroupAttribute), (6, 1))
    assertEquals(r.cells.map(_.queries).sum, books.kept - books.missingGroupAttribute)
    assert(
      r.findings.contains(ReportFinding.MissingCovariate(k("p4", "e"), "covariate:memory"))
    )
  }

  test("numeric terms group only through declared bins") {
    val low  = get(Bin.of("low", 0, 3, UpperEdge.Excluded))
    val high = get(Bin.of("high", 3, 5, UpperEdge.Included))
    val bins = get(Bins.of(Vector(low, high)))
    val r    =
      report(spec(Vector(Grouping.ByBins(NumericTerm.Covariate(confidence, rating), bins))))
    assertEquals(
      r.groups.map(_.render),
      Vector("covariate:confidence=low", "covariate:confidence=high")
    )
    assertEquals(estimate(r, r.groups(0)), Value.Missing(Absence.EmptyGroup))
    assert(Bins.of(Vector(high, low)).isLeft)
    // A closed upper edge holds its end value; an open one does not.
    assert(
      high.contains(5.0) && high.contains(3.0) && !low.contains(3.0) && !high.contains(5.5)
    )
    assertEquals(bins.locate(5.0), Some(high))
    assert(Bins.of(Vector(get(Bin.of("a", 0, 3, UpperEdge.Included)), high)).isLeft)
    assertNotEquals(
      typeCheckErrors("Grouping.ByLevel(NumericTerm.Occurrence)"),
      Nil
    )
    assertNotEquals(
      typeCheckErrors(
        "Grouping.ByBins(LevelTerm.Layout(LayoutField.Item), null.asInstanceOf[Bins])"
      ),
      Nil
    )
  }

  test("a value has no arithmetic and no zero to fall back on") {
    assertNotEquals(typeCheckErrors("summon[Numeric[Value[Double]]]"), Nil)
    assertNotEquals(typeCheckErrors("summon[cats.Monoid[Value[Double]]]"), Nil)
    assertNotEquals(typeCheckErrors("Value.Present(1.0) + Value.Present(2.0)"), Nil)
    assertNotEquals(typeCheckErrors("(Value.Present(1.0): Value[Double]).orZero"), Nil)
    assertNotEquals(typeCheckErrors("(Value.Present(1.0): Value[Double]).getOrElse(0.0)"), Nil)
    assertNotEquals(typeCheckErrors("(Value.Present(1.0): Value[Double]).sum"), Nil)
  }

  test("predicates only compare the terms whose values support the comparison") {
    assertNotEquals(
      typeCheckErrors(
        "Predicate.Cmp(LevelTerm.Layout(LayoutField.Participant), Comparison.Less, 1.0)"
      ),
      Nil
    )
    assertNotEquals(
      typeCheckErrors(
        "Predicate.AtLeast(LevelTerm.Categorical(null, null), \"x\")"
      ),
      Nil
    )
    val undeclared = ReportSpec.of(
      id,
      0,
      difference,
      Some(Predicate.In(memoryTerm, Vector("Unsure")))
    )
    assertEquals(
      undeclared.left.map(_.message),
      Left(
        SpecError
          .UndeclaredLevel("covariate:memory", "Unsure", Vector("Remembered", "Forgotten"))
          .message
      )
    )
  }

  test("three-valued logic: unknown only where the value decides") {
    import Truth.*
    assertEquals(Unknown.and(False), False)
    assertEquals(Unknown.or(True), True)
    assertEquals(Unknown.and(True), Unknown)
    assertEquals(Unknown.not, Unknown)
    val share = NumericTerm.Window(WindowMeasure.OutsideWindowShare)
    val both  = Predicate.Or(
      Predicate.Cmp(share, Comparison.Less, 0.5),
      Predicate.In(LevelTerm.Layout(LayoutField.Participant), Vector("p1"))
    )
    val undefined = Vector(query(k("p1", "a"), Some(0.1), Some(true), share = None))
    val t         = get(QueryTable.of(0, Vector("value"), schema, undefined))
    assertEquals(
      get(
        report(spec(filter = Some(both)), t).accountingOf(Role.Difference).toRight("books")
      ).kept,
      1
    )
  }

  test("covariates the report reads must be declared as the table declares them") {
    val other: LevelTerm.Ordinal = LevelTerm.Ordinal(memory, levels)
    val s                        = spec(Vector(Grouping.ByLevel(other)))
    assertEquals(
      Report.reduce(s, table, binding).left.map(_.message),
      Left(
        ReportError
          .CovariateMismatch(
            "memory",
            CovariateType.Ordinal(levels).render,
            CovariateType.Categorical(levels).render
          )
          .message
      )
    )
    val c = ReportSpec.of(
      id,
      0,
      difference,
      Some(Predicate.AtLeast(other, "Forgotten")),
      Vector(Grouping.ByLevel(memoryTerm))
    )
    assert(c.left.exists(_.isInstanceOf[SpecError.CovariateDeclarations]), c)
  }

  test("a stale binding is refused, naming the field") {
    val r     = report(spec())
    val moved = binding.copy(result = get(BindingDigest.parse("result", hex('e'))))
    assertEquals(r.checkCurrent(binding), Right(()))
    assertEquals(
      r.checkCurrent(moved),
      Left(ReportError.StaleBinding("result", s"sha256:${hex('c')}", s"sha256:${hex('e')}"))
    )
    assert(BindingDigest.parse("plan", "ABC").isLeft)
  }

  test("a stored report is rebuilt only when its accounting and cells agree") {
    val r = report(spec(Vector(Grouping.ByLevel(memoryTerm))))
    assertEquals(
      Report.reconstruct(
        r.spec,
        r.binding,
        r.groups,
        r.cells,
        r.contrasts,
        r.accounting,
        r.findings
      ),
      Right(r)
    )
    val dropped = r.cells.drop(1)
    assert(
      Report
        .reconstruct(
          r.spec,
          r.binding,
          r.groups,
          dropped,
          r.contrasts,
          r.accounting,
          r.findings
        )
        .isLeft
    )
    val books = r.accounting.map(a =>
      get(
        Accounting.of(
          a.role,
          a.eligible,
          a.kept - 1,
          a.filteredOut + 1,
          a.unknownPredicate,
          a.failed,
          a.missingGroupAttribute,
          a.belowMinimum
        )
      )
    )
    assert(
      Report
        .reconstruct(r.spec, r.binding, r.groups, r.cells, r.contrasts, books, r.findings)
        .isLeft
    )
    assert(Accounting.of(Role.Matched, 3, 1, 1, 0, 0, 0, 0).isLeft)
  }

  // ---------------------------------------------------------------- a stored study

  private val frame                = get(Frame.screen("report", 2, 2))
  private val grid                 = get(Grid.over(frame, 2, 2))
  private def clock(key: StudyKey) =
    ClockId(key.participant + "/" + key.stimulus + "/" + key.phase)
  private def trial(key: StudyKey, xs: Vector[(Double, Double)]) =
    val fixes = xs.zipWithIndex.map { case ((x, y), i) =>
      get(
        Event.Fixation.withoutDispersion(
          get(
            Interval
              .of(clock(key), Instant.micros(i * 1000L), Instant.micros(i * 1000L + 1000L))
          ),
          Pt[Px](x, y),
          1
        )
      )
    }
    Trial(key, (), get(Scanpath.of(frame, clock(key), IArray.from(fixes))))

  private val layout = Vector(
    ("p1", "a", Vector(0.5 -> 0.5, 1.5 -> 0.5), Vector(0.5 -> 0.5, 0.5 -> 1.5)),
    ("p1", "b", Vector(1.5 -> 1.5, 1.5 -> 0.5), Vector(1.5 -> 1.5, 0.5 -> 0.5)),
    ("p1", "c", Vector(0.5 -> 1.5, 0.5 -> 1.5), Vector(0.5 -> 1.5, 1.5 -> 1.5)),
    ("p2", "a", Vector(0.5 -> 0.5, 0.5 -> 0.5), Vector(0.5 -> 0.5, 1.5 -> 0.5)),
    ("p2", "b", Vector(1.5 -> 0.5, 1.5 -> 1.5), Vector(1.5 -> 0.5, 0.5 -> 1.5))
  )
  private val input = StudyInput(
    Trials(layout.flatMap { (p, i, recall, encode) =>
      Vector(trial(StudyKey(p, i, "recall"), recall), trial(StudyKey(p, i, "encode"), encode))
    })
  )
  private val plan = get(
    StudyPlan.cosine(
      input.reference,
      grid,
      "recall",
      "encode",
      Weight.Duration,
      Vector(StudyEstimate.Binned()),
      FailurePolicy.RequireAll
    )
  )
  private val result                            = get(plan.run(input))
  private val remembered: Set[(String, String)] = Set("p1" -> "a", "p1" -> "b", "p2" -> "b")
  private val covariates                        = get(
    CovariateTable.of(
      schema,
      layout.map { (p, i, _, _) =>
        StudyKey(p, i, "recall") -> get(
          Attributes.of(
            Vector(
              "memory" -> AttributeValue.Text(if remembered(p -> i) then "Remembered"
              else "Forgotten"),
              "confidence" -> AttributeValue.Integer(if i == "a" then 4L else 2L)
            )
          )
        )
      }
    )
  )
  private val source = get(ReportSource.study(plan, input, result, Some(covariates), binding))

  private def stored(key: StudyKey): Double =
    get(
      get(result.scales.head.contrast.left.map(_.message)).rows
        .find(_.key == key)
        .toRight("row")
        .flatMap(_.difference.left.map(_.message))
    ).value

  test("a stored study is reduced from its stored contrast rows, read by key") {
    val s = spec(
      Vector(Grouping.ByLevel(memoryTerm)),
      contrast = Some(LevelContrast(memoryTerm, "Remembered", "Forgotten")),
      spread = Spread.StandardDeviationAndError
    )
    val r           = get(Report.evaluate(s, source))
    val rememberedG = GroupKey(Vector("covariate:memory" -> "Remembered"))
    val cell        = get(r.cell(rememberedG, Role.Difference, "value").toRight("cell"))
    val p1          = (stored(k("p1", "a")) + stored(k("p1", "b"))) / 2
    val p2          = stored(k("p2", "b"))
    close(cell.estimate, (p1 + p2) / 2)
    assertEquals(
      cell.members,
      Vector(k("p1", "a"), k("p1", "b"), k("p2", "b")).map(ResultRef.ContrastRow(0, _))
    )
    assert(cell.dispersion.sem.exists(_.isPresent))
    val inspection = get(ResultInspection.study(plan, result, input, None))
    cell.members.foreach(ref => assert(inspection.contrastRow(ref).isRight, s"$ref"))
    val stat = get(r.contrasts.headOption.toRight("contrast"))
    assertEquals(stat.paired.map(_.participant), Vector("p1", "p2"))
    close(
      stat.estimate,
      ((p1 - stored(k("p1", "c"))) + (p2 - stored(k("p2", "a")))) / 2
    )
    val books = get(r.accountingOf(Role.Difference).toRight("books"))
    assertEquals((books.eligible, books.kept), (5, 5))
    assertEquals(
      get(ReportTables.all(r)).map(_.family),
      Vector(
        ResultFamily.ReportCells,
        ResultFamily.ReportParticipants,
        ResultFamily.ReportContrasts
      )
    )
  }

  test("a source refuses a result another plan computed, and an unknown scale or component") {
    val other = get(
      StudyPlan.cosine(
        input.reference,
        grid,
        "recall",
        "encode",
        Weight.Uniform,
        Vector(StudyEstimate.Binned()),
        FailurePolicy.RequireAll
      )
    )
    assert(
      ReportSource
        .study(other, input, result, None, binding)
        .left
        .exists {
          case ReportError.PlanMismatch(changes) => changes == Vector("weight"); case _ => false
        }
    )
    val far = get(ReportSpec.of(id, 3, difference))
    assertEquals(
      Report.evaluate(far, source).left.map(_.message),
      Left(ReportError.UnknownScale(3, 1).message)
    )
    val shape =
      get(ReportSpec.of(id, 0, get(ReportSelection.of(Vector(Role.Matched), Vector("shape")))))
    assertEquals(
      Report.evaluate(shape, source).left.map(_.message),
      Left(ReportError.UnknownComponent("shape", Vector("value")).message)
    )
    assertEquals(
      Report.evaluate(spec(), source, binding.copy(covariates = None)).left.map(_.message),
      Left(ReportError.StaleBinding("covariates", "none", s"sha256:${hex('d')}").message)
    )
  }
