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

import eyes4s.plan.*
import eyes4s.results.*
import io.circe.Json

/** Reports in a saved study: the report and its specification are manifest
  * entries, `report-of` binds the report to the stored result, input, plan
  * and ledger by canonical digest, and the resolver refuses a report bound
  * to anything else.
  */
class ReportManifestSuite extends munit.FunSuite:
  import ManifestFixtures.*

  private def get[E, A](e: Either[E, A]): A = e.fold(x => fail(s"$x"), identity)

  private val keys    = StudyCodecs.key(DefinitionId.studyKey)
  private val reports = ReportCodecs.report(keys)

  private val spec = get(
    ReportSpec.of(
      get(ReportId.of("by item")),
      0,
      get(ReportSelection.of(Vector(Role.Difference, Role.Matched), Vector("value"))),
      groupBy = Vector(Grouping.ByLevel(LevelTerm.Layout(LayoutField.Item)))
    )
  )

  /** The bound source: its binding computed from the values it reads. */
  private def source(
      withLedger: Boolean = true,
      covariates: CovariateSchema = CovariateSchema.empty
  ) =
    ReportSources.study(studies, inputs, results)(
      plan,
      input,
      result,
      Option.when(withLedger)(ledger),
      covariates
    )

  private val binding = get(source()).binding

  /** A report evaluated under a binding taken on trust: how a report stored
    * against since-changed documents looks.
    */
  private def evaluated(b: ReportBinding, s: ReportSpec = spec): Report[StudyKey] =
    get(Report.evaluate(s, get(ReportSource.study(plan, input, result, None, b))))

  private def graph(
      report: Report[StudyKey],
      stored: ReportSpec = spec,
      covariates: Option[String] = Some("ledger"),
      reportInput: String = "input"
  ): SavedManifest =
    get(
      SavedManifest.of(
        Vector(
          planArtifact,
          inputArtifact,
          ledgerArtifact,
          resultArtifact,
          baseArtifact,
          get(StoredArtifact.reportSpec("spec", stored)),
          get(StoredArtifact.report("report", reports, report))
        ),
        Vector(
          ManifestRelation.PlanInput(name("plan"), name("input")),
          ManifestRelation.ResultOf(name("result"), name("plan"), name("input")),
          ManifestRelation.LedgerOf(name("ledger"), name("input")),
          ManifestRelation.ReportOf(
            name("report"),
            name("spec"),
            name("result"),
            name(reportInput),
            covariates.map(name)
          )
        )
      )
    )

  private def resolve(saved: SavedManifest, withReports: Boolean = true) =
    ArtifactResolver
      .resolve(
        saved.address,
        saved.source,
        if withReports then decoders.withReports(reports) else decoders
      )
      .left
      .map(_.toVector)

  test("a report bound to the stored plan, input, result and ledger resolves") {
    val report   = evaluated(binding)
    val resolved = get(resolve(graph(report)))
    assertEquals(resolved.report(name("report")), Some(report))
    assertEquals(resolved.reportSpec(name("spec")), Some(spec))
    // A decorator forwards the report decoder rather than dropping it.
    val saved     = graph(report)
    val decorated = new ArtifactDecoders.Delegating(decoders.withReports(reports))
    val again     = ArtifactResolver.resolve(saved.address, saved.source, decorated)
    assertEquals(again.toOption.flatMap(_.report(name("report"))), Some(report))
    assertEquals(
      get(ReportCodecs.digest(get(results.codec.digest(result)), "result")),
      binding.result
    )
  }

  test("a report bound to another result, input, plan or ledger is refused, naming the field") {
    val other = get(BindingDigest.parse("result", "f" * 64))
    Vector(
      "plan"       -> binding.copy(plan = other),
      "input"      -> binding.copy(input = other),
      "result"     -> binding.copy(result = other),
      "covariates" -> binding.copy(covariates = None)
    ).foreach { (field, stale) =>
      val refused = resolve(graph(evaluated(stale)))
      assert(
        refused.left.exists(_.exists {
          case ResolveError.Relation(
                ManifestRelation.ReportOf(_, _, _, _, _),
                RelationMismatch.ReportBinding(`field`, _, _)
              ) =>
            true
          case _ => false
        }),
        s"$field: $refused"
      )
    }
  }

  test("a report of another specification than the stored one is refused") {
    val other = get(
      ReportSpec.of(
        get(ReportId.of("pooled")),
        0,
        spec.selection,
        groupBy = spec.groupBy,
        reduce = ReducePolicy.PooledQueries
      )
    )
    assert(
      resolve(graph(evaluated(binding, other))).left.exists(_.exists {
        case ResolveError.Relation(_, RelationMismatch.ReportSpec("pooled", "by item")) => true
        case _                                                                          => false
      })
    )
  }

  test("a report is refused without a report decoder, and needs exactly one report-of") {
    val saved = graph(evaluated(binding))
    assert(
      resolve(saved, withReports = false).left.exists(_.exists {
        case ResolveError.Decode(entry, CodecError.UnsupportedSchema("report", _, _)) =>
          entry == name("report")
        case _ => false
      })
    )
    assert(
      ScientificManifest
        .of(saved.manifest.entries, saved.manifest.relations.filterNot(_.kind == "report-of"))
        .isLeft
    )
  }

  test("a report without covariates writes its absent ledger as null, and reads it back") {
    val unbound  = binding.copy(covariates = None)
    val saved    = graph(evaluated(unbound), covariates = None)
    val json     = get(ScientificManifest.codec.encode(saved.manifest))
    val relation = get(
      json.hcursor
        .downField("value")
        .downField("relations")
        .values
        .flatMap(_.find(_.hcursor.get[String]("kind").contains("report-of")))
        .toRight("report-of")
    )
    assertEquals(relation.hcursor.downField("covariates").focus, Some(Json.Null))
    assertEquals(get(ScientificManifest.codec.decode(json)), saved.manifest)
    assert(resolve(saved).isRight)
  }

  test("the public source binds exactly the plan, input, result and ledger it reads") {
    val digests = (
      get(ReportCodecs.digest(get(studies.codec.digest(plan)), "plan")),
      get(ReportCodecs.digest(get(inputs.input.digest(input)), "input")),
      get(ReportCodecs.digest(get(results.codec.digest(result)), "result")),
      get(ReportCodecs.digest(get(inputs.ledger.digest(ledger)), "covariates"))
    )
    assertEquals(binding, ReportBinding(digests._1, digests._2, digests._3, Some(digests._4)))
    assertEquals(get(source(withLedger = false)).binding.covariates, None)
    val report = get(Report.evaluate(spec, get(source())))
    assertEquals(report.binding, binding)
  }

  test("a report that reads covariates needs a bound ledger that carries a trials table") {
    val memory = get(CovariateName.of("memory"))
    val levels = get(Levels.of(Vector("Remembered", "Forgotten")))
    val schema =
      get(CovariateSchema.of(Vector(Covariate(memory, CovariateType.Categorical(levels)))))
    val reading = get(
      ReportSpec.of(
        get(ReportId.of("memory")),
        0,
        spec.selection,
        groupBy = Vector(Grouping.ByLevel(LevelTerm.Categorical(memory, levels)))
      )
    )
    // No ledger: the source cannot bind covariates, and a report reading them is refused.
    assertEquals(
      source(withLedger = false, covariates = schema).left.map(_.message),
      Left(CodecError.Report(ReportError.UnboundCovariates(Vector("memory"))).message)
    )
    assertEquals(
      Report.evaluate(reading, get(source(withLedger = false))).left.map(_.message),
      Left(ReportError.UnboundCovariates(Vector("memory")).message)
    )
    // This ledger has no trials table, so it cannot be a covariate source either.
    assertEquals(
      source(covariates = schema).left.map(_.message),
      Left(CodecError.Report(ReportError.UnboundCovariates(Vector("memory"))).message)
    )
  }

  test("retained study sources validate each schema even after a successful read") {
    val retained = ReportSources.retainedStudy(studies, inputs, results)(
      plan,
      input,
      result,
      Some(ledger)
    )
    val memory = get(CovariateName.of("memory"))
    val levels = get(Levels.of(Vector("Remembered", "Forgotten")))
    val schema = get(
      CovariateSchema.of(Vector(Covariate(memory, CovariateType.Categorical(levels))))
    )
    assertEquals(get(retained(CovariateSchema.empty)).binding, binding)
    assertEquals(
      retained(schema).left.map(_.message),
      Left(CodecError.Report(ReportError.UnboundCovariates(Vector("memory"))).message)
    )
    val again = get(retained(CovariateSchema.empty))
    assertEquals(get(Report.evaluate(spec, again)), evaluated(binding))
  }

  test("retained study bindings belong to their own snapshot") {
    val withLedger = ReportSources.retainedStudy(studies, inputs, results)(
      plan,
      input,
      result,
      Some(ledger)
    )
    val withoutLedger = ReportSources.retainedStudy(studies, inputs, results)(
      plan,
      input,
      result,
      None
    )
    assertEquals(get(withLedger(CovariateSchema.empty)).binding, binding)
    assertEquals(
      get(withoutLedger(CovariateSchema.empty)).binding,
      binding.copy(covariates = None)
    )
    assertEquals(get(withLedger(CovariateSchema.empty)).binding, binding)
  }

  test("a report of other trials, bound to this study's documents, is refused naming them") {
    // The review's reproducer: a genuine binding over an unrelated query table.
    val forged  = get(Report.reduce(ReportFixtures.spec, ReportFixtures.table, binding))
    val refused = resolve(graph(forged, stored = ReportFixtures.spec))
    val named   = refused.left.toOption.toVector.flatten.collect {
      case ResolveError.Relation(_, RelationMismatch.ReportMembers(0, unknown)) => unknown
    }
    assertEquals(named.size, 1, refused)
    assert(named.head.exists(_.contains(StudyKey("p1", "a", "recall").toString)), named)
  }

  test("a report naming another input than its result's is refused") {
    val refused = resolve(graph(evaluated(binding), reportInput = "base"))
    assert(
      refused.left.exists(_.exists {
        case ResolveError.Relation(_, RelationMismatch.ReportInput("base", "input")) => true
        case _                                                                       => false
      }),
      refused
    )
  }

  /** The report with its cells changed, rebuilt through the checked reconstruction. */
  private def tampered(
      report: Report[StudyKey]
  )(change: Cell[StudyKey] => Cell[StudyKey]): Report[StudyKey] =
    get(
      Report.reconstruct(
        report.spec,
        report.binding,
        report.groups,
        report.cells.map(change),
        report.contrasts,
        report.accounting,
        report.findings
      )
    )

  test("a stored report whose values the shipped reduction does not give is refused") {
    val honest = get(Report.evaluate(spec, get(source())))
    assert(resolve(graph(honest)).isRight)
    // The review's case: +1000 on every estimate, with every binding intact.
    val inflated = tampered(honest)(c => c.copy(estimate = c.estimate.map(_ + 1000)))
    val first    = get(honest.cells.find(_.estimate.isPresent).toRight("no estimate"))
    val refused  = resolve(graph(inflated))
    val named    = refused.left.toOption.toVector.flatten.collect {
      case ResolveError.Relation(_, RelationMismatch.ReportCell(g, r, c, stored, recomputed)) =>
        (g, r, c, stored, recomputed)
    }
    assertEquals(named.size, 1, refused)
    val (group, role, component, stored, recomputed) = named.head
    assertEquals(
      (group, role, component),
      (first.group.render, first.role.toString, first.component)
    )
    assertEquals(
      stored,
      get(inflated.cell(first.group, first.role, first.component).toRight("cell")).toString
    )
    assertEquals(recomputed, first.toString)
  }

  test("a report cannot cite another scale's or role's rows, and its members drill down") {
    val honest = get(Report.evaluate(spec, get(source())))
    def rebuilt(change: ResultRef[StudyKey] => ResultRef[StudyKey]) =
      Report.reconstruct(
        honest.spec,
        honest.binding,
        honest.groups,
        honest.cells.map(c => c.copy(members = c.members.map(change))),
        honest.contrasts,
        honest.accounting,
        honest.findings
      )
    val shifted = rebuilt {
      case ResultRef.ContrastRow(_, k)       => ResultRef.ContrastRow(1, k)
      case ResultRef.Reduction(_, design, k) => ResultRef.Reduction(1, design, k)
      case other                             => other
    }
    assert(shifted.left.exists(_.message.contains("row of scale 0")), shifted)
    val crossed = rebuilt {
      case ResultRef.ContrastRow(s, k) => ResultRef.Reduction(s, StudyDesign.Control, k)
      case other                       => other
    }
    assert(crossed.left.exists(_.message.contains("row of scale 0")), crossed)
    // What resolves is what drills down: every member is a row the inspection finds.
    val resolved   = get(resolve(graph(honest)))
    val report     = get(resolved.report(name("report")).toRight("report"))
    val inspection = get(ResultInspection.study(plan, result, input, None))
    report.cells.flatMap(_.members).foreach {
      case ref @ ResultRef.ContrastRow(_, _) =>
        assert(inspection.contrastRow(ref).isRight, s"$ref")
      case ref => assert(inspection.reduction(ref).isRight, s"$ref")
    }
  }

  test(
    "a stored report with other accounting than the reduction's is refused naming the part"
  ) {
    val honest = get(Report.evaluate(spec, get(source())))
    val books  = honest.accounting.map(a =>
      get(
        Accounting.of(
          a.role,
          a.eligible + 1,
          a.kept,
          a.filteredOut + 1,
          a.unknownPredicate,
          a.failed,
          a.missingGroupAttribute,
          a.belowMinimum
        )
      )
    )
    val changed = get(
      Report.reconstruct(
        honest.spec,
        honest.binding,
        honest.groups,
        honest.cells,
        honest.contrasts,
        books,
        honest.findings
      )
    )
    assert(
      resolve(graph(changed)).left.exists(_.exists {
        case ResolveError.Relation(_, RelationMismatch.ReportRecomputed("accounting", _, _)) =>
          true
        case _ => false
      })
    )
  }
