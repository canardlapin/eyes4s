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

  private val binding = get(
    ReportCodecs.binding(
      (studies.codec, plan),
      (inputs.input, input),
      (results.codec, result),
      Some((inputs.ledger, ledger))
    )
  )

  private def evaluated(b: ReportBinding, s: ReportSpec = spec): Report[StudyKey] =
    get(Report.evaluate(s, get(ReportSource.study(plan, input, result, None, b))))

  private def graph(
      report: Report[StudyKey],
      stored: ReportSpec = spec,
      covariates: Option[String] = Some("ledger")
  ): SavedManifest =
    get(
      SavedManifest.of(
        Vector(
          planArtifact,
          inputArtifact,
          ledgerArtifact,
          resultArtifact,
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
            name("input"),
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
