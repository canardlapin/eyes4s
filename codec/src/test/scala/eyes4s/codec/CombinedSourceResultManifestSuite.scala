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

import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import eyes4s.results.*

/** The source and packed-result extensions coexist in one graph and decorator stack. */
class CombinedSourceResultManifestSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val spec   = get(ImportSpecCodec.study[Px].parse(SourceIdentityMirrors.importSpec))
  private val ledger = get(
    StudyInputCodecs.study[Px].ledger.parse(SourceIdentityMirrors.ledgerV4)
  )
  private val source = get(
    StoredArtifact.sourceFile("source.csv", SourceFormat.FixationCsv, get(Utf8.encode("x\n")))
  )
  private val description = get(
    StoredArtifact.importSpec("import.json", ImportSpecCodec.study[Px], spec)
  )
  private val storedLedger = get(
    StoredArtifact.ledger("source-ledger", StudyInputCodecs.study[Px], ledger)
  )
  private val packed = get(
    ResultManifest.packed("result", ManifestFixtures.results, ManifestFixtures.result)
  )
  private val sourceRelation: ManifestRelation.LedgerSource = ManifestRelation.LedgerSource(
    storedLedger.name,
    source.name,
    description.name,
    LedgerSourceRole.Primary
  )
  private val artifacts = Vector(
    ManifestFixtures.planArtifact,
    ManifestFixtures.inputArtifact,
    packed.result,
    storedLedger,
    source,
    description
  ) ++ packed.payloads
  private val relations = Vector(
    ManifestRelation
      .PlanInput(ManifestFixtures.planArtifact.name, ManifestFixtures.inputArtifact.name),
    ManifestRelation.ResultOf(
      packed.result.name,
      ManifestFixtures.planArtifact.name,
      ManifestFixtures.inputArtifact.name
    ),
    sourceRelation
  ) ++ packed.relations

  test("mixed source and packed-result graphs retain both routes through import decorators") {
    val base       = ManifestFixtures.decoders
    val wrapped    = new ArtifactDecoders.Delegating[StudyKey, Px](base) {}
    val registered = base.withImportSpecs(ImportSpecCodec.study[Px])
    val routes     = Vector(
      wrapped.withImportSpecs(ImportSpecCodec.study[Px]),
      new ArtifactDecoders.Delegating[StudyKey, Px](registered) {}
    )
    for
      decoders <- routes
      reverse  <- Vector(false, true)
    do
      val saved = get(
        SavedManifest.of(
          if reverse then artifacts.reverse else artifacts,
          if reverse then relations.reverse else relations
        )
      )
      val loaded = get(
        ArtifactResolver.resolve(saved.address, saved.source, decoders).left.map(_.toVector)
      )
      assertEquals(loaded.sourceFile(source.name), Some("x\n"))
      assertEquals(loaded.importSpec(description.name).map(_.digest), Some(spec.digest))
      assertEquals(
        loaded.results.map(_._2.encode),
        Vector(ManifestFixtures.results.codec.encode(ManifestFixtures.result))
      )
      assertEquals(loaded.payloads.map(_._1).toSet, packed.payloads.map(_.name).toSet)
  }

  test("raw source bytes and packed payloads cannot substitute for each other's endpoints") {
    val wrongSource = relations.map(r =>
      if r == sourceRelation then sourceRelation.copy(sourceFile = packed.payloads.head.name)
      else r
    )
    val wrongPayload = relations.map {
      case ManifestRelation.ResultPayloadOf(result, _) =>
        ManifestRelation.ResultPayloadOf(result, source.name)
      case other => other
    }
    Vector(wrongSource, wrongPayload).foreach { rs =>
      assert(ScientificManifest.of(artifacts.map(_.entry), rs).left.toOption.exists {
        case ManifestError.RoleMismatch(_, _, _, _) => true
        case _                                      => false
      })
    }
  }

  test("source bindings do not mask an orphan packed result payload") {
    val orphaned = relations.filterNot(packed.relations.contains)
    assert(ScientificManifest.of(artifacts.map(_.entry), orphaned).left.toOption.exists {
      case ManifestError.RelationCount(_, "result-payload-of", 0, _) => true
      case _                                                         => false
    })
  }

  test("valid packed result edges do not mask two primary sources for one ledger") {
    val second = get(
      StoredArtifact.sourceFile(
        "second-source.csv",
        SourceFormat.FixationCsv,
        get(Utf8.encode("x\n"))
      )
    )
    val duplicate = sourceRelation.copy(sourceFile = second.name)
    assertEquals(
      ScientificManifest
        .of((artifacts :+ second).map(_.entry), relations :+ duplicate)
        .left
        .toOption,
      Some(
        ManifestError.RelationCount(
          storedLedger.name,
          "ledger-source:primary",
          2,
          "exactly one"
        )
      )
    )
  }

  test(
    "mixed source and packed-result graphs re-evaluate reports through either decorator order"
  ) {
    val reports    = ReportCodecs.report(StudyCodecs.key(DefinitionId.studyKey))
    val reportSpec = get(
      ReportSpec.of(
        get(ReportId.of("mixed graph")),
        0,
        get(ReportSelection.of(Vector(Role.Difference, Role.Matched), Vector("value")))
      )
    )
    val reportSource = get(
      ReportSources
        .study(ManifestFixtures.studies, ManifestFixtures.inputs, ManifestFixtures.results)(
          ManifestFixtures.plan,
          ManifestFixtures.input,
          ManifestFixtures.result,
          None,
          CovariateSchema.empty
        )
    )
    val inlineBound  = get(Report.evaluate(reportSpec, reportSource))
    val archiveCodec = new DensityArchiveCodec(ManifestFixtures.results)
    val archive      =
      get(archiveCodec.encode(ManifestFixtures.result, DensityStorage.Packed)).archive
    val packedBinding = reportSource.binding.copy(
      result = get(ReportCodecs.digest(get(archiveCodec.codec.digest(archive)), "result"))
    )
    val packedSource = get(
      ReportSource.study(
        ManifestFixtures.plan,
        ManifestFixtures.input,
        ManifestFixtures.result,
        None,
        packedBinding
      )
    )
    val honest = get(Report.evaluate(reportSpec, packedSource))
    assert(honest.cells.exists(_.estimate.isPresent))
    val inflated = get(
      Report.reconstruct(
        honest.spec,
        honest.binding,
        honest.groups,
        honest.cells.map(cell => cell.copy(estimate = cell.estimate.map(_ + 1000))),
        honest.contrasts,
        honest.accounting,
        honest.findings
      )
    )
    val storedSpec = get(StoredArtifact.reportSpec("report-spec", reportSpec))
    val base       = ManifestFixtures.decoders
    val routes     = Vector(
      base.withImportSpecs(ImportSpecCodec.study[Px]).withReports(reports),
      base.withReports(reports).withImportSpecs(ImportSpecCodec.study[Px])
    ).map(route => new ArtifactDecoders.Delegating[StudyKey, Px](route) {})
    for
      decoders <- routes
      report   <- Vector(honest, inflated, inlineBound)
    do
      val storedReport = get(StoredArtifact.report("report", reports, report))
      val saved        = get(
        SavedManifest.of(
          artifacts ++ Vector(storedSpec, storedReport),
          relations :+ ManifestRelation.ReportOf(
            storedReport.name,
            storedSpec.name,
            packed.result.name,
            ManifestFixtures.inputArtifact.name,
            None
          )
        )
      )
      val loaded = ArtifactResolver.resolve(saved.address, saved.source, decoders)
      if report == honest then
        val resolved = get(loaded.left.map(_.toVector))
        assertEquals(resolved.report(storedReport.name), Some(honest))
        assertEquals(resolved.sourceFile(source.name), Some("x\n"))
        assertEquals(resolved.payloads.map(_._1).toSet, packed.payloads.map(_.name).toSet)
      else if report == inflated then
        assert(
          loaded.left.exists(_.toVector.exists {
            case ResolveError
                  .Relation(_: ManifestRelation.ReportOf, _: RelationMismatch.ReportCell) =>
              true
            case _ => false
          }),
          loaded
        )
      else
        assert(
          loaded.left.exists(_.toVector.exists {
            case ResolveError.Relation(_, RelationMismatch.ReportBinding("result", _, _)) =>
              true
            case _ => false
          }),
          loaded
        )
  }
