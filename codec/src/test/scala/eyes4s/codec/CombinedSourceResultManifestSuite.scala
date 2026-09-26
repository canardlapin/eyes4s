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
