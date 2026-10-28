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

class CompactManifestSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val limits = get(CompactArtifactLimits.of(1000000L, 268435456L))
  private val input  = get(
    StoredArtifact.compactInput(
      "input",
      ManifestFixtures.inputs,
      ManifestFixtures.input,
      limits
    )
  )
  private val ledger = get(
    StoredArtifact.compactLedger(
      "ledger",
      ManifestFixtures.inputs,
      ManifestFixtures.ledger,
      limits
    )
  )
  private val result = get(
    ResultManifest.compactPacked(
      "result",
      ManifestFixtures.results,
      ManifestFixtures.result,
      limits
    )
  )
  private val relations = Vector(
    ManifestRelation.PlanInput(ManifestFixtures.planArtifact.name, input.name),
    ManifestRelation.LedgerOf(ledger.name, input.name),
    ManifestRelation.ResultOf(
      result.result.name,
      ManifestFixtures.planArtifact.name,
      input.name
    )
  ) ++ result.relations
  private val saved = get(
    SavedManifest.of(
      Vector(ManifestFixtures.planArtifact, input, ledger, result.result) ++ result.payloads,
      relations
    )
  )

  test(
    "manifest v2 resolves actual compact input, ledger and packed result with unchanged scientific values"
  ) {
    assertEquals(
      get(Wire.definition(get(ScientificManifest.codec.encode(saved.manifest)), "schema")),
      CompactArchiveDefinitions.manifestV2
    )
    assertEquals(
      Vector(input, ledger, result.result).map(_.entry.media).distinct,
      Vector(MediaKind.ColumnarJson)
    )
    val resolved =
      get(ArtifactResolver.resolve(saved.address, saved.source, ManifestFixtures.decoders))
    assertEquals(
      get(ManifestFixtures.inputs.input.encode(resolved.inputs.head._2)),
      get(ManifestFixtures.inputs.input.encode(ManifestFixtures.input))
    )
    assertEquals(resolved.ledgers.head._2.records, ManifestFixtures.ledger.records)
    assertEquals(
      get(resolved.results.head._2.encode),
      get(ManifestFixtures.results.codec.encode(ManifestFixtures.result))
    )
    assertEquals(resolved.payloads.size, result.payloads.size)
  }

  test("a decoder decorator forwards the structured routes") {
    val decorated = new ArtifactDecoders.Delegating(ManifestFixtures.decoders) {}
    assert(ArtifactResolver.resolve(saved.address, saved.source, decorated).isRight)
  }

  test("v1 cannot admit compact media; default JSON artifacts keep their v1 envelope") {
    val document = get(ScientificManifest.codec.encode(saved.manifest))
    val old      = document.mapObject(_.add("schema", Wire.id(ScientificManifest.schema)))
    assert(ScientificManifest.codec.decode(old).isLeft)
    val legacy = get(
      SavedManifest.of(
        Vector(ManifestFixtures.planArtifact, ManifestFixtures.inputArtifact),
        Vector(
          ManifestRelation.PlanInput(
            ManifestFixtures.planArtifact.name,
            ManifestFixtures.inputArtifact.name
          )
        )
      )
    )
    assertEquals(
      get(Wire.definition(get(ScientificManifest.codec.encode(legacy.manifest)), "schema")),
      ScientificManifest.schema
    )
  }

  test("compact arrays and stored bytes retain the caller's limits") {
    val smallRows = get(CompactArtifactLimits.of(2L, 268435456L))
    assert(
      StoredArtifact
        .compactInput("input", ManifestFixtures.inputs, ManifestFixtures.input, smallRows)
        .isLeft
    )
    val smallBytes = get(CompactArtifactLimits.of(1000000L, 64L))
    assert(
      StoredArtifact
        .compactInput("input", ManifestFixtures.inputs, ManifestFixtures.input, smallBytes)
        .isLeft
    )
    assert(CompactArtifactLimits.of(0, 100).isLeft)
  }

  test(
    "verified masses reconstruct a loaded compact result without asking for a whole JSON document"
  ) {
    val loaded = get(
      ArtifactResolver.resolve(saved.address, saved.source, ManifestFixtures.decoders)
    ).results.head._2
    val restored = get(
      loaded.reconstructWith(
        ManifestFixtures.results,
        loaded.result.scales.map(_.estimation.map(_._2.toOption))
      )
    )
    assertEquals(
      get(ManifestFixtures.results.codec.digest(restored)),
      get(ManifestFixtures.results.codec.digest(ManifestFixtures.result))
    )
  }
