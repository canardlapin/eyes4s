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

import eyes4s.kernel.ContentHash
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

/** The frozen manifest-v1 on both platforms. It carries no floating-point
  * number, so its canonical bytes, and therefore its address, are identical
  * on the JVM and Scala.js; the JVM suite additionally resolves it end to end
  * from the pinned resource files.
  */
class ManifestV1Suite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A     = e.fold(error => fail(s"$error"), identity)
  private def name(value: String): ArtifactName = get(ArtifactName.of(value))
  private def utf8(text: String): IArray[Byte]  = get(Utf8.encode(text).left.map(i => s"at $i"))

  test("frozen manifest v1 fixes its entries, roles, schemas, lengths, digests and relations") {
    val manifest = get(ScientificManifest.codec.parse(ManifestV1Fixtures.manifestVersionOne))
    assertEquals(
      manifest.entries.map(e => (e.name.value, e.role, e.schema, e.media, e.length, e.layout)),
      Vector(
        ("plan", ArtifactRole.StudyPlan, DefinitionId.study, MediaKind.JsonText, 984L, None),
        (
          "input",
          ArtifactRole.StudyInput,
          DefinitionId.studyInput,
          MediaKind.JsonText,
          21898L,
          None
        ),
        (
          "ledger",
          ArtifactRole.AdmissionLedger,
          DefinitionId.admissionLedger,
          MediaKind.JsonText,
          18987L,
          None
        ),
        (
          "result",
          ArtifactRole.StudyResult,
          DefinitionId.studyResult,
          MediaKind.JsonText,
          75738L,
          None
        )
      )
    )
    assertEquals(
      manifest.entries.map(_.identity),
      Vector(None, ContentHash.parse("cebe7474ab5c2aec"), None, None)
    )
    assertEquals(
      manifest.relations,
      Vector(
        ManifestRelation.PlanInput(name("plan"), name("input")),
        ManifestRelation.ResultOf(name("result"), name("plan"), name("input"))
      )
    )
  }

  test(
    "frozen manifest v1 re-encodes byte-identically and keeps its address on both platforms"
  ) {
    val manifest = get(ScientificManifest.codec.parse(ManifestV1Fixtures.manifestVersionOne))
    val bytes    = get(ScientificManifest.bytes(manifest))
    assertEquals(bytes.toVector, utf8(ManifestV1Fixtures.manifestVersionOne).toVector)
    assertEquals(get(ScientificManifest.address(manifest)).hex, ManifestV1Fixtures.address)
    assertEquals(
      get(ScientificManifest.codec.encode(manifest)),
      get(io.circe.parser.parse(ManifestV1Fixtures.manifestVersionOne))
    )
  }

  test(
    "a whitespace-only difference is refused before decode, and unavailable entries are named"
  ) {
    val address  = get(ByteDigest.parse(ManifestV1Fixtures.address))
    val decoders = get(ArtifactDecoders.study[Px])
    // SavedStudyFixtures.versionOne equals study-v1.json as JSON, not byte for byte.
    val source = ByteSource.inMemory(
      Map(address      -> utf8(ManifestV1Fixtures.manifestVersionOne)),
      Map(name("plan") -> utf8(SavedStudyFixtures.versionOne))
    )
    assertEquals(
      ArtifactResolver.resolve(address, source, decoders).left.map(_.toVector),
      Left(
        Vector(
          ResolveError.Length(name("plan"), 984L, utf8(SavedStudyFixtures.versionOne).length),
          ResolveError.Missing(name("input")),
          ResolveError.Missing(name("ledger")),
          ResolveError.Missing(name("result"))
        )
      )
    )
  }

  test(
    "the same graph over the portable fixture strings resolves end to end on both platforms"
  ) {
    val studies = StudyCodecs.cosine[Px]
    val inputs  = StudyInputCodecs.study[Px]
    val input   = get(inputs.input.parse(StudyInputFixtures.inputVersionOne))
    val saved   = get(
      for
        plan <- StoredArtifact.bytes(
          "plan",
          ArtifactRole.StudyPlan,
          utf8(SavedStudyFixtures.versionOne),
          None
        )
        in <- StoredArtifact.bytes(
          "input",
          ArtifactRole.StudyInput,
          utf8(StudyInputFixtures.inputVersionOne),
          Some(input.hash)
        )
        ledger <- StoredArtifact.bytes(
          "ledger",
          ArtifactRole.AdmissionLedger,
          utf8(StudyInputFixtures.ledgerVersionOne),
          None
        )
        result <- StoredArtifact.bytes(
          "result",
          ArtifactRole.StudyResult,
          utf8(StudyResultFixtures.resultVersionOne),
          None
        )
        saved <- SavedManifest.of(
          Vector(plan, in, ledger, result),
          Vector(
            ManifestRelation.PlanInput(plan.name, in.name),
            ManifestRelation.ResultOf(result.name, plan.name, in.name)
          )
        )
      yield saved
    )
    // The compact strings are other bytes than the pretty resource files, with the same identities.
    val frozen = get(ScientificManifest.codec.parse(ManifestV1Fixtures.manifestVersionOne))
    assert(saved.manifest.entries.zip(frozen.entries).forall((a, b) => a.sha256 != b.sha256))
    assertEquals(saved.manifest.entries.map(_.identity), frozen.entries.map(_.identity))
    val resolved = get(
      ArtifactResolver
        .resolve(saved.address, saved.source, get(ArtifactDecoders.study[Px]))
        .left
        .map(_.toVector)
    )
    val plan = get(studies.codec.parse(SavedStudyFixtures.versionOne))
    assertEquals(
      resolved.result(name("result")).map(_.encode),
      Some(StudyResultCodecs.cosine[Px].codec.encode(get(plan.run(input))))
    )
  }
