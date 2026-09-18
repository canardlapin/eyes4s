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

/** The frozen manifest-v1 over the pinned resource files: byte-identical
  * re-encoding, independently known file digests, and end-to-end resolution
  * of the four pinned artifacts from an in-memory source.
  */
class ManifestV1JvmSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A        = e.fold(error => fail(s"$error"), identity)
  private def resource(name: String): IArray[Byte] = get(GenerateManifestV1.resource(name))
  private def text(name: String): String           =
    val bytes = resource(name)
    new String(Array.tabulate(bytes.length)(bytes(_)), "UTF-8")
  private def name(value: String): ArtifactName = get(ArtifactName.of(value))

  /** Digests computed outside eyes4s with `shasum -a 256`. */
  private val shasum = Map(
    "study-v1.json"       -> "f5f5a02f327b79250387365542540297bdc18cffe379648828d77cda3e668f71",
    "study-input-v1.json" -> "dd9922646e8ced281d3ed8f63fb38f36774c775ef836d95a76b66ebc664c3b52",
    "admission-ledger-v1.json" -> "114585a745fa08bbb6acbbdb58590e6e29f16fd8e53aa2da7d5f77bccd7c5447",
    "study-result-v1.json" -> "d8429e2f980919fde450192e0485cfbcfc7920923fb45ef4863ea7923869d9f0",
    "manifest-v1.json" -> ManifestV1Fixtures.address
  )

  /** Manifests by address, entries by name, each served from its resource file. */
  private lazy val source = ByteSource.inMemory(
    Map(get(ByteDigest.parse(ManifestV1Fixtures.address)) -> resource("manifest-v1.json")),
    GenerateManifestV1.files.map((entry, file) => name(entry) -> resource(file)).toMap
  )
  private lazy val decoders = get(ArtifactDecoders.study[Px])

  test("manifest-v1.json is what the writer produces and re-encodes byte-identically") {
    val bytes    = resource("manifest-v1.json")
    val manifest = get(ScientificManifest.codec.parse(text("manifest-v1.json")))
    val written  = get(GenerateManifestV1.written)
    assertEquals(get(ScientificManifest.bytes(manifest)).toVector, bytes.toVector)
    assertEquals(written.manifest, manifest)
    assertEquals(written.bytes.toVector, bytes.toVector)
    assertEquals(written.address.hex, ManifestV1Fixtures.address)
    assertEquals(text("manifest-v1.json"), ManifestV1Fixtures.manifestVersionOne)
  }

  test("every file has its independently known SHA-256, and every entry its file's length") {
    shasum.foreach((file, hex) =>
      assertEquals(ByteDigest.sha256(resource(file)).hex, hex, file)
    )
    val manifest = get(ScientificManifest.codec.parse(text("manifest-v1.json")))
    GenerateManifestV1.files.foreach { (entry, file) =>
      val declared = get(manifest.entry(name(entry)).toRight(entry))
      assertEquals(declared.sha256.hex, shasum(file), file)
      assertEquals(declared.length, resource(file).length.toLong, file)
    }
  }

  test("the pinned plan, input, ledger and result resolve end to end from memory") {
    val address  = get(ByteDigest.parse(ManifestV1Fixtures.address))
    val resolved = get(ArtifactResolver.resolve(address, source, decoders).left.map(_.toVector))
    val input    = get(resolved.input(name("input")).toRight("input"))
    assertEquals(input.reference.digest, "cebe7474ab5c2aec")
    assertEquals(
      get(resolved.plan(name("plan")).toRight("plan")).prerequisites(Some(input)),
      Vector.empty
    )
    assertEquals(
      get(resolved.result(name("result")).toRight("result")).encode.map(_.spaces2),
      Right(text("study-result-v1.json"))
    )
    assertEquals(
      get(resolved.ledger(name("ledger")).toRight("ledger")).outcome,
      AdmissionOutcome.Refused
    )
  }

  test("the pinned ledger records a refused import and cannot be related to the pinned input") {
    val manifest = get(ScientificManifest.codec.parse(text("manifest-v1.json")))
    val claimed  = get(
      ScientificManifest.of(
        manifest.entries,
        manifest.relations :+ ManifestRelation.LedgerOf(name("ledger"), name("input"))
      )
    )
    assertEquals(
      ArtifactResolver.resolveManifest(claimed, source, decoders).left.map(_.toVector),
      Left(
        Vector(
          ResolveError.Relation(
            ManifestRelation.LedgerOf(name("ledger"), name("input")),
            RelationMismatch.RefusedAdmission
          )
        )
      )
    )
    // Independently of its outcome, its admitted records do not cover the pinned input.
    val input  = get(StudyInputCodecs.study[Px].input.parse(text("study-input-v1.json")))
    val ledger = get(StudyInputCodecs.study[Px].ledger.parse(text("admission-ledger-v1.json")))
    assertEquals(ledger.checkAgainst(input), Left(AdmissionError.UnadmittedTrial(0)))
  }
