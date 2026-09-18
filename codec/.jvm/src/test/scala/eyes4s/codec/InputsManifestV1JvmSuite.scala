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

import eyes4s.plan.RecordingChannels
import eyes4s.kernel.Unit2D.Px
import io.circe.Json

/** The frozen UI-S6 fixtures over the recording and temporal inputs:
  * byte-identical writing, independently known file digests, byte-identical
  * re-encoding of every new JSON fixture, and end-to-end resolution of all
  * twelve entries from the resource files.
  */
class InputsManifestV1JvmSuite extends munit.FunSuite:
  import InputsManifestV1Fixtures.{files, payloadFiles}

  private def get[E, A](e: Either[E, A]): A        = e.fold(error => fail(s"$error"), identity)
  private def resource(name: String): IArray[Byte] = get(
    GenerateInputsManifestV1.resource(name)
  )
  private def text(name: String): String =
    val bytes = resource(name)
    new String(Array.tabulate(bytes.length)(bytes(_)), "UTF-8")
  private def name(value: String): ArtifactName = get(ArtifactName.of(value))
  private def parse(value: String): Json        = get(io.circe.parser.parse(value))

  /** Digests computed outside eyes4s with `shasum -a 256`. */
  private val shasum = Map(
    files.manifest        -> InputsManifestV1Fixtures.address,
    files.recordingInput  -> "9c20cc18ca05ca56da8e8c7e401e9c5f74e773920c3c4e7005dab25bfb1c4c6a",
    files.standalone      -> "7cef935f22ce42f450a14793e93ba0165d848b1d451a3aba6b2947cec38f01bb",
    files.binocularInput  -> "e1694accd76a1ce44d65eb299cfa618b11374b009a6f853e1710bff3f3aaac78",
    files.binocular       -> "fbd71faa7b90ed3fc82b85cf65561917d3b8d11e11a2915931e00c5f783a7b6b",
    files.packed          -> "92f805142ead0c4c61b6315d97922e41ab315afcb299cba4c0d07f9a82a5b187",
    payloadFiles(0)       -> "b0eb08db23502a251b675c9f0683108455c0f38c549a2c12ec249f7daf21d7b5",
    payloadFiles(1)       -> "9d92b6855fd8a4ccfd53bf41a75cb97381a420422c00ce381eb309304a533001",
    payloadFiles(2)       -> "b376730a4db09f72de102f105a7db3904683a5e3f384d047c0b3855a91d9f145",
    payloadFiles(3)       -> "4ead6bd01976f4f532fac378ff25513d11cb2258a3c2ef475086f9df5e03a795",
    files.sourceSupported -> "a4b17ec5c97a04eb4b3d1f47401b2c70cd6a32737325e474708137c4e9cea1e4",
    files.temporalBase    -> "e52de1c8b3d0d94817e0e6511a3bf2551abf17056a0877f58702e529daeba7e8",
    files.temporal        -> "bb149a480ba54c9320d767d32e4cce79e4461baa8df018b407f99b7d74676840",
    files.timeline        -> "506012173a961f8f6e8c6de1c8cae8aabb2d3fd4b28039bf36b6dd056a1a72ea",
    files.scores          -> "c2a7235b6d4c08897d1873a498b666a9c0affd07999f55fe0d22e9917a712a0f",
    files.temporalPlan    -> "861697d2f1ba961962b3ef211b0ce08d74597acc92b349f16692fd84f8b10913"
  )

  private lazy val manifest =
    get(ScientificManifest.codec.parse(text(files.manifest)))

  /** The manifest under its address and every entry under its file name. */
  private lazy val source = ByteSource.inMemory(
    Map(get(ByteDigest.parse(InputsManifestV1Fixtures.address)) -> resource(files.manifest)),
    manifest.entries.map(e => e.name -> resource(e.name.value)).toMap
  )

  test("every UI-S6 fixture and manifest-inputs-v1.json are what the writer produces") {
    val generated = get(GenerateInputsManifestV1.generated)
    assertEquals(generated.size, 11)
    generated.foreach((file, bytes) =>
      assertEquals(bytes.toVector, resource(file).toVector, file)
    )
    val written = get(GenerateInputsManifestV1.written(GenerateInputsManifestV1.resource))
    assertEquals(written.bytes.toVector, resource(files.manifest).toVector)
    assertEquals(written.manifest, manifest)
    assertEquals(written.address.hex, InputsManifestV1Fixtures.address)
    assertEquals(text(files.manifest), InputsManifestV1Fixtures.manifestInputsVersionOne)
  }

  test("every file has its independently known SHA-256, and every entry its file's length") {
    shasum.foreach((file, hex) =>
      assertEquals(ByteDigest.sha256(resource(file)).hex, hex, file)
    )
    assertEquals(manifest.entries.size, 12)
    manifest.entries.foreach { entry =>
      val file = entry.name.value
      assertEquals(entry.sha256.hex, shasum(file), file)
      assertEquals(entry.length, resource(file).length.toLong, file)
    }
  }

  test("the new JSON fixtures re-encode byte-identically and match the portable strings") {
    def check[A](file: String, compact: String, codec: VersionedCodec[A]): A =
      val value = get(codec.parse(text(file)))
      assertEquals(get(codec.encode(value)).spaces2, text(file), file)
      assertEquals(parse(text(file)), parse(compact), file)
      value
    check(
      files.standalone,
      InputsManifestV1Fixtures.standaloneRecordingVersionOne,
      RecordingInputCodecs.recording[Px]
    )
    check(
      files.binocular,
      InputsManifestV1Fixtures.binocularRecordingVersionOne,
      RecordingInputCodecs.binocular[Px]
    )
    check(
      files.timeline,
      InputsManifestV1Fixtures.timelineVersionOne,
      InputsManifestV1Fixtures.timelineCodec
    )
    val plan = check(
      files.temporalPlan,
      ConventionalPlanFixtures.temporalStudyVersionOne,
      ConventionalPlanFixtures.temporalCodec.codec
    )
    assertEquals(plan.diff(ConventionalPlanFixtures.temporalPlan), Vector.empty)
    val base = get(StudyInputCodecs.study[Px].input.parse(text(files.temporalBase)))
    assertEquals(
      get(StudyInputCodecs.study[Px].input.encode(base)).spaces2,
      text(files.temporalBase)
    )
    assertEquals(get(InputsManifestV1Fixtures.scoreDocument).spaces2, text(files.scores))
    assertEquals(
      parse(text(files.scores)),
      parse(InputsManifestV1Fixtures.scoreCodecsVersionOne)
    )
  }

  test("the packed recording decodes from its four payload files to the pinned recording") {
    val refs     = get(PackedRecordingCodecs.references(parse(text(files.packed))))
    val payloads = refs
      .zip(payloadFiles)
      .map((ref, file) => get(VerifiedPayload.verify(ref, resource(file))))
    val recording = get(
      PackedRecordingCodecs
        .recording[Px]
        .decode(parse(text(files.packed)), ref => payloads.find(_.ref == ref))
    )
    assertEquals(recording.contentHash, InputPayloadFixtures.monocular.contentHash)
    assertEquals(recording.samples.toVector, InputPayloadFixtures.monocular.samples.toVector)
    val repacked = get(PackedRecordingCodecs.recording[Px].encode(recording))
    assertEquals(repacked.document.spaces2, text(files.packed))
    assertEquals(
      repacked.payloads.map(_.bytes.toVector),
      payloadFiles.map(f => resource(f).toVector)
    )
  }

  test("all twelve entries resolve end to end from the resource files") {
    val address  = get(ByteDigest.parse(InputsManifestV1Fixtures.address))
    val resolved = get(
      ArtifactResolver
        .resolve(address, source, get(ArtifactDecoders.study[Px]))
        .left
        .map(_.toVector)
    )
    val recordingInput =
      get(resolved.recordingInput(name(files.recordingInput)).toRight("input"))
    assertEquals(recordingInput.reference, InputPayloadFixtures.input.reference)
    resolved.recording(name(files.standalone)) match
      case Some(RecordingChannels.Monocular(r)) =>
        assertEquals(r.samples.toVector, InputPayloadFixtures.monocular.samples.toVector)
      case other => fail(s"unexpected $other")
    resolved.recording(name(files.packed)) match
      case Some(RecordingChannels.Monocular(r)) =>
        assertEquals(r.samples.toVector, InputPayloadFixtures.monocular.samples.toVector)
      case other => fail(s"unexpected $other")
    resolved.recording(name(files.binocular)) match
      case Some(RecordingChannels.Binocular(b)) =>
        assertEquals(b.contentHash, InputPayloadFixtures.binocular.contentHash)
      case other => fail(s"unexpected $other")
    assertEquals(resolved.payloads.map(_._1.value), payloadFiles)
    assertEquals(
      resolved.input(name(files.sourceSupported)).map(_.reference),
      Some(InputPayloadFixtures.sourceSupportedStudy.reference)
    )
    val temporal = get(resolved.temporalInput(name(files.temporal)).toRight("temporal"))
    assertEquals(temporal.reference, InputPayloadFixtures.temporal.reference)
    assertEquals(
      resolved.input(name(files.temporalBase)).map(_.reference),
      Some(temporal.study.reference)
    )
  }
