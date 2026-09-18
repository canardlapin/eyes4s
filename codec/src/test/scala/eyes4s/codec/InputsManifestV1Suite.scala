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

import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import io.circe.Json

/** The frozen manifest-inputs v1 and the UI-S6 fixtures on both platforms.
  * The manifest carries no floating-point number, so its canonical bytes and
  * address are the same on the JVM and Scala.js, and so are the packed
  * payloads: both platforms pack the pinned recording to the frozen digests.
  * The JSON fixtures are compared as JSON values, which Scala.js promises;
  * the JVM suite additionally checks their bytes.
  */
class InputsManifestV1Suite extends munit.FunSuite:
  import InputsManifestV1Fixtures.*

  private def get[E, A](e: Either[E, A]): A     = e.fold(error => fail(s"$error"), identity)
  private def name(value: String): ArtifactName = get(ArtifactName.of(value))
  private def parse(value: String): Json        = get(io.circe.parser.parse(value))
  private def utf8(text: String): IArray[Byte]  = get(Utf8.encode(text).left.map(i => s"at $i"))
  private def frozen: ScientificManifest        =
    get(ScientificManifest.codec.parse(manifestInputsVersionOne))

  test(
    "frozen manifest-inputs v1 fixes its entries, roles, schemas, identities and relations"
  ) {
    val manifest = frozen
    assertEquals(
      manifest.entries.map(e => (e.name.value, e.role, e.schema, e.identity.map(_.render))),
      Vector(
        (
          files.recordingInput,
          ArtifactRole.RecordingInput,
          DefinitionId.recordingInput,
          Some("d4c0d76ef6fd5169")
        ),
        (
          files.standalone,
          ArtifactRole.Recording,
          DefinitionId.recording,
          Some("2c826dc41ae25e67")
        ),
        (
          files.binocularInput,
          ArtifactRole.RecordingInput,
          DefinitionId.recordingInput,
          Some("c5e4009b9cb153b0")
        ),
        (
          files.binocular,
          ArtifactRole.Recording,
          DefinitionId.binocularRecording,
          Some("39e3dafb2de70ca2")
        ),
        (
          files.packed,
          ArtifactRole.Recording,
          DefinitionId.packedRecording,
          Some("2c826dc41ae25e67")
        )
      ) ++ payloadFiles.map(f => (f, ArtifactRole.Payload, DefinitionId.packedArray, None)) ++
        Vector(
          (
            files.sourceSupported,
            ArtifactRole.StudyInput,
            DefinitionId.studyInput,
            Some("9ac7e83e4874dec7")
          ),
          (
            files.temporalBase,
            ArtifactRole.StudyInput,
            DefinitionId.studyInput,
            Some("4fd98f50693c79ad")
          ),
          (
            files.temporal,
            ArtifactRole.TemporalInput,
            DefinitionId.temporalStudyInput,
            Some("def40de68e529acf")
          )
        )
    )
    assertEquals(
      manifest.entries.flatMap(_.layout).map(l => (l.element, l.shape, l.order)),
      Vector(
        (ElementKind.Int64, Vector(8), ArrayOrder.RowMajor),
        (ElementKind.UInt8, Vector(8), ArrayOrder.RowMajor),
        (ElementKind.UInt8, Vector(8), ArrayOrder.RowMajor),
        (ElementKind.Float64, Vector(8, 3), ArrayOrder.ColumnMajor)
      )
    )
    assertEquals(
      manifest.relations,
      Vector(
        ManifestRelation.RecordingOf(name(files.recordingInput), name(files.standalone)),
        ManifestRelation.RecordingOf(name(files.binocularInput), name(files.binocular))
      ) ++ payloadFiles.map(f => ManifestRelation.PayloadOf(name(files.packed), name(f))) :+
        ManifestRelation.TemporalBase(name(files.temporal), name(files.temporalBase))
    )
  }

  test("frozen manifest-inputs v1 re-encodes byte-identically and keeps its address") {
    val bytes = get(ScientificManifest.bytes(frozen))
    assertEquals(bytes.toVector, utf8(manifestInputsVersionOne).toVector)
    assertEquals(get(ScientificManifest.address(frozen)).hex, address)
    assertEquals(ByteDigest.sha256(bytes).hex, address)
  }

  test("both platforms pack the pinned recording to the frozen payload digests and layouts") {
    val packed = get(PackedRecordingCodecs.recording[Px].encode(InputPayloadFixtures.monocular))
    val pinned = frozen.entries.filter(_.role == ArtifactRole.Payload)
    assertEquals(packed.payloads.map(_.ref.sha256), pinned.map(_.sha256))
    assertEquals(packed.payloads.map(p => Some(p.ref.layout)), pinned.map(_.layout))
    assertEquals(packed.payloads.map(_.bytes.length.toLong), pinned.map(_.length))
    assertEquals(packed.document, parse(packedRecordingVersionOne))
    // The float64 column holds exact IEEE bits: x, then y, then pupil, +0.0 where absent.
    val values = get(PackedArrays.unpack[Double](packed.payloads(3)))
    assertEquals(
      values.toVector.take(8).map(java.lang.Double.doubleToRawLongBits),
      Vector(500.0, 510.25, 0.0, 0.0, 515.0, 1200.0, 520.0, 525.0)
        .map(java.lang.Double.doubleToRawLongBits)
    )
    val decoded = get(
      PackedRecordingCodecs
        .recording[Px]
        .decode(parse(packedRecordingVersionOne), ref => packed.payloads.find(_.ref == ref))
    )
    assertEquals(decoded.samples.toVector, InputPayloadFixtures.monocular.samples.toVector)
  }

  test(
    "the standalone and binocular recordings decode to their typed sources on both platforms"
  ) {
    val standalone =
      get(RecordingInputCodecs.recording[Px].parse(standaloneRecordingVersionOne))
    assertEquals(standalone.contentHash.render, "2c826dc41ae25e67")
    assertEquals(standalone.samples.toVector, InputPayloadFixtures.monocular.samples.toVector)
    assertEquals(
      get(RecordingInputCodecs.recording[Px].encode(standalone)),
      parse(standaloneRecordingVersionOne)
    )
    val binocular = get(RecordingInputCodecs.binocular[Px].parse(binocularRecordingVersionOne))
    assertEquals(binocular.contentHash.render, "39e3dafb2de70ca2")
    assertEquals(
      binocular.rightGaze.toVector,
      InputPayloadFixtures.binocular.rightGaze.toVector
    )
    assertEquals(
      get(RecordingInputCodecs.binocular[Px].encode(binocular)),
      parse(binocularRecordingVersionOne)
    )
  }

  test("the timeline keeps equal instants beyond 2^53 in input order on both platforms") {
    val line = get(timelineCodec.parse(timelineVersionOne))
    assertEquals(line.clock, ClockId("presentation-display"))
    assertEquals(
      line.marks.map(m => (m.at.toMicros, m.value)),
      Vector(
        (-1500000L, StudyKey("s2", "c", "retest")),
        (0L, StudyKey("s1", "a", "encode")),
        (9007199254740993L, StudyKey("s1", "b", "encode")),
        (9007199254740993L, StudyKey("s1", "a", "recall"))
      )
    )
    assertEquals(line, timeline)
    assertEquals(get(timelineCodec.encode(line)), parse(timelineVersionOne))
  }

  test(
    "the four score schemas decode their pinned values to the exact bits on both platforms"
  ) {
    val envelopes            = get(parse(scoreCodecsVersionOne).asArray.toRight("array"))
    val codecs               = StudyResultCodecs
    val bits: Double => Long = java.lang.Double.doubleToRawLongBits
    assertEquals(
      get(codecs.similarity().decode(envelopes(0))).value,
      similarity.value
    )
    assertEquals(bits(get(codecs.similarity().decode(envelopes(0))).value), bits(0.1))
    assertEquals(bits(get(codecs.measureDistance().decode(envelopes(1))).value), bits(2.5))
    assertEquals(bits(get(codecs.scalar().decode(envelopes(2)))), bits(-0.3))
    assertEquals(
      bits(get(codecs.signedDifference().decode(envelopes(3))).value),
      bits(0.1 - 0.35)
    )
    assertEquals(get(scoreDocument), parse(scoreCodecsVersionOne))
  }

  test("temporal-base v1 is the temporal fixture's inline base study") {
    val inline = parse(InputPayloadFixtures.temporalInputVersionOne).hcursor
      .downField("value")
      .downField("study")
      .downField("payload")
      .focus
    assertEquals(
      Some(get(StudyInputCodecs.study[Px].input.encode(InputPayloadFixtures.temporal.study))),
      inline
    )
  }

  test("the same graph over the portable strings resolves end to end on both platforms") {
    val inputs = StudyInputCodecs.study[Px]
    val packed = get(PackedRecordingCodecs.recording[Px].encode(InputPayloadFixtures.monocular))
    val saved  = get(
      for
        recordingInput <- StoredArtifact.bytes(
          files.recordingInput,
          ArtifactRole.RecordingInput,
          utf8(InputPayloadFixtures.recordingInputVersionOne),
          Some(InputPayloadFixtures.input.hash)
        )
        standalone <- StoredArtifact.bytes(
          files.standalone,
          ArtifactRole.Recording,
          utf8(standaloneRecordingVersionOne),
          Some(InputPayloadFixtures.monocular.contentHash)
        )
        binocularInput <- StoredArtifact.bytes(
          files.binocularInput,
          ArtifactRole.RecordingInput,
          utf8(InputPayloadFixtures.binocularInputVersionOne),
          Some(InputPayloadFixtures.binocularInput.hash)
        )
        binocular <- StoredArtifact.bytes(
          files.binocular,
          ArtifactRole.Recording,
          utf8(binocularRecordingVersionOne),
          Some(InputPayloadFixtures.binocular.contentHash)
        )
        document <- StoredArtifact.bytes(
          files.packed,
          ArtifactRole.Recording,
          utf8(packedRecordingVersionOne),
          Some(InputPayloadFixtures.monocular.contentHash)
        )
        payloads <- payloadFiles
          .zip(packed.payloads)
          .foldLeft(
            Right(Vector.empty): Either[CodecError, Vector[StoredArtifact]]
          ) { case (acc, (file, payload)) =>
            acc.flatMap(done => StoredArtifact.payload(file, payload).map(done :+ _))
          }
        sourceSupported <- StoredArtifact.bytes(
          files.sourceSupported,
          ArtifactRole.StudyInput,
          utf8(InputPayloadFixtures.sourceSupportedVersionOne),
          Some(InputPayloadFixtures.sourceSupportedStudy.hash)
        )
        base <- StoredArtifact.input(
          files.temporalBase,
          inputs,
          InputPayloadFixtures.temporal.study
        )
        temporal <- StoredArtifact.bytes(
          files.temporal,
          ArtifactRole.TemporalInput,
          utf8(InputPayloadFixtures.temporalInputVersionOne),
          Some(InputPayloadFixtures.temporal.hash)
        )
        saved <- SavedManifest.of(
          Vector(recordingInput, standalone, binocularInput, binocular, document) ++ payloads ++
            Vector(sourceSupported, base, temporal),
          frozen.relations
        )
      yield saved
    )
    // Compact strings are other bytes than the pretty resource files, with the
    // same identities; the payloads are the same bytes on every platform.
    assertEquals(saved.manifest.entries.map(_.identity), frozen.entries.map(_.identity))
    assertEquals(
      saved.manifest.entries.filter(_.role == ArtifactRole.Payload).map(_.sha256),
      frozen.entries.filter(_.role == ArtifactRole.Payload).map(_.sha256)
    )
    val resolved = get(
      ArtifactResolver
        .resolve(saved.address, saved.source, get(ArtifactDecoders.study[Px]))
        .left
        .map(_.toVector)
    )
    resolved.recording(name(files.packed)) match
      case Some(RecordingChannels.Monocular(r)) =>
        assertEquals(r.contentHash, InputPayloadFixtures.monocular.contentHash)
      case other => fail(s"unexpected $other")
    assertEquals(
      resolved.temporalInput(name(files.temporal)).map(_.reference),
      Some(InputPayloadFixtures.temporal.reference)
    )
  }
