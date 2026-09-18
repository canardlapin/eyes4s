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

import cats.syntax.all.*
import eyes4s.kernel.Unit2D.Px

import java.nio.file.{Files, Paths}

/** Repository-only writer of the UI-S6 pinned fixtures, kept in the JVM test
  * source set rather than published: the standalone, binocular and packed
  * recordings with the packed recording's four payloads, the temporal
  * fixture's base study, a timeline, the four score schemas and
  * `manifest-inputs-v1.json`, the manifest over them and the pinned S3 input
  * payloads. `InputsManifestV1JvmSuite` checks that every resource is
  * exactly what this writer produces, so the fixtures cannot drift from it.
  *
  * Entry names are the resource file names, so a directory source that
  * serves each entry under its name resolves the manifest from the resource
  * directory itself.
  *
  * {{{
  * sbt "codecJVM/Test/runMain eyes4s.codec.GenerateInputsManifestV1 <output directory>"
  * }}}
  */
private[codec] object GenerateInputsManifestV1:
  import InputsManifestV1Fixtures.{files, payloadFiles}

  def resource(name: String): Either[String, IArray[Byte]] =
    GenerateManifestV1.resource(name)

  private def message[A](value: Either[CodecError, A]): Either[String, A] =
    value.left.map(_.message)

  /** The fixtures this writer produces, by file name, in manifest order. */
  def generated: Either[String, Vector[(String, IArray[Byte])]] =
    message(
      for
        standalone <- StoredArtifact.recording(files.standalone, InputPayloadFixtures.monocular)
        binocular  <- StoredArtifact.binocular(files.binocular, InputPayloadFixtures.binocular)
        packed     <- PackedRecordingCodecs.recording[Px].encode(InputPayloadFixtures.monocular)
        document   <- StoredArtifact.document(
          files.packed,
          ArtifactRole.Recording,
          packed.document,
          Some(InputPayloadFixtures.monocular.contentHash)
        )
        payloads <- payloadFiles
          .zip(packed.payloads)
          .traverse((name, payload) => StoredArtifact.payload(name, payload))
        base <- StoredArtifact.input(
          files.temporalBase,
          StudyInputCodecs.study[Px],
          InputPayloadFixtures.temporal.study
        )
        timeline <- InputsManifestV1Fixtures.timelineCodec
          .encode(InputsManifestV1Fixtures.timeline)
          .flatMap(json => Documents.utf8(json.spaces2))
        scores <- InputsManifestV1Fixtures.scoreDocument.flatMap(json =>
          Documents.utf8(json.spaces2)
        )
      yield Vector(
        files.standalone   -> standalone.bytes,
        files.binocular    -> binocular.bytes,
        files.packed       -> document.bytes,
        files.temporalBase -> base.bytes,
        files.timeline     -> timeline,
        files.scores       -> scores
      ) ++ payloads.map(p => p.name.value -> p.bytes)
    )

  /** The manifest over the pinned files as `read` supplies them. */
  def written(read: String => Either[String, IArray[Byte]]): Either[String, SavedManifest] =
    def stored(name: String, role: ArtifactRole, identity: Option[eyes4s.kernel.ContentHash]) =
      read(name).flatMap(bytes => message(StoredArtifact.bytes(name, role, bytes, identity)))
    for
      recordingInput <- stored(
        files.recordingInput,
        ArtifactRole.RecordingInput,
        Some(InputPayloadFixtures.input.hash)
      )
      standalone <- stored(
        files.standalone,
        ArtifactRole.Recording,
        Some(InputPayloadFixtures.monocular.contentHash)
      )
      binocularInput <- stored(
        files.binocularInput,
        ArtifactRole.RecordingInput,
        Some(InputPayloadFixtures.binocularInput.hash)
      )
      binocular <- stored(
        files.binocular,
        ArtifactRole.Recording,
        Some(InputPayloadFixtures.binocular.contentHash)
      )
      packed <- stored(
        files.packed,
        ArtifactRole.Recording,
        Some(InputPayloadFixtures.monocular.contentHash)
      )
      encoded <- message(
        PackedRecordingCodecs.recording[Px].encode(InputPayloadFixtures.monocular)
      )
      payloads <- payloadFiles.zip(encoded.payloads).traverse { (name, expected) =>
        read(name)
          .flatMap(bytes => VerifiedPayload.verify(expected.ref, bytes).left.map(_.message))
          .flatMap(verified => message(StoredArtifact.payload(name, verified)))
      }
      sourceSupported <- stored(
        files.sourceSupported,
        ArtifactRole.StudyInput,
        Some(InputPayloadFixtures.sourceSupportedStudy.hash)
      )
      temporalBase <- stored(
        files.temporalBase,
        ArtifactRole.StudyInput,
        Some(InputPayloadFixtures.temporal.study.hash)
      )
      temporal <- stored(
        files.temporal,
        ArtifactRole.TemporalInput,
        Some(InputPayloadFixtures.temporal.hash)
      )
      saved <- message(
        SavedManifest.of(
          Vector(
            recordingInput,
            standalone,
            binocularInput,
            binocular,
            packed
          ) ++ payloads ++ Vector(sourceSupported, temporalBase, temporal),
          Vector(
            ManifestRelation.RecordingOf(recordingInput.name, standalone.name),
            ManifestRelation.RecordingOf(binocularInput.name, binocular.name)
          ) ++ payloads.map(p => ManifestRelation.PayloadOf(packed.name, p.name)) :+
            ManifestRelation.TemporalBase(temporal.name, temporalBase.name)
        )
      )
    yield saved

  def main(arguments: Array[String]): Unit =
    (for
      directory <- arguments.headOption.toRight(
        "usage: GenerateInputsManifestV1 <output directory>"
      )
      fixtures <- generated
      table = fixtures.toMap
      saved <- written(name => table.get(name).fold(resource(name))(Right(_)))
    yield
      val out = Paths.get(directory)
      (fixtures :+ (files.manifest -> saved.bytes)).foreach { (name, bytes) =>
        Files.write(out.resolve(name), Array.tabulate(bytes.length)(bytes(_)))
      }
      saved.address.hex
    ) match
      case Left(message) =>
        Console.err.println(s"manifest-inputs-v1 not written: $message")
        System.exit(2)
      case Right(address) => println(s"wrote manifest-inputs-v1 with address $address")
