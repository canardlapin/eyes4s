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
import io.circe.Json

/** The pinned recording and temporal result archives on the JVM: the
  * resources are exactly the writer's output with independently known
  * digests, re-encode byte for byte, match the portable mirrors as JSON
  * values, and are reproduced byte for byte by re-executing the pinned plans
  * on the pinned inputs.
  */
class ResultArchivesV1JvmSuite extends munit.FunSuite:
  import GenerateResultArchivesV1.{recordingFile, temporalFile}

  private def get[E, A](e: Either[E, A]): A     = e.fold(error => fail(s"$error"), identity)
  private def bytes(name: String): Vector[Byte] =
    get(GenerateManifestV1.resource(name)).toVector
  private def text(name: String): String = String(bytes(name).toArray, "UTF-8")
  private def parse(value: String): Json = get(io.circe.parser.parse(value))

  /** Digests computed outside eyes4s with `shasum -a 256`. */
  private val shasum = Map(
    recordingFile -> "2727f9d196460ef1dd6f4a6753b85d093273f1d4baff5a4707959c89551ba585",
    temporalFile  -> "742c22bc8081921d711534a8234209d814e1df8a2e2c0637d8e41270255a11ca"
  )

  test("both archives are exactly the writer's output, with independently known digests") {
    val generated = get(GenerateResultArchivesV1.generated)
    assertEquals(generated.map(_._1), Vector(recordingFile, temporalFile))
    generated.foreach((file, written) => assertEquals(written.toVector, bytes(file), file))
    shasum.foreach((file, hex) =>
      assertEquals(ByteDigest.sha256(IArray.from(bytes(file))).hex, hex, file)
    )
  }

  test("both archives re-encode byte-identically and match the portable mirrors") {
    val recording = get(ArchiveFixtures.recordingResults.codec.parse(text(recordingFile)))
    assertEquals(
      get(ArchiveFixtures.recordingResults.codec.encode(recording)).spaces2,
      text(recordingFile)
    )
    assertEquals(
      parse(text(recordingFile)),
      parse(ResultArchiveMirrors.recordingResultVersionOne)
    )
    val temporal = get(ArchiveFixtures.temporalResults.codec.parse(text(temporalFile)))
    assertEquals(
      get(ArchiveFixtures.temporalResults.codec.encode(temporal)).spaces2,
      text(temporalFile)
    )
    assertEquals(
      parse(text(temporalFile)),
      parse(ResultArchiveMirrors.temporalResultVersionOne)
    )
  }

  test(
    "re-executing the pinned plans on the pinned inputs reproduces both archives byte for byte"
  ) {
    val recordingInput = get(
      RecordingInputCodecs.input[Px].parse(text("recording-input-v1.json"))
    )
    val plan  = get(ArchiveFixtures.recordingPlanFor(recordingInput))
    val rerun = get(plan.run(get(recordingInput.monocular.toRight("monocular"))))
    assertEquals(
      get(ArchiveFixtures.recordingResults.codec.encode(rerun)).spaces2,
      text(recordingFile)
    )
    val temporalInput = get(
      TemporalInputCodecs.study[Px]().input.parse(text("temporal-study-input-v1.json"))
    )
    val temporalPlan =
      get(ArchiveFixtures.temporalCodec.codec.parse(text("temporal-study-v1.json")))
    val rerunTemporal = get(temporalPlan.run(temporalInput))
    assertEquals(
      get(ArchiveFixtures.temporalResults.codec.encode(rerunTemporal)).spaces2,
      text(temporalFile)
    )
  }
