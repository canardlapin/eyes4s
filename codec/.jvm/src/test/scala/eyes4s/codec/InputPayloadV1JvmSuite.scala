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

/** The pretty-printed recording and temporal resource files re-encode byte
  * for byte on the JVM and agree with the portable compact strings. Scala.js
  * renders doubles differently, so the portable suite checks JSON value
  * identity instead.
  */
class InputPayloadV1JvmSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A  = e.fold(error => fail(s"$error"), identity)
  private def resource(name: String): String =
    val stream = Option(getClass.getResourceAsStream(s"/eyes4s/$name"))
      .getOrElse(fail(s"missing resource $name"))
    try String(stream.readAllBytes(), "UTF-8")
    finally stream.close()

  private def check[A](
      name: String,
      compact: String,
      codec: VersionedCodec[A]
  ): Unit =
    val text  = resource(name)
    val value = get(codec.parse(text))
    assertEquals(get(codec.encode(value)).spaces2, text)
    assertEquals(
      get(io.circe.parser.parse(text)),
      get(io.circe.parser.parse(compact))
    )

  test("recording-input-v1.json re-encodes byte-identically and matches the portable string") {
    check(
      "recording-input-v1.json",
      InputPayloadFixtures.recordingInputVersionOne,
      RecordingInputCodecs.input[Px]
    )
  }

  test("binocular-recording-input-v1.json re-encodes byte-identically") {
    check(
      "binocular-recording-input-v1.json",
      InputPayloadFixtures.binocularInputVersionOne,
      RecordingInputCodecs.input[Px]
    )
  }

  test("study-input-source-supported-v1.json re-encodes byte-identically") {
    check(
      "study-input-source-supported-v1.json",
      InputPayloadFixtures.sourceSupportedVersionOne,
      StudyInputCodecs.study[Px].input
    )
  }

  test("temporal-study-input-v1.json re-encodes byte-identically") {
    check(
      "temporal-study-input-v1.json",
      InputPayloadFixtures.temporalInputVersionOne,
      TemporalInputCodecs.study[Px]().input
    )
  }
