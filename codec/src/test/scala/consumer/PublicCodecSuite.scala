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

package consumer

import eyes4s.codec.*
import eyes4s.plan.*
import io.circe.Json

class PublicCodecSuite extends munit.FunSuite:
  test(
    "byte digests render the independent empty SHA256 and custom registrations declare absence"
  ) {
    assertEquals(
      ByteDigest.sha256(IArray.empty[Byte]).toString,
      "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    )
    assertEquals(StudyResultCodecs.schema, DefinitionId.studyResult)
    val registration = new RecordingRegistration:
      val methodId = DefinitionId.of("consumer.unavailable", 1).toOption.get
      def decode(json: Json): Either[CodecError, LoadedRecording] =
        Left(CodecError.Field("registration", json, "unavailable"))
    assertEquals(registration.schema, None)
    assert(registration.decode(Json.Null).isLeft)
  }
