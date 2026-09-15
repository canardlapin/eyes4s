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

package eyes4s.laws

import eyes4s.codec.*
import eyes4s.plan.*
import org.scalacheck.Gen

class CodecLawsSuite extends munit.DisciplineSuite:
  private def id(name: String): DefinitionId = DefinitionId.of(name, 1).toOption.get
  private val string                         = VersionedCodec.string(id("string"))
  private val key                            = StudyCodecs.key(id("study-key"))
  private val keys                           = for
    participant <- Gen.alphaStr
    stimulus    <- Gen.alphaStr
    phase       <- Gen.alphaStr
  yield StudyKey(participant, stimulus, phase)

  checkAll(
    "string",
    CodecLaws.roundTrip(string, Gen.alphaStr, (a: String, b: String) => a == b)
  )
  checkAll("key", CodecLaws.roundTrip(key, keys, (a: StudyKey, b: StudyKey) => a == b))
  checkAll(
    "conditional entries",
    CodecLaws.roundTrip(
      VersionedCodec.entries(id("map"), key, string),
      Gen.mapOf(Gen.zip(keys, Gen.alphaStr)),
      (a: Map[StudyKey, String], b: Map[StudyKey, String]) => a == b
    )
  )
