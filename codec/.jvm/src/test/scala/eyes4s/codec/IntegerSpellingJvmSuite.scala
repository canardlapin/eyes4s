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

import eyes4s.plan.DefinitionId

/** An integer member is spelled as an integer. The JVM parser keeps a
  * number's spelling; the Scala.js parser (the platform's `JSON.parse`) does
  * not, so `3.0` reads as `3` there and this check is JVM-only.
  */
class IntegerSpellingJvmSuite extends munit.FunSuite:
  test("an integer member spelled 3.0, 3e0 or -0 is refused") {
    val schema = DefinitionId.of("test.count", 1).toOption.get
    val codec  = VersionedCodec.of[Int](schema)(io.circe.Json.fromInt)(json =>
      Wire.field[Int](io.circe.Json.obj("n" -> json), "n")
    )
    def document(spelling: String) =
      s"""{"schema":{"name":"test.count","version":1},"value":$spelling}"""
    assertEquals(codec.parse(document("3")), Right(3))
    Vector("3.0", "3e0", "30e-1", "-0").foreach { spelling =>
      assert(
        codec.parse(document(spelling)).left.exists {
          case CodecError.Field("n", _, reason) => reason.contains("integer spelled as one")
          case _                                => false
        },
        spelling
      )
    }
  }
