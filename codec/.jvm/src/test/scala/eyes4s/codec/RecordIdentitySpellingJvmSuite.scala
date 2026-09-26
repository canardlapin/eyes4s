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

import eyes4s.codec.RecordIdentityCodecs.given
import eyes4s.plan.*
import io.circe.parser.parse

/** On the JVM the parser keeps a number's spelling, so an identity written as
  * `7214.0` or `7.214e3` is refused: an integer is spelled as an integer.
  */
class RecordIdentitySpellingJvmSuite extends munit.FunSuite:
  private def decoded(text: String) =
    parse(text).fold(e => fail(e.message), identity).as[DataRecord]

  test("an identity not spelled as an integer is refused, naming what it found") {
    Vector("7214.0", "7.214e3", "7214e0", "7214.5").foreach { number =>
      val result = decoded(s"""{"data-record":$number}""")
      assert(
        result.left.exists(
          _.message.contains(s"expected an integer spelled as one, got $number")
        ),
        s"$number: $result"
      )
    }
    assertEquals(decoded("""{"data-record":7214}"""), Right(DataRecord.of(7214).toOption.get))
    assert(
      parse("""{"fixation-number":6.0}""").toOption.get.as[FixationNumber].isLeft,
      "6.0 decoded as a fixation number"
    )
    assert(
      parse("""{"scanpath-position":5e0}""").toOption.get.as[ScanpathPosition].isLeft,
      "5e0 decoded as a scanpath position"
    )
  }
