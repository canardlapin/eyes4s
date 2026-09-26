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
import io.circe.Json
import io.circe.syntax.*

/** The pinned identity document: Studio's focus fixation, fixation 6 of its
  * trial (scanpath position 5), supplied by fixations.csv data record 7,214.
  * `versionOne` is the text of `codec/src/test/resources/values/record-identity-v1.json`,
  * portable to Scala.js; the JVM checks the file against it byte for byte.
  */
object RecordIdentityFixtures:
  val file: String = "record-identity-v1.json"

  val record: DataRecord         = DataRecord.of(7214).toOption.get
  val number: FixationNumber     = FixationNumber.of(6).toOption.get
  val position: ScanpathPosition = ScanpathPosition.of(5).toOption.get

  def document: Json = Json.obj(
    "dataRecord"       -> record.asJson,
    "fixationNumber"   -> number.asJson,
    "scanpathPosition" -> position.asJson
  )

  val versionOne: String = Vector(
    "{",
    """  "dataRecord" : {""",
    """    "data-record" : 7214""",
    "  },",
    """  "fixationNumber" : {""",
    """    "fixation-number" : 6""",
    "  },",
    """  "scanpathPosition" : {""",
    """    "scanpath-position" : 5""",
    "  }",
    "}"
  ).mkString("\n")
