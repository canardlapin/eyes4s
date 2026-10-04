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

import eyes4s.codec.ReportRefCodecs.given
import eyes4s.results.*
import io.circe.Json
import io.circe.syntax.*

/** The pinned report-reference document: Studio's trail "Summary · by
  * retrieval response › Remembered › P17" as a cell and a participant.
  * `versionOne` is the text of `codec/src/test/resources/values/report-ref-v1.json`,
  * portable to Scala.js; the JVM checks the file against it byte for byte.
  */
object ReportRefFixtures:
  val file: String = "report-ref-v1.json"

  val cell: ReportRef.Cell = ReportRef
    .cell(0, GroupKey(Vector("covariate:response" -> "Remembered")), Role.Difference, "value")
    .toOption
    .get
  val participant: ReportRef.Participant = ReportRef.participant(cell, "P17").toOption.get

  def document: Json = Json.obj(
    "cell"        -> (cell: ReportRef).asJson,
    "participant" -> (participant: ReportRef).asJson
  )

  val versionOne: String = Vector(
    "{",
    """  "cell" : {""",
    """    "cell" : {""",
    """      "scale" : 0,""",
    """      "group" : [""",
    "        {",
    """          "term" : "covariate:response",""",
    """          "level" : "Remembered"""",
    "        }",
    "      ],",
    """      "role" : "difference",""",
    """      "component" : "value"""",
    "    }",
    "  },",
    """  "participant" : {""",
    """    "participant" : {""",
    """      "cell" : {""",
    """        "scale" : 0,""",
    """        "group" : [""",
    "          {",
    """            "term" : "covariate:response",""",
    """            "level" : "Remembered"""",
    "          }",
    "        ],",
    """        "role" : "difference",""",
    """        "component" : "value"""",
    "      },",
    """      "name" : "P17"""",
    "    }",
    "  }",
    "}"
  ).mkString("\n")
