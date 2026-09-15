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

/** Pinned first study schema; matches src/test/resources/eyes4s/study-v1.json. */
object SavedStudyFixtures:
  val versionOne = """{
  "schema" : {
    "name" : "eyes4s.study",
    "version" : 1
  },
  "value" : {
    "layout" : {
      "name" : "eyes4s.participant-stimulus-phase",
      "version" : 1
    },
    "keySchema" : {
      "name" : "eyes4s.study-key",
      "version" : 1
    },
    "method" : {
      "name" : "eyes4s.cosine",
      "version" : 1
    },
    "input" : "cebe7474ab5c2aec",
    "frame" : {
      "id" : "matched-control-display",
      "unit" : "px",
      "xMin" : 0.0,
      "yMin" : 0.0,
      "xMax" : 2.0,
      "yMax" : 2.0,
      "yAxis" : "Down"
    },
    "gridId" : "matched-control-display@2x2",
    "nx" : 2,
    "ny" : 2,
    "focalPhase" : "recall",
    "referencePhase" : "encode",
    "weight" : "Duration",
    "policy" : {
      "kind" : "requireAll"
    },
    "estimates" : [
      {
        "kind" : "binned"
      }
    ],
    "parameters" : {
      "schema" : {
        "name" : "eyes4s.unit",
        "version" : 1
      },
      "value" : {

      }
    }
  }
}"""
