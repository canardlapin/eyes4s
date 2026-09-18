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

import eyes4s.compare.{MeasureDistance, Similarity}
import eyes4s.design.SignedDifference
import eyes4s.kernel.*
import eyes4s.plan.*
import io.circe.Json

/** The UI-S6 pinned fixtures over the recording and temporal inputs.
  *
  * `manifest-inputs-v1.json` lists the pinned S3 input payloads beside the
  * recordings their channels are (a standalone `recording@1`, a
  * `binocular-recording@1` and a `packed-recording@1` with its four
  * `packed-array@1` payloads) and the temporal fixture's base study, with
  * `recording-of`, `payload-of` and `temporal-base` relations. The timeline and
  * score fixtures pin the two remaining built-in schemas that no artifact
  * role carries. The manifest has no floating-point number, so its text and
  * address are the same on the JVM and Scala.js; the portable strings below
  * mirror the pretty-printed resource files as JSON values.
  */
object InputsManifestV1Fixtures:
  private def get[E, A](value: Either[E, A]): A =
    value.fold(e => throw new IllegalStateException(s"$e"), identity)

  /** Resource file names, which are also the manifest's entry names. */
  object files:
    val manifest        = "manifest-inputs-v1.json"
    val recordingInput  = "recording-input-v1.json"
    val standalone      = "recording-standalone-v1.json"
    val binocularInput  = "binocular-recording-input-v1.json"
    val binocular       = "binocular-recording-v1.json"
    val packed          = "packed-recording-v1.json"
    val sourceSupported = "study-input-source-supported-v1.json"
    val temporalBase    = "temporal-base-v1.json"
    val temporal        = "temporal-study-input-v1.json"
    val timeline        = "timeline-v1.json"
    val scores          = "score-codecs-v1.json"

  /** The packed recording's payload files in its declared column order. */
  val payloadFiles: Vector[String] =
    Vector("tMicros", "support", "lineage", "values").map(c => s"packed-recording-v1.$c.bin")

  val timelineCodec: VersionedCodec[Timeline[StudyKey]] =
    TimelineCodecs.timeline(DefinitionId.timeline, StudyCodecs.key(DefinitionId.studyKey))

  /** Trial presentations on one display clock: a negative instant, two equal
    * instants beyond JavaScript's exact integer range (kept in input order)
    * and the origin.
    */
  val timeline: Timeline[StudyKey] = get(
    Timeline.of(
      ClockId("presentation-display"),
      Vector(
        Mark(Instant.micros(0L), StudyKey("s1", "a", "encode")),
        Mark(Instant.micros(9007199254740993L), StudyKey("s1", "b", "encode")),
        Mark(Instant.micros(9007199254740993L), StudyKey("s1", "a", "recall")),
        Mark(Instant.micros(-1500000L), StudyKey("s2", "c", "retest"))
      )
    )
  )

  val similarity: Similarity                  = get(Similarity.of(0.1))
  val distance: MeasureDistance               = get(MeasureDistance.of(2.5))
  val scalar: Double                          = -0.3
  val difference: SignedDifference            = get(SignedDifference.between(0.1, 0.35))
  private val codecs                          = StudyResultCodecs
  def scoreDocument: Either[CodecError, Json] = for
    s <- codecs.similarity().encode(similarity)
    d <- codecs.measureDistance().encode(distance)
    c <- codecs.scalar().encode(scalar)
    g <- codecs.signedDifference().encode(difference)
  yield Json.arr(s, d, c, g)

  /** The SHA-256 of [[manifestInputsVersionOne]], computed with `shasum -a 256`. */
  val address: String = "540915965fd389ea6630a88d5f07f66af6bfecf7e542e4293b4f4560f6fbcebf"

  val manifestInputsVersionOne: String = """{
  "schema" : {
    "name" : "eyes4s.manifest",
    "version" : 1
  },
  "value" : {
    "entries" : [
      {
        "name" : "recording-input-v1.json",
        "role" : "recording-input",
        "schema" : {
          "name" : "eyes4s.recording-input",
          "version" : 1
        },
        "media" : "application/json",
        "length" : "2993",
        "sha256" : "9c20cc18ca05ca56da8e8c7e401e9c5f74e773920c3c4e7005dab25bfb1c4c6a",
        "identity" : "d4c0d76ef6fd5169",
        "layout" : null
      },
      {
        "name" : "recording-standalone-v1.json",
        "role" : "recording",
        "schema" : {
          "name" : "eyes4s.recording",
          "version" : 1
        },
        "media" : "application/json",
        "length" : "1846",
        "sha256" : "7cef935f22ce42f450a14793e93ba0165d848b1d451a3aba6b2947cec38f01bb",
        "identity" : "2c826dc41ae25e67",
        "layout" : null
      },
      {
        "name" : "binocular-recording-input-v1.json",
        "role" : "recording-input",
        "schema" : {
          "name" : "eyes4s.recording-input",
          "version" : 1
        },
        "media" : "application/json",
        "length" : "1742",
        "sha256" : "e1694accd76a1ce44d65eb299cfa618b11374b009a6f853e1710bff3f3aaac78",
        "identity" : "c5e4009b9cb153b0",
        "layout" : null
      },
      {
        "name" : "binocular-recording-v1.json",
        "role" : "recording",
        "schema" : {
          "name" : "eyes4s.binocular-recording",
          "version" : 1
        },
        "media" : "application/json",
        "length" : "1430",
        "sha256" : "fbd71faa7b90ed3fc82b85cf65561917d3b8d11e11a2915931e00c5f783a7b6b",
        "identity" : "39e3dafb2de70ca2",
        "layout" : null
      },
      {
        "name" : "packed-recording-v1.json",
        "role" : "recording",
        "schema" : {
          "name" : "eyes4s.packed-recording",
          "version" : 1
        },
        "media" : "application/json",
        "length" : "2221",
        "sha256" : "92f805142ead0c4c61b6315d97922e41ab315afcb299cba4c0d07f9a82a5b187",
        "identity" : "2c826dc41ae25e67",
        "layout" : null
      },
      {
        "name" : "packed-recording-v1.tMicros.bin",
        "role" : "payload",
        "schema" : {
          "name" : "eyes4s.packed-array",
          "version" : 1
        },
        "media" : "application/octet-stream",
        "length" : "64",
        "sha256" : "b0eb08db23502a251b675c9f0683108455c0f38c549a2c12ec249f7daf21d7b5",
        "identity" : null,
        "layout" : {
          "element" : "int64",
          "shape" : [
            8
          ],
          "order" : "row-major",
          "byteOrder" : "little-endian"
        }
      },
      {
        "name" : "packed-recording-v1.support.bin",
        "role" : "payload",
        "schema" : {
          "name" : "eyes4s.packed-array",
          "version" : 1
        },
        "media" : "application/octet-stream",
        "length" : "8",
        "sha256" : "9d92b6855fd8a4ccfd53bf41a75cb97381a420422c00ce381eb309304a533001",
        "identity" : null,
        "layout" : {
          "element" : "uint8",
          "shape" : [
            8
          ],
          "order" : "row-major",
          "byteOrder" : "little-endian"
        }
      },
      {
        "name" : "packed-recording-v1.lineage.bin",
        "role" : "payload",
        "schema" : {
          "name" : "eyes4s.packed-array",
          "version" : 1
        },
        "media" : "application/octet-stream",
        "length" : "8",
        "sha256" : "b376730a4db09f72de102f105a7db3904683a5e3f384d047c0b3855a91d9f145",
        "identity" : null,
        "layout" : {
          "element" : "uint8",
          "shape" : [
            8
          ],
          "order" : "row-major",
          "byteOrder" : "little-endian"
        }
      },
      {
        "name" : "packed-recording-v1.values.bin",
        "role" : "payload",
        "schema" : {
          "name" : "eyes4s.packed-array",
          "version" : 1
        },
        "media" : "application/octet-stream",
        "length" : "192",
        "sha256" : "4ead6bd01976f4f532fac378ff25513d11cb2258a3c2ef475086f9df5e03a795",
        "identity" : null,
        "layout" : {
          "element" : "float64",
          "shape" : [
            8,
            3
          ],
          "order" : "column-major",
          "byteOrder" : "little-endian"
        }
      },
      {
        "name" : "study-input-source-supported-v1.json",
        "role" : "study-input",
        "schema" : {
          "name" : "eyes4s.study-input",
          "version" : 1
        },
        "media" : "application/json",
        "length" : "5141",
        "sha256" : "a4b17ec5c97a04eb4b3d1f47401b2c70cd6a32737325e474708137c4e9cea1e4",
        "identity" : "9ac7e83e4874dec7",
        "layout" : null
      },
      {
        "name" : "temporal-base-v1.json",
        "role" : "study-input",
        "schema" : {
          "name" : "eyes4s.study-input",
          "version" : 1
        },
        "media" : "application/json",
        "length" : "32258",
        "sha256" : "e52de1c8b3d0d94817e0e6511a3bf2551abf17056a0877f58702e529daeba7e8",
        "identity" : "4fd98f50693c79ad",
        "layout" : null
      },
      {
        "name" : "temporal-study-input-v1.json",
        "role" : "temporal-study-input",
        "schema" : {
          "name" : "eyes4s.temporal-study-input",
          "version" : 1
        },
        "media" : "application/json",
        "length" : "50917",
        "sha256" : "bb149a480ba54c9320d767d32e4cce79e4461baa8df018b407f99b7d74676840",
        "identity" : "def40de68e529acf",
        "layout" : null
      }
    ],
    "relations" : [
      {
        "kind" : "recording-of",
        "input" : "recording-input-v1.json",
        "recording" : "recording-standalone-v1.json"
      },
      {
        "kind" : "recording-of",
        "input" : "binocular-recording-input-v1.json",
        "recording" : "binocular-recording-v1.json"
      },
      {
        "kind" : "payload-of",
        "owner" : "packed-recording-v1.json",
        "payload" : "packed-recording-v1.tMicros.bin"
      },
      {
        "kind" : "payload-of",
        "owner" : "packed-recording-v1.json",
        "payload" : "packed-recording-v1.support.bin"
      },
      {
        "kind" : "payload-of",
        "owner" : "packed-recording-v1.json",
        "payload" : "packed-recording-v1.lineage.bin"
      },
      {
        "kind" : "payload-of",
        "owner" : "packed-recording-v1.json",
        "payload" : "packed-recording-v1.values.bin"
      },
      {
        "kind" : "temporal-base",
        "temporal" : "temporal-study-input-v1.json",
        "base" : "temporal-base-v1.json"
      }
    ]
  }
}"""

  // Portable mirrors of the pretty-printed resource files, compared as JSON values.
  val standaloneRecordingVersionOne: String =
    """{"schema":{"name":"eyes4s.recording","version":1},"value":{"identities":{"frames":[{"id":"display","unit":"px","xMin":0.0,"yMin":0.0,"xMax":1000.0,"yMax":1000.0,"yAxis":"Down"}],"grids":[],"clocks":["tracker"]},"recording":{"frame":"display","clock":"tracker","eye":"left","pupilUnit":"arbitrary","rate":{"kind":"fixed","hz":500.0},"samplingToleranceMicros":"2","recording":"2c826dc41ae25e67","samples":{"length":8,"tMicros":["0","2000","4001","6000","8000","10000","12000","14000"],"state":["tracked","tracked","blink","lost","tracked","offScreen","tracked","tracked"],"x":[500.0,510.25,null,null,515.0,1200.0,520.0,525.0],"y":[500.0,500.0,null,null,502.0,-5.0,500.0,505.0],"pupil":[900.0,901.0,null,null,null,null,903.0,905.0],"lineage":["measured","measured>smoothed","measured","measured","interpolated","measured","interpolated>smoothed","measured"]}}}}"""

  val binocularRecordingVersionOne: String =
    """{"schema":{"name":"eyes4s.binocular-recording","version":1},"value":{"identities":{"frames":[{"id":"display","unit":"px","xMin":0.0,"yMin":0.0,"xMax":1000.0,"yMax":1000.0,"yAxis":"Down"}],"grids":[],"clocks":["tracker"]},"recording":{"frame":"display","clock":"tracker","pupilUnit":"arbitrary","rate":{"kind":"fixed","hz":2000.0},"samplingToleranceMicros":"1","recording":"39e3dafb2de70ca2","samples":{"length":2,"tMicros":["300000","300500"],"left":{"state":["tracked","tracked"],"x":[1.0,4.5],"y":[2.0,5.5],"pupil":[3.0,6.5]},"right":{"state":["tracked","lost"],"x":[4.0,null],"y":[5.0,null],"pupil":[6.0,null]}}}}}"""

  val packedRecordingVersionOne: String =
    """{"schema":{"name":"eyes4s.packed-recording","version":1},"value":{"identities":{"frames":[{"id":"display","unit":"px","xMin":0.0,"yMin":0.0,"xMax":1000.0,"yMax":1000.0,"yAxis":"Down"}],"grids":[],"clocks":["tracker"]},"recording":{"frame":"display","clock":"tracker","eye":"left","pupilUnit":"arbitrary","rate":{"kind":"fixed","hz":500.0},"samplingToleranceMicros":"2","recording":"2c826dc41ae25e67","samples":{"length":8,"lineages":["measured","measured>smoothed","interpolated","interpolated>smoothed"],"tMicros":{"sha256":"b0eb08db23502a251b675c9f0683108455c0f38c549a2c12ec249f7daf21d7b5","layout":{"element":"int64","shape":[8],"order":"row-major","byteOrder":"little-endian"}},"support":{"sha256":"9d92b6855fd8a4ccfd53bf41a75cb97381a420422c00ce381eb309304a533001","layout":{"element":"uint8","shape":[8],"order":"row-major","byteOrder":"little-endian"}},"lineage":{"sha256":"b376730a4db09f72de102f105a7db3904683a5e3f384d047c0b3855a91d9f145","layout":{"element":"uint8","shape":[8],"order":"row-major","byteOrder":"little-endian"}},"values":{"sha256":"4ead6bd01976f4f532fac378ff25513d11cb2258a3c2ef475086f9df5e03a795","layout":{"element":"float64","shape":[8,3],"order":"column-major","byteOrder":"little-endian"}}}}}}"""

  val timelineVersionOne: String =
    """{"schema":{"name":"eyes4s.timeline","version":1},"value":{"clock":"presentation-display","marks":[{"atMicros":"-1500000","value":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"c","phase":"retest"}}},{"atMicros":"0","value":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"a","phase":"encode"}}},{"atMicros":"9007199254740993","value":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"b","phase":"encode"}}},{"atMicros":"9007199254740993","value":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"a","phase":"recall"}}}]}}"""

  val scoreCodecsVersionOne: String =
    """[{"schema":{"name":"eyes4s.similarity","version":1},"value":0.1},{"schema":{"name":"eyes4s.measure-distance","version":1},"value":2.5},{"schema":{"name":"eyes4s.scalar","version":1},"value":-0.3},{"schema":{"name":"eyes4s.signed-difference","version":1},"value":-0.24999999999999997}]"""
