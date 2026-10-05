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

package eyes4s.studio.core.bundle

/** The golden t2 bundle: its part paths, the SHA-256 of its `project.json`
  * and a hand-written version 1 manifest of the same parts. The files of
  * fixtures/studio-bundles/t2.eyes are these bytes (checked by
  * studio-desktop's ProjectBundleGoldenSuite).
  */
object BundlePins:
  val t2Parts: Vector[String] = Vector(
    "datasets/r2.ae6dd534739238ee.json",
    "mappings/r2.5d9b410cd0b48432.json",
    "datasets/r3.9bc7a776790d1794.json",
    "mappings/r3.8e0b0f8f226a2cbb.json",
    "analyses/rev3.2acd63a0cfb4ed2f.json",
    "analyses/rev4.ce2a3d06a6748bd9.json",
    "analyses/draft-rev5.d9972409483c456b.json",
    "runs/5/run.5b78b0f656b5e858.json",
    "runs/6/run.8609de86b7f93424.json",
    "runs/7/run.c4e9ae17c3e39729.json",
    "reporting/spec.fcef1238d989f763.json",
    "figures/figure1.8daf50b41ce491ad.json",
    "figures/figure2.090bf219dbcc401d.json"
  )

  val t2ManifestSha256: String =
    "fff7dbbd28ebec8091e9c18b56205b5d8d2d1b33c8829fbd25b2258ad45adc1e"

  /** The t2 bundle's manifest as the pre-release version 1 wrote it: no
    * sharing options (everything travelled) and no science digest. Written
    * by hand, not by the version 1 writer.
    */
  val t2ManifestV1: String =
    """{
      |  "schema": { "name": "studio.project", "version": 1 },
      |  "value": {
      |    "document": { "name": "studio.document", "version": 3 },
      |    "inputs": [
      |      {
      |        "kind": { "Source": { "role": { "Fixations": {} } } },
      |        "name": "fixations.csv",
      |        "sha256": "19342ecedb6e089a190b4784a248907b27fedbef03ca3e6d4bc251702fbbc2f2",
      |        "length": 572152
      |      },
      |      {
      |        "kind": { "Source": { "role": { "Trials": {} } } },
      |        "name": "trials.csv",
      |        "sha256": "0668bccf6c672b706c0a138268f632fb5a768170578a01c58ee752176f0f8b99",
      |        "length": 57968
      |      }
      |    ],
      |    "parts": {
      |      "datasets": [
      |        {
      |          "dataset": {
      |            "path": "datasets/r2.ae6dd534739238ee.json",
      |            "sha256": "ae6dd534739238eef6ef6b5166d19b5e50b219a8f9e8785f73ff26af311e3820",
      |            "length": 1031
      |          },
      |          "mapping": {
      |            "path": "mappings/r2.5d9b410cd0b48432.json",
      |            "sha256": "5d9b410cd0b48432cea35fefea15b58df3a2aaded24e536174647a1a9afa0ea4",
      |            "length": 377
      |          }
      |        },
      |        {
      |          "dataset": {
      |            "path": "datasets/r3.9bc7a776790d1794.json",
      |            "sha256": "9bc7a776790d1794bc8032f29c2814f72762076fec157a3231b66db6aa401669",
      |            "length": 1049
      |          },
      |          "mapping": {
      |            "path": "mappings/r3.8e0b0f8f226a2cbb.json",
      |            "sha256": "8e0b0f8f226a2cbb601673e4f6561dfb4576daf852725cd29599598874f8da5e",
      |            "length": 426
      |          }
      |        }
      |      ],
      |      "analyses": [
      |        {
      |          "path": "analyses/rev3.2acd63a0cfb4ed2f.json",
      |          "sha256": "2acd63a0cfb4ed2fedfae02a935b5abf638bb13fc656d24d55c0109454d03ab2",
      |          "length": 722
      |        },
      |        {
      |          "path": "analyses/rev4.ce2a3d06a6748bd9.json",
      |          "sha256": "ce2a3d06a6748bd94f8d39cced647a531205795714026f8b37691540257c5717",
      |          "length": 722
      |        }
      |      ],
      |      "draft": {
      |        "path": "analyses/draft-rev5.d9972409483c456b.json",
      |        "sha256": "d9972409483c456b052961cce87fd5cc159b49964829480e354df40453dd4928",
      |        "length": 100
      |      },
      |      "runs": [
      |        {
      |          "path": "runs/5/run.5b78b0f656b5e858.json",
      |          "sha256": "5b78b0f656b5e858482ecc7ea5ea127e41c56d63b57d0b138ec188ed7546b8ea",
      |          "length": 83
      |        },
      |        {
      |          "path": "runs/6/run.8609de86b7f93424.json",
      |          "sha256": "8609de86b7f93424411ac663d4d448165ea9af37d082401ea1cc77ffc3e35c84",
      |          "length": 104
      |        },
      |        {
      |          "path": "runs/7/run.c4e9ae17c3e39729.json",
      |          "sha256": "c4e9ae17c3e3972997ad4e83c7fa2df3d55e1a415dd3b3a107e3f7249acba85e",
      |          "length": 83
      |        }
      |      ],
      |      "reporting": [
      |        {
      |          "path": "reporting/spec.fcef1238d989f763.json",
      |          "sha256": "fcef1238d989f763c7b21775b78252fb0023b04e74e28b98d0d25f21a5eff1f3",
      |          "length": 154
      |        }
      |      ],
      |      "figures": [
      |        {
      |          "path": "figures/figure1.8daf50b41ce491ad.json",
      |          "sha256": "8daf50b41ce491ad0b12fa5c002fb7ebebf26c08a5b53b8f39689eb398912874",
      |          "length": 796
      |        },
      |        {
      |          "path": "figures/figure2.090bf219dbcc401d.json",
      |          "sha256": "090bf219dbcc401d25a5873ecd94cef2fb82dd77c14393242c86dd1c28d9de71",
      |          "length": 171
      |        }
      |      ]
      |    },
      |    "presentation": {
      |      "perspective": { "Compare": {} },
      |      "theme": { "Light": {} },
      |      "stage": { "Dark": {} },
      |      "mapOpacity": 0.6,
      |      "underlay": false,
      |      "shownRun": 7,
      |      "layouts": []
      |    }
      |  }
      |}
      |""".stripMargin
