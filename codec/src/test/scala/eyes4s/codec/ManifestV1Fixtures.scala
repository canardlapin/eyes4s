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

/** Pinned first manifest schema; matches src/test/resources/eyes4s/manifest-v1.json
  * byte for byte. It lists the pinned study-v1 plan, study-input-v1 input,
  * admission-ledger-v1 ledger and study-result-v1 archive by the SHA-256 of
  * their resource files.
  */
object ManifestV1Fixtures:
  /** The SHA-256 of [[manifestVersionOne]], computed with `shasum -a 256`. */
  val address: String = "43d35a94e26a84c548c32ac2029e846a4abd639ec2c61074274018786987f736"

  val manifestVersionOne: String = """{
  "schema" : {
    "name" : "eyes4s.manifest",
    "version" : 1
  },
  "value" : {
    "entries" : [
      {
        "name" : "plan",
        "role" : "study-plan",
        "schema" : {
          "name" : "eyes4s.study",
          "version" : 1
        },
        "media" : "application/json",
        "length" : "984",
        "sha256" : "f5f5a02f327b79250387365542540297bdc18cffe379648828d77cda3e668f71",
        "identity" : null,
        "layout" : null
      },
      {
        "name" : "input",
        "role" : "study-input",
        "schema" : {
          "name" : "eyes4s.study-input",
          "version" : 1
        },
        "media" : "application/json",
        "length" : "21898",
        "sha256" : "dd9922646e8ced281d3ed8f63fb38f36774c775ef836d95a76b66ebc664c3b52",
        "identity" : "cebe7474ab5c2aec",
        "layout" : null
      },
      {
        "name" : "ledger",
        "role" : "admission-ledger",
        "schema" : {
          "name" : "eyes4s.admission-ledger",
          "version" : 1
        },
        "media" : "application/json",
        "length" : "18987",
        "sha256" : "114585a745fa08bbb6acbbdb58590e6e29f16fd8e53aa2da7d5f77bccd7c5447",
        "identity" : null,
        "layout" : null
      },
      {
        "name" : "result",
        "role" : "study-result",
        "schema" : {
          "name" : "eyes4s.study-result",
          "version" : 1
        },
        "media" : "application/json",
        "length" : "75738",
        "sha256" : "d8429e2f980919fde450192e0485cfbcfc7920923fb45ef4863ea7923869d9f0",
        "identity" : null,
        "layout" : null
      }
    ],
    "relations" : [
      {
        "kind" : "plan-input",
        "plan" : "plan",
        "input" : "input"
      },
      {
        "kind" : "result-of",
        "result" : "result",
        "plan" : "plan",
        "input" : "input"
      }
    ]
  }
}"""
