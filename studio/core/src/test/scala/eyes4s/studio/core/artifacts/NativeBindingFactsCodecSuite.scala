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

package eyes4s.studio.core.artifacts

import eyes4s.studio.core.command.CommandSamples
import eyes4s.studio.core.document.*
import io.circe.Json

class NativeBindingFactsCodecSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val legacy                            = CommandSamples.nativeFacts
  private val definition                        = get(
    NativeDatasetDefinition.of(DocumentSamples.t2.dataset(legacy.dataset).get)
  )
  private val bound = get(
    NativeBindingFacts.of(
      legacy.run,
      legacy.stamp,
      legacy.source,
      legacy.result,
      legacy.recipeSnapshot,
      Some(definition)
    )
  )
  private val ladder = get(NativeBindingFacts.ladder)
  private val codec  = get(NativeBindingFacts.codec)

  test("facts v1 pin remains exact; v2 expresses complete content and exact policy") {
    val old     = get(codec.encode(legacy))
    val current = get(codec.encode(bound))
    assertEquals(old.hcursor.downField("schema").get[Int]("version"), Right(1))
    assertEquals(current.hcursor.downField("schema").get[Int]("version"), Right(2))
    assertEquals(get(codec.decode(old)), legacy)
    assertEquals(get(codec.decode(current)), bound)
    assert(!old.hcursor.downField("value").downField("datasetDefinition").succeeded)
    assert(get(ladder.upTo(ladder.versions.head)).codec.encode(bound).isLeft)
    assert(get(ladder.upTo(ladder.versions.head)).codec.decode(current).isLeft)
    val lifted = get(ladder.lift(old))
    assert(
      lifted.hcursor.downField("value").downField("datasetDefinition").focus.contains(Json.Null)
    )
    assertEquals(get(codec.decode(lifted)), legacy)
    assertEquals(get(codec.encode(get(codec.decode(lifted)))), old)
  }

  test(
    "older payloads cannot smuggle definitions and current payloads require explicit evidence"
  ) {
    val old     = get(codec.encode(legacy)).hcursor.downField("value").focus.get
    val current = get(codec.encode(bound)).hcursor.downField("value").focus.get
    Vector(Json.Null, current.hcursor.downField("datasetDefinition").focus.get).foreach {
      value =>
        assert(
          ladder
            .readAt(ladder.versions.head, old.mapObject(_.add("datasetDefinition", value)))
            .isLeft
        )
    }
    assert(
      ladder.readAt(ladder.latest, current.mapObject(_.remove("datasetDefinition"))).isLeft
    )
    assert(
      ladder
        .readAt(ladder.latest, current.mapObject(_.add("datasetDefinition", Json.obj())))
        .isLeft
    )
    val missingPolicy = current.hcursor
      .downField("datasetDefinition")
      .withFocus(_.mapObject(_.remove("policy")))
      .top
      .get
    assert(ladder.readAt(ladder.latest, missingPolicy).isLeft)
    assert(old.mapObject(_.add("datasetDefinition", Json.Null)).as[NativeBindingFacts].isLeft)
  }

  test("dataset decision bookkeeping is excluded while admission policy remains explicit") {
    val spec      = DocumentSamples.t2.dataset(legacy.dataset).get
    val pending   = spec.copy(decision = AdmissionDecision.Pending)
    val verifying = spec.copy(decision =
      AdmissionDecision.Verifying(get(DatasetRevisionSpec.contentDigest(spec)))
    )
    assertEquals(
      get(NativeDatasetDefinition.of(pending)),
      get(NativeDatasetDefinition.of(verifying))
    )
    val withoutPolicy = spec.copy(decision =
      AdmissionDecision.Admitted(None, CoreBinding.unbound, CoreBinding.unbound)
    )
    assertNotEquals(get(NativeDatasetDefinition.of(withoutPolicy)), definition)
    val json              = get(NativeDatasetDefinition.of(withoutPolicy))
    val legacyPolicyFacts = get(
      NativeBindingFacts.of(
        legacy.run,
        legacy.stamp,
        legacy.source,
        legacy.result,
        legacy.recipeSnapshot,
        Some(json)
      )
    )
    assertEquals(get(codec.decode(get(codec.encode(legacyPolicyFacts)))), legacyPolicyFacts)
    assertEquals(
      get(codec.encode(legacyPolicyFacts)).hcursor.downField("schema").get[Int]("version"),
      Right(2)
    )
  }
