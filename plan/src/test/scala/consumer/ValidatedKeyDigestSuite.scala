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

package consumer

import eyes4s.design.KeyDigest
import eyes4s.plan.*

class ValidatedKeyDigestSuite extends munit.FunSuite:
  private case class LegacyKey(
      participant: String,
      phase: String,
      trial: String,
      occurrence: TrialOccurrence,
      item: String
  )
  private case class LegacyIdentity(
      participant: String,
      phase: String,
      trial: String,
      occurrence: TrialOccurrence
  )

  test("validated trial keys retain the exact former product-derived sampling digest") {
    val keyDigest      = KeyDigest.derived[LegacyKey]
    val identityDigest = KeyDigest.derived[LegacyIdentity]
    for
      text <- Vector("a", "a/b", "ab", "α", " spaced ")
      n    <- Vector(1, 2, 17)
    do
      val occurrence = TrialOccurrence.of(n).toOption.get
      val key        =
        TrialKey.of(text, "recall", "trial:" + text, occurrence, "item:" + text).toOption.get
      val identity = TrialIdentity.of(key)
      assertEquals(
        KeyDigest[TrialKey].digest(key),
        keyDigest.digest(
          LegacyKey(key.participant, key.phase, key.trial, key.occurrence, key.item)
        )
      )
      assertEquals(
        KeyDigest[TrialIdentity].digest(identity),
        identityDigest.digest(
          LegacyIdentity(
            identity.participant,
            identity.phase,
            identity.trial,
            identity.occurrence
          )
        )
      )
  }
