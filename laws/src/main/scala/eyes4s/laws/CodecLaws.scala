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

package eyes4s.laws

import eyes4s.codec.VersionedCodec
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import org.typelevel.discipline.Laws

/** Published persistence conformance for any conditional VersionedCodec. */
trait CodecLaws extends Laws:
  def roundTrip[A](
      codec: VersionedCodec[A],
      gen: Gen[A],
      equivalent: (A, A) => Boolean
  ): RuleSet =
    new SimpleRuleSet(
      "versionedCodec",
      "round trip preserves the value" -> forAll(gen) { value =>
        codec.encode(value).flatMap(codec.decode).exists(decoded => equivalent(value, decoded))
      },
      "reencoding is canonical" -> forAll(gen) { value =>
        codec.encode(value).exists { encoded =>
          codec.decode(encoded).flatMap(codec.encode).contains(encoded)
        }
      }
    )
object CodecLaws extends CodecLaws
