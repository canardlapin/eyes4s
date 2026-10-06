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

import eyes4s.codec.{CanonicalDigest, CodecError}
import eyes4s.plan.{AdmissionDecision as CoreAdmissionDecision}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.document.DigestJson.given
import eyes4s.studio.core.document.AdmissionDecision.coreDecision
import io.circe.{Codec, Decoder, DecodingFailure, Encoder}

/** The entire immutable dataset content and its separately recorded admission
  * policy. Decision lifecycle and ledger bindings are bookkeeping, excluded by
  * DatasetRevisionSpec.contentDigest (bead bd-01M49A48HB52R4AT20V94ZP6PY).
  */
final case class NativeDatasetDefinition private (
    content: CanonicalDigest[DatasetRevisionSpec],
    policy: Option[CoreAdmissionDecision]
) derives CanEqual

object NativeDatasetDefinition:
  def of(spec: DatasetRevisionSpec): Either[CodecError, NativeDatasetDefinition] =
    DatasetRevisionSpec
      .contentDigest(spec)
      .map(d => new NativeDatasetDefinition(d, spec.decision.admittedUnder))

  given Codec.AsObject[NativeDatasetDefinition] = Codec.AsObject.from(
    Decoder.instance { c =>
      for
        content <- c.get[CanonicalDigest[DatasetRevisionSpec]]("content")
        policy  <- c
          .downField("policy")
          .focus
          .toRight(
            DecodingFailure(
              "Dataset definition requires its recorded policy (null for legacy absence).",
              c.history
            )
          )
          .flatMap(_.as[Option[CoreAdmissionDecision]])
      yield new NativeDatasetDefinition(content, policy)
    },
    Encoder.forProduct2("content", "policy")(d => (d.content, d.policy))
  )
