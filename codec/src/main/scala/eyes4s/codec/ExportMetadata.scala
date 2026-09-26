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

import eyes4s.compare.*
import eyes4s.design.*
import eyes4s.kernel.*
import io.circe.Json

/** Internal bridge reuses existing archive wire conventions for export sidecars. */
private[eyes4s] object ExportMetadata:
  def provenance(p: Provenance): Json     = ResultWire.provenance(p)
  def compareError(e: CompareError): Json = ResultWire.compareError(e)
  def pairing[K](keys: VersionedCodec[K], p: PairingReport[K, K]): Either[CodecError, Json] =
    ResultWire.pairingReport(keys)(p)
  def reduction[K](keys: VersionedCodec[K], r: ReductionReport[K]): Either[CodecError, Json] =
    ResultWire.reductionReport(keys)(r)
  def reductionError[K](
      keys: VersionedCodec[K],
      e: ReductionError[K]
  ): Either[CodecError, Json] = ResultWire.reductionError(keys)(e)
  def contrastError[K](
      keys: VersionedCodec[K],
      e: ContrastRowError[K]
  ): Either[CodecError, Json] = ResultWire.contrastRowError(keys)(e)
  def evaluation[U <: Unit2D: UnitLabel](e: EvaluationInfo): Either[CodecError, Json] =
    ResultWire.evaluationInfo[U](e)
  def frame[U <: Unit2D: UnitLabel](f: Frame[U]): Json = DomainWire.frame(f)
