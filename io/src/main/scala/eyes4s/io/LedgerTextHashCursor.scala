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

package eyes4s.io

import eyes4s.design.SampleQuantum
import eyes4s.kernel.ContentHash

/** Hash one already-materialized, resource-checked field incrementally.
  * Work counts UTF-16 code units plus one end-of-field unit. No substring,
  * encoded byte array or source-wide completion traversal is created.
  */
private[io] final case class LedgerTextHashCursor private (
    text: String,
    index: Int,
    hash: ContentHash
):
  def advance(quantum: SampleQuantum): LedgerTextHashStep =
    var at    = index
    var value = hash
    var units = 0
    var done  = false
    while units < quantum.value && !done do
      units += 1
      if at == text.length then done = true
      else
        value = ContentHash.appendCodeUnit(value, text.charAt(at))
        at += 1
    if done then LedgerTextHashStep.Done(units, value)
    else LedgerTextHashStep.More(units, new LedgerTextHashCursor(text, at, value))

private[io] object LedgerTextHashCursor:
  def start(
      text: String,
      limits: LedgerExecutionLimits,
      at: LedgerResourceLocation
  ): Either[LedgerResourceError, LedgerTextHashCursor] =
    limits
      .check(LedgerResource.FieldCodeUnits, text.length.toLong, at)
      .map(_ => new LedgerTextHashCursor(text, 0, ContentHash.empty))

private[io] enum LedgerTextHashStep:
  case More(workUnits: Int, next: LedgerTextHashCursor)
  case Done(workUnits: Int, hash: ContentHash)
