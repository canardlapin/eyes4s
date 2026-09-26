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

/** The same UTF-16 ordering as String.compareTo, with one compared pair of
  * code units or one end-of-text decision per work unit. This supplies both
  * equality and ordering without an uncounted whole-string comparison.
  */
private[io] final case class LedgerTextOrderCursor private (
    left: String,
    right: String,
    index: Int
):
  def advance(quantum: SampleQuantum): LedgerTextOrderStep =
    var at                  = index
    var units               = 0
    var result: Option[Int] = None
    while units < quantum.value && result.isEmpty do
      units += 1
      if at == left.length || at == right.length then result = Some(left.length - right.length)
      else
        val difference = left.charAt(at).toInt - right.charAt(at).toInt
        if difference != 0 then result = Some(difference)
        else at += 1
    result match
      case Some(order) => LedgerTextOrderStep.Done(units, order)
      case None => LedgerTextOrderStep.More(units, new LedgerTextOrderCursor(left, right, at))

private[io] object LedgerTextOrderCursor:
  def start(
      left: String,
      right: String,
      limits: LedgerExecutionLimits,
      leftAt: LedgerResourceLocation,
      rightAt: LedgerResourceLocation
  ): Either[LedgerResourceError, LedgerTextOrderCursor] =
    for
      _ <- limits.check(LedgerResource.FieldCodeUnits, left.length.toLong, leftAt)
      _ <- limits.check(LedgerResource.FieldCodeUnits, right.length.toLong, rightAt)
    yield new LedgerTextOrderCursor(left, right, 0)

private[io] enum LedgerTextOrderStep:
  case More(workUnits: Int, next: LedgerTextOrderCursor)
  case Done(workUnits: Int, comparison: Int)
