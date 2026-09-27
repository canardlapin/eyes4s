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

package eyes4s.plan

/** The scientific operation whose completed objects a meter counts. */
enum StageKind derives CanEqual:
  case Estimating, Comparing, Reducing, Contrasting

/** Maps, comparisons, reduced keys, or rows (including imported records). */
enum CountUnit derives CanEqual:
  case Maps, Pairs, Keys, Rows

/** A refused progress value, retaining the operands that violated its contract. */
enum StageMeterError derives CanEqual:
  case NegativeDone(kind: StageKind, unit: CountUnit, done: Long)
  case NegativeTotal(kind: StageKind, unit: CountUnit, total: SegmentTotal)
  case BeyondTotal(kind: StageKind, unit: CountUnit, done: Long, total: SegmentTotal)
  case Regressed(kind: StageKind, unit: CountUnit, previous: Long, next: Long)

  def message: String = this match
    case NegativeDone(kind, unit, done) =>
      s"$kind progress in $unit has negative completed count $done."
    case NegativeTotal(kind, unit, total) =>
      s"$kind progress in $unit has negative total $total."
    case BeyondTotal(kind, unit, done, total) =>
      s"$kind progress in $unit completed $done beyond $total."
    case Regressed(kind, unit, previous, next) =>
      s"$kind progress in $unit regressed from $previous to $next."

/** Completed objects in one stage, distinct from the runner's work units.
  * An unresolved automatic count is [[SegmentTotal.Counting]], never an absent meter.
  */
final class StageMeter private (
    val kind: StageKind,
    val unit: CountUnit,
    val done: Long,
    val total: SegmentTotal
):
  /** Advance the same stage monotonically, optionally resolving its total. */
  def advanceTo(next: Long, total: SegmentTotal = total): Either[StageMeterError, StageMeter] =
    if next < done then Left(StageMeterError.Regressed(kind, unit, done, next))
    else StageMeter.of(kind, unit, next, total)

object StageMeter:
  def of(
      kind: StageKind,
      unit: CountUnit,
      done: Long,
      total: SegmentTotal
  ): Either[StageMeterError, StageMeter] =
    val bound = total match
      case SegmentTotal.Exact(n)                        => Some(n)
      case SegmentTotal.AtMost(n)                       => Some(n)
      case SegmentTotal.Counting | SegmentTotal.Unknown => None
    if done < 0 then Left(StageMeterError.NegativeDone(kind, unit, done))
    else if bound.exists(_ < 0) then Left(StageMeterError.NegativeTotal(kind, unit, total))
    else if bound.exists(done > _) then
      Left(StageMeterError.BeyondTotal(kind, unit, done, total))
    else Right(new StageMeter(kind, unit, done, total))
