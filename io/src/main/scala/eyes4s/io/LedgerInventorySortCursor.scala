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

private[io] enum LedgerInventorySortStage derives CanEqual:
  case Seed, Merge, Compare

private[io] enum LedgerInventoryLabelField derives CanEqual:
  case Participant, Phase, Trial

private[io] final case class LedgerInventoryMerge(
    width: Int,
    left: Int,
    right: Int,
    middle: Int,
    end: Int
)

private[io] enum LedgerInventorySortWork:
  case Seed(index: Int)
  case Run(width: Int, start: Int)
  case Merge(state: LedgerInventoryMerge)
  case Compare(
      state: LedgerInventoryMerge,
      field: LedgerInventoryLabelField,
      cursor: LedgerTextOrderCursor
  )
  case Emit(state: LedgerInventoryMerge, takeLeft: Boolean)

/** Stable label ordering of parsed inventory rows, represented by row indices.
  * This only prepares adjacent equal-label groups; inventory group presentation
  * must still follow first-source-record order, as TrialInventory.read does.
  *
  * Seeding, merge setup, copying and pass transitions each charge one logical
  * operation. Text comparisons charge UTF-16 pairs/end decisions. Persistent
  * Vector access/append is bounded by its index-tree depth and the explicit
  * buffer envelope, not asserted to be constant time or a wall-clock bound.
  *
  * `scope.before` accounts for the caller's retained input evidence. This cursor
  * adds one logical unit per live index in its two buffers, checking before each
  * append. Swapping buffers releases the old buffer's logical count; terminal
  * output is an existing Vector reference, with no bulk freeze or reconstruction.
  * Other parsed-row resource dimensions are the caller's preflight responsibility.
  */
private[io] final case class LedgerInventorySortCursor private (
    rows: Vector[TrialInventory.Row],
    scope: LedgerRetentionScope,
    order: Vector[Int],
    output: Vector[Int],
    work: LedgerInventorySortWork
):
  def stage: LedgerInventorySortStage = work match
    case LedgerInventorySortWork.Seed(_)          => LedgerInventorySortStage.Seed
    case LedgerInventorySortWork.Compare(_, _, _) => LedgerInventorySortStage.Compare
    case _                                        => LedgerInventorySortStage.Merge

  private def at(index: Int): LedgerResourceLocation =
    LedgerResourceLocation(LedgerResourceSource.Inventory, Some(rows(index).number.toLong))

  private def field(index: Int, part: LedgerInventoryLabelField): String =
    val label = TrialInventory.label(rows(index).identity)
    part match
      case LedgerInventoryLabelField.Participant => label._1
      case LedgerInventoryLabelField.Phase       => label._2
      case LedgerInventoryLabelField.Trial       => label._3

  private def comparison(state: LedgerInventoryMerge, part: LedgerInventoryLabelField) =
    val left  = order(state.left)
    val right = order(state.right)
    LedgerTextOrderCursor
      .start(field(left, part), field(right, part), scope.limits, at(left), at(right))
      .left
      .map(scope.restore)

  def advance(quantum: SampleQuantum): Either[LedgerResourceError, LedgerInventorySortStep] =
    import LedgerInventorySortWork.*
    import LedgerInventoryLabelField.*
    val inventory = LedgerResourceLocation(LedgerResourceSource.Inventory)
    def more(
        units: Int,
        next: LedgerInventorySortCursor
    ): Either[LedgerResourceError, LedgerInventorySortStep] =
      scope
        .total(next.order.size.toLong + next.output.size.toLong, inventory)
        .map(kept => LedgerInventorySortStep.More(stage, units, kept, next))
    def done(indices: Vector[Int]): Either[LedgerResourceError, LedgerInventorySortStep] =
      scope
        .total(indices.size.toLong, inventory)
        .map(kept => LedgerInventorySortStep.Done(1, kept, indices))
    def room(index: Int): Either[LedgerResourceError, Long] =
      scope.limits
        .add(
          LedgerResource.RetainedEvidenceUnits,
          order.size.toLong + output.size.toLong,
          1L,
          at(index)
        )
        .left
        .map(scope.restore)
    def emit(
        state: LedgerInventoryMerge,
        takeLeft: Boolean
    ): Either[LedgerResourceError, LedgerInventorySortStep] =
      val index = order(if takeLeft then state.left else state.right)
      val next  = if takeLeft then state.copy(left = state.left + 1)
      else state.copy(right = state.right + 1)
      room(index).flatMap(_ => more(1, copy(output = output :+ index, work = Merge(next))))
    work match
      case Seed(index) =>
        if index == rows.size then
          if rows.size <= 1 then done(order)
          else more(1, copy(work = Run(1, 0)))
        else
          for
            _ <- scope.limits
              .check(
                LedgerResource.FieldCodeUnits,
                field(index, Participant).length.toLong,
                at(index)
              )
              .left
              .map(scope.restore)
            _ <- scope.limits
              .check(
                LedgerResource.FieldCodeUnits,
                field(index, Phase).length.toLong,
                at(index)
              )
              .left
              .map(scope.restore)
            _ <- scope.limits
              .check(
                LedgerResource.FieldCodeUnits,
                field(index, Trial).length.toLong,
                at(index)
              )
              .left
              .map(scope.restore)
            _    <- room(index)
            step <- more(1, copy(order = order :+ index, work = Seed(index + 1)))
          yield step
      case Run(width, start) =>
        if start == rows.size then
          // This form avoids n+1 and width*2 overflowing for an Int-sized input.
          if width >= rows.size - rows.size / 2 then done(output)
          else more(1, copy(order = output, output = Vector.empty, work = Run(width * 2, 0)))
        else
          val middle = start + math.min(width, rows.size - start)
          val end    = middle + math.min(width, rows.size - middle)
          more(1, copy(work = Merge(LedgerInventoryMerge(width, start, middle, middle, end))))
      case Merge(state) =>
        if state.left == state.middle && state.right == state.end then
          more(1, copy(work = Run(state.width, state.end)))
        else if state.left == state.middle then emit(state, false)
        else if state.right == state.end then emit(state, true)
        else
          comparison(state, Participant).flatMap(cursor =>
            more(1, copy(work = Compare(state, Participant, cursor)))
          )
      case Compare(state, part, cursor) =>
        cursor.advance(quantum) match
          case LedgerTextOrderStep.More(units, next) =>
            more(units, copy(work = Compare(state, part, next)))
          case LedgerTextOrderStep.Done(units, compared) =>
            val nextPart = part match
              case Participant => Some(Phase)
              case Phase       => Some(Trial)
              case Trial       => None
            nextPart match
              case Some(nextField) if compared == 0 =>
                comparison(state, nextField).flatMap(next =>
                  more(units, copy(work = Compare(state, nextField, next)))
                )
              case _ => more(units, copy(work = Emit(state, compared <= 0)))
      case Emit(state, takeLeft) => emit(state, takeLeft)

private[io] object LedgerInventorySortCursor:
  def start(
      rows: Vector[TrialInventory.Row],
      limits: LedgerExecutionLimits,
      alreadyRetained: Long
  ): Either[LedgerResourceError, LedgerInventorySortCursor] =
    LedgerRetentionScope
      .start(limits, alreadyRetained, LedgerResourceLocation(LedgerResourceSource.Inventory))
      .map(scope =>
        new LedgerInventorySortCursor(
          rows,
          scope,
          Vector.empty,
          Vector.empty,
          LedgerInventorySortWork.Seed(0)
        )
      )

private[io] enum LedgerInventorySortStep:
  case More(
      stage: LedgerInventorySortStage,
      workUnits: Int,
      retainedUnits: Long,
      next: LedgerInventorySortCursor
  )
  case Done(workUnits: Int, retainedUnits: Long, indices: Vector[Int])
