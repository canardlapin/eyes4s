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

package eyes4s.design

/** Explicit allocation and traversal bounds for an exhaustive directed schedule.
  * The default retains all counts representable by the existing result API;
  * applications can choose smaller bounds before preparing work.
  */
final class PairScheduleBudget private (
    val maxSourceRows: Int,
    val maxCandidatePairs: Long,
    val maxSelectedPairs: Int
):
  /** Conservative planning uses raw operand sizes, before excluding duplicates. */
  def checkCounts(left: Long, right: Long): Either[PairScheduleError, Long] =
    val rows       = BigInt(left) + right
    val candidates = BigInt(left) * right
    if left < 0 || right < 0 then Left(PairScheduleError.InvalidCounts(left, right))
    else if rows > maxSourceRows then
      Left(PairScheduleError.SourceBudget(left, right, maxSourceRows))
    else if candidates > maxCandidatePairs then
      Left(PairScheduleError.CandidateBudget(left, right, maxCandidatePairs))
    else Right(candidates.toLong)

object PairScheduleBudget:
  val default: PairScheduleBudget =
    new PairScheduleBudget(Int.MaxValue, Long.MaxValue, Int.MaxValue)

  def of(
      maxSourceRows: Int,
      maxCandidatePairs: Long,
      maxSelectedPairs: Int
  ): Either[PairScheduleError, PairScheduleBudget] =
    if maxSourceRows < 0 || maxCandidatePairs < 0 || maxSelectedPairs < 0 then
      Left(PairScheduleError.InvalidBudget(maxSourceRows, maxCandidatePairs, maxSelectedPairs))
    else Right(new PairScheduleBudget(maxSourceRows, maxCandidatePairs, maxSelectedPairs))

/** Maximum candidate/diagnostic visits in one page, not a wall-clock promise
  * about arbitrary user-supplied relation predicates or key equality.
  */
final class PairQuantum private (val value: Int)
object PairQuantum:
  val default: PairQuantum                                   = new PairQuantum(1024)
  def of(value: Int): Either[PairScheduleError, PairQuantum] =
    if value <= 0 then Left(PairScheduleError.InvalidQuantum(value))
    else Right(new PairQuantum(value))

enum PairScheduleError derives CanEqual:
  case InvalidBudget(sourceRows: Int, candidatePairs: Long, selectedPairs: Int)
  case InvalidCounts(left: Long, right: Long)
  case InvalidQuantum(value: Int)
  case SourceBudget(left: Long, right: Long, maximum: Int)
  case CandidateBudget(left: Long, right: Long, maximum: Long)
  case SelectedBudget(relation: String, attempted: Long, maximum: Int)

  def message: String = this match
    case InvalidBudget(rows, candidates, selected) =>
      s"Pair budgets must be nonnegative: sourceRows=$rows, candidatePairs=$candidates, selectedPairs=$selected."
    case InvalidCounts(left, right) =>
      s"Pair operand counts must be nonnegative: left=$left, right=$right."
    case InvalidQuantum(value) => s"Pair page quantum must be positive, got $value."
    case SourceBudget(left, right, maximum) =>
      s"Pair operands left=$left and right=$right exceed the combined source-row budget $maximum."
    case CandidateBudget(left, right, maximum) =>
      s"Pair operands left=$left and right=$right exceed candidate-pair budget $maximum."
    case SelectedBudget(relation, attempted, maximum) =>
      s"Pair relation '$relation' would select $attempted pairs, exceeding budget $maximum."

/** Source positions address the original operands, before duplicate exclusion. */
final case class ScheduledPair[KL, KR] private[design] (
    leftIndex: Int,
    rightIndex: Int,
    left: KL,
    right: KR
) derives CanEqual

/** Pages never imply completion until the complete pairing report is available. */
enum PairPage[KL, KR]:
  case More(pairs: Vector[ScheduledPair[KL, KR]], workUnits: Int, next: PairCursor[KL, KR])
  case Done(pairs: Vector[ScheduledPair[KL, KR]], workUnits: Int, report: PairingReport[KL, KR])

/** A persistent cursor belongs to its schedule; callers cannot forge offsets or
  * transplant cursor state into another input. Re-reading it is deterministic.
  */
final class PairCursor[KL, KR] private[design] (
    private val schedule: DirectedPairSchedule[KL, KR],
    private[design] val leftPosition: Int,
    private[design] val rightPosition: Int,
    private[design] val rightReportPosition: Int,
    private[design] val foundLeft: Boolean,
    private[design] val matchedRight: Set[Int],
    private[design] val unmatchedLeft: Vector[KL],
    private[design] val unmatchedRight: Vector[KR],
    private[design] val selected: Int
):
  def advance(quantum: PairQuantum): Either[PairScheduleError, PairPage[KL, KR]] =
    schedule.advance(this, quantum)

/** An exhaustive schedule stores O(left + right) source metadata, never the
  * Cartesian pair table. Preparation groups duplicate keys in source order and
  * is bounded by maxSourceRows; enumeration and right-side diagnostics are paged.
  */
final class DirectedPairSchedule[KL, KR] private[design] (
    private val left: Vector[(Int, KL)],
    private val right: Vector[(Int, KR)],
    private val relation: Relation[KL, KR],
    val ambiguities: Vector[PairingAmbiguity[KL, KR]],
    val candidatePairCount: Long,
    val budget: PairScheduleBudget
):
  val pairSpace: PairSpace.BetweenDirected =
    PairSpace.BetweenDirected(relation.render, Selection.All)

  def start: PairCursor[KL, KR] =
    new PairCursor(this, 0, 0, 0, false, Set.empty, Vector.empty, Vector.empty, 0)

  private[design] def advance(
      cursor: PairCursor[KL, KR],
      quantum: PairQuantum
  ): Either[PairScheduleError, PairPage[KL, KR]] =
    var l                                  = cursor.leftPosition
    var r                                  = cursor.rightPosition
    var reportPosition                     = cursor.rightReportPosition
    var found                              = cursor.foundLeft
    var matchedRight                       = cursor.matchedRight
    var unmatchedLeft                      = cursor.unmatchedLeft
    var unmatchedRight                     = cursor.unmatchedRight
    var selected                           = cursor.selected
    var work                               = 0
    var failure: Option[PairScheduleError] = None
    val pairs                              = Vector.newBuilder[ScheduledPair[KL, KR]]

    while work < quantum.value && l < left.size && failure.isEmpty do
      if right.isEmpty then
        unmatchedLeft :+= left(l)._2
        l += 1
      else
        val (li, lk) = left(l)
        val (ri, rk) = right(r)
        if relation.accepts(lk, rk) then
          if selected == budget.maxSelectedPairs then
            failure = Some(
              PairScheduleError
                .SelectedBudget(relation.render, selected.toLong + 1L, budget.maxSelectedPairs)
            )
          else
            pairs += ScheduledPair(li, ri, lk, rk)
            selected += 1
            matchedRight += ri
            found = true
        r += 1
        if r == right.size then
          if !found then unmatchedLeft :+= lk
          found = false
          r = 0
          l += 1
      work += 1

    while work < quantum.value && l == left.size && reportPosition < right.size && failure.isEmpty
    do
      val (index, key) = right(reportPosition)
      if !matchedRight.contains(index) then unmatchedRight :+= key
      reportPosition += 1
      work += 1

    failure match
      case Some(error)                                            => Left(error)
      case None if l == left.size && reportPosition == right.size =>
        Right(
          PairPage.Done(
            pairs.result(),
            work,
            PairingReport(
              pairSpace,
              selected.toLong,
              selected,
              unmatchedLeft,
              unmatchedRight,
              ambiguities
            )
          )
        )
      case None =>
        Right(
          PairPage.More(
            pairs.result(),
            work,
            new PairCursor(
              this,
              l,
              r,
              reportPosition,
              found,
              matchedRight,
              unmatchedLeft,
              unmatchedRight,
              selected
            )
          )
        )

object DirectedPairSchedule:
  def exhaustive[KL, KR](
      left: Vector[KL],
      right: Vector[KR],
      relation: Relation[KL, KR],
      budget: PairScheduleBudget = PairScheduleBudget.default
  ): Either[PairScheduleError, DirectedPairSchedule[KL, KR]] =
    budget.checkCounts(left.size.toLong, right.size.toLong).map { _ =>
      val leftRows   = left.map(key => Trial(key, (), ()))
      val rightRows  = right.map(key => Trial(key, (), ()))
      val ld         = PairConstruction.duplicates(leftRows)
      val rd         = PairConstruction.duplicates(rightRows)
      val le         = PairConstruction.duplicateIndices(ld)
      val re         = PairConstruction.duplicateIndices(rd)
      val usableLeft = left.zipWithIndex.collect {
        case (key, index) if !le.contains(index) => index -> key
      }
      val usableRight = right.zipWithIndex.collect {
        case (key, index) if !re.contains(index) => index -> key
      }
      val ambiguities = ld
        .map(d => PairingAmbiguity.DuplicateLeft[KL, KR](d.key, d.occurrences.map(_.index))) ++
        rd.map(d => PairingAmbiguity.DuplicateRight[KL, KR](d.key, d.occurrences.map(_.index)))
      new DirectedPairSchedule(
        usableLeft,
        usableRight,
        relation,
        ambiguities,
        usableLeft.size.toLong * usableRight.size.toLong,
        budget
      )
    }
