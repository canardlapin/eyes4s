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
    private val schedule: ExhaustivePairSchedule[KL, KR],
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
private[design] final class ExhaustivePairSchedule[KL, KR](
    private val left: Vector[(Int, KL)],
    private val right: Vector[(Int, KR)],
    private val relation: Relation[KL, KR],
    val ambiguities: Vector[PairingAmbiguity[KL, KR]],
    val candidatePairCount: Long,
    val budget: PairScheduleBudget,
    val pairSpace: PairSpace
):
  private val incidentDiagnostics = pairSpace.storage == PairStorage.WithinUndirected

  private def acceptsPosition(leftIndex: Int, rightIndex: Int): Boolean = pairSpace match
    case _: PairSpace.BetweenDirected         => true
    case PairSpace.WithinDirected(_, self, _) =>
      self == SelfPolicy.Include || leftIndex != rightIndex
    case PairSpace.WithinUndirected(_, self) =>
      leftIndex < rightIndex || (self == SelfPolicy.Include && leftIndex == rightIndex)

  private val candidates = Relation.candidates(relation, right, (entry: (Int, KR)) => entry._2)

  def start: PairCursor[KL, KR] =
    new PairCursor(this, 0, 0, 0, false, Set.empty, Vector.empty, Vector.empty, 0)

  private[design] def advance(
      cursor: PairCursor[KL, KR],
      quantum: PairQuantum
  ): Either[PairScheduleError, PairPage[KL, KR]] =
    val page = enumerate(cursor, quantum.value, Some(budget.maxSelectedPairs))
    page.overflow match
      case Some(attempted) =>
        Left(
          PairScheduleError.SelectedBudget(relation.render, attempted, budget.maxSelectedPairs)
        )
      case None =>
        Right(page.next match
          case Left(report)  => PairPage.Done(page.pairs, page.workUnits, report)
          case Right(cursor) => PairPage.More(page.pairs, page.workUnits, cursor))

  /** Every scheduled pair and the complete report, in one traversal.
    *
    * No selected-pair cap applies: the realised vector is the only bound, which
    * is exactly the legacy `pair` contract. With `cap = None`, [[enumerate]]
    * never records an overflow, so this is total.
    */
  private[design] def complete: (Vector[ScheduledPair[KL, KR]], PairingReport[KL, KR]) =
    val pairs = Vector.newBuilder[ScheduledPair[KL, KR]]
    @annotation.tailrec
    def loop(cursor: PairCursor[KL, KR]): PairingReport[KL, KR] =
      val page = enumerate(cursor, Int.MaxValue, None)
      pairs ++= page.pairs
      page.next match
        case Left(report) => report
        case Right(next)  => loop(next)
    val report = loop(start)
    pairs.result() -> report

  /** One page of the exhaustive enumeration, shared by paged and complete traversal.
    *
    * `cap` is the selected-pair budget; `overflow` carries the attempted count
    * when the next accepted pair would exceed it, and is `None` whenever `cap`
    * is `None`.
    */
  private final case class Enumeration(
      pairs: Vector[ScheduledPair[KL, KR]],
      workUnits: Int,
      overflow: Option[Long],
      next: Either[PairingReport[KL, KR], PairCursor[KL, KR]]
  )

  private def enumerate(
      cursor: PairCursor[KL, KR],
      quantum: Int,
      cap: Option[Int]
  ): Enumeration =
    var l                      = cursor.leftPosition
    var r                      = cursor.rightPosition
    var reportPosition         = cursor.rightReportPosition
    var found                  = cursor.foundLeft
    var matchedRight           = cursor.matchedRight
    var unmatchedLeft          = cursor.unmatchedLeft
    var unmatchedRight         = cursor.unmatchedRight
    var selected               = cursor.selected
    var work                   = 0
    var overflow: Option[Long] = None
    val pairs                  = Vector.newBuilder[ScheduledPair[KL, KR]]

    while work < quantum && l < left.size && overflow.isEmpty do
      val group = candidates(left(l)._2)
      if group.isEmpty then
        if !incidentDiagnostics || !matchedRight.contains(left(l)._1) then
          unmatchedLeft :+= left(l)._2
        l += 1
      else
        val (li, lk) = left(l)
        val (ri, rk) = group(r)
        if acceptsPosition(li, ri) && relation.accepts(lk, rk) then
          if cap.contains(selected) then overflow = Some(selected.toLong + 1L)
          else
            pairs += ScheduledPair(li, ri, lk, rk)
            selected += 1
            matchedRight += ri
            if incidentDiagnostics then matchedRight += li
            found = true
        r += 1
        if r == group.size then
          if !found && (!incidentDiagnostics || !matchedRight.contains(li)) then
            unmatchedLeft :+= lk
          found = false
          r = 0
          l += 1
      work += 1

    while work < quantum && l == left.size && reportPosition < right.size && overflow.isEmpty
    do
      val (index, key) = right(reportPosition)
      if !matchedRight.contains(index) then unmatchedRight :+= key
      reportPosition += 1
      work += 1

    val next =
      if l == left.size && reportPosition == right.size then
        Left(
          PairingReport(
            pairSpace,
            selected.toLong,
            selected,
            unmatchedLeft,
            unmatchedRight,
            ambiguities
          )
        )
      else
        Right(
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
    Enumeration(pairs.result(), work, overflow, next)

/** Exhaustive between-collection traversal. Preparation retains source positions
  * and excludes every occurrence of duplicate keys; pages share the same engine
  * as [[WithinPairSchedule]] and ordinary exhaustive `pair` calls.
  */
final class DirectedPairSchedule[KL, KR] private[design] (
    private val engine: ExhaustivePairSchedule[KL, KR],
    val pairSpace: PairSpace.BetweenDirected
):
  def ambiguities: Vector[PairingAmbiguity[KL, KR]] = engine.ambiguities
  def candidatePairCount: Long                      = engine.candidatePairCount
  def budget: PairScheduleBudget                    = engine.budget
  def start: PairCursor[KL, KR]                     = engine.start
  private[design] def complete: (Vector[ScheduledPair[KL, KR]], PairingReport[KL, KR]) =
    engine.complete

/** Exhaustive within-collection traversal with explicit self/orientation policy.
  * Directed diagnostics distinguish incoming from outgoing edges. Canonically
  * undirected diagnostics report keys with no incident edge on both sides.
  * Budget planning conservatively counts the collection as both operands.
  * Bottom-k selection is not an exhaustive schedule.
  */
final class WithinPairSchedule[K] private[design] (
    private val engine: ExhaustivePairSchedule[K, K],
    val pairSpace: PairSpace.WithinDirected | PairSpace.WithinUndirected
):
  def ambiguities: Vector[PairingAmbiguity[K, K]] = engine.ambiguities
  def candidatePairCount: Long                    = engine.candidatePairCount
  def budget: PairScheduleBudget                  = engine.budget
  def start: PairCursor[K, K]                     = engine.start
  private[design] def complete: (Vector[ScheduledPair[K, K]], PairingReport[K, K]) =
    engine.complete

object WithinPairSchedule:
  def directed[K](
      keys: Vector[K],
      relation: Relation[K, K],
      self: SelfPolicy,
      budget: PairScheduleBudget = PairScheduleBudget.default
  ): Either[PairScheduleError, WithinPairSchedule[K]] =
    exhaustive(
      keys,
      relation,
      PairSpace.WithinDirected(relation.render, self, Selection.All),
      budget
    )

  /** Stores an edge only in increasing original source-position order; this
    * does not symmetrize an asymmetric relation. It matches Pairing's
    * canonicalUndirected contract exactly.
    */
  def canonicalUndirected[K](
      keys: Vector[K],
      relation: Relation[K, K],
      self: SelfPolicy,
      budget: PairScheduleBudget = PairScheduleBudget.default
  ): Either[PairScheduleError, WithinPairSchedule[K]] =
    exhaustive(keys, relation, PairSpace.WithinUndirected(relation.render, self), budget)

  private def exhaustive[K](
      keys: Vector[K],
      relation: Relation[K, K],
      space: PairSpace.WithinDirected | PairSpace.WithinUndirected,
      budget: PairScheduleBudget
  ): Either[PairScheduleError, WithinPairSchedule[K]] =
    budget.checkCounts(keys.size.toLong, keys.size.toLong).map { _ =>
      val duplicates = PairConstruction.duplicates(keys.map(key => Trial(key, (), ())))
      prepared(keys, relation, space, duplicates, budget)
    }

  private[design] def prepared[K, M, A](
      keys: Vector[K],
      relation: Relation[K, K],
      space: PairSpace.WithinDirected | PairSpace.WithinUndirected,
      duplicates: Vector[DuplicateTrials[K, M, A]],
      budget: PairScheduleBudget
  ): WithinPairSchedule[K] =
    new WithinPairSchedule(
      DirectedPairSchedule.engine(keys, keys, relation, duplicates, duplicates, budget, space),
      space
    )

object DirectedPairSchedule:
  def exhaustive[KL, KR](
      left: Vector[KL],
      right: Vector[KR],
      relation: Relation[KL, KR],
      budget: PairScheduleBudget = PairScheduleBudget.default
  ): Either[PairScheduleError, DirectedPairSchedule[KL, KR]] =
    budget.checkCounts(left.size.toLong, right.size.toLong).map { _ =>
      prepared(
        left,
        right,
        relation,
        PairConstruction.duplicates(left.map(key => Trial(key, (), ()))),
        PairConstruction.duplicates(right.map(key => Trial(key, (), ()))),
        budget
      )
    }

  /** Build from operands whose duplicate groups are already known, so pair
    * construction and paged scheduling share one exclusion rule and one
    * diagnostic source. Counts are not re-checked against the budget here.
    */
  private[design] def prepared[KL, ML, KR, MR, A, B](
      left: Vector[KL],
      right: Vector[KR],
      relation: Relation[KL, KR],
      leftDuplicates: Vector[DuplicateTrials[KL, ML, A]],
      rightDuplicates: Vector[DuplicateTrials[KR, MR, B]],
      budget: PairScheduleBudget
  ): DirectedPairSchedule[KL, KR] =
    val space = PairSpace.BetweenDirected(relation.render, Selection.All)
    new DirectedPairSchedule(
      engine(left, right, relation, leftDuplicates, rightDuplicates, budget, space),
      space
    )

  private[design] def engine[KL, ML, KR, MR, A, B](
      left: Vector[KL],
      right: Vector[KR],
      relation: Relation[KL, KR],
      leftDuplicates: Vector[DuplicateTrials[KL, ML, A]],
      rightDuplicates: Vector[DuplicateTrials[KR, MR, B]],
      budget: PairScheduleBudget,
      space: PairSpace
  ): ExhaustivePairSchedule[KL, KR] =
    val le         = PairConstruction.duplicateIndices(leftDuplicates)
    val re         = PairConstruction.duplicateIndices(rightDuplicates)
    val usableLeft = left.zipWithIndex.collect {
      case (key, index) if !le.contains(index) => index -> key
    }
    val usableRight = right.zipWithIndex.collect {
      case (key, index) if !re.contains(index) => index -> key
    }
    val ambiguities = leftDuplicates
      .map(d => PairingAmbiguity.DuplicateLeft[KL, KR](d.key, d.occurrences.map(_.index))) ++
      rightDuplicates.map(d =>
        PairingAmbiguity.DuplicateRight[KL, KR](d.key, d.occurrences.map(_.index))
      )
    new ExhaustivePairSchedule(
      usableLeft,
      usableRight,
      relation,
      ambiguities,
      usableLeft.size.toLong * usableRight.size.toLong,
      budget,
      space
    )
