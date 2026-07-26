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

import eyes4s.kernel.Provenance

/** A source trial together with its stable position in a [[Trials]] value. */
final case class IndexedTrial[K, M, A] private[design] (
    index: Int,
    trial: Trial[K, M, A]
) derives CanEqual

/** Every occurrence of a duplicate full key.
  *
  * Pair construction excludes all these occurrences rather than resolving the
  * duplicate to the first row. The complete source trials remain available to
  * the caller as diagnostic data.
  */
final case class DuplicateTrials[K, M, A] private[design] (
    key: K,
    occurrences: Vector[IndexedTrial[K, M, A]]
) derives CanEqual

/** Duplicate-key diagnostics for the two pairing operands. */
final case class PairingAmbiguities[KL, ML, KR, MR, A, B](
    left: Vector[DuplicateTrials[KL, ML, A]],
    right: Vector[DuplicateTrials[KR, MR, B]]
) derives CanEqual:
  def isEmpty: Boolean  = left.isEmpty && right.isEmpty
  def nonEmpty: Boolean = !isEmpty

/** A key-level ambiguity retained after source values have been evaluated. */
sealed trait PairingAmbiguity[KL, KR] derives CanEqual

object PairingAmbiguity:
  final case class DuplicateLeft[KL, KR](key: KL, indices: Vector[Int])
      extends PairingAmbiguity[KL, KR]

  final case class DuplicateRight[KL, KR](key: KR, indices: Vector[Int])
      extends PairingAmbiguity[KL, KR]

/** How the selected edges are oriented and stored. */
enum PairStorage derives CanEqual:
  case BetweenDirected, WithinDirected, WithinUndirected

/** Auditable diagnostics shared by pairing and pair evaluation. */
final case class PairingReport[KL, KR](
    storage: PairStorage,
    eligiblePairCount: Long,
    selectedPairCount: Int,
    unmatchedLeft: Vector[KL],
    unmatchedRight: Vector[KR],
    ambiguous: Vector[PairingAmbiguity[KL, KR]]
) derives CanEqual

/** Selected source pairs plus every pairing diagnostic.
  *
  * `eligiblePairCount` is measured before directed bottom-k selection;
  * `pairs.size` is the realised selected count. Ambiguous rows are not present
  * in `pairs`, `unmatchedLeft`, or `unmatchedRight`: they are retained in full
  * under `ambiguous`.
  */
final case class Paired[KL, ML, KR, MR, A, B] private[design] (
    pairs: Vector[(Trial[KL, ML, A], Trial[KR, MR, B])],
    eligiblePairCount: Long,
    unmatchedLeft: Vector[KL],
    unmatchedRight: Vector[KR],
    ambiguous: PairingAmbiguities[KL, ML, KR, MR, A, B],
    storage: PairStorage
) derives CanEqual:
  def selectedPairCount: Int = pairs.size

  def diagnostics: PairingReport[KL, KR] =
    PairingReport(
      storage,
      eligiblePairCount,
      selectedPairCount,
      unmatchedLeft,
      unmatchedRight,
      ambiguous.left.map(duplicate =>
        PairingAmbiguity.DuplicateLeft[KL, KR](
          duplicate.key,
          duplicate.occurrences.map(_.index)
        )
      ) ++ ambiguous.right.map(duplicate =>
        PairingAmbiguity.DuplicateRight[KL, KR](
          duplicate.key,
          duplicate.occurrences.map(_.index)
        )
      )
    )

/** One evaluated edge. Both source keys survive success or failure. */
final case class PairScore[KL, KR, E, S](
    left: KL,
    right: KR,
    result: Either[E, S]
) derives CanEqual

/** The primary, auditable result of pair evaluation.
  *
  * Reductions derive an `Analysis`; they never replace this edge-level result.
  */
final case class PairwiseAnalysis[KL, KR, E, S] private[design] (
    rows: Vector[PairScore[KL, KR, E, S]],
    diagnostics: PairingReport[KL, KR],
    provenance: Provenance
) derives CanEqual

/** Pair two distinct trial collections under a directed design. */
def pair[KL, ML, KR, MR, A, B](
    left: Trials[KL, ML, A],
    right: Trials[KR, MR, B],
    design: PairDesign.BetweenDirected[KL, KR]
)(using KeyDigest[KL], KeyDigest[KR]): Paired[KL, ML, KR, MR, A, B] =
  PairConstruction.between(left, right, design)

/** Pair one collection with itself while retaining orientation. */
def pair[K, M, A](
    trials: Trials[K, M, A],
    design: PairDesign.WithinDirected[K]
)(using KeyDigest[K]): Paired[K, M, K, M, A, A] =
  PairConstruction.withinDirected(trials, design)

/** Pair one collection with itself, storing every unordered edge once. */
def pair[K, M, A](
    trials: Trials[K, M, A],
    design: PairDesign.WithinUndirected[K]
): Paired[K, M, K, M, A, A] =
  PairConstruction.withinUndirected(trials, design)

private object PairConstruction:

  def between[KL, ML, KR, MR, A, B](
      left: Trials[KL, ML, A],
      right: Trials[KR, MR, B],
      design: PairDesign.BetweenDirected[KL, KR]
  )(using KeyDigest[KL], KeyDigest[KR]): Paired[KL, ML, KR, MR, A, B] =
    val leftDuplicates  = duplicates(left.rows)
    val rightDuplicates = duplicates(right.rows)
    val excludedLeft    = duplicateIndices(leftDuplicates)
    val excludedRight   = duplicateIndices(rightDuplicates)
    val usableLeft      = indexed(left.rows).filterNot(row => excludedLeft.contains(row.index))
    val usableRight = indexed(right.rows).filterNot(row => excludedRight.contains(row.index))

    val selected          = Vector.newBuilder[(Trial[KL, ML, A], Trial[KR, MR, B])]
    val unmatchedLeft     = Vector.newBuilder[KL]
    val eligibleRight     = scala.collection.mutable.Set.empty[Int]
    var eligiblePairCount = 0L

    usableLeft.foreach { leftRow =>
      val eligible =
        usableRight.filter(rightRow =>
          design.relation.accepts(leftRow.trial.key, rightRow.trial.key)
        )

      if eligible.isEmpty then unmatchedLeft += leftRow.trial.key
      else
        eligiblePairCount += eligible.size.toLong
        eligible.foreach(row => eligibleRight += row.index)
        select(leftRow.trial.key, eligible, design.selection).foreach(rightRow =>
          selected += leftRow.trial -> rightRow.trial
        )
    }

    val unmatchedRight =
      usableRight.collect {
        case row if !eligibleRight.contains(row.index) => row.trial.key
      }

    Paired(
      selected.result(),
      eligiblePairCount,
      unmatchedLeft.result(),
      unmatchedRight,
      PairingAmbiguities(leftDuplicates, rightDuplicates),
      PairStorage.BetweenDirected
    )

  def withinDirected[K, M, A](
      trials: Trials[K, M, A],
      design: PairDesign.WithinDirected[K]
  )(using KeyDigest[K]): Paired[K, M, K, M, A, A] =
    val foundDuplicates = duplicates(trials.rows)
    val excluded        = duplicateIndices(foundDuplicates)
    val usable          = indexed(trials.rows).filterNot(row => excluded.contains(row.index))

    val selected          = Vector.newBuilder[(Trial[K, M, A], Trial[K, M, A])]
    val unmatchedLeft     = Vector.newBuilder[K]
    val eligibleRight     = scala.collection.mutable.Set.empty[Int]
    var eligiblePairCount = 0L

    usable.foreach { leftRow =>
      val eligible = usable.filter { rightRow =>
        val selfAllowed =
          design.self == SelfPolicy.Include || leftRow.index != rightRow.index
        selfAllowed && design.relation.accepts(leftRow.trial.key, rightRow.trial.key)
      }

      if eligible.isEmpty then unmatchedLeft += leftRow.trial.key
      else
        eligiblePairCount += eligible.size.toLong
        eligible.foreach(row => eligibleRight += row.index)
        select(leftRow.trial.key, eligible, design.selection).foreach(rightRow =>
          selected += leftRow.trial -> rightRow.trial
        )
    }

    val unmatchedRight =
      usable.collect {
        case row if !eligibleRight.contains(row.index) => row.trial.key
      }
    val ambiguities =
      PairingAmbiguities[K, M, K, M, A, A](foundDuplicates, foundDuplicates)

    Paired(
      selected.result(),
      eligiblePairCount,
      unmatchedLeft.result(),
      unmatchedRight,
      ambiguities,
      PairStorage.WithinDirected
    )

  def withinUndirected[K, M, A](
      trials: Trials[K, M, A],
      design: PairDesign.WithinUndirected[K]
  ): Paired[K, M, K, M, A, A] =
    val foundDuplicates = duplicates(trials.rows)
    val excluded        = duplicateIndices(foundDuplicates)
    val usable          = indexed(trials.rows).filterNot(row => excluded.contains(row.index))

    val selected = Vector.newBuilder[(Trial[K, M, A], Trial[K, M, A])]
    val incident = scala.collection.mutable.Set.empty[Int]

    var leftPosition      = 0
    var eligiblePairCount = 0L
    while leftPosition < usable.size do
      val firstRight =
        if design.self == SelfPolicy.Include then leftPosition else leftPosition + 1
      var rightPosition = firstRight
      while rightPosition < usable.size do
        val leftRow  = usable(leftPosition)
        val rightRow = usable(rightPosition)
        if design.relation.accepts(leftRow.trial.key, rightRow.trial.key) then
          selected += leftRow.trial -> rightRow.trial
          eligiblePairCount += 1L
          incident += leftRow.index
          incident += rightRow.index
        rightPosition += 1
      leftPosition += 1

    val unmatched =
      usable.collect {
        case row if !incident.contains(row.index) => row.trial.key
      }
    val ambiguities =
      PairingAmbiguities[K, M, K, M, A, A](foundDuplicates, foundDuplicates)

    Paired(
      selected.result(),
      eligiblePairCount,
      unmatched,
      unmatched,
      ambiguities,
      PairStorage.WithinUndirected
    )

  private def select[KL, KR, MR, B](
      focal: KL,
      eligible: Vector[IndexedTrial[KR, MR, B]],
      selection: Selection
  )(using KeyDigest[KL], KeyDigest[KR]): Vector[IndexedTrial[KR, MR, B]] =
    selection match
      case Selection.All                          => eligible
      case Selection.BottomK(cap, seed, sampleId) =>
        Selection.bottomKBy(
          focal,
          eligible,
          _.trial.key,
          cap.value,
          seed,
          sampleId
        )

  private def indexed[K, M, A](
      rows: Vector[Trial[K, M, A]]
  ): Vector[IndexedTrial[K, M, A]] =
    rows.zipWithIndex.map { case (trial, index) => IndexedTrial(index, trial) }

  private def duplicates[K, M, A](
      rows: Vector[Trial[K, M, A]]
  ): Vector[DuplicateTrials[K, M, A]] =
    val seen       = scala.collection.mutable.Set.empty[Int]
    val duplicates = Vector.newBuilder[DuplicateTrials[K, M, A]]

    rows.indices.foreach { index =>
      if !seen.contains(index) then
        val matching =
          rows.indices.filter(other => rows(other).key == rows(index).key).toVector
        matching.foreach(seen += _)
        if matching.size > 1 then
          duplicates += DuplicateTrials(
            rows(index).key,
            matching.map(other => IndexedTrial(other, rows(other)))
          )
    }

    duplicates.result()

  private def duplicateIndices[K, M, A](
      duplicates: Vector[DuplicateTrials[K, M, A]]
  ): Set[Int] =
    duplicates.iterator.flatMap(_.occurrences.iterator.map(_.index)).toSet

end PairConstruction
