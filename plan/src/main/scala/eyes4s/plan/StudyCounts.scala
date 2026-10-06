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

import eyes4s.kernel.Provenance

/** Exact schedule counts before numerical evaluation. Unmatched and ambiguous
  * keys remain distinct; a window or estimation failure does not change eligibility.
  */
final class DesignCounts private[plan] (
    val eligiblePairs: Long,
    val focalWithPairs: Long,
    val unmatchedFocal: Long,
    val unmatchedReferences: Long,
    val ambiguousKeys: Long
)

/** Exact scheduled pairs for one focal key, before scoring or reduction.
  * A single matched reference is present exactly when matchedPairs is one;
  * multiple references remain visible in the count and matched cardinality.
  * Instances are produced only by the prepared study's bounded count cursor.
  * Each design caps its total selected pairs at PairScheduleBudget.maxSelectedPairs
  * (an Int); one key's count cannot exceed that total. Repeated full focal
  * keys are excluded by the schedule in all occurrences.
  */
final class QueryPairCounts[K] private[plan] (
    val matchedPairs: Int,
    val singleMatchedReference: Option[K],
    val controlPairs: Int
):
  private[plan] def addMatched(reference: K): QueryPairCounts[K] =
    new QueryPairCounts(
      matchedPairs + 1,
      Option.when(matchedPairs == 0)(reference),
      controlPairs
    )
  private[plan] def addControl: QueryPairCounts[K] =
    new QueryPairCounts(matchedPairs, singleMatchedReference, controlPairs + 1)

private[plan] object QueryPairCounts:
  def empty[K]: QueryPairCounts[K] = new QueryPairCounts(0, None, 0)

/** Matched and control counts from a single traversal of each schedule.
  * Cardinality is obtained from that same matched traversal. Totals count
  * attempted maps and pair rows, including failed outcomes, over every scale.
  */
final class StudyCounts[K] private[plan] (
    val matched: DesignCounts,
    val controls: DesignCounts,
    val cardinality: MatchedCardinality[K],
    val eligibleQueries: Long,
    val mapsPerScale: Long,
    val scales: Int,
    private[plan] val input: ArtifactRef[?],
    private[plan] val description: Vector[(String, Vector[Provenance.Param])],
    private[plan] val owner: StudyCountIdentity,
    val keysPerDesign: Long,
    private[plan] val pairingRefusal: Option[PlanError],
    /** Keys the control reduction has per scale: the focal keys with a
      * matched reference (bead S0.7b); [[keysPerDesign]] is every focal key,
      * which the matched reduction and the contrast have.
      */
    private[plan] val controlKeys: Long,
    /** Every distinct requested focal key, including unmatched and ambiguous
      * keys, with exact pair metadata from the same bounded traversal.
      */
    val queryPairs: Map[K, QueryPairCounts[K]]
):
  val pairRowsPerScale: Long   = matched.eligiblePairs + controls.eligiblePairs
  val totalPairs: Long         = pairRowsPerScale * scales
  val totalMaps: Long          = mapsPerScale * scales
  val totalReductionKeys: Long = (keysPerDesign + controlKeys) * scales
  val totalContrastRows: Long  = keysPerDesign * scales

/** Nominal evidence that completed counts came from this exact preparation. */
private[plan] final class StudyCountIdentity
