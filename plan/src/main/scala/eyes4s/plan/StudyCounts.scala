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
    private[plan] val pairingRefusal: Option[PlanError]
):
  val pairRowsPerScale: Long   = matched.eligiblePairs + controls.eligiblePairs
  val totalPairs: Long         = pairRowsPerScale * scales
  val totalMaps: Long          = mapsPerScale * scales
  val totalReductionKeys: Long = keysPerDesign * 2L * scales
  val totalContrastRows: Long  = keysPerDesign * scales

/** Nominal evidence that completed counts came from this exact preparation. */
private[plan] final class StudyCountIdentity
