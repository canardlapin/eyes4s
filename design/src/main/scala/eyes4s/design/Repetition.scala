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

import eyes4s.compare.*
import eyes4s.kernel.ContentHash

/** Repetition comparisons within participant, across distinct occasions.
  *
  * Targets share a stimulus; controls have a different stimulus. Both are directed
  * from the focal (left) trial, exclude self, and retain full-key ambiguity reports.
  * An occasion can be a phase or a (phase, repetition) projection, chosen explicitly.
  * No phase, participant or stimulus is inferred from row order or display strings.
  *
  * This is a convenience over PairDesign, pair, evaluatePairs and contrast, not
  * another sampler or execution engine (bead eyesim-repetition).
  */
final class RepetitionDesign[K] private (
    val matched: PairDesign.WithinDirected[K],
    val controls: PairDesign.WithinDirected[K]
):
  /** Preserve both endpoint keys, failed scores and selection diagnostics.
    * The specification must declare all score-affecting preprocessing and units.
    */
  def evaluate[M, A, S](
      trials: Trials[K, M, A],
      inputs: ContentHash,
      comparison: Compare[A, A, S],
      specification: EvaluationSpec
  )(using KeyDigest[K]): RepetitionResult[K, S] =
    val info = EvaluationInfo.comparison(comparison, specification)
    new RepetitionResult(
      evaluatePairs(pair(trials, matched), inputs, info)(comparison.compare),
      evaluatePairs(pair(trials, controls), inputs, info)(comparison.compare)
    )

object RepetitionDesign:
  /** Controls are selected only after participant, occasion and stimulus exclusions.
    * Selection.BottomK uses the existing keyed sampler; raising its cap preserves
    * its priorities. The default is exhaustive. Duplicate full keys are excluded
    * entirely and reported by the ordinary pair constructor.
    */
  def withinParticipant[K, P, I, O](
      participant: Projection[K, P],
      stimulus: Projection[K, I],
      occasion: Projection[K, O],
      controlSelection: Selection = Selection.All
  ): RepetitionDesign[K] =
    val across = Pairing.within[K].sameOn(participant).differentOn(occasion).excludingSelf
    new RepetitionDesign(
      across.sameOn(stimulus).directed,
      across.differentOn(stimulus).directed.copy(selection = controlSelection)
    )

/** Directed repetition edges before any reduction; eligibility is not success. */
final class RepetitionResult[K, S] private[design] (
    val matched: DirectedPairwiseAnalysis[K, K, CompareError, S],
    val controls: DirectedPairwiseAnalysis[K, K, CompareError, S]
):
  /** Same-stimulus mean minus different-stimulus mean for each focal trial.
    * The supplied failure policy governs both means; failed/missing keys and
    * realized contributing counts remain in the ordinary Contrast result.
    */
  def contrast[D](policy: FailurePolicy)(using
      ScoreMean[S],
      Contrastable[S, D],
      Ordering[K]
  ): Either[ContrastError[K], Contrast[K, S, D]] =
    eyes4s.design.contrast(matched.meanByLeft(policy), controls.meanByLeft(policy))
