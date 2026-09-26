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

package eyes4s.compare

import eyes4s.core.*
import eyes4s.kernel.*

/** A scanpath long enough for eyesim's baseline scanpath comparison: at least
  * [[BaselineScanpath.MinimumFixations]] fixations, where eyesim returns
  * missing components for anything shorter. Native [[MultiMatch]] itself
  * admits two-fixation paths; this type states the stricter baseline
  * cardinality once, so a short path is refused before comparison.
  */
final class BaselineScanpath[U <: Unit2D] private (val path: Scanpath[U])

object BaselineScanpath:
  /** eyesim `multi_match` requires three fixations per operand. */
  val MinimumFixations: Int = 3

  /** Admit `path` as the `operand` side of a baseline comparison; a shorter
    * path is [[CompareError.TooShort]] naming the operand, its fixation count
    * and the minimum.
    */
  def of[U <: Unit2D](
      path: Scanpath[U],
      operand: CompareOperand
  ): Either[CompareError, BaselineScanpath[U]] =
    Either.cond(
      path.n >= MinimumFixations,
      new BaselineScanpath(path),
      CompareError.TooShort(s"${operand.render} scanpath", path.n, MinimumFixations)
    )

object ScanpathComparison:
  /** The eyesim baseline scanpath comparison: [[MultiMatch]] over two
    * scanpaths admitted at the baseline cardinality. The five MultiMatch
    * components are the result; eyesim's sixth, transport-backed position
    * component has no native exact-EMD solver and no slot here.
    */
  def baseline[U <: Unit2D](
      left: BaselineScanpath[U],
      right: BaselineScanpath[U]
  ): Either[CompareError, MultiMatchScore] =
    MultiMatch[U].compare(left.path, right.path)
