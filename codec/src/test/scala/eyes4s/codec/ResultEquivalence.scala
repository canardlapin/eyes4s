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

package eyes4s.codec

import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.plan.*

/** Structural identity of two results for codec tests; the published version
  * for downstream law suites is `eyes4s.laws.StudyResultEquivalence`.
  */
object ResultEquivalence:
  def same[K, U <: Unit2D, S, D](a: StudyResult[K, U, S, D], b: StudyResult[K, U, S, D])(
      scores: (S, S) => Boolean,
      differences: (D, D) => Boolean
  ): Boolean =
    a.input == b.input && a.description == b.description && a.scales.size == b.scales.size &&
      a.scales.zip(b.scales).forall { case (x, y) => sameScale(x, y)(scores, differences) }

  def sameScale[K, U <: Unit2D, S, D](
      a: StudyScaleResult[K, U, S, D],
      b: StudyScaleResult[K, U, S, D]
  )(scores: (S, S) => Boolean, differences: (D, D) => Boolean): Boolean =
    a.estimate == b.estimate && a.excludedPhases == b.excludedPhases &&
      a.estimation.size == b.estimation.size &&
      a.estimation.zip(b.estimation).forall {
        case ((k, Right(x)), (l, Right(y))) =>
          k == l && x.grid == y.grid && x.values.toVector == y.values.toVector &&
          x.provenance == y.provenance
        case ((k, Left(x)), (l, Left(y))) => k == l && x == y
        case _                            => false
      } &&
      ((a.contrast, b.contrast) match
        case (Right(x), Right(y)) => sameContrast(x, y)(scores, differences)
        case (Left(x), Left(y))   => x == y
        case _                    => false)

  def sameContrast[K, S, D](a: Contrast[K, S, D], b: Contrast[K, S, D])(
      scores: (S, S) => Boolean,
      differences: (D, D) => Boolean
  ): Boolean =
    sameAnalysis(a.matched, b.matched)(scores) && sameAnalysis(a.control, b.control)(scores) &&
      a.rows.size == b.rows.size &&
      a.rows.zip(b.rows).forall { case (x, y) =>
        x.key == y.key && sameOption(x.matched, y.matched)(scores) &&
        sameOption(x.control, y.control)(scores) &&
        ((x.difference, y.difference) match
          case (Right(p), Right(q)) => differences(p, q)
          case (Left(p), Left(q))   => p == q
          case _                    => false)
      }

  def sameAnalysis[K, S](a: Analysis[K, S], b: Analysis[K, S])(
      scores: (S, S) => Boolean
  ): Boolean =
    a.entries.size == b.entries.size &&
      a.entries.zip(b.entries).forall { case (x, y) => sameRow(x, y)(scores) } &&
      a.diagnostics == b.diagnostics && a.provenance == b.provenance &&
      a.evaluation == b.evaluation && a.source.diagnostics == b.source.diagnostics &&
      a.source.provenance == b.source.provenance && a.source.rows.size == b.source.rows.size &&
      a.source.rows.zip(b.source.rows).forall { case (x, y) =>
        x.left == y.left && x.right == y.right &&
        ((x.result, y.result) match
          case (Right(p), Right(q)) => scores(p, q)
          case (Left(p), Left(q))   => p == q
          case _                    => false)
      }

  private def sameOption[K, S](a: Option[ReductionRow[K, S]], b: Option[ReductionRow[K, S]])(
      scores: (S, S) => Boolean
  ): Boolean = (a, b) match
    case (Some(x), Some(y)) => sameRow(x, y)(scores)
    case (None, None)       => true
    case _                  => false

  def sameRow[K, S](a: ReductionRow[K, S], b: ReductionRow[K, S])(
      scores: (S, S) => Boolean
  ): Boolean =
    a.key == b.key && a.successful == b.successful && a.failed == b.failed &&
      a.contributing == b.contributing &&
      ((a.result, b.result) match
        case (Right(p), Right(q)) => scores(p, q)
        case (Left(p), Left(q))   => p == q
        case _                    => false)
