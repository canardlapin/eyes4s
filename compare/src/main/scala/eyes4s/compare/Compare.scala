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

import eyes4s.kernel.*
import eyes4s.surface.EstimateError

/** What kind of number a measure produces, and what range it lives in.
  *
  * `eyesim` has to explain this in vignette prose -- "a note on units", telling
  * the reader that Fisher-z is unbounded while Pearson is not -- because the
  * information exists nowhere in the code. A caller averaging correlations
  * without transforming them, or treating a distance as a similarity, gets no
  * warning from anything.
  */
enum MeasureScale derives CanEqual:
  /** `[-1, 1]`, and not safe to average directly. */
  case Correlation

  /** Unbounded; the transform that makes correlations averageable. */
  case FisherZ

  /** Unbounded similarity where larger values mean greater agreement.
    *
    * Distinct from [[FisherZ]]: an NSS score is unbounded and similarity-shaped
    * but is not a transformed correlation.
    */
  case UnboundedSimilarity

  /** `[0, 1]`. */
  case Probability

  case Bounded(lo: Double, hi: Double)

  /** `[0, inf)`, and lower means closer -- the opposite ordering to the rest. */
  case DistanceLike

  def render: String = this match
    case Correlation         => "correlation [-1, 1]"
    case FisherZ             => "Fisher z (unbounded)"
    case UnboundedSimilarity => "unbounded similarity, higher is closer"
    case Probability         => "probability [0, 1]"
    case Bounded(l, h)       => f"bounded [$l%.3g, $h%.3g]"
    case DistanceLike        => "distance [0, inf), lower is closer"

/** Everything an application needs to present a measure without hardcoding a
  * table (PRD APP-15).
  */
final case class MeasureInfo(
    name: String,
    summary: String,
    scale: MeasureScale,
    citation: Option[String]
):
  def render: String = s"$name -- ${scale.render}"

/** Which input of a comparison caused a structured failure. */
enum CompareOperand derives CanEqual:
  case Left
  case Right
  case Both
  case Model
  case Observed

  def render: String = this match
    case Left     => "left input"
    case Right    => "right input"
    case Both     => "both inputs"
    case Model    => "model"
    case Observed => "observed data"

/** Failures comparing two things. */
enum CompareError derives CanEqual:
  case Grids(underlying: SurfaceError)
  case Frames(underlying: GeometryError)
  case Estimation(underlying: EstimateError)
  case ConstantInput(measure: String, operand: CompareOperand)
  case EmptyInput(measure: String, operand: CompareOperand, total: Double)
  case ZeroNorm(measure: String, leftNorm: Double, rightNorm: Double)
  case NonPositiveDirections(measure: String, directions: Int)
  case CostMatrixLimitExceeded(measure: String, cells: Int, limit: Int)
  case NonPositiveRegularisation(measure: String, value: Double)
  case NonPositiveIterations(measure: String, iterations: Int)
  case NonPositiveCellLimit(measure: String, limit: Int)
  case TooShort(what: String, got: Int, needed: Int)

  def message: String = this match
    case Grids(e)                        => e.message
    case Frames(e)                       => e.message
    case Estimation(e)                   => e.message
    case ConstantInput(measure, operand) =>
      s"$measure is undefined because the ${operand.render} is constant."
    case EmptyInput(measure, operand, total) =>
      s"$measure needs positive mass in the ${operand.render}, got total=$total."
    case ZeroNorm(measure, left, right) =>
      s"$measure needs non-zero input norms, got left=$left and right=$right."
    case NonPositiveDirections(measure, directions) =>
      s"$measure needs at least one direction, got $directions."
    case CostMatrixLimitExceeded(measure, cells, limit) =>
      s"$measure would form a ${cells}x$cells cost matrix, above the " +
        s"$limit-cell limit. Use sliced Wasserstein, which is linear in the grid."
    case NonPositiveRegularisation(measure, value) =>
      s"$measure needs positive regularisation, got $value."
    case NonPositiveIterations(measure, iterations) =>
      s"$measure needs at least one iteration, got $iterations."
    case NonPositiveCellLimit(measure, limit) =>
      s"$measure needs a positive cell limit, got $limit."
    case TooShort(w, g, n) =>
      s"Comparing $w needs at least $n elements, got $g."

/** A comparison of an `A` with a `B`, yielding a typed score.
  *
  * ==Heterogeneous on purpose==
  *
  * The interesting comparisons in this field are not same-type. A saliency
  * model is a map and the data it is scored against is a point set; a template
  * is a map and a trial may be a path. `eyesim` has no way to express the
  * map-versus-points case at all, which is why the entire saliency-benchmark
  * literature is outside its reach.
  *
  * ==The result is a type, not a shape==
  *
  * `S` is whatever the measure actually produces. MultiMatch produces five
  * named numbers, and it says so. `eyesim` returns a scalar, an unnamed vector,
  * or a named vector depending on the method and an aggregation flag, which is
  * why it needs ninety-five lines of reshaping machinery gated on a string
  * comparison against the literal "multimatch".
  */
trait Compare[-A, -B, +S]:
  def info: MeasureInfo
  def compare(a: A, b: B): Either[CompareError, S]

  final def scale: MeasureScale = info.scale

/** A comparison that is symmetric in its arguments, without necessarily
  * satisfying the stronger laws of a metric.
  *
  * ==Why this is its own interface== (PRD C-9)
  *
  * Evaluating an unordered pair requires knowing that the answer does not
  * depend on which element you call first. Most comparisons here are symmetric,
  * but not all -- a divergence is not, and neither is a map-versus-points score
  * where the two sides are different kinds of thing. Demanding a `Metric` would
  * be too strong, since cosine similarity and the MultiMatch aggregate are both
  * symmetric and neither is a metric.
  *
  * So symmetry gets an interface of its own, and an unordered evaluation
  * requires it rather than quietly picking an orientation. Every instance
  * carries a symmetry law suite.
  */
trait SymmetricCompare[A, +S] extends Compare[A, A, S]

/** A true metric: identity of indiscernibles, symmetry, triangle inequality.
  *
  * Shipping under this interface is a claim, and `eyes4s-laws` checks it.
  */
trait Metric[A] extends SymmetricCompare[A, MeasureDistance]:
  def distance(a: A, b: A): Either[CompareError, MeasureDistance] = compare(a, b)

/** Symmetric, zero on identical inputs, but with no triangle inequality.
  *
  * Where most "distances" in this field actually live.
  */
trait Semimetric[A] extends SymmetricCompare[A, MeasureDistance]

/** Zero on identical inputs and non-negative, but not symmetric.
  *
  * KL is the obvious member. A divergence cannot be used for an unordered
  * comparison, and the absence of [[SymmetricCompare]] in its ancestry is what
  * prevents it.
  */
trait Divergence[A] extends Compare[A, A, MeasureDistance]

/** Symmetric and positive semi-definite: an inner-product-like similarity. */
trait Kernel[A] extends SymmetricCompare[A, Similarity]

/** A non-negative separation produced by a comparison measure.
  *
  * The explicit name keeps it distinct from the kernel's spatial
  * `Distance[U]`, which is a length in frame units, without leaking an
  * implementation-oriented suffix into user code.
  */
opaque type MeasureDistance = Double

object MeasureDistance:
  def apply(v: Double): MeasureDistance = v
  extension (d: MeasureDistance)
    def value: Double  = d
    def render: String = f"$d%.6g"
  given cats.kernel.Order[MeasureDistance] =
    cats.kernel.Order.from((a, b) => java.lang.Double.compare(a, b))

/** A similarity: higher means more alike. */
opaque type Similarity = Double

object Similarity:
  def apply(v: Double): Similarity = v
  extension (s: Similarity)
    def value: Double  = s
    def render: String = f"$s%.6g"
  given cats.kernel.Order[Similarity] =
    cats.kernel.Order.from((a, b) => java.lang.Double.compare(a, b))
