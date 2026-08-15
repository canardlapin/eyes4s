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

/** The five MultiMatch dimensions, as five named fields.
  *
  * ==A product type, not a named vector== (PRD C-4)
  *
  * `eyesim` returns a bare named numeric vector here, which is why every batch
  * use of it in its own vignette is `lapply(...) |> bind_rows()`, and why its
  * analysis engine needs ninety-five lines of reshaping gated on
  * `identical(method, "multimatch")` -- a string comparison deciding the shape
  * of a result. A product type makes all of that unnecessary.
  *
  * Every dimension is in `[0, 1]` with higher meaning more similar.
  */
final class MultiMatchScore private (
    shapeScore: Similarity01,
    directionScore: Similarity01,
    lengthScore: Similarity01,
    positionScore: Similarity01,
    durationScore: Similarity01
) derives CanEqual:

  def shape: Double     = shapeScore.value
  def direction: Double = directionScore.value
  def length: Double    = lengthScore.value
  def position: Double  = positionScore.value
  def duration: Double  = durationScore.value

  /** The unweighted mean of the five, for callers who want one number.
    *
    * Offered, but not the default and not the type: the dimensions answer
    * different questions and averaging them discards exactly the information
    * MultiMatch exists to provide.
    */
  def mean: Double = (shape + direction + length + position + duration) / 5.0

  def render: String =
    f"multimatch(shape=$shape%.3f dir=$direction%.3f len=$length%.3f " +
      f"pos=$position%.3f dur=$duration%.3f)"

  override def equals(other: Any): Boolean = other match
    case that: MultiMatchScore =>
      shape == that.shape && direction == that.direction && length == that.length &&
      position == that.position && duration == that.duration
    case _ => false

  override def hashCode: Int =
    Seq(shape, direction, length, position, duration).hashCode

  override def toString: String = render

object MultiMatchScore:
  def of(
      shape: Double,
      direction: Double,
      length: Double,
      position: Double,
      duration: Double
  ): Either[ComparisonValueError, MultiMatchScore] =
    for
      shapeScore     <- Similarity01.of("shape", shape)
      directionScore <- Similarity01.of("direction", direction)
      lengthScore    <- Similarity01.of("length", length)
      positionScore  <- Similarity01.of("position", position)
      durationScore  <- Similarity01.of("duration", duration)
    yield new MultiMatchScore(
      shapeScore,
      directionScore,
      lengthScore,
      positionScore,
      durationScore
    )

/** MultiMatch scanpath comparison (Jarodzka et al. 2010; Dewhurst et al. 2012).
  *
  * ==What it is==
  *
  * Align two saccade sequences by the similarity of their movement vectors,
  * then report five separate median differences over the aligned pairs. The
  * point of the method is that two scanpaths can be alike in shape and unalike
  * in position, or alike in timing and unalike in direction, and one number
  * cannot say which.
  *
  * ==Symmetric, and not a metric==
  *
  * Symmetric: the cost matrix transposes and the medians run over the same
  * pairs. Not a metric, and this is worth stating plainly because the values
  * look like similarities and invite being treated as one: a median of
  * differences does not satisfy the triangle inequality, so it ships as
  * [[SymmetricCompare]] and nothing stronger (PRD C-3).
  *
  * ==Normalisation is by the frame's diagonal==
  *
  * Which is carried by the scanpath, not passed as an argument. `eyesim`
  * requires a `screensize` at every call site, recomputes the diagonal inline
  * at five separate places, and errors if the argument is missing -- a value
  * that was available all along on the data.
  *
  * ==Simplification is not implemented==
  *
  * Canonical MultiMatch optionally merges short adjacent saccades before
  * comparing, controlled by amplitude, direction and duration thresholds. That
  * step is absent here, as it is absent from eyesim's R implementation -- which
  * has it only through a Python bridge. Use [[Merge]] beforehand if you want
  * it; the difference is stated rather than silently absorbed.
  */
object MultiMatch:

  def apply[U <: Unit2D]: SymmetricCompare[Scanpath[U], MultiMatchScore] =
    new SymmetricCompare[Scanpath[U], MultiMatchScore]:

      val info = MeasureInfo(
        "MultiMatch",
        "five-dimensional scanpath comparison; symmetric, and NOT a metric",
        MeasureScale.Bounded(0.0, 1.0),
        Some("Jarodzka, Holmqvist & Nystrom (2010); Dewhurst et al. (2012)")
      )

      def compare(
          a: Scanpath[U],
          b: Scanpath[U]
      ): Either[CompareError, MultiMatchScore] =
        for
          _ <- Agreement.frames(a.frame, b.frame).left.map(CompareError.Frames.apply)
          sa = a.transitions
          sb = b.transitions
          _    <- Either.cond(sa.nonEmpty, (), CompareError.TooShort("a scanpath", a.n, 2))
          _    <- Either.cond(sb.nonEmpty, (), CompareError.TooShort("a scanpath", b.n, 2))
          path <- Alignment.monotoneLattice.align(sa, sb) { (x, y) =>
            // Aligned on the SHAPE dimension alone -- the difference between
            // the two movement vectors -- and the other four dimensions are
            // then read off the correspondence it produces.
            val dx = x.displacement.dx - y.displacement.dx
            val dy = x.displacement.dy - y.displacement.dy
            math.hypot(dx, dy)
          }
          result <- score(a, b, sa, sb, path)
        yield result

      private def score(
          left: Scanpath[U],
          right: Scanpath[U],
          sa: Vector[ScanpathTransition[U]],
          sb: Vector[ScanpathTransition[U]],
          path: AlignmentPath
      ): Either[CompareError, MultiMatchScore] =
        val diagonal = left.frame.diagonal
        val pairs    = path.matches

        def median(values: Vector[Double]): Double =
          val vs = values.sorted
          if vs.isEmpty then 0.0
          else if vs.length % 2 == 1 then vs(vs.length / 2)
          else
            val upper = vs.length / 2
            (vs(upper - 1) + vs(upper)) / 2.0

        def medianOf(f: (ScanpathTransition[U], ScanpathTransition[U]) => Double): Double =
          median(pairs.map((i, j) => f(sa(i), sb(j))))

        val shape = 1.0 - medianOf { (x, y) =>
          math.hypot(
            x.displacement.dx - y.displacement.dx,
            x.displacement.dy - y.displacement.dy
          )
        } / (2.0 * diagonal)

        val direction = 1.0 - medianOf { (x, y) =>
          x.direction.separationFrom(y.direction).toRadians
        } / math.Pi

        val length = 1.0 - medianOf { (x, y) =>
          math.abs(x.amplitude - y.amplitude)
        } / diagonal

        val position = 1.0 - medianOf { (x, y) =>
          x.from.distanceTo(y.from)
        } / diagonal

        val duration = 1.0 - median(
          pairs.map { (i, j) =>
            // MultiMatch associates each saccade with the fixation it leaves,
            // so its duration dimension compares those fixation durations.
            val dx = left.fixations(i).duration.toSeconds
            val dy = right.fixations(j).duration.toSeconds
            val mx = math.max(dx, dy)
            if mx <= 0.0 then 0.0 else math.abs(dx - dy) / mx
          }
        )

        MultiMatchScore
          .of(
            clamp(shape),
            clamp(direction),
            clamp(length),
            clamp(position),
            clamp(duration)
          )
          .left
          .map(CompareError.InvalidScore("MultiMatch", _))

      private def clamp(v: Double): Double = math.max(0.0, math.min(1.0, v))

end MultiMatch
