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

package eyes4s.kernel

/** A finite measure on the plane: weighted positions in a frame.
  *
  * ==The occupancy half of the duality==
  *
  * A path through the plane has a shadow -- the measure it induces on that
  * plane, once you stop caring about the order in which it was traced. This is
  * that shadow. The map producing it is explicitly lossy, and lives in
  * `eyes4s-core` where paths do.
  *
  * ==One operation generates the layer==
  *
  * [[integrate]] is the primitive. Everything order-free is an integral against
  * a different integrand:
  *
  *   - the mass in a region -- dwell time -- is the integral of its indicator;
  *   - a smoothed map is the integral of a kernel centred at each cell;
  *   - the classical saliency scores are integrals of a saliency map;
  *   - the count of positions is the integral of the constant one, under
  *     uniform weights.
  *
  * `eyesim` implements the first as `fixation_overlap`, the third as
  * `sample_density` with a hand-rolled nearest-cell lookup, and has no way at
  * all to express the saliency family. They are the same operation.
  *
  * ==What this cannot do==
  *
  * Anything order-dependent. Scanpath length, transition matrices, alignment,
  * recurrence, first-entry times: the ordering is gone, and no integrand
  * recovers it. Those live on the path, not here.
  */
final class PointMeasure[U <: Unit2D] private (
    val frame: Frame[U],
    val positions: IArray[Pt[U]],
    val weights: IArray[Double],
    val provenance: Provenance
):

  def size: Int = positions.length

  def isEmpty: Boolean = positions.isEmpty

  /** Total mass. Not necessarily one -- see [[normalised]]. */
  def total: Double =
    var s = 0.0
    var i = 0
    while i < weights.length do
      s += weights(i)
      i += 1
    s

  /** `∫ f dμ`, the whole point of the type. */
  def integrate(f: Pt[U] => Double): Double =
    var s = 0.0
    var i = 0
    while i < positions.length do
      s += weights(i) * f(positions(i))
      i += 1
    s

  /** Mass falling inside a region. Dwell time, when the weights are durations. */
  def massIn(r: Region[U]): Double =
    integrate(p => if r.contains(p) then 1.0 else 0.0)

  /** Mass falling in each cell of a grid, in the grid's index order.
    *
    * Positions outside the grid's frame contribute nowhere rather than being
    * clamped to an edge cell, which would pile off-screen data onto the border.
    */
  def binned(g: Grid[U]): Either[GeometryError, IArray[Double]] =
    Agreement.frames(frame, g.frame).map { _ =>
      val acc = Array.fill(g.size)(0.0)
      var i   = 0
      while i < positions.length do
        g.indexOf(positions(i)).foreach(ix => acc(ix) += weights(i))
        i += 1
      IArray.unsafeFromArray(acc)
    }

  /** The same positions, rescaled to unit total mass. */
  def normalised: Either[SurfaceError, PointMeasure[U]] =
    val t = total
    if !t.isFinite || t <= 0.0 then Left(SurfaceError.DegenerateTotal(t))
    else
      Right(
        new PointMeasure(
          frame,
          positions,
          IArray.tabulate(size)(i => weights(i) / t),
          provenance.andThen(Provenance.Step.text("normalise", "of", "measure"))
        )
      )

  /** Keep only positions satisfying a predicate, with their weights. */
  def filter(p: Pt[U] => Boolean): PointMeasure[U] =
    val keep = (0 until size).filter(i => p(positions(i)))
    new PointMeasure(
      frame,
      IArray.from(keep.map(positions.apply)),
      IArray.from(keep.map(weights.apply)),
      provenance.andThen(Provenance.Step("filter"))
    )

  /** Drop positions falling outside the frame. */
  def withinFrame: PointMeasure[U] = filter(frame.contains)

  def render(using u: UnitLabel[U]): String =
    f"measure($size positions, total=$total%.4g, ${frame.id}/${u.symbol})"

end PointMeasure

object PointMeasure:

  def of[U <: Unit2D](
      frame: Frame[U],
      positions: IArray[Pt[U]],
      weights: IArray[Double]
  ): Either[SurfaceError, PointMeasure[U]] =
    if positions.length != weights.length then
      Left(SurfaceError.LengthMismatch(positions.length, weights.length))
    else
      var bad = -1
      var i   = 0
      while i < weights.length && bad < 0 do
        if !weights(i).isFinite || weights(i) < 0.0 then bad = i
        i += 1
      if bad >= 0 then Left(SurfaceError.NegativeWeight(bad, weights(bad)))
      else
        Right(
          new PointMeasure(
            frame,
            positions,
            weights,
            Provenance.raw(
              ContentHash.combine(
                ContentHash.of(IArray.from(positions.toSeq.flatMap(p => Seq(p.x, p.y)))),
                ContentHash.of(weights)
              )
            )
          )
        )

  /** Every position counting equally. */
  def uniform[U <: Unit2D](
      frame: Frame[U],
      positions: IArray[Pt[U]]
  ): Either[SurfaceError, PointMeasure[U]] =
    of(frame, positions, IArray.fill(positions.length)(1.0))

  def empty[U <: Unit2D](frame: Frame[U]): PointMeasure[U] =
    new PointMeasure(frame, IArray.empty, IArray.empty, Provenance.raw(ContentHash.empty))

end PointMeasure
