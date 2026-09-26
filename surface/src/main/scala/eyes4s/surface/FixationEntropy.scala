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

package eyes4s.surface

import eyes4s.core.{Scanpath, Weight}
import eyes4s.kernel.*

/** Failures on the way from a scanpath's fixations to an entropy. Every case
  * names the operands that failed: the fixation, the scale, the weight or the
  * frames involved.
  */
enum FixationEntropyError derives CanEqual:

  /** The scanpath is in one frame and the lattice or grid in another. */
  case FrameMismatch(path: FrameId, target: FrameId, underlying: GeometryError)

  /** A lattice needs at least one cell along each axis. */
  case DegenerateLattice(nx: Int, ny: Int)

  /** A padded-range lattice pads by a finite, non-negative fraction of the span. */
  case InvalidPadding(padding: Double)

  /** The lattice bounds are not a rectangle the frame's units can hold. */
  case LatticeBounds(underlying: GeometryError)

  /** Under [[OutsideLattice.Refuse]], the first fixation outside the lattice. */
  case FixationOutsideLattice(fixation: Int, x: Double, y: Double)

  /** No fixation left any mass on the lattice or grid. */
  case NoOccupancy(fixations: Int, outside: Int)

  /** The scanpath's occupancy measure could not be formed. */
  case Occupancy(underlying: SurfaceError)

  /** The data-driven bandwidth could not be chosen. */
  case Bandwidth(underlying: IqrBandwidthError)

  /** Smoothing at the named scale failed. */
  case Estimate(sigma: Double, underlying: EstimateError)

  /** A multiscale entropy needs at least one scale. */
  case NoScales

  /** A scale was requested or supplied twice, or weighted twice. */
  case DuplicateScale(sigma: Double)

  /** A supplied scale's map is on a grid the other scales' maps are not on. */
  case ScaleGrid(sigma: Double, underlying: SurfaceError)

  /** A weighted reduction has no weight for this scale. */
  case MissingScaleWeight(sigma: Double)

  /** A weighted reduction names a scale the result does not have. */
  case UnknownScaleWeight(sigma: Double)

  /** A scale weight must be finite and non-negative. */
  case InvalidScaleWeight(sigma: Double, weight: Double)

  /** Scale weights must have a positive, finite total. */
  case DegenerateScaleWeights(total: Double)

  def message: String = this match
    case FrameMismatch(path, target, e) =>
      s"The scanpath is in frame '$path' and the entropy lattice or grid is over '$target': ${e.message}"
    case DegenerateLattice(nx, ny) =>
      s"An occupancy lattice needs at least one cell per axis; nx=$nx, ny=$ny."
    case InvalidPadding(padding) =>
      s"A padded-range lattice needs a finite, non-negative padding fraction; padding=$padding."
    case LatticeBounds(e) => s"The occupancy lattice bounds are invalid: ${e.message}"
    case FixationOutsideLattice(index, x, y) =>
      s"Fixation $index at ($x, $y) lies outside the occupancy lattice, which refuses it."
    case NoOccupancy(fixations, outside) =>
      s"None of the $fixations fixations left mass to measure; $outside lie outside the lattice or grid."
    case Occupancy(e)          => s"The occupancy measure could not be formed: ${e.message}"
    case Bandwidth(e)          => s"The entropy bandwidth could not be chosen: ${e.message}"
    case Estimate(sigma, e)    => s"Smoothing at sigma=$sigma failed: ${e.message}"
    case NoScales              => "A multiscale entropy needs at least one scale."
    case DuplicateScale(sigma) =>
      s"The scale sigma=$sigma appears twice; each scale is requested, supplied or weighted once."
    case ScaleGrid(sigma, e) =>
      s"The map at sigma=$sigma is not on the grid of the other scales: ${e.message}"
    case MissingScaleWeight(sigma) =>
      s"The weighted reduction has no weight for the scale sigma=$sigma."
    case UnknownScaleWeight(sigma) =>
      s"The weighted reduction names sigma=$sigma, which is not a scale of this result."
    case InvalidScaleWeight(sigma, weight) =>
      s"The weight of the scale sigma=$sigma must be finite and non-negative; it was $weight."
    case DegenerateScaleWeights(total) =>
      s"Scale weights must have a positive, finite total; the total was $total."

end FixationEntropyError

/** What happens to a fixation outside an [[OccupancyLattice]]. There is no
  * default: moving, dropping and refusing a fixation give different entropies.
  */
enum OutsideLattice derives CanEqual:

  /** The first such fixation is a [[FixationEntropyError.FixationOutsideLattice]]. */
  case Refuse

  /** Such fixations contribute nothing and are listed in the result. */
  case Exclude

  /** Such fixations count in the nearest edge cell along each axis, and are
    * listed in the result. This is eyesim's grid method (`findInterval` with
    * `all.inside = TRUE`).
    */
  case ClampToEdgeCell

/** Whether a coordinate on a lattice's upper bound belongs to its last cell.
  *
  * Every other cell is half-open, `[lower, upper)`. [[Closed]] makes the last
  * cell `[lower, upper]`, as eyesim's grid method does
  * (`rightmost.closed = TRUE`); [[Open]] keeps it half-open like a [[Frame]].
  */
enum LatticeUpperEdge derives CanEqual:
  case Closed, Open

/** An equal-width counting lattice over explicit bounds in a scanpath's frame.
  *
  * Unlike a [[Grid]], a lattice may extend past its frame: eyesim's default
  * pads the observed range of the fixations, which can reach off the display.
  * Cell `index = iy * nx + ix`, the [[Grid]] order. Column `ix` covers
  * `[xMin + ix * w, xMin + (ix + 1) * w)` with `w = (xMax - xMin) / nx`, and
  * the last column ends exactly at `xMax`.
  */
final class OccupancyLattice[U <: Unit2D] private (
    val frame: Frame[U],
    val bounds: Bounds[U],
    val nx: Int,
    val ny: Int,
    val upperEdge: LatticeUpperEdge
):

  def size: Int = nx * ny

  /** The cell of an axis coordinate, or `None` when it is outside the lattice
    * along that axis. Breakpoints are `lower + k * ((upper - lower) / n)` with
    * the last one exactly `upper`, which is how R's `seq` builds them.
    */
  private def cellAlong(v: Double, lower: Double, upper: Double, n: Int): Option[Int] =
    if v < lower || v > upper then None
    else if v == upper then if upperEdge == LatticeUpperEdge.Closed then Some(n - 1) else None
    else
      val step = (upper - lower) / n
      var k    = 0
      while k + 1 < n && lower + (k + 1) * step <= v do k += 1
      Some(k)

  private def clampAlong(v: Double, lower: Double, upper: Double, n: Int): Int =
    cellAlong(v, lower, upper, n).getOrElse(if v < lower then 0 else n - 1)

  /** The cell holding a point, or `None` outside the lattice. */
  def cellOf(p: Pt[U]): Option[Int] =
    for
      ix <- cellAlong(p.x, bounds.xMin, bounds.xMax, nx)
      iy <- cellAlong(p.y, bounds.yMin, bounds.yMax, ny)
    yield iy * nx + ix

  /** The nearest edge cell along each axis for a point outside the lattice. */
  def clampedCellOf(p: Pt[U]): Int =
    clampAlong(p.y, bounds.yMin, bounds.yMax, ny) * nx +
      clampAlong(p.x, bounds.xMin, bounds.xMax, nx)

  def render(using u: UnitLabel[U]): String =
    s"lattice ${nx}x$ny over ${bounds.render} in ${frame.id}, upper edge $upperEdge"

end OccupancyLattice

object OccupancyLattice:

  def of[U <: Unit2D](
      frame: Frame[U],
      bounds: Bounds[U],
      nx: Int,
      ny: Int,
      upperEdge: LatticeUpperEdge
  ): Either[FixationEntropyError, OccupancyLattice[U]] =
    if nx <= 0 || ny <= 0 || nx.toLong * ny.toLong > Int.MaxValue then
      Left(FixationEntropyError.DegenerateLattice(nx, ny))
    else Right(new OccupancyLattice(frame, bounds, nx, ny, upperEdge))

  /** A lattice over the whole frame. */
  def overFrame[U <: Unit2D](
      frame: Frame[U],
      nx: Int,
      ny: Int,
      upperEdge: LatticeUpperEdge
  ): Either[FixationEntropyError, OccupancyLattice[U]] =
    of(frame, frame.bounds, nx, ny, upperEdge)

  /** eyesim's default bounds: the observed range of the fixation centres along
    * each axis, widened by `padding` times its span on both sides. A zero span
    * is replaced by one frame unit before padding.
    *
    * The bounds depend on the data, so two scanpaths' entropies on their own
    * padded lattices are measured on different cells. Use [[of]] or
    * [[overFrame]] to compare scanpaths.
    */
  def paddedRange[U <: Unit2D](
      path: Scanpath[U],
      nx: Int,
      ny: Int,
      padding: Double,
      upperEdge: LatticeUpperEdge
  ): Either[FixationEntropyError, OccupancyLattice[U]] =
    if !padding.isFinite || padding < 0.0 then
      Left(FixationEntropyError.InvalidPadding(padding))
    else
      val xs                                               = path.fixations.map(_.centre.x)
      val ys                                               = path.fixations.map(_.centre.y)
      def padded(lo: Double, hi: Double): (Double, Double) =
        val raw  = hi - lo
        val span = if !raw.isFinite || raw <= 2.220446049250313e-16 then 1.0 else raw
        (lo - padding * span, hi + padding * span)
      val (x0, x1) = padded(xs.min, xs.max)
      val (y0, y1) = padded(ys.min, ys.max)
      Bounds
        .of[U](x0, y0, x1, y1)
        .left
        .map(FixationEntropyError.LatticeBounds.apply)
        .flatMap(of(path.frame, _, nx, ny, upperEdge))

end OccupancyLattice

/** Which entropy a multiscale result reports: raw, in the result's base, or
  * relative to the maximum over the grid's cells (eyesim `normalize = TRUE`).
  */
enum EntropyQuantity derives CanEqual:
  case Raw, Relative

/** How the entropies of several scales become one number.
  *
  * There is no unweighted default hidden behind a missing argument: [[Mean]]
  * weighs every scale equally, [[Weighted]] names each scale's weight.
  */
enum ScaleReduction[U <: Unit2D]:
  case Mean()
  case Weighted(weights: Vector[(Sigma[U], Double)])

/** The bandwidth of a density entropy: a stated standard deviation, or
  * eyesim's type-7 IQR rule ([[IqrBandwidth]]) on the scanpath's occupancy.
  */
enum DensityBandwidth[U <: Unit2D]:
  case Fixed(sigma: Sigma[U])
  case IqrSuggested(clamp: IqrBandwidthClamp)

/** The entropy of fixation counts or dwell on an [[OccupancyLattice]].
  *
  * `outside` lists the fixations, by index, that fell outside the lattice:
  * excluded or clamped according to `outsidePolicy`.
  */
final class OccupancyEntropy[U <: Unit2D] private[surface] (
    val lattice: OccupancyLattice[U],
    val weight: Weight,
    val outsidePolicy: OutsideLattice,
    val cells: IArray[Double],
    val entropy: Entropy,
    val relativeEntropy: Double,
    val outside: Vector[Int]
)

/** The entropy of a smoothed fixation map.
  *
  * `outside` lists the fixations, by index, whose centres lie outside the
  * grid's frame; the native smoother bins them nowhere, so they carry no mass.
  */
final class DensityEntropy[U <: Unit2D] private[surface] (
    val mass: Mass[U],
    val sigma: Sigma[U],
    val entropy: Entropy,
    val relativeEntropy: Double,
    val outside: Vector[Int]
)

/** One scale's entropy. */
final case class ScaleEntropy[U <: Unit2D](
    sigma: Sigma[U],
    entropy: Entropy,
    relativeEntropy: Double
) derives CanEqual

/** Entropies of one map at several bandwidths, in ascending order of sigma.
  *
  * Every scale has a value: a scale that cannot be estimated fails the whole
  * construction rather than dropping out of a later mean. [[values]] is
  * eyesim's `aggregate = "none"` and [[reduce]] with [[ScaleReduction.Mean]]
  * its `aggregate = "mean"`.
  */
final class MultiscaleEntropy[U <: Unit2D] private (
    val grid: Grid[U],
    val base: LogBase,
    val levels: Vector[ScaleEntropy[U]],
    val outside: Vector[Int]
):

  def scales: Vector[Sigma[U]] = levels.map(_.sigma)

  private def pick(level: ScaleEntropy[U], quantity: EntropyQuantity): Double =
    quantity match
      case EntropyQuantity.Raw      => level.entropy.value
      case EntropyQuantity.Relative => level.relativeEntropy

  /** Each scale with its entropy, labelled by its sigma. */
  def values(quantity: EntropyQuantity): Vector[(Sigma[U], Double)] =
    levels.map(l => l.sigma -> pick(l, quantity))

  /** One number across scales under an explicit reduction. */
  def reduce(
      quantity: EntropyQuantity,
      reduction: ScaleReduction[U]
  ): Either[FixationEntropyError, Double] =
    reduction match
      case ScaleReduction.Mean() =>
        Right(levels.map(pick(_, quantity)).sum / levels.length)
      case ScaleReduction.Weighted(weights) =>
        val known   = levels.map(_.sigma.value).toSet
        val checked = weights.foldLeft[Either[FixationEntropyError, Map[Double, Double]]](
          Right(Map.empty)
        ) { case (acc, (sigma, w)) =>
          acc.flatMap { seen =>
            val s = sigma.value
            if !known.contains(s) then Left(FixationEntropyError.UnknownScaleWeight(s))
            else if seen.contains(s) then Left(FixationEntropyError.DuplicateScale(s))
            else if !w.isFinite || w < 0.0 then
              Left(FixationEntropyError.InvalidScaleWeight(s, w))
            else Right(seen.updated(s, w))
          }
        }
        for
          byScale <- checked
          _       <- levels
            .find(l => !byScale.contains(l.sigma.value))
            .map(l => FixationEntropyError.MissingScaleWeight(l.sigma.value))
            .toLeft(())
          total = levels.map(l => byScale(l.sigma.value)).sum
          _ <- Either.cond(
            total.isFinite && total > 0.0,
            (),
            FixationEntropyError.DegenerateScaleWeights(total)
          )
        yield levels.map(l => byScale(l.sigma.value) * pick(l, quantity)).sum / total

end MultiscaleEntropy

object MultiscaleEntropy:

  /** Entropies of supplied maps, one per scale, all on one grid. */
  def of[U <: Unit2D](
      levels: Vector[(Sigma[U], Mass[U])],
      base: LogBase
  ): Either[FixationEntropyError, MultiscaleEntropy[U]] =
    build(levels, base, Vector.empty)

  /** Entropies of every level of a pyramid. */
  def ofPyramid[U <: Unit2D](pyramid: Pyramid[U], base: LogBase): MultiscaleEntropy[U] =
    new MultiscaleEntropy(pyramid.grid, base, pyramid.levels.map(level(_, base)), Vector.empty)

  private def level[U <: Unit2D](scale: (Sigma[U], Mass[U]), base: LogBase): ScaleEntropy[U] =
    val (sigma, mass) = scale
    ScaleEntropy(sigma, mass.entropy(base), mass.relativeEntropy(base))

  private[surface] def build[U <: Unit2D](
      levels: Vector[(Sigma[U], Mass[U])],
      base: LogBase,
      outside: Vector[Int]
  ): Either[FixationEntropyError, MultiscaleEntropy[U]] =
    levels.headOption match
      case None             => Left(FixationEntropyError.NoScales)
      case Some((_, first)) =>
        for
          _ <- duplicate(levels.map(_._1))
            .map(FixationEntropyError.DuplicateScale.apply)
            .toLeft(())
          _ <- levels.foldLeft[Either[FixationEntropyError, Unit]](Right(())) {
            case (acc, (sigma, mass)) =>
              acc.flatMap(_ =>
                Agreement
                  .grids(first.grid, mass.grid)
                  .left
                  .map(FixationEntropyError.ScaleGrid(sigma.value, _))
                  .map(_ => ())
              )
          }
        yield new MultiscaleEntropy(
          first.grid,
          base,
          levels.sortBy(_._1.value).map(level(_, base)),
          outside
        )

  private[surface] def duplicate[U <: Unit2D](sigmas: Vector[Sigma[U]]): Option[Double] =
    sigmas.map(_.value).groupBy(identity).collectFirst { case (s, all) if all.length > 1 => s }

end MultiscaleEntropy

/** From a scanpath's fixations to the Shannon entropy of where they fell.
  *
  * eyesim's `fixation_entropy` on a fixation group, with each of its implicit
  * choices made an argument: the occupancy weight, the lattice or grid and
  * what happens to a fixation outside it, the bandwidth, the edge policy of
  * the smoother, the log base and, across scales, the reduction.
  *
  *   - [[occupancy]] counts (or accumulates dwell) in the cells of an
  *     [[OccupancyLattice]]; with [[Weight.Uniform]], a lattice from
  *     [[OccupancyLattice.paddedRange]] with padding `0.05`,
  *     [[LatticeUpperEdge.Closed]] and [[OutsideLattice.ClampToEdgeCell]], it
  *     is eyesim's `method = "grid"`.
  *   - [[density]] smooths the occupancy with the native [[Smoother]] on a
  *     [[Grid]] over the scanpath's frame and measures the resulting [[Mass]].
  *     It is not eyesim's `method = "density"`, which evaluates a continuous
  *     `ks` estimate at endpoint-inclusive lattice points; the entropy step
  *     is the same.
  *   - [[multiscale]] does the same at several bandwidths.
  */
object FixationEntropy:

  def occupancy[U <: Unit2D](
      path: Scanpath[U],
      lattice: OccupancyLattice[U],
      weight: Weight,
      outsidePolicy: OutsideLattice,
      base: LogBase
  ): Either[FixationEntropyError, OccupancyEntropy[U]] =
    for
      _        <- sameFrame(path, lattice.frame)
      assigned <- path.fixations.indices.foldLeft[Either[
        FixationEntropyError,
        Vector[(Int, Option[Int])]
      ]](Right(Vector.empty)) { (acc, i) =>
        acc.flatMap { done =>
          val c = path.fixations(i).centre
          lattice.cellOf(c) match
            case some @ Some(_) => Right(done :+ (i -> some))
            case None           =>
              outsidePolicy match
                case OutsideLattice.Refuse =>
                  Left(FixationEntropyError.FixationOutsideLattice(i, c.x, c.y))
                case OutsideLattice.Exclude         => Right(done :+ (i -> None))
                case OutsideLattice.ClampToEdgeCell =>
                  Right(done :+ (i -> Some(lattice.clampedCellOf(c))))
        }
      }
      measure <- path.occupancy(weight).left.map(FixationEntropyError.Occupancy.apply)
      outside = assigned.collect {
        case (i, _) if lattice.cellOf(path.fixations(i).centre).isEmpty => i
      }
      cells = {
        val acc = Array.fill(lattice.size)(0.0)
        assigned.foreach { case (i, cell) => cell.foreach(k => acc(k) += measure.weights(i)) }
        IArray.unsafeFromArray(acc)
      }
      entropy <- Entropy
        .ofWeights(cells, base)
        .left
        .map(_ => FixationEntropyError.NoOccupancy(path.n, outside.length))
    yield new OccupancyEntropy(
      lattice,
      weight,
      outsidePolicy,
      cells,
      entropy,
      Entropy.relative(entropy, lattice.size),
      outside
    )

  def density[U <: Unit2D](
      path: Scanpath[U],
      grid: Grid[U],
      bandwidth: DensityBandwidth[U],
      edges: EdgePolicy,
      weight: Weight,
      base: LogBase
  ): Either[FixationEntropyError, DensityEntropy[U]] =
    for
      prepared <- prepare(path, grid, weight)
      (measure, outside) = prepared
      sigma <- bandwidth match
        case DensityBandwidth.Fixed(s)            => Right(s)
        case DensityBandwidth.IqrSuggested(clamp) =>
          IqrBandwidth.suggest(measure, clamp).left.map(FixationEntropyError.Bandwidth.apply)
      mass <- smooth(measure, grid, sigma, edges)
      entropy = mass.entropy(base)
    yield new DensityEntropy(mass, sigma, entropy, mass.relativeEntropy(base), outside)

  def multiscale[U <: Unit2D](
      path: Scanpath[U],
      grid: Grid[U],
      sigmas: Vector[Sigma[U]],
      edges: EdgePolicy,
      weight: Weight,
      base: LogBase
  ): Either[FixationEntropyError, MultiscaleEntropy[U]] =
    for
      _ <- Either.cond(sigmas.nonEmpty, (), FixationEntropyError.NoScales)
      _ <- MultiscaleEntropy
        .duplicate(sigmas)
        .map(FixationEntropyError.DuplicateScale.apply)
        .toLeft(())
      prepared <- prepare(path, grid, weight)
      (measure, outside) = prepared
      levels <- sigmas
        .sortBy(_.value)
        .foldLeft[Either[
          FixationEntropyError,
          Vector[(Sigma[U], Mass[U])]
        ]](Right(Vector.empty)) { (acc, s) =>
          acc.flatMap(done => smooth(measure, grid, s, edges).map(m => done :+ (s -> m)))
        }
      result <- MultiscaleEntropy.build(levels, base, outside)
    yield result

  private def sameFrame[U <: Unit2D](
      path: Scanpath[U],
      target: Frame[U]
  ): Either[FixationEntropyError, Unit] =
    Agreement
      .frames(path.frame, target)
      .left
      .map(FixationEntropyError.FrameMismatch(path.frame.id, target.id, _))
      .map(_ => ())

  /** The occupancy measure and the fixations outside the grid's frame, which
    * the smoother bins nowhere. A scanpath with no fixation inside is refused
    * here, naming how many fell outside, rather than as a smoother's bare
    * `NoMass`.
    */
  private def prepare[U <: Unit2D](
      path: Scanpath[U],
      grid: Grid[U],
      weight: Weight
  ): Either[FixationEntropyError, (PointMeasure[U], Vector[Int])] =
    for
      _       <- sameFrame(path, grid.frame)
      measure <- path.occupancy(weight).left.map(FixationEntropyError.Occupancy.apply)
      outside = path.fixations.indices.filterNot(i =>
        grid.frame.contains(path.fixations(i).centre)
      )
      inside = path.fixations.indices.filterNot(outside.contains).map(measure.weights(_)).sum
      _ <- Either.cond(
        inside > 0.0,
        (),
        FixationEntropyError.NoOccupancy(path.n, outside.length)
      )
    yield (measure, outside.toVector)

  private def smooth[U <: Unit2D](
      measure: PointMeasure[U],
      grid: Grid[U],
      sigma: Sigma[U],
      edges: EdgePolicy
  ): Either[FixationEntropyError, Mass[U]] =
    Smoother
      .gaussian(sigma, edges)
      .density(measure, grid)
      .left
      .map(FixationEntropyError.Estimate(sigma.value, _))

end FixationEntropy
