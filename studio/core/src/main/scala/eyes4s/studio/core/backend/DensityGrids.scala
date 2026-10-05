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

package eyes4s.studio.core.backend

import cats.syntax.all.*
import io.circe.{Codec, Decoder, DecodingFailure, Encoder}

/** Why a served density grid (protocol `MapGridOf`) is refused. Every case
  * names the run and the estimation it is of.
  */
enum DensityGridError derives CanEqual:
  case SigmaNotPositive(run: RunId, address: ResultAddress, degrees: Double)
  case EmptyGrid(run: RunId, address: ResultAddress, columns: Int, rows: Int)
  case CellCount(run: RunId, address: ResultAddress, columns: Int, rows: Int, cells: Int)

  /** Cell `index` holds `value`, which is not a finite, non-negative density. */
  case CellNotDensity(run: RunId, address: ResultAddress, index: Int, value: Double)

  /** A cell's angular size is not a finite number of degrees above zero. */
  case CellDegreesNotPositive(run: RunId, address: ResultAddress, width: Double, height: Double)

  /** Level `index` encloses `coverage` of the mass, outside `(0, 1]`, or its
    * `threshold` is not a finite, non-negative density.
    */
  case LevelOutOfRange(
      run: RunId,
      address: ResultAddress,
      index: Int,
      coverage: Double,
      threshold: Double
  )

  /** The region the grid covers has no area or is not finite. */
  case RegionEmpty(
      run: RunId,
      address: ResultAddress,
      left: Double,
      top: Double,
      right: Double,
      bottom: Double
  )

  def message: String = this match
    case SigmaNotPositive(r, a, d) =>
      s"The density grid of ${a.render} in ${r.label} has σ $d°, which is not above zero."
    case EmptyGrid(r, a, c, w) =>
      s"The density grid of ${a.render} in ${r.label} has $c × $w cells."
    case CellCount(r, a, c, w, n) =>
      s"The density grid of ${a.render} in ${r.label} lists $n values for $c × $w cells."
    case CellNotDensity(r, a, i, v) =>
      s"Cell $i of the density grid of ${a.render} in ${r.label} holds $v, which is not a density."
    case CellDegreesNotPositive(r, a, w, h) =>
      s"The cells of the density grid of ${a.render} in ${r.label} are $w° × $h°."
    case LevelOutOfRange(r, a, i, c, t) =>
      s"Level $i of the density grid of ${a.render} in ${r.label} encloses $c of the mass " +
        s"at threshold $t."
    case RegionEmpty(r, a, l, t, rt, b) =>
      s"The density grid of ${a.render} in ${r.label} covers ($l, $t) to ($rt, $b), " +
        "which has no area."

/** A cell's angular size: its width and height in degrees of visual angle,
  * which the backend states only when the study declares an angular scale.
  */
final case class CellDegrees(width: Double, height: Double) derives CanEqual, Codec.AsObject

/** One highest-density region of a map (eyes4s `MassLevel`): the smallest
  * set of the fullest cells that holds `coverage` of the mass is the set of
  * cells at or above `threshold`. Its isoline is drawn at `threshold`.
  */
final case class DensityLevel(coverage: Double, threshold: Double)
    derives CanEqual,
      Codec.AsObject

/** One trial's density estimate at one scale of a run (protocol `MapGridOf`;
  * UI-E): eyes4s's map of the run's result at `ResultAddress.Estimation(scale,
  * trial)`, never a preview or a studio computation. The grid covers `region`
  * of the screen (the analysis window, or the screen when the study has
  * none) in `columns × rows` cells; `cells` are in row-major order, x
  * fastest, with `order` stating which row is the top, and every cell has a
  * value. `levels` are the backend's highest-density regions, computed on
  * read, in the order it states them.
  */
final case class DensityGrid private (
    run: RunId,
    scale: Int,
    trial: TrialKey,
    sigmaDegrees: Double,
    region: ScreenRegion,
    columns: Int,
    rows: Int,
    order: RowOrder,
    cellDegrees: Option[CellDegrees],
    cells: Vector[Double],
    levels: Vector[DensityLevel]
) derives CanEqual:

  /** The result this grid is: the trial's estimation at the scale. */
  def address: ResultAddress = ResultAddress.Estimation(scale, trial)

  /** A cell's width and height in screen pixels. */
  def cellWidthPx: Double  = region.width / columns
  def cellHeightPx: Double = region.height / rows

object DensityGrid:
  def of(
      run: RunId,
      scale: Int,
      trial: TrialKey,
      sigmaDegrees: Double,
      region: ScreenRegion,
      columns: Int,
      rows: Int,
      order: RowOrder,
      cellDegrees: Option[CellDegrees],
      cells: Vector[Double],
      levels: Vector[DensityLevel]
  ): Either[DensityGridError, DensityGrid] =
    val address             = ResultAddress.Estimation(scale, trial)
    def positive(v: Double) = v.isFinite && v > 0.0
    def density(v: Double)  = v.isFinite && v >= 0.0
    if !positive(sigmaDegrees) then
      Left(DensityGridError.SigmaNotPositive(run, address, sigmaDegrees))
    else if columns <= 0 || rows <= 0 then
      Left(DensityGridError.EmptyGrid(run, address, columns, rows))
    else if cells.size.toLong != columns.toLong * rows then
      Left(DensityGridError.CellCount(run, address, columns, rows, cells.size))
    else
      cellDegrees
        .collect {
          case d if !positive(d.width) || !positive(d.height) =>
            DensityGridError.CellDegreesNotPositive(run, address, d.width, d.height)
        }
        .orElse(cells.iterator.zipWithIndex.collectFirst {
          case (v, i) if !density(v) => DensityGridError.CellNotDensity(run, address, i, v)
        })
        .orElse(levels.iterator.zipWithIndex.collectFirst {
          case (l, i)
              if !l.coverage.isFinite || l.coverage <= 0.0 || l.coverage > 1.0 ||
                !density(l.threshold) =>
            DensityGridError.LevelOutOfRange(run, address, i, l.coverage, l.threshold)
        })
        .toLeft(
          DensityGrid(
            run,
            scale,
            trial,
            sigmaDegrees,
            region,
            columns,
            rows,
            order,
            cellDegrees,
            cells,
            levels
          )
        )

  /** The region a grid covers, refused as the grid's when it has no area. */
  def region(
      run: RunId,
      scale: Int,
      trial: TrialKey,
      left: Double,
      top: Double,
      right: Double,
      bottom: Double
  ): Either[DensityGridError, ScreenRegion] =
    ScreenRegion
      .of(trial, left, top, right, bottom)
      .leftMap(_ =>
        DensityGridError.RegionEmpty(
          run,
          ResultAddress.Estimation(scale, trial),
          left,
          top,
          right,
          bottom
        )
      )

  given Encoder.AsObject[DensityGrid] = Encoder.forProduct11(
    "run",
    "scale",
    "trial",
    "sigmaDegrees",
    "region",
    "columns",
    "rows",
    "order",
    "cellDegrees",
    "cells",
    "levels"
  )(g =>
    (
      g.run,
      g.scale,
      g.trial,
      g.sigmaDegrees,
      g.region,
      g.columns,
      g.rows,
      g.order,
      g.cellDegrees,
      g.cells,
      g.levels
    )
  )

  given Decoder[DensityGrid] = Decoder.instance { c =>
    (
      c.get[RunId]("run"),
      c.get[Int]("scale"),
      c.get[TrialKey]("trial"),
      c.get[Double]("sigmaDegrees"),
      c.downField("region")
        .as[(Double, Double, Double, Double)](using
          Decoder.forProduct4("left", "top", "right", "bottom")(
            (l: Double, t: Double, r: Double, b: Double) => (l, t, r, b)
          )
        ),
      c.get[Int]("columns"),
      c.get[Int]("rows"),
      c.get[RowOrder]("order"),
      c.get[Option[CellDegrees]]("cellDegrees"),
      c.get[Vector[Double]]("cells"),
      c.get[Vector[DensityLevel]]("levels")
    ).flatMapN { case (r, s, t, sigma, (l, tp, rt, b), w, h, o, deg, cs, ls) =>
      region(r, s, t, l, tp, rt, b)
        .flatMap(of(r, s, t, sigma, _, w, h, o, deg, cs, ls))
        .leftMap(e => DecodingFailure(e.message, c.history))
    }
  }
