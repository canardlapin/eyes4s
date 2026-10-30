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

package eyes4s.studio.app.compare

import eyes4s.studio.app.Intent
import eyes4s.studio.app.figures.{FigurePanels, TileMap}
import eyes4s.studio.app.maps.{ColourLimits, LimitsScope, MapPalette}
import eyes4s.studio.app.nav.Place
import eyes4s.studio.app.plot.LadderScale
import eyes4s.studio.app.text.Format
import eyes4s.studio.core.backend.{DensityGrid, RunId, TrialKey}
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}

/** One tile of the pair's control strip: a reference's density map at the
  * focused scale and its cosine to the query. Enter or a click opens its pair.
  */
final case class StripTileVM(
    ref: StudioRef,
    trial: TrialKey,
    score: String,
    matched: Boolean,
    current: Boolean,
    map: TileMap,
    open: Intent,
    name: String
) derives CanEqual

/** The pair's control strip (overhaul decision 7; the golden's "Matched and
  * 19 controls"): the matched reference, then every control by descending
  * cosine, each its density map on one colour limit, and B, the controls'
  * mean, between the tiles where it falls. Two balanced rows: the first
  * holds [[firstRow]] cells, B's marker among them.
  */
final case class PairStripVM(
    title: String,
    note: String,
    tiles: Vector[StripTileVM],
    b: Option[(Int, String)],
    firstRow: Int
) derives CanEqual:

  /** The strip as one line for assistive technology and tests. */
  def text: String = (tiles.map(t => s"${t.trial.trial} ${t.score}") ++
    b.map((i, v) => s"B $v at $i")).mkString(", ")

object PairStrip:

  /** The strip of `query` at `at` in `run`; `maps` serves each reference's
    * density grid at that scale, or why not, or nothing yet.
    */
  def vm(
      run: RunId,
      scale: ScaleIndex,
      at: LadderScale,
      shown: Option[StudioRef],
      maps: TrialKey => Option[Either[String, DensityGrid]]
  ): PairStripVM =
    val scored = at.controls
      .sortBy(c => (c.cosine.fold(1)(_ => 0), c.cosine.fold(0.0)(-_), c.reference.trial))
    val heads = (at.matched, at.matchedTrial, Format.decimal(at.m, 2), true) +:
      scored.map(c => (c.ref, c.reference, c.cosine.fold("—")(Format.decimal(_, 2)), false))
    val read =
      heads.map((_, trial, _, _) => FigurePanels.tileMap(run, scale, trial, maps(trial)))
    val grids      = read.collect { case Right((g, _)) => g }
    val styles     = ColourLimits.forPanels(MapPalette.Mass, LimitsScope.Shared, grids)
    val (tiles, _) = heads.zip(read).foldLeft((Vector.empty[StripTileVM], styles)) {
      case ((done, next), ((ref, trial, score, matched), answer)) =>
        val (map, rest) = answer match
          case Right((g, grid)) => (TileMap.Drawn(g, next.head, grid.region), next.tail)
          case Left(state)      => (state, next)
        val role = if matched then "matched" else "control"
        (
          done :+ StripTileVM(
            ref,
            trial,
            score,
            matched,
            shown.contains(ref),
            map,
            Intent.Explain(Place.At(ref)),
            s"${trial.trial}, $role, cosine $score"
          ),
          rest
        )
    }
    // B falls after the controls at or above it.
    val bAt   = 1 + scored.count(_.cosine.exists(_ >= at.b))
    val cells = tiles.size + 1
    PairStripVM(
      s"Matched and ${at.controls.size} controls",
      s"density at ${at.label} on one colour limit, sorted by cosine to the query; " +
        "B is their mean; Enter opens a pair",
      tiles,
      Some((bAt, Format.decimal(at.b, 2))),
      (cells + 1) / 2
    )
