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

import eyes4s.studio.app.maps.{
  ColourLimits,
  LimitsScope,
  MapGrid,
  MapId,
  MapOpacity,
  MapPalette,
  MapStyle
}
import eyes4s.studio.app.text.{PanelText, PanelTextId}
import eyes4s.studio.core.backend.{DensityGrid, ScreenRegion}

/** A panel's stored result map, or its explicit read state. */
enum CompareMapPanel derives CanEqual:
  case Reading(id: MapId.Result)
  case Unavailable(id: MapId.Result, reason: String)
  case Drawn(
      id: MapId.Result,
      grid: MapGrid,
      region: ScreenRegion,
      style: MapStyle,
      opacity: MapOpacity,
      scope: LimitsScope
  )

/** Portable state of the query/reference map reads. Only wanted identities
  * are retained; an answer for another run, trial or scale cannot stand in
  * for one. Styles never alter the stored cells or contour thresholds.
  */
final case class CompareMaps private (
    wanted: Vector[MapId.Result],
    answers: Map[MapId.Result, Option[Either[String, (MapGrid, ScreenRegion)]]],
    opacity: MapOpacity,
    scope: LimitsScope
) derives CanEqual:
  def failed: Boolean = answers.values.exists(_.exists(_.isLeft))

  def panels: Vector[CompareMapPanel] =
    val grids = wanted.flatMap(id => answers.get(id).flatten.flatMap(_.toOption).map(_._1))
    wanted.map { id =>
      answers.get(id).flatten match
        case None                        => CompareMapPanel.Reading(id)
        case Some(Left(reason))          => CompareMapPanel.Unavailable(id, reason)
        case Some(Right((grid, region))) =>
          val together = if scope == LimitsScope.Shared then grids else Vector(grid)
          CompareMapPanel.Drawn(
            id,
            grid,
            region,
            ColourLimits.spanning(MapPalette.Mass, together),
            opacity,
            scope
          )
    }

object CompareMaps:
  val empty: CompareMaps =
    CompareMaps(Vector.empty, Map.empty, MapOpacity.Default, LimitsScope.Shared)

  /** Returns the next state and exact new reads. The same identity in both
    * panels needs one read, without losing a displayed panel.
    */
  def follow(
      state: CompareMaps,
      wanted: Vector[MapId.Result],
      opacity: MapOpacity
  ): (CompareMaps, Vector[MapId.Result]) =
    val reads    = wanted.distinct.filterNot(state.answers.contains)
    val retained = state.answers.filter((id, _) => wanted.contains(id)) ++ reads.map(_ -> None)
    (state.copy(wanted = wanted, answers = retained, opacity = opacity), reads)

  /** Late replies change nothing. A mismatched identity or failed read
    * remains visible and retryable.
    */
  def read(
      state: CompareMaps,
      id: MapId.Result,
      answer: Either[String, DensityGrid]
  ): CompareMaps =
    if !state.answers.contains(id) then state
    else state.copy(answers = state.answers.updated(id, Some(answer.flatMap(decode(id, _)))))

  def chooseLimits(state: CompareMaps, scope: LimitsScope): CompareMaps =
    state.copy(scope = scope)

  def retry(state: CompareMaps): (CompareMaps, Vector[MapId.Result]) =
    val reads =
      state.wanted.distinct.filter(id => state.answers.get(id).flatten.exists(_.isLeft))
    (state.copy(answers = state.answers ++ reads.map(_ -> None)), reads)

  private def decode(
      id: MapId.Result,
      grid: DensityGrid
  ): Either[String, (MapGrid, ScreenRegion)] =
    if grid.run != id.run || grid.scale != id.scale.value || grid.trial != id.trial then
      Left(
        PanelText(
          PanelTextId.MapIdentity,
          id.label,
          s"${grid.run.label} · ${grid.address.render}"
        )
      )
    else
      MapGrid
        .of(
          id,
          grid.columns,
          grid.rows,
          grid.order,
          grid.cells.map(Some(_)),
          grid.levels.map(_.threshold)
        )
        .left
        .map(_.message)
        .map(_ -> grid.region)
