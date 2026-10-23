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

package eyes4s.studio.desktop.compare

import eyes4s.studio.app.compare.{CompareMapPanel, CompareMaps, PanelFocus, TrialPanelsVM}
import eyes4s.studio.app.maps.{LimitsScope, MapId, MapOpacity}
import eyes4s.studio.app.text.{PanelText, PanelTextId}
import eyes4s.studio.desktop.trial.MapRequest
import eyes4s.studio.viz.trial.{MapCoverage, ScreenRect}
import javafx.application.Platform

/** Performs portable map-read effects and binds their display models. */
private[compare] final class CompareMapHost(inputs: SummaryInputs, panels: TrialPanelsView):
  private var state    = CompareMaps.empty
  private var disposed = false

  def chooseLimits(next: LimitsScope): Unit = if !disposed then
    state = CompareMaps.chooseLimits(state, next)
    render()

  def retry(): Unit = if !disposed then changed(CompareMaps.retry(state))

  def sync(focus: Option[PanelFocus], vm: TrialPanelsVM, alpha: Double): Unit = if !disposed
  then
    val wanted: Vector[MapId.Result] = focus.toVector.flatMap(f =>
      (vm.query.toVector ++ vm.reference.toVector).map(p =>
        MapId.Result(f.run, p.trial, f.scale)
      )
    )
    MapOpacity.of(alpha).foreach(opacity => changed(CompareMaps.follow(state, wanted, opacity)))

  private def changed(step: (CompareMaps, Vector[MapId.Result])): Unit =
    state = step._1
    step._2.foreach(id =>
      inputs.mapGrid(
        id,
        answer =>
          Platform.runLater(() =>
            if !disposed then
              state = CompareMaps.read(state, id, answer)
              render()
          )
      )
    )
    render()

  private def render(): Unit =
    val shown = state.panels.map {
      case CompareMapPanel.Reading(_) => (None, Some(PanelText(PanelTextId.MapReading)))
      case CompareMapPanel.Unavailable(_, why) =>
        (None, Some(PanelText(PanelTextId.MapUnavailable, why)))
      case CompareMapPanel.Drawn(_, grid, region, style, opacity, scope) =>
        ScreenRect.of(region.left, region.top, region.right, region.bottom) match
          case Right(rect) =>
            (Some(MapRequest(grid, style, opacity, MapCoverage.Region(rect), scope)), None)
          case Left(error) => (None, Some(PanelText(PanelTextId.MapUnavailable, error.message)))
    }
    panels.showMaps(
      shown.lift(0).getOrElse((None, None)),
      shown.lift(1).getOrElse((None, None))
    )
    panels.mapFailed(state.failed)

  def dispose(): Unit =
    disposed = true
    state = CompareMaps.empty
