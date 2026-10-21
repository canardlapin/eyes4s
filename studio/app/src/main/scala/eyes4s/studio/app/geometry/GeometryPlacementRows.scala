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

package eyes4s.studio.app.geometry

import cats.syntax.all.*
import eyes4s.studio.app.AppModel
import eyes4s.studio.app.nav.Place
import eyes4s.studio.app.plot.*
import eyes4s.studio.app.text.{Format, GeometryText, GeometryTextId}
import eyes4s.studio.core.backend.PlacementPreview
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.core.selection.{PlacementCount, StudioRef}

/** The served trial tallies a placement count opens. No admission statuses,
  * coordinates or counts are inferred here; these are the backend's rows.
  */
object GeometryPlacementRows:

  /** A response belongs to the exact currently displayed source/geometry,
    * never to an earlier draft or an earlier geometry under the same id.
    */
  def current(panel: GeometryPanel): Option[PlacementPreview] =
    panel.placement.toOption.filter(p =>
      panel.shown.exists(s => p.dataset == s.id && panel.placementKey == PlacementKey.of(s))
    )

  def count(panel: GeometryPanel, model: AppModel): PlacementCount =
    model.navigation
      .trail(Perspective.Data)
      .reverseIterator
      .collectFirst {
        case Place.At(StudioRef.PlacementTally(dataset, region))
            if panel.shown.exists(_.id == dataset) =>
          region
      }
      .getOrElse(PlacementCount.PlacedRecords)

  def caption(panel: GeometryPanel, model: AppModel): String =
    import GeometryTextId.*
    current(panel) match
      case Some(p) =>
        GeometryText(
          PlacementRecordsCaption,
          p.dataset.label,
          GeometryText.placementCount(count(panel, model)),
          Format.count(p.unplaced.size.toLong)
        )
      case None =>
        panel.placement match
          case Loading.Failed(why) => GeometryText(PlacementRecordsFailed, why)
          case _                   => GeometryText(PlacementRecordsWaiting)

  def source(panel: GeometryPanel, model: AppModel): Either[PlotSourceError, PlotSource] =
    import GeometryTextId.*
    val fields = Vector(
      ("participant", RecordsParticipant, ColumnFormat.Label),
      ("phase", RecordsPhase, ColumnFormat.Label),
      ("trial", RecordsTrial, ColumnFormat.Label),
      ("occurrence", RecordsOccurrence, ColumnFormat.Count),
      ("records", RecordsTotal, ColumnFormat.Count),
      ("outside-window", RecordsWindow, ColumnFormat.Count),
      ("outside-screen", RecordsScreen, ColumnFormat.Count)
    )
    val selectedCount = count(panel, model)
    val rows          = current(panel).toVector.flatMap { p =>
      p.trials
        .filter(t =>
          selectedCount match
            case PlacementCount.PlacedRecords => t.records > 0
            case PlacementCount.OutsideWindow => t.outsideWindow > 0
            case PlacementCount.OutsideScreen => t.outsideScreen > 0
        )
        .map(t =>
          PlotRow(
            StudioRef.TrialPlacementTally(p.dataset, t.trial),
            Vector(
              PlotValue.Text(t.trial.participant),
              PlotValue.Text(t.trial.phase.label),
              PlotValue.Text(t.trial.trial),
              PlotValue.Number(t.trial.occurrence.toDouble),
              PlotValue.Number(t.records.toDouble),
              PlotValue.Number(t.outsideWindow.toDouble),
              PlotValue.Number(t.outsideScreen.toDouble)
            )
          )
        )
    }
    for
      columns <- fields.traverse { (id, text, format) =>
        ColumnId.of(id).map(c => PlotColumn(c, GeometryText(text), format))
      }
      result <- PlotSource(caption(panel, model), columns, rows)
    yield result
