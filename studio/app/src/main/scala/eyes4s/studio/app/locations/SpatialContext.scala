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

package eyes4s.studio.app.locations

import eyes4s.studio.app.AppModel
import eyes4s.studio.app.nav.Place
import eyes4s.studio.core.document.*

/** Select scientific kind from the actual navigation and shown run, never from a timed projection. */
object SpatialContext:
  def data(model: AppModel): Option[LocationDatasetRevision] =
    val selected = model.navigation.trail(Perspective.Data).reverseIterator.collectFirst {
      case Place.Dataset(id) => id
    }
    selected
      .fold(model.document.datasets.lastOption)(model.document.datasetDefinition)
      .collect { case value: LocationDatasetRevision =>
        value
      }

  def analysis(model: AppModel): Option[LocationAnalysisRevision] =
    val trail    = model.navigation.trail(Perspective.Analysis)
    val revision = trail.reverseIterator.collectFirst { case Place.Revision(id) => id }
    val family   = trail.reverseIterator.collectFirst { case Place.Family(id) => id }
    val chosen   = revision.orElse(
      family.flatMap(id =>
        model.document.analyses
          .findLast(a => model.document.familyOf(a.id).contains(id))
          .map(_.id)
      )
    )
    (if revision.nonEmpty || family.nonEmpty then
       chosen.flatMap(model.document.analysisDefinition)
     else model.document.analyses.lastOption)
      .collect { case value: LocationAnalysisRevision =>
        value
      }

  def analysisData(model: AppModel): Option[LocationDatasetRevision] =
    analysis(model)
      .flatMap(a => model.document.datasetDefinition(a.dataset))
      .collect { case value: LocationDatasetRevision =>
        value
      }
      .orElse(Option.when(model.document.analyses.isEmpty)(data(model)).flatten)

  def shown(model: AppModel): Option[RunRef] =
    model.document.presentation.shownRun
      .flatMap(model.document.run)
      .filter(run =>
        model.document.analysisDefinition(run.analysis).exists(_.kind == StudyKind.Locations)
      )

  def active(model: AppModel): Boolean = at(model, model.perspective)

  def at(model: AppModel, perspective: Perspective): Boolean = perspective match
    case Perspective.Data                         => data(model).nonEmpty
    case Perspective.Analysis                     => analysisData(model).nonEmpty
    case Perspective.Results | Perspective.Report =>
      shown(model).nonEmpty || (model.document.presentation.shownRun.isEmpty && analysisData(
        model
      ).nonEmpty)
