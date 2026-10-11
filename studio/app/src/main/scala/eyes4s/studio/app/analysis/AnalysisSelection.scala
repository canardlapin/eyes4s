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

package eyes4s.studio.app.analysis

import eyes4s.studio.app.AppModel
import eyes4s.studio.app.nav.Place
import eyes4s.studio.core.backend.AnalysisRevision
import eyes4s.studio.core.document.*

/** Analysis navigation names identities, independently of mutable names and presets
  * (bead q-analysis-family-identity). Selecting history never selects a shown result.
  */
object AnalysisSelection:
  def context(
      document: StudioDocument,
      revision: Option[AnalysisRevision]
  ): Option[DraftContext] =
    revision match
      case Some(id) =>
        document.draftContext
          .filter(_.id == id)
          .orElse(document.analysis(id).map(a => DraftContext.saved(a.id, a)))
      case None =>
        document.draftContext
          .orElse(document.latestAnalysis.map(a => DraftContext.saved(a.id, a)))

  def latest(document: StudioDocument, family: AnalysisFamilyId): Option[AnalysisRevision] =
    document.draft
      .filter(d => document.familyOf(d.id).contains(family))
      .map(_.id)
      .orElse(
        document.analyses.findLast(a => document.familyOf(a.id).contains(family)).map(_.id)
      )

  def selected(model: AppModel): Option[DraftContext] =
    val document = model.document
    val trail    = model.navigation.trail(Perspective.Analysis)
    val revision = trail.reverseIterator.collectFirst { case Place.Revision(id) => id }
    val family   = trail.reverseIterator.collectFirst { case Place.Family(id) => id }
    revision
      .flatMap(id => context(document, Some(id)))
      .orElse(
        family.flatMap(id => latest(document, id)).flatMap(id => context(document, Some(id)))
      )
      .orElse(context(document, None))

  def name(document: StudioDocument, family: AnalysisFamilyId): Option[String] =
    document.draft
      .flatMap(_.origin match
        case DraftOrigin.NewFamily(value, _, _, _) if value.id == family => Some(value.name)
        case _                                                           => None)
      .orElse(document.analysisFamilies.flatMap(_.family(family)).map(_.name))
      .orElse(
        document.analyses
          .findLast(a => document.familyOf(a.id).contains(family))
          .map(_.studio.name.value)
      )
      .orElse(
        document.draftContext
          .filter(c => document.familyOf(c.id).contains(family))
          .map(_.studio.name.value)
      )

  def trail(document: StudioDocument, revision: AnalysisRevision): Vector[Place] =
    Vector(Place.Analyses) ++ document.familyOf(revision).map(Place.Family(_)) ++
      Vector(Place.Revision(revision))
