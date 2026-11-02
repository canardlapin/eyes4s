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

import eyes4s.codec.{ByteDigest, CanonicalDigest}
import eyes4s.studio.app.{AppEffect, AppModel, Intent}
import eyes4s.studio.app.layout.StudioLayouts
import eyes4s.studio.app.nav.{Location, Place}
import eyes4s.studio.core.backend.{AnalysisRevision, DatasetRevision}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.command.{Command, JournalEntry}
import eyes4s.studio.core.fixture.StoryMoments

class SpatialContextSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)

  private val base    = get(StoryMoments.t1)
  private val input   = get(CanonicalDigest.parse[LocationInputArtifact]("1" * 64))
  private val plan    = get(CanonicalDigest.parse[LocationPlanArtifact]("2" * 64))
  private val sources = get(LocationBundleSources.of(LocationBundleSources.roles.map { role =>
    Source(
      role,
      get(SourcePath.of(s"figrim/${role.label.replace(' ', '-')}.txt")),
      ByteDigest.sha256(IArray.from(role.label.getBytes)),
      None
    )
  }))
  private val datasetId  = DatasetRevision(base.datasets.last.id.number + 1)
  private val analysisId = AnalysisRevision(base.analyses.last.id.number + 1)
  private val locations  = LocationDatasetRevision(
    datasetId,
    None,
    sources,
    LocationProfile.FigrimV1,
    LocationAdmissionDecision.Admitted(input)
  )
  private val analysis = LocationAnalysisRevision(
    analysisId,
    datasetId,
    CoreBinding.Bound(plan),
    LocationRecipe(LocationProfile.FigrimV1, CoreBinding.Bound(input)),
    get(RevisionName.of("FIGRIM spatial comparison")),
    "fixed spatial profile"
  )
  private val document = get(
    StudioDocument.of(
      base.datasets :+ locations,
      base.analyses :+ analysis,
      None,
      base.runs,
      Vector.empty,
      Vector.empty,
      PresentationState.default,
      base.jobs
    )
  )

  private def navigate(model: AppModel, perspective: Perspective, trail: Vector[Place]) =
    AppModel.update(model, Intent.Navigate(Location(perspective, trail)))._1

  test("explicit timed and spatial selections resolve only their selected scientific kind") {
    val opened    = AppModel.open(document, None)
    val timedData =
      navigate(opened, Perspective.Data, Vector(Place.Dataset(base.datasets.head.id)))
    val spatialData   = navigate(opened, Perspective.Data, Vector(Place.Dataset(datasetId)))
    val timedAnalysis = navigate(
      opened,
      Perspective.Analysis,
      Vector(Place.Analyses, Place.Revision(base.analyses.head.id))
    )
    val spatialAnalysis = navigate(
      opened,
      Perspective.Analysis,
      Vector(Place.Analyses, Place.Revision(analysisId))
    )
    assertEquals(SpatialContext.data(timedData), None)
    assertEquals(SpatialContext.data(spatialData), Some(locations))
    assertEquals(SpatialContext.analysis(timedAnalysis), None)
    assertEquals(SpatialContext.analysis(spatialAnalysis), Some(analysis))
  }

  test("unknown explicit selections never fall back to the newest spatial definition") {
    val opened      = AppModel.open(document, None)
    val unknownData =
      navigate(opened, Perspective.Data, Vector(Place.Dataset(DatasetRevision(999))))
    val unknownAnalysis = navigate(
      opened,
      Perspective.Analysis,
      Vector(Place.Analyses, Place.Revision(AnalysisRevision(999)))
    )
    assertEquals(SpatialContext.data(unknownData), None)
    assertEquals(SpatialContext.analysis(unknownAnalysis), None)
  }

  test("only an explicitly selected spatial kind receives the spatial layout") {
    val opened = AppModel.open(document, None)
    val timed = navigate(opened, Perspective.Data, Vector(Place.Dataset(base.datasets.head.id)))
    val spatial = navigate(opened, Perspective.Data, Vector(Place.Dataset(datasetId)))
    assertEquals(
      timed.layout,
      StudioLayouts.layoutFor(Perspective.Data, timed.location.trail, true)
    )
    assertEquals(spatial.layout, StudioLayouts.spatial(Perspective.Data))
  }

  test("spatial results and report navigation do not bootstrap timed reporting") {
    val opened   = AppModel.open(document, None)
    val selected = navigate(
      opened,
      Perspective.Analysis,
      Vector(Place.Analyses, Place.Revision(analysisId))
    )
    val (results, resultEffects) =
      AppModel.update(selected, Intent.SwitchPerspective(Perspective.Results))
    val (report, reportEffects) =
      AppModel.update(results, Intent.SwitchPerspective(Perspective.Report))
    val bootstrap = (resultEffects ++ reportEffects).collect {
      case AppEffect.Journal(JournalEntry.Apply(_: Command.PutReporting)) =>
        true
    }
    assertEquals(bootstrap, Vector.empty)
    assertEquals(results.document.reporting, Vector.empty)
    assertEquals(report.document.reporting, Vector.empty)
    assertEquals(results.layout, StudioLayouts.spatial(Perspective.Results))
    assertEquals(report.layout, StudioLayouts.spatial(Perspective.Report))
  }
