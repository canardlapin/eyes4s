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

package eyes4s.studio.core.real

import cats.effect.IO
import eyes4s.kernel.Sigma
import eyes4s.plan.{DensityView, StudyEstimate}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.fixture.{GoldenAssets, GoldenCsv, StoryMoments}
import munit.CatsEffectSuite

class RealTrialPreviewSuite extends CatsEffectSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private lazy val document                     = get(RealBackendConformanceSuite.trialLayout)
  private lazy val prepared                     =
    val dataset  = get(document.dataset(StoryMoments.r3).toRight("no dataset"))
    val admitted = get(
      RealAdmission.admit(
        dataset,
        GoldenCsv.fixations,
        GoldenCsv.trials,
        get(GoldenAssets.registry(dataset))
      )
    )
    val recipe = get(document.analysis(StoryMoments.rev4).toRight("no analysis")).recipe
    get(RealPrepared.of(StoryMoments.rev4, StoryMoments.r3, recipe, admitted))

  test(
    "real trial preview carries native density, declared geometry and preview-specific levels"
  ) {
    val trial  = TrialKey("P17", Phase.Retrieval, "ret_07", 1)
    val source = prepared.admitted.input.trials.rows
      .find(t =>
        t.key.participant == trial.participant && t.key.phase == trial.phase.label && t.key.trial == trial.trial && t.key.occurrence.value == trial.occurrence
      )
      .getOrElse(fail("no focus trial"))
    val angular = prepared.plan.angularScale.getOrElse(fail("no angular scale"))
    val sigma   = get(Sigma.deg(2.0).flatMap(angular.sigma))
    val mass    = get(
      eyes4s.plan.StudyDensity.estimate(
        prepared.plan,
        StudyEstimate.Gaussian(sigma, eyes4s.surface.EdgePolicy.Truncate),
        source.key,
        source.value
      )
    )
    val density = get(DensityView.of(mass))
    val levels  = get(density.levels(Vector(0.5, 0.8)))
    RealStudyBackend.resource[IO](document, RealBackendConformanceSuite.golden).use { backend =>
      backend.trialPreview(StoryMoments.rev4, trial).map { result =>
        val view = get(result)
        assertEquals(view.cells, mass.values.toVector.map(Some(_)))
        assertEquals(view.levels, levels.map(_.threshold))
        assertEquals(view.sigmaDegrees, 2.0)
        assertEquals((view.columns, view.rows), (mass.grid.nx, mass.grid.ny))
        val geometry = prepared.plan.geometry
        geometry match
          case eyes4s.plan.StudyGeometry.Windowed(window, _, _) =>
            assertEquals(
              (view.region.left, view.region.top, view.region.right, view.region.bottom),
              (window.region.xMin, window.region.yMin, window.region.xMax, window.region.yMax)
            )
          case _ => fail("expected windowed golden plan")
        assertEquals(view.order, RowOrder.TopFirst)
      }
    }
  }

  test("real preview refuses an all-outside trial and names an unknown trial") {
    val outsideKey = prepared.work.windowTallies
      .collectFirst {
        case (key, Right(tally)) if tally.allOutside =>
          TrialKey(key.participant, Phase(key.phase), key.trial, key.occurrence.value)
      }
      .getOrElse(fail("golden input must contain an all-outside trial"))
    RealStudyBackend.resource[IO](document, RealBackendConformanceSuite.golden).use { backend =>
      for
        outside <- backend.trialPreview(StoryMoments.rev4, outsideKey)
        unknown <- backend.trialPreview(
          StoryMoments.rev4,
          TrialKey("P99", Phase.Retrieval, "missing", 1)
        )
      yield
        assertEquals(
          outside.left.toOption.map(_.code),
          Some("studio-backend.trial-view-refused")
        )
        assertEquals(
          unknown,
          Left(
            BackendError
              .UnknownTrial(StoryMoments.r3, TrialKey("P99", Phase.Retrieval, "missing", 1))
          )
        )
    }
  }
