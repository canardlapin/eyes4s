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
import eyes4s.codec.CanonicalDigest
import eyes4s.studio.core.document.{CoreBinding, StudioDocument, StudyPlanArtifact}
import eyes4s.studio.core.fixture.StoryMoments
import munit.CatsEffectSuite

class RealPlanBindingSuite extends CatsEffectSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)

  test("a saved plan binding cannot name a different recipe's native artifact") {
    val original = get(RealBackendConformanceSuite.trialLayout)
    val recorded = get(CanonicalDigest.parse[StudyPlanArtifact]("0" * 64))
    val document = get(
      StudioDocument.of(
        original.datasets,
        original.analyses.map(a =>
          if a.id == StoryMoments.rev4 then a.copy(plan = CoreBinding.Bound(recorded)) else a
        ),
        original.draft,
        original.runs,
        original.reporting,
        original.figures,
        original.presentation,
        original.jobs
      )
    )
    RealStudyBackend.resource[IO](document, RealBackendConformanceSuite.golden).use { backend =>
      for
        preview  <- backend.preview(StoryMoments.rev4)
        counting <- backend
          .previewCounting(
            StoryMoments.rev4,
            get(eyes4s.studio.core.preview.PreviewBudget.of(1))
          )
          .compile
          .toVector
        jobs <- backend.jobs
      yield
        assert(preview.isLeft)
        val message = preview.left.toOption.map(_.message).getOrElse(fail("no refusal"))
        assert(message.contains(StoryMoments.rev4.label), message)
        assert(message.contains(recorded.sha256.hex), message)
        assert(message.contains("prepared"), message)
        assertEquals(
          counting.map(_.left.toOption.map(_.code)),
          Vector(Some("studio-backend.registry-refused"))
        )
        assertEquals(jobs, Vector.empty)
    }
  }
