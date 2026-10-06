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
import eyes4s.studio.core.command.{Command, History}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.{GoldenAssets, GoldenCsv, StoryMoments}
import munit.CatsEffectSuite

class RealSemanticInputBindingSuite extends CatsEffectSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private lazy val original                     = get(RealBackendConformanceSuite.trialLayout)
  private lazy val spec                         = original.dataset(StoryMoments.r3).get
  private def admit(text: String)               = get(
    RealAdmission.admit(spec, text, GoldenCsv.trials, get(GoldenAssets.registry(spec)))
  )
  private lazy val admitted   = admit(GoldenCsv.fixations)
  private lazy val semantic   = SemanticIdentity.fromCore(admitted.evidence.source.records)
  private lazy val configured = get(
    RealPrepared.configure(
      StoryMoments.rev4,
      StoryMoments.r3,
      original.analysis(StoryMoments.rev4).get.recipe,
      admitted
    )
  )
  private def document(recipe: Recipe): StudioDocument = get(
    StudioDocument.of(
      original.datasets,
      original.analyses.map(a =>
        if a.id == StoryMoments.rev4 then a.copy(recipe = recipe) else a
      ),
      original.draft,
      original.runs,
      original.reporting,
      original.figures,
      original.presentation,
      original.jobs
    )
  )

  test(
    "BindPlan's parsed source identity enriches the registry while canonical input remains a separate identity"
  ) {
    val plan = get(RealPreview.stamp(configured)).plan match
      case CoreBinding.Bound(value) => value
      case _                        => fail("native plan was not bound")
    val bound = get(
      History.start(original).apply(Command.BindPlan(StoryMoments.rev4, plan, semantic))
    ).history.document
    assertEquals(
      semantic,
      SemanticIdentity.fromCore(
        admit(GoldenCsv.fixations.replace("\n", "\r\n")).evidence.source.records
      )
    )
    val extra   = GoldenCsv.fixations.linesIterator.map(line => line + ",unused").mkString("\n")
    val renamed = admit(extra)
    assertNotEquals(semantic, SemanticIdentity.fromCore(renamed.evidence.source.records))
    assertEquals(
      renamed.input.reference,
      admitted.input.reference,
      "unused source fields change source semantics, not the admitted payload"
    )
    RealStudyBackend.resource[IO](original, RealBackendConformanceSuite.golden).use { backend =>
      for
        registered <- backend.synchronize(bound)
        preview    <- backend.preview(StoryMoments.rev4)
        cleared    <- backend.synchronize(
          document(original.analysis(StoryMoments.rev4).get.recipe)
        )
      yield
        assertEquals(registered, Right(()))
        assert(preview.isRight, preview.toString)
        assert(cleared.isLeft)
    }
  }

  test(
    "incorrect source identity refuses before publishing any registry change or starting a job"
  ) {
    val wrong =
      get(SemanticIdentity.of(if semantic.value == "0000000000000000" then "ffffffffffffffff"
      else "0000000000000000"))
    val attempted =
      document(original.analysis(StoryMoments.rev4).get.recipe.copy(input = Some(wrong)))
    RealStudyBackend.resource[IO](original, RealBackendConformanceSuite.golden).use { backend =>
      for
        refused       <- backend.synchronize(attempted)
        stillOriginal <- backend.preview(StoryMoments.rev4)
        jobs          <- backend.jobs
      yield
        assertEquals(refused.left.toOption.map(_.code), Some("studio-backend.registry-refused"))
        val message = refused.left.toOption.map(_.message).getOrElse(fail("missing refusal"))
        assert(message.contains(StoryMoments.rev4.label))
        assert(message.contains(wrong.value))
        assert(message.contains(semantic.value))
        assert(stillOriginal.isRight, stillOriginal.toString)
        assertEquals(jobs, Vector.empty)
    }
  }
