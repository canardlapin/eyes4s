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

import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.StoryMoments

class SemanticInputRegistrySuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val original                          = get(StoryMoments.t2)
  private val source                            = get(SemanticIdentity.of("00112233445566ff"))
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
    "a saved recipe permits only initial source identity enrichment and retains every scientific field"
  ) {
    val recipe   = original.analysis(StoryMoments.rev4).get.recipe
    val declared = recipe.copy(input = Some(source))
    val enriched = get(RealRegistry.of(original).synchronize(document(declared))).next
    assertEquals(enriched.revisions(StoryMoments.rev4)._2.input, Some(source))
    assert(enriched.synchronize(document(declared)).isRight)
    assert(enriched.synchronize(document(recipe)).isLeft)
    assert(
      enriched
        .synchronize(
          document(declared.copy(input = Some(get(SemanticIdentity.of("ffeeddccbbaa9988")))))
        )
        .isLeft
    )
    assert(
      RealRegistry
        .of(original)
        .synchronize(document(declared.copy(grid = get(GridSize.of(32, 24)))))
        .isLeft
    )
  }
