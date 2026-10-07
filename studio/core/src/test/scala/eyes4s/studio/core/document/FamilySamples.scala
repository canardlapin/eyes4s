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

package eyes4s.studio.core.document

import eyes4s.studio.core.backend.*

/** Interleaved independent families with equal display names. */
object FamilySamples:
  def get[E, A](value: Either[E, A]): A =
    value.fold(error => throw new AssertionError(error.toString), identity)

  val a: AnalysisFamilyId = get(AnalysisFamilyId.of(1))
  val b: AnalysisFamilyId = get(AnalysisFamilyId.of(2))
  private val source      = DocumentSamples.t2
  private val seed        = source.analyses.last
  val a1                  = seed.copy(id = AnalysisRevision(1))
  val b2                  = seed.copy(id = AnalysisRevision(2))
  val a3                  = seed.copy(id = AnalysisRevision(3))
  val b4                  = seed.copy(id = AnalysisRevision(4))
  val analyses            = Vector(a1, b2, a3, b4)
  val registry            = get(
    AnalysisFamilyRegistry.of(
      Vector(get(AnalysisFamily.of(a, "Same name")), get(AnalysisFamily.of(b, "Same name"))),
      Vector((a1.id, a), (b2.id, b), (a3.id, a), (b4.id, b)).map((revision, family) =>
        get(AnalysisFamilyOwner.of(revision, family))
      ),
      analyses.map(_.id)
    )
  )
  val change: RecipeChange = RecipeChange.Grid(a1.recipe.grid, get(GridSize.of(32, 24)))
  val draftA: Draft        = get(Draft.against(AnalysisRevision(5), a1, None, Vector(change)))

  def run(number: Int, analysis: AnalysisRevisionSpec, state: RunLifecycle): RunRef =
    RunRef(RunId(number), analysis.id, analysis.dataset, state, CoreBinding.unbound)

  def document(
      runs: Vector[RunRef] = Vector.empty,
      draft: Option[Draft] = None,
      shown: Option[RunId] = None,
      figures: Vector[FigureSpec] = Vector.empty
  ): StudioDocument =
    val p    = source.presentation
    val view = get(
      PresentationState.of(
        p.perspective,
        p.theme,
        p.stage,
        p.mapOpacity,
        p.underlay,
        shown,
        p.layouts
      )
    )
    get(
      StudioDocument.of(
        source.datasets,
        analyses,
        draft,
        runs,
        source.reporting,
        figures,
        view,
        Vector.empty,
        Some(registry)
      )
    )
