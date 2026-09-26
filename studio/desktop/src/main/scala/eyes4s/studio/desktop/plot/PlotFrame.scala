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

package eyes4s.studio.desktop.plot

import eyes4s.studio.viz.plot.{PlotScene, PlotSceneError, PlotSurface, PlotTransform, SceneId}
import intaglio.RenderPlan
import intaglio.javafx.{JavaFxProgram, JavaFxRenderError, JavaFxRenderer}

/** Why a canvas host could not show a scene. Every case names the scene. */
enum CanvasPlotError derives CanEqual:

  /** The scene, its surface or its transform was refused. */
  case Scene(error: PlotSceneError)

  /** Intaglio's JavaFX backend refused to compile the scene on its surface. */
  case Compile(sceneId: SceneId, surface: PlotSurface, error: JavaFxRenderError)

  /** Compilation threw, which Intaglio's contract does not allow; a defect upstream. */
  case Unexpected(sceneId: SceneId, surface: PlotSurface, description: String)

  def message: String = this match
    case Scene(e)                => e.message
    case Compile(id, surface, e) =>
      s"scene ${id.value} on a ${surface.deviceWidth}x${surface.deviceHeight} surface: " +
        s"Intaglio JavaFX compile failed: ${e.message}"
    case Unexpected(id, surface, d) =>
      s"scene ${id.value} on a ${surface.deviceWidth}x${surface.deviceHeight} surface: " +
        s"compilation threw $d"

/** One compiled scene on one surface: what a canvas host draws.
  *
  * `plan` is the scene bound to the render context it was compiled against,
  * `program` the JavaFX drawing operations in device pixels, and `transform`
  * the data, device and canvas mapping for the same context. An input adapter
  * (S4.2) compiles its picking plan from `plan`, so picking, marks and the
  * transform share one layout.
  */
final case class PlotFrame private (
    sceneId: SceneId,
    surface: PlotSurface,
    transform: PlotTransform,
    plan: RenderPlan,
    program: JavaFxProgram
)

object PlotFrame:

  /** Lays `scene` out on `surface` and compiles it. Pure and thread-free. */
  def compile(scene: PlotScene, surface: PlotSurface): Either[CanvasPlotError, PlotFrame] =
    for
      transform <- PlotTransform.resolve(scene, surface).left.map(CanvasPlotError.Scene(_))
      plan = RenderPlan(scene.scene, transform.renderContext)
      program <- JavaFxRenderer
        .compile(plan)
        .left
        .map(CanvasPlotError.Compile(scene.id, surface, _))
    yield PlotFrame(scene.id, surface, transform, plan, program)
