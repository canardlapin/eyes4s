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
import intaglio.interaction.{NamedPicking, NamedPickingPlan}
import intaglio.javafx.{JavaFxProgram, JavaFxRenderError, JavaFxRenderer}
import intaglio.{DeviceScene, IntaglioError, RenderPlan}

/** Why a canvas host could not show a scene. Every case names the scene or the
  * surface values it refused.
  */
enum CanvasPlotError derives CanEqual:

  /** The scene, its surface or its transform was refused. */
  case Scene(error: PlotSceneError)

  /** The host's size and output scale do not make a usable surface. */
  case Surface(error: PlotSceneError)

  /** The window's horizontal and vertical output scales differ; device pixels
    * must be square.
    */
  case AnisotropicScale(scaleX: Double, scaleY: Double)

  /** Intaglio's JavaFX backend refused to compile the scene on its surface. */
  case Compile(sceneId: SceneId, surface: PlotSurface, error: JavaFxRenderError)

  /** Intaglio could not compile the named picking plan of the scene on its surface. */
  case Picking(sceneId: SceneId, surface: PlotSurface, error: IntaglioError)

  /** Compilation threw, which Intaglio's contract does not allow; a defect upstream. */
  case Unexpected(sceneId: SceneId, surface: PlotSurface, description: String)

  def message: String = this match
    case Scene(e)               => e.message
    case Surface(e)             => e.message
    case AnisotropicScale(x, y) =>
      s"the window's output scale is ${x}x horizontally and ${y}x vertically; " +
        "the canvas host needs square device pixels"
    case Compile(id, surface, e) =>
      s"scene ${id.value} on a ${surface.deviceWidth}x${surface.deviceHeight} surface: " +
        s"Intaglio JavaFX compile failed: ${e.message}"
    case Picking(id, surface, e) =>
      s"scene ${id.value} on a ${surface.deviceWidth}x${surface.deviceHeight} surface: " +
        s"Intaglio picking plan failed: ${e.message}"
    case Unexpected(id, surface, d) =>
      s"scene ${id.value} on a ${surface.deviceWidth}x${surface.deviceHeight} surface: " +
        s"compilation threw $d"

/** One compiled scene on one surface: what a canvas host draws.
  *
  * `plan` is the scene bound to the render context it was compiled against,
  * `program` the JavaFX drawing operations in device pixels, and `transform`
  * the data, device and canvas mapping for the same context. `device` is the
  * plan lowered to device pixels, with the resolved frame (and inverse) of
  * every named viewport, and `picking` Intaglio's named picking plan of the
  * same scene and context (S4.2): input resolves against exactly what is
  * drawn. Both are built off the FX thread with the program.
  */
final case class PlotFrame private (
    sceneId: SceneId,
    surface: PlotSurface,
    transform: PlotTransform,
    plan: RenderPlan,
    program: JavaFxProgram,
    device: DeviceScene,
    picking: NamedPickingPlan
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
      device <- plan.deviceScene.left.map(e =>
        CanvasPlotError.Scene(PlotSceneError.Graphics(scene.id.value, "lowering the scene", e))
      )
      picking <- NamedPicking
        .compile(scene.scene, plan.context)
        .left
        .map(CanvasPlotError.Picking(scene.id, surface, _))
    yield PlotFrame(scene.id, surface, transform, plan, program, device, picking)
