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

package eyes4s.studio.viz.plot

import intaglio.{IntaglioError, Interval, Scene, SceneSemantics, Viewport, YDirection}

/** Why a plot scene, surface or transform could not be built. Every case names
  * the values it rejected.
  */
enum PlotSceneError derives CanEqual:

  /** A scene identity must contain a non-blank character. */
  case BlankSceneId(value: String)

  /** The data panel is rotated; the canvas transform is axis-aligned only. */
  case RotatedPanel(sceneId: String, degrees: Double)

  /** A data axis has zero width, so canvas positions cannot be mapped back to it. */
  case DegenerateDomain(sceneId: String, axis: String, lower: Double, upper: Double)

  /** A surface needs a positive, finite logical size and device scale. */
  case InvalidSurface(logicalWidth: Double, logicalHeight: Double, deviceScale: Double)

  /** No top-level grob of the scene is drawn in the data panel. */
  case PanelNotInScene(sceneId: String, topLevelGrobs: Int)

  /** The data panel resolved to no area on the surface. */
  case EmptyPanel(sceneId: String, deviceWidth: Double, deviceHeight: Double)

  /** Intaglio refused a value while `during` (for example resolving the panel). */
  case Graphics(sceneId: String, during: String, error: IntaglioError)

  def message: String = this match
    case BlankSceneId(v)     => s"scene id '$v' is blank"
    case RotatedPanel(id, d) =>
      s"scene $id: the data panel is rotated by $d degrees; a canvas transform must be axis-aligned"
    case DegenerateDomain(id, a, lo, hi) =>
      s"scene $id: the $a domain [$lo, $hi] has zero width"
    case InvalidSurface(w, h, s) =>
      s"surface ${w}x$h at device scale $s: size and scale must be positive and finite"
    case PanelNotInScene(id, n) =>
      s"scene $id: none of its $n top-level grobs is drawn in the data panel's viewport"
    case EmptyPanel(id, w, h) =>
      s"scene $id: the data panel resolves to ${w}x$h device pixels, which has no area"
    case Graphics(id, during, e) => s"scene $id: Intaglio refused $during: ${e.message}"

/** The identity of a plot scene.
  *
  * A builder names what a scene shows, so a host can tell a new scene from the
  * same scene re-laid out at another size, and an input adapter (S4.2) can key
  * a picking plan to the scene it was compiled from.
  */
final case class SceneId private (value: String) derives CanEqual

object SceneId:
  def apply(value: String): Either[PlotSceneError, SceneId] =
    val trimmed = value.trim
    if trimmed.isEmpty then Left(PlotSceneError.BlankSceneId(value))
    else Right(new SceneId(trimmed))

/** A position in a scene's data coordinates, the units of its panel's scales. */
final case class DataPoint(x: Double, y: Double) derives CanEqual

/** The panel whose data coordinates a scene's marks share.
  *
  * It is the Intaglio viewport that the scene's data grobs are drawn in. Its
  * placement is resolved by Intaglio itself ([[PlotTransform]]), so the scene,
  * its marks and any picking resolve positions through the same frame. The
  * panel is axis-aligned and both of its domains have positive width.
  */
final case class DataPanel private (viewport: Viewport):

  /** The data range across the panel, left to right. */
  def xDomain: Interval = viewport.xScale

  /** The data range across the panel, bottom to top when [[yDirection]] is up. */
  def yDomain: Interval = viewport.yScale

  /** Whether data y increases up the canvas (the default) or down it. */
  def yDirection: YDirection = viewport.yDirection

object DataPanel:
  def apply(sceneId: SceneId, viewport: Viewport): Either[PlotSceneError, DataPanel] =
    def domain(axis: String, interval: Interval): Either[PlotSceneError, Unit] =
      Either.cond(
        interval.width > 0.0,
        (),
        PlotSceneError.DegenerateDomain(sceneId.value, axis, interval.lower, interval.upper)
      )
    for
      _ <- Either.cond(
        viewport.angleDegrees == 0.0,
        (),
        PlotSceneError.RotatedPanel(sceneId.value, viewport.angleDegrees)
      )
      _ <- domain("x", viewport.xScale)
      _ <- domain("y", viewport.yScale)
    yield new DataPanel(viewport)

/** A scene built in studio-viz, ready for a canvas host.
  *
  * `scene` holds every grob; the data grobs among them are drawn in `panel`,
  * which is the viewport of one of its top-level grobs, so the panel resolves
  * against the device exactly as Intaglio resolves those grobs. The scene is
  * resolution-independent: a host lays it out again for each surface without
  * changing its data geometry.
  */
final case class PlotScene private (id: SceneId, scene: Scene, panel: DataPanel):

  /** The same scene carrying `semantics` (S4.6): its title, alt text and
    * summary, which an SVG export and a host's accessible text read.
    */
  def withSemantics(semantics: SceneSemantics): PlotScene =
    copy(scene = scene.withSemantics(semantics))

object PlotScene:
  def apply(id: SceneId, scene: Scene, panel: DataPanel): Either[PlotSceneError, PlotScene] =
    Either.cond(
      scene.grobs.exists(_.viewport.contains(panel.viewport)),
      new PlotScene(id, scene, panel),
      PlotSceneError.PanelNotInScene(id.value, scene.grobs.size)
    )
