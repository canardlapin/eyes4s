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

import intaglio.{
  DeviceFrame,
  DevicePoint,
  LengthExpr,
  LengthResolver,
  RenderContext,
  YDirection
}

/** A position on a host's canvas in logical (layout) pixels, y down. */
final case class CanvasPoint(x: Double, y: Double) derives CanEqual

/** The drawing surface a scene is laid out for: a logical size, in the
  * layout pixels of the host, and the device scale of the display.
  *
  * The device raster is the logical size times the scale, rounded up to whole
  * pixels, so a 2x display gets twice the pixels in each direction. Physical
  * lengths (points) resolve at 96 logical pixels per inch.
  */
final case class PlotSurface private (
    logicalWidth: Double,
    logicalHeight: Double,
    deviceScale: Double
) derives CanEqual:

  /** The raster width in device pixels. */
  def deviceWidth: Int = PlotSurface.devicePixels(logicalWidth, deviceScale)

  /** The raster height in device pixels. */
  def deviceHeight: Int = PlotSurface.devicePixels(logicalHeight, deviceScale)

  /** The Intaglio render context for this surface. */
  def renderContext(sceneId: SceneId): Either[PlotSceneError, RenderContext] =
    RenderContext(
      width = deviceWidth,
      height = deviceHeight,
      pixelsPerInch = PlotSurface.LogicalPixelsPerInch * deviceScale,
      deviceScale = deviceScale
    ).left.map(PlotSceneError.Graphics(sceneId.value, "the surface's render context", _))

object PlotSurface:

  /** Physical units resolve at this many logical pixels per inch (CSS and JavaFX). */
  val LogicalPixelsPerInch: Double = 96.0

  // A raster edge within this many device pixels of a whole pixel is that
  // pixel, so 480 x 1.25 is 600 pixels even when the product rounds up.
  private val SnapEpsilon = 1e-6

  /** The largest raster edge accepted, in device pixels. It is a conservative
    * bound chosen to stay within the texture size JavaFX pipelines commonly
    * support; it is not a limit JavaFX guarantees, and a pipeline with a
    * smaller maximum texture can still fail to draw an accepted surface.
    */
  val MaxDevicePixels: Int = 8192

  def apply(
      logicalWidth: Double,
      logicalHeight: Double,
      deviceScale: Double
  ): Either[PlotSceneError, PlotSurface] =
    def valid(v: Double) = v.isFinite && v > 0.0
    def fits(v: Double)  = v * deviceScale <= MaxDevicePixels.toDouble
    if valid(logicalWidth) && valid(logicalHeight) && valid(deviceScale) &&
      fits(logicalWidth) && fits(logicalHeight)
    then Right(new PlotSurface(logicalWidth, logicalHeight, deviceScale))
    else Left(PlotSceneError.InvalidSurface(logicalWidth, logicalHeight, deviceScale))

  private def devicePixels(logical: Double, scale: Double): Int =
    math.max(1, math.ceil(logical * scale - SnapEpsilon).toInt)

/** The one mapping between a scene's data coordinates, the device raster and
  * the host canvas, for one scene on one surface.
  *
  * The panel frame is resolved by Intaglio's own `LengthResolver`, and data
  * positions are resolved through it as native lengths, exactly as Intaglio
  * lowers the scene's marks. The scene drawn on the canvas, the marks in it and
  * a picking plan compiled against [[PlotSurface.renderContext]] (S4.2)
  * therefore agree by construction; `CanvasPlotHostFxSuite` checks the drawn
  * marks against it.
  *
  * `renderContext` is the context the scene is compiled against on this
  * surface; a picking plan must use the same one.
  *
  * Resizing changes the surface and so the panel frame, never the data
  * domains: a data point maps to a new canvas position, and that position maps
  * back to the same data point.
  */
final class PlotTransform private (
    val sceneId: SceneId,
    val surface: PlotSurface,
    val panel: DataPanel,
    val renderContext: RenderContext,
    val panelFrame: DeviceFrame
):
  private val resolver =
    LengthResolver(
      renderContext.deviceContext,
      panelFrame,
      renderContext.fontRegistry,
      renderContext.lineHeightPt
    )

  /** Where Intaglio draws the data point `p`, in device pixels. */
  def dataToDevice(p: DataPoint): Either[PlotSceneError, DevicePoint] =
    val point =
      for
        x <- LengthExpr.native(p.x).flatMap(resolver.x)
        y <- LengthExpr.native(p.y).flatMap(resolver.y)
      yield DevicePoint(x, y)
    point.left.map(PlotSceneError.Graphics(sceneId.value, s"data point (${p.x}, ${p.y})", _))

  /** The data point drawn at device position `d`; the inverse of [[dataToDevice]]. */
  def deviceToData(d: DevicePoint): DataPoint =
    val fx = (d.x - panelFrame.x) / panelFrame.width
    val fy = panelFrame.yDirection match
      case YDirection.Up   => (panelFrame.y + panelFrame.height - d.y) / panelFrame.height
      case YDirection.Down => (d.y - panelFrame.y) / panelFrame.height
    DataPoint(
      panel.xDomain.lower + fx * panel.xDomain.width,
      panel.yDomain.lower + fy * panel.yDomain.width
    )

  /** A device position in the host's logical canvas pixels. */
  def deviceToCanvas(d: DevicePoint): CanvasPoint =
    CanvasPoint(d.x / surface.deviceScale, d.y / surface.deviceScale)

  /** A logical canvas position in device pixels. */
  def canvasToDevice(c: CanvasPoint): DevicePoint =
    DevicePoint(c.x * surface.deviceScale, c.y * surface.deviceScale)

  /** Where the data point `p` appears on the host canvas, in logical pixels. */
  def dataToCanvas(p: DataPoint): Either[PlotSceneError, CanvasPoint] =
    dataToDevice(p).map(deviceToCanvas)

  /** The data point under a logical canvas position. */
  def canvasToData(c: CanvasPoint): DataPoint =
    deviceToData(canvasToDevice(c))

object PlotTransform:

  /** Resolves `scene`'s panel on `surface`. */
  def resolve(scene: PlotScene, surface: PlotSurface): Either[PlotSceneError, PlotTransform] =
    for
      context <- surface.renderContext(scene.id)
      root = DeviceFrame.root(context.deviceContext)
      frame <- LengthResolver(
        context.deviceContext,
        root,
        context.fontRegistry,
        context.lineHeightPt
      )
        .childFrame(scene.panel.viewport)
        .left
        .map(PlotSceneError.Graphics(scene.id.value, "the data panel's frame", _))
      nonEmpty <- Either.cond(
        frame.width > 0.0 && frame.height > 0.0,
        frame,
        PlotSceneError.EmptyPanel(scene.id.value, frame.width, frame.height)
      )
    yield new PlotTransform(scene.id, surface, scene.panel, context, nonEmpty)
