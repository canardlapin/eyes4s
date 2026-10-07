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

import eyes4s.studio.viz.plot.{PlotScene, PlotSurface, PlotTransform, SceneId}
import intaglio.javafx.{JavaFxCanvasContext, JavaFxGraphicsContext, JavaFxRenderer}
import javafx.application.Platform
import javafx.beans.property.{ReadOnlyObjectProperty, ReadOnlyObjectWrapper}
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.collections.{ListChangeListener, WeakListChangeListener}
import javafx.scene.canvas.{Canvas, GraphicsContext}
import javafx.scene.layout.Region
import javafx.scene.shape.Rectangle
import javafx.scene.transform.Scale
import javafx.stage.{Screen, Window}

import java.util.concurrent.{Executor, ExecutorService, Executors, RejectedExecutionException}
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/** What a [[CanvasPlotHost]] is showing. */
enum PlotHostStatus derives CanEqual:

  /** No scene. */
  case Empty

  /** A scene, but no surface yet: the host is not in a window or has no area. An
    * unusable surface (too large, or a non-square output scale) is [[Failed]].
    */
  case Waiting(sceneId: SceneId)

  /** The scene is being compiled for `surface`; the previous frame, if any, stays up. */
  case Compiling(sceneId: SceneId, surface: PlotSurface)

  /** `frame` is on the canvas. */
  case Drawn(frame: PlotFrame)

  /** The scene could not be shown on this surface; the canvas is blank. */
  case Failed(error: CanvasPlotError)

  /** The host was disposed and shows nothing. */
  case Disposed

/** Draws feedback over a [[CanvasPlotHost]]'s frame without touching the
  * scene (S4.2): hover, selection and focus rings.
  */
trait PlotOverlay:
  /** Paints the fast-changing layer (hover, focus) on the host's top overlay
    * canvas, whose raster is `frame`'s device raster (device pixels, cleared
    * before each call). FX thread only.
    */
  def paint(gc: GraphicsContext, frame: PlotFrame): Unit

  /** Paints the slow-changing layer (the selection) on a canvas between the
    * scene and the top layer. It is redrawn only with the scene or on
    * `repaintOverlay(under = true)`, so a large selection is not redrawn on
    * every pointer move.
    */
  def paintUnder(gc: GraphicsContext, frame: PlotFrame): Unit = ()

/** How often a host compiled a scene, drew its base canvas, its top overlay
  * and its under-overlay (selection) layer.
  */
final case class PlotHostProfile(
    compiles: Long,
    baseDraws: Long,
    overlayDraws: Long,
    underDraws: Long = 0L
) derives CanEqual

/** An Intaglio scene on a JavaFX canvas (S4.1).
  *
  * The host lays a [[PlotScene]] out for its current size and the output scale
  * of its window, compiles it with Intaglio's pure `JavaFxRenderer.compile` on
  * `compiler`, off the FX thread, and draws the compiled program on a canvas on
  * the FX thread.
  *
  * '''HiDPI.''' The canvas texture has one pixel per device pixel: its raster
  * is the surface's device size, so a 2x display gets a 2x raster rather than
  * an upscaled 1x one. JavaFX already backs every canvas with a texture
  * [[CanvasPlotHost.canvasPixelScale]] times its size (the highest output scale
  * of any screen, rounded up), so the canvas is the device size divided by that
  * scale, draws the device-pixel program through a matching `1/k` graphics
  * transform, and a node transform brings it back to layout size. Sizing the
  * canvas to device pixels instead made the texture k times too large on each
  * edge on a HiDPI screen, and a 2x screen then exhausted Prism's texture pool
  * (JavaFX dereferences the null texture: bd-01M3DPFKG2MXDGW7SS92FWXJSW).
  * A canvas's peer fixes its texture scale when it is created, so the host
  * records the scale with its canvases and, when its window's output scale or
  * the screens change it, replaces them and redraws the frame on the new ones.
  * Device pixels are assumed square: a window whose horizontal and vertical
  * output scales differ is refused with [[CanvasPlotError.AnisotropicScale]].
  * [[setOutputScaleOverride]] replaces the window's scale, for snapshots and
  * tests.
  *
  * '''Resize''' lays the same scene out again. Its data geometry does not
  * change; the [[PlotTransform]] of each frame maps data, device and canvas
  * coordinates for that frame's layout, and it is the transform the frame's
  * marks were lowered with. A compile superseded by a later size or scene is
  * discarded, not drawn, and at most one compile per host is in flight.
  *
  * '''Hooks for input (S4.2).''' [[frame]] exposes the drawn [[PlotFrame]]: its
  * scene identity, its transform, its device scene with resolved viewport
  * frames and its named picking plan. The host itself handles no input. An
  * input adapter listens on the host `Region`, not on the canvases, which are
  * mouse-transparent unmanaged children, replaced when the texture scale
  * changes, whose local units are device pixels divided by that scale; the
  * host's local coordinates are the logical canvas coordinates of
  * [[eyes4s.studio.viz.plot.CanvasPoint]]. Feedback is drawn by a
  * [[PlotOverlay]] on two canvases above the scene, a slow selection layer
  * and a fast hover/focus layer: [[repaintOverlay]] redraws the top one (and
  * the selection layer only when asked), never compiling or redrawing the
  * scene ([[profile]] counts each).
  *
  * '''Disposal.''' [[dispose]] stops listening to the scene, window and screens,
  * releases the canvas raster and the renderer's image and pattern caches, and
  * ignores compiles still in flight. The renderer's caches are also dropped
  * whenever a different scene value is shown, even under the same
  * [[eyes4s.studio.viz.plot.SceneId]], so replaced rasters do not accumulate.
  * A host is used on the FX thread only.
  */
final class CanvasPlotHost private[plot] (
    compiler: Executor,
    renderer: GraphicsContext => JavaFxGraphicsContext,
    pixelScaleOf: () => Double
) extends Region:

  /** A host compiling on `compiler`, drawing through `renderer`. */
  private[plot] def this(
      compiler: Executor,
      renderer: GraphicsContext => JavaFxGraphicsContext
  ) = this(compiler, renderer, () => CanvasPlotHost.canvasPixelScale)

  /** A host compiling on `compiler`. */
  def this(compiler: Executor) = this(compiler, gc => JavaFxCanvasContext(gc))

  /** A host compiling on the shared studio plot compiler. */
  def this() = this(CanvasPlotHost.sharedCompiler)

  // The canvases, created for the texture scale their peers will be given.
  private var layers  = CanvasLayers(pixelScaleOf())
  private def canvas  = layers.scene.canvas
  private def overlay = layers.overlay.canvas
  private def under   = layers.under.canvas
  getChildren.addAll(layers.nodes*)
  getStyleClass.add("plot-host")

  // The device raster rounds up to whole pixels; keep it inside the host.
  private val bounds = Rectangle()
  bounds.widthProperty.bind(widthProperty)
  bounds.heightProperty.bind(heightProperty)
  setClip(bounds)

  private val statusWrapper =
    ReadOnlyObjectWrapper[PlotHostStatus](this, "status", PlotHostStatus.Empty)

  private var plotScene: Option[PlotScene]  = None
  private var scaleOverride: Option[Double] = None
  // Right(None): no surface yet; Left: the surface cannot be used.
  private var surface: Either[CanvasPlotError, Option[PlotSurface]] = Right(None)
  private var drawing: Option[JavaFxGraphicsContext]                = None
  private var shown: Option[PlotFrame]                              = None
  private var requested: Long                                       = 0L
  private var inFlight: Boolean                                     = false
  private var disposed: Boolean                                     = false
  private var painter: Option[PlotOverlay]                          = None
  private var compiles: Long                                        = 0L
  private var baseDraws: Long                                       = 0L
  private var overlayDraws: Long                                    = 0L
  private var underDraws: Long                                      = 0L

  // The output scale of the window this host is in. The chain observes the
  // current scene's window property and that window's scale only while the
  // listener below is attached, and follows the host from scene to scene.
  private val window: ObservableValue[Window] = sceneProperty.flatMap[Window](_.windowProperty)
  private val windowScaleX: ObservableValue[Number] =
    window.flatMap[Number](_.outputScaleXProperty)
  private val windowScaleY: ObservableValue[Number] =
    window.flatMap[Number](_.outputScaleYProperty)

  private val relayout: ChangeListener[Any] = (_, _, _) => updateSurface()
  // A window moving between screens, or screens changing, can change the
  // texture scale new canvas peers get; check it before laying out again.
  private val rescale: ChangeListener[Any] = (_, _, _) =>
    refreshPixelScale()
    updateSurface()
  private val screensChanged: ListChangeListener[Screen] = _ => refreshPixelScale()
  // Weak, so a host that is never disposed does not stay reachable from the screens.
  private val screensWeak = WeakListChangeListener(screensChanged)
  windowScaleX.addListener(rescale)
  windowScaleY.addListener(rescale)
  widthProperty.addListener(relayout)
  heightProperty.addListener(relayout)
  Screen.getScreens.addListener(screensWeak)

  /** What the host is showing. */
  def status: ReadOnlyObjectProperty[PlotHostStatus] = statusWrapper.getReadOnlyProperty

  /** The frame whose pixels are on the canvas, if any.
    *
    * While a new layout or scene compiles, this stays the previous frame, which
    * is still what the user sees; input should be resolved against it.
    */
  def frame: Option[PlotFrame] = shown

  /** The transform of the frame on the canvas, if any. */
  def transform: Option[PlotTransform] = frame.map(_.transform)

  /** The device scale the host lays out for: the override, else the window's
    * horizontal output scale (which must equal its vertical one).
    */
  def outputScale: Option[Double] =
    scaleOverride.orElse(Option(windowScaleX.getValue).map(_.doubleValue))

  // The override, else the window's scale if its pixels are square.
  private def squareScale: Either[CanvasPlotError, Option[Double]] =
    scaleOverride match
      case Some(s) => Right(Some(s))
      case None    =>
        (Option(windowScaleX.getValue), Option(windowScaleY.getValue)) match
          case (Some(x), Some(y)) if x.doubleValue != y.doubleValue =>
            Left(CanvasPlotError.AnisotropicScale(x.doubleValue, y.doubleValue))
          case (Some(x), _) => Right(Some(x.doubleValue))
          case (None, _)    => Right(None)

  /** The text summary `scene` carries (S4.6), if it carries one. */
  def summaryOf(scene: PlotScene): Option[String] =
    scene.scene.semantics.plots.headOption.flatMap(_.description)

  /** Shows `scene`, replacing the current one. */
  def show(scene: PlotScene): Unit =
    onFxThread("show")
    if !disposed then
      // A new scene value may carry new rasters: start with empty caches.
      if !plotScene.exists(_ eq scene) then drawing = None
      plotScene = Some(scene)
      // The scene's text summary (S4.6) is the host's accessible help: what
      // every mark accounts for, beside the summary its text names.
      setAccessibleHelp(summaryOf(scene).orNull)
      schedule()

  /** Draws `next` over the scene from now on; `None` removes the overlay. */
  def setOverlay(next: Option[PlotOverlay]): Unit =
    onFxThread("setOverlay")
    if !disposed then
      painter = next
      repaintOverlay(under = true)

  /** Redraws the top overlay layer, and with `under` the selection layer, over
    * the frame on the canvas; the scene is untouched.
    */
  def repaintOverlay(under: Boolean = false): Unit =
    onFxThread("repaintOverlay")
    if !disposed then
      if under then
        val ugc = this.under.getGraphicsContext2D
        ugc.clearRect(0.0, 0.0, this.under.getWidth, this.under.getHeight)
        for
          frame <- shown
          paint <- painter
        do
          inDevicePixels(ugc)(paint.paintUnder(ugc, frame))
          underDraws += 1
      val gc = overlay.getGraphicsContext2D
      gc.clearRect(0.0, 0.0, overlay.getWidth, overlay.getHeight)
      for
        frame <- shown
        paint <- painter
      do
        inDevicePixels(gc)(paint.paint(gc, frame))
        overlayDraws += 1

  /** The texture JavaFX backs each of the host's canvases with, in pixels: the
    * canvas size times the texture scale the canvases were created for.
    */
  private[plot] def canvasTexture: (Int, Int) =
    val k = layers.pixelScale
    (math.ceil(canvas.getWidth * k).toInt, math.ceil(canvas.getHeight * k).toInt)

  /** The texture scale the current canvases were created for. */
  private[plot] def texturePixelScale: Double = layers.pixelScale

  /** Recreates the canvases if the texture scale new canvas peers would get has
    * changed, and redraws the frame on them from its compiled program. A
    * canvas's peer fixes its texture scale when it is created, so a canvas made
    * for another scale would draw at too low a resolution or with too large a
    * texture. The host calls this when its window's output scale or the
    * screens change.
    */
  private[plot] def refreshPixelScale(): Unit =
    if replaceStaleLayers() then
      shown match
        case Some(frame) =>
          draw(frame)
          repaintOverlay(under = true)
        case None => ()

  // Replaces the canvases if their texture scale is stale; true if it did.
  private def replaceStaleLayers(): Boolean =
    val k     = pixelScaleOf()
    val stale = !disposed && k != layers.pixelScale
    if stale then
      layers.release()
      layers = CanvasLayers(k)
      getChildren.setAll(layers.nodes*)
      drawing = None // the renderer's context wrapped the old canvas
    stale

  /** How often this host compiled, drew the scene and drew the overlay. */
  def profile: PlotHostProfile = PlotHostProfile(compiles, baseDraws, overlayDraws, underDraws)

  /** Removes the scene and blanks the canvas. */
  def clear(): Unit =
    onFxThread("clear")
    if !disposed then
      plotScene = None
      drawing = None
      setAccessibleHelp(null)
      schedule()

  /** Lays out for `scale` instead of the window's output scale; `None` restores it. */
  def setOutputScaleOverride(scale: Option[Double]): Unit =
    onFxThread("setOutputScaleOverride")
    scaleOverride = scale
    updateSurface()

  /** Stops listening, releases the raster and caches, and shows nothing. Idempotent. */
  def dispose(): Unit =
    onFxThread("dispose")
    if !disposed then
      disposed = true
      windowScaleX.removeListener(rescale)
      windowScaleY.removeListener(rescale)
      widthProperty.removeListener(relayout)
      heightProperty.removeListener(relayout)
      Screen.getScreens.removeListener(screensWeak)
      bounds.widthProperty.unbind()
      bounds.heightProperty.unbind()
      plotScene = None
      surface = Right(None)
      drawing = None
      painter = None
      blank()
      layers.release()
      getChildren.clear()
      statusWrapper.set(PlotHostStatus.Disposed)

  private def onFxThread(operation: String): Unit =
    if !Platform.isFxApplicationThread then
      throw IllegalStateException(
        s"CanvasPlotHost.$operation must run on the JavaFX application thread, " +
          s"not ${Thread.currentThread.getName}"
      )

  private def updateSurface(): Unit =
    if !disposed then
      // No area yet (not laid out) is waiting; an area that cannot be a
      // surface is a failure that names its size and scale.
      val next = squareScale.flatMap {
        case Some(scale) if getWidth > 0.0 && getHeight > 0.0 =>
          PlotSurface(getWidth, getHeight, scale)
            .map(Some(_))
            .left
            .map(CanvasPlotError.Surface(_))
        case _ => Right(None)
      }
      if next != surface then
        surface = next
        schedule()

  // Records a change and starts a compile unless one is in flight; `deliver`
  // starts the next one when a result arrives for a superseded request.
  private def schedule(): Unit =
    requested += 1
    (plotScene, surface) match
      case (Some(scene), Right(Some(target))) =>
        statusWrapper.set(PlotHostStatus.Compiling(scene.id, target))
        if !inFlight then launch(scene, target, requested)
      case (Some(_), Left(error)) =>
        blank()
        layers.release()
        statusWrapper.set(PlotHostStatus.Failed(error))
      case (Some(scene), Right(None)) =>
        blank()
        statusWrapper.set(PlotHostStatus.Waiting(scene.id))
      case (None, _) =>
        blank()
        statusWrapper.set(PlotHostStatus.Empty)

  private def launch(scene: PlotScene, target: PlotSurface, token: Long): Unit =
    inFlight = true
    try
      compiler.execute { () =>
        // Deliver something whatever happens, or `inFlight` would never clear
        // and the host would stop compiling; a fatal error is still rethrown.
        var result: Either[CanvasPlotError, PlotFrame] =
          Left(CanvasPlotError.Unexpected(scene.id, target, "compilation did not complete"))
        try result = PlotFrame.compile(scene, target)
        catch
          case NonFatal(e) =>
            result = Left(CanvasPlotError.Unexpected(scene.id, target, e.toString))
        finally
          val delivered = result
          Platform.runLater(() => deliver(token, delivered))
      }
    catch
      case e: RejectedExecutionException =>
        inFlight = false
        blank()
        statusWrapper.set(
          PlotHostStatus.Failed(
            CanvasPlotError.Unexpected(scene.id, target, s"the compiler refused the task: $e")
          )
        )

  private def deliver(token: Long, result: Either[CanvasPlotError, PlotFrame]): Unit =
    inFlight = false
    if !disposed then
      if token == requested then
        result match
          case Right(frame) =>
            compiles += 1
            try
              draw(frame)
              shown = Some(frame)
              repaintOverlay(under = true)
              statusWrapper.set(PlotHostStatus.Drawn(frame))
            catch
              case NonFatal(e) =>
                blank()
                statusWrapper.set(
                  PlotHostStatus.Failed(
                    CanvasPlotError.Unexpected(
                      frame.sceneId,
                      frame.surface,
                      s"drawing threw $e"
                    )
                  )
                )
          case Left(error) =>
            blank()
            statusWrapper.set(PlotHostStatus.Failed(error))
      else
        (plotScene, surface) match
          case (Some(scene), Right(Some(target))) => launch(scene, target, requested)
          case _                                  => ()

  private def draw(frame: PlotFrame): Unit =
    replaceStaleLayers(): Unit
    val k      = layers.pixelScale
    val width  = frame.surface.deviceWidth.toDouble / k
    val height = frame.surface.deviceHeight.toDouble / k
    val back   = k / frame.surface.deviceScale
    layers.all.foreach { layer =>
      val c = layer.canvas
      if c.getWidth != width then c.setWidth(width)
      if c.getHeight != height then c.setHeight(height)
      layer.toLayout.setX(back)
      layer.toLayout.setY(back)
    }
    val gc = canvas.getGraphicsContext2D
    // With no transform or clip, clearing the whole canvas also discards its
    // queued commands, so redraws do not accumulate.
    gc.clearRect(0.0, 0.0, width, height)
    val context = drawing.getOrElse(renderer(gc))
    drawing = Some(context)
    inDevicePixels(gc)(JavaFxRenderer.draw(frame.program, context))
    baseDraws += 1

  // Runs `f` with `gc` drawing in device pixels, whatever the texture scale.
  private def inDevicePixels(gc: GraphicsContext)(f: => Unit): Unit =
    val k = layers.pixelScale
    gc.save()
    try
      gc.scale(1.0 / k, 1.0 / k)
      f
    finally gc.restore()

  private def blank(): Unit =
    shown = None
    layers.all.foreach { layer =>
      val c = layer.canvas
      c.getGraphicsContext2D.clearRect(0.0, 0.0, c.getWidth, c.getHeight)
    }

/** One of a host's canvases and the transform from its units to layout units. */
private final class CanvasLayer:
  val canvas: Canvas  = Canvas(0.0, 0.0)
  val toLayout: Scale = Scale(1.0, 1.0, 0.0, 0.0)
  canvas.setManaged(false)
  canvas.setMouseTransparent(true)
  canvas.getTransforms.add(toLayout)

/** A host's scene, selection and hover canvases, created together for the
  * texture scale `pixelScale` that their peers are expected to get.
  */
private final class CanvasLayers(val pixelScale: Double):
  val scene: CanvasLayer   = CanvasLayer()
  val under: CanvasLayer   = CanvasLayer()
  val overlay: CanvasLayer = CanvasLayer()

  /** Bottom to top. */
  val all: List[CanvasLayer] = List(scene, under, overlay)
  def nodes: List[Canvas]    = all.map(_.canvas)

  /** Zero-size canvases free their textures at their next render. */
  def release(): Unit = nodes.foreach { c =>
    c.setWidth(0.0)
    c.setHeight(0.0)
  }

object CanvasPlotHost:

  /** How many texture pixels JavaFX gives each unit of a canvas created now:
    * the highest recommended output scale of any screen, rounded up, which is
    * how Prism's `NGCanvas` sizes its textures when its peer is created. A host
    * records it when it creates its canvases and recreates them when it
    * changes.
    */
  def canvasPixelScale: Double =
    Screen.getScreens.asScala
      .flatMap(s => List(s.getOutputScaleX, s.getOutputScaleY))
      .foldLeft(1.0)(math.max)
      .ceil

  /** One daemon thread that compiles scenes for every host, in submission order. */
  lazy val sharedCompiler: ExecutorService =
    Executors.newSingleThreadExecutor { runnable =>
      val thread = Thread(runnable, "eyes4s-studio-plot-compile")
      thread.setDaemon(true)
      thread
    }
