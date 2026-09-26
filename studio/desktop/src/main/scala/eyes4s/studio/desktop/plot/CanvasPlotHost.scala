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
import javafx.scene.canvas.{Canvas, GraphicsContext}
import javafx.scene.layout.Region
import javafx.scene.shape.Rectangle
import javafx.scene.transform.Scale
import javafx.stage.Window

import java.util.concurrent.{Executor, ExecutorService, Executors, RejectedExecutionException}
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
  /** Paints on the host's overlay canvas, whose raster is `frame`'s device
    * raster (device pixels, cleared before each call). FX thread only.
    */
  def paint(gc: GraphicsContext, frame: PlotFrame): Unit

/** How often a host compiled a scene, drew its base canvas and its overlay. */
final case class PlotHostProfile(compiles: Long, baseDraws: Long, overlayDraws: Long)
    derives CanEqual

/** An Intaglio scene on a JavaFX canvas (S4.1).
  *
  * The host lays a [[PlotScene]] out for its current size and the output scale
  * of its window, compiles it with Intaglio's pure `JavaFxRenderer.compile` on
  * `compiler`, off the FX thread, and draws the compiled program on a canvas on
  * the FX thread.
  *
  * '''HiDPI.''' The canvas has one pixel per device pixel: its raster is the
  * surface's device size, and a `1/scale` transform brings it back to layout
  * size, so a 2x display gets a 2x raster rather than an upscaled 1x one.
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
  * mouse-transparent unmanaged children whose local coordinates are device
  * pixels; the host's local coordinates are the logical canvas coordinates
  * of [[eyes4s.studio.viz.plot.CanvasPoint]]. Feedback is drawn by a
  * [[PlotOverlay]] on a second canvas above the scene: [[repaintOverlay]]
  * redraws only that canvas, never compiling or redrawing the scene
  * ([[profile]] counts both).
  *
  * '''Disposal.''' [[dispose]] stops listening to the scene and window,
  * releases the canvas raster and the renderer's image and pattern caches, and
  * ignores compiles still in flight. The renderer's caches are also dropped
  * whenever a different scene value is shown, even under the same
  * [[eyes4s.studio.viz.plot.SceneId]], so replaced rasters do not accumulate.
  * A host is used on the FX thread only.
  */
final class CanvasPlotHost private[plot] (
    compiler: Executor,
    renderer: GraphicsContext => JavaFxGraphicsContext
) extends Region:

  /** A host compiling on `compiler`. */
  def this(compiler: Executor) = this(compiler, gc => JavaFxCanvasContext(gc))

  /** A host compiling on the shared studio plot compiler. */
  def this() = this(CanvasPlotHost.sharedCompiler)

  private val canvas          = Canvas(0.0, 0.0)
  private val overlay         = Canvas(0.0, 0.0)
  private val toLayout        = Scale(1.0, 1.0, 0.0, 0.0)
  private val overlayToLayout = Scale(1.0, 1.0, 0.0, 0.0)
  canvas.setManaged(false)
  canvas.setMouseTransparent(true)
  canvas.getTransforms.add(toLayout)
  overlay.setManaged(false)
  overlay.setMouseTransparent(true)
  overlay.getTransforms.add(overlayToLayout)
  getChildren.addAll(canvas, overlay)
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

  // The output scale of the window this host is in. The chain observes the
  // current scene's window property and that window's scale only while the
  // listener below is attached, and follows the host from scene to scene.
  private val window: ObservableValue[Window] = sceneProperty.flatMap[Window](_.windowProperty)
  private val windowScaleX: ObservableValue[Number] =
    window.flatMap[Number](_.outputScaleXProperty)
  private val windowScaleY: ObservableValue[Number] =
    window.flatMap[Number](_.outputScaleYProperty)

  private val relayout: ChangeListener[Any] = (_, _, _) => updateSurface()
  windowScaleX.addListener(relayout)
  windowScaleY.addListener(relayout)
  widthProperty.addListener(relayout)
  heightProperty.addListener(relayout)

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

  /** Shows `scene`, replacing the current one. */
  def show(scene: PlotScene): Unit =
    onFxThread("show")
    if !disposed then
      // A new scene value may carry new rasters: start with empty caches.
      if !plotScene.exists(_ eq scene) then drawing = None
      plotScene = Some(scene)
      schedule()

  /** Draws `next` over the scene from now on; `None` removes the overlay. */
  def setOverlay(next: Option[PlotOverlay]): Unit =
    onFxThread("setOverlay")
    if !disposed then
      painter = next
      repaintOverlay()

  /** Redraws the overlay over the frame on the canvas; the scene is untouched. */
  def repaintOverlay(): Unit =
    onFxThread("repaintOverlay")
    if !disposed then
      val gc = overlay.getGraphicsContext2D
      gc.clearRect(0.0, 0.0, overlay.getWidth, overlay.getHeight)
      for
        frame <- shown
        paint <- painter
      do
        gc.save()
        try paint.paint(gc, frame)
        finally gc.restore()
        overlayDraws += 1

  /** How often this host compiled, drew the scene and drew the overlay. */
  def profile: PlotHostProfile = PlotHostProfile(compiles, baseDraws, overlayDraws)

  /** Removes the scene and blanks the canvas. */
  def clear(): Unit =
    onFxThread("clear")
    if !disposed then
      plotScene = None
      drawing = None
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
      windowScaleX.removeListener(relayout)
      windowScaleY.removeListener(relayout)
      widthProperty.removeListener(relayout)
      heightProperty.removeListener(relayout)
      bounds.widthProperty.unbind()
      bounds.heightProperty.unbind()
      plotScene = None
      surface = Right(None)
      drawing = None
      painter = None
      blank()
      canvas.setWidth(0.0)
      canvas.setHeight(0.0)
      overlay.setWidth(0.0)
      overlay.setHeight(0.0)
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
              repaintOverlay()
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
    val width  = frame.surface.deviceWidth.toDouble
    val height = frame.surface.deviceHeight.toDouble
    if canvas.getWidth != width then canvas.setWidth(width)
    if canvas.getHeight != height then canvas.setHeight(height)
    toLayout.setX(1.0 / frame.surface.deviceScale)
    toLayout.setY(1.0 / frame.surface.deviceScale)
    if overlay.getWidth != width then overlay.setWidth(width)
    if overlay.getHeight != height then overlay.setHeight(height)
    overlayToLayout.setX(1.0 / frame.surface.deviceScale)
    overlayToLayout.setY(1.0 / frame.surface.deviceScale)
    val gc = canvas.getGraphicsContext2D
    // With no transform or clip, clearing the whole canvas also discards its
    // queued commands, so redraws do not accumulate.
    gc.clearRect(0.0, 0.0, width, height)
    val context = drawing.getOrElse(renderer(gc))
    drawing = Some(context)
    JavaFxRenderer.draw(frame.program, context)
    baseDraws += 1

  private def blank(): Unit =
    shown = None
    canvas.getGraphicsContext2D.clearRect(0.0, 0.0, canvas.getWidth, canvas.getHeight)
    overlay.getGraphicsContext2D.clearRect(0.0, 0.0, overlay.getWidth, overlay.getHeight)

object CanvasPlotHost:

  /** One daemon thread that compiles scenes for every host, in submission order. */
  lazy val sharedCompiler: ExecutorService =
    Executors.newSingleThreadExecutor { runnable =>
      val thread = Thread(runnable, "eyes4s-studio-plot-compile")
      thread.setDaemon(true)
      thread
    }
