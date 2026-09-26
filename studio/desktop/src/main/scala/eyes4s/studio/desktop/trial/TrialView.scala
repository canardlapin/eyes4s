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

package eyes4s.studio.desktop.trial

import eyes4s.studio.app.tokens.{StageToken, Tokens}
import eyes4s.studio.core.assets.{AssetLink, AssetRef}
import eyes4s.studio.desktop.plot.CanvasPlotHost
import eyes4s.studio.viz.trial.{StimulusRaster, TrialScene, TrialSceneError, TrialSceneInput}
import javafx.application.Platform
import javafx.beans.property.{ReadOnlyObjectProperty, ReadOnlyObjectWrapper}
import javafx.scene.layout.Region

import java.util.concurrent.{Executor, ExecutorService, Executors, RejectedExecutionException}

/** What a [[TrialView]] shows. */
enum TrialViewStatus derives CanEqual:
  case Empty

  /** `scene` is handed to the canvas host; its image may still be loading. */
  case Shown(scene: TrialScene)

  /** The input was refused; the view shows its stage only. */
  case Refused(error: TrialSceneError)

  case Disposed

/** A trial on its stage (ticket S4.3a): the trial scene of studio-viz on a
  * [[CanvasPlotHost]], with the stimulus read through the asset registry.
  *
  * The display's asset, when it is stored (`AssetLink.Present`), is read from
  * `source` (the project bundle, or the golden fixture's stimuli), checked
  * against its registered digest and decoded on `loader`, off the FX thread.
  * Until then the scene shows the labelled loading state; a failure shows the
  * hatched unreadable state. A missing asset (`AssetLink.Missing`) is never
  * read: the registry already says it has no bytes, and the scene hatches it.
  * Decoded rasters are kept per asset for the life of the view.
  *
  * The host keeps the scene's aspect ([[TrialScene.fit]]), centred; the rest
  * of the view is the stage colour. The view handles no input itself:
  * [[TrialInputAdapter]] adds picking, the roving cursor and the selection
  * overlay (S4.2). It is used on the FX thread only.
  */
final class TrialView(source: StimulusSource, loader: Executor) extends Region:

  /** A view reading `source` on the shared stimulus loader. */
  def this(source: StimulusSource) = this(source, TrialView.sharedLoader)

  private val host = CanvasPlotHost()
  getChildren.add(host)
  getStyleClass.add("trial-view")

  private val statusWrapper =
    ReadOnlyObjectWrapper[TrialViewStatus](this, "status", TrialViewStatus.Empty)

  private var current: Option[TrialSceneInput]       = None
  private var rasters: Map[AssetRef, StimulusRaster] = Map.empty
  private var requested: Set[AssetRef]               = Set.empty
  private var aspect: Option[Double]                 = None
  private var disposed: Boolean                      = false

  /** What the view shows. */
  def status: ReadOnlyObjectProperty[TrialViewStatus] = statusWrapper.getReadOnlyProperty

  /** The canvas host, for the input adapter (S4.2) and tests. */
  def plotHost: CanvasPlotHost = host

  /** The input the view shows, if any (its theme, stage and trial, for S4.2). */
  def input: Option[TrialSceneInput] = current

  /** Shows `input`; its `rasters` are replaced by the ones this view loaded. */
  def show(input: TrialSceneInput): Unit =
    onFxThread("show")
    if !disposed then
      current = Some(input)
      setStyle(
        s"-fx-background-color: ${Tokens.staged(input.stage, StageToken.Stage).javaFxCss};"
      )
      input.display.asset.foreach {
        case AssetLink.Present(asset) => request(asset)
        case AssetLink.Missing(_)     => ()
      }
      render()

  /** Removes the trial. */
  def clear(): Unit =
    onFxThread("clear")
    if !disposed then
      current = None
      aspect = None
      host.clear()
      statusWrapper.set(TrialViewStatus.Empty)

  /** Disposes the canvas host and drops the loaded rasters. Idempotent. */
  def dispose(): Unit =
    onFxThread("dispose")
    if !disposed then
      disposed = true
      current = None
      rasters = Map.empty
      host.dispose()
      getChildren.clear()
      statusWrapper.set(TrialViewStatus.Disposed)

  private def request(asset: AssetRef): Unit =
    if !requested(asset) then
      requested += asset
      try
        loader.execute { () =>
          val raster = Stimuli.load(source, asset)
          Platform.runLater(() => deliver(asset, raster))
        }
      catch
        case e: RejectedExecutionException =>
          deliver(asset, StimulusRaster.Unreadable(s"the loader refused the task: $e"))

  private def deliver(asset: AssetRef, raster: StimulusRaster): Unit =
    if !disposed then
      rasters += asset -> raster
      if current.exists(_.display.asset.contains(AssetLink.Present(asset))) then render()

  private def render(): Unit =
    current.foreach { input =>
      TrialScene(input.copy(rasters = rasters)) match
        case Right(scene) =>
          if !aspect.contains(scene.aspect) then
            aspect = Some(scene.aspect)
            requestLayout()
          host.show(scene.plot)
          statusWrapper.set(TrialViewStatus.Shown(scene))
        case Left(error) =>
          aspect = None
          host.clear()
          statusWrapper.set(TrialViewStatus.Refused(error))
    }

  override protected def layoutChildren(): Unit =
    val w = getWidth
    val h = getHeight
    aspect match
      case Some(a) =>
        val (fw, fh) = TrialScene.fit(w, h, a)
        host.resizeRelocate(
          math.floor((w - fw) / 2.0),
          math.floor((h - fh) / 2.0),
          math.floor(fw),
          math.floor(fh)
        )
      case None => host.resizeRelocate(0.0, 0.0, w, h)

  private def onFxThread(operation: String): Unit =
    if !Platform.isFxApplicationThread then
      throw IllegalStateException(
        s"TrialView.$operation must run on the JavaFX application thread, " +
          s"not ${Thread.currentThread.getName}"
      )

object TrialView:

  /** One daemon thread that reads and decodes stimuli for every view. */
  lazy val sharedLoader: ExecutorService =
    Executors.newSingleThreadExecutor { runnable =>
      val thread = Thread(runnable, "eyes4s-studio-stimulus-loader")
      thread.setDaemon(true)
      thread
    }
