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

import eyes4s.studio.app.maps.{LimitsScope, MapGrid, MapOpacity, MapStyle, RasterKey}
import eyes4s.studio.app.tokens.{StageToken, Tokens}
import eyes4s.studio.core.assets.{AssetLink, AssetRef}
import eyes4s.studio.desktop.maps.{MapRasterStore, RasterRefusal}
import eyes4s.studio.desktop.plot.CanvasPlotHost
import eyes4s.studio.viz.maps.MapLegendScene
import eyes4s.studio.viz.plot.PlotSceneError
import eyes4s.studio.viz.trial.{
  MapCoverage,
  RememberedImage,
  StimulusRaster,
  TrialMap,
  TrialScene,
  TrialSceneError,
  TrialSceneInput
}
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

/** Where a [[TrialView]] shows its map's legend (review ticket 17). */
enum MapLegendPlacement derives CanEqual:

  /** On its own canvas under the trial's caption, as the Main board does. */
  case UnderTrial

  /** Not on the stage: the host shows the legend's keys in its own legend
    * row, as the Explore board does ([[MapLegendScene.keys]]).
    */
  case InHostLegend

/** A map to draw over a trial (ticket S4.3b): the run's grid, the style it
  * is coloured in, the global opacity, the screen region it covers (a
  * run's map covers the image frame; a backend preview its window, S6.2),
  * and whether the panels shown with it share its limits (its legend says so).
  */
final case class MapRequest(
    grid: MapGrid,
    style: MapStyle,
    opacity: MapOpacity = MapOpacity.Default,
    covers: MapCoverage = MapCoverage.ImageFrame,
    limits: LimitsScope = LimitsScope.PerPanel
) derives CanEqual:
  def key: RasterKey = RasterKey(grid.map, style)

/** A trial on its stage (ticket S4.3a): the trial scene of studio-viz on a
  * [[CanvasPlotHost]], with the stimulus read through the asset registry.
  *
  * The display's asset, when it is stored (`AssetLink.Present`), is read from
  * `source` (the project bundle, or the golden fixture's stimuli), checked
  * against its registered digest and decoded on `loader`, off the FX thread.
  * Until then the scene shows the labelled loading state; a failure shows the
  * hatched unreadable state. A missing asset (`AssetLink.Missing`) is never
  * read: the registry already says it has no bytes, and the scene hatches it.
  * Decoded rasters are kept per asset, at most [[TrialView.RasterLimit]] of
  * them: the least recently shown one that the trial does not draw goes first.
  * A failed read keeps its typed [[StimulusError]] ([[stimulusFailures]]),
  * and [[retry]] reads the failed assets again.
  *
  * The data viewport keeps the native screen-pixel aspect inside the canvas.
  * The caption and legend retain the whole pane width; the unused data area
  * is the stage colour. The view handles no input itself:
  * [[TrialInputAdapter]] adds picking, the roving cursor and the selection
  * overlay (S4.2). It is used on the FX thread only.
  *
  * A result map ([[showMap]], S4.3b) is drawn from its raster in `maps`,
  * the map raster cache, rendered off the FX thread; until the raster
  * arrives the trial shows without it. A remembered image shown as an
  * underlay is read like the display's own image.
  *
  * Under a drawn mass map the view shows the map's legend
  * ([[MapLegendScene]], review ticket 17) where the Main board does: on a
  * second canvas directly below the trial's caption, as wide as the trial,
  * [[MapLegendScene.HeightPx]] tall. A difference map and no map have none.
  * The legend's description ends the trial's own (its scene summary). A
  * host with a legend row of its own places it there instead
  * ([[placeLegend]]); the view then keeps no room for it.
  */
final class TrialView(source: StimulusSource, loader: Executor, maps: MapRasterStore)
    extends Region:

  /** A view reading `source` on the shared stimulus loader and map store. */
  def this(source: StimulusSource) = this(source, TrialView.sharedLoader, TrialView.sharedMaps)

  /** A view reading `source` on `loader`, with the shared map store. */
  def this(source: StimulusSource, loader: Executor) =
    this(source, loader, TrialView.sharedMaps)

  private val host       = CanvasPlotHost()
  private val legendHost = CanvasPlotHost()
  legendHost.getStyleClass.add("map-legend")
  getChildren.addAll(host, legendHost)
  getStyleClass.add("trial-view")

  private val statusWrapper =
    ReadOnlyObjectWrapper[TrialViewStatus](this, "status", TrialViewStatus.Empty)

  private var current: Option[TrialSceneInput]       = None
  private var rasters: Map[AssetRef, StimulusRaster] = Map.empty
  private var recency: Vector[AssetRef]              = Vector.empty
  private val failuresWrapper                        =
    ReadOnlyObjectWrapper[Map[AssetRef, StimulusError]](this, "stimulusFailures", Map.empty)
  private var requested: Set[AssetRef]       = Set.empty
  private var aspect: Option[Double]         = None
  private var disposed: Boolean              = false
  private var mapRequest: Option[MapRequest] = None
  private var mapLayer: Option[TrialMap]     = None
  private val refusalWrapper                 =
    ReadOnlyObjectWrapper[Option[RasterRefusal]](this, "mapRefused", None)
  private var legendShown: Option[Either[PlotSceneError, MapLegendScene]] = None
  private var placement: MapLegendPlacement                               =
    MapLegendPlacement.UnderTrial
  legendHost.setVisible(false)

  /** What the view shows. */
  def status: ReadOnlyObjectProperty[TrialViewStatus] = statusWrapper.getReadOnlyProperty

  /** Why each asset that could not be read was not, typed; observable. */
  def stimulusFailures: ReadOnlyObjectProperty[Map[AssetRef, StimulusError]] =
    failuresWrapper.getReadOnlyProperty

  /** The decoded rasters held, least recently shown first. */
  def heldRasters: Vector[AssetRef] = recency

  /** Reads every failed asset again; the trial shows them loading meanwhile. */
  def retry(): Unit =
    onFxThread("retry")
    if !disposed then
      val failed = failuresWrapper.get.keySet
      failuresWrapper.set(Map.empty)
      rasters --= failed
      recency = recency.filterNot(failed)
      requested --= failed
      current.foreach(i => needed(i).foreach(request))
      val failedMap = Option.when(refusalWrapper.get.nonEmpty)(mapRequest).flatten
      failedMap.foreach { m =>
        showMap(None)
        showMap(Some(m))
      }
      render()

  /** The canvas host, for the input adapter (S4.2) and tests. */
  def plotHost: CanvasPlotHost = host

  /** The map legend's canvas host, under the trial's; for tests. */
  def legendPlotHost: CanvasPlotHost = legendHost

  /** The legend of the map drawn, if the view draws one. */
  def legend: Option[MapLegendScene] = legendShown.flatMap(_.toOption)

  /** Where the view shows its map's legend. */
  def legendPlacement: MapLegendPlacement = placement

  /** Shows the map's legend at `where` from now on. */
  def placeLegend(where: MapLegendPlacement): Unit =
    onFxThread("placeLegend")
    if !disposed && where != placement then
      placement = where
      showLegend(legendShown.map(_.map(Some(_))))

  /** Whether the legend is drawn under the trial. */
  def legendUnderTrial: Boolean = legend.isDefined && placement == MapLegendPlacement.UnderTrial

  /** Why the drawn map's legend could not be built, if it could not. */
  def legendRefused: Option[PlotSceneError] = legendShown.flatMap(_.left.toOption)

  /** The input the view shows, if any (its theme, stage and trial, for S4.2). */
  def input: Option[TrialSceneInput] = current

  /** The exact requested grid and display style; independent of whether
    * its asynchronous raster has arrived yet.
    */
  def requestedMap: Option[MapRequest] = mapRequest

  /** Shows `input`; its `rasters` are replaced by the ones this view loaded. */
  def show(input: TrialSceneInput): Unit =
    onFxThread("show")
    if !disposed then
      // A map belongs to its trial: another trial starts without one, and a
      // raster still on its way for the old trial is not installed.
      if !current.exists(_.display.trial == input.display.trial) then
        mapRequest = None
        mapLayer = None
        refusalWrapper.set(None)
      current = Some(input)
      setStyle(
        s"-fx-background-color: ${Tokens.staged(input.stage, StageToken.Stage).javaFxCss};"
      )
      needed(input).foreach(request)
      render()

  /** Draws `map` over the trial, or no map. The raster comes from the map
    * store; the trial is drawn again when it arrives.
    */
  def showMap(map: Option[MapRequest]): Unit =
    onFxThread("showMap")
    if !disposed && map != mapRequest then
      mapRequest = map
      mapLayer = None
      refusalWrapper.set(None)
      map match
        case None    => render()
        case Some(m) =>
          maps.request(m.grid, m.style)(answer => mapArrived(m, answer)) match
            case Left(refusal) => refusalWrapper.set(Some(refusal))
            case Right(())     => ()
          render()

  /** Why the map asked for could not be drawn, if it could not. */
  def mapRefused: Option[RasterRefusal] = refusalWrapper.get

  /** Why the map asked for could not be drawn, observable: set when the
    * store refuses the request at once or when its delivery is a refusal,
    * cleared by a new map, another trial and dispose.
    */
  def mapRefusedProperty: ReadOnlyObjectProperty[Option[RasterRefusal]] =
    refusalWrapper.getReadOnlyProperty

  private def mapArrived(
      m: MapRequest,
      answer: Either[RasterRefusal, eyes4s.studio.app.maps.MapRaster]
  ): Unit =
    if !disposed && mapRequest.contains(m) &&
      current.exists(_.display.trial == m.grid.map.trial)
    then
      answer match
        case Right(raster) =>
          mapLayer = Some(TrialMap(m.grid, raster, m.opacity, m.covers, m.limits))
          render()
        case Left(refusal) => refusalWrapper.set(Some(refusal))

  /** Removes the trial. */
  def clear(): Unit =
    onFxThread("clear")
    if !disposed then
      current = None
      aspect = None
      host.clear()
      showLegend(None)
      statusWrapper.set(TrialViewStatus.Empty)

  /** Disposes the canvas host and drops the loaded rasters. Idempotent. */
  def dispose(): Unit =
    onFxThread("dispose")
    if !disposed then
      disposed = true
      current = None
      rasters = Map.empty
      recency = Vector.empty
      failuresWrapper.set(Map.empty)
      mapRequest = None
      mapLayer = None
      refusalWrapper.set(None)
      legendShown = None
      host.dispose()
      legendHost.dispose()
      getChildren.clear()
      statusWrapper.set(TrialViewStatus.Disposed)

  // The stored assets `input` draws: its display's and a shown underlay's.
  private def needed(input: TrialSceneInput): Vector[AssetRef] =
    input.display.asset.collect { case AssetLink.Present(a) => a }.toVector ++
      (input.remembered match
        case RememberedImage.Shown(a) => Vector(a)
        case _                        => Vector.empty)

  private def request(asset: AssetRef): Unit =
    if !requested(asset) then
      requested += asset
      try
        loader.execute { () =>
          val read = Stimuli.read(source, asset)
          Platform.runLater(() => deliver(asset, read))
        }
      catch
        case e: RejectedExecutionException =>
          deliver(asset, Left(StimulusError.NotLoaded(asset.file, e.toString)))

  private def deliver(
      asset: AssetRef,
      read: Either[StimulusError, intaglio.RasterImage]
  ): Unit =
    if !disposed && requested(asset) then
      read.left.foreach(e => failuresWrapper.set(failuresWrapper.get.updated(asset, e)))
      rasters += asset -> Stimuli.raster(read)
      touch(asset)
      if current.exists(needed(_).contains(asset)) then render()

  // Marks `asset` most recently shown and lets go of the oldest rasters over
  // the limit that the trial does not draw; a let-go asset is read again
  // when it is next needed.
  private def touch(asset: AssetRef): Unit =
    recency = recency.filterNot(_ == asset) :+ asset
    val keep  = current.fold(Set.empty[AssetRef])(needed(_).toSet)
    val over  = recency.size - TrialView.RasterLimit
    val drops = recency.filterNot(keep).take(math.max(0, over))
    rasters --= drops
    requested --= drops
    recency = recency.filterNot(drops.contains)

  private def render(): Unit =
    current.foreach { input =>
      needed(input).filter(rasters.contains).foreach(touch)
      val ready   = input.copy(rasters = rasters, map = mapLayer)
      val wrapped = TrialCaptions.lines(ready, getWidth)
      TrialScene(ready.copy(options = ready.options.copy(captionLines = wrapped))) match
        case Right(scene) =>
          if !aspect.contains(scene.aspect) then
            aspect = Some(scene.aspect)
            requestLayout()
          host.show(scene.plot)
          showLegend(mapLayer.map { m =>
            MapLegendScene(
              m.raster.key.style.palette,
              m.limits,
              m.opacity,
              input.theme,
              input.stage
            )
          })
          statusWrapper.set(TrialViewStatus.Shown(scene))
        case Left(error) =>
          aspect = None
          host.clear()
          showLegend(None)
          statusWrapper.set(TrialViewStatus.Refused(error))
    }

  // Shows the legend `built`, or none: a difference map's is none, and a
  // refused one is kept as its error and not drawn.
  private def showLegend(built: Option[Either[PlotSceneError, Option[MapLegendScene]]]): Unit =
    val next = built.flatMap {
      case Right(legend) => legend.map(Right(_))
      case Left(error)   => Some(Left(error))
    }
    val was = legendUnderTrial
    legendShown = next
    legend.filter(_ => legendUnderTrial) match
      case Some(l) => legendHost.show(l.plot)
      case None    => legendHost.clear()
    legendHost.setVisible(legendUnderTrial)
    if was != legendUnderTrial then requestLayout()

  private var captionSize = Option.empty[(Double, Double)]

  override protected def layoutChildren(): Unit =
    val w = getWidth
    val h = getHeight
    aspect match
      case Some(_) =>
        // Intaglio fits the data frame, including its picking transform. The
        // caption and legend need the pane's width, not the narrower image's.
        val below  = if legendUnderTrial then MapLegendScene.HeightPx else 0.0
        val width  = math.floor(w)
        val height = math.floor(math.max(0.0, h - below))
        host.resizeRelocate(0.0, 0.0, width, height)
        legendHost.resizeRelocate(0.0, height, width, below)
      case None =>
        host.resizeRelocate(0.0, 0.0, w, h)
        legendHost.resizeRelocate(0.0, h, w, 0.0)
    if !captionSize.contains((w, h)) then
      captionSize = Some((w, h))
      render()

  private def onFxThread(operation: String): Unit =
    if !Platform.isFxApplicationThread then
      throw IllegalStateException(
        s"TrialView.$operation must run on the JavaFX application thread, " +
          s"not ${Thread.currentThread.getName}"
      )

object TrialView:

  /** The most decoded stimulus rasters a view holds at once. */
  val RasterLimit: Int = 6

  /** The map raster store every view draws maps from. */
  lazy val sharedMaps: MapRasterStore = MapRasterStore()

  /** One daemon thread that reads and decodes stimuli for every view. */
  lazy val sharedLoader: ExecutorService =
    Executors.newSingleThreadExecutor { runnable =>
      val thread = Thread(runnable, "eyes4s-studio-stimulus-loader")
      thread.setDaemon(true)
      thread
    }
