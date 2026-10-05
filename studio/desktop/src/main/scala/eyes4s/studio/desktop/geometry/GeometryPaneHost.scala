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

package eyes4s.studio.desktop.geometry

import eyes4s.studio.app.geometry.*
import eyes4s.studio.app.tokens.{StageVariant, Theme}
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.backend.{AdmissionSummary, DatasetRevision, PlacementPreview}
import eyes4s.studio.core.document.{DatasetRevisionSpec, Perspective, StageAppearance}
import eyes4s.studio.desktop.runtime.StudioSession
import eyes4s.studio.viz.geometry.GeometryScene
import eyes4s.studio.viz.plot.PlotScene
import javafx.application.Platform

import java.util.concurrent.{Executor, ExecutorService, Executors, RejectedExecutionException}
import scala.util.control.NonFatal

/** Where the geometry panel reads a revision's fixation source and asks for
  * its admission counts. `done` may be called on any thread.
  */
trait GeometryInputs:
  /** The backend's placement preview of `spec` (S5.5); `done` on any thread. */
  def placement(spec: DatasetRevisionSpec, done: Either[String, PlacementPreview] => Unit): Unit
  def admission(dataset: DatasetRevision, done: Either[String, AdmissionSummary] => Unit): Unit

object GeometryInputs:
  /** The window's backend. */
  def of(session: StudioSession): GeometryInputs =
    new GeometryInputs:
      def placement(
          spec: DatasetRevisionSpec,
          done: Either[String, PlacementPreview] => Unit
      ): Unit =
        session.run(session.backend.placement(spec)) {
          case Left(e)          => done(Left(Option(e.getMessage).getOrElse(e.toString)))
          case Right(Left(err)) => done(Left(err.message))
          case Right(Right(p))  => done(Right(p))
        }
      def admission(
          dataset: DatasetRevision,
          done: Either[String, AdmissionSummary] => Unit
      ): Unit =
        session.run(session.backend.admission(dataset)) {
          case Left(e)          => done(Left(Option(e.getMessage).getOrElse(e.toString)))
          case Right(Left(err)) => done(Left(err.message))
          case Right(Right(s))  => done(Right(s))
        }

/** A redraw of the placement pictures: which key they were drawn for, and
  * how long after the change that asked for them they were all on their
  * canvases.
  */
final case class RedrawReceipt(generation: Long, key: PicturesKey, millis: Double)

/** The Data perspective's geometry pane on the desktop (Data.dc.html,
  * geometry; see [[GeometryPanel]]). It follows the model's selected dataset
  * revision, reads that revision's fixation source through `inputs` and
  * parses it off the JavaFX thread, asks the backend for its counts, and
  * draws the placement pictures, also derived off the JavaFX thread, each
  * time the revision's geometry, corrections, policy or the chosen trial
  * change. The panel's commands go to the app through `app`; a change that
  * creates a revision moves the Data selection to it. Use on the JavaFX
  * thread.
  */
final class GeometryPaneHost(
    model: () => AppModel,
    app: Intent => Unit,
    inputs: GeometryInputs,
    worker: Executor = GeometryPaneHost.sharedWorker
):
  private var panel                              = GeometryPanel.empty
  private var pictures: Option[GeometryPictures] = None
  private var requested: Option[PicturesKey]     = None
  private var generation                         = 0L
  private var changedAt                          = System.nanoTime()
  private var receiptLog                         = Vector.empty[RedrawReceipt]
  private var started                            = false

  val view: GeometryPanelView = GeometryPanelView(dispatch)
  def node: javafx.scene.Node = view.node

  /** The panel's state now. */
  def state: GeometryPanel = panel

  /** Every redraw so far, oldest first. */
  def receipts: Vector[RedrawReceipt] = receiptLog

  /** The pictures on the canvases, if any. */
  def shownPictures: Option[GeometryPictures] = pictures

  /** Follow the model. Nothing is read until the Data perspective has been
    * shown.
    */
  def sync(m: AppModel): Unit =
    started = started || m.perspective == Perspective.Data || panel.shown.isDefined
    if started then
      val (next, effects) = GeometryPanel.sync(panel, m)
      panel = next
      perform(effects)
      refresh()

  /** A user action on the panel. */
  def dispatch(intent: GeometryIntent): Unit =
    val (next, effects) = GeometryPanel.update(panel, model(), intent)
    panel = next
    perform(effects)
    refresh()

  private def perform(effects: Vector[GeometryEffect]): Unit =
    val before  = model().document
    val changes = effects.collect { case GeometryEffect.App(i) => i }
    effects.foreach {
      case GeometryEffect.App(i)                   => app(i)
      case GeometryEffect.RequestPlacement(key, s) => place(key, s)
      case GeometryEffect.RequestCounts(d)         => counts(d)
    }
    if changes.nonEmpty then GeometryPanel.follow(before, model().document).foreach(app)

  /** The backend places the records; nothing is placed here. */
  private def place(key: PlacementKey, spec: DatasetRevisionSpec): Unit =
    inputs.placement(
      spec,
      result => Platform.runLater(() => dispatch(GeometryIntent.PlacementRead(key, result)))
    )

  private def counts(dataset: DatasetRevision): Unit =
    inputs.admission(
      dataset,
      result => Platform.runLater(() => dispatch(GeometryIntent.CountsRead(dataset, result)))
    )

  /** Render the view-model and, when the pictures' key changed, draw them. */
  private def refresh(): Unit =
    val m = model()
    view.render(GeometryPanelVM.of(panel, m, pictures))
    val key = GeometryPictures.keyOf(panel, m)
    if key != requested then
      requested = key
      generation += 1
      changedAt = System.nanoTime()
      key match
        // While a changed revision is placed again, its last pictures stay.
        case None if panel.placement == Loading.Waiting => ()
        case None                                       => view.clearScenes()
        case Some(k)                                    => draw(generation, k, m)

  private def draw(current: Long, key: PicturesKey, m: AppModel): Unit =
    val spec      = m.document.dataset(key.dataset)
    val positions = panel.placement.toOption
    val theme     = m.theme match
      case eyes4s.studio.core.document.Theme.Light => Theme.Light
      case eyes4s.studio.core.document.Theme.Dark  => Theme.Dark
    val stage = m.document.presentation.stage match
      case StageAppearance.Dark  => StageVariant.Dark
      case StageAppearance.Mid   => StageVariant.Mid
      case StageAppearance.Light => StageVariant.Light
    (spec, positions) match
      case (Some(s), Some(ps)) =>
        try
          worker.execute { () =>
            val built = GeometryPaneHost.scenes(current, key, s, ps, theme, stage)
            Platform.runLater(() => deliver(current, built))
          }
        catch case e: RejectedExecutionException => deliver(current, Left(e.toString))
      case _ => ()

  private def deliver(
      current: Long,
      built: Either[String, (GeometryPictures, Vector[PlotScene], PlotScene)]
  ): Unit =
    if current == generation then
      built match
        case Left(reason) =>
          pictures = None
          view.clearScenes()
          panel = panel.copy(problem = Some(reason))
          view.render(GeometryPanelVM.of(panel, model(), pictures))
        case Right((drawn, thumbs, all)) =>
          pictures = Some(drawn)
          view.render(GeometryPanelVM.of(panel, model(), pictures))
          val from = changedAt
          view.showScenes(
            thumbs,
            all,
            () =>
              receiptLog = receiptLog :+ RedrawReceipt(
                current,
                drawn.key,
                (System.nanoTime() - from) / 1e6
              )
          )

  def dispose(): Unit = view.dispose()

object GeometryPaneHost:

  /** One daemon thread that parses sources and derives pictures for every pane. */
  lazy val sharedWorker: ExecutorService =
    Executors.newSingleThreadExecutor { runnable =>
      val thread = Thread(runnable, "eyes4s-studio-geometry")
      thread.setDaemon(true)
      thread
    }

  /** The pictures of `key` and their scenes, off the JavaFX thread. */
  def scenes(
      generation: Long,
      key: PicturesKey,
      spec: DatasetRevisionSpec,
      preview: PlacementPreview,
      theme: Theme,
      stage: StageVariant
  ): Either[String, (GeometryPictures, Vector[PlotScene], PlotScene)] =
    try
      for
        drawn  <- Right(GeometryPictures.of(key, spec, preview))
        thumbs <- drawn.thumbnails.zipWithIndex
          .foldLeft[Either[String, Vector[PlotScene]]](Right(Vector.empty)) {
            case (acc, (t, i)) =>
              acc.flatMap(done =>
                GeometryScene
                  .thumbnail(s"geometry.thumb.$generation.$i", theme, stage, drawn.frame, t)
                  .left
                  .map(_.message)
                  .map(done :+ _)
              )
          }
        all <- GeometryScene
          .density(s"geometry.density.$generation", stage, drawn.frame, drawn.density)
          .left
          .map(_.message)
      yield (drawn, thumbs, all)
    catch case NonFatal(e) => Left(Option(e.getMessage).getOrElse(e.toString))
