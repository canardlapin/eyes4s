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

package eyes4s.studio.desktop.shell

import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.app.appearance.Appearance
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.desktop.harness.{StageSize, StudioTheme, VisualPixels}
import eyes4s.studio.desktop.plot.{CanvasPlotError, CanvasPlotHost, PlotHostStatus}
import eyes4s.studio.desktop.trial.TrialViewStatus
import eyes4s.studio.viz.plot.PlotSceneError
import intaglio.GraphicsError
import javafx.geometry.Bounds
import javafx.scene.canvas.Canvas
import java.nio.file.{Files, Path}

/** Restoring a trial viewport preserves its pixels, including sampled canvas edges. */
class CanvasBoundaryFxSuite extends StoryCaptureFixture:
  override val munitTimeout: scala.concurrent.duration.Duration =
    scala.concurrent.duration.Duration(180, "s")
  private def box(b: Bounds): ujson.Value = ujson.Arr(
    b.getMinX,
    b.getMinY,
    b.getWidth,
    b.getHeight
  )

  private def describe(name: String, h: CanvasPlotHost): ujson.Value =
    val status = h.status.get match
      case PlotHostStatus.Empty           => "Empty"
      case PlotHostStatus.Waiting(_)      => "Waiting"
      case PlotHostStatus.Disposed        => "Disposed"
      case PlotHostStatus.Compiling(_, _) => "Compiling"
      case PlotHostStatus.Drawn(_)        => "Drawn"
      case PlotHostStatus.Failed(e)       => "Failed: " + e.message
    import scala.jdk.CollectionConverters.*
    ujson.Obj(
      "name"         -> name,
      "status"       -> status,
      "sceneBounds"  -> box(h.localToScene(h.getBoundsInLocal)),
      "layoutBounds" -> box(h.getLayoutBounds),
      "clip" -> Option(h.getClip).fold[ujson.Value](ujson.Null)(n => box(n.getBoundsInLocal)),
      "surface"   -> h.frame.fold("")(_.surface.toString),
      "transform" -> h.frame.fold("")(_.transform.toString),
      "profile"   -> h.profile.toString,
      "canvases" -> ujson.Arr.from(h.getChildrenUnmodifiable.asScala.collect { case c: Canvas =>
        ujson.Obj(
          "width"       -> c.getWidth,
          "height"      -> c.getHeight,
          "sceneBounds" -> box(c.localToScene(c.getBoundsInLocal)),
          "transforms"  -> c.getTransforms.toString
        )
      })
    )

  for theme <- StudioTheme.values do
    fxStage.test("trial pixels survive restored size and scale " + theme.id) { fx =>
      val model = AppModel
        .update(
          StoryModels.t2Compare,
          Intent.SetAppearance(if theme == StudioTheme.Dark then Appearance.Dark
          else Appearance.Light)
        )
        ._1
      val w = boot(
        fx,
        model,
        StoryMoment.T2,
        panels = golden,
        project = Some(fixtureProject),
        clock = () => None
      )
      val trials = Vector(w.summary.panels.queryView, w.summary.panels.referenceView)
      eventually(fx, "both stored maps") {
        trials.forall(_.status.get match
          case TrialViewStatus.Shown(s) => s.map.nonEmpty
          case _                        => false)
      }
      val hosts = Vector(
        "query"            -> trials(0).plotHost,
        "query-legend"     -> trials(0).legendPlotHost,
        "reference"        -> trials(1).plotHost,
        "reference-legend" -> trials(1).legendPlotHost
      )
      def drawn(): Unit = eventually(fx, "all four actual canvas frames drawn") {
        hosts.forall(_._2.status.get.isInstanceOf[PlotHostStatus.Drawn])
      }
      RestingFocus.clear(fx, w)
      drawn()
      var phases                           = Vector.empty[ujson.Value]
      var directory                        = Option.empty[Path]
      def record(value: ujson.Value): Unit =
        phases :+= value
        directory.foreach(p =>
          Files.writeString(
            p.resolve("canvas-boundary.json"),
            ujson.write(
              ujson.Obj(
                "theme"  -> theme.id,
                "os"     -> System.getProperty("os.name"),
                "java"   -> System.getProperty("java.version"),
                "scene"  -> ujson.Arr(fx.sceneSize._1, fx.sceneSize._2),
                "phases" -> ujson.Arr.from(phases)
              ),
              indent = 2
            ) + "\n"
          ): Unit
        )
      def resized(label: String): Unit =
        try
          // Deliberately undersized intermediate surfaces may refuse the caption's
          // panel extent. Only that typed size refusal (or zero-area Waiting) is
          // allowed here; the baseline and every restored surface must be Drawn.
          eventually(fx, "resized frames settle with an explicit surface-size outcome") {
            hosts.forall { (_, host) =>
              host.status.get match
                case PlotHostStatus.Drawn(_)   => true
                case PlotHostStatus.Waiting(_) => host.getWidth <= 0.0 || host.getHeight <= 0.0
                case PlotHostStatus.Failed(
                      CanvasPlotError.Scene(
                        PlotSceneError.Graphics(
                          _,
                          "the data panel's frame",
                          GraphicsError.InvalidExtent(_)
                        )
                      )
                    ) =>
                  true
                case PlotHostStatus.Failed(
                      CanvasPlotError.Scene(PlotSceneError.EmptyPanel(_, width, height))
                    ) =>
                  width <= 0.0 || height <= 0.0
                case _ => false
            }
          }
        finally
          record(
            ujson.Obj(
              "phase" -> label,
              "hosts" -> runOnFx(
                ujson.Arr.from(hosts.map((name, host) => describe(name, host)))
              )
            )
          )
      def capture(label: String): List[java.awt.image.BufferedImage] =
        val before = runOnFx(ujson.Arr.from(hosts.map((name, host) => describe(name, host))))
        val files  = fx.snapshot(theme)
        directory = Some(files.head.getParent)
        val images = files.map(VisualPixels.read)
        files.foreach(p =>
          Files.copy(
            p,
            p.resolveSibling(label + "-" + p.getFileName.toString),
            java.nio.file.StandardCopyOption.REPLACE_EXISTING
          )
        )
        val after = runOnFx(ujson.Arr.from(hosts.map((name, host) => describe(name, host))))
        record(ujson.Obj("phase" -> label, "before" -> before, "after" -> after))
        images
      val viewport = runOnFx {
        val bounds = trials.map(t => t.localToScene(t.getLayoutBounds))
        val left   = math.floor(bounds.map(_.getMinX).min).toInt
        val top    = math.floor(bounds.map(_.getMinY).min).toInt
        (
          left,
          top,
          math.ceil(bounds.map(_.getMaxX).max).toInt - left,
          math.ceil(bounds.map(_.getMaxY).max).toInt - top
        )
      }
      def trialPixels(image: java.awt.image.BufferedImage, scale: Int) =
        image.getSubimage(
          viewport._1 * scale,
          viewport._2 * scale,
          viewport._3 * scale,
          viewport._4 * scale
        )
      val baseline                     = capture("baseline")
      val differences                  = Vector.newBuilder[String]
      def compare(label: String): Unit =
        val images = capture(label)
        images.zip(baseline).zipWithIndex.foreach { case ((a, b), i) =>
          val diff = VisualPixels
            .compare(trialPixels(b, i + 1), trialPixels(a, i + 1))
            .fold(e => fail(e), identity)
          record(
            ujson.Obj(
              "phase"         -> label,
              "snapshotScale" -> (i + 1),
              "difference"    -> diff.json
            )
          )
          if !diff.accepted then differences += s"$label at${i + 1}x: $diff"
        }
      val originalLimits = runOnFx(trials.map(t => (t.getMaxWidth, t.getMaxHeight)))
      val originalSizes  = runOnFx(trials.map(t => (t.getWidth, t.getHeight)))
      // Resize the component within a fixed native window. This exercises texture
      // reuse without coupling the invariant to the platform's window compositor.
      for reduction <- Vector(1.0, 17.0, 33.0) do
        runOnFx(
          trials.zip(originalSizes).foreach { (t, size) => t.setMaxHeight(size._2 - reduction) }
        )
        fx.awaitLayout(); resized("shrunk-height-" + reduction.toString)
        runOnFx(trials.zip(originalLimits).foreach { (t, limit) => t.setMaxHeight(limit._2) })
        fx.awaitLayout(); drawn()
        compare("height-" + reduction.toString)
      for reduction <- Vector(1.0, 17.0, 33.0) do
        runOnFx(
          trials.zip(originalSizes).foreach { (t, size) => t.setMaxWidth(size._1 - reduction) }
        )
        fx.awaitLayout(); resized("shrunk-width-" + reduction.toString)
        runOnFx(trials.zip(originalLimits).foreach { (t, limit) => t.setMaxWidth(limit._1) })
        fx.awaitLayout(); drawn()
        compare("width-" + reduction.toString)
      // A small native screen may collapse the canvas while captions stay visible.
      // Exercise that legitimate Waiting state explicitly, then demand the original pixels.
      runOnFx(trials.foreach(_.setMaxHeight(1.0)))
      fx.awaitLayout(); resized("collapsed-height")
      assert(runOnFx(hosts.exists { (_, host) =>
        host.status.get.isInstanceOf[PlotHostStatus.Waiting] &&
        (host.getWidth <= 0.0 || host.getHeight <= 0.0)
      }))
      runOnFx(trials.zip(originalLimits).foreach { (t, limit) => t.setMaxHeight(limit._2) })
      fx.awaitLayout(); drawn()
      compare("restored-collapsed-height")
      runOnFx(hosts.foreach(_._2.setOutputScaleOverride(Some(2.0))))
      fx.awaitLayout(); drawn()
      runOnFx(hosts.foreach(_._2.setOutputScaleOverride(None)))
      fx.awaitLayout(); drawn()
      compare("restored-native-scale")
      val errors = differences.result()
      assert(errors.isEmpty, errors.mkString("\n"))
    }

/** The same raster invariant at the native Mac runner's constrained scene size. */
class SmallCanvasBoundaryFxSuite extends CanvasBoundaryFxSuite:
  override protected def stageSize: StageSize = StageSize(1024, 656)
