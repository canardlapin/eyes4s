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

import eyes4s.studio.app.maps.*
import eyes4s.studio.app.text.{TrialText, TrialTextId}
import eyes4s.studio.app.tokens.{PaletteToken, StageVariant, Theme, Tokens}
import eyes4s.studio.core.assets.{AssetLink, AssetRef, Display, TrialDisplay}
import eyes4s.studio.core.backend.RunId
import eyes4s.studio.core.selection.ScaleIndex
import eyes4s.studio.desktop.harness.{
  FxStage,
  SnapshotScale,
  StageSize,
  StudioFxSuite,
  StudioTheme
}
import eyes4s.studio.desktop.maps.MapRasterStore
import eyes4s.studio.desktop.plot.{PlotFrame, PlotHostStatus}
import eyes4s.studio.desktop.typography.StudioFonts
import eyes4s.studio.viz.plot.DataPoint
import eyes4s.studio.viz.trial.*
import intaglio.{Grob, value}
import javafx.geometry.Point2D
import javafx.scene.layout.StackPane

import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import scala.concurrent.duration.Duration

/** The trial view's result map on real JavaFX (ticket S4.3b): the run's
  * grid drawn from the map raster cache at the global opacity, unchanged in
  * value; its backend isoline levels cased and legible on the stage; and the
  * remembered image as an underlay, disclosed whenever it is on.
  */
class TrialViewMapFxSuite extends StudioFxSuite:

  override protected def stageSize: StageSize = StageSize(720, 440)

  override val munitTimeout: Duration = Duration(180, "s")

  override def beforeAll(): Unit =
    super.beforeAll()
    val problems = runOnFx(StudioFonts.loadAll())
    assert(problems.isEmpty, problems.map(_.message).mkString("\n"))

  private val TimeoutMillis = 60000L

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private val golden: StimulusSource = StimulusSource.directory(GoldenTrials.stimuli)

  private lazy val ret07: TrialDisplay = GoldenTrials.display("P17", "ret_07")
  private lazy val blank: TrialDisplay = ret07.copy(display = Display.Blank)
  private lazy val beach: AssetRef     = GoldenTrials.display("P17", "enc_03").asset match
    case Some(AssetLink.Present(a)) => a
    case other                      => fail(s"enc_03 should name a stored image, not $other")

  private val Columns = 64
  private val Rows    = 48

  /** Test values, not results: run 7's map of P17 ret_07 at scale 2, mass
    * rising to the right and downward (so a flip would show), stored bottom
    * row first.
    */
  private lazy val sloped: MapGrid =
    val topFirst = Vector.tabulate(Columns * Rows)(i =>
      Some(0.6 * (i % Columns) / 63.0 + 0.4 * (i / Columns) / 47.0)
    )
    right(
      MapGrid.of(
        MapId(RunId(7), ret07.trial, right(ScaleIndex.of(2))),
        Columns,
        Rows,
        RowOrder.BottomFirst,
        topFirst.grouped(Columns).toVector.reverse.flatten,
        Vector(0.5)
      )
    )

  /** Test values, not results: run 7's map of P17 ret_07 at scale 2, mass
    * rising to the right across columns, contoured at 0.5.
    */
  private lazy val grid: MapGrid =
    right(
      MapGrid.of(
        MapId(RunId(7), ret07.trial, right(ScaleIndex.of(2))),
        Columns,
        Rows,
        RowOrder.TopFirst,
        Vector.tabulate(Columns * Rows)(i => Some((i % Columns) / 63.0)),
        Vector(0.5)
      )
    )

  private lazy val style: MapStyle       = ColourLimits.spanning(MapPalette.Mass, Vector(grid))
  private lazy val slopedStyle: MapStyle =
    ColourLimits.spanning(MapPalette.Mass, Vector(sloped))

  private def input(d: TrialDisplay, remembered: RememberedImage = RememberedImage.Absent) =
    TrialSceneInput(
      d,
      GoldenTrials.screen,
      Vector.empty,
      MarkStyle.Role(TrialRole.Query),
      Theme.Light,
      StageVariant.Dark,
      remembered = remembered
    )

  private final class Wired(fx: FxStage, made: => MapRasterStore = MapRasterStore()):
    val store = made
    val view  = runOnFx(TrialView(golden, TrialView.sharedLoader, store))
    fx.show(runOnFx(StackPane(view)))
    def dispose(): Unit =
      runOnFx(view.dispose())
      store.close()

  /** Waits until the view shows a drawn scene that `ready` accepts. */
  private def drawn(w: Wired, k: Double)(
      ready: TrialScene => Boolean
  ): (TrialScene, PlotFrame) =
    val deadline = System.currentTimeMillis + TimeoutMillis
    def settled  = runOnFx {
      w.view.getScene.getRoot.applyCss()
      w.view.getScene.getRoot.layout()
      val host = w.view.plotHost
      (w.view.status.get, host.status.get) match
        case (TrialViewStatus.Shown(scene), PlotHostStatus.Drawn(frame))
            if ready(scene) && (frame.plan.scene eq scene.plot.scene) &&
              frame.surface.deviceScale == k && frame.surface.logicalWidth == host.getWidth &&
              frame.surface.logicalHeight == host.getHeight =>
          Some((scene, frame))
        case _ => None
    }
    var result = settled
    while result.isEmpty do
      if System.currentTimeMillis > deadline then
        fail(s"not drawn within $TimeoutMillis ms: ${runOnFx(w.view.status.get)}")
      Thread.sleep(5)
      result = settled
    result.get

  private def snapshot(fx: FxStage, k: SnapshotScale): BufferedImage =
    ImageIO.read(fx.snapshot(StudioTheme.Light, List(k)).head.toFile)

  // The snapshot pixel under screen position (x, y) of the trial.
  private def pixelAt(w: Wired, frame: PlotFrame, x: Double, y: Double, k: Int): (Int, Int) =
    val c = right(frame.transform.dataToCanvas(DataPoint(x, y)))
    val s = runOnFx(w.view.plotHost.localToScene(Point2D(c.x, c.y)))
    (math.floor(s.getX * k).toInt, math.floor(s.getY * k).toInt)

  private def rgb(image: BufferedImage, px: (Int, Int)): (Int, Int, Int) =
    val argb = image.getRGB(px._1, px._2)
    ((argb >> 16) & 0xff, (argb >> 8) & 0xff, argb & 0xff)

  private def luminance(c: (Int, Int, Int)): Double =
    def lin(v: Int) =
      val s = v / 255.0
      if s <= 0.03928 then s / 12.92 else math.pow((s + 0.055) / 1.055, 2.4)
    0.2126 * lin(c._1) + 0.7152 * lin(c._2) + 0.0722 * lin(c._3)

  private def contrast(a: (Int, Int, Int), b: (Int, Int, Int)): Double =
    val (la, lb) = (luminance(a), luminance(b))
    (math.max(la, lb) + 0.05) / (math.min(la, lb) + 0.05)

  // The image frame's geometry on the screen.
  private lazy val (frameLeft, frameTop) =
    (ret07.placement.left.toDouble, ret07.placement.top.toDouble)
  private lazy val cell = ret07.placement.width.toDouble / Columns

  fxStage.test(
    "the map is the run's grid from the raster cache, drawn at 0.6, values unchanged"
  ) { fx =>
    val w    = Wired(fx)
    val hash = sloped.contentHash
    runOnFx {
      w.view.plotHost.setOutputScaleOverride(Some(1.0))
      w.view.show(input(blank))
      w.view.showMap(Some(MapRequest(sloped, slopedStyle)))
    }
    val (scene, frame) = drawn(w, 1.0)(_.map.contains(sloped.map))
    // The cache holds the raster of exactly this grid, rendered off the FX thread.
    val key    = RasterKey(sloped.map, slopedStyle)
    val cached = w.store.snapshot
    assert(cached.contains(key))
    val (held, _) = cached.get(key)
    assertEquals(
      held.map(_.argb.toVector),
      Some(MapRaster.render(sloped, slopedStyle).argb.toVector)
    )
    // The scene's map is that raster at the global opacity, cell for cell.
    val image = scene.plot.scene.grobs
      .flatMap(g => g +: g.children)
      .collectFirst {
        case i: Grob.Image if i.name.exists(_.value == TrialScene.MapName) => i.image
      }
      .getOrElse(fail("no map image"))
    val expected = held.get.drawn(MapOpacity.Default)
    assertEquals(image.width * image.height, expected.length)
    // The grid's values are as the backend served them.
    assertEquals(sloped.contentHash, hash)
    // On screen: a cell far from the isoline is its ramp colour at 0.6 over
    // the blank screen.
    val img    = snapshot(fx, SnapshotScale.X1)
    val px     = pixelAt(w, frame, frameLeft + 5.5 * cell, frameTop + 5.5 * cell, 1)
    val argb   = MapColours.argb(slopedStyle, sloped.atTop(5, 5))
    val screen = Tokens.palette(PaletteToken.Screen)
    def mix(c: Int, s: Int) = math.round(c * 0.6 + s * 0.4).toInt
    val want                = (
      mix((argb >> 16) & 0xff, screen.red),
      mix((argb >> 8) & 0xff, screen.green),
      mix(argb & 0xff, screen.blue)
    )
    val got = rgb(img, px)
    assert(
      (0 to 2).forall(i =>
        math.abs(
          got.productElement(i).asInstanceOf[Int] - want.productElement(i).asInstanceOf[Int]
        ) <= 10
      ),
      s"map cell $got, expected about $want"
    )
    // No map, no raster drawn.
    runOnFx(w.view.showMap(None))
    drawn(w, 1.0)(_.map.isEmpty)
    w.dispose()
  }

  fxStage.test(
    "isolines at the backend's level are cased: ink stands off its casing on the stage"
  ) { fx =>
    val w = Wired(fx)
    runOnFx {
      w.view.plotHost.setOutputScaleOverride(Some(2.0))
      w.view.show(input(blank))
      w.view.showMap(Some(MapRequest(grid, style)))
    }
    val (_, frame) = drawn(w, 2.0)(_.map.contains(grid.map))
    val img        = snapshot(fx, SnapshotScale.X2)
    // The grid crosses 0.5 midway across the frame: a vertical isoline.
    val crossing = frameLeft + (Isolines.of(grid).head.segments.head._1.x) * cell
    Vector(0.3, 0.5, 0.7).foreach { fy =>
      val y        = frameTop + fy * ret07.placement.height
      val (cx, cy) = pixelAt(w, frame, crossing, y, 2)
      // The ink is the darkest pixel across the line, the casing the
      // lightest beside it.
      val across = (-8 to 8).map(dx => rgb(img, (cx + dx, cy)))
      val ink    = across.minBy(luminance)
      val casing = across.maxBy(luminance)
      val c      = contrast(ink, casing)
      assert(c >= 3.0, f"at ${fy * 100}%.0f%% height: ink $ink on casing $casing, $c%.2f")
    }
    w.dispose()
  }

  fxStage.test("the remembered-image underlay is disclosed whenever it is on") { fx =>
    val w     = Wired(fx)
    val shown = TrialText(TrialTextId.RememberedShown)
    runOnFx {
      w.view.plotHost.setOutputScaleOverride(Some(1.0))
      w.view.show(input(blank, RememberedImage.Shown(beach)))
    }
    def underlaid(s: TrialScene) =
      s.plot.scene.grobs
        .flatMap(g => g +: g.children)
        .exists {
          case i: Grob.Image => i.name.exists(_.value == TrialScene.UnderlayName)
          case _             => false
        }
    val (on, _) = drawn(w, 1.0)(underlaid)
    assertEquals(on.disclosure, Some(shown))
    assert(
      on.plot.scene.grobs.flatMap(g => g +: g.children).exists {
        case t: Grob.Text => t.label == shown
        case _            => false
      },
      "the disclosure is not drawn"
    )
    fx.snapshot(StudioTheme.Light)
    // Toggled off: no underlay, and the caption says it is not shown.
    runOnFx(w.view.show(input(blank, RememberedImage.Hidden(beach))))
    val (off, _) = drawn(w, 1.0)(s => !underlaid(s))
    assertEquals(off.disclosure, Some(TrialText(TrialTextId.RememberedHidden)))
    w.dispose()
  }

  // --- Another trial ---------------------------------------------------------------

  private lazy val enc03: TrialDisplay = GoldenTrials.display("P17", "enc_03")

  private def settledOn(w: Wired, d: TrialDisplay): TrialScene =
    drawn(w, 1.0)(s =>
      !s.frameArt.isInstanceOf[FrameArt.Loading] && s.marks.isEmpty &&
        runOnFx(w.view.input).exists(_.display == d)
    )._1

  fxStage.test("showing another trial drops the map: the new trial is drawn, not refused") {
    fx =>
      val w = Wired(fx)
      runOnFx {
        w.view.plotHost.setOutputScaleOverride(Some(1.0))
        w.view.show(input(blank))
        w.view.showMap(Some(MapRequest(grid, style)))
      }
      drawn(w, 1.0)(_.map.contains(grid.map))
      runOnFx(w.view.show(input(enc03)))
      val b = settledOn(w, enc03)
      assertEquals(b.map, None)
      assert(runOnFx(w.view.status.get).isInstanceOf[TrialViewStatus.Shown])
      assertEquals(runOnFx(w.view.mapRefused), None)
      w.dispose()
  }

  fxStage.test("a map arriving after another trial is shown is not installed") { fx =>
    // Renders wait until released, so the raster arrives late.
    val held = java.util.concurrent.LinkedBlockingQueue[Runnable]()
    val w    = Wired(
      fx,
      MapRasterStore.on(
        RasterBudget.Default,
        r => held.put(r),
        r => javafx.application.Platform.runLater(r)
      )
    )
    runOnFx {
      w.view.plotHost.setOutputScaleOverride(Some(1.0))
      w.view.show(input(blank))
      w.view.showMap(Some(MapRequest(grid, style)))
      w.view.show(input(enc03))
    }
    settledOn(w, enc03)
    // Now ret_07's raster is rendered; its delivery runs on the FX thread.
    assertEquals(held.size, 1)
    while !held.isEmpty do held.take().run()
    runOnFx(())
    runOnFx(())
    val b = settledOn(w, enc03)
    assertEquals(b.map, None)
    assert(runOnFx(w.view.status.get).isInstanceOf[TrialViewStatus.Shown])
    w.dispose()
  }

  fxStage.test("a refused map is observable and cleared by dispose") { fx =>
    val w = Wired(fx)
    w.store.close()
    var seen = Vector.empty[Option[eyes4s.studio.desktop.maps.RasterRefusal]]
    runOnFx {
      w.view.mapRefusedProperty.addListener((_, _, now) => seen = seen :+ now)
      w.view.show(input(blank))
      w.view.showMap(Some(MapRequest(grid, style)))
    }
    assertEquals(
      runOnFx(w.view.mapRefused),
      Some(eyes4s.studio.desktop.maps.RasterRefusal.Closed(RasterKey(grid.map, style)))
    )
    assertEquals(seen.lastOption.flatten.map(_.productPrefix), Some("Closed"))
    runOnFx(w.view.dispose())
    assertEquals(runOnFx(w.view.mapRefused), None)
    assertEquals(seen.lastOption, Some(None))
  }
