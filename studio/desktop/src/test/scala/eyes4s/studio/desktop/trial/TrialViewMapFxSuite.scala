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
import eyes4s.studio.app.tokens.{Colour, PaletteToken, StageVariant, Theme, ThemedToken, Tokens}
import eyes4s.studio.core.assets.{AssetLink, AssetRef, Display, TrialDisplay}
import eyes4s.studio.core.backend.RunId
import eyes4s.studio.core.selection.{ScaleIndex, FixationIndex}
import eyes4s.studio.core.document.{ScreenSize, ImagePlacement}
import eyes4s.plan.MapPlacement
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
import eyes4s.studio.viz.maps.MapLegendScene
import eyes4s.studio.viz.plot.{DataPoint, PlotScene}
import eyes4s.studio.viz.trial.*
import intaglio.{Grob, DeviceElement, DevicePrimitive, RenderPlan, value}
import intaglio.javafx.JavaFxCanvasContext
import javafx.scene.canvas.Canvas
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

  // `size` holds the view to that layout size, centred on the stage.
  private final class Wired(
      fx: FxStage,
      made: => MapRasterStore = MapRasterStore(),
      size: Option[(Double, Double)] = None
  ):
    val store = made
    val view  = runOnFx(TrialView(golden, TrialView.sharedLoader, store))
    size.foreach((w, h) => runOnFx(view.setMaxSize(w, h)))
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
    "the map is the run's grid from the raster cache, drawn at 0.85, values unchanged"
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
    // On screen: a cell far from the isoline is its ramp colour over the
    // blank screen, at 0.85 times its own alpha (below 25% of the limits, a
    // mass cell fades in).
    val img    = snapshot(fx, SnapshotScale.X1)
    val px     = pixelAt(w, frame, frameLeft + 5.5 * cell, frameTop + 5.5 * cell, 1)
    val argb   = MapColours.argb(slopedStyle, sloped.atTop(5, 5))
    val screen = Tokens.palette(PaletteToken.Screen)
    val a      = ((argb >>> 24) & 0xff) / 255.0 * 0.85
    assert(a < 0.85, s"cell (5, 5) should lie under the alpha floor, at alpha $a")
    def mix(c: Int, s: Int) = math.round(c * a + s * (1.0 - a)).toInt
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

  // --- Cells without coverage (review ticket 8) ---------------------------------

  /** `grid` without a value in a 16 × 16 block of cells from column 8, row 8
    * down from the top: test values, not results.
    */
  private lazy val holed: MapGrid =
    right(
      MapGrid.of(
        grid.map,
        Columns,
        Rows,
        RowOrder.TopFirst,
        grid.cells.zipWithIndex.map { (c, i) =>
          val (x, y) = (i % Columns, i / Columns)
          if x >= 8 && x < 24 && y >= 8 && y < 24 then None else c
        },
        grid.levels
      )
    )

  /** How far, per channel, a pixel of the hatch may lie from an opaque mix
    * of its two inks: the canvas's rounding of an antialiased stripe edge. A
    * hatch the screen shows through lies about 25 off it.
    */
  private val InkMixTolerance = 6

  // How far `c` lies from the nearest opaque mix of `a` and `b`, per channel.
  private def offMix(c: (Int, Int, Int), a: Colour, b: Colour): Double =
    val (p, q, r) =
      (Vector(c._1, c._2, c._3), Vector(a.red, a.green, a.blue), Vector(b.red, b.green, b.blue))
    val d = q.zip(r).map((u, v) => (v - u).toDouble)
    val t =
      (p.zip(q).zip(d).map { case ((x, u), e) => (x - u) * e }.sum / d.map(e => e * e).sum)
        .max(0.0)
        .min(1.0)
    p.zip(q).zip(d).map { case ((x, u), e) => math.abs(x - (u + t * e)) }.max

  fxStage.test("cells without coverage are hatched on screen and counted in the caption") {
    fx =>
      val holedStyle = ColourLimits.spanning(MapPalette.Mass, Vector(holed))
      val warnText   = Tokens.themed(Theme.Light, ThemedToken.WarnText)
      val warnSoft   = Tokens.themed(Theme.Light, ThemedToken.WarnSoft)
      // The trial drawn at several sizes (the stage's, a smaller one, each
      // with and without the map's legend taking room under it) and output
      // scales 1 and 2, so the rows' shared edges cross device pixels at
      // many offsets.
      for
        size  <- Vector(None, Some((611.0, 383.0)))
        place <- Vector(MapLegendPlacement.UnderTrial, MapLegendPlacement.InHostLegend)
        k     <- Vector(1, 2)
      do
        val w = Wired(fx, size = size)
        runOnFx {
          w.view.placeLegend(place)
          w.view.plotHost.setOutputScaleOverride(Some(k.toDouble))
          w.view.show(input(blank))
          w.view.showMap(Some(MapRequest(holed, holedStyle)))
        }
        val (scene, frame) = drawn(w, k.toDouble)(_.map.contains(holed.map))
        assert(scene.caption.endsWith(" · 256 cells without coverage"), scene.caption)
        val img = snapshot(fx, if k == 1 then SnapshotScale.X1 else SnapshotScale.X2)
        // Every device pixel inside the hole: the hatch's stripes and ground,
        // opaque, never the blank screen showing through and never a ramp
        // colour.
        val (x0, y0) = pixelAt(w, frame, frameLeft + 9.0 * cell, frameTop + 9.0 * cell, k)
        val (x1, y1) = pixelAt(w, frame, frameLeft + 23.0 * cell, frameTop + 23.0 * cell, k)
        val hole     = for
          py <- y0 to y1
          px <- x0 to x1
        yield (px, py) -> rgb(img, (px, py))
        val at   = s"${size.getOrElse("stage")}, $place, scale $k"
        val seen = hole.map(_._2).distinct
        val off  = hole.filter((_, c) => offMix(c, warnSoft, warnText) > InkMixTolerance)
        assert(off.isEmpty, s"$at: the hatch is not opaque at ${off.take(8)}")
        assert(seen.exists(near(_, warnText, 40)), s"$at: no hatch stroke: $seen")
        assert(seen.exists(near(_, warnSoft, 24)), s"$at: no hatch ground: $seen")
        w.dispose()
  }

  private def near(c: (Int, Int, Int), t: Colour, tol: Int) =
    math.abs(c._1 - t.red) <= tol && math.abs(c._2 - t.green) <= tol &&
      math.abs(c._3 - t.blue) <= tol

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

  // --- The map legend (review ticket 17) ------------------------------------------

  /** How far, per channel, a snapshot pixel may lie from the colour composited
    * by hand: the canvas's own rounding of a translucent fill.
    */
  private val CompositeTolerance = 10

  /** Waits until the legend host shows `legend`'s scene at scale 1. */
  private def legendDrawn(w: Wired): (MapLegendScene, PlotFrame) =
    val deadline = System.currentTimeMillis + TimeoutMillis
    def settled  = runOnFx {
      w.view.getScene.getRoot.applyCss()
      w.view.getScene.getRoot.layout()
      val host = w.view.legendPlotHost
      (w.view.legend, host.status.get) match
        case (Some(l), PlotHostStatus.Drawn(frame))
            if (frame.plan.scene eq l.plot.scene) && frame.surface.deviceScale == 1.0 &&
              frame.surface.logicalWidth == host.getWidth &&
              frame.surface.logicalHeight == host.getHeight =>
          Some((l, frame))
        case _ => None
    }
    var result = settled
    while result.isEmpty do
      if System.currentTimeMillis > deadline then
        fail(s"legend not drawn within $TimeoutMillis ms: ${runOnFx(w.view.legend)}")
      Thread.sleep(5)
      result = settled
    result.get

  fxStage.test(
    "a mass map's legend is drawn under the trial's caption: ramp strip, floor, isoline key"
  ) { fx =>
    val w = Wired(fx)
    runOnFx {
      w.view.plotHost.setOutputScaleOverride(Some(1.0))
      w.view.legendPlotHost.setOutputScaleOverride(Some(1.0))
      w.view.show(input(blank))
      w.view.showMap(Some(MapRequest(grid, style)))
    }
    drawn(w, 1.0)(_.map.contains(grid.map))
    val (legend, _) = legendDrawn(w)
    // Directly under the trial canvas (its caption band is the canvas's
    // foot), as wide as the trial, the legend's height.
    val (trialBox, legendBox, shown) = runOnFx {
      val t = w.view.plotHost
      val l = w.view.legendPlotHost
      (
        (t.getLayoutX, t.getLayoutY, t.getWidth, t.getHeight),
        (l.getLayoutX, l.getLayoutY, l.getWidth, l.getHeight),
        l.isVisible
      )
    }
    assert(shown, "the legend is hidden")
    assertEquals(legendBox._1, trialBox._1)
    assertEquals(legendBox._2, trialBox._2 + trialBox._4)
    assertEquals(legendBox._3, trialBox._3)
    assertEquals(legendBox._4, MapLegendScene.HeightPx)
    assert(runOnFx(w.view.getHeight) >= trialBox._2 + trialBox._4 + legendBox._4)
    // On screen: the zero swatch is see-through (the stage shows), the last
    // is the ramp's darkest stop at 0.85 over the stage.
    val img                                  = snapshot(fx, SnapshotScale.X1)
    def swatchPixel(k: Int): (Int, Int, Int) =
      val p = runOnFx(
        w.view.legendPlotHost.localToScene(
          Point2D(MapLegendScene.StripLeftPx + 10.0 * k + 5.0, MapLegendScene.FirstRowPx)
        )
      )
      rgb(img, (math.floor(p.getX).toInt, math.floor(p.getY).toInt))
    val stage   = Tokens.staged(StageVariant.Dark, eyes4s.studio.app.tokens.StageToken.Stage)
    val darkest = Tokens.massRamp.stops.last
    def near(c: (Int, Int, Int), want: (Int, Int, Int)) =
      (0 to 2).forall(i =>
        math.abs(
          c.productElement(i).asInstanceOf[Int] - want.productElement(i).asInstanceOf[Int]
        ) <= CompositeTolerance
      )
    def over(a: Double)(c: Int, s: Int) = math.round(c * a + s * (1.0 - a)).toInt
    val zero                            = swatchPixel(0)
    assert(near(zero, (stage.red, stage.green, stage.blue)), s"zero swatch $zero")
    val full = swatchPixel(MapLegendScene.Swatches - 1)
    val want = (
      over(0.85)(darkest.red, stage.red),
      over(0.85)(darkest.green, stage.green),
      over(0.85)(darkest.blue, stage.blue)
    )
    assert(near(full, want), s"max swatch $full, expected about $want")
    // Its words are the catalogue's, the floor's included.
    val words = legend.plot.scene.grobs
      .flatMap(g => g +: g.children)
      .collect { case t: Grob.Text => t.label }
    assert(words.contains(MapLegendScene.rampText), words)
    assert(MapLegendScene.rampText.endsWith("fades below 25% of max"), MapLegendScene.rampText)
    w.dispose()
  }

  fxStage.test("no map, or a difference map, draws no legend; another trial drops it") { fx =>
    val w = Wired(fx)
    runOnFx {
      w.view.plotHost.setOutputScaleOverride(Some(1.0))
      w.view.show(input(blank))
    }
    drawn(w, 1.0)(_.map.isEmpty)
    def none(): Unit =
      runOnFx {
        assertEquals(w.view.legend, None)
        assert(!w.view.legendPlotHost.isVisible, "a legend is shown")
      }
    none()
    // A difference map: no board draws a legend for one on a trial.
    val difference = ColourLimits.spanning(MapPalette.Difference, Vector(grid))
    runOnFx(w.view.showMap(Some(MapRequest(grid, difference))))
    drawn(w, 1.0)(_.map.contains(grid.map))
    none()
    assertEquals(runOnFx(w.view.legendRefused), None)
    // A mass map draws one; removing the map removes it.
    runOnFx(w.view.showMap(Some(MapRequest(grid, style))))
    drawn(w, 1.0)(s => s.map.contains(grid.map) && runOnFx(w.view.legend).nonEmpty)
    runOnFx(w.view.showMap(None))
    drawn(w, 1.0)(_.map.isEmpty)
    none()
    w.dispose()
  }

  fxStage.test("the trial view's accessible description ends with the legend's") { fx =>
    val w = Wired(fx)
    runOnFx {
      w.view.plotHost.setOutputScaleOverride(Some(1.0))
      w.view.legendPlotHost.setOutputScaleOverride(Some(1.0))
      w.view.show(input(blank))
      w.view.showMap(Some(MapRequest(grid, style, limits = LimitsScope.Shared)))
    }
    drawn(w, 1.0)(_.map.contains(grid.map))
    val (legend, _) = legendDrawn(w)
    val said        =
      "Map legend: fixation duration mass per cell from 0 to max, fading out below 25 " +
        "percent of max, shared colour limits across panels; isolines enclose 50 and 90 " +
        "percent of mass"
    assertEquals(legend.description, said)
    val (trialHelp, legendHelp) = runOnFx(
      (
        Option(w.view.plotHost.getAccessibleHelp),
        Option(w.view.legendPlotHost.getAccessibleHelp)
      )
    )
    assert(trialHelp.exists(_.endsWith(" " + said)), s"trial view help: $trialHelp")
    assertEquals(legendHelp, Some(said))
    // Without the map, the trial's description says nothing of a legend.
    runOnFx(w.view.showMap(None))
    drawn(w, 1.0)(_.map.isEmpty)
    val bare = runOnFx(Option(w.view.plotHost.getAccessibleHelp))
    assert(bare.exists(h => !h.contains("Map legend")), s"trial view help: $bare")
    w.dispose()
  }

  private final case class Ink(
      label: String,
      left: Double,
      top: Double,
      right: Double,
      bottom: Double
  )

  private def textInk(plot: PlotScene, frame: PlotFrame): Vector[Ink] = runOnFx {
    def flatten(elements: Vector[DeviceElement]): Vector[DevicePrimitive] = elements.flatMap {
      case DeviceElement.Mark(primitive)          => Vector(primitive)
      case DeviceElement.Group(_, _, _, children) => flatten(children)
      case DeviceElement.Annotated(_, children)   => flatten(children)
    }
    val device  = right(RenderPlan(plot.scene, frame.transform.renderContext).deviceScene)
    val metrics = JavaFxCanvasContext(Canvas().getGraphicsContext2D)
    flatten(device.elements).collect {
      case DevicePrimitive.TextRun(
            label,
            x,
            y,
            horizontal,
            vertical,
            rotation,
            fontPx,
            family,
            gp,
            _
          ) =>
        assertEquals(rotation, 0.0)
        val box = metrics
          .measureText(label, family, fontPx, gp.fontWeight, horizontal, vertical)
          .getOrElse(fail("the actual JavaFX context did not measure caption text"))
        Ink(
          label,
          x + box.left,
          y + box.top,
          x + box.left + box.width,
          y + box.top + box.height
        )
    }
  }

  fxStage.test("Main's 432 px trial pane keeps all caption wording clear") { fx =>
    val w = Wired(fx, size = Some(432.0 -> 270.0))
    try
      runOnFx {
        w.view.show(
          input(ret07, RememberedImage.Hidden(beach))
            .copy(fixations = GoldenTrials.fixations("P17", "ret_07"))
        )
        w.view.showMap(Some(MapRequest(grid, style, limits = LimitsScope.Shared)))
      }
      val nativeScale = runOnFx(fx.stage.getOutputScaleX)
      Vector(nativeScale, 1.0, 2.0).distinct.foreach { scale =>
        runOnFx(
          w.view.plotHost.setOutputScaleOverride(Option.when(scale != nativeScale)(scale))
        )
        val (scene, frame) = drawn(w, scale)(_.map.contains(grid.map))
        val ink            = textInk(scene.plot, frame)
        val joined         = ink.map(_.label).mkString(" ")
        Vector(
          scene.caption,
          TrialText(TrialTextId.MarkerArea),
          TrialText(TrialTextId.OrderNote),
          scene.disclosure.get
        ).foreach(s => assert(joined.contains(s), s"missing caption wording: $s"))
        ink.foreach { i =>
          assert(i.left >= 0 && i.right <= frame.surface.deviceWidth + 0.001, i)
          assert(i.top >= 0 && i.bottom <= frame.surface.deviceHeight + 0.001, i)
        }
        ink.combinations(2).foreach { pair =>
          val a = pair(0); val b = pair(1)
          assert(
            a.right <= b.left || b.right <= a.left || a.bottom <= b.top || b.bottom <= a.top,
            s"overlapping text: $a / $b"
          )
        }
      }
      runOnFx(w.view.plotHost.setOutputScaleOverride(None))
      drawn(w, nativeScale)(_.map.contains(grid.map))
      fx.snapshot(StudioTheme.Light): Unit
    finally w.dispose()
  }

  fxStage.test(
    "normal pane keeps caption and legend words clear without stretching image coordinates"
  ) { fx =>
    val w       = Wired(fx, size = Some(680.0 -> 370.0))
    val display = ret07.copy(
      display = Display.Unknown(None),
      placement = right(ImagePlacement.of(112, 84, 800, 600))
    )
    val fixations = Vector(400.0 -> 300.0, 600.0 -> 400.0).zipWithIndex.map {
      case ((x, y), i) =>
        right(
          TrialFixation.of(
            display.trial,
            right(FixationIndex.of(i + 1)),
            x,
            y,
            200 + 100 * i,
            MapPlacement.InWindow
          )
        )
    }
    val content = input(display, RememberedImage.Hidden(beach)).copy(
      screen = right(ScreenSize.of(1024, 768)),
      fixations = fixations,
      options = TrialSceneOptions(order = true),
      marks = MarkStyle.Neutral
    )
    try
      runOnFx {
        w.view.plotHost.setOutputScaleOverride(Some(1.0))
        w.view.legendPlotHost.setOutputScaleOverride(Some(1.0))
        w.view.show(content)
        w.view.showMap(Some(MapRequest(grid, style, limits = LimitsScope.Shared)))
      }
      val (scene, frame)        = drawn(w, 1.0)(_.map.contains(grid.map))
      val (legend, legendFrame) = legendDrawn(w)
      fx.snapshot(StudioTheme.Light): Unit
      val trial   = textInk(scene.plot, frame)
      val caption = trial.find(_.label == scene.caption).get
      val area    = trial.find(_.label == TrialText(TrialTextId.MarkerArea)).get
      assert(
        caption.right + 8.0 <= area.left,
        s"caption and marker explanation overlap: $caption / $area; canvas ${frame.surface.logicalWidth}"
      )
      val order      = trial.find(_.label == TrialText(TrialTextId.OrderNote)).get
      val remembered = trial.find(_.label == scene.disclosure.get).get
      assert(
        order.right + 8.0 <= remembered.left,
        s"order and remembered-image explanations overlap: $order / $remembered"
      )
      val boundsTolerancePx = 1e-6
      trial.foreach { ink =>
        assert(
          ink.left >= -boundsTolerancePx && ink.right <= frame.surface.deviceWidth + boundsTolerancePx,
          ink
        )
        assert(
          ink.top >= -boundsTolerancePx && ink.bottom <= frame.surface.deviceHeight + boundsTolerancePx,
          ink
        )
      }
      textInk(legend.plot, legendFrame).foreach { ink =>
        assert(
          ink.left >= -boundsTolerancePx && ink.right <= legendFrame.surface.deviceWidth + boundsTolerancePx,
          ink
        )
        assert(
          ink.top >= -boundsTolerancePx && ink.bottom <= legendFrame.surface.deviceHeight + boundsTolerancePx,
          ink
        )
      }
      val start  = right(frame.transform.dataToCanvas(DataPoint(112.0, 84.0)))
      val across = right(frame.transform.dataToCanvas(DataPoint(912.0, 84.0)))
      val down   = right(frame.transform.dataToCanvas(DataPoint(112.0, 684.0)))
      assertEqualsDouble(
        (across.x - start.x) / 800.0,
        (down.y - start.y) / 600.0,
        boundsTolerancePx
      )
    finally w.dispose()
  }
