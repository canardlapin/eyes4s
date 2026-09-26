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

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import eyes4s.codec.ByteDigest
import eyes4s.studio.app.tokens.{
  Colour,
  PaletteToken,
  StageToken,
  StageVariant,
  Theme,
  ThemedToken,
  Tokens
}
import eyes4s.studio.core.assets.{AssetLink, AssetRef, Display, TrialDisplay}
import eyes4s.studio.desktop.harness.{
  FxStage,
  SnapshotScale,
  StageSize,
  StudioFxSuite,
  StudioTheme
}
import eyes4s.studio.desktop.platform.FileProjectStore
import eyes4s.studio.desktop.plot.{PlotFrame, PlotHostStatus}
import eyes4s.studio.desktop.typography.StudioFonts
import eyes4s.studio.viz.plot.DataPoint
import eyes4s.studio.viz.trial.*
import intaglio.javafx.JavaFxCommand
import intaglio.{blue, green, red, value}
import javafx.geometry.Point2D
import javafx.scene.layout.StackPane

import java.awt.image.BufferedImage
import java.nio.file.Path
import javax.imageio.ImageIO
import scala.concurrent.duration.Duration

/** The trial view (S4.3a): every display kind in both themes on every stage,
  * missing never blank (pixel probe), no image behind a blank query, one
  * transform for image, marks and picking, stimuli read through the asset
  * registry from the golden fixture and the bundle, and the board snapshots.
  */
class TrialViewFxSuite extends StudioFxSuite:

  // The trial panes of the Main and Explore boards are about this size.
  override protected def stageSize: StageSize = StageSize(720, 440)

  override val munitTimeout: Duration = Duration(120, "s")

  override def beforeAll(): Unit =
    super.beforeAll()
    val problems = runOnFx(StudioFonts.loadAll())
    assert(problems.isEmpty, problems.map(_.message).mkString("\n"))

  private val TimeoutMillis = 30000L

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private val golden: StimulusSource = StimulusSource.directory(GoldenTrials.stimuli)

  // --- Trials of the golden fixture, through the asset registry ---------------
  private lazy val ret07 = GoldenTrials.display("P17", "ret_07")
  private lazy val enc03 = GoldenTrials.display("P17", "enc_03")
  private lazy val enc08 = GoldenTrials.display("P01", "enc_08") // forest-044.png, missing
  private lazy val beach: AssetRef = enc03.asset match
    case Some(AssetLink.Present(a)) => a
    case other                      => fail(s"enc_03 should name a stored image, not $other")

  private lazy val ret07Fix = GoldenTrials.fixations("P17", "ret_07")
  private lazy val enc03Fix = GoldenTrials.fixations("P17", "enc_03")

  /** One display of each kind, on the golden geometry. */
  private lazy val kinds: List[(String, TrialDisplay)] = List(
    "image"       -> enc03,
    "blank"       -> ret07.copy(display = Display.Blank),
    "blank-cross" -> ret07,
    "cue"         -> ret07.copy(display = Display.Cue(None)),
    "unknown"     -> ret07.copy(display = Display.Unknown(None)),
    "missing"     -> enc08
  )

  private def input(
      d: TrialDisplay,
      fixations: Vector[TrialFixation],
      marks: MarkStyle,
      theme: Theme,
      stage: StageVariant = StageVariant.Dark,
      options: TrialSceneOptions = TrialSceneOptions()
  ): TrialSceneInput =
    TrialSceneInput(d, GoldenTrials.screen, fixations, marks, theme, stage, options = options)

  // --- The view on a stage ------------------------------------------------------

  private def viewIn(fx: FxStage): TrialView =
    val view = runOnFx(TrialView(golden))
    fx.show(runOnFx(StackPane(view)))
    view

  /** Shows `in` at output scale `k` and waits until its final scene is drawn:
    * the stimulus decided (loaded or refused) and the host's frame current.
    */
  private def showAndDraw(
      view: TrialView,
      in: TrialSceneInput,
      k: Double
  ): (TrialScene, PlotFrame) =
    runOnFx {
      view.plotHost.setOutputScaleOverride(Some(k))
      view.show(in)
    }
    val deadline = System.currentTimeMillis + TimeoutMillis
    // Settled: the stimulus decided, the view laid out for the scene's aspect,
    // and the host's frame drawn for that scene at that size and scale.
    def settled: Option[(TrialScene, PlotFrame)] = runOnFx {
      view.getScene.getRoot.applyCss()
      view.getScene.getRoot.layout()
      val host = view.plotHost
      (view.status.get, host.status.get) match
        case (TrialViewStatus.Shown(scene), PlotHostStatus.Drawn(frame))
            if !scene.frameArt.isInstanceOf[FrameArt.Loading] &&
              (frame.plan.scene eq scene.plot.scene) && frame.surface.deviceScale == k &&
              frame.surface.logicalWidth == host.getWidth &&
              frame.surface.logicalHeight == host.getHeight =>
          Some((scene, frame))
        case _ => None
    }
    var result = settled
    while result.isEmpty do
      if System.currentTimeMillis > deadline then
        fail(s"not drawn within $TimeoutMillis ms: ${runOnFx(view.status.get)}")
      Thread.sleep(5)
      result = settled
    result.get

  private def snapshot(
      fx: FxStage,
      view: TrialView,
      in: TrialSceneInput,
      scale: SnapshotScale
  ): (TrialScene, PlotFrame, Path, BufferedImage) =
    val (scene, frame) = showAndDraw(view, in, scale.factor.toDouble)
    val theme          = if in.theme == Theme.Dark then StudioTheme.Dark else StudioTheme.Light
    val file           = fx.snapshot(theme, List(scale)).head
    (scene, frame, file, ImageIO.read(file.toFile))

  // The snapshot pixel under screen position (x, y) of the trial.
  private def pixelAt(
      view: TrialView,
      frame: PlotFrame,
      x: Double,
      y: Double,
      k: Int
  ): (Int, Int) =
    val c = right(frame.transform.dataToCanvas(DataPoint(x, y)))
    val s = runOnFx(view.plotHost.localToScene(Point2D(c.x, c.y)))
    (math.floor(s.getX * k).toInt, math.floor(s.getY * k).toInt)

  private def rgb(image: BufferedImage, px: (Int, Int)): (Int, Int, Int) =
    val argb = image.getRGB(px._1, px._2)
    ((argb >> 16) & 0xff, (argb >> 8) & 0xff, argb & 0xff)

  private def near(actual: (Int, Int, Int), c: Colour, tolerance: Int = 12): Boolean =
    math.abs(actual._1 - c.red) <= tolerance && math.abs(actual._2 - c.green) <= tolerance &&
      math.abs(actual._3 - c.blue) <= tolerance

  private def hex(c: (Int, Int, Int)): String = f"#${c._1}%02X${c._2}%02X${c._3}%02X"

  private val screenGrey: Colour = Tokens.palette(PaletteToken.Screen)

  // --- Every display kind x theme x stage ----------------------------------------

  for
    kind  <- List("image", "blank", "blank-cross", "cue", "unknown", "missing")
    stage <- StageVariant.values.toList
  do
    fxStage.test(s"$kind on the ${stage.cssName} stage renders in light and dark") { fx =>
      val d    = kinds.toMap.apply(kind)
      val view = viewIn(fx)
      Theme.values.foreach { theme =>
        val in                        = input(d, Vector.empty, MarkStyle.Neutral, theme, stage)
        val (scene, frame, file, img) = snapshot(fx, view, in, SnapshotScale.X1)
        val stageColour               = Tokens.staged(stage, StageToken.Stage)
        val corner                    = runOnFx(view.plotHost.localToScene(Point2D(3.0, 3.0)))
        val cornerPx                  = rgb(img, (corner.getX.toInt, corner.getY.toInt))
        assert(near(cornerPx, stageColour), s"$file: stage corner is ${hex(cornerPx)}")
        // A point of the image frame away from its centre.
        val probe = rgb(img, pixelAt(view, frame, 560.0, 800.0, 1))
        kind match
          case "image" =>
            assertEquals(scene.frameArt, FrameArt.Picture(beach))
            val raster =
              right(Stimuli.decode(beach.file, right(Stimuli.verified(golden, beach))))
            val want = raster.pixelUnsafe(560 - 448, 800 - 156)
            assert(
              math.abs(probe._1 - want.red) <= 24 && math.abs(probe._2 - want.green) <= 24 &&
                math.abs(probe._3 - want.blue) <= 24,
              s"$file: image pixel ${hex(probe)} vs stimulus $want"
            )
          case "blank" | "blank-cross" | "cue" =>
            assert(
              near(probe, screenGrey),
              s"$file: frame is ${hex(probe)}, not the screen grey"
            )
          case "unknown" =>
            assert(near(probe, stageColour), s"$file: unknown frame is ${hex(probe)}")
          case "missing" =>
            assertEquals(scene.frameArt, FrameArt.Missing(enc08.asset.get.fileName))
            assert(!near(probe, screenGrey), s"$file: missing frame drawn as blank")
          case other => fail(s"no expectation for $other")
      }
      runOnFx(view.dispose())
    }

  // --- Missing is not blank; a blank query shows no image --------------------------

  // Colours along a row across the image frame, 16 screen px apart.
  private def frameRow(view: TrialView, frame: PlotFrame, img: BufferedImage, y: Double) =
    (464 until 1464 by 16).map(x => rgb(img, pixelAt(view, frame, x.toDouble, y, 1))).toVector

  fxStage.test("a missing asset is hatched, never blank (pixel probe)") { fx =>
    val view                   = viewIn(fx)
    val options                = TrialSceneOptions(windowOutline = false)
    val (_, mFrame, _, mImage) =
      snapshot(
        fx,
        view,
        input(enc08, Vector.empty, MarkStyle.Neutral, Theme.Light, options = options),
        SnapshotScale.X1
      )
    val missing                = frameRow(view, mFrame, mImage, 300.0)
    val (_, bFrame, _, bImage) = snapshot(
      fx,
      view,
      input(
        ret07.copy(display = Display.Blank),
        Vector.empty,
        MarkStyle.Neutral,
        Theme.Light,
        options = options
      ),
      SnapshotScale.X1
    )
    val blank = frameRow(view, bFrame, bImage, 300.0)
    assert(blank.forall(near(_, screenGrey)), s"blank row: ${blank.map(hex).distinct}")
    assert(!missing.exists(near(_, screenGrey)), s"missing row: ${missing.map(hex).distinct}")
    val warnText = Tokens.themed(Theme.Light, ThemedToken.WarnText)
    val warnSoft = Tokens.themed(Theme.Light, ThemedToken.WarnSoft)
    assert(missing.exists(near(_, warnText, 40)), "no hatch stroke in the missing frame")
    assert(missing.exists(near(_, warnSoft, 24)), "no hatch ground in the missing frame")
    runOnFx(view.dispose())
  }

  fxStage.test("a blank-display query shows no image, even with the item's image loaded") {
    fx =>
      val view = viewIn(fx)
      // The matched reference first, so beach-042.png is decoded and cached.
      val (encScene, _) = showAndDraw(
        view,
        input(enc03, enc03Fix, MarkStyle.Role(TrialRole.Matched), Theme.Light),
        1.0
      )
      assertEquals(encScene.frameArt, FrameArt.Picture(beach))
      val (scene, frame, file, img) =
        snapshot(
          fx,
          view,
          input(ret07, ret07Fix, MarkStyle.Role(TrialRole.Query), Theme.Light),
          SnapshotScale.X1
        )
      assertEquals(scene.frameArt, FrameArt.ScreenWithCross)
      // Frame points clear of the marks, the order lines' ends and the cross.
      val probes = for
        x <- 480 until 1460 by 40
        y <- 180 until 900 by 40
        if ret07Fix.forall(f => math.hypot(f.screenX - x, f.screenY - y) > 60) &&
          math.hypot(960.0 - x, 540.0 - y) > 40
      yield (x, y)
      val lines                           = ret07Fix.sliding(2).toVector
      def onLine(x: Int, y: Int): Boolean = lines.exists { pair =>
        val (a, b)   = (pair(0), pair(1))
        val (dx, dy) = (b.screenX - a.screenX, b.screenY - a.screenY)
        val t        = (((x - a.screenX) * dx + (y - a.screenY) * dy) / (dx * dx + dy * dy))
          .max(0.0)
          .min(1.0)
        math.hypot(a.screenX + t * dx - x, a.screenY + t * dy - y) < 12
      }
      val clear = probes.filterNot(onLine)
      assert(clear.size > 100, s"only ${clear.size} probes")
      val off = clear
        .map((x, y) => (x, y) -> rgb(img, pixelAt(view, frame, x, y, 1)))
        .filterNot(p => near(p._2, screenGrey))
      assert(
        off.isEmpty,
        s"$file: not screen grey at ${off.take(5).map((p, c) => s"$p ${hex(c)}")}"
      )
      runOnFx(view.dispose())
  }

  // --- One transform for image, marks and picking, in the host --------------------

  fxStage.test("image, marks and picking share the drawn frame's transform") { fx =>
    val view = viewIn(fx)
    List(1.0, 2.0).foreach { k =>
      val (scene, frame) = showAndDraw(
        view,
        input(enc03, enc03Fix, MarkStyle.Role(TrialRole.Matched), Theme.Light),
        k
      )
      assert(frame.plan.context eq frame.transform.renderContext)
      val marks = frame.program.commands.collect {
        case JavaFxCommand.PointBatch(points, _, _, _, Some(n))
            if n.value == TrialScene.MarksName =>
          points
      }.flatten
      assertEquals(marks.size, 13)
      scene.marks.zip(marks).foreach { (m, at) =>
        val want = right(frame.transform.dataToDevice(m.at))
        assertEqualsDouble(at.x, want.x, 1e-9)
        assertEqualsDouble(at.y, want.y, 1e-9)
        val back = frame.transform.canvasToData(frame.transform.deviceToCanvas(at))
        assertEqualsDouble(back.x, m.at.x, 1e-6)
        assertEqualsDouble(back.y, m.at.y, 1e-6)
      }
      val images = frame.program.commands.collect { case i: JavaFxCommand.Image => i }
      assertEquals(images.size, 1)
      val topLeft = right(frame.transform.dataToDevice(DataPoint(448.0, 156.0)))
      assertEqualsDouble(images.head.x, topLeft.x, 1e-9)
      assertEqualsDouble(images.head.y, topLeft.y, 1e-9)
      // The host canvas keeps the scene's aspect.
      assertEqualsDouble(
        frame.surface.logicalWidth / frame.surface.logicalHeight,
        scene.aspect,
        0.01
      )
    }
    runOnFx(view.dispose())
  }

  // --- Stimuli through the registry: fixture directory and bundle -------------------

  test("a stored stimulus loads from the golden fixture and from the bundle, checked") {
    val fromFixture = Stimuli.load(golden, beach)
    val store       = FileProjectStore.at[IO](GoldenTrials.bundle).unsafeRunSync()
    val fromBundle  = Stimuli.load(StimulusSource.bundle(store), beach)
    (fromFixture, fromBundle) match
      case (StimulusRaster.Loaded(a), StimulusRaster.Loaded(b)) =>
        assertEquals((a.width, a.height), (1024, 768))
        assertEquals(a, b)
      case other => fail(s"not loaded: $other")
    val forged = beach.copy(sha256 = ByteDigest.sha256(IArray.from("forged".getBytes)))
    Stimuli.verified(golden, forged) match
      case Left(StimulusError.DigestMismatch(f, _, actual)) =>
        assertEquals(f, beach.file)
        assertEquals(actual, beach.sha256)
      case other => fail(s"unexpected $other")
    val absent = enc08.asset.get.fileName
    Stimuli.verified(golden, AssetRef(absent, beach.sha256)) match
      case Left(e: StimulusError.NotStored) =>
        assert(e.message.contains("forest-044.png"), e.message)
      case other => fail(s"unexpected $other")
  }

  // --- Board snapshots ----------------------------------------------------------------

  private def boardSnapshots(
      fx: FxStage,
      in: Theme => TrialSceneInput
  ): List[(TrialScene, Path)] =
    val view  = viewIn(fx)
    val shots = for
      theme <- List(Theme.Light, Theme.Dark)
      scale <- List(SnapshotScale.X1, SnapshotScale.X2)
    yield
      val (scene, _, file, _) = snapshot(fx, view, in(theme), scale)
      (scene, file)
    runOnFx(view.dispose())
    shots

  fxStage.test("snapshot: P17 ret_07 as the Compare query (Main board)") { fx =>
    val shots =
      boardSnapshots(fx, t => input(ret07, ret07Fix, MarkStyle.Role(TrialRole.Query), t))
    val scene = shots.head._1
    assertEquals(scene.frameArt, FrameArt.ScreenWithCross)
    assertEquals(scene.marks.size, 12)
    assertEquals(scene.marks.count(_.window == WindowSide.Outside), 1)
    assert(scene.caption.startsWith("Displayed: blank + fixation cross"), scene.caption)
    shots.foreach((_, f) => println(s"snapshot: $f"))
  }

  fxStage.test("snapshot: P17 enc_03 as the matched reference (Main board)") { fx =>
    val shots =
      boardSnapshots(fx, t => input(enc03, enc03Fix, MarkStyle.Role(TrialRole.Matched), t))
    val scene = shots.head._1
    assertEquals(scene.frameArt, FrameArt.Picture(beach))
    assertEquals(scene.marks.size, 13)
    assertEquals(scene.marks.count(_.window == WindowSide.Outside), 1)
    shots.foreach((_, f) => println(s"snapshot: $f"))
  }

  fxStage.test("snapshot: P17 enc_03 in Explore with neutral marks (Explore board)") { fx =>
    val shots = boardSnapshots(fx, t => input(enc03, enc03Fix, MarkStyle.Neutral, t))
    val scene = shots.head._1
    assertEquals(scene.marks.size, 13)
    assertEquals(
      scene.caption,
      "Displayed: image beach-042.png · 1024×768 at (448, 156) in 1920×1080"
    )
    shots.foreach((_, f) => println(s"snapshot: $f"))
  }

  fxStage.test("dispose releases the host and ignores later loads") { fx =>
    val view = viewIn(fx)
    runOnFx {
      view.show(input(enc03, enc03Fix, MarkStyle.Neutral, Theme.Light))
      view.dispose()
      view.dispose()
      view.show(input(ret07, ret07Fix, MarkStyle.Neutral, Theme.Light))
    }
    assertEquals(runOnFx(view.status.get), TrialViewStatus.Disposed)
    assertEquals(runOnFx(view.plotHost.status.get), PlotHostStatus.Disposed)
  }
