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

import eyes4s.studio.app.tokens.{Colour, Theme, ThemedToken, Tokens}
import eyes4s.studio.desktop.harness.{FxStage, SnapshotScale, StudioFxSuite, StudioTheme}
import eyes4s.studio.desktop.typography.StudioFonts
import eyes4s.studio.viz.plot.*
import intaglio.{
  DevicePoint,
  Grob,
  Interval,
  Point,
  RasterDimensions,
  RasterImage,
  Rgba32,
  Scene,
  Size,
  Viewport,
  value
}
import intaglio.javafx.{JavaFxCanvasContext, JavaFxCommand, JavaFxRenderer}
import javafx.application.Platform
import javafx.geometry.Point2D
import javafx.scene.canvas.Canvas
import javafx.scene.layout.{Region, StackPane}

import java.lang.ref.WeakReference
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, Executor, TimeUnit}
import javax.imageio.ImageIO
import scala.concurrent.duration.Duration
import scala.jdk.CollectionConverters.*

/** The canvas plot host (S4.1): the op-log golden, snapshots, the shared
  * transform across resize, off-thread compilation, failure and disposal.
  */
class CanvasPlotHostFxSuite extends StudioFxSuite:

  override def beforeAll(): Unit =
    super.beforeAll()
    val problems = runOnFx(StudioFonts.loadAll())
    assert(problems.isEmpty, problems.map(_.message).mkString("\n"))

  // The leak test runs 1,000 compile-and-draw cycles.
  override val munitTimeout: Duration = Duration(180, "s")

  private val TimeoutMillis = 30000L

  /** How long collection may take to clear the disposed hosts. */
  private val CollectMillis = 10000L

  /** Positions agree to well below a device pixel. */
  private val Tolerance = 1e-9

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private def reference(theme: Theme): PlotScene = right(ReferenceScene(theme))

  private def surface(w: Double, h: Double, scale: Double) = right(PlotSurface(w, h, scale))

  /** Polls the host on the FX thread until `p` holds; returns the status. */
  private def awaitStatus(host: CanvasPlotHost)(p: PlotHostStatus => Boolean): PlotHostStatus =
    val deadline = System.currentTimeMillis + TimeoutMillis
    var status   = runOnFx(host.status.get)
    while !p(status) do
      if System.currentTimeMillis > deadline then
        fail(s"host did not reach the expected status within $TimeoutMillis ms; it is $status")
      Thread.sleep(2)
      status = runOnFx(host.status.get)
    status

  private def awaitFrame(host: CanvasPlotHost, scale: Double): PlotFrame =
    awaitStatus(host) {
      case PlotHostStatus.Drawn(f) => f.surface.deviceScale == scale
      case _                       => false
    } match
      case PlotHostStatus.Drawn(f) => f
      case other                   => fail(s"not drawn: $other")

  // ---------------------------------------------------------------------------
  // Op-log golden
  // ---------------------------------------------------------------------------

  private val goldenResource = "eyes4s/studio/desktop/plot/reference-scene.oplog"

  private def opLog: String =
    val sections =
      for (theme, w, h, scale) <- List(
          (Theme.Light, 480.0, 320.0, 1.0),
          (Theme.Light, 480.0, 320.0, 2.0),
          (Theme.Dark, 480.0, 320.0, 1.0)
        )
      yield
        val frame    = right(PlotFrame.compile(reference(theme), surface(w, h, scale)))
        val recorder = RecordingContext()
        JavaFxRenderer.draw(frame.program, recorder)
        val header =
          s"# ${frame.sceneId.value} on ${w.toInt}x${h.toInt} at ${scale}x " +
            s"(${frame.surface.deviceWidth}x${frame.surface.deviceHeight} device pixels)"
        (header +: recorder.log).mkString("\n")
    sections.mkString("", "\n\n", "\n")

  private def buildRoot: Path =
    val in = getClass.getClassLoader.getResourceAsStream("eyes4s/studio/desktop/build-root.txt")
    try Paths.get(String(in.readAllBytes(), UTF_8).trim)
    finally in.close()

  test("the reference scene's op log equals its golden") {
    val actual = opLog
    if sys.env.contains("EYES4S_UPDATE_GOLDENS") then
      val file = buildRoot.resolve(s"studio/desktop/src/test/resources/$goldenResource")
      Files.createDirectories(file.getParent)
      Files.write(file, actual.getBytes(UTF_8)): Unit
    val in = Option(getClass.getClassLoader.getResourceAsStream(goldenResource)).getOrElse(
      fail(s"missing $goldenResource; run with EYES4S_UPDATE_GOLDENS=1 to write it")
    )
    val golden =
      try String(in.readAllBytes(), UTF_8)
      finally in.close()
    assertNoDiff(actual, golden)
  }

  // ---------------------------------------------------------------------------
  // Drawing and HiDPI
  // ---------------------------------------------------------------------------

  private def hostIn(fx: FxStage, pad: Double = 0.0): (CanvasPlotHost, StackPane) =
    val (host, root) = runOnFx {
      val host = CanvasPlotHost()
      val root = StackPane(host)
      root.setStyle(s"-fx-padding: ${pad}px")
      (host, root)
    }
    fx.show(root)
    (host, root)

  private def near(image: java.awt.image.BufferedImage, x: Int, y: Int, c: Colour): Boolean =
    val argb = image.getRGB(x, y)
    val d    = List(
      ((argb >> 16) & 0xff) - c.red,
      ((argb >> 8) & 0xff) - c.green,
      (argb & 0xff) - c.blue
    )
    d.forall(v => math.abs(v) <= 24)

  // The pixel of `snapshot` under the centre of `mark`, at snapshot scale `k`.
  private def pixelUnder(
      host: CanvasPlotHost,
      frame: PlotFrame,
      mark: ReferenceMark,
      k: Int
  ): (Int, Int) =
    val c       = right(frame.transform.dataToCanvas(mark.at))
    val inScene = runOnFx(host.localToScene(Point2D(c.x, c.y)))
    (math.floor(inScene.getX * k).toInt, math.floor(inScene.getY * k).toInt)

  List(StudioTheme.Light -> Theme.Light, StudioTheme.Dark -> Theme.Dark).foreach {
    (studioTheme, theme) =>
      fxStage.test(s"draws the reference scene at 1x and 2x (${theme.toString.toLowerCase})") {
        fx =>
          val (host, _) = hostIn(fx, pad = 24.0)
          val snaps     = List(SnapshotScale.X1, SnapshotScale.X2).map { scale =>
            val k = scale.factor
            runOnFx {
              host.setOutputScaleOverride(Some(k.toDouble))
              host.show(reference(theme))
            }
            val frame = awaitFrame(host, k.toDouble)
            val size  = runOnFx((host.getWidth, host.getHeight))
            assertEquals(
              (frame.surface.deviceWidth, frame.surface.deviceHeight),
              (math.ceil(size._1 * k).toInt, math.ceil(size._2 * k).toInt)
            )
            // One texture pixel per device pixel, whatever the screen's scale.
            assertEquals(
              runOnFx(host.canvasTexture),
              (frame.surface.deviceWidth, frame.surface.deviceHeight)
            )
            val file  = fx.snapshot(studioTheme, List(scale)).head
            val image = ImageIO.read(file.toFile)
            ReferenceScene.marks.filter(_.role != ReferenceRole.Control).foreach { mark =>
              val token =
                if mark.role == ReferenceRole.Query then ThemedToken.Query
                else ThemedToken.Match
              val (px, py) = pixelUnder(host, frame, mark, k)
              assert(
                near(image, px, py, Tokens.themed(theme, token)),
                s"$mark at ${k}x: pixel ($px, $py) is " +
                  f"${image.getRGB(px, py) & 0xffffff}%06X, not the ${token.cssName} token"
              )
            }
            // Outside the panel the host draws the scene's surface fill.
            val corner   = runOnFx(host.localToScene(Point2D(4.0, 4.0)))
            val (cx, cy) = ((corner.getX * k).toInt, (corner.getY * k).toInt)
            assert(
              near(image, cx, cy, Tokens.themed(theme, ThemedToken.Surface)),
              f"surface at ${k}x: pixel ($cx, $cy) is ${image.getRGB(cx, cy) & 0xffffff}%06X"
            )
            file
          }
          assertEquals(
            snaps.map(_.getFileName.toString),
            List(s"${studioTheme.id}-1x.png", s"${studioTheme.id}-2x.png")
          )
          runOnFx(host.dispose())
      }
  }

  fxStage.test("without an override the host lays out for its window's output scale") { fx =>
    val (host, _) = hostIn(fx)
    val window    = runOnFx(fx.stage.getOutputScaleX)
    runOnFx(host.show(reference(Theme.Light)))
    val frame = awaitFrame(host, window)
    assertEquals(runOnFx(host.outputScale), Some(window))
    assertEquals(frame.surface.deviceScale, window)
    runOnFx(host.dispose())
  }

  fxStage.test("the host's accessible help is the scene's text summary, and none without one") {
    fx =>
      val (host, _) = hostIn(fx)
      val plain     = reference(Theme.Light).withSemantics(intaglio.SceneSemantics.empty)
      val summed    = plain.withSemantics(
        intaglio.SceneSemantics.single(
          eyes4s.studio.viz.plot.SceneSummaries.semantics(plain.id, "Title", "Alt", "Summary")
        )
      )
      runOnFx(host.show(summed))
      assertEquals(runOnFx(host.getAccessibleHelp), "Summary")
      // Explicitly removing semantics leaves no stale help.
      runOnFx(host.show(plain))
      assertEquals(runOnFx(Option(host.getAccessibleHelp)), None)
      runOnFx(host.show(summed))
      runOnFx(host.clear())
      assertEquals(runOnFx(Option(host.getAccessibleHelp)), None)
      runOnFx(host.dispose())
  }

  fxStage.test("a texture-scale change replaces the canvases and redraws without compiling") {
    fx =>
      var k    = 1.0 // read and written on the FX thread only
      val host = runOnFx(
        CanvasPlotHost(CanvasPlotHost.sharedCompiler, gc => JavaFxCanvasContext(gc), () => k)
      )
      fx.show(runOnFx(StackPane(host)))
      runOnFx {
        host.setOutputScaleOverride(Some(2.0))
        host.show(reference(Theme.Light))
      }
      val frame  = awaitFrame(host, 2.0)
      val device = (frame.surface.deviceWidth, frame.surface.deviceHeight)
      def state  = runOnFx(
        (
          host.getChildrenUnmodifiable.asScala.toList,
          host.profile,
          host.canvasTexture,
          host.texturePixelScale
        )
      )
      val (canvasesBefore, profileBefore, textureBefore, scaleBefore) = state
      assertEquals((textureBefore, scaleBefore), (device, 1.0))

      // The same scale again changes nothing.
      runOnFx(host.refreshPixelScale())
      assertEquals(state, (canvasesBefore, profileBefore, textureBefore, scaleBefore))

      runOnFx {
        k = 2.0
        host.refreshPixelScale()
      }
      val (canvasesAfter, profileAfter, textureAfter, scaleAfter) = state
      assertEquals((textureAfter, scaleAfter), (device, 2.0))
      assertEquals(canvasesAfter.size, 3)
      assert(canvasesAfter.forall(c => !canvasesBefore.exists(_ eq c)), "canvases not replaced")
      // The old canvases are released: zero size frees their textures.
      assert(
        runOnFx(canvasesBefore.collect { case c: Canvas => (c.getWidth, c.getHeight) })
          .forall(_ == (0.0, 0.0))
      )
      assertEquals(
        profileAfter,
        profileBefore.copy(baseDraws = profileBefore.baseDraws + 1)
      )
      assertEquals(runOnFx(host.status.get), PlotHostStatus.Drawn(frame))

      // The redrawn frame is where its transform says.
      val image =
        ImageIO.read(fx.snapshot(StudioTheme.Light, List(SnapshotScale.X2)).head.toFile)
      ReferenceScene.marks.filter(_.role == ReferenceRole.Query).foreach { mark =>
        val (px, py) = pixelUnder(host, frame, mark, 2)
        assert(
          near(image, px, py, Tokens.themed(Theme.Light, ThemedToken.Query)),
          f"$mark: pixel ($px, $py) is ${image.getRGB(px, py) & 0xffffff}%06X"
        )
      }
      runOnFx(host.dispose())
  }

  // ---------------------------------------------------------------------------
  // One transform for scene, marks and picking, across resize
  // ---------------------------------------------------------------------------

  // The device positions of the reference marks in the program the host drew.
  private def drawnMarks(frame: PlotFrame): Vector[DevicePoint] =
    frame.program.commands.collect {
      case JavaFxCommand.PointBatch(points, _, _, _, Some(name))
          if name.value == ReferenceScene.marksName =>
        points
    }.flatten

  private def assertSharedTransform(frame: PlotFrame): Unit =
    val drawn = drawnMarks(frame)
    assertEquals(drawn.size, ReferenceScene.marks.size)
    ReferenceScene.marks.zip(drawn).foreach { (mark, at) =>
      val predicted = right(frame.transform.dataToDevice(mark.at))
      assertEqualsDouble(predicted.x, at.x, Tolerance, s"$mark x")
      assertEqualsDouble(predicted.y, at.y, Tolerance, s"$mark y")
      val back = frame.transform.canvasToData(frame.transform.deviceToCanvas(at))
      assertEqualsDouble(back.x, mark.at.x, Tolerance, s"$mark data x")
      assertEqualsDouble(back.y, mark.at.y, Tolerance, s"$mark data y")
    }
    assertEquals(frame.plan.context, frame.transform.renderContext)

  fxStage.test("resize re-lays out the scene without changing its data geometry") { fx =>
    val (host, _)                            = hostIn(fx)
    def resizeTo(w: Double, h: Double): Unit =
      runOnFx {
        host.setPrefSize(w, h)
        host.setMaxSize(Region.USE_PREF_SIZE, Region.USE_PREF_SIZE)
      }
      fx.awaitLayout()
    runOnFx(host.setOutputScaleOverride(Some(2.0)))
    resizeTo(480, 320)
    runOnFx(host.show(reference(Theme.Light)))
    val before = awaitFrame(host, 2.0)
    resizeTo(900, 500)
    val after = awaitStatus(host) {
      case PlotHostStatus.Drawn(f) => f.surface != before.surface
      case _                       => false
    } match
      case PlotHostStatus.Drawn(f) => f
      case other                   => fail(s"not drawn: $other")

    assertEquals(
      List(before, after).map(f => (f.surface.logicalWidth, f.surface.logicalHeight)),
      List((480.0, 320.0), (900.0, 500.0))
    )
    assertEquals(after.sceneId, before.sceneId)
    assertEquals(after.transform.panel, before.transform.panel)
    List(before, after).foreach(assertSharedTransform)
    // The same data point moved on the canvas and still maps back to itself.
    ReferenceScene.marks.foreach { mark =>
      val b = right(before.transform.dataToCanvas(mark.at))
      val a = right(after.transform.dataToCanvas(mark.at))
      assertNotEquals(a, b)
      assertEquals(
        runOnFx(host.transform.map(_.canvasToData(a))).map(p => (p.x, p.y)).map { (x, y) =>
          (math.rint(x * 1e6) / 1e6, math.rint(y * 1e6) / 1e6)
        },
        Some((mark.at.x, mark.at.y))
      )
    }
    runOnFx(host.dispose())
  }

  // ---------------------------------------------------------------------------
  // Threading, failure and disposal
  // ---------------------------------------------------------------------------

  fxStage.test("compiles off the FX thread and draws on it") { fx =>
    val threads             = ConcurrentLinkedQueue[Boolean]()
    val recording: Executor = task =>
      CanvasPlotHost.sharedCompiler.execute { () =>
        threads.add(Platform.isFxApplicationThread)
        task.run()
      }
    val host = runOnFx(CanvasPlotHost(recording))
    fx.show(runOnFx(StackPane(host)))
    runOnFx {
      host.setOutputScaleOverride(Some(1.0))
      host.show(reference(Theme.Light))
    }
    awaitFrame(host, 1.0)
    assert(!threads.isEmpty)
    assert(threads.asScala.forall(!_), "a compile ran on the FX thread")
    intercept[IllegalStateException](host.show(reference(Theme.Light)))
    runOnFx(host.dispose())
  }

  fxStage.test("a scene that cannot be laid out fails with a typed error and a blank canvas") {
    fx =>
      val (host, _) = hostIn(fx)
      val id        = right(SceneId("studio.test.empty-panel"))
      val viewport  = Viewport.unsafe(
        origin = Point.npcUnsafe(0.1, 0.1),
        size = Size.npcUnsafe(0.0, 0.5),
        xScale = Interval.unsafe(0.0, 1.0),
        yScale = Interval.unsafe(0.0, 1.0)
      )
      val flat  = right(DataPanel(id, viewport))
      val scene = right(
        PlotScene(id, Scene(Vector(Grob.group(Vector.empty, Some(viewport)))), flat)
      )
      runOnFx {
        host.setOutputScaleOverride(Some(1.0))
        host.show(scene)
      }
      awaitStatus(host)(_.isInstanceOf[PlotHostStatus.Failed]) match
        case PlotHostStatus.Failed(CanvasPlotError.Scene(e: PlotSceneError.EmptyPanel)) =>
          assertEquals(e.sceneId, "studio.test.empty-panel")
          assert(e.message.contains("studio.test.empty-panel"))
        case other => fail(s"unexpected $other")
      assertEquals(runOnFx(host.frame), None)
      runOnFx(host.dispose())
  }

  fxStage.test("dispose detaches the host, releases the canvas and ignores later scenes") {
    fx =>
      val (host, root) = hostIn(fx)
      runOnFx {
        host.setOutputScaleOverride(Some(1.0))
        host.show(reference(Theme.Light))
      }
      awaitFrame(host, 1.0)
      runOnFx {
        host.dispose()
        host.dispose()
        host.show(reference(Theme.Dark))
      }
      assertEquals(runOnFx(host.status.get), PlotHostStatus.Disposed)
      assertEquals(runOnFx(host.getChildrenUnmodifiable.size), 0)
      runOnFx(root.getChildren.remove(host))
  }

  fxStage.test("a superseded compile is not drawn; the shown frame stays until the next") {
    fx =>
      val queue           = ConcurrentLinkedQueue[Runnable]()
      val gated: Executor = task => queue.add(task): Unit
      def runNext(): Unit =
        Option(queue.poll()).getOrElse(fail("no compile queued")).run()
        runOnFx(()) // the delivery was queued on the FX thread before this
      val host = runOnFx {
        val h = CanvasPlotHost(gated)
        h.setMaxSize(800, 600) // 3x remains inside the aggregate texture budget.
        h
      }
      fx.show(runOnFx(StackPane(host)))
      runOnFx {
        host.setOutputScaleOverride(Some(1.0))
        host.show(reference(Theme.Light))
      }
      runNext()
      val first = runOnFx(host.frame).getOrElse(fail("the first frame was not drawn"))
      assertEquals(first.surface.deviceScale, 1.0)

      // Two changes while a compile is in flight start one compile, not two.
      runOnFx {
        host.setOutputScaleOverride(Some(2.0))
        host.setOutputScaleOverride(Some(3.0))
      }
      assertEquals(queue.size, 1)
      assert(runOnFx(host.status.get).isInstanceOf[PlotHostStatus.Compiling])
      assertEquals(runOnFx(host.frame), Some(first))

      // The in-flight compile is for 2x, which is stale: it is dropped and 3x starts.
      runNext()
      assertEquals(runOnFx(host.frame), Some(first))
      assertEquals(queue.size, 1)
      runNext()
      assertEquals(runOnFx(host.frame).map(_.surface.deviceScale), Some(3.0))
      assertEquals(queue.size, 0)
      runOnFx(host.dispose())
  }

  fxStage.test("a surface too large to draw fails with a typed error, not Waiting") { fx =>
    assumeFullStage(fx)
    val (host, _) = hostIn(fx)
    runOnFx {
      host.setOutputScaleOverride(Some(6.0)) // 1440 x 6 = 8640 device pixels
      host.show(reference(Theme.Light))
    }
    awaitStatus(host)(_.isInstanceOf[PlotHostStatus.Failed]) match
      case PlotHostStatus.Failed(CanvasPlotError.Surface(e: PlotSceneError.InvalidSurface)) =>
        assertEquals((e.logicalWidth, e.logicalHeight, e.deviceScale), (1440.0, 900.0, 6.0))
        assert(e.message.contains("1440.0x900.0"), e.message)
      case other => fail(s"unexpected $other")
    assertEquals(runOnFx(host.frame), None)
    runOnFx(host.setOutputScaleOverride(Some(1.0)))
    awaitFrame(host, 1.0)
    runOnFx(host.dispose())
  }

  // A scene with one raster of `side` x `side` pixels in its data panel.
  private def rasterScene(id: SceneId, side: Int): PlotScene =
    val viewport = Viewport.unsafe(
      xScale = Interval.unsafe(0.0, 1.0),
      yScale = Interval.unsafe(0.0, 1.0)
    )
    val image = RasterImage.tabulate(RasterDimensions.unsafe(side, side))((x, y) =>
      Rgba32.unsafe(x * 40, y * 40, 0)
    )
    val grob = Grob.imageUnsafe(image, Point.npcUnsafe(0.5, 0.5), Size.npcUnsafe(0.5, 0.5))
    right(
      PlotScene(
        id,
        Scene(Vector(Grob.group(Vector(grob), Some(viewport)))),
        right(DataPanel(id, viewport))
      )
    )

  fxStage.test("a new scene value under the same id gets fresh renderer caches") { fx =>
    val adapters = ConcurrentLinkedQueue[RecordingContext]()
    val host     = runOnFx(
      CanvasPlotHost(
        CanvasPlotHost.sharedCompiler,
        _ => { val r = RecordingContext(); adapters.add(r); r }
      )
    )
    fx.show(runOnFx(StackPane(host)))
    val id     = right(SceneId("studio.test.raster"))
    val first  = rasterScene(id, 2)
    val second = rasterScene(id, 3)
    runOnFx {
      host.setOutputScaleOverride(Some(1.0))
      host.show(first)
    }
    awaitFrame(host, 1.0)
    runOnFx(host.show(second))
    awaitStatus(host) {
      case PlotHostStatus.Drawn(f) => f.plan.scene eq second.scene
      case _                       => false
    }
    // Showing the same value again redraws with the same caches.
    runOnFx(host.show(second))
    awaitStatus(host)(_.isInstanceOf[PlotHostStatus.Drawn])
    def images(r: RecordingContext) =
      r.log.filter(_.startsWith("drawImage(")).map(_.takeWhile(_ != ',')).distinct
    assertEquals(
      adapters.asScala.toList.map(images),
      List(Vector("drawImage(2x2"), Vector("drawImage(3x3"))
    )
    runOnFx(host.dispose())
  }

  // ---------------------------------------------------------------------------
  // Leaks
  // ---------------------------------------------------------------------------

  fxStage.test("1,000 open/close cycles retain no host, canvas or listener") { fx =>
    val container = runOnFx(StackPane())
    fx.show(container)
    val scene  = reference(Theme.Light)
    val cycles = 1000
    val refs   = (1 to cycles).map { i =>
      val host = runOnFx {
        val host = CanvasPlotHost()
        container.getChildren.add(host)
        // Alternate the window's scale and an override, so both paths are cycled.
        if i % 2 == 0 then host.setOutputScaleOverride(Some(2.0))
        host.show(scene)
        host
      }
      // Every third host is disposed with its compile still in flight.
      if i % 3 != 0 then awaitStatus(host)(_.isInstanceOf[PlotHostStatus.Drawn]): Unit
      runOnFx {
        host.dispose()
        container.getChildren.remove(host)
      }
      WeakReference(host)
    }
    // Let the compiles of hosts disposed mid-flight deliver (and be ignored).
    val drained = CountDownLatch(1)
    CanvasPlotHost.sharedCompiler.execute(() => drained.countDown())
    assert(drained.await(TimeoutMillis, TimeUnit.MILLISECONDS))
    fx.awaitLayout() // a pulse releases the removed nodes' peers

    def alive    = refs.count(_.get != null)
    val deadline = System.currentTimeMillis + CollectMillis
    while alive > 0 && System.currentTimeMillis < deadline do
      System.gc()
      Thread.sleep(20)
      fx.awaitLayout()
    assertEquals(alive, 0, s"$alive of $cycles disposed hosts are still reachable")
  }

  fxStage.test(
    "the software renderer accepts the byte boundary and refuses 8192-square before allocation"
  ) { fx =>
    val host = runOnFx {
      val h = CanvasPlotHost()
      h.setManaged(false)
      h.setOutputScaleOverride(Some(1.0))
      h.resize(4096, 2730)
      h
    }
    fx.show(runOnFx(StackPane(host)))
    runOnFx(host.show(reference(Theme.Light)))
    val frame = awaitFrame(host, 1.0)
    assertEquals(frame.surface.canvasTextureBytes, 4096L * 2730 * 12)
    val compiled = runOnFx(host.profile.compiles)
    runOnFx(host.resize(8192, 8192))
    awaitStatus(host)(_.isInstanceOf[PlotHostStatus.Failed]) match
      case PlotHostStatus.Failed(
            CanvasPlotError.Surface(error: PlotSceneError.CanvasTextureBudget)
          ) =>
        assertEquals(error.requiredBytes, 805306368L)
        assertEquals(error.budgetBytes, PlotSurface.MaxCanvasTextureBytes)
      case other => fail(other.toString)
    assertEquals(runOnFx(host.profile.compiles), compiled)
    assertEquals(runOnFx(host.canvasTexture), (0, 0))
    assertEquals(runOnFx(host.frame), None)
    runOnFx(host.resize(800, 600))
    awaitFrame(host, 1.0)
    runOnFx(host.dispose())
  }
