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

package eyes4s.studio.desktop.harness

import java.awt.image.BufferedImage
import java.nio.file.{Files, Path, Paths}
import java.util.concurrent.{CompletableFuture, CountDownLatch, ExecutionException, TimeUnit}
import javafx.application.Platform
import javafx.scene.image.{PixelFormat, WritableImage}
import javafx.scene.layout.StackPane
import javafx.scene.transform.Transform
import javafx.scene.{Parent, Scene, SnapshotParameters}
import javafx.stage.Stage
import javax.imageio.ImageIO

/** The size of a test stage's scene, in logical pixels. */
final case class StageSize(width: Int, height: Int)

object StageSize:
  /** The board size every design reference is drawn at (DESIGN_SPEC section 11). */
  val Board: StageSize = StageSize(1440, 900)

/** A snapshot theme: the style classes on the scene root (DESIGN_SPEC section 4). */
enum StudioTheme(val id: String, val styleClasses: List[String]):
  case Light extends StudioTheme("light", List("es"))
  case Dark  extends StudioTheme("dark", List("es", "dark"))

/** A snapshot raster scale: 1 for standard displays, 2 for HiDPI. */
enum SnapshotScale(val factor: Int):
  case X1 extends SnapshotScale(1)
  case X2 extends SnapshotScale(2)

/** Base for JavaFX tests of studio-desktop.
  *
  * The toolkit starts once per forked test JVM. Each `fxStage` test gets a
  * shown stage of [[stageSize]], a [[StudioRobot]] for clicks and keys, and
  * snapshot writing to `<snapshots>/<suite>/<test>/<theme>-<scale>x.png`, where
  * `<snapshots>` is the `eyes4s.studio.snapshots` property set by the build
  * (`target/studio-snapshots`).
  *
  * CI runs these tests on Linux under `xvfb-run` with the software pipeline, and
  * on macOS as functional tests only: golden comparisons, when added, are
  * Linux-only.
  */
abstract class StudioFxSuite extends munit.FunSuite:

  /** The scene size of each test stage. */
  protected def stageSize: StageSize = StageSize.Board

  override def beforeAll(): Unit =
    super.beforeAll()
    StudioFxSuite.startToolkit()

  /** A shown stage per test, closed afterwards whatever the outcome. */
  protected val fxStage: FunFixture[FxStage] = FunFixture[FxStage](
    setup =
      test => FxStage.open(StudioFxSuite.segment(getClass.getSimpleName), test.name, stageSize),
    teardown = _.close()
  )

  /** Runs `body` on the FX application thread and returns its result. */
  protected def runOnFx[A](body: => A): A = StudioFxSuite.runOnFx(body)

/** One shown test stage and the operations a test performs on it. */
final class FxStage private (
    val stage: Stage,
    val scene: Scene,
    suiteSegment: String,
    testSegment: String
):
  val robot: StudioRobot = StudioRobot(this)

  /** Runs `body` on the FX application thread and returns its result. */
  def runOnFx[A](body: => A): A = StudioFxSuite.runOnFx(body)

  /** Replaces the scene root and waits for it to be laid out. */
  def show(root: Parent): Unit =
    runOnFx(scene.setRoot(root))
    awaitLayout()

  /** Applies CSS and layout now, then waits for the layout pass of the next pulse.
    *
    * It returns from a post-layout pulse listener, so that pulse may not have
    * rendered yet; snapshots render synchronously and do not need it to.
    */
  def awaitLayout(): Unit =
    val pulsed = CountDownLatch(1)
    runOnFx {
      scene.getRoot.applyCss()
      scene.getRoot.layout()
      lazy val listener: Runnable = () =>
        scene.removePostLayoutPulseListener(listener)
        pulsed.countDown()
      scene.addPostLayoutPulseListener(listener)
      Platform.requestNextPulse()
    }
    if !pulsed.await(StudioFxSuite.TimeoutSeconds, TimeUnit.SECONDS) then
      throw AssertionError(s"no layout pulse within ${StudioFxSuite.TimeoutSeconds} s")

  /** The scene's size in logical pixels. */
  def sceneSize: (Double, Double) = runOnFx((scene.getWidth, scene.getHeight))

  /** Renders the scene root in `theme` at each scale and writes one PNG per scale. */
  def snapshot(
      theme: StudioTheme,
      scales: List[SnapshotScale] = List(SnapshotScale.X1, SnapshotScale.X2)
  ): List[Path] =
    runOnFx {
      val classes = scene.getRoot.getStyleClass
      StudioTheme.values.foreach(t => classes.removeAll(t.styleClasses*))
      classes.addAll(theme.styleClasses*)
    }
    awaitLayout()
    scales.map { scale =>
      val image = runOnFx {
        val k      = scale.factor.toDouble
        val params = SnapshotParameters()
        params.setTransform(Transform.scale(k, k))
        params.setFill(scene.getFill)
        val target = WritableImage(
          math.ceil(scene.getWidth * k).toInt,
          math.ceil(scene.getHeight * k).toInt
        )
        scene.getRoot.snapshot(params, target)
      }
      val file = StudioFxSuite.snapshotRoot
        .resolve(suiteSegment)
        .resolve(testSegment)
        .resolve(s"${theme.id}-${scale.factor}x.png")
      StudioFxSuite.writePng(image, file)
      file
    }

  private[harness] def close(): Unit = runOnFx(stage.close())

object FxStage:
  private[harness] def open(suite: String, test: String, size: StageSize): FxStage =
    StudioFxSuite.runOnFx {
      val stage = Stage()
      val scene = Scene(StackPane(), size.width.toDouble, size.height.toDouble)
      stage.setTitle(s"$suite · $test")
      stage.setScene(scene)
      stage.setResizable(false)
      stage.show()
      FxStage(stage, scene, suite, StudioFxSuite.segment(test))
    }

object StudioFxSuite:
  private[harness] val TimeoutSeconds = 30L

  /** Where snapshots are written; the build points it at `target/studio-snapshots`. */
  val snapshotRoot: Path =
    Paths.get(sys.props.getOrElse("eyes4s.studio.snapshots", "target/studio-snapshots"))

  private lazy val started: Unit =
    val ready = CountDownLatch(1)
    try Platform.startup(() => ready.countDown())
    catch case _: IllegalStateException => ready.countDown() // already running
    Platform.setImplicitExit(false)
    if !ready.await(TimeoutSeconds, TimeUnit.SECONDS) then
      throw AssertionError(s"JavaFX toolkit did not start within $TimeoutSeconds s")

  /** Starts the JavaFX toolkit once per JVM. */
  def startToolkit(): Unit = started

  /** Runs `body` on the FX application thread, rethrowing its failure here. */
  def runOnFx[A](body: => A): A =
    startToolkit()
    if Platform.isFxApplicationThread then body
    else
      val result = CompletableFuture[A]()
      Platform.runLater { () =>
        try result.complete(body): Unit
        catch case t: Throwable => result.completeExceptionally(t): Unit
      }
      try result.get(TimeoutSeconds, TimeUnit.SECONDS)
      catch case e: ExecutionException => throw e.getCause

  /** A file-name-safe path segment for a suite or test name. */
  def segment(name: String): String =
    name.trim.replaceAll("[^A-Za-z0-9._-]+", "-").replaceAll("^-+|-+$", "") match
      case "" => "unnamed"
      case s  => s

  private[harness] def writePng(image: WritableImage, file: Path): Unit =
    val w      = image.getWidth.toInt
    val h      = image.getHeight.toInt
    val pixels = new Array[Int](w * h)
    image.getPixelReader.getPixels(0, 0, w, h, PixelFormat.getIntArgbInstance, pixels, 0, w)
    val buffered = BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
    buffered.setRGB(0, 0, w, h, pixels, 0, w)
    Files.createDirectories(file.getParent)
    if !ImageIO.write(buffered, "png", file.toFile) then
      throw AssertionError(s"no PNG writer available for $file")
