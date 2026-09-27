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
import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.{CompletableFuture, CountDownLatch, ExecutionException, TimeUnit}
import javafx.animation.AnimationTimer
import javafx.application.Platform
import javafx.scene.image.{PixelFormat, WritableImage}
import javafx.scene.layout.StackPane
import javafx.scene.shape.Rectangle
import javafx.scene.transform.Transform
import javafx.scene.{Parent, Scene, SnapshotParameters}
import javafx.stage.Stage
import javax.imageio.ImageIO
import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

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
  *
  * '''Render failures fail the test.''' JavaFX catches a failure during a pulse
  * or a render job and only prints it, so a test could pass while its scene
  * failed to draw. Every test here ends by letting the toolkit finish two
  * pulses and the render jobs they queued, and fails, with each stack trace, if
  * [[RenderFailures]] recorded anything on a JavaFX thread during the test. A
  * failure recorded after a test settled fails the next test, which names the
  * test it followed: none may pass unnoticed. A test that provokes a render
  * failure on purpose carries the [[StudioFxSuite.RenderFailureExpected]] tag
  * and asserts on [[takeRenderFailures]] itself. What JavaFX reports through
  * `PlatformLogger` or `System.Logger` instead of a stack trace (CSS parse and
  * lookup warnings, image loading errors) is not captured.
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

  /** Lets queued pulses and render jobs finish, then removes and returns what
    * JavaFX reported on its threads. For tests tagged
    * [[StudioFxSuite.RenderFailureExpected]].
    */
  protected def takeRenderFailures(): Vector[RenderFailure] =
    StudioFxSuite.settle()
    RenderFailures.drain()

  override def munitTestTransforms: List[TestTransform] =
    super.munitTestTransforms :+ TestTransform(
      "render failures",
      test =>
        val expected = test.tags.contains(StudioFxSuite.RenderFailureExpected)
        test.withBody { () =>
          // Recorded after the previous test settled: its failures, whatever
          // this test's tag, reported here because that test has finished.
          val before   = RenderFailures.drain()
          val previous =
            StudioFxSuite.previousTest.getAndSet(s"${getClass.getName}.${test.name}")
          if before.nonEmpty then
            val owner = Option(previous).fold("before the first test")(p => s"after $p ended")
            Future.failed(StudioFxSuite.renderFailed(test, before, owner))
          else
            test
              .body()
              .transformWith { outcome =>
                // A settle that times out joins the test's own failure, never replaces it.
                val verdict: Option[Throwable] = Try(takeRenderFailures()) match
                  case Failure(settleFailed)                         => Some(settleFailed)
                  case Success(during) if expected || during.isEmpty => None
                  case Success(during)                               =>
                    Some(StudioFxSuite.renderFailed(test, during, "during this test"))
                (outcome, verdict) match
                  case (_, None)                => Future.fromTry(outcome)
                  case (Success(_), Some(fail)) => Future.failed(fail)
                  case (Failure(e), Some(fail)) =>
                    e.addSuppressed(fail)
                    Future.failed(e)
              }(using munitExecutionContext)
        }
    )

  /** Requires the scene to have its full [[stageSize]].
    *
    * A window manager clamps a stage to the display, so on a display smaller than
    * the stage the scene is smaller than requested. A test whose assertions
    * depend on the full size (board text, row metrics, device-pixel limits) calls
    * this first. It fails, unless the environment variable
    * `EYES4S_STUDIO_SMALL_DISPLAY` is `skip`, when it skips the test instead: the
    * hosted macOS runner sets that (its display is 1024x768), and the Linux job,
    * whose virtual display is 1920x1200, runs every such test.
    */
  protected def assumeFullStage(fx: FxStage)(using munit.Location): Unit =
    val (w, h)   = fx.sceneSize
    val expected = (stageSize.width.toDouble, stageSize.height.toDouble)
    val message  =
      s"the display clamped the ${stageSize.width}x${stageSize.height} stage to a ${w}x$h scene"
    if sys.env.get(StudioFxSuite.SmallDisplayVariable).contains("skip") then
      assume((w, h) == expected, message)
    else assertEquals((w, h), expected, message)

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

  // The last test that began, as `suite.test`, or null before the first.
  private[harness] val previousTest = AtomicReference[String](null)

  /** Marks a test that provokes a render failure on purpose; it asserts on
    * `takeRenderFailures()` instead of failing on the capture.
    */
  val RenderFailureExpected: munit.Tag = munit.Tag("RenderFailureExpected")

  /** The failure of `test` for render failures recorded `when`. */
  def renderFailed(
      test: munit.Test,
      failures: Vector[RenderFailure],
      when: String
  ): munit.FailException =
    val traces = failures.zipWithIndex
      .map((f, i) => s"[${i + 1}/${failures.size}] ${f.describe}")
      .mkString("\n")
    munit.FailException(
      s"JavaFX reported ${failures.size} render failure(s) $when; " +
        s"tag the test RenderFailureExpected if that is intended:\n$traces",
      failures.head.error,
      test.location
    )

  /** Waits for two more pulses, so the first one's paint is queued, then
    * renders a node synchronously. The render thread runs one job at a time,
    * so that render finishes after every paint queued before it.
    */
  private[harness] def settle(): Unit =
    val pulses = CountDownLatch(2)
    val timer  = runOnFx {
      val timer = new AnimationTimer:
        def handle(now: Long): Unit = pulses.countDown()
      timer.start()
      Platform.requestNextPulse()
      timer
    }
    try
      if !pulses.await(TimeoutSeconds, TimeUnit.SECONDS) then
        throw AssertionError(s"no two pulses within $TimeoutSeconds s")
    finally runOnFx(timer.stop())
    runOnFx(Rectangle(1.0, 1.0).snapshot(null, null)): Unit

  /** Set to `skip` where the display cannot hold a board-size stage. */
  val SmallDisplayVariable = "EYES4S_STUDIO_SMALL_DISPLAY"

  /** Where snapshots are written; the build points it at `target/studio-snapshots`. */
  val snapshotRoot: Path =
    Paths.get(sys.props.getOrElse("eyes4s.studio.snapshots", "target/studio-snapshots"))

  private lazy val started: Unit =
    RenderFailures.installStream()
    val ready = CountDownLatch(1)
    try
      Platform.startup { () =>
        RenderFailures.installOnFxThread()
        ready.countDown()
      }
    catch
      case _: IllegalStateException => // already running
        Platform.runLater { () =>
          RenderFailures.installOnFxThread()
          ready.countDown()
        }
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
