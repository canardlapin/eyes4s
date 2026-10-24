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
import eyes4s.studio.app.analysis.DesignPreview
import eyes4s.studio.app.appearance.Appearance
import eyes4s.studio.app.compare.SummaryAnswer
import eyes4s.studio.app.figures.{PanelBody, TileMap}
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.{FxStage, StudioFxSuite, StudioTheme, VisualPixels}
import eyes4s.studio.desktop.plot.PlotTwinStatus
import eyes4s.studio.desktop.trial.TrialViewStatus
import eyes4s.studio.viz.trial.FrameArt
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}
import java.security.MessageDigest
import scala.concurrent.duration.Duration

/** Full JavaFX regression matrix: 5 perspectives × 3 story moments × 2 themes.
  * Browser references remain human semantic reviews. Pixels are compared only
  * on Linux Monocle at native scale1; actual GTK2 remains its independent court.
  * Missing baselines fail after captures/receipts are written; no auto-promotion.
  */
class VisualParitySuite extends StoryCaptureFixture:
  override val munitTimeout: Duration = Duration(180, "s")
  private val buildRoot: Path         =
    val in = getClass.getClassLoader.getResourceAsStream("eyes4s/studio/desktop/build-root.txt")
    try Paths.get(String(in.readAllBytes(), UTF_8).trim)
    finally in.close()
  private val candidateFingerprint: String =
    val process = new ProcessBuilder(
      "python3",
      buildRoot.resolve("tools/studio-visual-parity/candidate.py").toString,
      buildRoot.toString
    )
      .directory(buildRoot.toFile)
      .redirectError(ProcessBuilder.Redirect.INHERIT)
      .start()
    val output = String(process.getInputStream.readAllBytes(), UTF_8).trim
    assertEquals(process.waitFor(), 0)
    assert(output.matches("[0-9a-f]{64}"), s"Invalid visual candidate fingerprint: $output")
    output
  private def hash(p: Path): String = MessageDigest
    .getInstance("SHA-256")
    .digest(Files.readAllBytes(p))
    .map(b => f"${b & 255}%02x")
    .mkString
  private def relative(p: Path): String = buildRoot.relativize(p).toString.replace('\\', '/')

  private val moments =
    Vector(("t1", StoryMoment.T1), ("t2", StoryMoment.T2), ("t3", StoryMoment.T3))
  private def seed(moment: StoryMoment, p: Perspective): AppModel =
    val base = moment match
      case StoryMoment.T1 => StoryModels.t1Data
      case StoryMoment.T2 =>
        p match
          case Perspective.Explore  => StoryModels.t2Explore
          case Perspective.Analysis => StoryModels.t2Analysis
          case Perspective.Figures  => StoryModels.t2Figures
          case _                    => StoryModels.t2Compare
      case StoryMoment.T3 => StoryModels.t3Summary
    AppModel.update(base, Intent.SwitchPerspective(p))._1

  private def ready(fx: FxStage, w: StudioWindow, moment: StoryMoment, p: Perspective): Unit =
    eventually(fx, "summary, queries and all requested report replies") {
      val s = w.summary.summary
      s.run.isEmpty || (s.summary.nonEmpty && s.queries.nonEmpty && (s.summary match
        case Some(SummaryAnswer.Answered(r)) => s.reports.size == r.scales.size * 2
        case _                               => true))
    }
    if p == Perspective.Data then
      eventually(fx, "source form bytes and geometry thumbnails") {
        w.columnMapping.loaded.isDone && w.geometry.shownPictures.nonEmpty
      }
    if p == Perspective.Analysis then
      eventually(fx, "resolved preview is terminal") {
        w.resolvedDesign.state.preview match
          case DesignPreview.Preparing | DesignPreview.Counting(_, _, _, _) => false
          case _                                                            => true
      }
    if moment == StoryMoment.T2 && p == Perspective.Explore then
      eventually(fx, "stored stimulus, preview map and timeline") {
        (w.explore.trialView.status.get match
          case TrialViewStatus.Shown(s) =>
            s.map.nonEmpty && s.frameArt.isInstanceOf[FrameArt.Picture]
          case _ => false
        ) && w.timeline.vm.enabled
      }
    if moment == StoryMoment.T2 && p == Perspective.Compare then
      eventually(fx, "stored Compare maps and trial panels") {
        Vector(w.summary.panels.queryView, w.summary.panels.referenceView)
          .forall(_.status.get match
            case TrialViewStatus.Shown(s) => s.map.nonEmpty
            case _                        => false)
      }
    if moment == StoryMoment.T3 && p == Perspective.Compare then
      eventually(fx, "Results plot and24overall participant rows") {
        w.summary.participantPlot.status.get
          .isInstanceOf[PlotTwinStatus.Shown] && w.summary.participants.rows.size == 24
      }
    if p == Perspective.Figures then
      eventually(fx, "figure panel inputs and density grids are terminal") {
        w.figures.vm.page.forall(_.panels.forall(_.body match
          case PanelBody.Waiting(_) => false
          case PanelBody.Maps(m)    => m.tiles.forall(_.map != TileMap.Waiting)
          case _                    => true)) && w.figures.vm.methods.forall(m =>
          !m.text.fold(_.startsWith("Reading "), _ => false)
        )
      }
      if moment != StoryMoment.T1 then
        eventually(fx, "figure gaze assets and participant plot") {
          w.figures.vm.page.forall(page =>
            page.panels.forall(panel =>
              panel.body match
                case PanelBody.Gaze(_) =>
                  w.figures
                    .gaze(page.figure, panel.letter)
                    .exists(_.status.get match
                      case TrialViewStatus.Shown(s) =>
                        s.frameArt match
                          case _: FrameArt.Picture | FrameArt.ScreenWithCross => true
                          case _                                              => false
                      case _ => false)
                case PanelBody.Plot(_) =>
                  w.figures
                    .plot(page.figure, panel.letter)
                    .exists(_.status.get.isInstanceOf[PlotTwinStatus.Shown])
                case _ => true
            )
          )
        }
    RestingFocus.clear(fx, w)
    assertEquals(takeRenderFailures().map(_.toString), Vector.empty[String])

  for (tag, moment) <- moments; p <- Perspective.values; theme <- StudioTheme.values do
    val name = s"$tag-${p.label}-${theme.id}"
    fxStage.test(name) { fx =>
      val model = AppModel
        .update(
          seed(moment, p),
          Intent.SetAppearance(if theme == StudioTheme.Dark then Appearance.Dark
          else Appearance.Light)
        )
        ._1
      val w = boot(
        fx,
        model,
        moment,
        panels = golden,
        project = Some(fixtureProject),
        clock = () => None
      )
      assertEquals(fx.sceneSize, (1440.0, 900.0))
      ready(fx, w, moment, p)
      val first        = fx.snapshot(theme)
      val before       = first.map(VisualPixels.read)
      val beforeHashes = first.map(hash)
      assertEquals(takeRenderFailures().map(_.toString), Vector.empty[String])
      val actual      = fx.snapshot(theme)
      val nativeScale = runOnFx(fx.stage.getOutputScaleX)
      val compare     = System
        .getProperty("os.name")
        .toLowerCase
        .contains("linux") && StudioFxSuite.headless && nativeScale == 1.0
      val problems = Vector.newBuilder[String]
      val captures = actual.zipWithIndex.map { (file, index) =>
        val image  = VisualPixels.read(file)
        val repeat = VisualPixels.compare(before(index), image).fold(e => fail(e), identity)
        if !repeat.accepted then
          problems += s"$name, ${file.getFileName}: unstable repeat $repeat"
        val baseline = buildRoot.resolve(
          s"studio/desktop/src/test/resources/eyes4s/studio/desktop/visual-parity/$name/${file.getFileName}"
        )
        var comparison: ujson.Value   = ujson.Null
        var baselineHash: ujson.Value = ujson.Null
        if compare then
          if !Files.isRegularFile(baseline) then
            problems += s"Missing baseline $baseline; review the uploaded capture before adding it."
          else
            baselineHash = ujson.Str(hash(baseline))
            VisualPixels.compare(VisualPixels.read(baseline), image) match
              case Left(e)     => problems += s"$baseline: $e"
              case Right(diff) =>
                comparison = diff.json
                if !diff.accepted then
                  val diffFile = file.resolveSibling(
                    file.getFileName.toString.stripSuffix(".png") + "-diff.png"
                  )
                  VisualPixels.writeDiff(VisualPixels.read(baseline), image, diffFile)
                  problems += s"$baseline versus $file: $diff; diff at $diffFile"
        ujson.Obj(
          "actual"            -> relative(file),
          "actualSha256"      -> hash(file),
          "width"             -> image.getWidth,
          "height"            -> image.getHeight,
          "baseline"          -> relative(baseline),
          "baselineSha256"    -> baselineHash,
          "comparison"        -> comparison,
          "repeatFirstSha256" -> beforeHashes(index),
          "repeat"            -> repeat.json
        )
      }
      val errors  = problems.result()
      val receipt = ujson.Obj(
        "candidateFingerprint" -> candidateFingerprint,
        "osName"               -> System.getProperty("os.name"),
        "osArch"               -> System.getProperty("os.arch"),
        "javaVersion"          -> System.getProperty("java.version"),
        "javaFxVersion"        -> System.getProperty("javafx.runtime.version", "unknown"),
        "glassPlatform"        -> System.getProperty("glass.platform", "platform-default"),
        "prismOrder"           -> System.getProperty("prism.order", "default"),
        "schema"               -> 1,
        "state"                -> name,
        "moment"               -> tag,
        "perspective"          -> p.label,
        "theme"                -> theme.id,
        "nativeScaleX"         -> nativeScale,
        "nativeScaleY"         -> runOnFx(fx.stage.getOutputScaleY),
        "mode"  -> (if compare then "linux-monocle-goldens" else "functional-only"),
        "focus" -> "programmatic scene root; no keyboard ring; no active text caret",
        "maximumChannelDelta"    -> VisualPixels.MaximumChannelDelta,
        "maximumChangedFraction" -> VisualPixels.MaximumChangedFraction,
        "sourceSha256"           -> hash(
          buildRoot.resolve(
            "studio/desktop/src/test/scala/eyes4s/studio/desktop/shell/VisualParitySuite.scala"
          )
        ),
        "captures" -> ujson.Arr.from(captures),
        "problems" -> ujson.Arr.from(errors)
      )
      Files.writeString(
        actual.head.getParent.resolve("review.json"),
        ujson.write(receipt, indent = 2) + "\n"
      )
      assert(errors.isEmpty, errors.mkString("\n"))
    }
