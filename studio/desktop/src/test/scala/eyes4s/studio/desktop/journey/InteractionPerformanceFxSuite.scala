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

package eyes4s.studio.desktop.journey

import cats.effect.unsafe.implicits.global
import eyes4s.codec.ByteDigest
import eyes4s.plan.AdmissionDecision as NativeAdmissionDecision
import eyes4s.studio.app.{AppModel, Intent, Notice}
import eyes4s.studio.app.analysis.{DesignPreview, DesignRows}
import eyes4s.studio.app.compare.SummaryAnswer
import eyes4s.studio.app.figures.PanelBody
import eyes4s.studio.app.geometry.Loading
import eyes4s.studio.app.nav.{DataSection, Location, Place}
import eyes4s.studio.core.backend.{Phase, TrialKey}
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.{CoreBinding, Perspective, Preset, SourceRole}
import eyes4s.studio.core.selection.*
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.explore.NavigatorDisplays
import eyes4s.studio.desktop.harness.{StudioFxSuite, StudioTheme}
import eyes4s.studio.desktop.platform.TempDirs
import eyes4s.studio.desktop.runtime.DatasetSourceHosts
import eyes4s.studio.desktop.trial.TrialViewStatus
import eyes4s.studio.viz.trial.FrameArt
import io.circe.Json
import com.sun.javafx.tk.{RenderJob, Toolkit}
import java.util.concurrent.{CountDownLatch, TimeUnit}
import java.lang.management.ManagementFactory
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Paths}
import scala.concurrent.duration.*
import scala.collection.mutable.ArrayBuffer

/** Explicit S10.6 court: EYES4S_STUDIO_PERFORMANCE=true. Ordinary tests do not
  * silently substitute a shared CI runner for the named performance machine.
  */
class InteractionPerformanceFxSuite extends GoldenWindow:
  import NativeCommandJourneyFixture.*
  override val munitTimeout: Duration = 45.minutes
  private val output                  = Paths.get(
    sys.env.getOrElse("EYES4S_STUDIO_PERFORMANCE_OUTPUT", "target/studio-performance")
  )
  private val origin   = get(ViewId.of("qualification.performance"))
  private var sequence = 0L

  private def until(what: String, seconds: Int = 30)(ready: => Boolean): Unit =
    val deadline = System.nanoTime() + seconds.seconds.toNanos
    while !runOnFx(ready) do
      if System.nanoTime() >= deadline then fail(s"Timed out: $what")
      Thread.sleep(2)

  private def expanded(factor: Int): Inputs =
    val original = load.unsafeRunSync()
    if factor == 1 then original
    else
      Inputs(original.entries.map { (source, data) =>
        val lines  = String(data.toArray, UTF_8).linesIterator.toVector
        val copied = (0 until factor).toVector.flatMap { copy =>
          lines.tail.map { line =>
            val comma = line.indexOf(',')
            require(comma > 0 && line.take(comma).matches("P[0-9]+"))
            if copy == 0 then line else s"${line.take(comma)}-copy$copy${line.drop(comma)}"
          }
        }
        val bytes = (lines.head +: copied).mkString("", "\n", "\n").getBytes(UTF_8).toVector
        source.copy(bytes = ByteDigest.sha256(IArray.from(bytes))) -> bytes
      })

  private def rows(inputs: Inputs, role: SourceRole): Int =
    String(inputs.entries.find(_._1.role == role).get._2.toArray, UTF_8).linesIterator.size - 1

  private def trialReady(w: StudioWindow, key: TrialKey): Boolean =
    w.explore.state.trial.contains(key) && w.explore.vm.shown.exists(_.trial == key) &&
      w.explore.input.targets.exists(_.targets.nonEmpty) &&
      w.timeline.vm.enabled && w.sourceRecords.current.total.nonEmpty &&
      (w.explore.trialView.status.get match
        case TrialViewStatus.Shown(s) =>
          s.map.nonEmpty && s.frameArt.isInstanceOf[FrameArt.Picture]
        case _ => false)

  private def perspectiveReady(w: StudioWindow, p: Perspective): Boolean =
    w.runtime.model.perspective == p && (p match
      case Perspective.Data =>
        w.columnMapping.loaded.isDone && w.geometry.shownPictures.nonEmpty
      case Perspective.Explore  => w.explore.state.trial.exists(trialReady(w, _))
      case Perspective.Analysis =>
        w.resolvedDesign.state.preview match
          case DesignPreview.Preparing | DesignPreview.Counting(_, _, _, _) => false
          case _                                                            => true
      case Perspective.Compare =>
        w.summary.summary.summary.exists {
          case SummaryAnswer.Answered(r) => w.summary.summary.reports.size == r.scales.size * 2
          case _                         => false
        } && w.summary.participantPlot.plot.nonEmpty
      case Perspective.Figures =>
        w.figures.vm.page.exists(page =>
          page.panels.nonEmpty && page.panels.forall(panel =>
            panel.body match
              case PanelBody.Plot(_) =>
                w.figures.plot(page.figure, panel.letter).exists(_.plot.nonEmpty)
              case _ => false
          )
        ))

  private def heap: Long = ManagementFactory.getMemoryMXBean.getHeapMemoryUsage.getUsed
  private def rss: Long  =
    val p = new ProcessBuilder("ps", "-o", "rss=", "-p", ProcessHandle.current().pid().toString)
      .start()
    val value = String(p.getInputStream.readAllBytes(), UTF_8).trim.toLong * 1024L
    assertEquals(p.waitFor(), 0)
    value

  if sys.env.get("EYES4S_STUDIO_PERFORMANCE").contains("true") then
    for factor <- Vector(1, 10) do
      fxStage.test(s"production interactions at ${factor}x public fixture") { fx =>
        assertEquals(fx.sceneSize, (1440.0, 900.0))
        val inputs   = expanded(factor)
        val original = load.unsafeRunSync()
        assertEquals(
          rows(inputs, SourceRole.Fixations),
          rows(original, SourceRole.Fixations) * factor
        )
        assertEquals(
          rows(inputs, SourceRole.Trials),
          rows(original, SourceRole.Trials) * factor
        )
        val initial           = get(AppModel.newProject)
        val (folder, cleanup) =
          TempDirs.resource(s"eyes4s-performance-$factor-").allocated.unsafeRunSync()
        val (port, release) = NativeJourneyProject
          .open(folder.resolve("study.eyes"), initial.document, inputs)
          .allocated
          .unsafeRunSync()
        val measures    = ArrayBuffer.empty[Json]
        val memory      = ArrayBuffer.empty[Json]
        var storage     = Json.obj("status" -> Json.fromString("pending"))
        val receiptFile = output.resolve(s"fixture-${factor}x.json")
        Files.createDirectories(output)
        def memoryAt(label: String): Unit = memory += Json.obj(
          "phase"        -> Json.fromString(label),
          "heapBytes"    -> Json.fromLong(heap),
          "rssBytes"     -> Json.fromLong(rss),
          "nonHeapBytes" -> Json.fromLong(
            ManagementFactory.getMemoryMXBean.getNonHeapMemoryUsage.getUsed
          )
        )
        def write(status: String): Unit =
          val receipt = Json.obj(
            "status"           -> Json.fromString(status),
            "factor"           -> Json.fromInt(factor),
            "pid"              -> Json.fromLong(ProcessHandle.current().pid()),
            "jvm"              -> Json.fromString(System.getProperty("java.runtime.version")),
            "maximumHeapBytes" -> Json.fromLong(Runtime.getRuntime.maxMemory()),
            "glassApplication" -> Json.fromString(StudioFxSuite.glassApplication),
            "requestedGraphicsPipeline" -> Json.fromString(
              System.getProperty("prism.order", "default")
            ),
            "fixationRows" -> Json.fromInt(rows(inputs, SourceRole.Fixations)),
            "trialRows"    -> Json.fromInt(rows(inputs, SourceRole.Trials)),
            "sources"      -> Json.arr(inputs.entries.map { (source, bytes) =>
              Json.obj(
                "role"   -> Json.fromString(source.role.toString),
                "sha256" -> Json.fromString(source.bytes.hex),
                "bytes"  -> Json.fromInt(bytes.size)
              )
            }*),
            "scene"        -> Json.arr(Json.fromInt(1440), Json.fromInt(900)),
            "scale"        -> Json.fromDoubleOrNull(runOnFx(fx.stage.getOutputScaleX)),
            "measurements" -> Json.arr(measures.toVector*),
            "memory"       -> Json.arr(memory.toVector*),
            "storage"      -> storage
          )
          Files.writeString(receiptFile, receipt.spaces2 + "\n"): Unit
        var window: Option[StudioWindow] = None
        try
          println(s"[performance] ${factor}x importing and admitting native inputs")
          memoryAt("before-window")
          val w = boot(
            fx,
            initial,
            project = Some(port),
            displays = NavigatorDisplays.stored(port),
            nativeSources = Some(DatasetSourceHosts.stored(port))
          )
          window = Some(w)
          assertEquals(w.session.fixture, None)
          dispatch(fx, w, Intent.Dispatch(inputs.importCommand))
          dispatch(
            fx,
            w,
            Intent.Navigate(
              Location(
                Perspective.Data,
                Vector(Place.Dataset(dataset), Place.DataView(DataSection.Admission))
              )
            )
          )
          until("admission counts", 180) {
            w.admission.state.counts.isInstanceOf[Loading.Ready[?]]
          }
          runOnFx(w.admission.view.choices(NativeAdmissionDecision.ReviewExclusions).fire())
          until("admission enabled") { !w.admission.view.admit.isDisabled }
          runOnFx(w.admission.view.admit.fire())
          until("admitted", 300) {
            w.runtime.model.document.dataset(dataset).exists(_.decision.isAdmitted)
          }
          println(s"[performance] ${factor}x preparing native analysis")
          dispatch(fx, w, Intent.SwitchPerspective(Perspective.Analysis))
          until("preset enabled") { w.recipe.enabled(Preset.EncodingRetrieval) }
          runOnFx(w.recipe.choose(Preset.EncodingRetrieval))
          until("ready native preview", 600) {
            w.resolvedDesign.state.preview.receipt.isDefined && w.preflight.runButton._2
          }
          println(s"[performance] ${factor}x executing native analysis")
          runOnFx(w.preflight.pressRun())
          until("completed native run", 1800) {
            w.runtime.model.jobs.ready.exists(_.run == run)
          }
          until("archive completion or explicit budget refusal", 300) {
            w.runtime.model.document
              .run(run)
              .exists(_.archive.isInstanceOf[CoreBinding.Bound[?]]) ||
            w.runtime.model.notice.exists(_.isInstanceOf[Notice.ArtifactsRefused])
          }
          storage = runOnFx {
            w.runtime.model.notice match
              case Some(Notice.ArtifactsRefused(_, `run`, diagnostic)) =>
                assertEquals(factor, 10, diagnostic.message)
                assertEquals(diagnostic.code, "native-artifacts.budget")
                assert(diagnostic.message.contains("density cells"), diagnostic.message)
                Json.obj(
                  "status"                   -> Json.fromString("documented-budget-refusal"),
                  "code"                     -> Json.fromString(diagnostic.code),
                  "message"                  -> Json.fromString(diagnostic.message),
                  "coldRestorationQualified" -> Json.fromBoolean(false)
                )
              case _ =>
                assert(
                  w.runtime.model.document
                    .run(run)
                    .exists(_.archive.isInstanceOf[CoreBinding.Bound[?]])
                )
                Json.obj("status" -> Json.fromString("verified-archive-stored"))
          }
          println(s"[performance] ${factor}x storage ${storage.noSpaces}")
          dispatch(fx, w, Intent.Dispatch(Command.PutReporting(reporting)))
          dispatch(fx, w, Intent.ShowRun(run))
          dispatch(fx, w, Intent.Dispatch(Command.CreateFigure(run, reporting.id, panels)))
          until("complete design rows", 180) {
            w.resolvedDesign.state.rowState == DesignRows.Complete
          }
          val expectedQueries = String(
            inputs.entries.find(_._1.role == SourceRole.Trials).get._2.toArray,
            UTF_8
          ).linesIterator.drop(1).count(_.split(',')(1) == "Retrieval")
          assertEquals(runOnFx(w.resolvedDesign.state.rows.size), expectedQueries)
          println(
            s"[performance] ${factor}x native run and all $expectedQueries design rows ready"
          )
          val keys = Vector("enc_03", "enc_04").map(TrialKey("P17", Phase.Encoding, _, 1))
          def navigate(key: TrialKey): Unit = w.runtime.dispatch(
            Intent.Navigate(
              Location(Perspective.Explore, Vector(Place.At(StudioRef.Trial(key))))
            )
          )
          keys.foreach { key =>
            runOnFx(navigate(key)); until(s"warm $key")(trialReady(w, key)); fx.awaitLayout()
          }
          def rendered(): Unit =
            fx.awaitLayout()
            val completed = CountDownLatch(1)
            // Enqueued from the FX thread after the post-layout callback: that
            // pulse has submitted its paint before this fence can be queued.
            runOnFx(Toolkit.getToolkit.addRenderJob(new RenderJob(() => completed.countDown())))
            assert(completed.await(30, TimeUnit.SECONDS), "renderer did not finish the frame")
          def measure(name: String, count: Int, limit: Double, prepare: Int => Unit = _ => ())(
              action: Int => Unit
          )(ready: Int => Boolean): Unit =
            val values = (0 until count + 10).toVector.map { i =>
              prepare(i)
              val started = System.nanoTime()
              runOnFx(action(i))
              until(name)(ready(i))
              rendered()
              val elapsed = (System.nanoTime() - started).toDouble / 1e6
              if i < 10 then None else Some(elapsed)
            }.flatten
            val sorted = values.sorted
            val p95    = sorted(math.ceil(values.size * .95).toInt - 1)
            measures += Json.obj(
              "name"      -> Json.fromString(name),
              "count"     -> Json.fromInt(values.size),
              "warmup"    -> Json.fromInt(10),
              "limitMs"   -> Json.fromDoubleOrNull(limit),
              "p95Ms"     -> Json.fromDoubleOrNull(p95),
              "maximumMs" -> Json.fromDoubleOrNull(sorted.last),
              "valuesMs"  -> Json.arr(values.map(Json.fromDoubleOrNull)*),
              "passed"    -> Json.fromBoolean(p95 < limit)
            )
            memoryAt(name)
            write("measuring")
            println(s"[performance] ${factor}x $name p95=$p95 ms limit=$limit")
          val key = keys.head
          runOnFx(navigate(key)); until("selected trial")(trialReady(w, key))
          val sourceRows = String(
            inputs.entries.find(_._1.role == SourceRole.Fixations).get._2.toArray,
            UTF_8
          ).linesIterator
            .drop(1)
            .zipWithIndex
            .collect {
              case (line, index) if line.startsWith("P17,Encoding,enc_03,1,") =>
                line.split(',')(4).toInt -> index
            }
            .toMap
          def selected(i: Int) = StudioRef.Fixation(key, get(FixationIndex.of(1 + i % 2)))
          measure("linked-selection", 100, 100.0) { i =>
            sequence += 1
            w.runtime.dispatch(
              Intent.Select(
                SelectionInput(
                  InputStamp(
                    w.runtime.model.selection.context,
                    origin,
                    sequence,
                    InputCause.Keyboard
                  ),
                  SelectionMode.Replace,
                  Vector(selected(i))
                )
              )
            )
          } { i =>
            w.explore.input.state.selected == Vector(selected(i)) &&
            w.timeline.twin.table.modelRowSelected.zipWithIndex.collect { case (true, row) =>
              row
            } == Vector(i % 2) &&
            w.sourceRecords.current.cursor.contains(sourceRows(1 + i % 2)) &&
            w.inspector.titleText.startsWith(s"Fixation ${1 + i % 2} of")
          }
          measure("cached-trial-switch", 100, 250.0)(i => navigate(keys(i % 2)))(i =>
            trialReady(w, keys(i % 2))
          )
          // Prepare each destination's meaningful content once, before its warm-up.
          dispatch(
            fx,
            w,
            Intent.Navigate(Location(Perspective.Compare, Vector(Place.Summary(reporting.id))))
          )
          until("summary reports", 180)(perspectiveReady(w, Perspective.Compare))
          dispatch(fx, w, Intent.Navigate(Location(Perspective.Figures, Vector(Place.Figures))))
          until("figure", 180)(perspectiveReady(w, Perspective.Figures))
          dispatch(
            fx,
            w,
            Intent.Navigate(Location(Perspective.Data, Vector(Place.Dataset(dataset))))
          )
          until("source and geometry", 180)(perspectiveReady(w, Perspective.Data))
          Perspective.values.foreach { target =>
            val away =
              if target == Perspective.Explore then Perspective.Data else Perspective.Explore
            measure(
              s"perspective-${target.toString}",
              50,
              150.0,
              _ => {
                runOnFx(w.runtime.dispatch(Intent.SwitchPerspective(away)))
                until(s"prepare $away")(perspectiveReady(w, away))
                rendered()
              }
            ) { _ =>
              w.runtime.dispatch(Intent.SwitchPerspective(target))
            } { _ => perspectiveReady(w, target) }
          }
          Perspective.values.foreach { p =>
            dispatch(fx, w, Intent.SwitchPerspective(p));
            until(s"snapshot $p", 180)(perspectiveReady(w, p))
            fx.snapshot(StudioTheme.Light).foreach { source =>
              val destination =
                output.resolve(s"fixture-${factor}x-${p.toString}-${source.getFileName}")
              Files.copy(
                source,
                destination,
                java.nio.file.StandardCopyOption.REPLACE_EXISTING
              ): Unit
            }
          }
          System.gc(); Thread.sleep(250)
          memoryAt("after-collection")
          write("completed")
          assert(
            measures.forall(_.hcursor.get[Boolean]("passed").contains(true)),
            measures.map(_.noSpaces).mkString("\n")
          )
          assert(heap <= 4L * 1024 * 1024 * 1024, s"retained heap $heap exceeds4GiB")
          assert(
            memory.forall(_.hcursor.get[Long]("rssBytes").exists(_ <= 8L * 1024 * 1024 * 1024))
          )
          write("passed")
        catch
          case error: Throwable =>
            write("failed")
            throw error
        finally
          window.foreach(w => { runOnFx(w.close()); opened -= w })
          release.unsafeRunSync()
          cleanup.unsafeRunSync()
      }
