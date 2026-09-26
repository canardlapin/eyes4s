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

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import eyes4s.studio.app.{ClockTime, Intent, StoryModels}
import eyes4s.studio.core.backend.{DiagnosticLevel, DiagnosticOrigin, StudioDiagnostic}
import eyes4s.studio.core.bundle.{BundleSamples, LockOwner, SharingOptions}
import eyes4s.studio.core.command.JournalEntry
import eyes4s.studio.core.fixture.{StoryMoment, StoryMoments}
import eyes4s.studio.core.session.{ProjectSession, SaveReceipt}
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.StudioTheme
import eyes4s.studio.desktop.platform.{FileProjectStore, TempDirs}
import eyes4s.studio.desktop.runtime.{ProjectPort, SessionPort}
import javafx.scene.control.Label

import java.nio.file.Files
import scala.jdk.CollectionConverters.*

/** The status bar (ticket S1.8): always four slots; the selection path
  * follows the selection within 100 ms; the job slot mirrors the jobs chip;
  * "Saved hh:mm" is the time of the last atomic save of the project session.
  */
class StatusBarFxSuite extends ShellFxSuite:

  private def slots(w: StudioWindow): Vector[String] = runOnFx {
    val s = w.shell.statusBar
    s.node.getChildren.asScala.toVector.map(n =>
      n.getStyleClass.asScala.lastOption.getOrElse("")
    )
  }

  private def statusLine(w: StudioWindow): String =
    texts(w).find(_.startsWith("status: ")).getOrElse(fail("no status line"))

  private def expectedStatus(w: StudioWindow): String =
    expected(w).find(_.startsWith("status: ")).getOrElse(fail("no status line"))

  private def path(w: StudioWindow): String = runOnFx {
    w.shell.statusBar.selected.getChildren.asScala.toVector
      .filter(_.isManaged)
      .collect { case l: Label => drawn(l) }
      .mkString(" ")
  }

  private val boards = Vector(
    ("DataEmpty", () => StoryModels.firstRun, StoryMoment.T2),
    ("t1 Data", () => StoryModels.t1Data, StoryMoment.T1),
    ("t2 Explore", () => StoryModels.t2Explore, StoryMoment.T2),
    ("t2 Analysis", () => StoryModels.t2Analysis, StoryMoment.T2),
    ("t2 Compare", () => StoryModels.t2Compare, StoryMoment.T2),
    ("t2 Figures", () => StoryModels.t2Figures, StoryMoment.T2),
    ("t3 Summary", () => StoryModels.t3Summary, StoryMoment.T3)
  )

  fxStage.test("always four slots, 24 px, with the board's words") { fx =>
    boards.foreach { (board, model, moment) =>
      val w = boot(fx, model(), moment)
      assertEquals(
        slots(w),
        Vector(
          "status-selected",
          "bar-rule",
          "status-hint",
          "",
          "status-job",
          "bar-rule",
          "status-saved"
        ),
        board
      )
      assertEqualsDouble(runOnFx(w.shell.statusBar.node.getHeight), 24, 1)
      assertNoDiff(statusLine(w), expectedStatus(w), board)
      runOnFx(w.close())
    }
  }

  fxStage.test("the selection path follows the selection bus within 100 ms") { fx =>
    val w = boot(fx, StoryModels.t2Compare)
    assertEquals(path(w), "Selected: P17 › ret_07 × enc_03 (matched) · σ 2°")
    // A view submits a selection input, as the trial view will (S4.2).
    val input =
      StoryModels.select(runOnFx(w.runtime.model), "compare.query-trial", StoryModels.query)
    val start = System.nanoTime
    runOnFx(w.runtime.dispatch(input))
    var shown = path(w)
    while shown == "Selected: P17 › ret_07 × enc_03 (matched) · σ 2°" &&
      System.nanoTime - start < 1_000_000_000L
    do shown = path(w)
    val elapsedMs = (System.nanoTime - start) / 1_000_000L
    assert(elapsedMs < 100, s"the path took $elapsedMs ms")
    assertEquals(
      shown,
      s"Selected: ${expectedStatus(w).stripPrefix("status: Selected: ").takeWhile(_ != '|').trim}"
    )
    // A cleared selection says so, and the slot stays.
    val clear = eyes4s.studio.core.selection.SelectionInput(
      eyes4s.studio.core.selection.InputStamp(
        runOnFx(w.runtime.model.selection.context),
        eyes4s.studio.core.selection.ViewId.of("compare.query-trial").toOption.get,
        1L,
        eyes4s.studio.core.selection.InputCause.Keyboard
      ),
      eyes4s.studio.core.selection.SelectionMode.Clear,
      Vector.empty
    )
    dispatch(fx, w, Intent.Select(clear))
    assertEquals(path(w), "No selection")
    assertNoDiff(statusLine(w), expectedStatus(w))
  }

  private val lost = StudioDiagnostic(
    "studio-execution.lost-job",
    DiagnosticLevel.Error,
    DiagnosticOrigin.Host,
    Vector.empty,
    "lost"
  )

  fxStage.test("the job slot mirrors the jobs chip: running, progress, Cancel, failed") { fx =>
    val w              = boot(fx, StoryModels.t3Summary, StoryMoment.T3)
    val bar            = w.shell.statusBar
    val chip           = w.shell.appBar.jobs
    def mirror(): Unit =
      val parts = runOnFx(chip.drawnParts)
      val slot  =
        runOnFx((drawn(bar.jobText), Option.when(bar.jobCount.isManaged)(drawn(bar.jobCount))))
      // Title · stage in the slot's words; the chip's count without its unit.
      assertEquals(slot._1, parts.take(2).mkString(" · "))
      assertEquals(slot._2, parts.lift(2).map(_.stripSuffix(" pairs")))
    assertEquals(
      statusLine(w),
      "status: Selected: P17 · by retrieval response · σ 2° | Enter on a participant opens its queries · F6 next pane | Run 8 · Comparing 21,400 / 44,845 Cancel | Saved 10:24"
    )
    mirror()
    w.session.await(w.session.backend.advanceToPairs(StoryMoments.run8Job, 30_000L))
    eventually(fx, "30,000 pairs")(drawn(bar.jobCount) == "30,000 / 44,845")
    mirror()
    assertEquals(
      runOnFx(
        drawn(
          chip.chip.getGraphic
            .asInstanceOf[javafx.scene.layout.HBox]
            .getChildren
            .asScala
            .last
            .asInstanceOf[Label]
        )
      ),
      "30,000 / 44,845 pairs"
    )
    fx.snapshot(StudioTheme.Light)
    w.session.await(w.session.backend.fail(StoryMoments.run8Job, Vector(lost, lost)))
    eventually(fx, "the failed slot")(drawn(bar.jobText) == "Run 8 failed · 2 diagnostics")
    assertEquals(runOnFx(chip.text), "Run 8 failed · 2 diagnostics")
    assert(runOnFx(!bar.jobAction.isManaged && !bar.jobCount.isManaged))
  }

  fxStage.test(
    "the slot's Cancel is the chip's Cancel; a focused Cancel keeps focus as the count ticks"
  ) { fx =>
    val w   = boot(fx, StoryModels.t3Summary, StoryMoment.T3)
    val bar = w.shell.statusBar
    runOnFx(bar.jobAction.requestFocus())
    fx.awaitLayout()
    assert(
      runOnFx(fx.scene.getFocusOwner eq bar.jobAction),
      s"Cancel never took focus: ${runOnFx(fx.scene.getFocusOwner)}"
    )
    w.session.await(w.session.backend.advanceToPairs(StoryMoments.run8Job, 25_000L))
    eventually(fx, "25,000 pairs")(drawn(bar.jobCount) == "25,000 / 44,845")
    assert(
      runOnFx(fx.scene.getFocusOwner eq bar.jobAction),
      s"Cancel lost focus to ${runOnFx(fx.scene.getFocusOwner)}"
    )
    assertEquals(runOnFx(bar.jobAction.getAccessibleText), "Cancel")
    fx.robot.click(runOnFx(bar.jobAction))
    eventually(fx, "the cancelled job")(
      !w.shell.appBar.jobs.text.startsWith("Run 8 · Comparing")
    )
    assertEquals(runOnFx(drawn(bar.jobText)), runOnFx(w.shell.appBar.jobs.text))
  }

  fxStage.test("'Saved hh:mm' is the time of the project session's last atomic save") { fx =>
    TempDirs
      .resource("eyes4s-status")
      .use { dir =>
        IO.blocking {
          val model = StoryModels.t2Compare
          val store = FileProjectStore.at[IO](dir.resolve("memory-study.eyes")).unsafeRunSync()
          val owner = LockOwner.of("StatusBarFxSuite").fold(e => fail(e.toString), identity)
          val session = ProjectSession
            .create(
              store,
              owner,
              model.document,
              SharingOptions.complete,
              BundleSamples.inputsFor(model.document)
            )
            .unsafeRunSync()
            .fold(e => fail(e.message), identity)
          val port  = SessionPort.start(session)
          val clock = () => ClockTime.of(14, 7).toOption
          val w     = boot(fx, model, project = Some(port), clock = clock)
          assertEquals(runOnFx(drawn(w.shell.statusBar.saved)), "Saved 10:24")
          // An edit: discard the draft (journaled, then saved).
          dispatch(fx, w, Intent.RequestDiscardDraft)
          dispatch(fx, w, Intent.Confirm)
          eventually(fx, "the save")(drawn(w.shell.statusBar.saved) == "Saved 14:07")
          assertNoDiff(statusLine(w), expectedStatus(w))
          assert(!runOnFx(w.runtime.model.save.edited))
          // What was saved is the model's document, on disk.
          assertEquals(session.saved.unsafeRunSync(), runOnFx(w.runtime.model.document))
          assertEquals(session.saved.unsafeRunSync().draft, None)
          assert(Files.exists(dir.resolve("memory-study.eyes").resolve("project.json")))
          runOnFx(w.close())
          opened.clear()
          session.close.unsafeRunSync(): Unit
        }
      }
      .unsafeRunSync()
  }

  /** A project whose saves fail. */
  private final class FailingPort extends ProjectPort:
    def journal(entry: JournalEntry): Unit                    = ()
    def save(done: Either[String, SaveReceipt] => Unit): Unit = done(Left("disk full"))
    def close(): Unit                                         = ()

  fxStage.test("a failed save keeps the last time and says why") { fx =>
    val w = boot(fx, StoryModels.t2Compare, project = Some(FailingPort()))
    dispatch(fx, w, Intent.RequestDiscardDraft)
    dispatch(fx, w, Intent.Confirm)
    eventually(fx, "the notice")(w.shell.notice.node.isVisible)
    assertEquals(
      runOnFx(drawn(w.shell.notice.text)),
      "The project could not be saved: disk full"
    )
    assertEquals(runOnFx(drawn(w.shell.statusBar.saved)), "Saved 10:24")
    assert(runOnFx(w.runtime.model.save.edited))
  }
