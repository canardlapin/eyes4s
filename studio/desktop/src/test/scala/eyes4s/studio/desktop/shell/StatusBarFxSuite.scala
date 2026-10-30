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

import java.nio.file.Files
import scala.jdk.CollectionConverters.*

/** The status bar (ticket S1.8; overhaul v4): the context hint and "Saved
  * hh:mm", the time of the last atomic save of the project session. The job
  * it once mirrored is the state button's, tested here as it ticks.
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

  private val boards = Vector(
    ("DataEmpty", () => StoryModels.firstRun, StoryMoment.T2),
    ("t1 Data", () => StoryModels.t1Data, StoryMoment.T1),
    ("t2 Explore", () => StoryModels.t2Explore, StoryMoment.T2),
    ("t2 Analysis", () => StoryModels.t2Analysis, StoryMoment.T2),
    ("t2 Compare", () => StoryModels.t2Compare, StoryMoment.T2),
    ("t2 Figures", () => StoryModels.t2Figures, StoryMoment.T2),
    ("t3 Summary", () => StoryModels.t3Summary, StoryMoment.T3)
  )

  fxStage.test("two slots, 26 px: the hint, and Saved at the right") { fx =>
    assumeFullStage(fx)
    boards.foreach { (board, model, moment) =>
      val w = boot(fx, model(), moment)
      assertEquals(slots(w), Vector("status-hint", "", "status-saved"), board)
      assertEqualsDouble(runOnFx(w.shell.statusBar.node.getHeight), 26, 1)
      assertNoDiff(statusLine(w), expectedStatus(w), board)
      runOnFx(w.close())
    }
  }

  private val lost = StudioDiagnostic(
    "studio-execution.lost-job",
    DiagnosticLevel.Error,
    DiagnosticOrigin.Host,
    Vector.empty,
    "lost"
  )

  fxStage.test("the state button's job slot ticks with the job; a focused ✕ keeps its focus") {
    fx =>
      assumeFullStage(fx)
      val w     = boot(fx, StoryModels.t3Summary, StoryMoment.T3)
      val state = w.shell.appBar.state
      assertEquals(runOnFx(state.drawnSlots.last), "Run 8 · comparing 21,400 / 44,845")
      runOnFx(state.cancel.requestFocus())
      fx.awaitLayout()
      assert(
        runOnFx(fx.scene.getFocusOwner eq state.cancel),
        s"✕ never took focus: ${runOnFx(fx.scene.getFocusOwner)}"
      )
      w.session.await(w.session.fixture.get.advanceToPairs(StoryMoments.run8Job, 30_000L))
      eventually(fx, "30,000 pairs")(
        state.drawnSlots.lastOption.contains("Run 8 · comparing 30,000 / 44,845")
      )
      assert(
        runOnFx(fx.scene.getFocusOwner eq state.cancel),
        s"✕ lost focus to ${runOnFx(fx.scene.getFocusOwner)}"
      )
      fx.snapshot(StudioTheme.Light)
      w.session.await(w.session.fixture.get.fail(StoryMoments.run8Job, Vector(lost, lost)))
      eventually(fx, "the failed slot")(
        state.drawnSlots.lastOption.contains("Run 8 failed · 2 diagnostics")
      )
      assert(runOnFx(!state.cancel.isVisible))
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
