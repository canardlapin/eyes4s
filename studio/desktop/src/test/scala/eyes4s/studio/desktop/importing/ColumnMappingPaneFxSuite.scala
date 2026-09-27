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

package eyes4s.studio.desktop.importing

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.app.importing.{
  ChoiceVM,
  ColumnMappingPane,
  PaneNotice,
  WizardIntent,
  WizardProblem
}
import eyes4s.studio.app.layout.StudioLayouts
import eyes4s.studio.app.nav.{Location, Place}
import eyes4s.studio.core.backend.DatasetRevision
import eyes4s.studio.core.bundle.{BundleSamples, InputKind, LockOwner, SharingOptions}
import eyes4s.studio.core.command.HistoryStack
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.core.importing.ColumnChoice
import eyes4s.studio.core.session.ProjectSession
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.FxStage
import eyes4s.studio.desktop.platform.{FileProjectStore, TempDirs}
import eyes4s.studio.desktop.runtime.{ProjectPort, SessionPort}
import eyes4s.studio.desktop.shell.ShellFxSuite
import eyes4s.studio.desktop.trial.GoldenTrials
import eyes4s.studio.app.vm.{A11yRole, FocusStop}
import javafx.scene.AccessibleRole
import javafx.scene.control.ComboBox
import javafx.scene.input.KeyCode

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** The import wizard hosted in the Data perspective's column-mapping pane
  * (bead bd-01M3G8PH400E7F6716ZBHZ2XVH; Data.dc.html, column mapping):
  * selecting a dataset revision in the Data perspective re-reads its own
  * files from the project into a re-map, a commit re-admits it with one
  * command, and one undo restores it. Every interaction goes through the
  * robot's synthetic events or the controls' own values, so no test depends
  * on OS window focus.
  */
class ColumnMappingPaneFxSuite extends ShellFxSuite:

  private val r2 = DatasetRevision(2)
  private val r3 = DatasetRevision(3)
  private val r4 = DatasetRevision(4)

  /** A project whose reads wait until the test answers them. */
  private final class HeldPort extends ProjectPort:
    val held = scala.collection.mutable.ArrayBuffer
      .empty[(Source, Either[String, IArray[Byte]] => Unit)]
    def journal(entry: eyes4s.studio.core.command.JournalEntry): Unit                    = ()
    def save(done: Either[String, eyes4s.studio.core.session.SaveReceipt] => Unit): Unit =
      done(Left("held"))
    def close(): Unit = ()
    override def readInput(source: Source, done: Either[String, IArray[Byte]] => Unit): Unit =
      held += (source -> done)

    /** Answer a held read with the golden file of its role. */
    def answer(read: (Source, Either[String, IArray[Byte]] => Unit)): Unit =
      val name = read._1.path.value.split('/').last
      read._2(Right(bytes(GoldenTrials.golden.resolve(name))))

  private def bytes(p: Path): IArray[Byte] = IArray.unsafeFromArray(Files.readAllBytes(p))

  /** A project holding t1, with the golden files stored as its inputs. */
  private def project(dir: Path): (ProjectSession[IO], SessionPort) =
    val store   = FileProjectStore.at[IO](dir.resolve("memory-study.eyes")).unsafeRunSync()
    val owner   = LockOwner.of("ColumnMappingPaneFxSuite").fold(e => fail(e.toString), identity)
    val t1      = StoryModels.t1Data.document
    val session = ProjectSession
      .create(store, owner, t1, SharingOptions.complete, BundleSamples.inputsFor(t1))
      .unsafeRunSync()
      .fold(e => fail(e.message), identity)
    Vector(SourceRole.Fixations -> "fixations.csv", SourceRole.Trials -> "trials.csv").foreach {
      (role, name) =>
        session
          .importInput(InputKind.Source(role), name, bytes(GoldenTrials.golden.resolve(name)))
          .unsafeRunSync()
          .fold(e => fail(e.message), _ => ())
    }
    (session, SessionPort.start(session))

  private def selectData(fx: FxStage, w: StudioWindow, id: DatasetRevision): Unit =
    dispatch(fx, w, Intent.Navigate(Location(Perspective.Data, Vector(Place.Dataset(id)))))

  /** Wait until the pane shows `id` with every source read. */
  private def loaded(fx: FxStage, w: StudioWindow, id: DatasetRevision): Unit =
    eventually(fx, s"the pane to load ${id.label}") {
      val p = w.columnMapping
      p.dataset.map(_.id).contains(id) && p.loaded.isDone
    }
    // One pulse: the new rows' role menus get their skins, which is where a
    // combo box turns a new value into its action.
    fx.awaitLayout()

  /** Pick `choice` in `column`'s role menu, as a user's pick does, and check
    * that the wizard took it.
    */
  private def pick(fx: FxStage, w: StudioWindow, column: String, choice: ColumnChoice): Unit =
    val box = menu(w, column)
    runOnFx(box.getItems.asScala.find(_.choice == choice).foreach(box.setValue))
    fx.awaitLayout()
    assertEquals(role(w, column), Some(choice))
    assertEquals(
      runOnFx(w.columnMapping.wizard.model.fixations.flatMap(_._2.columns.collectFirst {
        case (c, chosen) if c.name.value == column => chosen
      })),
      Some(choice)
    )

  private def menu(w: StudioWindow, column: String): ComboBox[ChoiceVM] =
    runOnFx(w.columnMapping.wizard.view.fixations.menu(column))
      .getOrElse(fail(s"no role menu for $column"))

  private def role(w: StudioWindow, column: String): Option[ColumnChoice] =
    runOnFx(Option(menu(w, column).getValue).map(_.choice))

  private def withProject(body: (ProjectSession[IO], SessionPort) => Unit): Unit =
    val dir = Files.createTempDirectory("eyes4s-remap")
    try
      val (session, port) = project(dir)
      try body(session, port)
      finally
        opened.foreach(w => runOnFx(w.close()))
        opened.clear()
        port.close()
        session.close.unsafeRunSync(): Unit
    finally TempDirs.remove(dir)

  fxStage.test("Data selection to re-admit through the pane; one undo restores the revision") {
    fx =>
      assumeFullStage(fx)
      withProject { (_, port) =>
        val start = StoryModels.t1Data
        val w     = boot(fx, start, StoryMoment.T1, project = Some(port))
        // The wizard is the Data perspective's column-mapping pane.
        val paneNode = runOnFx(w.host.node(StudioLayouts.columnMapping))
          .getOrElse(fail("the column-mapping pane was not built"))
        assert(runOnFx(paneNode.getScene eq fx.scene))
        assert(runOnFx(w.columnMapping.node.getParent eq paneNode))
        loaded(fx, w, r3)
        assertEquals(runOnFx(w.columnMapping.wizard.model.problem), None)
        assertEquals(role(w, "occurrence"), Some(ColumnChoice.Role(ColumnRole.Occurrence)))
        assertEquals(runOnFx(drawn(w.columnMapping.wizard.view.commit)), "Apply to r3")

        // Selecting r2 re-reads r2's files: its occurrence column is unmapped.
        selectData(fx, w, r2)
        loaded(fx, w, r2)
        assertEquals(role(w, "occurrence"), Some(ColumnChoice.Attribute))
        assertEquals(runOnFx(drawn(w.columnMapping.wizard.view.commit)), "Re-admit as r4")

        // Back on pending r3: re-map occurrence as a pass-through attribute.
        selectData(fx, w, r3)
        loaded(fx, w, r3)
        val before = runOnFx(w.runtime.model)
        pick(fx, w, "occurrence", ColumnChoice.Attribute)
        fx.robot.click(w.columnMapping.wizard.view.commit)
        fx.awaitLayout()
        val after   = runOnFx(w.runtime.model)
        val revised = after.document.dataset(r3).getOrElse(fail("r3 is gone"))
        assertEquals(revised.mapping.column(ColumnRole.Occurrence), None)
        assertEquals(revised.attributes.columns.map(_.value), Vector("occurrence"))
        // One command: no new revision, and one undo step.
        assertEquals(after.document.datasets.map(_.id), before.document.datasets.map(_.id))
        assertEquals(
          AppModel.update(after, Intent.Undo(HistoryStack.Science))._1.document,
          before.document
        )
        // The pane reloads onto the revised r3.
        eventually(fx, "the pane to reload r3 as revised") {
          w.columnMapping.dataset.contains(revised) && w.columnMapping.loaded.isDone
        }
        assertEquals(role(w, "occurrence"), Some(ColumnChoice.Attribute))
        assertEquals(runOnFx(w.columnMapping.wizard.model.problem), None)

        // One undo restores r3, and the pane follows it.
        dispatch(fx, w, Intent.Undo(HistoryStack.Science))
        assertEquals(runOnFx(w.runtime.model.document), before.document)
        eventually(fx, "the pane to reload r3 as it was") {
          w.columnMapping.dataset == before.document.dataset(
            r3
          ) && w.columnMapping.loaded.isDone
        }
        assertEquals(role(w, "occurrence"), Some(ColumnChoice.Role(ColumnRole.Occurrence)))
      }
  }

  fxStage.test("re-admitting admitted r2 selects the new r4; a second commit adds nothing") {
    fx =>
      assumeFullStage(fx)
      withProject { (_, port) =>
        val w = boot(fx, StoryModels.t1Data, StoryMoment.T1, project = Some(port))
        loaded(fx, w, r3)
        selectData(fx, w, r2)
        loaded(fx, w, r2)
        val before = runOnFx(w.runtime.model)
        pick(fx, w, "occurrence", ColumnChoice.Role(ColumnRole.Occurrence))
        // r2 left its time unit undeclared, which blocks the re-import.
        runOnFx {
          val t = w.columnMapping.wizard.view.time
          t.getItems.asScala.find(_.unit.contains(TimeUnit.Milliseconds)).foreach(t.setValue)
        }
        fx.awaitLayout()
        assertEquals(
          runOnFx(w.columnMapping.wizard.model.fixations.flatMap(_._2.time)),
          Some(TimeUnit.Milliseconds)
        )
        assertEquals(runOnFx(drawn(w.columnMapping.wizard.view.commit)), "Re-admit as r4")
        fx.robot.click(w.columnMapping.wizard.view.commit)
        fx.awaitLayout()
        val after = runOnFx(w.runtime.model)
        assertEquals(after.document.datasets.map(_.id), Vector(r2, r3, r4))
        assertEquals(after.document.dataset(r4).flatMap(_.parent), Some(r2))
        // The Data selection moved to r4, and the pane shows r4 as it is.
        assertEquals(ColumnMappingPane.selected(after).map(_.id), Some(r4))
        loaded(fx, w, r4)
        assertEquals(runOnFx(drawn(w.columnMapping.wizard.view.commit)), "Apply to r4")
        assertEquals(role(w, "occurrence"), Some(ColumnChoice.Role(ColumnRole.Occurrence)))
        assertEquals(runOnFx(w.columnMapping.shownNotice), None)
        // Committing again changes nothing: no r5.
        fx.robot.click(w.columnMapping.wizard.view.commit)
        fx.awaitLayout()
        assertEquals(runOnFx(w.runtime.model.document.datasets.map(_.id)), Vector(r2, r3, r4))
        assertEquals(
          runOnFx(w.columnMapping.wizard.model.problem),
          Some(WizardProblem.NoChange(r4))
        )
        // One undo removes r4 again.
        dispatch(fx, w, Intent.Undo(HistoryStack.Science))
        assertEquals(runOnFx(w.runtime.model.document), before.document)
      }
  }

  fxStage.test("a read that answers after the selection moved on is dropped") { fx =>
    assumeFullStage(fx)
    val port = HeldPort()
    val w    = boot(fx, StoryModels.t1Data, StoryMoment.T1, project = Some(port))
    assertEquals(runOnFx(port.held.size), 2) // r3's two files, not answered
    val r3Settled = runOnFx(w.columnMapping.loaded)
    selectData(fx, w, r2)
    assertEquals(runOnFx(port.held.size), 4)
    // The superseded load's future ends: it is cancelled, never left pending.
    assert(runOnFx(r3Settled.isCancelled))
    val r3Load = runOnFx(port.held.take(2).toVector)
    runOnFx(port.held.drop(2).toVector).foreach(port.answer)
    loaded(fx, w, r2)
    // An edit on r2 that r3's late answers must not overwrite.
    pick(fx, w, "occurrence", ColumnChoice.Role(ColumnRole.Occurrence))
    r3Load.foreach(port.answer)
    eventually(fx, "r3's late answers") { w.columnMapping.readsAnswered == 4L }
    fx.awaitLayout()
    assertEquals(runOnFx(w.columnMapping.dataset.map(_.id)), Some(r2))
    assertEquals(
      runOnFx(w.columnMapping.wizard.model.target),
      eyes4s.studio.app.importing.WizardTarget.Remap(r2)
    )
    assertEquals(role(w, "occurrence"), Some(ColumnChoice.Role(ColumnRole.Occurrence)))
    assertEquals(
      runOnFx(
        w.columnMapping.wizard.model.fixations.map(_._2.columnsFor(ColumnRole.Occurrence))
      ),
      Some(Vector(ColumnName.of("occurrence").toOption.get))
    )
  }

  fxStage.test("a commit the app does not apply leaves nothing half-done: the pane reverts") {
    fx =>
      val port  = HeldPort()
      val model = StoryModels.t1Data
      val sent  = scala.collection.mutable.ArrayBuffer.empty[Intent]
      val pane  = runOnFx(
        ColumnMappingPaneHost(
          () => model,
          sent += _, // an app that applies nothing
          ImportWizardHost.fxPlatform(() => fx.stage, noPresets(), None),
          Some(port)
        )
      )
      runOnFx(pane.sync(model))
      runOnFx(port.held.toVector).foreach(port.answer)
      eventually(fx, "the pane to load r3") { pane.loaded.isDone && pane.readsAnswered == 2L }
      runOnFx(
        pane.wizard.dispatch(
          WizardIntent.Choose(
            SourceRole.Fixations,
            ColumnName.of("occurrence").toOption.get,
            ColumnChoice.Attribute
          )
        )
      )
      runOnFx(pane.wizard.dispatch(WizardIntent.Commit))
      assertEquals(runOnFx(sent.size), 1)
      // The model did not move, so the close after the commit reads r3 again.
      assertEquals(runOnFx(port.held.size), 4)
      runOnFx(port.held.drop(2).toVector).foreach(port.answer)
      eventually(fx, "the pane to read r3 again") {
        pane.readsAnswered == 4L && pane.loaded.isDone
      }
      assertEquals(
        runOnFx(pane.wizard.model.fixations.map(_._2.columnsFor(ColumnRole.Occurrence))),
        Some(Vector(ColumnName.of("occurrence").toOption.get))
      )
  }

  fxStage.test("edits dropped by a change elsewhere, and unreadable presets, are noticed") {
    fx =>
      assumeFullStage(fx)
      val presetDir = Files.createTempDirectory("eyes4s-bad-presets")
      try
        Files.writeString(presetDir.resolve("6261.json"), "not a preset")
        withProject { (_, port) =>
          val w = boot(
            fx,
            StoryModels.t1Data,
            StoryMoment.T1,
            project = Some(port),
            presets = eyes4s.studio.desktop.platform.FilePresetStore(presetDir)
          )
          loaded(fx, w, r3)
          eventually(fx, "the presets notice") {
            w.columnMapping.shownNotice.exists {
              case PaneNotice.PresetsUnreadable(errors) =>
                errors.exists(_.contains("6261.json"))
              case _ => false
            }
          }
          pick(fx, w, "occurrence", ColumnChoice.Attribute)
          // Another edit revises r3 under the pane.
          val r3spec = runOnFx(w.runtime.model.document.dataset(r3).get)
          dispatch(
            fx,
            w,
            Intent.Dispatch(
              eyes4s.studio.core.command.Command.ReviseDataset(
                r3,
                r3spec.mapping,
                DeclaredUnits(Some(TimeUnit.Seconds)),
                r3spec.geometry,
                r3spec.attributes
              )
            )
          )
          loaded(fx, w, r3)
          assertEquals(runOnFx(w.columnMapping.shownNotice), Some(PaneNotice.EditsDropped(r3)))
          assertEquals(role(w, "occurrence"), Some(ColumnChoice.Role(ColumnRole.Occurrence)))
          assert(
            runOnFx(w.columnMapping.node.getChildren.asScala.exists {
              case l: javafx.scene.control.Label =>
                l.isVisible && l.getText == PaneNotice.EditsDropped(r3).message
              case _ => false
            })
          )
        }
      finally TempDirs.remove(presetDir)
  }

  fxStage.test("board parity: the pane's header, kind, columns and declared units") { fx =>
    assumeFullStage(fx)
    withProject { (_, port) =>
      val w = boot(fx, StoryModels.t1Data, StoryMoment.T1, project = Some(port))
      loaded(fx, w, r3)
      val view = w.columnMapping.wizard.view
      assertEquals(runOnFx(drawn(view.kind)), "Dataset · re-admit")
      assertEquals(
        runOnFx(view.fixations.head.getChildren.asScala.toVector.collect {
          case l: javafx.scene.control.Label => drawn(l)
        }),
        Vector("fixations.csv column", "First records", "Role", "Units (declared)")
      )
      // A re-map reads the project's files: no file to choose.
      assert(!runOnFx(view.fixations.choose.isVisible))
      // The dock tab names the pane: the wizard shows no tab strip of its own.
      assert(runOnFx(view.tabs.values.forall(t => !t.isVisible && !t.isManaged)))
      assertEquals(runOnFx(drawn(view.cancel)), "Revert")
      val onset = runOnFx(view.fixations.rowNode("onset_ms")).getOrElse(fail("no onset_ms row"))
      assertEquals(
        runOnFx(onset.getChildren.asScala.toVector.collect {
          case l: javafx.scene.control.Label => drawn(l)
        }.last),
        "ms"
      )
      assertEquals(
        runOnFx(view.fixations.rowNode("x").map(_.getHeight)).map(math.round),
        Some(28L)
      )
      fx.snapshot(eyes4s.studio.desktop.harness.StudioTheme.Light): Unit
    }
  }

  fxStage.test("Tab from the pane visits the wizard's derived stops, then leaves the pane") {
    fx =>
      withProject { (_, port) =>
        val w = boot(fx, StoryModels.t1Data, StoryMoment.T1, project = Some(port))
        loaded(fx, w, r3)
        val derived = runOnFx(w.paneStops(StudioLayouts.columnMapping))
        // The time unit, one role menu per column, the preset name, the
        // trial key's occurrence toggle (S5.3), Revert and the commit.
        // No tab strip in a re-map.
        assertEquals(derived.size, 1 + 10 + 1 + 1 + 2)
        assertEquals(
          derived(12),
          FocusStop(A11yRole.ToggleButton, "Occurrence: column occurrence")
        )
        val pane = runOnFx(w.host.node(StudioLayouts.columnMapping)).get
        runOnFx(pane.requestFocus())
        fx.awaitLayout()
        def stop(): FocusStop = runOnFx {
          val n    = fx.scene.getFocusOwner
          val role = n.getAccessibleRole match
            case AccessibleRole.TOGGLE_BUTTON => A11yRole.ToggleButton
            case AccessibleRole.COMBO_BOX     => A11yRole.ComboBox
            case AccessibleRole.TEXT_FIELD    => A11yRole.TextField
            case AccessibleRole.BUTTON        => A11yRole.Button
            case AccessibleRole.PARENT        => A11yRole.Region
            case other                        => fail(s"unexpected role $other of $n")
          FocusStop(role, n.getAccessibleText)
        }
        val walked = derived.indices.toVector.map { _ =>
          fx.robot.press(KeyCode.TAB)
          stop()
        }
        assertEquals(walked, derived)
        fx.robot.press(KeyCode.TAB)
        assertEquals(stop(), FocusStop(A11yRole.Region, "Admission"))
      }
  }

  fxStage.test("a re-map whose key is on a many-valued column completes its streaming check") {
    fx =>
      withProject { (session, port) =>
        // A revision whose fixation file has 5,000 trial labels, past the
        // sniffer's cap: the key is checked in one pass over the project's
        // stored input, which the pane registers as the file's reader.
        val header =
          "participant,phase,trial,occurrence,ordinal,x,y,onset_ms,duration_ms,sample_count"
        val rows = (1 to 5000).map(i => s"P01,Encoding,t$i,1,1,960.0,540.0,0,200,100") :+
          "P01,Encoding,t7,2,2,960.0,540.0,300,200,100"
        val text = (header +: rows).mkString("", "\n", "\n")
        val big = IArray.unsafeFromArray(text.getBytes(java.nio.charset.StandardCharsets.UTF_8))
        session
          .importInput(InputKind.Source(SourceRole.Fixations), "big-fixations.csv", big)
          .unsafeRunSync()
          .fold(e => fail(e.message), _ => ())
        val start  = StoryModels.t1Data
        val r3spec = start.document.dataset(r3).getOrElse(fail("no r3"))
        val source = Source(
          SourceRole.Fixations,
          SourcePath.of("big-fixations.csv").fold(e => fail(e.message), identity),
          eyes4s.codec.ByteDigest.sha256(big),
          None
        )
        val command = eyes4s.studio.core.command.Command.ImportSources(
          Some(r3),
          Sources.of(Vector(source)).fold(e => fail(e.message), identity),
          r3spec.mapping,
          r3spec.units,
          r3spec.geometry,
          r3spec.attributes
        )
        val model = AppModel.update(start, Intent.Dispatch(command))._1
        assert(model.document.dataset(r4).isDefined, "r4 was not created")
        val w = boot(fx, model, StoryMoment.T1, project = Some(port))
        selectData(fx, w, r4)
        loaded(fx, w, r4)
        runOnFx(w.columnMapping.wizard.keyChecked)
          .get(60, java.util.concurrent.TimeUnit.SECONDS)
        fx.awaitLayout()
        val key = w.columnMapping.wizard.view.key
        assertEquals(runOnFx(key.count.getText), "5,000 keys")
        assertEquals(
          runOnFx(key.detail.getText),
          "· 1 key names more than one occurrence · Occurrence 1–2 · 1 later presentations"
        )
      }
  }

  fxStage.test("Revert drops the pane's edits and reads the revision again") { fx =>
    assumeFullStage(fx)
    withProject { (_, port) =>
      val w = boot(fx, StoryModels.t1Data, StoryMoment.T1, project = Some(port))
      loaded(fx, w, r3)
      pick(fx, w, "occurrence", ColumnChoice.Attribute)
      val document = runOnFx(w.runtime.model.document)
      fx.robot.click(w.columnMapping.wizard.view.cancel)
      eventually(fx, "the pane to read r3 again") {
        w.columnMapping.loaded.isDone &&
        w.columnMapping.wizard.model.fixations.exists(
          _._2.columnsFor(ColumnRole.Occurrence).map(_.value) == Vector("occurrence")
        )
      }
      assertEquals(role(w, "occurrence"), Some(ColumnChoice.Role(ColumnRole.Occurrence)))
      assertEquals(runOnFx(w.runtime.model.document), document)
    }
  }

  fxStage.test("without a project the pane says it cannot read the revision's files") { fx =>
    val w = boot(fx, StoryModels.t1Data, StoryMoment.T1)
    loaded(fx, w, r3)
    // Each source is refused; the last one read is the problem shown.
    runOnFx(w.columnMapping.wizard.model.problem) match
      case Some(WizardProblem.ReadFailed(path, _)) => assertEquals(path, "inputs/trials.csv")
      case other => fail(s"expected a read failure, got $other")
    assertEquals(runOnFx(w.columnMapping.wizard.model.fixations), None)
  }
