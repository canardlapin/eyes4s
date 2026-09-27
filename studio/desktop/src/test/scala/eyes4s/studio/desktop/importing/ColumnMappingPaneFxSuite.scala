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
import eyes4s.studio.app.importing.{ChoiceVM, WizardProblem}
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
import eyes4s.studio.desktop.runtime.SessionPort
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
        // The selected tab, the time unit, one role menu per column, the
        // preset name, Revert and the commit.
        assertEquals(derived.size, 1 + 1 + 10 + 1 + 2)
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
