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

package eyes4s.studio.desktop.geometry

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import eyes4s.studio.core.bundle.{BundleSamples, InputKind, LockOwner, SharingOptions}
import eyes4s.studio.core.document.SourceRole
import eyes4s.studio.core.session.ProjectSession
import eyes4s.studio.desktop.platform.{FileProjectStore, TempDirs}
import eyes4s.studio.desktop.runtime.SessionPort
import eyes4s.studio.desktop.trial.GoldenTrials
import java.nio.file.Files

import eyes4s.studio.app.StoryModels
import eyes4s.studio.app.geometry.{GeometryIntent, GeometryPanel}
import eyes4s.studio.app.layout.StudioLayouts
import eyes4s.studio.app.plot.PlotValue
import eyes4s.studio.core.fixture.{StoryMoment, StoryMoments}
import eyes4s.studio.core.importing.GeometryField
import eyes4s.studio.core.selection.{PlacementCount, StudioRef}
import eyes4s.studio.desktop.harness.StudioTheme
import eyes4s.studio.desktop.shell.ShellFxSuite
import javafx.scene.input.KeyCode

/** Placement counts open actual matching trial rows in the retained Data dock. */
class GeometryCountsFxSuite extends ShellFxSuite:

  /** A real named project supplies and verifies the golden source bytes. */
  private def withProject(body: SessionPort => Unit): Unit =
    val dir = Files.createTempDirectory("eyes4s-geometry-counts-")
    try
      val store    = FileProjectStore.at[IO](dir.resolve("memory-study.eyes")).unsafeRunSync()
      val owner    = LockOwner.of("GeometryCountsFxSuite").fold(e => fail(e.message), identity)
      val document = StoryModels.t1Data.document
      val session  = ProjectSession
        .create(
          store,
          owner,
          document,
          SharingOptions.complete,
          BundleSamples.inputsFor(document)
        )
        .unsafeRunSync()
        .fold(e => fail(e.message), identity)
      Vector(SourceRole.Fixations -> "fixations.csv", SourceRole.Trials -> "trials.csv")
        .foreach { (role, name) =>
          val bytes =
            IArray.unsafeFromArray(Files.readAllBytes(GoldenTrials.golden.resolve(name)))
          session
            .importInput(InputKind.Source(role), name, bytes)
            .unsafeRunSync()
            .fold(e => fail(e.message), _ => ())
        }
      val port = SessionPort.start(session)
      try body(port)
      finally
        opened.foreach(w => runOnFx(w.close()))
        opened.clear()
        port.close()
        session.close.unsafeRunSync(): Unit
    finally TempDirs.remove(dir)

  fxStage.test("a geometry count opens its matching source-placement trials") { fx =>
    withProject { port =>
      assumeFullStage(fx)
      val w = boot(fx, StoryModels.t1Data, StoryMoment.T1, project = Some(port))
      eventually(fx, "the current source placement") {
        w.geometry.state.placement.toOption.isDefined
      }
      runOnFx(
        w.geometry.view.node
          .lookup(".edge-to-edge")
          .asInstanceOf[javafx.scene.control.ScrollPane]
          .setVvalue(1.0)
      )
      fx.awaitLayout()
      fx.robot.click(runOnFx(w.geometry.view.outsideWindow.value))
      eventually(fx, "409 matching trials in the Records outside frame pane") {
        w.geometry.placementRows.source.exists(_.rows.size == 409) &&
        w.host.dock.state
          .groupOf(w.host.dockId(StudioLayouts.outsideFrame))
          .exists(_.active == w.host.dockId(StudioLayouts.outsideFrame))
      }
      val rows = runOnFx(w.geometry.placementRows.source).get.rows
      assertEquals(rows.map(_.values(5)).collect { case PlotValue.Number(n) => n }.sum, 543.0)
      assert(rows.forall(_.ref.isAggregate))
      val expected = StudioRef.PlacementTally(StoryMoments.r3, PlacementCount.OutsideWindow)
      assertEquals(
        runOnFx(w.runtime.model.location.trail.last),
        eyes4s.studio.app.nav.Place.At(expected)
      )
      eventually(fx, "the named project's source mapping") { w.columnMapping.loaded.isDone }
      fx.snapshot(StudioTheme.Light): Unit
      runOnFx {
        val row = w.geometry.placementRows.rowNode(0).getOrElse(fail("no first rendered trial"))
        val cells =
          row.getGraphic.asInstanceOf[javafx.scene.layout.Pane].getChildrenUnmodifiable
        val trial = cells.get(2).asInstanceOf[javafx.scene.control.Label]
        val ink   = javafx.scene.text.Text(trial.getText)
        ink.setFont(trial.getFont)
        val padding    = trial.getInsets.getLeft + trial.getInsets.getRight
        val PixelSlack = 1.0
        assert(
          trial.getWidth + PixelSlack >= ink.getLayoutBounds.getWidth + padding,
          s"trial ${trial.getText}: cell ${trial.getWidth}, text ${ink.getLayoutBounds.getWidth}, padding $padding"
        )
      }
      runOnFx(w.geometry.placementRows.requestFocus())
      fx.awaitLayout()
      fx.robot.press(KeyCode.HOME)
      fx.robot.press(KeyCode.ENTER)
      eventually(fx, "the selected row names its trial in the geometry example") {
        GeometryPanel.focus(w.runtime.model)._1.nonEmpty
      }
    }
  }

  fxStage.test("editing geometry under a draft id refreshes the count and its rows") { fx =>
    withProject { port =>
      val w = boot(fx, StoryModels.t1Data, StoryMoment.T1, project = Some(port))
      eventually(fx, "the initial placement") {
        w.geometry.state.placement.toOption.isDefined
      }
      val before = runOnFx(w.geometry.view.outsideWindow.value.getText)
      runOnFx {
        w.geometry.dispatch(GeometryIntent.EditField(GeometryField.ImageLeft, "0"))
        w.geometry.dispatch(GeometryIntent.CommitFields)
      }
      eventually(fx, "the revised draft's placement") {
        w.geometry.state.shown.exists(_.geometry.image.left == 0) &&
        w.geometry.state.placement.toOption.isDefined &&
        w.geometry.view.outsideWindow.value.getText != before
      }
      runOnFx(w.geometry.view.outsideWindow.value.fire())
      val preview = runOnFx(w.geometry.state.placement.toOption).get
      eventually(fx, "the rows agree with the revised placement") {
        w.geometry.placementRows.source.exists(_.rows.size == preview.trialsOutsideWindow)
      }
      val actual = runOnFx(w.geometry.placementRows.source).get.rows
        .map(_.values(5))
        .collect { case PlotValue.Number(n) => n }
        .sum
      assertEquals(actual, preview.outsideWindowRecords.toDouble)
    }
  }
