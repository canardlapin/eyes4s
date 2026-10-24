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

package eyes4s.studio.desktop.compare

import eyes4s.studio.app.StoryModels
import eyes4s.studio.app.nav.Place
import eyes4s.studio.app.compare.{ReportAnswer, SummaryIntent}
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.{FxStage, StudioTheme}
import eyes4s.studio.desktop.shell.{CompositeFocus, KeyboardFocus, ShellFxSuite}
import javafx.scene.{Node, Parent}
import javafx.scene.input.KeyCode
import scala.jdk.CollectionConverters.*

class ParticipantsNavigatorFxSuite extends ShellFxSuite:
  private def loaded(fx: FxStage, w: StudioWindow): Unit =
    eventually(fx, "all24overall participants at2degrees") {
      w.summary.participants.rows.size == 24 && w.summary.participants.rows.head.value == "+0.42"
    }
  private def descendants(n: Node): Vector[Node] = n match
    case p: Parent => n +: p.getChildrenUnmodifiable.asScala.toVector.flatMap(descendants)
    case _         => Vector(n)

  fxStage.test("Results mounts24served participants with exact refs and separate run counts") {
    fx =>
      val w = boot(fx, StoryModels.t3Summary, StoryMoment.T3)
      loaded(fx, w)
      val rows   = runOnFx(w.summary.participants.rows)
      val report = runOnFx(w.summary.summary.reports((StoryModels.sigma2, true)))
        .asInstanceOf[ReportAnswer.Answered]
        .report
      assert(rows.forall(row => report.participants.exists(_.ref == row.ref)))
      assertEquals(rows.find(_.participant == "P05").get.accounting, "17 / 20 · 3 failed")
      val node = w.summary.participants.node
      assertEquals(runOnFx(node.getParent.getId), "pane-compare.participants")
      assert(runOnFx(node.isVisible))
      assert(runOnFx(CompositeFocus.isPaneContent(node)))
      assertEquals(
        runOnFx(descendants(node).filter(_.isFocusTraversable).map(_.getClass.getSimpleName)),
        Vector.empty
      )
      runOnFx {
        node.applyCss()
        assertEquals(node.getPadding.getLeft, 4.0)
        val first = node.lookup(".participants-row").asInstanceOf[javafx.scene.layout.Region]
        assertEquals(first.getPadding.getLeft, 6.0)
      }
      fx.snapshot(StudioTheme.Light)
      val p17 = rows.find(_.participant == "P17").get
      runOnFx(w.summary.participants.open(p17.ref))
      eventually(fx, "exact overall ref on the trail") {
        w.runtime.model.location.trail.lastOption.contains(Place.At(p17.ref))
      }
      assertEquals(
        runOnFx(w.runtime.model.location.trail.head),
        Place.Summary(StoryModels.reporting)
      )
      assert(runOnFx(w.summary.participants.rows.find(_.ref == p17.ref).exists(_.selected)))
      eventually(fx, "participant query group opens with all20queries") {
        w.summary.navigatorVM.groups.exists(g =>
          g.label == "P17" && g.open && g.entries.size == 20
        )
      }
  }

  fxStage.test(
    "one pane Tab stop carries the row cursor; arrows and Enter activate exact cells"
  ) { fx =>
    val w = boot(fx, StoryModels.t3Summary, StoryMoment.T3)
    loaded(fx, w)
    val pane = runOnFx(w.summary.participants.node.getParent)
    runOnFx(KeyboardFocus.byKeyboard(pane))
    assert(runOnFx(pane.isFocusVisible))
    assert(!runOnFx(CompositeFocus.hasNoCursor(pane)))
    runOnFx {
      pane.applyCss()
      val cursor = w.summary.participants.node
        .lookup(".participants-cursor")
        .asInstanceOf[javafx.scene.layout.Region]
      val c = eyes4s.studio.app.tokens.Tokens.themed(
        eyes4s.studio.app.tokens.Theme.Light,
        eyes4s.studio.app.tokens.ThemedToken.Accent
      )
      val accent = javafx.scene.paint.Color.rgb(c.red, c.green, c.blue, c.alphaPercent / 100.0)
      assertEquals(cursor.getBorder.getStrokes.get(0).getTopStroke, accent)
      KeyboardFocus.byPointer(pane)
      pane.applyCss()
      assertEquals(
        cursor.getBorder.getStrokes.get(0).getTopStroke,
        javafx.scene.paint.Color.TRANSPARENT
      )
      KeyboardFocus.byKeyboard(pane)
    }
    fx.robot.press(KeyCode.END)
    fx.awaitLayout()
    val last = runOnFx(w.summary.participants.rows.last)
    assert(last.cursor)
    assert(runOnFx(w.summary.participants.stopText).exists(_.contains(last.spoken)))
    fx.robot.press(KeyCode.ENTER)
    eventually(fx, "activated last participant") {
      w.runtime.model.location.trail.lastOption.contains(Place.At(last.ref)) &&
      w.summary.participants.rows.filter(_.selected).map(_.participant) == Vector(
        last.participant
      ) &&
      w.summary.navigatorVM.groups.exists(g =>
        g.label == last.participant && g.open && g.entries.size == 20
      )
    }
  }

  fxStage.test("changing scale clears old cells while refused reports expose their reason") {
    fx =>
      val w = boot(fx, StoryModels.t3Summary, StoryMoment.T3)
      loaded(fx, w)
      runOnFx(
        w.summary.dispatch(
          SummaryIntent.ReportRead(
            w.summary.summary.run.get,
            eyes4s.studio.app.compare.CompareSummary.overall(w.summary.summary.spec.get).get,
            StoryModels.sigma2,
            true,
            ReportAnswer.Failed("overall report unavailable")
          )
        )
      )
      assertEquals(
        runOnFx(w.summary.participants.emptyText),
        Some("overall report unavailable")
      )
      assertEquals(runOnFx(w.summary.participants.rows.size), 0)
  }

  fxStage.test(
    "Results Tab order has one Participants stop and is recorded from actual traversal"
  ) { fx =>
    val w = boot(fx, StoryModels.t3Summary, StoryMoment.T3)
    loaded(fx, w)
    eventually(fx, "all scale reports have answered") {
      w.summary.vm.scales.forall(_.available)
    }
    val first = runOnFx(w.shell.appBar.project)
    runOnFx(first.requestFocus())
    fx.awaitLayout()
    val visited = Vector.newBuilder[(javafx.scene.AccessibleRole, String)]
    var count   = 0
    while count == 0 || !runOnFx(fx.scene.getFocusOwner eq first) do
      assert(count < 80, "Results traversal did not return to the project chip")
      visited += runOnFx {
        val current = fx.scene.getFocusOwner
        (current.getAccessibleRole, current.getAccessibleText)
      }
      fx.robot.press(KeyCode.TAB)
      count += 1
    val stops        = visited.result()
    val participants = stops.filter((role, name) =>
      role == javafx.scene.AccessibleRole.LIST_VIEW && name.startsWith("Participants,")
    )
    assertEquals(participants.size, 1)
    assert(participants.head._2.contains("P01, overall D +0.42, n 19"))
    assert(
      stops.forall((role, name) =>
        role != javafx.scene.AccessibleRole.NODE && name != null && name.nonEmpty
      )
    )
    val in = getClass.getClassLoader.getResourceAsStream("eyes4s/studio/desktop/build-root.txt")
    val root = try
      java.nio.file.Paths
        .get(String(in.readAllBytes(), java.nio.charset.StandardCharsets.UTF_8).trim)
    finally in.close()
    val file = root.resolve("docs/studio/a11y/tab-order-results.txt")
    // Annotation slots depend on the actual native window size. Keep the complete
    // spoken text as runtime evidence; compare stable stop order and descriptions.
    val evidence =
      root.resolve("target/studio-snapshots/ParticipantsNavigatorFxSuite/tab-order.json")
    java.nio.file.Files.createDirectories(evidence.getParent)
    java.nio.file.Files.writeString(
      evidence,
      ujson.write(
        ujson.Obj(
          "logicalWidth"  -> fx.sceneSize._1,
          "logicalHeight" -> fx.sceneSize._2,
          "stops"         -> ujson.Arr.from(
            stops.map((role, name) => ujson.Obj("role" -> role.toString, "name" -> name))
          )
        ),
        indent = 2
      ) + "\n"
    )
    val text = stops
      .map { (role, name) =>
        val stable =
          if role == javafx.scene.AccessibleRole.PARENT && name.startsWith(
              "Participant plot of "
            )
          then
            assert(name.contains(". Annotations: ") && name.contains(". One focus stop;"))
            name.take(
              name.indexOf(". Annotations: ")
            ) + ". Annotations: [responsive annotations recorded in tab-order.json]" + name
              .substring(name.indexOf(". One focus stop;"))
          else name
        s"$role: $stable"
      }
      .mkString("", "\n", "\n")
    if sys.env.get("EYES4S_UPDATE_GOLDENS").contains("1") then
      java.nio.file.Files.writeString(file, text): Unit
    assert(java.nio.file.Files.isRegularFile(file), s"Missing $file")
    assertEquals(java.nio.file.Files.readString(file), text)
  }
