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
import eyes4s.studio.app.compare.{CompareSummary, NavigatorKind, ReportAnswer}
import eyes4s.studio.app.nav.Place
import eyes4s.studio.core.backend.{Phase, TrialKey}
import eyes4s.studio.core.fixture.{StoryMoment, StoryMoments}
import eyes4s.studio.core.selection.StudioRef
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.{FxStage, StudioTheme}
import eyes4s.studio.desktop.shell.ShellFxSuite
import javafx.scene.input.KeyCode
import javafx.scene.control.ToggleButton
import scala.jdk.CollectionConverters.*

import scala.concurrent.duration.Duration

/** Compare's Queries and Items navigators in the studio window (ticket
  * S8.1; Main.dc.html, left) at t2: the count strip is 480 = 454 + 3 + 9 +
  * 14 with by design n/a, P17's queries show their D at 2° or their status,
  * a click opens a query, the filter narrows, and a project with no run
  * says so.
  */
class QueriesNavigatorFxSuite extends ShellFxSuite:

  override val munitTimeout: Duration = Duration(180, "s")

  private def loaded(fx: FxStage, w: StudioWindow): Unit =
    eventually(fx, "the 2° report is served")(
      w.summary.vm.scales.exists(c => c.scale == StoryModels.sigma2 && c.available)
    )
    runOnFx {
      w.summary.participantNode
        .lookupAll(".toggle-button")
        .asScala
        .collectFirst {
          case b: ToggleButton if b.getText == "σ 2°" => b
        }
        .getOrElse(fail("no 2° control"))
        .fire()
    }
    // Participant headers use the separate overall evaluation, not the
    // grouped report that enables the scale control or the query rows.
    eventually(fx, "the overall participant report at 2° is served") {
      val summary = w.summary.summary
      summary.reports.get((StoryModels.sigma2, true)).exists {
        case ReportAnswer.Answered(view) =>
          summary.run.contains(view.run) && view.scale == StoryModels.sigma2.value &&
          summary.spec.flatMap(CompareSummary.overall).exists(_.id == view.reporting)
        case _ => false
      }
    }
    eventually(fx, "the run's queries are read") {
      w.summary.queries.stripLines.nonEmpty && w.summary.queries.rows.nonEmpty
    }

  fxStage.test("the count strip is 480 = 454 + 3 + 9 + 14, by design n/a") { fx =>
    val w = boot(fx, StoryModels.t2Compare, StoryMoment.T2)
    loaded(fx, w)
    val strip = runOnFx(w.summary.queries.stripLines)
    assertEquals(
      strip,
      Vector(
        ("Query contrasts requested", "480"),
        ("Contributing", "454"),
        ("Failed (empty map)", "3"),
        ("No match · query not admitted", "9 · 14"),
        ("By design (n/a for this preset)", "n/a")
      )
    )
    val counts = strip.take(3).map(_._2.toInt) ++ strip(3)._2.split(" · ").map(_.toInt)
    assertEquals(counts.head, counts.tail.sum)
    fx.snapshot(StudioTheme.Light)
  }

  fxStage.test(
    "participants list their queries with D at 2° or status; a click opens a query"
  ) { fx =>
    val w = boot(fx, StoryModels.t2Compare, StoryMoment.T2)
    loaded(fx, w)
    val rows = runOnFx(w.summary.queries.rows)
    // 24 participant headers; P17 is open, holding the trail's query.
    assertEquals(rows.count(r => r.size == 1), 24)
    assert(rows.contains(Vector("P17  19 of 20 · +0.38")), rows.take(3))
    assert(rows.contains(Vector("ret_07", "beach-042", "+0.38")), rows)
    assert(rows.contains(Vector("ret_09", "market-066", "not admitted")), rows)
    // Zero-centred ink bars: positive D starts at the zero line and runs right,
    // in proportion to D; negative D runs left and ends at it; no D, no bar.
    runOnFx(w.summary.queries.press("P05"))
    fx.awaitLayout()
    val shown = runOnFx {
      // A report can replace the rows after awaitLayout returns. Lay out and
      // measure in the same FX turn so newly rendered ink has its width.
      w.root.applyCss()
      w.root.layout()
      w.summary.queries.rows.filter(_.size == 3).zip(w.summary.queries.bars)
    }
    def bar(trial: String, d: String) = shown
      .collectFirst { case (Vector(`trial`, _, `d`), b) =>
        b
      }
      .getOrElse(fail(s"no $trial at $d"))
    val (zero, ink) = bar("ret_07", "+0.38")
    val (x07, w07)  = ink.getOrElse(fail("ret_07 has no bar"))
    assert(x07 > zero && w07 > 0.0, (zero, x07, w07))
    assertEqualsDouble(bar("ret_06", "+0.50")._2.get._2 / w07, 0.50 / 0.38, 0.05)
    assertEquals(bar("ret_09", "not admitted")._2, None)
    val negative = shown.filter(_._1(2).startsWith("−"))
    assert(negative.nonEmpty, shown.map(_._1))
    negative.foreach { case (row, (z, b)) =>
      val (x, width) = b.getOrElse(fail(s"$row has no bar"))
      assert(width > 0.0, row)
      assertEqualsDouble(x + width, z, 1e-9)
    }
    // A click on ret_04 opens it: the trail ends at its contrast.
    val ret04 = StudioRef.QueryContrast(
      StoryMoments.run7,
      StoryModels.sigma2,
      TrialKey("P17", Phase.Retrieval, "ret_04", 1)
    )
    runOnFx(w.summary.queries.open(ret04))
    fx.awaitLayout()
    assertEquals(runOnFx(w.runtime.model.location.trail.last), Place.At(ret04))
  }

  fxStage.test(
    "the pane's stop takes the row cursor: Down, Right, Down, Enter opens P01's first query"
  ) { fx =>
    val w = boot(fx, StoryModels.t2Compare, StoryMoment.T2)
    loaded(fx, w)
    val stop = runOnFx(w.summary.queries.node.getParent)
    runOnFx(stop.requestFocus())
    fx.awaitLayout()
    val name = runOnFx(w.summary.queries.stopText)
    Vector(KeyCode.DOWN, KeyCode.RIGHT, KeyCode.DOWN).foreach { k =>
      fx.robot.press(k)
      fx.awaitLayout()
    }
    // The stop announces the row under the cursor after its own name.
    val first = runOnFx(w.summary.navigatorVM.groups.head.entries.head)
    assertEquals(
      runOnFx(w.summary.queries.stopText),
      Some((name.toVector :+ s"P01: ${first.spoken}").mkString(", "))
    )
    fx.robot.press(KeyCode.ENTER)
    fx.awaitLayout()
    val trail = runOnFx(w.runtime.model.location.trail)
    trail.last match
      case Place.At(StudioRef.QueryContrast(_, _, key)) =>
        assertEquals((key.participant, key.trial), ("P01", "ret_01"))
      case other => fail(s"the trail ends at $other")
  }

  fxStage.test("the filter narrows queries and items; a project with no run says so") { fx =>
    val w = boot(fx, StoryModels.t2Compare, StoryMoment.T2)
    loaded(fx, w)
    runOnFx(w.summary.queries.filter.setText("beach-042"))
    fx.awaitLayout()
    // One filter: the Items field shows the text that filters it.
    assertEquals(runOnFx(w.summary.items.filter.getText), "beach-042")
    val shown = runOnFx(w.summary.queries.rows).filter(_.size > 1)
    assert(shown.nonEmpty && shown.forall(_(1) == "beach-042"), shown)
    val items = runOnFx(w.summary.items.rows).filter(_.size == 1)
    assertEquals(items, Vector(Vector("beach-042  " + shown.size + " queries")))
    // The strip stays the run's.
    assertEquals(
      runOnFx(w.summary.queries.stripLines).head,
      ("Query contrasts requested", "480")
    )
    // Enter in the field filters; it does not reach the row cursor, here on
    // a query that Enter on the stop would open.
    runOnFx(w.summary.queries.node.getParent.requestFocus())
    fx.awaitLayout()
    Vector(KeyCode.DOWN, KeyCode.RIGHT, KeyCode.DOWN).foreach(k => fx.robot.press(k))
    assert(
      runOnFx(w.summary.navigatorVM.cursorText(NavigatorKind.Queries))
        .exists(_.contains("beach-042"))
    )
    val trail = runOnFx(w.runtime.model.location.trail)
    runOnFx(w.summary.queries.filter.requestFocus())
    fx.awaitLayout()
    fx.robot.press(KeyCode.ENTER)
    fx.awaitLayout()
    assertEquals(runOnFx(w.runtime.model.location.trail), trail)
    // Clearing it in Items clears it for Queries.
    runOnFx(w.summary.items.filter.setText(""))
    fx.awaitLayout()
    assertEquals(runOnFx(w.summary.queries.filter.getText), "")
    // No run yet.
    val empty = boot(fx, StoryModels.firstRun, StoryMoment.T2)
    assertEquals(
      runOnFx(empty.summary.queries.emptyText),
      Some("No run yet — Open Analysis ⌘3")
    )
  }
