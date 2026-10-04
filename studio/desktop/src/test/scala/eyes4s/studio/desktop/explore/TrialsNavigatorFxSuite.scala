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

package eyes4s.studio.desktop.explore

import eyes4s.studio.app.StoryModels
import eyes4s.studio.app.explore.{NavigatorRowKind, NavigatorRowVM, TrialGlyph}
import eyes4s.studio.app.layout.StudioLayouts
import eyes4s.studio.app.nav.Place
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.core.fixture.{MockStudy, StoryMoment}
import eyes4s.studio.core.selection.StudioRef
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.FxStage
import eyes4s.studio.desktop.shell.ShellFxSuite
import javafx.scene.control.{Label, ListCell}
import javafx.scene.input.{KeyCode, KeyEvent}

import scala.jdk.CollectionConverters.*

/** The trials navigator hosted in Explore's Trials and Items panes (ticket
  * S6.1; Explore.dc.html, left), in a studio window over the fake backend at
  * story moment t2: P16 has one trial quarantined, P17 ret_09 and one P18
  * trial are absent; P17's enc_03 is on Explore's trail and selected; the
  * glyphs show each trial's display or that it was not admitted.
  *
  * Every interaction goes through the controls' own events, so no test
  * depends on OS window focus.
  */
class TrialsNavigatorFxSuite extends ShellFxSuite:

  private val p17ret09 = MockStudy.key("P17", "ret_09")

  /** A window at t2 in Explore with the trials and displays read. */
  private def ready(fx: FxStage): StudioWindow =
    assumeFullStage(fx)
    val w = boot(fx, StoryModels.t2Explore, StoryMoment.T2)
    eventually(fx, "the navigator's trials and displays") {
      val s = w.navigator.state
      s.entries.toOption.isDefined && s.displays.toOption.isDefined
    }
    w

  private def items(v: TrialsNavigatorView): Vector[NavigatorRowVM] =
    runOnFx(v.rows.getItems.asScala.toVector)

  private def press(fx: FxStage, v: TrialsNavigatorView, code: KeyCode): Unit =
    runOnFx(
      v.rows.fireEvent(KeyEvent(KeyEvent.KEY_PRESSED, "", "", code, false, false, false, false))
    )
    fx.awaitLayout()

  /** The drawn cell of `row`, scrolled into view. */
  private def cell(fx: FxStage, v: TrialsNavigatorView, row: NavigatorRowVM): ListCell[?] =
    runOnFx(v.rows.scrollTo(row))
    fx.awaitLayout()
    runOnFx(
      v.rows
        .lookupAll(".list-cell")
        .asScala
        .collect { case c: ListCell[?] if c.getItem == row => c }
        .headOption
    ).getOrElse(fail(s"no cell drawn for ${row.label}"))

  private def texts(c: ListCell[?]): Vector[String] = runOnFx(
    c.lookupAll(".label")
      .asScala
      .collect { case l: Label => l.getText }
      .filter(_.nonEmpty)
      .toVector
  )

  fxStage.test("t2: the board's statuses, P17 ret_09 absent, enc_03 selected") { fx =>
    val w    = ready(fx)
    val pane = runOnFx(w.host.node(StudioLayouts.trials))
      .getOrElse(fail("the Trials pane was not built"))
    assert(runOnFx(pane.getScene eq fx.scene))
    val v                 = w.navigator.trials
    val rows              = items(v)
    def header(p: String) =
      rows.find(r => r.kind == NavigatorRowKind.Participant && r.label == p).get
    assertEquals(
      Vector("P16", "P17", "P18").map(p => p -> header(p).detail),
      Vector("P16" -> "40 · 1 quar.", "P17" -> "40 · 1 absent", "P18" -> "40 · 1 absent")
    )
    // The list selects Explore's trial.
    val selected = runOnFx(v.rows.getSelectionModel.getSelectedItem)
    assertEquals(selected.ref, Some(StudioRef.Trial(StoryModels.p17enc03)))
    val enc03 = cell(fx, v, selected)
    assertEquals(texts(enc03).toSet, Set("enc_03", "beach-042"))
    assert(runOnFx(enc03.getStyleClass.contains("current")))
    assert(runOnFx(enc03.lookup(".nav-glyph").getStyleClass.contains("glyph-image")))
    // ret_09 is absent: its own row under the collapsed retrieval run.
    val ret09 = rows.find(_.ref.contains(StudioRef.Trial(p17ret09))).get
    val c09   = cell(fx, v, ret09)
    assertEquals(texts(c09).toSet, Set("ret_09", "market-066", "absent · no records"))
    assertEquals(
      runOnFx(c09.getAccessibleText),
      "ret_09 market-066, absent: in trials.csv, no fixation records"
    )
    assert(runOnFx(c09.getStyleClass.contains("warn")))
    assert(runOnFx(c09.lookup(".nav-glyph").getStyleClass.contains("glyph-not-admitted")))
    val range = rows.find(_.kind == NavigatorRowKind.Range).get
    assertEquals((range.label, range.item), ("ret_01–20", "19 admitted"))
    val rangeCell = cell(fx, v, range)
    assert(runOnFx(rangeCell.lookup(".nav-glyph").getStyleClass.contains("glyph-blank-cross")))
    assertEquals(
      runOnFx(v.footer.getText),
      "Explore is view-only. Nothing here changes an analysis."
    )
    assertEquals(runOnFx(v.rows.getAccessibleText), "Trials of r3")
    assertEquals(runOnFx(v.filter.getAccessibleText), "Filter trials")
    assertEquals(runOnFx(v.filter.getPromptText), "Filter participant, trial, item")
    assertEquals(
      runOnFx(w.navigator.trialsStops).map(_.render),
      Vector("text-field: Filter trials", "list: Trials of r3")
    )
  }

  fxStage.test("Enter on a trial explores it; Enter on a group opens it") { fx =>
    val w     = ready(fx)
    val v     = w.navigator.trials
    val ret09 = items(v).find(_.ref.contains(StudioRef.Trial(p17ret09))).get
    runOnFx(v.rows.getSelectionModel.select(ret09))
    press(fx, v, KeyCode.ENTER)
    val m = runOnFx(w.runtime.model)
    assertEquals(m.perspective, Perspective.Explore)
    assertEquals(m.location.trail.lastOption, Some(Place.At(StudioRef.Trial(p17ret09))))
    // The selection follows Explore's trial; the retrieval phase opens on it.
    assertEquals(
      runOnFx(v.rows.getSelectionModel.getSelectedItem).ref,
      Some(StudioRef.Trial(p17ret09))
    )
    // Encoding, which no longer holds it, closes to its run.
    assertEquals(
      items(v).filter(_.kind == NavigatorRowKind.Range).map(_.label),
      Vector("enc_01–20")
    )
    // P16 opens with Right and closes with Left; the list stays on P16.
    def onP16 = runOnFx(Option(v.rows.getSelectionModel.getSelectedItem).map(_.label))
    val p16 = items(v).find(r => r.kind == NavigatorRowKind.Participant && r.label == "P16").get
    runOnFx(v.rows.getSelectionModel.select(p16))
    press(fx, v, KeyCode.RIGHT)
    assertEquals(items(v).find(_.label == "P16").flatMap(_.open), Some(true))
    assertEquals(onP16, Some("P16"))
    press(fx, v, KeyCode.LEFT)
    assertEquals(items(v).find(_.label == "P16").flatMap(_.open), Some(false))
    assertEquals(onP16, Some("P16"))
  }

  fxStage.test("the user's row keeps the selection: Right twice on P16 stays on P16") { fx =>
    val w        = ready(fx)
    val v        = w.navigator.trials
    def selected = runOnFx(Option(v.rows.getSelectionModel.getSelectedItem))
    val p16 = items(v).find(r => r.kind == NavigatorRowKind.Participant && r.label == "P16").get
    runOnFx(v.rows.getSelectionModel.select(p16))
    press(fx, v, KeyCode.RIGHT)
    assertEquals(selected.map(r => (r.label, r.open)), Some(("P16", Some(true))))
    press(fx, v, KeyCode.RIGHT)
    assertEquals(selected.map(r => (r.label, r.open)), Some(("P16", Some(true))))
    // A row without a trial keeps it too: P16's first closed run.
    val range = items(v).find(_.kind == NavigatorRowKind.Range).get
    runOnFx(v.rows.getSelectionModel.select(range))
    press(fx, v, KeyCode.LEFT)
    assertEquals(selected.map(_.kind), Some(NavigatorRowKind.Range))
    // Opening it moves the user to its phase's row, not to Explore's trial.
    press(fx, v, KeyCode.RIGHT)
    assertEquals(
      selected.map(r => (r.kind, r.ref, r.open)),
      Some((NavigatorRowKind.Phase, range.ref, Some(true)))
    )
  }

  fxStage.test("the filter lists matching trials; the Items pane opens an item") { fx =>
    val w = ready(fx)
    val v = w.navigator.trials
    runOnFx(v.filter.setText("ret_09"))
    fx.awaitLayout()
    val trials = items(v).filter(_.kind == NavigatorRowKind.Trial)
    assert(trials.nonEmpty)
    assert(trials.forall(_.label == "ret_09"), trials.map(_.label))
    assertEquals(runOnFx(w.navigator.state.filter), "ret_09")
    // The Items pane, shown in the navigator's group.
    runOnFx(w.runtime.dispatch(eyes4s.studio.app.Intent.FocusPane(StudioLayouts.items)))
    fx.awaitLayout()
    val i = w.navigator.items
    runOnFx(i.filter.setText("forest-044"))
    fx.awaitLayout()
    val item = items(i).find(_.kind == NavigatorRowKind.Item).get
    assertEquals(item.label, "forest-044")
    assert(item.detail.endsWith("images missing"), item.detail)
    runOnFx(i.rows.getSelectionModel.select(item))
    press(fx, i, KeyCode.ENTER)
    val opened = items(i).filter(_.kind == NavigatorRowKind.Trial)
    assert(opened.nonEmpty)
    assert(opened.exists(_.glyph.contains(TrialGlyph.MissingAsset)), opened)
    val missing = cell(fx, i, opened.find(_.glyph.contains(TrialGlyph.MissingAsset)).get)
    assert(runOnFx(missing.lookup(".nav-glyph").getStyleClass.contains("glyph-missing")))

  }
