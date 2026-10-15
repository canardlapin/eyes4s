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

import eyes4s.studio.app.StoryModels
import eyes4s.studio.app.layout.StudioLayouts
import eyes4s.studio.app.plot.{
  ColumnFormat,
  ColumnId,
  PlotColumn,
  PlotRow,
  PlotSource,
  PlotValue
}
import eyes4s.studio.app.tokens.{Theme, ThemedToken, Tokens}
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.core.selection.{SelectionState, StudioRef, ViewId}
import eyes4s.studio.desktop.StudioStyles
import eyes4s.studio.desktop.admission.AdmissionLedgerView
import eyes4s.studio.desktop.analysis.ResolvedDesignView
import eyes4s.studio.desktop.data.SourcesPaneHost
import eyes4s.studio.desktop.explore.{
  ExploreTimelineHost,
  ExploreTrialViewPane,
  RecordSources,
  TrialsNavigatorView
}
import eyes4s.studio.desktop.figures.FiguresHost
import eyes4s.studio.desktop.geometry.GeometryPanelView
import eyes4s.studio.desktop.importing.{ImportWizardView, TrialKeyView}
import eyes4s.studio.desktop.plot.{KeyboardWithin, TableTwinView}
import javafx.collections.FXCollections
import javafx.scene.Node
import javafx.scene.control.{
  Button,
  ButtonBase,
  ComboBox,
  Hyperlink,
  ListView,
  TextField,
  ToggleButton
}
import javafx.scene.input.KeyCode
import javafx.scene.layout.{HBox, Region, StackPane, VBox}
import javafx.scene.paint.Color

import java.nio.file.{Files, Paths}
import scala.jdk.CollectionConverters.*

/** Focus-visible (DESIGN_SPEC section 15, REVIEW ticket 14). A control draws
  * its 2 px accent ring only while the keyboard brought the focus
  * (`:focus-visible`); a click or a programmatic focus draws none. A primary
  * action's ring is accent, an on-accent gap and its own accent fill at
  * insets 0, 2, 4. A pressed control takes the hairline fill; a pressed
  * primary action is drawn as at rest. A composite pane draws no ring while
  * its rows carry the cursor, and a composite with no row under its cursor
  * (none yet, or no rows) draws the ring on the container. A pane that
  * draws a canvas's ring on its edge draws it only while the canvas shows
  * its own (`:keyboard-within`), never for a click.
  *
  * Focus is reached as a person reaches it ([[KeyboardFocus]]): by traversal
  * for the keyboard, by `requestFocus()` for a click or the program. Each
  * check reads the rendered background fills and border strokes.
  */
class FocusVisibleFxSuite extends ShellFxSuite:

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private def colour(t: ThemedToken): Color =
    val c = Tokens.themed(Theme.Light, t)
    Color.rgb(c.red, c.green, c.blue, c.alphaPercent / 100.0)

  private val accent   = colour(ThemedToken.Accent)
  private val onAccent = colour(ThemedToken.OnAccent)
  private val hairline = colour(ThemedToken.Hairline)

  /** The background fills of `r` as (colour, top inset), outermost first. */
  private def fills(r: Region): List[(Color, Double)] =
    Option(r.getBackground).toList.flatMap(
      _.getFills.asScala.toList.map(f => (f.getFill.asInstanceOf[Color], f.getInsets.getTop))
    )

  /** The colours of `r`'s border strokes. */
  private def strokes(r: Region): List[Any] =
    Option(r.getBorder).toList.flatMap(_.getStrokes.asScala.toList.map(_.getTopStroke))

  /** Whether `r` draws the accent: as a fill or as a border. */
  private def ringed(r: Region): Boolean =
    fills(r).exists(_._1 == accent) || strokes(r).contains(accent)

  /** `r` after `move` and CSS. */
  private def after[A](r: Region)(move: => Unit)(read: => A): A = runOnFx {
    move
    r.getScene.getRoot.applyCss()
    read
  }

  private def keyed[A](r: Region)(read: => A): A   = after(r)(KeyboardFocus.byKeyboard(r))(read)
  private def pointed[A](r: Region)(read: => A): A = after(r)(KeyboardFocus.byPointer(r))(read)

  private def sheet(resource: String): String =
    Option(getClass.getClassLoader.getResource(resource))
      .fold(fail(s"no $resource on the classpath"))(_.toExternalForm)

  /** `controls` under the studio's sheets and their view's `resource`, as
    * each view adds its own sheet to its node.
    */
  private def group(resource: Option[String], controls: Region*): HBox = runOnFx {
    val g = HBox(4.0, controls*)
    resource.foreach(r => g.getStylesheets.add(sheet(r)))
    g
  }

  private def themed(content: Node*): VBox = runOnFx {
    val root = VBox(4.0, content*)
    root.getStylesheets.setAll(right(StudioStyles.stylesheets(Theme.Light))*)
    root.setPrefSize(1200, 800)
    root
  }

  private def styled[A <: Region](node: A, classes: String*): A = runOnFx {
    node.getStyleClass.addAll(classes*)
    node.setFocusTraversable(true)
    node
  }

  private def button(classes: String*): Button = styled(runOnFx(Button("Export")), classes*)
  private def toggle(classes: String*): ToggleButton =
    styled(runOnFx(ToggleButton("1°")), classes*)
  private def field(classes: String*): TextField =
    styled(runOnFx(TextField("ret_07")), classes*)

  /** Every control rule the sweep gated on `:focus-visible`, one control
    * each, under the sheet of the view that draws it.
    */
  private def controls(): (Vector[(String, Region)], Vector[(String, Button)], Vector[Node]) =
    val plain: Vector[(String, Option[String], Region)] = Vector(
      ("crumb", None, button("crumb")),
      ("icon button", None, button("icon-button")),
      ("bar button", None, button("bar-button")),
      ("chip", None, button("chip")),
      ("perspective", None, toggle("perspective")),
      ("import tab", Some(ImportWizardView.stylesheetResource), toggle("import-tab")),
      (
        "role select",
        Some(ImportWizardView.stylesheetResource),
        styled(runOnFx(ComboBox[String]()), "role-select")
      ),
      ("import field", Some(ImportWizardView.stylesheetResource), field("import-field")),
      ("import button", Some(ImportWizardView.stylesheetResource), button("import-button")),
      ("key toggle", Some(TrialKeyView.stylesheetResource), toggle("key-block", "key-toggle")),
      ("geometry field", Some(GeometryPanelView.stylesheetResource), field("geometry-field")),
      (
        "geometry button",
        Some(GeometryPanelView.stylesheetResource),
        button("geometry-button")
      ),
      (
        "geometry picture",
        Some(GeometryPanelView.stylesheetResource),
        styled(runOnFx(StackPane()), "geometry-picture")
      ),
      ("design chip", Some(ResolvedDesignView.stylesheetResource), toggle("design-chip")),
      ("nav filter", Some(TrialsNavigatorView.stylesheetResource), field("nav-filter")),
      ("nav button", Some(TrialsNavigatorView.stylesheetResource), button("nav-button")),
      (
        "timeline button",
        Some(ExploreTimelineHost.stylesheetResource),
        button("timeline-button")
      ),
      (
        "timeline speed",
        Some(ExploreTimelineHost.stylesheetResource),
        toggle("timeline-speed")
      ),
      ("sources button", Some(SourcesPaneHost.stylesheetResource), button("sources-button")),
      (
        "figures row",
        Some(FiguresHost.stylesheetResource),
        styled(runOnFx(VBox()), "figures-row")
      ),
      (
        "explore toggle",
        Some(ExploreTrialViewPane.stylesheetResource),
        toggle("explore-toggle")
      ),
      (
        "explore button",
        Some(ExploreTrialViewPane.stylesheetResource),
        button("explore-button")
      ),
      (
        "ledger count",
        Some(AdmissionLedgerView.stylesheetResource),
        styled(runOnFx(Hyperlink("12")), "ledger-count")
      ),
      ("ledger button", Some(AdmissionLedgerView.stylesheetResource), button("ledger-button"))
    )
    val primary: Vector[(String, Option[String], Button)] = Vector(
      (
        "import primary",
        Some(ImportWizardView.stylesheetResource),
        button("import-button", "primary")
      ),
      (
        "geometry primary",
        Some(GeometryPanelView.stylesheetResource),
        button("geometry-button", "primary")
      ),
      (
        "ledger primary",
        Some(AdmissionLedgerView.stylesheetResource),
        button("ledger-button", "primary")
      ),
      ("preflight run", Some(TableTwinView.stylesheetResource), button("preflight-run"))
    )
    val groups = (plain ++ primary)
      .groupBy(_._2)
      .toVector
      .map((resource, cs) => group(resource, cs.map(_._3)*))
    (plain.map((n, _, c) => (n, c)), primary.map((n, _, c) => (n, c)), groups)

  fxStage.test("keyboard focus draws a control's ring; a click or the program draws none") {
    fx =>
      val (plain, primary, groups) = controls()
      fx.show(themed(groups*))
      fx.awaitLayout()
      plain.foreach { (name, c) =>
        assert(!pointed(c)(ringed(c)), s"$name: a ring under programmatic focus")
        assert(keyed(c)(ringed(c)), s"$name: no ring under keyboard focus")
      }
      // A pointer-focused primary action is its plain accent fill, no ring.
      primary.foreach { (name, c) =>
        assertEquals(pointed(c)(fills(c)), List(accent -> 0.0), name)
      }
  }

  fxStage.test("a focused primary action: accent, on-accent and accent at insets 0, 2, 4") {
    fx =>
      val (_, primary, groups) = controls()
      fx.show(themed(groups*))
      fx.awaitLayout()
      primary.foreach { (name, c) =>
        assertEquals(
          keyed(c)(fills(c)),
          List(accent -> 0.0, onAccent -> 2.0, accent -> 4.0),
          name
        )
      }
  }

  fxStage.test(
    "a pressed control takes the hairline fill; a pressed primary stays as at rest"
  ) { fx =>
    val (plain, primary, groups) = controls()
    fx.show(themed(groups*))
    fx.awaitLayout()
    val pressable = plain.collect { case (n, b: ButtonBase) => (n, b) }
    assert(pressable.size >= 15, pressable.map(_._1))
    pressable.foreach { (name, b) =>
      val drawn = after(b)(b.arm())(fills(b))
      runOnFx(b.disarm())
      assertEquals(drawn.map(_._1), List(hairline), name)
    }
    primary.foreach { (name, b) =>
      val drawn = after(b)(b.arm())(fills(b))
      runOnFx(b.disarm())
      assertEquals(drawn, List(accent -> 0.0), name)
    }
    // Pressed with keyboard focus (Space on a focused button): the ring
    // comes after the pressed fill, so focus is never hidden.
    val bar  = plain.collectFirst { case ("bar button", b: Button) => b }.get
    val both = after(bar) {
      KeyboardFocus.byKeyboard(bar)
      bar.arm()
    }(fills(bar))
    runOnFx(bar.disarm())
    assertEquals(both.headOption.map(_._1), Some(accent))
  }

  private val view = right(ViewId.of("test.focus-visible"))

  private def source(rows: Int): PlotSource = right(
    PlotSource(
      "Pairs",
      Vector(
        PlotColumn(right(ColumnId.of("query")), "Query", ColumnFormat.Label),
        PlotColumn(right(ColumnId.of("cosine")), "Cosine", ColumnFormat.Decimal(2))
      ),
      Vector.tabulate(rows)(i =>
        PlotRow(
          StudioRef.Participant(f"P$i%02d"),
          Vector(PlotValue.Text(f"P$i%02d"), PlotValue.Number(i / 10.0))
        )
      )
    )
  )

  fxStage.test("a composite with no row under its cursor draws the ring on the container") {
    fx =>
      val table = runOnFx {
        val t = TableTwinView.attach(view, SelectionState.empty, _ => ())
        t.show(source(0))
        t
      }
      val list = runOnFx {
        val l = ListView(FXCollections.observableArrayList[String]())
        CompositeFocus.track(l)
        l
      }
      val design = runOnFx {
        val d = StackPane(VBox())
        d.getStyleClass.add("design-table")
        d.setFocusTraversable(true)
        CompositeFocus.noCursor(d, true)
        d
      }
      fx.show(
        themed(
          runOnFx(Button("away")),
          table,
          list,
          group(Some(ResolvedDesignView.stylesheetResource), design)
        )
      )
      fx.awaitLayout()
      val composites =
        Vector[(String, Region)](("table", table), ("list", list), ("design", design))
      def containerRing(clue: String): Unit = composites.foreach { (name, c) =>
        assert(runOnFx(CompositeFocus.hasNoCursor(c)), s"$clue: $name")
        assert(!pointed(c)(ringed(c)), s"$clue: $name: a ring under programmatic focus")
        assert(keyed(c)(strokes(c)).contains(accent), s"$clue: $name: no ring on the container")
      }
      containerRing("no rows")
      // Rows, but none under the cursor yet.
      runOnFx {
        table.show(source(3))
        list.getItems.addAll("enc_01", "enc_02")
        list.getFocusModel.focus(-1)
      }
      fx.awaitLayout()
      // A table with rows has a home row for focus to put the cursor on, so
      // the row carries the ring and the container none.
      assert(!runOnFx(CompositeFocus.hasNoCursor(table)), "rows without a cursor")
      assert(
        !keyed(table)(strokes(table)).contains(accent),
        "rows without a cursor: container ring"
      )
      // A row under the cursor carries the ring, and the container none.
      runOnFx {
        table.moveCursor(Some(source(3).rows(1).ref))
        list.getFocusModel.focus(0)
        CompositeFocus.noCursor(design, false)
      }
      fx.awaitLayout()
      composites.foreach { (name, c) =>
        assert(!runOnFx(CompositeFocus.hasNoCursor(c)), name)
        assert(!keyed(c)(strokes(c)).contains(accent), s"$name: a ring on the container")
      }
      // Emptied again, the list says so.
      runOnFx(list.getItems.clear())
      assert(runOnFx(CompositeFocus.hasNoCursor(list)))
      runOnFx(table.dispose())
  }

  fxStage.test("a composite pane draws no ring while its rows carry the cursor") { fx =>
    val w     = boot(fx, StoryModels.t2Explore, StoryMoment.T2)
    val pane  = runOnFx(w.host.node(StudioLayouts.sourceRecords).get.asInstanceOf[Region])
    val trial = runOnFx(w.host.node(StudioLayouts.trials).get.asInstanceOf[Region])
    eventually(fx, "the records are read") {
      w.sourceRecords.current.total.exists(_ > 0) && w.sourceRecords.current.cursor.nonEmpty
    }
    fx.awaitLayout()
    assert(runOnFx(pane.getStyleClass.contains(CompositeFocus.PaneStyle)))
    assert(!runOnFx(trial.getStyleClass.contains(CompositeFocus.PaneStyle)))
    // The records pane: no ring of its own; its cursor row draws the frame.
    assert(!keyed(pane)(ringed(pane)), runOnFx((fills(pane), strokes(pane))))
    // The rows are read and rebound as pages arrive: read the cursor row once
    // CSS has caught up with them.
    eventually(fx, "the cursor row draws the frame") {
      pane.getScene.getRoot.applyCss()
      pane
        .lookupAll(".records-row")
        .asScala
        .collect { case r: Region => r }
        .filter(r =>
          r.isVisible && r.getPseudoClassStates.asScala.exists(_.getPseudoClassName == "cursor")
        )
        .exists(r => fills(r).headOption.map(_._1).contains(accent))
    }
    // A pane whose content is not a composite keeps its ring.
    assertEquals(keyed(trial)(fills(trial)).headOption.map(_._1), Some(accent))
    assert(!pointed(trial)(ringed(trial)))
  }

  fxStage.test("a composite pane with no rows draws the ring on its edge") { fx =>
    val w =
      boot(fx, StoryModels.t2Explore, StoryMoment.T2, records = Some(RecordSources.notServed))
    val pane = runOnFx(w.host.node(StudioLayouts.sourceRecords).get.asInstanceOf[Region])
    eventually(fx, "the refusal is shown")(w.sourceRecords.current.total.isEmpty)
    fx.awaitLayout()
    assert(runOnFx(CompositeFocus.hasNoCursor(pane)))
    assert(keyed(pane)(strokes(pane)).contains(accent))
    assert(!pointed(pane)(ringed(pane)))
  }

  fxStage.test("a clicked trial canvas draws no stage ring; Tab, or a key after it, does") {
    fx =>
      val w = boot(fx, StoryModels.t2Explore, StoryMoment.T2)
      eventually(fx, "the trial is drawn")(w.explore.input.targets.nonEmpty)
      val host  = runOnFx(w.explore.trialView.plotHost)
      val stage = runOnFx(
        KeyboardWithin
          .chain(host)
          .collectFirst { case r: Region if r.getStyleClass.contains("explore-stage") => r }
          .getOrElse(fail("the trial canvas is not on the explore stage"))
      )
      def ring: Boolean = after(stage)(())(strokes(stage).contains(accent))
      assume(runOnFx(fx.stage.isFocused), "the test window does not hold OS focus")
      // A click focuses the canvas and draws no ring (DESIGN_SPEC section 15).
      fx.robot.click(host)
      assert(runOnFx(host.isFocused), "a click focuses the trial canvas")
      assert(!ring, "a click drew the stage's ring")
      // A key after the click operates the canvas by keyboard.
      fx.robot.press(KeyCode.RIGHT)
      assert(ring, "a key after the click drew no ring")
      runOnFx(KeyboardFocus.away(host))
      assert(!ring, "the ring outlived the focus")
      // Tab brings the focus: the ring, with no key pressed on the canvas.
      runOnFx(KeyboardFocus.byKeyboard(host))
      assert(ring, "Tab drew no ring")
  }

  /** `(selector, declarations)` of every rule in `css`, comments removed. */
  private def rules(css: String): Vector[(String, String)] =
    css
      .replaceAll("(?s)/\\*.*?\\*/", "")
      .split("}")
      .toVector
      .flatMap(r =>
        r.split("\\{", 2) match
          case Array(selector, body) => Some((selector.trim, body))
          case _                     => None
      )

  test("no stylesheet draws the accent under :focused, which a click sets") {
    val shell =
      Option(getClass.getClassLoader.getResource(StudioStyles.shellStylesheetResource))
        .getOrElse(fail(s"no ${StudioStyles.shellStylesheetResource} on the classpath"))
    val sheets = Files
      .list(Paths.get(shell.toURI).getParent)
      .iterator
      .asScala
      .toVector
      .filter(_.getFileName.toString.endsWith(".css"))
    assert(sheets.size > 10, sheets)
    // A cell's :focused is its list's focus model (the cursor), not focus.
    val focused = """(?<!-cell):focused""".r
    val offends = for
      sheet             <- sheets
      (selectors, body) <- rules(Files.readString(sheet))
      if body.contains("-es-accent")
      selector <- selectors.split(",").toVector.map(_.trim)
      if focused.findFirstIn(selector).nonEmpty
    yield s"${sheet.getFileName}: $selector"
    assertEquals(offends, Vector.empty)
  }

  test("no stylesheet draws the accent under :focus-within, which a click sets") {
    val shell =
      Option(getClass.getClassLoader.getResource(StudioStyles.shellStylesheetResource))
        .getOrElse(fail(s"no ${StudioStyles.shellStylesheetResource} on the classpath"))
    val sheets = Files
      .list(Paths.get(shell.toURI).getParent)
      .iterator
      .asScala
      .toVector
      .filter(_.getFileName.toString.endsWith(".css"))
    // A pane that draws a canvas's ring on its edge uses :keyboard-within.
    val offends = for
      sheet             <- sheets
      (selectors, body) <- rules(Files.readString(sheet))
      if body.contains("-es-accent")
      selector <- selectors.split(",").toVector.map(_.trim)
      if selector.contains(":focus-within")
    yield s"${sheet.getFileName}: $selector"
    assertEquals(offends, Vector.empty)
  }
