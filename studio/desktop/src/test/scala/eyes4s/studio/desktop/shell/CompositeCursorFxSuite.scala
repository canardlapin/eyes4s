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

import eyes4s.studio.app.Intent
import eyes4s.studio.app.plot.{
  ColumnFormat,
  ColumnId,
  PlotColumn,
  PlotRow,
  PlotSource,
  PlotValue
}
import eyes4s.studio.app.tokens.{Theme, ThemedToken, Tokens}
import eyes4s.studio.core.selection.{SelectionState, StudioRef, ViewId}
import eyes4s.studio.desktop.StudioStyles
import eyes4s.studio.desktop.analysis.ResolvedDesignView
import eyes4s.studio.desktop.explore.TrialsNavigatorView
import eyes4s.studio.desktop.harness.StudioFxSuite
import eyes4s.studio.desktop.plot.TableTwinView
import javafx.collections.FXCollections
import javafx.css.PseudoClass
import javafx.scene.Parent
import javafx.scene.control.{ListCell, ListView}
import javafx.scene.layout.{HBox, Region, StackPane, VBox}
import javafx.scene.paint.Color

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*

/** Composite focus (DESIGN_SPEC section 15, REVIEW ticket 12): a list, a
  * tree or a table is one focus stop whose container draws no ring; its
  * cursor row draws a 2 px accent frame around its own fill, and a cursor
  * row that is also selected keeps the selection fill inside the frame.
  *
  * As in A11yTreeSuite, `:focused` is set explicitly on the focus stop:
  * JavaFX shows it only while the window has focus, which a headless run
  * cannot guarantee. Each check reads the rendered background fills.
  */
class CompositeCursorFxSuite extends StudioFxSuite:

  private val Focused = PseudoClass.getPseudoClass("focused")

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private def colour(t: ThemedToken): Color =
    val c = Tokens.themed(Theme.Light, t)
    Color.rgb(c.red, c.green, c.blue, c.alphaPercent / 100.0)

  private val accent  = colour(ThemedToken.Accent)
  private val surface = colour(ThemedToken.Surface)
  private val selFill = colour(ThemedToken.SelFill)

  /** The background fills of `r` as (colour, top inset). */
  private def fills(r: Region): List[(Color, Double)] =
    Option(r.getBackground).toList.flatMap(
      _.getFills.asScala.toList.map(f => (f.getFill.asInstanceOf[Color], f.getInsets.getTop))
    )

  /** `stop` drawn with and without keyboard focus; `read` runs after CSS. */
  private def drawn[A](stop: Region, focused: Boolean)(read: => A): A = runOnFx {
    stop.pseudoClassStateChanged(Focused, focused)
    stop.applyCss()
    read
  }

  private def themed(content: Parent, sheets: String*): StackPane = runOnFx {
    val root = StackPane(content)
    root.getStylesheets.setAll(right(StudioStyles.stylesheets(Theme.Light))*)
    sheets.foreach(s =>
      Option(getClass.getClassLoader.getResource(s))
        .foreach(url => root.getStylesheets.add(url.toExternalForm))
    )
    root.setPrefSize(480, 320)
    root
  }

  private def cell(list: ListView[String], index: Int): ListCell[?] = runOnFx {
    list
      .lookupAll(".list-cell")
      .asScala
      .collectFirst { case c: ListCell[?] if c.getIndex == index => c }
      .getOrElse(fail(s"no cell for row $index"))
  }

  /** A list with the focus model's cursor on row 0. */
  private def list(
      fx: eyes4s.studio.desktop.harness.FxStage,
      styles: String*
  ): ListView[String] =
    val l = runOnFx {
      val l = ListView(FXCollections.observableArrayList("enc_01", "enc_02", "enc_03"))
      l.getStyleClass.addAll(styles*)
      l
    }
    fx.show(themed(l, TrialsNavigatorView.stylesheetResource))
    fx.awaitLayout()
    runOnFx(l.getFocusModel.focus(0))
    l

  fxStage.test("a focused list draws no ring; its cursor row draws the accent frame") { fx =>
    Vector(Vector.empty[String], Vector("nav-rows")).foreach { styles =>
      val l     = list(fx, styles*)
      val clue  = s"list ${styles.mkString(" ")}"
      val box   = drawn(l, focused = false)(fills(l))
      val row   = cell(l, 0)
      val other = cell(l, 1)
      assertEquals(drawn(l, focused = true)(fills(l)), box, s"$clue: the container's ring")
      assert(!box.exists(_._1 == accent), s"$clue: $box")
      assertEquals(
        drawn(l, focused = true)(fills(row)),
        List(accent -> 0.0, surface -> 2.0),
        clue
      )
      assert(!drawn(l, focused = true)(fills(other)).exists(_._1 == accent), clue)
      // The cursor shows only while the list has focus.
      assert(!drawn(l, focused = false)(fills(row)).exists(_._1 == accent), clue)
      // Cursor and selection: the selection fill inside the frame.
      runOnFx(l.getSelectionModel.select(0))
      runOnFx(l.getFocusModel.focus(0))
      assertEquals(
        drawn(l, focused = true)(fills(row)),
        List(accent -> 0.0, selFill -> 2.0),
        clue
      )
    }
  }

  private val view = right(ViewId.of("test.composite"))

  private val pairs: PlotSource = right(
    PlotSource(
      "Pairs",
      Vector(
        PlotColumn(right(ColumnId.of("query")), "Query", ColumnFormat.Label),
        PlotColumn(right(ColumnId.of("cosine")), "Cosine", ColumnFormat.Decimal(2))
      ),
      Vector.tabulate(6)(i =>
        PlotRow(
          StudioRef.Participant(f"P$i%02d"),
          Vector(PlotValue.Text(f"P$i%02d"), PlotValue.Number(i / 10.0))
        )
      )
    )
  )

  fxStage.test("a focused table draws no ring; its cursor row draws the accent frame") { fx =>
    val intents = ArrayBuffer.empty[Intent]
    val table   = runOnFx {
      val t = TableTwinView.attach(view, SelectionState.empty, intents += _)
      t.show(pairs)
      t
    }
    fx.show(themed(table))
    fx.awaitLayout()
    runOnFx(table.moveCursor(Some(pairs.rows(2).ref)))
    fx.awaitLayout()
    def row(i: Int): Region      = runOnFx(table.rowNode(i).getOrElse(fail(s"no row $i")))
    def border(focused: Boolean) = drawn(table, focused)(
      Option(table.getBorder).toList.flatMap(
        _.getStrokes.asScala.toList.map(_.getTopStroke)
      )
    )
    assertEquals(
      drawn(table, focused = true)(fills(table)),
      drawn(table, focused = false)(fills(table))
    )
    assert(!border(focused = true).contains(accent), border(focused = true))
    assertEquals(
      drawn(table, focused = true)(fills(row(2))),
      List(accent -> 0.0, surface -> 2.0)
    )
    assert(!drawn(table, focused = true)(fills(row(1))).exists(_._1 == accent))
    assert(!drawn(table, focused = false)(fills(row(2))).exists(_._1 == accent))
    // Enter selects the cursor row; the bus projects it back.
    runOnFx(table.requestFocus())
    fx.robot.press(javafx.scene.input.KeyCode.ENTER)
    val input = intents.collectFirst { case Intent.Select(i) => i }.getOrElse(fail("no Select"))
    runOnFx(table.project(right(SelectionState.empty.submit(input))))
    fx.awaitLayout()
    assertEquals(
      drawn(table, focused = true)(fills(row(2))),
      List(accent -> 0.0, selFill -> 2.0)
    )
    runOnFx(table.dispose())
  }

  private val CursorRow = PseudoClass.getPseudoClass("cursor")
  private val Selected  = PseudoClass.getPseudoClass("selected")

  private def rows(n: Int, style: String): Vector[HBox] = runOnFx {
    Vector.fill(n) {
      val r = HBox()
      r.getStyleClass.add(style)
      r.setMinHeight(22.0)
      r
    }
  }

  fxStage.test("the design table and the source records draw the cursor, not the stop") { fx =>
    val design = rows(3, "design-row")
    val table  = runOnFx {
      val t = StackPane(VBox(design*))
      t.getStyleClass.add("design-table")
      t
    }
    val records = rows(3, "records-row")
    val pane    = runOnFx {
      val p = VBox(records*)
      p.getStyleClass.add("pane-placeholder")
      p
    }
    fx.show(
      themed(
        VBox(table, pane),
        ResolvedDesignView.stylesheetResource,
        TableTwinView.stylesheetResource
      )
    )
    fx.awaitLayout()
    runOnFx {
      design(1).pseudoClassStateChanged(CursorRow, true)
      records(1).pseudoClassStateChanged(CursorRow, true)
    }
    // The design table: no ring on the stop, the frame on the cursor row.
    assertEquals(
      drawn(table, focused = true)(fills(table)),
      drawn(table, focused = false)(fills(table))
    )
    assertEquals(
      drawn(table, focused = true)(fills(design(1))),
      List(accent -> 0.0, surface -> 2.0)
    )
    assert(!drawn(table, focused = true)(fills(design(0))).exists(_._1 == accent))
    assert(!drawn(table, focused = false)(fills(design(1))).exists(_._1 == accent))
    // The records: their cursor shows while the hosting pane has focus.
    assertEquals(
      drawn(pane, focused = true)(fills(records(1))),
      List(accent -> 0.0, surface -> 2.0)
    )
    assert(!drawn(pane, focused = false)(fills(records(1))).exists(_._1 == accent))
    runOnFx(records(1).pseudoClassStateChanged(Selected, true))
    assertEquals(
      drawn(pane, focused = true)(fills(records(1))),
      List(accent -> 0.0, selFill -> 2.0)
    )
  }
