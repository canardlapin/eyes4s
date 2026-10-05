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

package eyes4s.studio.desktop.plot

import eyes4s.studio.app.Intent
import eyes4s.studio.app.plot.{
  ColumnFormat,
  ColumnId,
  PlotColumn,
  PlotRow,
  PlotSource,
  PlotValue,
  TableColumns
}
import eyes4s.studio.app.tokens.Theme
import eyes4s.studio.core.selection.{SelectionState, StudioRef, ViewId}
import eyes4s.studio.desktop.StudioStyles
import eyes4s.studio.desktop.harness.StudioFxSuite
import eyes4s.studio.desktop.typography.StudioFonts
import javafx.scene.{AccessibleAttribute, AccessibleRole, Node}
import javafx.scene.control.Labeled
import javafx.scene.layout.StackPane

import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*

/** The Table tab as a table (bead bd-01M3JD4G5D3GF9NHPWANXFS3PX): rows
  * virtualised, the focus stop answering JavaFX's table attributes (rows,
  * columns, the cursor row, the selected rows, the cell at a row and
  * column), the accessible text announced only when it changes, and
  * columns sized to their content with the header and rows aligned.
  */
class TableTwinViewFxSuite extends StudioFxSuite:

  override def beforeAll(): Unit =
    super.beforeAll()
    val problems = runOnFx(StudioFonts.loadAll())
    assert(problems.isEmpty, problems.map(_.message).mkString("\n"))

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private val view = right(ViewId.of("test.table"))

  private def source(n: Int): PlotSource = right(
    PlotSource(
      "Pairs",
      Vector(
        PlotColumn(right(ColumnId.of("query")), "Query", ColumnFormat.Label),
        PlotColumn(right(ColumnId.of("reference")), "Reference trial", ColumnFormat.Label),
        PlotColumn(right(ColumnId.of("cosine")), "Cosine", ColumnFormat.Decimal(2))
      ),
      Vector.tabulate(n)(i =>
        PlotRow(
          StudioRef.Participant(f"P$i%05d"),
          Vector(
            PlotValue.Text(f"P$i%05d"),
            PlotValue.Text(s"enc_${i % 40}"),
            PlotValue.Number((i % 100) / 100.0)
          )
        )
      )
    )
  )

  private def ask(n: Node, a: AccessibleAttribute, ps: AnyRef*): AnyRef =
    runOnFx(n.queryAccessibleAttribute(a, ps*))

  private def int(n: Node, a: AccessibleAttribute): Int =
    ask(n, a).asInstanceOf[Integer].intValue

  private def shown(
      fx: eyes4s.studio.desktop.harness.FxStage,
      src: PlotSource,
      pins: Boolean = false
  ): (TableTwinView, ArrayBuffer[Intent]) =
    val intents = ArrayBuffer.empty[Intent]
    val table   = runOnFx {
      val t = TableTwinView.attach(view, SelectionState.empty, intents += _, pins)
      t.show(src)
      t
    }
    val root = runOnFx {
      val r = StackPane(table)
      r.getStylesheets.setAll(right(StudioStyles.stylesheets(Theme.Light).left.map(_.message))*)
      r.setPrefSize(640, 400)
      r
    }
    fx.show(root)
    (table, intents)

  fxStage.test("pending and cleared tables are not unnamed focus stops") { _ =>
    runOnFx {
      val table = TableTwinView.attach(view, SelectionState.empty, _ => ())
      assert(!table.isFocusTraversable)
      table.show(source(0))
      assert(table.isFocusTraversable)
      assert(Option(table.getAccessibleText).exists(_.trim.nonEmpty))
      table.clear()
      assert(!table.isFocusTraversable)
      table.show(source(1))
      assert(table.isFocusTraversable)
      assert(Option(table.getAccessibleText).exists(_.trim.nonEmpty))
      table.dispose()
    }
  }

  fxStage.test("a long source builds only the rows in view, and answers as a table") { fx =>
    val src        = source(5000)
    val (table, _) = shown(fx, src)
    val built      = runOnFx(table.builtRows)
    assert(built > 0 && built < 60, s"$built row nodes for 5,000 rows")
    assertEquals(runOnFx(table.getAccessibleRole), AccessibleRole.TABLE_VIEW)
    assertEquals(int(table, AccessibleAttribute.ROW_COUNT), 5000)
    assertEquals(int(table, AccessibleAttribute.COLUMN_COUNT), 3)
    assertEquals(ask(table, AccessibleAttribute.MULTIPLE_SELECTION), java.lang.Boolean.TRUE)
    val top = runOnFx(table.topRow)
    assertEquals(top, Some(0))
    // A cell far out of view answers its row and column without scrolling
    // the list (review F1): a reader reads rows in ranges.
    val cell = ask(
      table,
      AccessibleAttribute.CELL_AT_ROW_COLUMN,
      Integer.valueOf(4000),
      Integer.valueOf(2)
    )
      .asInstanceOf[Node]
    assertEquals(runOnFx(cell.getAccessibleRole), AccessibleRole.TABLE_CELL)
    assertEquals(int(cell, AccessibleAttribute.ROW_INDEX), 4000)
    assertEquals(int(cell, AccessibleAttribute.COLUMN_INDEX), 2)
    assertEquals(ask(cell, AccessibleAttribute.TEXT), src.text(4000, 2).get)
    assertEquals(ask(cell, AccessibleAttribute.SELECTED), java.lang.Boolean.FALSE)
    val row =
      ask(table, AccessibleAttribute.ROW_AT_INDEX, Integer.valueOf(4001)).asInstanceOf[Node]
    assertEquals(runOnFx(row.getAccessibleRole), AccessibleRole.TABLE_ROW)
    assertEquals(int(row, AccessibleAttribute.INDEX), 4001)
    assert(
      ask(row, AccessibleAttribute.TEXT).toString.contains("P04001"),
      ask(row, AccessibleAttribute.TEXT).toString
    )
    val header =
      ask(table, AccessibleAttribute.COLUMN_AT_INDEX, Integer.valueOf(1)).asInstanceOf[Labeled]
    assertEquals(runOnFx(header.getText), "Reference trial")
    assertEquals(runOnFx(header.getAccessibleRole), AccessibleRole.TABLE_COLUMN)
    assertEquals(
      ask(
        table,
        AccessibleAttribute.CELL_AT_ROW_COLUMN,
        Integer.valueOf(5000),
        Integer.valueOf(0)
      ),
      null
    )
    assert(runOnFx(table.builtRows) < 60, "answering did not build every row")
    assertEquals(runOnFx(table.topRow), top, "answering scrolled the table")
    assert(!runOnFx(table.inView(4000)))
    // A reader that asks to see a row scrolls to it.
    runOnFx(table.executeAccessibleAction(javafx.scene.AccessibleAction.SHOW_ITEM, row))
    assert(runOnFx(table.inView(4001)), "SHOW_ITEM did not scroll to row 4,001")
    runOnFx(table.dispose())
  }

  fxStage.test(
    "the cursor row is the focus item and the selected rows are the selected items"
  ) { fx =>
    val src              = source(300)
    val (table, intents) = shown(fx, src)
    assertEquals(ask(table, AccessibleAttribute.FOCUS_ITEM), null)
    val focusNews     = runOnFx(table.focusNotified)
    val selectionNews = runOnFx(table.selectionNotified)
    runOnFx(table.moveCursor(Some(src.rows(250).ref)))
    fx.awaitLayout()
    assertEquals(runOnFx(table.focusNotified), focusNews + 1)
    val focus = ask(table, AccessibleAttribute.FOCUS_ITEM).asInstanceOf[Node]
    assertEquals(int(focus, AccessibleAttribute.ROW_INDEX), 250)
    assertEquals(int(focus, AccessibleAttribute.COLUMN_INDEX), 0)
    def selectedRows: Vector[Int] =
      runOnFx(
        table
          .queryAccessibleAttribute(AccessibleAttribute.SELECTED_ITEMS)
          .asInstanceOf[java.util.List[Node]]
          .asScala
          .toVector
      ).map(int(_, AccessibleAttribute.ROW_INDEX))
    assertEquals(selectedRows, Vector.empty)
    // Enter on the cursor row asks the bus; the bus projects it back.
    runOnFx(table.requestFocus())
    fx.robot.press(javafx.scene.input.KeyCode.ENTER)
    val input = intents.collectFirst { case Intent.Select(i) => i }.getOrElse(fail("no Select"))
    runOnFx(table.project(right(SelectionState.empty.submit(input))))
    fx.awaitLayout()
    assertEquals(selectedRows, Vector(250))
    assertEquals(runOnFx(table.selectionNotified), selectionNews + 1)
    // The re-render restyled the rows in place: the focus item a reader holds
    // is the same node, and still in the scene (review F2).
    val after = ask(table, AccessibleAttribute.FOCUS_ITEM).asInstanceOf[Node]
    assert(after eq focus, "the focus item's node changed on a re-render")
    assert(runOnFx(after.getScene) != null)
    assertEquals(runOnFx(table.focusNotified), focusNews + 1)
    // The selected row is drawn as selected.
    assert(
      runOnFx(table.rowNode(250).get.getPseudoClassStates.contains(TableTwinView.Selected)),
      "row 250 is not drawn as selected"
    )
    // A reader scrolls the cursor row out of view: its node is gone, so the
    // focus item is announced again, still row 250.
    val top = ask(table, AccessibleAttribute.ROW_AT_INDEX, Integer.valueOf(10))
    runOnFx(table.executeAccessibleAction(javafx.scene.AccessibleAction.SHOW_ITEM, top))
    fx.awaitLayout()
    assert(!runOnFx(table.inView(250)))
    assertEquals(runOnFx(table.focusNotified), focusNews + 2)
    val moved = ask(table, AccessibleAttribute.FOCUS_ITEM).asInstanceOf[Node]
    assertEquals(int(moved, AccessibleAttribute.ROW_INDEX), 250)
    val cell = ask(
      table,
      AccessibleAttribute.CELL_AT_ROW_COLUMN,
      Integer.valueOf(250),
      Integer.valueOf(1)
    )
      .asInstanceOf[Node]
    assertEquals(ask(cell, AccessibleAttribute.SELECTED), java.lang.Boolean.TRUE)
    runOnFx(table.dispose())
  }

  fxStage.test("the accessible text is announced only when it changes") { fx =>
    val src        = source(20)
    val (table, _) = shown(fx, src)
    val first      = runOnFx(table.textNotified)
    // The same source again, and a projection that changes nothing: no news.
    runOnFx(table.show(src))
    runOnFx(table.project(SelectionState.empty))
    assertEquals(runOnFx(table.textNotified), first)
    runOnFx(table.moveCursor(Some(src.rows(3).ref)))
    assertEquals(runOnFx(table.textNotified), first + 1)
    assert(
      runOnFx(table.getAccessibleText).contains("P00003"),
      runOnFx(table.getAccessibleText)
    )
    runOnFx(table.moveCursor(Some(src.rows(3).ref)))
    assertEquals(runOnFx(table.textNotified), first + 1)
    runOnFx(table.dispose())
  }

  fxStage.test("rows are read from the nodes drawn on screen, and from the state off it") {
    fx =>
      val src       = source(300)
      val offscreen = runOnFx {
        val t = TableTwinView.attach(view, SelectionState.empty, _ => ())
        t.show(src)
        t
      }
      assert(!runOnFx(offscreen.onScreen))
      // Off screen there are no drawn rows to read: only the state's.
      intercept[IllegalStateException](runOnFx(offscreen.rowTexts))
      assertEquals(
        runOnFx(offscreen.modelRowTexts),
        src.rows.indices.toVector.flatMap(src.cells)
      )
      runOnFx(offscreen.dispose())
      val (table, _) = shown(fx, src)
      assert(runOnFx(table.onScreen))
      assertEquals(runOnFx(table.topRow), Some(0))
      assertEquals(runOnFx(table.rowTexts), src.rows.indices.toVector.flatMap(src.cells))
      assertEquals(runOnFx(table.rowSelected), Vector.fill(300)(false))
      // Reading every row scrolled through them all, then back.
      assertEquals(runOnFx(table.topRow), Some(0))
      assert(runOnFx(table.builtRows) < 60)
      runOnFx(table.dispose())
  }

  fxStage.test("the cursor scrolls the list only to a row out of view") { fx =>
    val src                     = source(300)
    val (table, _)              = shown(fx, src)
    def to(i: Int): Option[Int] =
      runOnFx(table.moveCursor(Some(src.rows(i).ref)))
      runOnFx(table.topRow)
    assertEquals(to(5), Some(0))
    val far = to(250).getOrElse(fail("no rows in view"))
    assert(far > 200 && far <= 250, s"row 250's cursor shows from row $far")
    assertEquals(to(far + 3), Some(far))
    runOnFx(table.dispose())
  }

  fxStage.test("columns take their content's shares, and the header and rows align") { fx =>
    val src        = source(5)
    val (table, _) = shown(fx, src)
    val shares     = TableColumns.shares(src)
    assertEquals(runOnFx(table.columnShares), shares)
    def lefts(n: Node): Vector[(Double, Double)] = runOnFx(
      n.asInstanceOf[javafx.scene.Parent]
        .getChildrenUnmodifiable
        .asScala
        .toVector
        .map(c => (c.localToScene(c.getLayoutBounds).getMinX, c.getLayoutBounds.getWidth))
    )
    val head = lefts(ask(table, AccessibleAttribute.HEADER).asInstanceOf[Node])
    val row  = lefts(
      runOnFx(table.rowNode(2).get.getGraphic)
    )
    assertEquals(head.size, 3)
    val total = head.map(_._2).sum
    head.zip(shares).foreach { case ((_, w), share) =>
      assertEqualsDouble(w / total, share, 0.01)
    }
    // Five rows need no scroll bar: the columns line up to the pixel.
    head.zip(row).foreach { case ((hx, hw), (rx, rw)) =>
      assertEqualsDouble(rx, hx, 1.0)
      assertEqualsDouble(rw, hw, 1.0)
    }
    runOnFx(table.dispose())
  }

  fxStage.test(
    "pinned · selected: the selected row is row 0, once, annotated; deselected, it returns"
  ) { fx =>
    val src              = source(300)
    val (table, intents) = shown(fx, src, pins = true)
    runOnFx(table.moveCursor(Some(src.rows(250).ref)))
    runOnFx(table.requestFocus())
    fx.robot.press(javafx.scene.input.KeyCode.ENTER)
    val input = intents.collectFirst { case Intent.Select(i) => i }.getOrElse(fail("no Select"))
    runOnFx(table.project(right(SelectionState.empty.submit(input))))
    fx.awaitLayout()
    def cellText(row: Int, column: Int): String =
      val c = ask(
        table,
        AccessibleAttribute.CELL_AT_ROW_COLUMN,
        Integer.valueOf(row),
        Integer.valueOf(column)
      )
        .asInstanceOf[Node]
      assertEquals(int(c, AccessibleAttribute.ROW_INDEX), row)
      ask(c, AccessibleAttribute.TEXT).toString
    // Row 0 is P00250, pinned; the others follow in order, without it.
    assertEquals(int(table, AccessibleAttribute.ROW_COUNT), 300)
    assertEquals(cellText(0, 0), "P00250, pinned · selected")
    assertEquals(cellText(1, 0), "P00000")
    assertEquals(cellText(251, 0), "P00251")
    assertEquals(runOnFx(table.modelRowTexts.map(_.head).count(_ == "P00250")), 1)
    // It is drawn so: the row node at 0, its annotation shown.
    val top = runOnFx(table.rowNode(0).get)
    assert(runOnFx(top.getPseudoClassStates.contains(TableTwinView.Pinned)))
    assert(
      runOnFx(top.lookupAll(".table-twin-note").asScala.exists {
        case l: javafx.scene.control.Label => l.getText == "pinned · selected" && l.isVisible
        case _                             => false
      }),
      "no annotation drawn"
    )
    // The cursor stays on P00250, now row 0; Down goes to P00000.
    val focus = ask(table, AccessibleAttribute.FOCUS_ITEM).asInstanceOf[Node]
    assertEquals(int(focus, AccessibleAttribute.ROW_INDEX), 0)
    fx.robot.press(javafx.scene.input.KeyCode.DOWN)
    assertEquals(runOnFx(table.state.cursor), Some(src.rows(0).ref))
    // Deselected, P00250 is row 250 again and row 0 is P00000.
    runOnFx(table.project(SelectionState.empty))
    fx.awaitLayout()
    assertEquals(cellText(0, 0), "P00000")
    assertEquals(cellText(250, 0), "P00250")
    runOnFx(table.dispose())
  }
