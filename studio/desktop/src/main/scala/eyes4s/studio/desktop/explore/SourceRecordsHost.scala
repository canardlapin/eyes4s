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

import eyes4s.studio.app.explore.*
import eyes4s.studio.app.plot.ViewSelection
import eyes4s.studio.app.text.{RecordText, RecordTextId}
import eyes4s.studio.app.vm.{A11yRole, FocusStop}
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.selection.ViewId
import eyes4s.studio.desktop.plot.TableTwinView
import javafx.application.Platform
import javafx.collections.FXCollections
import javafx.geometry.Pos
import javafx.scene.control.skin.VirtualFlow
import javafx.scene.control.{Label, ListCell, ListView, ToggleButton}
import javafx.scene.input.{KeyCode, KeyEvent, MouseEvent}
import javafx.scene.layout.{HBox, Priority, Region, VBox}

/** The window's source records sources. */
object RecordSources:

  /** The window's backend (protocol 1.7 `sourceRecords`): rows `from` to
    * `from + size - 1` (from 0) are records `from + 1` onwards, at most a
    * wire page's worth, in the table's terms ([[SourceRecords.served]]).
    */
  def of(session: eyes4s.studio.desktop.runtime.StudioSession): SourceRecordsSource =
    (revision, from, size, done) =>
      val count = math.min(size, eyes4s.studio.core.backend.SourceRecordPage.Limit)
      session.run(session.backend.sourceRecords(revision, from + 1, count)) {
        case Left(e)          => done(Left(Option(e.getMessage).getOrElse(e.toString)))
        case Right(Left(err)) => done(Right(BackendAnswer.Refused(err.message)))
        case Right(Right(p))  => done(Right(BackendAnswer.Answered(SourceRecords.served(p))))
      }

  /** A window with no source records: every page is refused, saying so. */
  val notServed: SourceRecordsSource = (_, _, _, done) =>
    done(Right(BackendAnswer.Refused(RecordText(RecordTextId.NotServed))))

/** Explore's source records table on the desktop (ticket S6.4;
  * Explore.dc.html, bottom; see [[SourceRecords]]). The rows are a
  * virtualised list: only the visible rows have cells, and a page is read
  * when its rows come into view. The pane is one focus stop with a row
  * cursor; 'Show raw record' is the one control inside it. It follows the
  * model and the selection, and reads pages from `source` and a selected
  * fixation's record from `locate`. Use on the JavaFX thread.
  */
final class SourceRecordsHost(
    model: () => AppModel,
    app: Intent => Unit,
    source: SourceRecordsSource,
    locate: TrialViewInputs
):
  private val viewId =
    ViewId
      .of("explore.source-records")
      .fold(e => throw IllegalStateException(e.message), identity)
  private var state    = SourceRecords.initial(ViewSelection.initial(viewId, model().selection))
  private var disposed = false
  private var made     = 0

  private val status = Label()
  status.getStyleClass.addAll("t12", "records-status")
  private val raw = Label()
  raw.getStyleClass.addAll("mono", "t11", "records-raw")
  raw.setWrapText(true)
  private val showRaw = ToggleButton(RecordText(RecordTextId.ShowRaw))
  showRaw.getStyleClass.add("tog")
  showRaw.setAccessibleText(RecordText(RecordTextId.ShowRaw))
  showRaw.setFocusTraversable(true)
  showRaw.setOnAction(_ => dispatch(SourceRecordsIntent.ShowRaw(showRaw.isSelected)))

  // Column widths of Explore.dc.html (bottom), with Degrees added.
  private val widths = Vector(70.0, 50.0, 50.0, 64.0, 56.0, 104.0, 74.0, 100.0, 60.0, 80.0)

  private def cellsBox(texts: Vector[String], style: String): HBox =
    val box = HBox(8.0)
    texts.zip(widths).foreach { (t, w) =>
      val l = Label(t)
      l.getStyleClass.addAll("mono", "t11", style)
      l.setMinWidth(w)
      l.setPrefWidth(w)
      l.setMaxWidth(w)
      box.getChildren.add(l)
    }
    box.setAlignment(Pos.CENTER_LEFT)
    box.setPadding(javafx.geometry.Insets(0, 16, 0, 16))
    box

  private val header = cellsBox(SourceRecords.headers, "records-header")
  header.setMinHeight(24.0)

  private final class RowCell extends ListCell[Integer]:
    made += 1
    private val box    = cellsBox(Vector.fill(widths.size)(""), "records-cell")
    private val labels =
      (0 until widths.size).map(i => box.getChildren.get(i).asInstanceOf[Label])
    setGraphic(null)
    setPrefHeight(22.0)
    getStyleClass.add("records-row")
    addEventHandler(
      MouseEvent.MOUSE_CLICKED,
      (_: MouseEvent) => Option(getItem).foreach(i => dispatch(SourceRecordsIntent.Click(i)))
    )

    override protected def updateItem(item: Integer, empty: Boolean): Unit =
      super.updateItem(item, empty)
      pseudoClassStateChanged(TableTwinView.Cursor, false)
      pseudoClassStateChanged(TableTwinView.Selected, false)
      if empty || item == null then
        setGraphic(null)
        setAccessibleText(null)
      else
        SourceRecords.rowVM(state, item) match
          case SourceRowVM.Shown(cells, cursor, selected, name) =>
            cells.zip(labels).foreach((t, l) => l.setText(t))
            pseudoClassStateChanged(TableTwinView.Cursor, cursor)
            pseudoClassStateChanged(TableTwinView.Selected, selected)
            setAccessibleText(name)
          case SourceRowVM.Reading =>
            labels.foreach(_.setText(""))
            labels.head.setText(RecordText(RecordTextId.Reading))
            setAccessibleText(RecordText(RecordTextId.Reading))
          case SourceRowVM.Failed(why) =>
            labels.foreach(_.setText(""))
            labels.head.setText(why)
            setAccessibleText(why)
        setGraphic(box)

  private val list = ListView[Integer]()
  list.setFixedCellSize(22.0)
  list.setFocusTraversable(false)
  list.setCellFactory(_ => RowCell())
  list.getStyleClass.add("records-list")
  VBox.setVgrow(list, Priority.ALWAYS)

  private val spacer = Region()
  HBox.setHgrow(spacer, Priority.ALWAYS)
  private val toolbar = HBox(8.0, status, spacer, showRaw)
  toolbar.setAlignment(Pos.CENTER_LEFT)
  toolbar.setPadding(javafx.geometry.Insets(2, 16, 2, 16))

  /** The pane's content. */
  val node: VBox = VBox(toolbar, raw, header, list)
  node.getStyleClass.add("source-records")
  Option(getClass.getClassLoader.getResource(TableTwinView.stylesheetResource))
    .foreach(url => node.getStylesheets.add(url.toExternalForm))

  // The pane's own stop (the node hosting this one) takes the cursor's keys.
  // A key on the toggle is the toggle's, not the cursor's.
  private val keys: javafx.event.EventHandler[KeyEvent] = e =>
    val move = if e.getTarget eq showRaw then None
    else
      e.getCode match
        case KeyCode.UP | KeyCode.KP_UP     => Some(SourceRecordsIntent.Move(RecordMove.Up))
        case KeyCode.DOWN | KeyCode.KP_DOWN => Some(SourceRecordsIntent.Move(RecordMove.Down))
        case KeyCode.PAGE_UP                => Some(SourceRecordsIntent.Move(RecordMove.PageUp))
        case KeyCode.PAGE_DOWN => Some(SourceRecordsIntent.Move(RecordMove.PageDown))
        case KeyCode.HOME      => Some(SourceRecordsIntent.Move(RecordMove.First))
        case KeyCode.END       => Some(SourceRecordsIntent.Move(RecordMove.Last))
        case KeyCode.ENTER | KeyCode.SPACE => Some(SourceRecordsIntent.Activate)
        case _                             => None
    move.foreach { m =>
      dispatch(m)
      e.consume()
    }
  node.parentProperty.addListener { (_, was, now) =>
    Option(was).foreach(_.removeEventHandler(KeyEvent.KEY_PRESSED, keys))
    Option(now).foreach(_.addEventHandler(KeyEvent.KEY_PRESSED, keys))
  }

  // The visible rows, from the list's flow, reported as the viewport.
  private var flow: Option[VirtualFlow[?]] = None
  list.skinProperty.addListener((_, _, _) => watchFlow())

  private def watchFlow(): Unit =
    list.lookup(".virtual-flow") match
      case f: VirtualFlow[?] if !flow.contains(f) =>
        flow = Some(f)
        f.positionProperty.addListener((_, _, _) => reportViewport())
        f.heightProperty.addListener { (_, _, _) =>
          revealPending()
          reportViewport()
        }
      case _ => ()

  private var reported = (-1, -1)

  private def reportViewport(): Unit =
    watchFlow()
    flow.foreach { f =>
      (Option(f.getFirstVisibleCell), Option(f.getLastVisibleCell)) match
        case (Some(a), Some(b)) =>
          val view = (a.getIndex, b.getIndex - a.getIndex + 1)
          if view != reported then
            reported = view
            dispatch(SourceRecordsIntent.Viewport(view._1, view._2))
        case _ => ()
    }

  /** Row cells made so far: the list makes only the cells it shows. */
  def cellsMade: Int = made

  /** The list, for tests that scroll it. */
  def rows: ListView[Integer] = list

  /** The state now. */
  def current: SourceRecords = state

  /** The verbatim record shown, if 'Show raw record' is on. */
  def rawText: Option[String] = Option.when(raw.isVisible)(raw.getText)

  /** Toggles 'Show raw record', as the user does. */
  def toggleRaw(): Unit = showRaw.fire()

  /** The controls inside the pane's own stop: 'Show raw record'. */
  def focusStops: Vector[FocusStop] =
    Vector(FocusStop(A11yRole.ToggleButton, RecordText(RecordTextId.ShowRaw)))

  def sync(m: AppModel): Unit = if !disposed then
    val before          = state.cursor
    val (next, effects) = SourceRecords.sync(state, m)
    state = next
    perform(effects)
    render(before != state.cursor)

  def dispatch(intent: SourceRecordsIntent): Unit = if !disposed then
    val before          = state.cursor
    val (next, effects) = SourceRecords.update(state, intent, model())
    state = next
    perform(effects)
    render(before != state.cursor)

  private def perform(effects: Vector[SourceRecordsEffect]): Unit =
    effects.foreach {
      case SourceRecordsEffect.RequestPage(r, page, from, size, ask) =>
        source.page(
          r,
          from,
          size,
          a => Platform.runLater(() => dispatch(SourceRecordsIntent.PageRead(r, page, ask, a)))
        )
      case SourceRecordsEffect.Locate(r, trial) =>
        locate.fixations(
          r,
          trial,
          a => Platform.runLater(() => dispatch(SourceRecordsIntent.Located(r, trial, a)))
        )
      case SourceRecordsEffect.App(i) => app(i)
    }

  private def render(cursorMoved: Boolean): Unit =
    val said = SourceRecords.status(state)
    status.setText(said.getOrElse(""))
    val total = state.total.getOrElse(0)
    if list.getItems.size != total then
      // The row indices, without boxing a list of them: a row is its index.
      val indices = new java.util.AbstractList[Integer]:
        def get(i: Int): Integer = Integer.valueOf(i)
        def size: Int            = total
      list.setItems(FXCollections.observableList(indices))
      // A cursor set before the rows existed is revealed now they do.
      revealPending()
    val line = SourceRecords.rawLine(state)
    raw.setText(line.getOrElse(""))
    raw.setVisible(line.isDefined)
    raw.setManaged(line.isDefined)
    showRaw.setSelected(state.raw)
    if cursorMoved then state.cursor.foreach(scrollTo)
    list.refresh()
    reportViewport()

  // The cursor row still to be brought into view: kept until the list has
  // the row and the flow has laid out cells, since a scroll before then does
  // not hold (the cursor can arrive before the first page says how many rows
  // there are).
  private var reveal: Option[Int] = None

  private def scrollTo(i: Int): Unit =
    reveal = Some(i)
    revealPending()

  private def revealPending(): Unit =
    watchFlow()
    reveal.filter(_ < list.getItems.size).foreach { i =>
      (flow.filter(_.getHeight > 0), shownRange) match
        case (Some(_), Some((first, last))) =>
          // A row above the view goes to the top, one below it to the bottom
          // (scrolling the row before it to the top would leave it cut off).
          if i < first then list.scrollTo(i)
          else if i > last then list.scrollTo(math.max(0, i - (last - first) + 1))
          reveal = None
        case _ => list.scrollTo(i)
    }

  /** The first and last rows the flow shows. */
  def shownRange: Option[(Int, Int)] =
    flow.flatMap(f =>
      for
        a <- Option(f.getFirstVisibleCell)
        b <- Option(f.getLastVisibleCell)
      yield (a.getIndex, b.getIndex)
    )

  /** Whether row `i` is wholly in view. */
  def visible(i: Int): Boolean =
    flow.exists { f =>
      (Option(f.getFirstVisibleCell), Option(f.getLastVisibleCell)) match
        case (Some(a), Some(b)) => a.getIndex <= i && i <= b.getIndex
        case _                  => false
    }

  def dispose(): Unit = disposed = true
