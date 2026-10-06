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

import cats.effect.IO
import eyes4s.studio.app.explore.*
import eyes4s.studio.app.text.{InspectorText, InspectorTextId}
import eyes4s.studio.app.vm.{A11yRole, FocusStop}
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.navigation.StudyNavigator
import eyes4s.studio.core.selection.StudioRef
import eyes4s.studio.desktop.plot.TableTwinView
import eyes4s.studio.desktop.runtime.StudioSession
import javafx.application.Platform
import javafx.geometry.Pos
import javafx.scene.control.{Hyperlink, Label, ScrollPane, ToggleButton}
import javafx.scene.layout.{GridPane, HBox, Priority, Region, VBox}

/** Where the inspector reads which pairs use a trial. `done` may be called
  * on any thread.
  */
trait UsedByInputs:
  def usedBy(map: StudioRef, done: Either[String, BackendAnswer[UsedByAnswer]] => Unit): Unit

object UsedByInputs:
  /** The window's backend navigator. */
  def of(session: StudioSession): UsedByInputs = (map, done) =>
    val navigator: StudyNavigator[IO] = session.navigator
    session.run(FixationInspector.readUsedBy[IO](navigator)(map)) {
      case Left(e)          => done(Left(Option(e.getMessage).getOrElse(e.toString)))
      case Right(Left(err)) => done(Right(BackendAnswer.Refused(err.message)))
      case Right(Right(u))  => done(Right(BackendAnswer.Answered(u)))
    }

/** Explore's fixation inspector on the desktop (ticket S6.5; Explore.dc.html,
  * right; see [[FixationInspector]]): the selected fixation's timing and
  * coordinates, its source record, the run's pairs that use its trial as
  * links, and its trial. It only binds [[FixationInspectorVM]]; a link sends
  * its intent. Use on the JavaFX thread.
  */
final class FixationInspectorHost(
    app: Intent => Unit,
    trials: TrialViewInputs,
    records: SourceRecordsSource,
    used: UsedByInputs
):
  private var state    = FixationInspector.empty
  private var disposed = false

  private def label(styles: String*): Label =
    val l = Label()
    l.getStyleClass.addAll(styles*)
    l

  private val status = label("t12", "inspector-status")
  private val title  = label("t13")
  private val kind   = label("kind")
  kind.setText(InspectorText(InspectorTextId.ViewOnly))
  private val spacer = Region()
  HBox.setHgrow(spacer, Priority.ALWAYS)
  private val head = HBox(8.0, title, spacer, kind)
  head.setAlignment(Pos.CENTER_LEFT)

  private def grid(): GridPane =
    val g = GridPane()
    g.setHgap(8.0)
    g.setVgap(5.0)
    g.getStyleClass.add("kv")
    g

  private val fixation  = grid()
  private val frameNote = label("t11", "inspector-note")
  frameNote.setWrapText(true)
  private val source = grid()
  private val raw    = label("mono", "t11", "records-raw")
  raw.setWrapText(true)
  private val showRaw = ToggleButton(InspectorText(InspectorTextId.ShowRaw))
  showRaw.getStyleClass.add("btn")
  showRaw.setAccessibleText(InspectorText(InspectorTextId.ShowRawName))
  showRaw.setOnAction(_ => dispatch(InspectorIntent.ShowRaw(showRaw.isSelected)))
  private val usedTitle = label("t13")
  private val links     = VBox(2.0)
  private val usedNote  = label("t11", "inspector-note")
  private val trial     = grid()

  private def section(heading: Option[String], children: javafx.scene.Node*): VBox =
    val box = VBox(8.0)
    heading.foreach(h => box.getChildren.add({ val l = label("t13"); l.setText(h); l }))
    box.getChildren.addAll(children*)
    box.getStyleClass.add("sect")
    box

  private val content = VBox(
    VBox(8.0, status, head, fixation, frameNote),
    section(Some(InspectorText(InspectorTextId.SourceTitle)), source, showRaw, raw),
    section(None, usedTitle, links, usedNote),
    section(Some(InspectorText(InspectorTextId.TrialTitle)), trial)
  )
  content.setPadding(javafx.geometry.Insets(12, 14, 12, 14))
  content.setSpacing(10.0)

  /** The pane's content. */
  val node: ScrollPane = ScrollPane(content)
  node.setFitToWidth(true)
  node.setFocusTraversable(false)
  node.getStyleClass.add("fixation-inspector")
  Option(getClass.getClassLoader.getResource(TableTwinView.stylesheetResource))
    .foreach(url => node.getStylesheets.add(url.toExternalForm))

  /** The view-model now shown. */
  def vm: FixationInspectorVM = FixationInspector.vm(state)

  /** The state now. */
  def current: FixationInspector = state

  /** The used-by links as shown: their words. */
  def linkLabels: Vector[String] =
    import scala.jdk.CollectionConverters.*
    links.getChildren.asScala.toVector.collect { case h: Hyperlink => h.getText }

  /** Follows the link with `label`, as a click does. */
  def follow(label: String): Unit =
    import scala.jdk.CollectionConverters.*
    links.getChildren.asScala
      .collectFirst { case h: Hyperlink if h.getText == label => h }
      .foreach(_.fire())

  /** The labelled lines shown, by section, as text. */
  def lines: Vector[(String, String)] =
    import scala.jdk.CollectionConverters.*
    Vector(fixation, source, trial).flatMap(g =>
      g.getChildren.asScala.toVector.collect { case l: Label => l.getText }.grouped(2).collect {
        case Vector(a, b) => (a, b)
      }
    )

  /** The title, and the raw record if shown. */
  def titleText: String       = title.getText
  def rawText: Option[String] = Option.when(raw.isVisible)(raw.getText)

  /** Toggles 'Show raw record', as the user does. */
  def toggleRaw(): Unit = showRaw.fire()

  /** The controls inside the pane's own stop: 'Show raw record' and the links. */
  def focusStops: Vector[FocusStop] =
    if state.focus.isEmpty then Vector.empty
    else
      FocusStop(A11yRole.ToggleButton, InspectorText(InspectorTextId.ShowRawName)) +:
        vm.usedBy.map(l => FocusStop(A11yRole.Link, l.label))

  def sync(m: AppModel): Unit = if !disposed then
    val (next, effects) = FixationInspector.sync(state, m)
    state = next
    perform(effects)
    render()

  def dispatch(intent: InspectorIntent): Unit = if !disposed then
    val (next, effects) = FixationInspector.update(state, intent)
    state = next
    perform(effects)
    render()

  private def later(i: InspectorIntent): Unit = Platform.runLater(() => dispatch(i))

  private def perform(effects: Vector[InspectorEffect]): Unit =
    effects.foreach {
      case InspectorEffect.ReadFixations(r, t, ask) =>
        trials.fixations(r, t, a => later(InspectorIntent.FixationsRead(r, t, ask, a)))
      case InspectorEffect.ReadRecord(r, record, ask) =>
        records.page(
          r,
          record - 1,
          1,
          a => later(InspectorIntent.RecordRead(r, record, ask, a))
        )
      case InspectorEffect.ReadUsedBy(map, ask) =>
        used.usedBy(map, a => later(InspectorIntent.UsedByRead(map, ask, a)))
      case InspectorEffect.ReadDisplays(dataset, ask) =>
        trials.displays(dataset, a => later(InspectorIntent.DisplaysRead(ask, a)))
    }

  private def fill(g: GridPane, lines: Vector[eyes4s.studio.app.explore.InspectorLine]): Unit =
    g.getChildren.clear()
    lines.zipWithIndex.foreach { (l, i) =>
      val k = label("inspector-key"); k.setText(l.label)
      val v = label("mono"); v.setText(l.value)
      v.setWrapText(true)
      v.setMinHeight(Region.USE_PREF_SIZE)
      k.setMinWidth(118.0)
      g.add(k, 0, i)
      g.add(v, 1, i)
    }

  private def render(): Unit =
    val v = vm
    status.setText(v.status.getOrElse(""))
    status.setVisible(v.status.isDefined)
    status.setManaged(v.status.isDefined)
    title.setText(v.title)
    fill(fixation, v.fixation)
    frameNote.setText(v.frameNote.getOrElse(""))
    fill(source, v.source)
    raw.setText(v.raw.getOrElse(""))
    raw.setVisible(v.raw.isDefined)
    raw.setManaged(v.raw.isDefined)
    showRaw.setSelected(state.raw)
    usedTitle.setText(v.usedByTitle)
    links.getChildren.setAll(v.usedBy.map { l =>
      val h = Hyperlink(l.label)
      h.setAccessibleText(l.label)
      h.setOnAction(_ => app(l.go))
      h
    }*)
    usedNote.setText(v.usedByNote.getOrElse(""))
    usedNote.setVisible(v.usedByNote.isDefined)
    usedNote.setManaged(v.usedByNote.isDefined)
    fill(trial, v.trial)
    content.setVisible(true)

  def dispose(): Unit = disposed = true
