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
import eyes4s.studio.app.icons.Icon
import eyes4s.studio.app.vm.{StateButtonVM, StateSectionVM, StateSlotVM}
import eyes4s.studio.desktop.icons.{IconSize, StudioIcons}
import javafx.scene.Node
import javafx.scene.control.{Button, Label}
import javafx.scene.input.{KeyCode, KeyEvent}
import javafx.scene.layout.{HBox, Priority, Region, VBox}
import javafx.stage.Popup

import scala.jdk.CollectionConverters.*

/** The study-wide state button (overhaul decisions 2 and 3): three slots,
  * Run · Data · Analysis, then a chevron; beside it, while a job can be
  * cancelled, a 28 px ✕. The button opens the state menu, a [[Popup]] under
  * it with the Run, Dataset, Analysis and Jobs sections. A pending cancel
  * opens the menu at its inline confirmation alone, with Keep running
  * focused: Enter or Esc keeps the run.
  */
final class StateButton(dispatch: Intent => Unit):

  private val parts = HBox()
  parts.getStyleClass.add("state-parts")

  val button: Button = Button()
  button.getStyleClass.add("state")
  button.setGraphic(parts)
  button.setMnemonicParsing(false)

  val cancel: Button = Button()
  cancel.getStyleClass.addAll("icon-button", "state-cancel")
  cancel.setGraphic(StudioIcons.graphic(Icon.Close, IconSize.Px14))

  val node: HBox = HBox(button, cancel)
  node.getStyleClass.add("state-cluster")

  /** The state menu's content: one section per entry, or the confirmation. */
  val content: VBox = VBox()
  // "root": the window's .root rules define the tokens, and a popup's
  // content is outside the window's scene graph.
  content.getStyleClass.addAll("root", "state-menu")

  val menu: Popup = Popup()
  menu.setAutoHide(true)
  menu.setHideOnEscape(true)
  menu.getContent.add(content)

  private var shown: Option[StateButtonVM] = None
  private var confirming                   = false

  // Esc or a click outside closes the menu; while it asks, that keeps the
  // run (Dismiss), as Keep running does.
  menu.setOnHidden(_ =>
    if confirming then
      confirming = false
      shown.flatMap(_.confirm).foreach(_ => dispatch(Intent.Dismiss))
  )
  content.addEventFilter(
    KeyEvent.KEY_PRESSED,
    (e: KeyEvent) =>
      if e.getCode == KeyCode.ESCAPE then
        menu.hide()
        e.consume()
  )

  button.setOnAction(_ => if menu.isShowing then menu.hide() else open())

  /** The parts as drawn, for tests: each slot's words, in order. */
  def drawnSlots: Vector[String] =
    parts.getChildren.asScala.toVector.collect {
      case h: HBox if h.getStyleClass.contains("state-part") =>
        h.getChildren.asScala.collect { case l: Label => l.getText }.mkString(" ")
    }

  /** The menu's sections as drawn, for tests: each head. */
  def drawnSections: Vector[String] =
    content.getChildren.asScala.toVector.flatMap {
      case v: VBox =>
        v.getChildren.asScala.collectFirst {
          case h: HBox if h.getStyleClass.contains("state-section-head") =>
            h.getChildren.asScala.collect { case l: Label => l.getText }.mkString
        }
      case _ => None
    }

  /** Slot `i`'s marks as drawn, for tests: its dot's tone ("tone-current")
    * or "draftmark".
    */
  def drawnMarks(i: Int): Vector[String] =
    parts.getChildren.asScala.toVector
      .collect { case h: HBox if h.getStyleClass.contains("state-part") => h }
      .lift(i)
      .toVector
      .flatMap(_.getChildren.asScala.toVector.collect {
        case r: Region if r.getStyleClass.contains("dot") =>
          r.getStyleClass.asScala.filter(_.startsWith("tone-")).mkString
        case r: Region if r.getStyleClass.contains("draftmark") => "draftmark"
      })

  /** Whether the menu is open at the inline confirmation. */
  def asking: Boolean = confirming

  /** The Analysis slot's progress as drawn, while a job runs. */
  def drawnProgress: Option[Double] = shown.flatMap(_.analysis.progress)

  /** The menu as drawn, one line per section, in ShellText's form: head,
    * facts, note, then the buttons (`[x]` disabled).
    */
  def drawnMenu: Vector[String] =
    content.getChildren.asScala.toVector.collect { case v: VBox =>
      v.getChildren.asScala.toVector
        .flatMap {
          case h: HBox if h.getStyleClass.contains("state-section-head") =>
            Vector(h.getChildren.asScala.collect { case l: Label => l.getText }.mkString)
          case h: HBox if h.getStyleClass.contains("state-fact") =>
            Vector(h.getChildren.asScala.collect { case l: Label => l.getText }.mkString(" "))
          case l: Label                                             => Vector(l.getText)
          case h: HBox if h.getStyleClass.contains("state-actions") =>
            Vector(
              h.getChildren.asScala
                .collect { case b: Button =>
                  if b.isDisabled then s"[${b.getText}]" else b.getText
                }
                .mkString(" / ")
            )
          case _ => Vector.empty
        }
        .mkString("menu: ", " | ", "")
    }

  def render(vm: StateButtonVM): Unit =
    if !shown.contains(vm) then
      val before = shown
      shown = Some(vm)
      val slots = Vector(vm.run, vm.data, vm.analysis).map(StateButton.slot)
      val drawn = slots.zipWithIndex.flatMap { (s, i) =>
        if i == 0 then Vector(s) else Vector(StateButton.sep(), s)
      } :+ StateButton.chevron()
      parts.getChildren.setAll(drawn*)
      button.setAccessibleText(vm.accessible)
      vm.cancel match
        case Some(a) =>
          cancel.setAccessibleText(a.accessible)
          cancel.setDisable(!a.enabled)
          cancel.setOnAction(_ => dispatch(a.intent))
          StateButton.show(cancel, true)
        case None => StateButton.show(cancel, false)
      (before.flatMap(_.confirm), vm.confirm) match
        case (_, Some(c))    => ask(c)
        case (Some(_), None) =>
          confirming = false
          menu.hide()
        case (None, None) => if menu.isShowing then fill(vm.menu)
      ()

  /** Open the menu at its sections. */
  def open(): Unit = shown.foreach { vm =>
    vm.confirm.fold(fill(vm.menu))(c => fill(Vector(c)))
    place()
  }

  private def ask(c: StateSectionVM): Unit =
    fill(Vector(c))
    confirming = true
    place()
    StateButton.defaultButton(content).foreach(_.requestFocus())

  private def fill(sections: Vector[StateSectionVM]): Unit =
    content.getChildren.setAll(sections.map(section)*): Unit

  // Under the button, its left edge on the button's; the popup carries the
  // window's stylesheets, so its tokens follow the theme.
  private def place(): Unit =
    Option(button.getScene).foreach { scene =>
      val sheets = scene.getStylesheets.asScala ++ scene.getRoot.getStylesheets.asScala
      content.getStylesheets.setAll(sheets.distinct.asJava)
      // The theme's classes on the window's root (dark) reach the popup too.
      val theme = scene.getRoot.getStyleClass.asScala.filterNot(_ == "root").toVector
      content.getStyleClass.setAll((Vector("root", "state-menu") ++ theme).distinct.asJava)
      val at = button.localToScreen(0.0, button.getHeight + 4.0)
      if at != null && !menu.isShowing then menu.show(button, at.getX, at.getY)
    }

  private def section(s: StateSectionVM): VBox =
    val mark = StateButton.marks(s.dot, s.draft)
    val head = HBox((mark :+ Fx.label(s.head, "state-section-title"))*)
    head.getStyleClass.add("state-section-head")
    val facts = s.facts.map { (k, v) =>
      val row = HBox(Fx.label(k, "state-fact-key"), Fx.label(v, "state-fact-value"))
      row.getStyleClass.add("state-fact")
      row
    }
    val note     = s.note.map(n => Fx.label(n, "state-note"))
    val progress = s.progress.map(p => StateButton.bar(p, "state-menu-bar"))
    val actions  = Option.when(s.actions.nonEmpty) {
      val buttons = s.actions.zipWithIndex.map { (a, i) =>
        val classes = Vector("bar-button", "state-action") ++
          Option.when(s.destructive.contains(i))("state-destructive") ++
          Option.when(s.default.contains(i))("state-default")
        val b = Fx.button(a, dispatch, classes*)
        b.setDefaultButton(s.default.contains(i))
        b
      }
      val row = HBox((if s.default.isDefined then Fx.spacer() +: buttons else buttons)*)
      row.getStyleClass.add("state-actions")
      row
    }
    val box = VBox((Vector(head) ++ facts ++ note ++ progress ++ actions)*)
    box.getStyleClass.add("state-section")
    box

object StateButton:

  /** The slot's marks: its dot (Run), or the dashed draft mark. */
  def marks(dot: Option[eyes4s.studio.app.vm.FreshnessTone], draft: Boolean): Vector[Node] =
    dot.map { t =>
      val d = Region()
      d.getStyleClass.addAll("dot", s"tone-${ContextStrip.toneClass(t)}")
      d: Node
    }.toVector ++ Option.when(draft) {
      val m = Region()
      m.getStyleClass.add("draftmark")
      m: Node
    }

  def slot(s: StateSlotVM): HBox =
    val text  = Fx.label(s.text, "state-text")
    val count = s.count.map(c => Fx.label(c, "num", "state-count"))
    val bar   = s.progress.map(p => this.bar(p, "state-bar"))
    val h     = HBox((marks(s.dot, s.draft) ++ Vector(text) ++ count ++ bar)*)
    h.getStyleClass.add("state-part")
    if s.muted then h.getStyleClass.add("muted"): Unit
    h

  /** A progress bar: a track and its filled share, as the golden draws it. */
  def bar(fraction: Double, style: String): Region =
    val fill = Region()
    fill.getStyleClass.add("state-bar-fill")
    val track = HBox(fill)
    track.getStyleClass.addAll("state-bar-track", style)
    track.widthProperty.addListener((_, _, w) =>
      fill.setPrefWidth(math.round(w.doubleValue * fraction.max(0.0).min(1.0)).toDouble)
    )
    HBox.setHgrow(fill, Priority.NEVER)
    track

  def sep(): Region =
    val r = Region()
    r.getStyleClass.add("state-sep")
    r

  def chevron(): Node =
    val c = StudioIcons.graphic(Icon.ChevronDown, IconSize.Px16)
    c.getStyleClass.add("state-chevron")
    c

  def show(n: Node, visible: Boolean): Unit =
    n.setVisible(visible)
    n.setManaged(visible)

  private def defaultButton(root: javafx.scene.Parent): Option[Button] =
    root.lookupAll(".state-default").asScala.collectFirst { case b: Button => b }
