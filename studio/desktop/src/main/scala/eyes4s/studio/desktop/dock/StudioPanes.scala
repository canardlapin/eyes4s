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

package eyes4s.studio.desktop.dock

import eyes4s.studio.app.layout.{LayoutSpec, PaneDecl}
import eyes4s.studio.app.vm.{A11y, A11yRole, PaneFocus}
import eyes4s.studio.desktop.shell.CompositeFocus
import javafx.collections.SetChangeListener
import javafx.css.PseudoClass
import javafx.scene.AccessibleRole
import javafx.scene.control.Label
import javafx.scene.layout.{Priority, VBox}
import scaladock.{PaneCodec, PaneContent, PaneId, PaneType}
import scaladock.fx.{PaneContext, PaneFactories, PaneView}

import scala.collection.mutable

/** The studio's pane views (ticket S1.5a).
  *
  * Until each screen's ticket lands, every pane is a placeholder: a titled,
  * empty panel. Views are created by the dock, once per pane id, and this
  * registry records every creation and disposal, so a test can prove that
  * switching perspectives never rebuilds or disposes a pane.
  */
final class StudioPanes(spec: LayoutSpec):

  private val declared: Map[String, PaneDecl] =
    spec.all.flatMap(_.panes).map(p => p.id.value -> p).toMap

  /** The dock behind the pane headers, once the host has one. */
  private var chrome: Option[StudioPanes.Chrome] = None

  /** Give the headers their dock, and draw them. */
  def attach(c: StudioPanes.Chrome): Unit =
    chrome = Some(c)
    refresh()

  /** Redraw every header from the dock: its group's panes and which shows. */
  def refresh(): Unit = views.values.foreach(_.header(chrome, declared))

  /** A pane's subtitle, beside its title (the golden's grey `.sub`). */
  def describe(pane: PaneId, subtitle: String): Unit =
    subtitles.update(pane, subtitle)
    views.get(pane).foreach(_.subtitle(subtitle))

  private val subtitles = mutable.Map.empty[PaneId, String]

  /** A pane's own header controls, at its right (toggles, segmented bars). */
  def controls(pane: PaneId, nodes: javafx.scene.Node*): Unit =
    extras.update(pane, nodes.toVector)
    views.get(pane).foreach(_.controls(nodes.toVector))

  private val extras = mutable.Map.empty[PaneId, Vector[javafx.scene.Node]]

  private val live       = mutable.LinkedHashMap.empty[PaneId, javafx.scene.Node]
  private val views      = mutable.Map.empty[PaneId, StudioPanes.Placeholder]
  private val hosted     = mutable.Map.empty[PaneId, javafx.scene.Node]
  private val focusPlans = mutable.Map.empty[PaneId, PaneFocus]
  private val created    = mutable.ArrayBuffer.empty[PaneId]
  private val gone       = mutable.ArrayBuffer.empty[PaneId]

  /** The live view of `pane`, if the dock has built it and not disposed it. */
  def node(pane: PaneId): Option[javafx.scene.Node] = live.get(pane)

  /** Every pane view built so far, in creation order (a pane twice if rebuilt). */
  def creations: Vector[PaneId] = created.toVector

  /** Every pane view disposed so far. */
  def disposals: Vector[PaneId] = gone.toVector

  /** Show `content` in `pane`'s view in place of its placeholder title: now,
    * if the dock has built the view, else when it does. The pane stays one
    * focus stop with its role and name; its content's controls follow it.
    */
  def host(pane: PaneId, content: javafx.scene.Node): Unit =
    hosted.update(pane, content)
    views.get(pane).foreach(_.show(content))

  /** Bind the portable contract even when the dock has not mounted the pane yet. */
  def focus(pane: PaneId, plan: PaneFocus): Unit =
    focusPlans.update(pane, plan)
    views.get(pane).foreach(_.focus(plan))

  val factories: PaneFactories =
    PaneFactories.empty.register(StudioPanes.placeholder) { state =>
      val id    = summon[PaneContext[ujson.Value]].paneId
      val decl  = declared.get(id.value)
      val title = decl.fold(id.value)(_.title.text)
      val role  = decl.fold(A11yRole.Region)(d => A11y.role(d.kind))
      val name  = decl.fold(title)(_.accessibleName)
      val view  = StudioPanes.Placeholder(id, title, name, role, state)
      hosted.get(id).foreach(view.show)
      subtitles.get(id).foreach(view.subtitle)
      extras.get(id).foreach(view.controls)
      view.header(chrome, declared)
      focusPlans.get(id).foreach(view.focus)
      created += id
      views.update(id, view)
      live.update(id, view.node)
      new PaneView[ujson.Value]:
        def node: javafx.scene.Node  = view.node
        def snapshot(): ujson.Value  = view.state
        override def dispose(): Unit =
          gone += id
          // A pane rebuilt under the same id keeps its new view.
          if views.get(id).exists(_ eq view) then views.remove(id): Unit
          live.remove(id): Unit
    }

object StudioPanes:

  /** What a pane's header asks of the dock: its group's panes in order and
    * the one that shows, and the group's actions.
    */
  trait Chrome:
    def group(pane: PaneId): Option[(Vector[PaneId], PaneId)]
    def select(pane: PaneId): Unit
    def popOut(pane: PaneId): Unit
    def collapse(pane: PaneId): Unit

  /** The golden's pane header (overhaul v4), 36 px. */
  val HeaderPx: Double = 36

  /** The one pane type until real panes exist; its state is opaque JSON. */
  val placeholder: PaneType[ujson.Value] =
    PaneType[ujson.Value]("eyes4s.studio.placeholder")(using PaneCodec.json)

  /** The content every declared pane starts with. */
  val content: PaneContent = PaneContent(placeholder, ujson.Null)

  /** A pane's role as JavaFX names it (S1.11): a plot is an image with a
    * summary, a table and a navigator have row cursors, anything else is a
    * group of controls.
    */
  def accessibleRole(role: A11yRole): AccessibleRole = role match
    case A11yRole.Plot         => AccessibleRole.IMAGE_VIEW
    case A11yRole.Table        => AccessibleRole.TABLE_VIEW
    case A11yRole.List         => AccessibleRole.LIST_VIEW
    case A11yRole.Button       => AccessibleRole.BUTTON
    case A11yRole.ToggleButton => AccessibleRole.TOGGLE_BUTTON
    case A11yRole.MenuButton   => AccessibleRole.MENU_BUTTON
    case A11yRole.Region       => AccessibleRole.PARENT
    case A11yRole.ComboBox     => AccessibleRole.COMBO_BOX
    case A11yRole.TextField    => AccessibleRole.TEXT_FIELD
    case A11yRole.TextArea     => AccessibleRole.TEXT_AREA
    case A11yRole.RadioButton  => AccessibleRole.RADIO_BUTTON
    case A11yRole.Link         => AccessibleRole.HYPERLINK
    case A11yRole.Slider       => AccessibleRole.SLIDER
    case A11yRole.CheckBox     => AccessibleRole.CHECK_BOX

  /** A titled, empty panel: one focus stop (DESIGN_SPEC section 10), named
    * by its title, with its kind's role. Content whose rows carry the pane's
    * focus makes it a composite pane ([[CompositeFocus.paneContent]]), which
    * follows the content's `no-cursor` state.
    */
  private[dock] final class Placeholder(
      id: PaneId,
      title: String,
      name: String,
      role: A11yRole,
      val state: ujson.Value
  ):
    private val heading = Label(title)
    heading.getStyleClass.addAll("pane-title", "pane-placeholder-title")
    heading.setMinWidth(javafx.scene.layout.Region.USE_PREF_SIZE)
    private val sub = Label()
    sub.getStyleClass.add("pane-sub")
    sub.setMinWidth(0)
    private val own    = javafx.scene.layout.HBox()
    private val switch = javafx.scene.layout.HBox()
    private val twin   = javafx.scene.layout.HBox()
    private val action = javafx.scene.layout.HBox()
    own.getStyleClass.add("pane-controls")
    switch.getStyleClass.add("pane-seg")
    twin.getStyleClass.add("pane-seg")
    action.getStyleClass.add("pane-actions")

    /** The header: title, subtitle, then the pane's own controls, its
      * group's switch, Plot | Table, and pop out (or close, for an inspector).
      */
    val bar: javafx.scene.layout.HBox =
      val spacer = javafx.scene.layout.Region()
      javafx.scene.layout.HBox.setHgrow(spacer, Priority.ALWAYS)
      javafx.scene.layout.HBox.setHgrow(sub, Priority.SOMETIMES)
      val h = javafx.scene.layout.HBox(heading, sub, spacer, own, switch, twin, action)
      h.getStyleClass.add("pane-header")
      h.setMinHeight(HeaderPx)
      h.setPrefHeight(HeaderPx)
      h.setMaxHeight(HeaderPx)
      h

    // An unhosted pane's empty body under its header; hosted content takes
    // its place as the pane's own child (navigators read their pane as their
    // parent).
    private val empty = javafx.scene.layout.Region()
    empty.getStyleClass.add("pane-body")
    VBox.setVgrow(empty, Priority.ALWAYS)

    def subtitle(text: String): Unit = sub.setText(text)

    def controls(nodes: Vector[javafx.scene.Node]): Unit = own.getChildren.setAll(nodes*): Unit

    private def segment(
        text: String,
        on: Boolean,
        act: () => Unit
    ): javafx.scene.control.ToggleButton =
      val b = javafx.scene.control.ToggleButton(text)
      b.setMnemonicParsing(false)
      b.setSelected(on)
      b.setFocusTraversable(false)
      b.getStyleClass.add("pane-seg-button")
      b.setOnAction { _ =>
        b.setSelected(on) // the dock decides; the header follows it
        if !on then act()
      }
      b

    private def iconAction(icon: eyes4s.studio.app.icons.Icon, name: String, act: () => Unit) =
      val b = javafx.scene.control.Button()
      b.setGraphic(
        eyes4s.studio.desktop.icons.StudioIcons
          .graphic(icon, eyes4s.studio.desktop.icons.IconSize.Px16)
      )
      b.getStyleClass.addAll("icon-button", "pane-action")
      b.setAccessibleText(name)
      b.setTooltip(javafx.scene.control.Tooltip(name))
      b.setFocusTraversable(false)
      b.setOnAction(_ => act())
      b

    /** Draw the header from the dock's view of this pane's group. */
    def header(chrome: Option[Chrome], declared: Map[String, PaneDecl]): Unit =
      import eyes4s.studio.app.layout.{PaneKind, StudioLayouts}
      def decl(p: PaneId)    = declared.get(p.value)
      def titleOf(p: PaneId) = decl(p).fold(p.value)(_.title.text)
      val group              = chrome.flatMap(_.group(id))
      val panes              = group.fold(Vector(id))(_._1)
      val ids                = panes.map(_.value).toSet
      // A plot's Table twin rides with its plot: Plot | Table, not a segment.
      def plotOf(p: PaneId): Option[PaneId] =
        StudioLayouts.twinPlot(p.value).map(PaneId(_)).filter(q => ids(q.value))
      def tableOf(p: PaneId): Option[PaneId] =
        Option
          .when(decl(p).exists(_.kind == PaneKind.Plot))(
            PaneId(StudioLayouts.twinTable(p.value))
          )
          .filter(q => ids(q.value))
      val primaries = panes.filter(p => plotOf(p).isEmpty)
      val primary   = plotOf(id).getOrElse(id)
      heading.setText(titleOf(primary))
      chrome match
        case Some(c) =>
          switch.getChildren.setAll(
            (if primaries.size > 1 then
               primaries.map(p => segment(titleOf(p), p == primary, () => c.select(p)))
             else Vector.empty)*
          )
          twin.getChildren.setAll(
            tableOf(primary).toVector.flatMap { t =>
              Vector(
                segment("Plot", id == primary, () => c.select(primary)),
                segment("Table", id == t, () => c.select(t))
              )
            }*
          )
          val inspector = decl(id).exists(_.kind == PaneKind.Inspector)
          action.getChildren.setAll(
            if inspector then
              iconAction(
                eyes4s.studio.app.icons.Icon.Minimize,
                s"Close ${titleOf(primary)}",
                () => c.collapse(id)
              )
            else
              iconAction(
                eyes4s.studio.app.icons.Icon.PopOut,
                s"Open ${titleOf(primary)} in a new window",
                () => c.popOut(id)
              )
          )
        case None =>
          switch.getChildren.clear()
          twin.getChildren.clear()
          action.getChildren.clear()
      Vector(switch, twin, action).foreach { box =>
        val shown = !box.getChildren.isEmpty
        box.setVisible(shown)
        box.setManaged(shown)
      }

    val node: VBox =
      val box = VBox(bar, empty)
      box.getStyleClass.add("pane-placeholder")
      box.setId(s"pane-${id.value}")
      box.setAccessibleText(name)
      box.setAccessibleRole(accessibleRole(role))
      box.setFocusTraversable(true)
      box

    private var hosted: Option[javafx.scene.Node]          = None
    private val cursorless: SetChangeListener[PseudoClass] = _ => followCursor()

    private def followCursor(): Unit =
      CompositeFocus.noCursor(node, hosted.exists(CompositeFocus.hasNoCursor))

    def focus(plan: PaneFocus): Unit =
      node.setFocusTraversable(plan.container.nonEmpty)
      if plan.container.isEmpty then node.setAccessibleRole(AccessibleRole.PARENT)
      plan.container.foreach { stop =>
        node.setAccessibleRole(accessibleRole(stop.role))
        node.setAccessibleText(stop.name)
      }

    /** Replace the title with a pane's real content, filling the pane. */
    def show(content: javafx.scene.Node): Unit =
      VBox.setVgrow(content, Priority.ALWAYS)
      node.getStyleClass.add("pane-hosted")
      node.getChildren.setAll(bar, content): Unit
      hosted.foreach(_.getPseudoClassStates.removeListener(cursorless))
      hosted = Option.when(CompositeFocus.isPaneContent(content))(content)
      node.getStyleClass.remove(CompositeFocus.PaneStyle): Unit
      hosted.foreach { c =>
        node.getStyleClass.add(CompositeFocus.PaneStyle): Unit
        c.getPseudoClassStates.addListener(cursorless)
      }
      followCursor()
