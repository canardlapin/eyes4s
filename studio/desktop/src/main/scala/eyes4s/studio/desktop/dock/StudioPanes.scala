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
import eyes4s.studio.app.vm.{A11y, A11yRole}
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

  private val live    = mutable.LinkedHashMap.empty[PaneId, javafx.scene.Node]
  private val views   = mutable.Map.empty[PaneId, StudioPanes.Placeholder]
  private val hosted  = mutable.Map.empty[PaneId, javafx.scene.Node]
  private val created = mutable.ArrayBuffer.empty[PaneId]
  private val gone    = mutable.ArrayBuffer.empty[PaneId]

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

  val factories: PaneFactories =
    PaneFactories.empty.register(StudioPanes.placeholder) { state =>
      val id    = summon[PaneContext[ujson.Value]].paneId
      val decl  = declared.get(id.value)
      val title = decl.fold(id.value)(_.title.text)
      val role  = decl.fold(A11yRole.Region)(d => A11y.role(d.kind))
      val view  = StudioPanes.Placeholder(id, title, role, state)
      hosted.get(id).foreach(view.show)
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
    case A11yRole.RadioButton  => AccessibleRole.RADIO_BUTTON

  /** A titled, empty panel: one focus stop (DESIGN_SPEC section 10), named
    * by its title, with its kind's role.
    */
  private[dock] final class Placeholder(
      id: PaneId,
      title: String,
      role: A11yRole,
      val state: ujson.Value
  ):
    val node: VBox =
      val heading = Label(title)
      heading.getStyleClass.addAll("pane-placeholder-title", "t13")
      val box = VBox(heading)
      box.getStyleClass.add("pane-placeholder")
      box.setId(s"pane-${id.value}")
      box.setAccessibleText(title)
      box.setAccessibleRole(accessibleRole(role))
      box.setFocusTraversable(true)
      box

    /** Replace the title with a pane's real content, filling the pane. */
    def show(content: javafx.scene.Node): Unit =
      VBox.setVgrow(content, Priority.ALWAYS)
      node.getStyleClass.add("pane-hosted")
      node.getChildren.setAll(content): Unit
