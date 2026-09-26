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

import eyes4s.studio.app.layout.{
  Axis as StudioAxis,
  LayoutNode,
  LayoutSpec,
  PaneDecl,
  PerspectiveLayout
}
import scaladock.{
  Cell,
  Header,
  HeaderButtons,
  LayoutSettings,
  LayoutState,
  Node,
  NodeId,
  Pane,
  Size
}

/** The studio's UI-neutral [[LayoutSpec]] as scaladock layouts (ticket S1.5a).
  *
  * Pure: every node and pane id is derived from the spec, so one layout
  * always maps to the same `LayoutState`, and a studio pane id is its
  * scaladock pane id: a pane declared in two layouts (Compare's scale profile)
  * is one retained, live view.
  */
object DockLayouts:

  /** The dock's metrics: 28 px tab headers (DESIGN_SPEC section 3). */
  val settings: LayoutSettings = LayoutSettings(headerPx = 28)

  /** Studio panes are singletons the user cannot close; nothing pops out
    * until S1.5b.
    */
  val defaultHeader: HeaderButtons =
    HeaderButtons(close = false, maximize = true, popOut = false, minimize = false)

  /** A navigator group may also be minimized to a strip (DESIGN_SPEC section 3). */
  val navigatorHeader: HeaderButtons = defaultHeader.copy(minimize = true)

  /** The scaladock perspective name of a layout: its [[eyes4s.studio.app.layout.LayoutId]]. */
  def name(layout: PerspectiveLayout): String = layout.id.value

  /** The scaladock pane id of a studio pane. */
  def paneId(pane: eyes4s.studio.app.layout.PaneId): scaladock.PaneId =
    scaladock.PaneId(pane.value)

  /** `layout` as a scaladock layout. */
  def state(layout: PerspectiveLayout): LayoutState =
    LayoutState.of(node(layout.id.value, Vector.empty, layout.root))

  /** Every layout of `spec`, by perspective name, in declaration order. */
  def states(spec: LayoutSpec): Vector[(String, LayoutState)] =
    spec.all.map(l => name(l) -> state(l))

  private def axis(a: StudioAxis): scaladock.Axis = a match
    case StudioAxis.Horizontal => scaladock.Axis.Horizontal
    case StudioAxis.Vertical   => scaladock.Axis.Vertical

  private def nodeId(layout: String, path: Vector[Int]): NodeId =
    NodeId((layout +: path.map(_.toString)).mkString("/"))

  private def pane(decl: PaneDecl): Pane =
    Pane(paneId(decl.id), StudioPanes.content, decl.title.text, closable = false)

  private def node(layout: String, path: Vector[Int], n: LayoutNode): Node = n match
    case LayoutNode.Split(a, children) =>
      Node.Split(
        nodeId(layout, path),
        axis(a),
        children.toVector.zipWithIndex.map { case ((child, weight), i) =>
          Cell(node(layout, path :+ i, child), Size.Fr(weight))
        }
      )
    case LayoutNode.Group(panes, selected, navigator) =>
      val active = panes.toVector.lift(selected).getOrElse(panes.head)
      Node.Group(
        nodeId(layout, path),
        panes.toVector.map(pane),
        paneId(active.id),
        Header.Shown(if navigator then navigatorHeader else defaultHeader)
      )
