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
import javafx.scene.control.Label
import javafx.scene.layout.VBox
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
  private val created = mutable.ArrayBuffer.empty[PaneId]
  private val gone    = mutable.ArrayBuffer.empty[PaneId]

  /** The live view of `pane`, if the dock has built it and not disposed it. */
  def node(pane: PaneId): Option[javafx.scene.Node] = live.get(pane)

  /** Every pane view built so far, in creation order (a pane twice if rebuilt). */
  def creations: Vector[PaneId] = created.toVector

  /** Every pane view disposed so far. */
  def disposals: Vector[PaneId] = gone.toVector

  val factories: PaneFactories =
    PaneFactories.empty.register(StudioPanes.placeholder) { state =>
      val id    = summon[PaneContext[ujson.Value]].paneId
      val title = declared.get(id.value).fold(id.value)(_.title.text)
      val view  = StudioPanes.Placeholder(id, title, state)
      created += id
      live.update(id, view.node)
      new PaneView[ujson.Value]:
        def node: javafx.scene.Node  = view.node
        def snapshot(): ujson.Value  = view.state
        override def dispose(): Unit =
          gone += id
          live.remove(id): Unit
    }

object StudioPanes:

  /** The one pane type until real panes exist; its state is opaque JSON. */
  val placeholder: PaneType[ujson.Value] =
    PaneType[ujson.Value]("eyes4s.studio.placeholder")(using PaneCodec.json)

  /** The content every declared pane starts with. */
  val content: PaneContent = PaneContent(placeholder, ujson.Null)

  /** A titled, empty panel. */
  private final class Placeholder(id: PaneId, title: String, val state: ujson.Value):
    val node: VBox =
      val heading = Label(title)
      heading.getStyleClass.addAll("pane-placeholder-title", "t13")
      val box = VBox(heading)
      box.getStyleClass.add("pane-placeholder")
      box.setId(s"pane-${id.value}")
      box.setAccessibleText(title)
      box
