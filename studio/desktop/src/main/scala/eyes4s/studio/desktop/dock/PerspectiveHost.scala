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

import eyes4s.studio.app.AppModel
import eyes4s.studio.app.layout.{LayoutSpec, PaneId as StudioPaneId}
import eyes4s.studio.core.document.{LayoutBlob, Perspective, SavedLayout}
import scaladock.{DockEvent, LayoutCodec, PaneId}
import scaladock.fx.{Dock, DockTheme, Perspectives}

/** Why saved layouts could not be restored; nothing changed. */
final case class LayoutRestoreError(perspectives: Vector[Perspective], reason: String):
  def message: String =
    s"The saved layouts of ${perspectives.map(_.label).mkString(", ")} could not be restored: $reason"

/** The perspective host (ticket S1.5a): ONE scaladock `Dock` whose named
  * perspectives are the layouts of the studio's [[LayoutSpec]], managed by
  * scaladock's `Perspectives` (628c46f). Switching perspectives retains every
  * pane that leaves, so a pane is built once and a pane id shared by two
  * layouts is one live view; the dock never disposes a studio pane.
  *
  * It follows the model ([[sync]]) and reports the user's dock gestures
  * through `onFocus`; it decides nothing. Use on the JavaFX thread.
  */
final class PerspectiveHost(spec: LayoutSpec, theme: DockTheme, onFocus: StudioPaneId => Unit):

  val panes: StudioPanes = StudioPanes(spec)

  val dock: Dock =
    Dock(
      panes.factories,
      settings = DockLayouts.settings,
      defaultHeader = DockLayouts.defaultHeader
    )

  val perspectives: Perspectives = Perspectives(dock)

  private val defaults = DockLayouts.states(spec)
  private val paneIds  =
    spec.all.flatMap(_.panes).map(p => DockLayouts.paneId(p.id) -> p.id).toMap

  locally:
    dock.setTheme(theme)
    // The app keymap owns F6, ⌘⇧↩ and the rest (CommandRegistry); the dock
    // follows the model instead of reading keys itself.
    dock.setKeyBindings(Map.empty)
    defaults.foreach(perspectives.define)
    dock.events.subscribe {
      case DockEvent.PaneFocused(id, _) => paneIds.get(id).foreach(onFocus)
      case _                            => ()
    }: Unit

  /** The layout name the dock shows. */
  def active: Option[String] = perspectives.active

  /** Show the model's layout, its focused pane, and its maximize state. */
  def sync(model: AppModel): Unit =
    val layout = model.layout
    perspectives.show(DockLayouts.name(layout))
    val focused = DockLayouts.paneId(model.focusedPane)
    if !dock.state.focused.contains(focused) && dock.state.findPane(focused).isDefined then
      dock.focus(focused)
    if model.isMaximized != dock.state.maximized.isDefined then dock.toggleMaximizeFocused()

  /** Return every layout of `perspective` to its default arrangement. */
  def reset(perspective: Perspective): Unit =
    spec.layouts(perspective).foreach(l => perspectives.reset(DockLayouts.name(l)): Unit)

  /** Each perspective's current arrangement as the document saves it:
    * `None` where every layout of the perspective is at its default.
    */
  def capture(): Vector[(Perspective, Option[LayoutBlob])] =
    val saved = perspectives.save().obj("perspectives").obj
    val start = defaults.toMap.view.mapValues(LayoutCodec.encode).toMap
    Perspective.values.toVector.map { p =>
      val layouts = spec.layouts(p).map(DockLayouts.name)
      val changed = layouts.exists(n => saved.get(n).exists(v => !start.get(n).contains(v)))
      p -> Option.when(changed)(
        LayoutBlob(ujson.write(ujson.Obj.from(layouts.flatMap(n => saved.get(n).map(n -> _)))))
      )
    }

  /** Restore the document's saved layouts, then show the model's layout. */
  def restore(saved: Vector[SavedLayout], model: AppModel): Either[LayoutRestoreError, Unit] =
    val parsed = saved.map { s =>
      scala.util
        .Try(ujson.read(s.layout.text).obj.toVector)
        .toEither
        .left
        .map(e => LayoutRestoreError(Vector(s.perspective), String.valueOf(e.getMessage)))
    }
    parsed.collectFirst { case Left(e) => e } match
      case Some(e) => Left(e)
      case None    =>
        val entries = parsed.collect { case Right(es) => es }.flatten
        val all     = ujson.Obj(
          "active"       -> ujson.Str(DockLayouts.name(model.layout)),
          "perspectives" -> ujson.Obj.from(entries)
        )
        if entries.isEmpty then Right(sync(model))
        else
          perspectives
            .load(all)
            .left
            .map(e => LayoutRestoreError(saved.map(_.perspective), e.message))
            .map(_ => sync(model))

  /** The live view of a studio pane, if built. */
  def node(pane: StudioPaneId): Option[javafx.scene.Node] =
    panes.node(DockLayouts.paneId(pane))

  /** The scaladock id of a studio pane. */
  def dockId(pane: StudioPaneId): PaneId = DockLayouts.paneId(pane)
