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

import eyes4s.studio.app.{AppModel, DockCommand}
import eyes4s.studio.app.layout.{LayoutSpec, PaneId as StudioPaneId}
import eyes4s.studio.core.document.{LayoutBlob, Perspective, SavedLayout}
import scaladock.{DockEvent, LayoutCodec, LayoutState, PaneId}
import scaladock.fx.{Dock, DockAction, DockTheme, Perspectives}

/** A saved layout that could not be read; its perspective shows the default. */
final case class UnreadableLayout(perspective: Perspective, reason: String) derives CanEqual:
  def message: String = s"The saved layout of ${perspective.label} could not be read: $reason"

object PerspectiveHost:

  /** Put keyboard focus on `pane`'s first focus stop (its own node when it is
    * one), if it is shown in a scene.
    */
  def focusInside(pane: javafx.scene.Node): Unit =
    def first(n: javafx.scene.Node): Option[javafx.scene.Node] =
      if n.isFocusTraversable && !n.isDisabled && n.isVisible then Some(n)
      else
        n match
          case p: javafx.scene.Parent =>
            import scala.jdk.CollectionConverters.*
            p.getChildrenUnmodifiable.asScala.iterator.flatMap(first).nextOption()
          case _ => None
    if pane.getScene != null then first(pane).getOrElse(pane).requestFocus()

/** What the user did in the dock itself, which the model must follow. */
enum DockGesture derives CanEqual:
  case Focused(pane: StudioPaneId)

  /** A group was maximized (its header button); `pane` is its active tab. */
  case Maximized(pane: StudioPaneId)
  case Restored

/** The perspective host (ticket S1.5a): ONE scaladock `Dock` whose named
  * perspectives are the layouts of the studio's [[LayoutSpec]], managed by
  * scaladock's `Perspectives` (628c46f). Switching perspectives retains every
  * pane that leaves, so a pane is built once and a pane id shared by two
  * layouts is one live view; the dock never disposes a studio pane.
  *
  * It follows the model ([[sync]]) and reports the user's own dock gestures
  * (focus, the header's maximize and restore) through `report`; it decides
  * nothing. Use on the JavaFX thread.
  */
final class PerspectiveHost(spec: LayoutSpec, theme: DockTheme, report: DockGesture => Unit):

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

  /** Focus and maximize follow the model, so they are not part of a saved
    * arrangement: two arrangements that differ only there are the same.
    */
  private def normal(s: LayoutState): LayoutState = s.copy(focused = None, maximized = None)

  private val start: Map[String, LayoutState] =
    defaults.map((name, s) => name -> normal(s)).toMap

  locally:
    dock.setTheme(theme)
    // The app keymap owns F6, ⇧F6, ⌃⇥, ⌘⇧↩ and the rest (CommandRegistry);
    // the dock follows the model instead of reading keys itself.
    dock.setKeyBindings(Map.empty)
    defaults.foreach(perspectives.define)
    dock.events.subscribe {
      case DockEvent.PaneFocused(id, _) =>
        paneIds.get(id).foreach(p => report(DockGesture.Focused(p)))
      case DockEvent.GroupMaximized(g) =>
        dock.state
          .findGroup(g)
          .flatMap(group => paneIds.get(group.active))
          .foreach(p => report(DockGesture.Maximized(p)))
      case DockEvent.GroupRestored(_) => report(DockGesture.Restored)
      case _                          => ()
    }: Unit

  /** The layout name the dock shows. */
  def active: Option[String] = perspectives.active

  private var focusRequested: Option[PaneId] = None

  /** Show the model's layout, its focused pane, and its maximize state. */
  def sync(model: AppModel): Unit =
    val layout = model.layout
    perspectives.show(DockLayouts.name(layout))
    val focused = DockLayouts.paneId(model.focusedPane)
    if !dock.state.focused.contains(focused) && dock.state.findPane(focused).isDefined then
      dock.focus(focused)
      // The model moved focus (F6, ⌘1–5, a tab menu's Show table): keyboard
      // focus follows it into the pane, which is one focus stop (S1.11) —
      // once per move, so a render never steals focus back.
      if !focusRequested.contains(focused) then
        focusRequested = Some(focused)
        node(model.focusedPane).foreach(PerspectiveHost.focusInside)
    else focusRequested = dock.state.focused
    if model.isMaximized != dock.state.maximized.isDefined then dock.toggleMaximizeFocused()

  /** Run a dock command (⌃⇥, ⌃⇧⇥); the dock reports the focus it moves. */
  def perform(command: DockCommand): Unit = command match
    case DockCommand.NextTab     => dock.perform(DockAction.NextTab)
    case DockCommand.PreviousTab => dock.perform(DockAction.PreviousTab)

  /** Return every layout of `perspective` to its default arrangement. */
  def reset(perspective: Perspective): Unit =
    spec.layouts(perspective).foreach(l => perspectives.reset(DockLayouts.name(l)): Unit)

  /** Each perspective's current arrangement as the document saves it, focus
    * and maximize left out: `None` where every layout of the perspective is
    * at its default.
    */
  def capture(): Vector[(Perspective, Option[LayoutBlob])] =
    val saved = perspectives.save().obj("perspectives").obj
    val now   = saved.toVector.flatMap { (name, json) =>
      LayoutCodec.decode(json, dock.paneTypes).toOption.map(s => name -> normal(s))
    }.toMap
    Perspective.values.toVector.map { p =>
      val layouts = spec.layouts(p).map(DockLayouts.name)
      val changed = layouts.exists(n => now.get(n).exists(s => !start.get(n).contains(s)))
      p -> Option.when(changed)(
        LayoutBlob(
          ujson.write(
            ujson.Obj.from(
              layouts.flatMap(n => now.get(n).map(s => n -> LayoutCodec.encode(s)))
            )
          )
        )
      )
    }

  /** One saved perspective's layouts, each decoded, or why it cannot be read. */
  private def read(s: SavedLayout): Either[UnreadableLayout, Vector[(String, ujson.Value)]] =
    def unreadable(reason: String) = UnreadableLayout(s.perspective, reason)
    val names                      = spec.layouts(s.perspective).map(DockLayouts.name).toSet
    scala.util
      .Try(ujson.read(s.layout.text).obj.toVector)
      .toEither
      .left
      .map(e => unreadable(String.valueOf(e.getMessage)))
      .flatMap(
        _.foldLeft[Either[UnreadableLayout, Vector[(String, ujson.Value)]]](
          Right(Vector.empty)
        ) { case (acc, (name, json)) =>
          acc.flatMap { ok =>
            if !names(name) then
              Left(unreadable(s"${s.perspective.label} has no layout named '$name'"))
            else
              LayoutCodec
                .decode(json, dock.paneTypes)
                .left
                .map(e => unreadable(e.message))
                .map(_ => ok :+ (name -> json))
          }
        }
      )

  /** Restore the document's saved layouts, then show the model's layout. A
    * saved layout that cannot be read is skipped, so its perspective shows
    * its default; the window always opens. Returns the skipped ones.
    */
  def restore(saved: Vector[SavedLayout], model: AppModel): Vector[UnreadableLayout] =
    val results    = saved.map(s => s -> read(s))
    val unreadable = results.collect { case (_, Left(u)) => u }
    val readable   = results.collect { case (s, Right(es)) => s -> es }
    val entries    = readable.flatMap(_._2)
    val refused    =
      if entries.isEmpty then Vector.empty
      else
        perspectives
          .load(
            ujson.Obj(
              "active"       -> ujson.Str(DockLayouts.name(model.layout)),
              "perspectives" -> ujson.Obj.from(entries)
            )
          )
          .fold(
            e => readable.map((s, _) => UnreadableLayout(s.perspective, e.message)),
            _ => Vector.empty
          )
    sync(model)
    unreadable ++ refused

  /** The live view of a studio pane, if built. */
  def node(pane: StudioPaneId): Option[javafx.scene.Node] =
    panes.node(DockLayouts.paneId(pane))

  /** Show `content` in a studio pane (see [[StudioPanes.host]]). */
  def host(pane: StudioPaneId, content: javafx.scene.Node): Unit =
    panes.host(DockLayouts.paneId(pane), content)

  /** The scaladock id of a studio pane. */
  def dockId(pane: StudioPaneId): PaneId = DockLayouts.paneId(pane)

  /** The studio pane a scaladock pane id names, if it is one. */
  def studioPane(pane: PaneId): Option[StudioPaneId] = paneIds.get(pane)
