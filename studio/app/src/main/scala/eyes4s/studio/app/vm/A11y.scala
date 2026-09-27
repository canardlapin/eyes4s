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

package eyes4s.studio.app.vm

import eyes4s.studio.app.AppModel
import eyes4s.studio.app.layout.{PaneDecl, PaneKind}
import eyes4s.studio.app.text.{MessageId, Messages}

/** What a focus stop is to assistive technology (S1.11). A shell maps each
  * role onto its toolkit's accessible roles.
  */
enum A11yRole derives CanEqual:
  case Button, ToggleButton, MenuButton

  /** A plot or trial view: one focus stop with a roving cursor (DESIGN_SPEC
    * section 10); its name summarises it.
    */
  case Plot

  /** A table: one focus stop with a row cursor. */
  case Table

  /** A navigator: one focus stop with a row cursor. */
  case List

  /** Any other pane: a form, an inspector, a text or a start page. */
  case Region

  /** The role as the committed tab-order files spell it. */
  def id: String = this match
    case Button       => "button"
    case ToggleButton => "toggle-button"
    case MenuButton   => "menu-button"
    case Plot         => "plot"
    case Table        => "table"
    case List         => "list"
    case Region       => "region"

/** One stop of the Tab order: its role and its accessible name. */
final case class FocusStop(role: A11yRole, name: String) derives CanEqual:
  def render: String = s"${role.id}: $name"

/** The accessibility baseline of the shell (ticket S1.11): the role of each
  * pane and the Tab order of each perspective, derived from the same
  * view-models the shell renders. A shell's traversal must visit exactly
  * these stops, in this order; docs/studio/a11y/tab-order-<perspective>.txt
  * commits them for the board moments.
  */
object A11y:

  /** A pane is one focus stop; its role follows its kind. */
  def role(kind: PaneKind): A11yRole = kind match
    case PaneKind.Plot                                                       => A11yRole.Plot
    case PaneKind.Table                                                      => A11yRole.Table
    case PaneKind.Navigator                                                  => A11yRole.List
    case PaneKind.Form | PaneKind.Inspector | PaneKind.Text | PaneKind.Start =>
      A11yRole.Region

  /** A pane's stop: its tab title names it. */
  def pane(decl: PaneDecl): FocusStop = FocusStop(role(decl.kind), decl.title.text)

  private def button(a: ActionVM): Option[FocusStop] =
    Option.when(a.enabled)(FocusStop(A11yRole.Button, a.label))

  /** Every stop Tab visits, top to bottom: app bar (the project chip, the
    * selected perspective, the jobs chip when it opens something, Cancel),
    * context strip, notice,
    * confirmation, banner, one stop per visible dock group (the pane it
    * shows), and the status bar's action. A disabled button is no stop.
    */
  def tabOrder(model: AppModel, messages: Messages = Messages.english): Vector[FocusStop] =
    val vm     = Shell.project(model, messages)
    val bar    = vm.appBar
    val jobs   = bar.jobs
    val appBar = Vector(FocusStop(A11yRole.MenuButton, bar.projectAccessible)) ++
      // The switcher is one toggle group: Tab stops on its selected button
      // and the arrow keys move within it (⌘1–5 reach any perspective).
      bar.perspectives
        .filter(_.selected)
        .map(p => FocusStop(A11yRole.ToggleButton, p.accessible)) ++
      jobs.open.map(_ => FocusStop(A11yRole.Button, jobs.accessible)) ++
      jobs.action.flatMap(button)
    val context = button(vm.context.back).toVector ++ button(vm.context.forward) ++
      vm.context.trail.map(c => FocusStop(A11yRole.Button, c.accessible)) ++
      vm.context.draft.map(d => FocusStop(A11yRole.Button, d.accessible))
    val notice  = vm.notice.map(_ => FocusStop(A11yRole.Button, messages(MessageId.Dismiss)))
    val confirm = vm.confirmation.toVector.flatMap(c => button(c.confirm) ++ button(c.cancel))
    val banner  = vm.banner.toVector.flatMap(_.actions.flatMap(button))
    val layout  = model.layout
    val focused = model.focusedPane
    val groups  =
      if model.isMaximized then layout.groupOf(focused).toVector else layout.groups
    val dock = groups.map { g =>
      pane(g.panes.find(_.id == focused).getOrElse(layout.selectedPane(g)))
    }
    val status = vm.status.job.action.flatMap(button)
    appBar ++ context ++ notice ++ confirm ++ banner ++ dock ++ status

  /** The committed file's text: one stop per line. */
  def render(stops: Vector[FocusStop]): String = stops.map(_.render).mkString("", "\n", "\n")
