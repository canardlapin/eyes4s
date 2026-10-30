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

/** A plain-text rendering of the shell view-models, one labelled line per
  * slot, for golden snapshots and the headless driver (S3.6). The slot texts
  * are the view-model strings verbatim; only the markers are added:
  * `[x]` a disabled button, `(current)` the selected perspective, `*` the
  * current crumb.
  */
object ShellText:

  private def action(a: ActionVM): String = if a.enabled then a.label else s"[${a.label}]"

  def appBar(vm: AppBarVM): Vector[String] =
    val perspectives = vm.perspectives
      .map(p => s"${p.label}${if p.selected then " (current)" else ""}")
      .mkString(" | ")
    Vector(s"app.project: ${vm.project}", s"app.perspectives: $perspectives")

  /** The state button: its three slots, then the separate cancel. */
  def state(vm: StateButtonVM): Vector[String] =
    Vector(s"app.state: ${vm.text}") ++ vm.cancel.map(c => s"app.state.cancel: ${action(c)}") ++
      vm.analysis.progress.map(f =>
        s"app.state.progress: ${eyes4s.studio.app.text.Format.percent(f)}"
      )

  /** The state menu, one line per section; the confirmation alone when it asks. */
  def stateMenu(vm: StateButtonVM): Vector[String] =
    vm.confirm.fold(vm.menu)(Vector(_)).map { s =>
      val parts = Vector(s.head) ++ s.facts.map((k, v) => s"$k $v") ++ s.note ++
        Option.when(s.actions.nonEmpty)(s.actions.map(action).mkString(" / "))
      s"menu: ${parts.mkString(" | ")}"
    }

  def context(vm: ContextStripVM): Vector[String] =
    val trail =
      vm.trail.map(c => if c.current then s"*${c.label}*" else c.label).mkString(" / ")
    Vector(
      s"context.nav: ${action(vm.back)} | ${action(vm.forward)}",
      s"context.trail: $trail"
    )

  def status(vm: StatusBarVM): Vector[String] = Vector(s"status: ${vm.hint} | ${vm.saved}")

  def render(vm: ShellVM): String =
    (Vector(s"window: ${vm.window.title}${if vm.window.edited then " (edited)" else ""}") ++
      appBar(vm.appBar) ++ state(vm.state) ++ context(vm.context) ++ status(vm.status) ++
      vm.notice.map(n => s"notice: ${n.text}") ++ vm.confirmation.map(c =>
        s"confirm: ${c.text} | ${action(c.confirm)} | ${action(c.cancel)}"
      ) ++ vm.state.confirm.map(_ => stateMenu(vm.state).mkString("\n")))
      .mkString("\n")
