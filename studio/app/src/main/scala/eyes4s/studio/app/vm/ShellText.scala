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
      .map(p => s"${p.label} ${p.shortcut}${if p.selected then " (current)" else ""}")
      .mkString(" | ")
    val jobs = (Vector(vm.jobs.text) ++ vm.jobs.action.map(action)).mkString(" | ")
    Vector(
      s"app.name: ${vm.appName}",
      s"app.project: ${vm.project}",
      s"app.perspectives: $perspectives",
      s"app.jobs: $jobs"
    ) ++ vm.jobs.progress.map(f =>
      s"app.jobs.progress: ${eyes4s.studio.app.text.Format.percent(f)}"
    )

  def context(vm: ContextStripVM): Vector[String] =
    val trail =
      vm.trail.map(c => if c.current then s"*${c.label}*" else c.label).mkString(" › ")
    Vector(
      s"context.nav: ${action(vm.back)} | ${action(vm.forward)}",
      s"context.trail: $trail",
      s"context.freshness: ${vm.freshness.text}"
    ) ++ vm.newer.map(n => s"context.newer: ${n.text}") ++ vm.notes.map(n =>
      s"context.note: $n"
    ) ++ vm.draft.map(d => s"context.draft: ${d.text}")

  def banner(vm: Option[DraftBannerVM]): Vector[String] = vm.toVector.flatMap { b =>
    Vector(s"banner.lead: ${b.lead}") ++
      Option.when(b.detail.nonEmpty)(s"banner.detail: ${b.detail}") ++
      Option.when(b.actions.nonEmpty)(
        s"banner.actions: ${b.actions.map(action).mkString(" | ")}"
      )
  }

  def status(vm: StatusBarVM): Vector[String] =
    val selected = vm.selected.fold(vm.noSelection)(p => s"${vm.selectedLabel} $p")
    val job = (Vector(vm.job.text) ++ vm.job.count ++ vm.job.action.map(action)).mkString(" ")
    Vector(s"status: $selected | ${vm.hint} | $job | ${vm.saved}")

  def render(vm: ShellVM): String =
    (Vector(s"window: ${vm.window.title}${if vm.window.edited then " (edited)" else ""}") ++
      appBar(vm.appBar) ++ context(vm.context) ++ banner(vm.banner) ++ status(vm.status) ++
      vm.notice.map(n => s"notice: ${n.text}") ++ vm.confirmation.map(c =>
        s"confirm: ${c.text} | ${action(c.confirm)} | ${action(c.cancel)}"
      ))
      .mkString("\n")
