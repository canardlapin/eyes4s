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

import eyes4s.studio.app.Intent
import eyes4s.studio.core.document.Perspective

// The shell's view-models (DESIGN_SPEC section 3). Every string is final:
// a shell renders it as given and dispatches the intents it carries, and
// computes nothing. Pane view-models arrive with their screens.

/** A button: its words, whether it is enabled, and what it dispatches. */
final case class ActionVM(
    label: String,
    enabled: Boolean,
    intent: Intent,
    accessibleName: Option[String] = None
) derives CanEqual:
  def accessible: String = accessibleName.getOrElse(label)

/** The native window: its title and the macOS document-edited marker. */
final case class WindowVM(title: String, edited: Boolean) derives CanEqual

final case class PerspectiveButtonVM(
    perspective: Perspective,
    label: String,
    shortcut: String,
    selected: Boolean,
    intent: Intent,
    /** The name assistive technology reads: "Compare (⌘4)". */
    accessible: String
) derives CanEqual

/** The jobs chip's states (S1.4). */
enum JobsChipState derives CanEqual:
  case Idle, Queued, Running, Cancelling, Failed, Ready

/** The jobs chip: "No jobs"; "Run 8 · Comparing · 21,400 / 44,845 pairs"
  * with a progress bar and Cancel; "Run 8 failed · 2 diagnostics".
  * `title`, `stage` and `count` are drawn as separate runs of text (the
  * title semibold, the count in Plex Mono); [[text]] joins them.
  */
final case class JobsChipVM(
    state: JobsChipState,
    title: String,
    stage: Option[String],
    count: Option[String],
    progress: Option[Double],
    action: Option[ActionVM],
    open: Option[Intent],
    accessible: String
) derives CanEqual:
  def text: String = (Vector(title) ++ stage ++ count).mkString(" · ")

final case class AppBarVM(
    appName: String,
    project: String,
    /** The project chip's accessible name: "Project memory-study". */
    projectAccessible: String,
    perspectives: Vector[PerspectiveButtonVM],
    jobs: JobsChipVM
) derives CanEqual

/** One trail crumb; the current one is drawn in ink 600. Every crumb is a
  * live button (S1.6). `opens` is the perspective it lands in, when that is
  * not the current one (a fixation crumb opens Explore); `accessible` says
  * so, or that the crumb is the current location.
  */
final case class CrumbVM(
    label: String,
    current: Boolean,
    intent: Intent,
    opens: Option[Perspective],
    accessible: String
) derives CanEqual

/** The freshness dot and weight. */
enum FreshnessTone derives CanEqual:
  case Current, Stale, Running, Failed, Cancelled, NoRun, PendingData

  /** The badge of the two-chip form (Results board, t3): the shown run is
    * current, but a newer one is running beside it, so the dot is hollow.
    */
  case Showing

/** The freshness badge ("Analysis rev 4 · run 7 · data r3 · current"). */
final case class FreshnessVM(text: String, tone: FreshnessTone) derives CanEqual

/** The dashed draft chip ("Draft rev 5 · 1 change · ready"); opens Analysis. */
final case class DraftChipVM(text: String, blocked: Boolean, intent: Intent, accessible: String)
    derives CanEqual

final case class ContextStripVM(
    back: ActionVM,
    forward: ActionVM,
    trail: Vector[CrumbVM],
    freshness: FreshnessVM,
    /** The newer run's chip beside the badge ("Rev 5 · run 8 running · 48%"). */
    newer: Option[FreshnessVM],
    notes: Vector[String],
    draft: Option[DraftChipVM]
) derives CanEqual

/** The dock-wide banner of Compare and Figures: a semibold lead sentence,
  * the detail, and its buttons.
  */
final case class DraftBannerVM(lead: String, detail: String, actions: Vector[ActionVM])
    derives CanEqual

/** The status bar's job slot: it mirrors the jobs chip. */
final case class StatusJobVM(text: String, count: Option[String], action: Option[ActionVM])
    derives CanEqual

/** The status bar's four slots, always all four (S1.8). `selected` is the
  * path after the "Selected:" label, or `None` for "No selection".
  */
final case class StatusBarVM(
    selectedLabel: String,
    selected: Option[String],
    noSelection: String,
    hint: String,
    job: StatusJobVM,
    saved: String
) derives CanEqual

/** A destructive intent awaiting confirmation (Discard draft): the
  * question, then the confirming and the cancelling button.
  */
final case class ConfirmationVM(text: String, confirm: ActionVM, cancel: ActionVM)
    derives CanEqual

/** A notice to show once (a refused command, undo at a barrier). */
final case class NoticeVM(text: String, dismiss: Intent) derives CanEqual

/** Everything the shell draws around the dock. */
final case class ShellVM(
    window: WindowVM,
    appBar: AppBarVM,
    context: ContextStripVM,
    banner: Option[DraftBannerVM],
    status: StatusBarVM,
    notice: Option[NoticeVM],
    confirmation: Option[ConfirmationVM]
) derives CanEqual
