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

package eyes4s.studio.core.freshness

import eyes4s.studio.core.backend.{
  DiagnosticOrigin,
  ProgressTotal,
  RunId,
  StageKind,
  StudioDiagnostic
}
import eyes4s.studio.core.document.{Draft, RecipeChange, RunRef}

/** The reference English rendering of the freshness values, in the board
  * grammar (DESIGN_SPEC sections 3 and 12). It exists for tests and as the
  * specification studio-app's resource-bundle strings must reproduce; it is
  * never an identity.
  */
object FreshnessText:

  /** "21,400": digits grouped in threes, the same on the JVM and Scala.js. */
  def count(value: Long): String =
    val digits  = math.abs(value).toString
    val grouped = digits.reverse.grouped(3).mkString(",").reverse
    if value < 0 then "-" + grouped else grouped

  private def plural(n: Int, one: String, many: String): String =
    s"$n ${if n == 1 then one else many}"

  private def title(run: RunId): String = s"Run ${run.number}"

  private def stage(kind: StageKind): String = kind.productPrefix

  private def total(t: ProgressTotal): String = t match
    case ProgressTotal.Exact(n)  => count(n)
    case ProgressTotal.AtMost(n) => s"≤ ${count(n)}"
    case ProgressTotal.Unknown   => "counting…"

  /** "Analysis rev 4 · run 7 · data r3 · current"; while a newer run runs,
    * the shown run alone, "Showing analysis rev 4 · run 7 · data r3" (with
    * " · stale" when it is not current), beside the [[newer]] chip (Results
    * board).
    */
  def badge(badge: Badge): String = badge match
    case Badge.NoRun(analysis, data) =>
      Vector(
        analysis.map(a => s"Analysis ${a.label}"),
        Some(badge.tone.word),
        data.map(d => s"data ${d.label}")
      ).flatten.mkString(" · ")
    case Badge.Shown(run, standing, newer) =>
      val shown = s"${run.analysis.label} · ${run.id.label} · data ${run.dataset.label}"
      (standing, newer) match
        case (_, None)                      => s"Analysis $shown · ${badge.tone.word}"
        case (RunStanding.Current, Some(_)) => s"Showing analysis $shown"
        case (_, Some(_))                   => s"Showing analysis $shown · ${badge.tone.word}"

  /** The chip beside the badge while a newer run runs: "Rev 5 · run 8
    * running · 48%", the percentage of pairs compared when the total is
    * exact.
    */
  def newer(badge: Badge): Option[String] = badge match
    case Badge.Shown(_, _, Some(r)) =>
      val head    = s"${r.revision.label.capitalize} · ${r.run.label} running"
      val percent = r.meter.collect {
        case RunMeter(_, done, ProgressTotal.Exact(total)) if total > 0 =>
          s"${math.round(done.toDouble / total.toDouble * 100)}%"
      }
      Some(percent.fold(head)(p => s"$head · $p"))
    case _ => None

  /** The shown run's label while a newer revision runs: "superseded — rev 5
    * running". Not a staleness: the run stays current until the newer one
    * completes.
    */
  def newerNote(badge: Badge): Option[String] = badge match
    case Badge.Shown(_, _, Some(r)) => Some(s"superseded — ${r.revision.label} running")
    case _                          => None

  /** The jobs-chip text: "Run 8 · Comparing · 21,400 / 44,845 pairs",
    * "Run 8 failed · 2 diagnostics".
    */
  def activity(activity: RunActivity): String = activity match
    case RunActivity.Running(run, _, None)    => s"${title(run)} · running"
    case RunActivity.Running(run, _, Some(m)) =>
      s"${title(run)} · ${stage(m.stage)} · ${count(m.completedPairs)} / ${total(m.totalPairs)} pairs"
    case RunActivity.Failed(run, _, None)    => s"${title(run)} failed"
    case RunActivity.Failed(run, _, Some(n)) =>
      s"${title(run)} failed · ${plural(n, "diagnostic", "diagnostics")}"
    case RunActivity.Cancelled(run, _, None)    => s"${title(run)} cancelled"
    case RunActivity.Cancelled(run, _, Some(k)) => s"${title(run)} cancelled · ${stage(k)}"

  /** "No jobs" when nothing is active. */
  def jobs(activity: Option[RunActivity]): String = activity.fold("No jobs")(this.activity)

  /** "Draft rev 5 · 1 change · ready". */
  def draft(chip: DraftChip): String =
    val state = chip.readiness match
      case DraftReadiness.Unchecked   => "not checked"
      case DraftReadiness.Ready       => "ready"
      case DraftReadiness.Blocked(bs) => plural(bs.length, "blocker", "blockers")
    s"Draft ${chip.draft.id.label} · ${plural(chip.changes, "change", "changes")} · $state"

  /** "Stale · run 5 belongs to rev 3"; with only a dataset reason, "Stale ·
    * run 5 used data r2; r3 is admitted".
    */
  def stale(run: RunRef, standing: RunStanding): Option[String] = standing match
    case RunStanding.Stale(reasons) =>
      Some(reasons.head match
        case StaleReason.Superseded(_, _) =>
          s"Stale · ${run.id.label} belongs to ${run.analysis.label}"
        case StaleReason.DatasetMoved(data, latest) =>
          s"Stale · ${run.id.label} used data ${data.label}; ${latest.label} is admitted")
    case _ => None

  /** One blocker as the System board shows it: "Studio check · 2 matched
    * references for P11 ret_05 (occurrences 1, 2)". The code stays the
    * identity; the subject is the diagnostic's typed loci.
    */
  def blocker(d: StudioDiagnostic): String =
    val origin = d.origin match
      case DiagnosticOrigin.Host     => "Studio check"
      case DiagnosticOrigin.EyesCore => "eyes4s"
    s"$origin · ${d.message}"

  /** "adds σ 8°" when only scales were added; the changes otherwise. */
  def describe(changes: Vector[RecipeChange]): String = changes match
    case Vector(RecipeChange.Scales(before, after)) if before.values.forall(after.contains) =>
      "adds " + after.values.filterNot(before.contains).map(_.render).mkString(", ")
    case _ => changes.map(_.render).mkString("; ")

  private def draftClause(draft: Draft, changes: Vector[RecipeChange], data: Option[String]) =
    val what =
      (data.toVector ++ Option.when(changes.nonEmpty)(describe(changes))).mkString("; ")
    s"Draft ${draft.id.label} $what and has not been run."

  /** The dock-wide banner. */
  def banner(banner: Banner): String = banner match
    case Banner.NoRun(_)                       => "Compare · No run yet"
    case Banner.NewerRunning(_, running, diff) =>
      val what = if diff.isEmpty then "" else s", ${describe(diff)}"
      s"${title(running.run)} (${running.revision.label}$what) is running — results will " +
        "not replace this view until you choose Show."
    case Banner.NewerEnded(shown, ended) =>
      s"${activity(ended)} — still showing ${shown.id.label} (analysis ${shown.analysis.label})."
    case Banner.NewerCompleted(shown, newer, _) =>
      s"Showing ${shown.id.label} (analysis ${shown.analysis.label}). ${title(newer.id)} " +
        s"(${newer.analysis.label}) has finished — choose Show to see it."
    case Banner.DraftNotRun(shown, draft, diff, data) =>
      s"Showing ${shown.id.label} (analysis ${shown.analysis.label}). " +
        draftClause(draft, diff, data.map(d => s"moves to data ${d.label}"))
    case Banner.ShownStale(shown, _) =>
      s"Showing ${shown.id.label} (analysis ${shown.analysis.label}, data " +
        s"${shown.dataset.label}), which is no longer current."

  /** "Figure 2 · run 5 · rev 3 · data r2 · stale". */
  def figure(f: FigureFreshness): String =
    val word = f.standing match
      case RunStanding.Current      => "current"
      case RunStanding.Stale(_)     => "stale"
      case RunStanding.Running      => "running"
      case RunStanding.Failed       => "failed"
      case RunStanding.Cancelled(_) => "cancelled"
    s"${f.figure.label} · ${f.run.id.label} · ${f.run.analysis.label} · data " +
      s"${f.run.dataset.label} · $word"

  /** "Run 5 (rev 3) used r2 · becomes stale when r3 is admitted" (Data board). */
  def pending(p: PendingDataset, runs: Vector[RunRef]): Vector[String] =
    p.wouldStale.flatMap(id => runs.find(_.id == id)).map { r =>
      s"${title(r.id)} (${r.analysis.label}) used ${r.dataset.label} · becomes stale when " +
        s"${p.dataset.label} is admitted"
    }
