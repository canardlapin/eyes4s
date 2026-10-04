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

package eyes4s.studio.core.figures

import cats.data.NonEmptyVector
import eyes4s.studio.core.diff.{DatasetDiff, DatasetDiffText}
import eyes4s.studio.core.document.ReportingWeight
import eyes4s.studio.core.freshness.{FreshnessText, StaleReason}

/** The reference English rendering of a figure's binding in the
  * Figures.dc.html grammar (navigator, stale notice, inspector, rebind).
  * Like `FreshnessText`, it is the specification studio-app's strings
  * reproduce, never an identity.
  */
object FigureText:

  /** "run 5 · rev 3 · data r2". */
  def binding(b: BoundRun): String =
    s"${b.run.id.label} · ${b.analysis.id.label} · data ${b.dataset.id.label}"

  /** "Participant means, equal weight". */
  def unit(weight: ReportingWeight): String = weight match
    case ReportingWeight.ParticipantMeans => "Participant means, equal weight"
    case ReportingWeight.PooledQueries    => "Pooled query trials, equal weight"

  /** One reason the bound run is stale. A dataset move is said with the
    * diff between the run's data and the latest admitted revision, when
    * `data` is that diff: "dataset r3 changed the admission status of 4
    * trials".
    */
  def cause(reason: StaleReason, data: Option[DatasetDiff]): String = reason match
    case StaleReason.DatasetMoved(from, latest) =>
      data
        .filter(d => d.from == from && d.to == latest)
        .fold(s"dataset ${latest.label} is admitted")(DatasetDiffText.staleCause)
    case StaleReason.Superseded(by, revision) =>
      s"${revision.label} (${by.label}) supersedes it"

  /** The stale notice: "Bound to run 5 · rev 3 · data r2, which is no
    * longer current: dataset r3 changed the admission status of 4 trials.
    * The figure still shows run 5 exactly as it was." The data reason comes
    * first, as on the board; no reason is left out.
    */
  def stale(
      bound: BoundRun,
      reasons: NonEmptyVector[StaleReason],
      data: Option[DatasetDiff]
  ): String =
    val ordered = reasons.toVector.sortBy {
      case _: StaleReason.DatasetMoved => 0
      case _: StaleReason.Superseded   => 1
    }
    s"Bound to ${binding(bound)}, which is no longer current: " +
      ordered.map(cause(_, data)).mkString("; ") +
      s". The figure still shows ${bound.run.id.label} exactly as it was."

  /** "Keep as rev 3". */
  def keep(bound: BoundRun): String = s"Keep as ${bound.analysis.id.label}"

  /** The rebind dialog's title: "Rebind Figure 2 to run 7?". */
  def rebindTitle(p: RebindProposal): String =
    s"Rebind ${p.figure.label} to ${p.to.run.id.label}?"

  /** The plan diff: "Plan: rev 3 → rev 4, adds σ 8°", or the same plan. */
  def plan(p: RebindProposal): String =
    val revisions = s"${p.from.analysis.id.label} → ${p.to.analysis.id.label}"
    if p.plan.isEmpty then s"Plan: $revisions, no change to the analysis"
    else s"Plan: $revisions, ${FreshnessText.describe(p.plan)}"

  /** The data diff: "Data: r2 → r3, onset declared ms; …", or the same data. */
  def data(p: RebindProposal): String = p.data match
    case None       => s"Data: ${p.to.dataset.id.label}, unchanged"
    case Some(diff) =>
      val summary = DatasetDiffText.summary(diff)
      val what    = if summary.isEmpty then "no change to its content" else summary
      s"Data: ${diff.from.label} → ${diff.to.label}, $what"

  /** "Panel C shows σ 8°, which run 7 does not compute." */
  def conflicts(p: RebindProposal): Vector[String] =
    p.conflicts.map((letter, sigma) =>
      s"Panel ${letter.value} shows ${sigma.render}, which ${p.to.run.id.label} does not compute."
    )
