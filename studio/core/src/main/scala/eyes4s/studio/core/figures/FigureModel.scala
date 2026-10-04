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

import cats.syntax.all.*
import eyes4s.studio.core.backend.{AnalysisRevision, DatasetRevision, RunId}
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.diff.{DatasetDiff, DiffError, StatusDiff}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.freshness.{Freshness, RunStanding}

/** Why a figure's binding could not be resolved or rebound. Every case names
  * its operands.
  */
enum FigureError derives CanEqual:
  case UnknownFigure(figure: FigureId, known: Vector[FigureId])
  case UnknownRun(figure: FigureId, run: RunId)
  case UnknownReporting(figure: FigureId, reporting: ReportingId)
  case UnknownAnalysis(run: RunId, analysis: AnalysisRevision)
  case UnknownDataset(run: RunId, dataset: DatasetRevision)

  /** Only a completed run can be bound. */
  case NotCompleted(run: RunId, state: RunLifecycle)

  /** The figure already binds `run`. */
  case SameRun(figure: FigureId, run: RunId)
  case Diff(error: DiffError)

  def message: String = this match
    case UnknownFigure(f, known) =>
      s"The document has no ${f.label} (it has ${known.map(_.label).mkString(", ")})."
    case UnknownRun(f, r) => s"${f.label} binds ${r.label}, which the document does not have."
    case UnknownReporting(f, r) =>
      s"${f.label} binds reporting spec ${r.value}, which is missing."
    case UnknownAnalysis(r, a) =>
      s"${r.label} names ${a.label}, which the document does not have."
    case UnknownDataset(r, d) =>
      s"${r.label} used data ${d.label}, which the document does not have."
    case NotCompleted(r, state) => s"${r.label} cannot be bound: it has not completed ($state)."
    case SameRun(f, r)          => s"${f.label} already binds ${r.label}."
    case Diff(e)                => e.message

/** A run with the revisions it names: what a figure's binding shows
  * ("run 5 · rev 3 · data r2").
  */
final case class BoundRun(
    run: RunRef,
    analysis: AnalysisRevisionSpec,
    dataset: DatasetRevisionSpec
) derives CanEqual

object BoundRun:
  def of(document: StudioDocument, run: RunRef): Either[FigureError, BoundRun] =
    for
      analysis <- document
        .analysis(run.analysis)
        .toRight(FigureError.UnknownAnalysis(run.id, run.analysis))
      dataset <- document
        .dataset(run.dataset)
        .toRight(FigureError.UnknownDataset(run.id, run.dataset))
    yield BoundRun(run, analysis, dataset)

/** What a figure renders from (ticket S9.1): the one run and the one
  * reporting spec it binds, with their revisions. It is resolved from the
  * figure alone: the run the document shows, the latest run and the
  * freshness of either never enter, so a figure renders from its bound run,
  * never the current one.
  */
final case class FigureSource(
    figure: FigureSpec,
    bound: BoundRun,
    reporting: ReportingSpec
) derives CanEqual:
  def run: RunRef = bound.run

object FigureSource:
  def of(document: StudioDocument, figure: FigureId): Either[FigureError, FigureSource] =
    for
      spec <- document.figures
        .find(_.id == figure)
        .toRight(FigureError.UnknownFigure(figure, document.figures.map(_.id)))
      run       <- document.run(spec.run).toRight(FigureError.UnknownRun(figure, spec.run))
      bound     <- BoundRun.of(document, run)
      reporting <- document.reporting
        .find(_.id == spec.reporting)
        .toRight(FigureError.UnknownReporting(figure, spec.reporting))
    yield FigureSource(spec, bound, reporting)

/** A rebind of a figure to another run, shown before it is applied (S9.1:
  * "Rebind shows the plan/data diff before applying"): what changes in the
  * analysis plan and in the data between the bound run and `to`, and the
  * panels whose scale `to` does not compute. Nothing changes until
  * [[command]] is dispatched.
  */
final case class RebindProposal(
    figure: FigureId,
    from: BoundRun,
    to: BoundRun,
    reporting: ReportingId,
    plan: Vector[RecipeChange],
    data: Option[DatasetDiff],
    conflicts: Vector[(PanelLetter, Sigma)]
) derives CanEqual:
  /** The rebind, when every panel's scale is one `to` computes. */
  def command: Option[Command] =
    Option.when(conflicts.isEmpty)(Command.BindFigure(figure, to.run.id, reporting))

  /** The same proposal with the trial statuses of its data compared. */
  def withStatus(status: StatusDiff): Either[FigureError, RebindProposal] =
    data.traverse(_.withStatus(status)).bimap(FigureError.Diff(_), d => copy(data = d))

object RebindProposal:
  /** Rebind `figure` to `to`, which must have completed, keeping its
    * reporting spec and panels. The data diff is present when the runs used
    * different dataset revisions, with `status` as its status comparison.
    */
  def of(
      document: StudioDocument,
      figure: FigureId,
      to: RunId,
      status: StatusDiff
  ): Either[FigureError, RebindProposal] =
    for
      source <- FigureSource.of(document, figure)
      target <- document.run(to).toRight(FigureError.UnknownRun(figure, to))
      _      <- Either.cond(
        target.state == RunLifecycle.Completed,
        (),
        FigureError.NotCompleted(to, target.state)
      )
      _     <- Either.cond(source.run.id != to, (), FigureError.SameRun(figure, to))
      bound <- BoundRun.of(document, target)
      from = source.bound
      data <-
        if from.dataset.id == bound.dataset.id then Right(None)
        else
          DatasetDiff
            .of(from.dataset, bound.dataset, status)
            .bimap(FigureError.Diff(_), Some(_))
      scales = bound.analysis.recipe.scales.values
    yield RebindProposal(
      figure,
      from,
      bound,
      source.figure.reporting,
      RecipeChange.between(from.analysis.recipe, bound.analysis.recipe),
      data,
      source.figure.namedScales.filterNot((_, sigma) => scales.contains(sigma))
    )

  /** The run "Rebind…" offers: the latest completed run that is current. */
  def target(freshness: Freshness): Option[RunRef] =
    freshness.runs.reverse.collectFirst {
      case f if f.standing == RunStanding.Current && f.run.state == RunLifecycle.Completed =>
        f.run
    }
