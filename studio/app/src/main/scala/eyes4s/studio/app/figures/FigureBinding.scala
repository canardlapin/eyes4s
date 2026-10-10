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

package eyes4s.studio.app.figures

import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.backend.{DatasetRevision, RunId}
import eyes4s.studio.core.diff.{DatasetDiff, StatusDiff}
import eyes4s.studio.core.document.FigureId
import eyes4s.studio.core.figures.{FigureError, FigureSource, FigureText, RebindProposal}
import eyes4s.studio.core.freshness.{RunStanding, StaleReason}

/** A user action or platform fact the figure binding's views dispatch. */
enum FigureIntent derives CanEqual:
  case Select(figure: FigureId)

  /** "Keep as rev N": the figure stays bound to its run; its stale notice
    * is put away for this session, for that run only (a later stale
    * binding of the figure is shown again). Keeping is not a command (`Command`,
    * S2.2): freshness stays derived, so the figure is still marked stale.
    */
  case Keep(figure: FigureId)

  /** "Rebind…": propose rebinding to the latest current run. */
  case Rebind(figure: FigureId)
  case ConfirmRebind
  case CancelRebind

  /** The trial statuses of `from` and `to` compared, or why not (S5.8). */
  case StatusRead(from: DatasetRevision, to: DatasetRevision, status: StatusDiff)

/** What the platform or the app must do after an update. */
enum FigureEffect derives CanEqual:
  /** Dispatch an app intent (the rebind's `BindFigure`). */
  case App(intent: Intent)

  /** Read the ledgers of `from` and `to` and compare their trial statuses
    * (`StatusChanges.between`); the answer is [[FigureIntent.StatusRead]].
    */
  case RequestStatus(from: DatasetRevision, to: DatasetRevision)

/** A figure in the navigator: "Figure 2 · Stale · run 5 · rev 3 · data r2". */
final case class FigureRowVM(
    figure: FigureId,
    title: String,
    status: String,
    stale: Boolean,
    binding: String,
    reporting: String,
    selected: Boolean
) derives CanEqual

/** The stale notice under a stale figure, with Rebind… and Keep as rev N. */
final case class StaleNoticeVM(text: String, rebind: String, keep: String) derives CanEqual

/** The inspector's binding section ("Figure 1 binding · Locked · scientific"). */
final case class BindingVM(
    title: String,
    lock: String,
    run: String,
    reporting: String,
    unit: String,
    rebind: String,
    note: String
) derives CanEqual

/** The rebind dialog: the plan and data diff, shown before anything changes. */
final case class RebindDialogVM(
    title: String,
    plan: String,
    data: String,
    conflicts: Vector[String],
    confirm: String,
    cancel: String,
    canConfirm: Boolean
) derives CanEqual

final case class FiguresVM(
    header: String,
    rows: Vector[FigureRowVM],
    notice: Option[StaleNoticeVM],
    binding: Option[BindingVM],
    dialog: Option[RebindDialogVM],
    problem: Option[String]
) derives CanEqual

/** The figure model's behaviour in the Figures perspective (ticket S9.1;
  * Figures.dc.html, left and inspector). A figure binds one run and one
  * reporting spec and renders from them ([[FigureSource]]); a stale one says
  * why, from the freshness derivation and the dataset diff (S5.8), and
  * offers Rebind… and Keep as rev N. Rebinding first shows a
  * [[RebindProposal]], the plan and data diff, and only its confirmation
  * dispatches `BindFigure`, which is an ordinary undoable edit.
  *
  * `statuses` holds the trial status comparisons asked for, keyed by the
  * pair of dataset revisions: `NotRead` until the platform answers.
  */
final case class FigureBinding private (
    selected: Option[FigureId],
    kept: Vector[(FigureId, RunId)],
    rebind: Option[RebindProposal],
    statuses: Map[(DatasetRevision, DatasetRevision), StatusDiff],
    problem: Option[String]
) derives CanEqual:
  def status(from: DatasetRevision, to: DatasetRevision): StatusDiff =
    statuses.getOrElse((from, to), StatusDiff.NotRead)

object FigureBinding:
  val empty: FigureBinding = FigureBinding(None, Vector.empty, None, Map.empty, None)

  private val none: Vector[FigureEffect] = Vector.empty

  /** The dataset move a figure's run is stale for, if any. */
  private def moved(
      model: AppModel,
      figure: FigureId
  ): Option[(DatasetRevision, DatasetRevision)] =
    model.freshness
      .figure(figure)
      .flatMap(_.standing match
        case RunStanding.Stale(reasons) =>
          reasons.toVector.collectFirst { case StaleReason.DatasetMoved(f, t) => (f, t) }
        case _ => None)

  /** Ask for a status comparison not yet asked for; until its answer it is
    * [[StatusDiff.NotRead]].
    */
  private def ask(
      binding: FigureBinding,
      pair: Option[(DatasetRevision, DatasetRevision)]
  ): (FigureBinding, Vector[FigureEffect]) =
    pair.filterNot(binding.statuses.contains) match
      case Some(p) =>
        (
          binding.copy(statuses = binding.statuses.updated(p, StatusDiff.NotRead)),
          Vector(FigureEffect.RequestStatus(p._1, p._2))
        )
      case None => (binding, none)

  def update(
      binding: FigureBinding,
      model: AppModel,
      intent: FigureIntent
  ): (FigureBinding, Vector[FigureEffect]) =
    import FigureIntent.*
    intent match
      case Select(figure) =>
        ask(binding.copy(selected = Some(figure), problem = None), moved(model, figure))
      case Keep(figure) =>
        FigureSource.of(model.document, figure) match
          case Left(e)  => (binding.copy(problem = Some(e.message)), none)
          case Right(s) =>
            (
              binding.copy(
                kept = (binding.kept :+ (figure -> s.run.id)).distinct,
                problem = None
              ),
              none
            )
      case Rebind(figure) =>
        RebindProposal.target(model.freshness) match
          case None =>
            (binding.copy(problem = Some(FigureBindingText.noTarget(figure))), none)
          case Some(to) =>
            val status = FigureSource
              .of(model.document, figure)
              .toOption
              .fold(StatusDiff.NotRead)(s => binding.status(s.bound.dataset.id, to.dataset))
            RebindProposal.of(model.document, figure, to.id, status) match
              case Left(e)  => (binding.copy(problem = Some(e.message)), none)
              case Right(p) =>
                ask(
                  binding.copy(selected = Some(figure), rebind = Some(p), problem = None),
                  p.data.map(d => (d.from, d.to))
                )
      case CancelRebind  => (binding.copy(rebind = None), none)
      case ConfirmRebind =>
        binding.rebind match
          case None        => (binding, none)
          case Some(shown) =>
            // The proposal was built against an earlier model: rebuild it
            // against this one, and dispatch only what the dialog showed.
            val status = shown.data.fold(StatusDiff.NotRead)(_.status)
            RebindProposal.of(model.document, shown.figure, shown.to.run.id, status) match
              case Left(e) =>
                (binding.copy(rebind = None, problem = Some(e.message)), none)
              case Right(now) if now != shown =>
                (
                  binding.copy(
                    rebind = Some(now),
                    problem = Some(FigureBindingText.changed(shown))
                  ),
                  none
                )
              case Right(now) =>
                now.command.fold((binding, none))(bind =>
                  (
                    binding.copy(rebind = None, problem = None),
                    Vector(FigureEffect.App(Intent.Dispatch(bind)))
                  )
                )
      case StatusRead(from, to, status) =>
        status match
          case StatusDiff.Compared(c) if (c.from, c.to) != (from, to) =>
            (
              binding.copy(problem =
                Some(FigureBindingText.misdirected(from, to, c.from, c.to))
              ),
              none
            )
          case _ =>
            val updated = binding.rebind match
              case Some(p) if p.data.exists(d => d.from == from && d.to == to) =>
                p.withStatus(status).map(Some(_))
              case other => Right(other)
            updated match
              case Left(e)       => (binding.copy(problem = Some(e.message)), none)
              case Right(rebind) =>
                (
                  binding.copy(
                    statuses = binding.statuses.updated((from, to), status),
                    rebind = rebind
                  ),
                  none
                )

  /** A navigator row: the figure's binding and standing, or, when its
    * binding does not resolve, the row says why (it is never dropped).
    */
  def row(
      binding: FigureBinding,
      id: FigureId,
      source: Either[FigureError, FigureSource],
      standing: Option[RunStanding]
  ): FigureRowVM = source match
    case Left(e) =>
      FigureRowVM(
        id,
        id.label,
        "unresolved",
        false,
        e.message,
        "",
        binding.selected.contains(id)
      )
    case Right(s) =>
      FigureRowVM(
        id,
        id.label,
        FigureBindingText.status(standing, binding.kept.contains(id -> s.run.id)),
        standing.exists(_.isInstanceOf[RunStanding.Stale]),
        FigureText.binding(s.bound),
        s"Reporting: ${s.reporting.name}",
        binding.selected.contains(id)
      )

  /** The view of the Figures navigator, stale notice, binding and dialog. */
  def view(binding: FigureBinding, model: AppModel): FiguresVM =
    val document = model.document
    val resolved = document.figures.map(f => f.id -> FigureSource.of(document, f.id))
    val sources  = resolved.flatMap(_._2.toOption)
    def standing(figure: FigureId) = model.freshness.figure(figure).map(_.standing)
    def kept(s: FigureSource)      = binding.kept.contains(s.figure.id -> s.run.id)
    val rows     = resolved.map((id, source) => row(binding, id, source, standing(id)))
    val selected = binding.selected.flatMap(f => sources.find(_.figure.id == f))
    val notice   = selected.filterNot(kept).flatMap { s =>
      standing(s.figure.id).collect { case RunStanding.Stale(reasons) =>
        val data = moved(model, s.figure.id).flatMap { (from, to) =>
          (document.dataset(from), document.dataset(to)) match
            case (Some(f), Some(t)) => DatasetDiff.of(f, t, binding.status(from, to)).toOption
            case _                  => None
        }
        StaleNoticeVM(
          FigureText.stale(s.bound, reasons, data),
          "Rebind…",
          FigureText.keep(s.bound)
        )
      }
    }
    FiguresVM(
      FigureBindingText.count(rows.size),
      rows,
      notice,
      selected.map(s =>
        BindingVM(
          s"${s.figure.id.label} binding",
          "Locked · scientific",
          FigureText.binding(s.bound),
          s.reporting.name,
          FigureText.unit(s.reporting.weighting),
          "Rebind figure…",
          "One run and one reporting spec for the whole figure; methods.md cites them once. " +
            "Rebinding replaces the binding in place; Undo restores the previous one."
        )
      ),
      binding.rebind.map(p =>
        RebindDialogVM(
          FigureText.rebindTitle(p),
          FigureText.plan(p),
          FigureText.data(p),
          FigureText.conflicts(p),
          "Rebind",
          "Cancel",
          p.command.isDefined
        )
      ),
      binding.problem
    )

/** The figure binding's English text beyond [[FigureText]]. */
object FigureBindingText:
  def count(n: Int): String = if n == 1 then "1 figure" else s"$n figures"

  /** The rebind dialog was out of date when confirmed. */
  def changed(p: RebindProposal): String =
    s"${p.figure.label} or its runs changed since the rebind was proposed; review it again."

  /** A status answer whose comparison is of other revisions than asked. */
  def misdirected(
      from: DatasetRevision,
      to: DatasetRevision,
      foundFrom: DatasetRevision,
      foundTo: DatasetRevision
  ): String =
    s"The trial status comparison asked for ${from.label} → ${to.label} " +
      s"answered ${foundFrom.label} → ${foundTo.label}; it was not used."

  def noTarget(figure: FigureId): String =
    s"${figure.label} cannot be rebound: no completed run is current."

  /** The navigator chip: "current", "Stale", "Stale · kept". */
  def status(standing: Option[RunStanding], kept: Boolean): String = standing match
    case Some(RunStanding.Current)      => "current"
    case Some(RunStanding.Stale(_))     => if kept then "Stale · kept" else "Stale"
    case Some(RunStanding.Running)      => "running"
    case Some(RunStanding.Cancelled(_)) => "cancelled"
    case Some(RunStanding.Failed)       => "failed"
    case None                           => "unbound"
