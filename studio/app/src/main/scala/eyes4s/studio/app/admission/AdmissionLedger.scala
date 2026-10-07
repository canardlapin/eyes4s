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

package eyes4s.studio.app.admission

import eyes4s.codec.CanonicalDigest
import eyes4s.plan.AdmissionDecision as CoreAdmissionDecision
import eyes4s.studio.app.geometry.{GeometryPanel, Loading}
import eyes4s.studio.app.importing.InventoryAnswer
import eyes4s.studio.app.nav.{DataSection, Location, Place}
import eyes4s.studio.app.text.{LedgerText, LedgerTextId}
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.backend.{
  AdmissionSummary,
  BackendError,
  DatasetRevision,
  LedgerEntry,
  TrialKey
}
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.{
  AdmissionDecision,
  CoreBinding,
  DatasetRevisionSpec,
  Perspective
}
import eyes4s.studio.core.diff.{DiffError, LedgerUnavailable, StatusChanges, StatusDiff}
import eyes4s.studio.core.selection.{StudioRef, TallyRegion}

/** What eyes4s answered when asked to admit a dataset revision. */
enum AdmissionAnswer derives CanEqual:
  case Answered(summary: AdmissionSummary)

  /** The backend refused, naming why (an unknown dataset, a refused
    * inventory).
    */
  case Refused(error: BackendError)

  /** The backend itself failed; `reason` names what failed. */
  case Failed(reason: String)

/** A user action or platform fact the ledger's view dispatches. */
enum LedgerIntent derives CanEqual:
  /** Choose eyes4s's `AdmissionDecision` for the admission: whether it
    * requires every trial, or admits the admissible ones and records the
    * exclusions with their causes.
    */
  case ChooseDecision(decision: CoreAdmissionDecision)

  /** A count was activated: open its trials, or close them if they are open. */
  case Open(ref: StudioRef)
  case Close

  /** A trial of the opened count was activated: explain it in Explore. */
  case OpenTrial(trial: TrialKey)

  /** "Admit as rN". */
  case Admit

  /** Ask again for the shown revision's counts and ledger, after a read
    * failed.
    */
  case Retry

  /** The backend's admission summary asked for `dataset` by ask `ask`. */
  case CountsRead(dataset: DatasetRevision, ask: Int, answer: AdmissionAnswer)

  /** The backend's whole ledger of `dataset` asked for by ask `ask`, or why
    * it could not be read.
    */
  case LedgerRead(
      dataset: DatasetRevision,
      ask: Int,
      result: Either[String, Vector[LedgerEntry]]
  )

  /** The whole ledger of `parent`, the revision the shown one re-imports,
    * asked for by ask `ask`, or why it is not there (S5.8: the trials whose
    * status changed).
    */
  case ParentLedgerRead(
      parent: DatasetRevision,
      ask: Int,
      result: Either[LedgerUnavailable, Vector[LedgerEntry]]
  )

  /** The backend answered the verification of `dataset`'s `content`
    * (the app's `RequestAdmission`).
    */
  case Verified(
      dataset: DatasetRevision,
      content: CanonicalDigest[DatasetRevisionSpec],
      answer: AdmissionAnswer
  )

/** What the platform or the app must do after a ledger update. */
enum LedgerEffect derives CanEqual:
  /** Dispatch an app intent: a document command or a navigation, in order. */
  case App(intent: Intent)

  /** Ask the backend for `dataset`'s admission summary; the answer is
    * [[LedgerIntent.CountsRead]], carrying `ask`.
    */
  case RequestCounts(dataset: DatasetRevision, ask: Int)

  /** Read `dataset`'s whole ledger; the answer is [[LedgerIntent.LedgerRead]],
    * carrying `ask`.
    */
  case RequestLedger(dataset: DatasetRevision, ask: Int)

  /** Read the whole ledger of `parent`, the shown revision's parent; the
    * answer is [[LedgerIntent.ParentLedgerRead]], carrying `ask`.
    */
  case RequestParentLedger(parent: DatasetRevision, ask: Int)

/** The Data perspective's admission ledger (ticket S5.6; Data.dc.html,
  * admission). It shows eyes4s's admission of the selected dataset revision
  * as counts from its trial inventory (admitted, quarantined by cause, and
  * absent apart), opens the trials behind each count, and admits the
  * revision.
  *
  * Every count is the backend's ([[eyes4s.studio.core.selection.LedgerCounts]])
  * and carries its [[StudioRef]]. Opening a count navigates the Data trail to
  * it, so the open count is the trail's, with back and forward; its trials
  * are the backend ledger's entries with the count's disposition.
  *
  * "Admit as rN" admits the revision with the chosen decision: Require
  * complete refuses while any trial is quarantined. A pending revision is
  * first sent for verification ([[Command.VerifyDataset]]); when eyes4s's
  * answer for exactly the verified content arrives ([[LedgerIntent.Verified]])
  * the revision is admitted ([[Command.Admit]]), which records the decision
  * in the revision. Admission is a history barrier, and the runs on older
  * data become stale by the freshness derivation (S2.7).
  *
  * `ask` numbers the ledger's reads of the shown revision: an edit to its
  * content, or [[LedgerIntent.Retry]], asks again, and an answer to an
  * earlier ask is ignored.
  */
final case class AdmissionLedger(
    shown: Option[DatasetRevisionSpec],
    counts: Loading[AdmissionSummary],
    refusal: Option[BackendError],
    entries: Loading[Vector[LedgerEntry]],
    ask: Int,
    decision: CoreAdmissionDecision,
    admitting: Option[DatasetRevision],
    problem: Option[String],
    parentLedger: Option[Either[LedgerUnavailable, Vector[LedgerEntry]]]
) derives CanEqual:

  /** Whether the shown revision's trial statuses were compared with its
    * parent's (S5.8): both ledgers read, or the parent's unavailable.
    */
  def status: StatusDiff =
    (shown.flatMap(s => s.parent.map(s.id -> _)), parentLedger, entries) match
      case (Some((_, parent)), Some(Left(why)), _) => StatusDiff.Unavailable(parent, why)
      case (Some((id, _)), Some(Right(_)), Loading.Failed(why)) =>
        StatusDiff.Unavailable(id, LedgerUnavailable.Failed(why))
      case (Some((id, parent)), Some(Right(before)), Loading.Ready(after)) =>
        StatusChanges
          .between(parent, before, id, after)
          .fold(
            e =>
              val dataset = e match
                case DiffError.RepeatedTrials(d, _) => d
                case _                              => parent
              StatusDiff.Unavailable(dataset, LedgerUnavailable.Failed(e.message))
            ,
            StatusDiff.Compared(_)
          )
      case _ => StatusDiff.NotRead

  /** What admission has said about the shown revision's inventory. */
  def inventory: InventoryAnswer = (counts, refusal) match
    case (Loading.Ready(s), _) => InventoryAnswer.Summary(s)
    case (_, Some(e))          => InventoryAnswer.Refused(e)
    case _                     => InventoryAnswer.NotAsked

object AdmissionLedger:

  val empty: AdmissionLedger =
    AdmissionLedger(
      None,
      Loading.Idle,
      None,
      Loading.Idle,
      0,
      CoreAdmissionDecision.RequireComplete,
      None,
      None,
      None
    )

  private val none: Vector[LedgerEffect] = Vector.empty

  private def t(id: LedgerTextId, args: String*): String = LedgerText(id, args*)

  /** The count the Data trail has open for `dataset`, if any: its last
    * place, when that is a count of the dataset's ledger.
    */
  def opened(model: AppModel, dataset: DatasetRevision): Option[StudioRef] =
    model.navigation.trail(Perspective.Data).lastOption.collect {
      case Place.At(ref @ StudioRef.InventoryCount(d, _)) if d == dataset => ref
      case Place.At(ref @ StudioRef.WindowTally(d, TallyRegion.OutsideScreen))
          if d == dataset =>
        ref
    }

  /** The Data trail of the ledger of `dataset`, open at `ref` if given: a
    * cause under the quarantined count that holds it.
    */
  def trail(dataset: DatasetRevision, ref: Option[StudioRef]): Vector[Place] =
    Vector(Place.Dataset(dataset), Place.DataView(DataSection.Admission)) ++
      ref.toVector.flatMap(r => r.parent.toVector.map(Place.At(_)) :+ Place.At(r))

  /** The trials quarantined or with no admissible record. */
  def heldBack(summary: AdmissionSummary): Int = summary.quarantinedTotal

  /** Whether `decision` admits a revision with these counts. */
  def permits(decision: CoreAdmissionDecision, summary: AdmissionSummary): Boolean =
    decision == CoreAdmissionDecision.ReviewExclusions || heldBack(summary) == 0

  /** What a revision asks eyes4s to admit: everything but its decision. */
  private def content(spec: DatasetRevisionSpec): DatasetRevisionSpec =
    spec.copy(decision = AdmissionDecision.Pending)

  /** Ask again for the counts and the ledger of `spec`; a problem with the
    * earlier answers no longer stands.
    */
  private def askFor(ledger: AdmissionLedger, spec: DatasetRevisionSpec) =
    val next = ledger.ask + 1
    (
      ledger.copy(
        shown = Some(spec),
        counts = Loading.Waiting,
        refusal = None,
        entries = Loading.Waiting,
        ask = next,
        problem = None,
        parentLedger = None
      ),
      Vector(
        LedgerEffect.RequestCounts(spec.id, next),
        LedgerEffect.RequestLedger(spec.id, next)
      ) ++ spec.parent.map(LedgerEffect.RequestParentLedger(_, next))
    )

  /** Follow the model's selected dataset revision: a newly selected one
    * resets the ledger and asks for its counts and its ledger; the same one
    * keeps them and follows its admission decision, and asks again when its
    * content changed (an edit to a pending revision). A revision that is
    * pending again (its verification refused or withdrawn) is no longer
    * waiting to be admitted.
    */
  def sync(ledger: AdmissionLedger, model: AppModel): (AdmissionLedger, Vector[LedgerEffect]) =
    val now = GeometryPanel.selected(model)
    (ledger.shown, now) match
      case (Some(was), Some(spec)) if was.id == spec.id =>
        val pending = spec.decision == AdmissionDecision.Pending
        val kept    = ledger.copy(admitting = ledger.admitting.filterNot(_ => pending))
        if content(was) == content(spec) then (kept.copy(shown = now), none)
        else askFor(kept, spec)
      case (_, None)       => (empty, none)
      case (_, Some(spec)) => askFor(empty.copy(ask = ledger.ask), spec)

  /** The Elm-style update: pure; effects are data. */
  def update(
      ledger: AdmissionLedger,
      model: AppModel,
      intent: LedgerIntent
  ): (AdmissionLedger, Vector[LedgerEffect]) =
    import LedgerIntent.*
    val live = ledger.shown.flatMap(s => model.document.dataset(s.id))
    intent match
      case ChooseDecision(d) => (ledger.copy(decision = d, problem = None), none)
      case Open(ref)         =>
        live.fold((ledger, none)) { spec =>
          val next = if opened(model, spec.id).contains(ref) then None else Some(ref)
          (ledger, Vector(navigate(spec.id, next)))
        }
      case Close =>
        live.fold((ledger, none))(spec => (ledger, Vector(navigate(spec.id, None))))
      case OpenTrial(key) =>
        (ledger, Vector(LedgerEffect.App(Intent.Explain(Place.At(StudioRef.Trial(key))))))
      case Admit => admit(ledger, live)
      case Retry =>
        live.fold((ledger, none))(askFor(ledger, _))
      case CountsRead(dataset, n, ans) =>
        if !answers(ledger, dataset, n) then (ledger, none)
        else (answered(ledger, ans), none)
      case LedgerRead(dataset, n, result) =>
        if !answers(ledger, dataset, n) then (ledger, none)
        else (ledger.copy(entries = result.fold(Loading.Failed(_), Loading.Ready(_))), none)
      case ParentLedgerRead(parent, n, result) =>
        if !(ledger.shown.exists(_.parent.contains(parent)) && ledger.ask == n) then
          (ledger, none)
        else (ledger.copy(parentLedger = Some(result)), none)
      case Verified(dataset, content, ans) =>
        if !ledger.shown.exists(_.id == dataset) then
          (ledger.copy(admitting = ledger.admitting.filterNot(_ == dataset)), none)
        else
          val read    = answered(ledger, ans)
          val waiting = ledger.admitting.contains(dataset)
          val cleared = read.copy(admitting = None)
          (ans, live.map(_.decision)) match
            case (AdmissionAnswer.Answered(s), Some(AdmissionDecision.Verifying(c)))
                if waiting && c == content =>
              if permits(ledger.decision, s) then
                (cleared, Vector(admitCommand(dataset, c, ledger.decision)))
              else (cleared.copy(problem = Some(refusedText(dataset, s))), none)
            case (AdmissionAnswer.Answered(_), _) => (cleared, none)
            case (AdmissionAnswer.Refused(e), _)  =>
              (cleared.copy(problem = Option.when(waiting)(e.message)), none)
            case (AdmissionAnswer.Failed(why), _) =>
              (cleared.copy(problem = Option.when(waiting)(why)), none)

  /** Whether a read of `dataset` by ask `n` answers the ledger's last ask. */
  private def answers(ledger: AdmissionLedger, dataset: DatasetRevision, n: Int): Boolean =
    ledger.shown.exists(_.id == dataset) && ledger.ask == n

  private def navigate(dataset: DatasetRevision, ref: Option[StudioRef]): LedgerEffect =
    LedgerEffect.App(Intent.Navigate(Location(Perspective.Data, trail(dataset, ref))))

  private def answered(ledger: AdmissionLedger, answer: AdmissionAnswer): AdmissionLedger =
    answer match
      case AdmissionAnswer.Answered(s) => ledger.copy(counts = Loading.Ready(s), refusal = None)
      case AdmissionAnswer.Refused(e)  =>
        ledger.copy(counts = Loading.Failed(e.message), refusal = Some(e))
      case AdmissionAnswer.Failed(why) =>
        ledger.copy(counts = Loading.Failed(why), refusal = None)

  private def refusedText(dataset: DatasetRevision, s: AdmissionSummary): String =
    t(
      LedgerTextId.AdmitRefused,
      dataset.label,
      heldBack(s).toString,
      s.admitted.toString
    )

  /** Admit `dataset`'s verified `content` under `decision`, which the
    * revision records.
    */
  private def admitCommand(
      dataset: DatasetRevision,
      content: CanonicalDigest[DatasetRevisionSpec],
      decision: CoreAdmissionDecision
  ): LedgerEffect =
    // The backend binds the eyes4s ledger and inventory artifacts (S3.7); the
    // fake has none to bind.
    LedgerEffect.App(
      Intent.Dispatch(
        Command.Admit(
          dataset,
          content,
          Some(decision),
          CoreBinding.unbound,
          CoreBinding.unbound
        )
      )
    )

  /** Why "Admit as rN" does nothing now, if it does nothing. */
  def blocked(ledger: AdmissionLedger, spec: DatasetRevisionSpec): Option[String] =
    spec.decision match
      case AdmissionDecision.Admitted(_, _, _) =>
        Some(t(LedgerTextId.AdmittedStatus, spec.id.label))
      case _ if ledger.admitting.contains(spec.id) =>
        Some(t(LedgerTextId.AdmitVerifying, spec.id.label))
      case _ =>
        ledger.counts match
          case Loading.Ready(s) if s.dataset == spec.id =>
            Option.unless(permits(ledger.decision, s))(refusedText(spec.id, s))
          case Loading.Failed(why) => Some(t(LedgerTextId.CountsFailed, spec.id.label, why))
          case _                   => Some(t(LedgerTextId.AdmitWaiting, spec.id.label))

  private def admit(
      ledger: AdmissionLedger,
      live: Option[DatasetRevisionSpec]
  ): (AdmissionLedger, Vector[LedgerEffect]) =
    live match
      case None       => (ledger, none)
      case Some(spec) =>
        blocked(ledger, spec) match
          case Some(why) => (ledger.copy(problem = Some(why)), none)
          case None      =>
            spec.decision match
              case AdmissionDecision.Pending =>
                (
                  ledger.copy(admitting = Some(spec.id), problem = None),
                  Vector(LedgerEffect.App(Intent.Dispatch(Command.VerifyDataset(spec.id))))
                )
              case AdmissionDecision.Verifying(content) =>
                (
                  ledger.copy(problem = None),
                  Vector(admitCommand(spec.id, content, ledger.decision))
                )
              case AdmissionDecision.Admitted(_, _, _) => (ledger, none)
