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

/** eyes4s `AdmissionDecision`: whether admission requires every trial, or
  * admits the admissible ones and records the exclusions with their causes.
  */
enum LedgerDecision derives CanEqual:
  case RequireComplete, ReviewExclusions

/** A user action or platform fact the ledger's view dispatches. */
enum LedgerIntent derives CanEqual:
  case ChooseDecision(decision: LedgerDecision)

  /** A count was activated: open its trials, or close them if they are open. */
  case Open(ref: StudioRef)
  case Close

  /** A trial of the opened count was activated: explain it in Explore. */
  case OpenTrial(trial: TrialKey)

  /** "Admit as rN". */
  case Admit

  /** The backend's admission summary asked for `dataset`. */
  case CountsRead(dataset: DatasetRevision, answer: AdmissionAnswer)

  /** The backend's whole ledger of `dataset`, or why it could not be read. */
  case LedgerRead(dataset: DatasetRevision, result: Either[String, Vector[LedgerEntry]])

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
    * [[LedgerIntent.CountsRead]].
    */
  case RequestCounts(dataset: DatasetRevision)

  /** Read `dataset`'s whole ledger; the answer is [[LedgerIntent.LedgerRead]]. */
  case RequestLedger(dataset: DatasetRevision)

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
  * the revision is admitted ([[Command.Admit]]). Admission is a history
  * barrier, and the runs on older data become stale by the freshness
  * derivation (S2.7).
  */
final case class AdmissionLedger(
    shown: Option[DatasetRevisionSpec],
    counts: Loading[AdmissionSummary],
    refusal: Option[BackendError],
    entries: Loading[Vector[LedgerEntry]],
    decision: LedgerDecision,
    admitting: Option[DatasetRevision],
    problem: Option[String]
) derives CanEqual:

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
      LedgerDecision.RequireComplete,
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
      case Place.At(ref @ StudioRef.WindowTally(d, TallyRegion.OutsideScreen)) if d == dataset =>
        ref
    }

  /** The Data trail of the ledger of `dataset`, open at `ref` if given: a
    * cause under the quarantined count that holds it.
    */
  def trail(dataset: DatasetRevision, ref: Option[StudioRef]): Vector[Place] =
    Vector(Place.Dataset(dataset), Place.DataView(DataSection.Admission)) ++
      ref.toVector.flatMap(r => r.parent.toVector.map(Place.At(_)) :+ Place.At(r))

  /** The trials quarantined or with no admissible record. */
  def heldBack(summary: AdmissionSummary): Int = summary.quarantinedTrials + summary.noFixations

  /** Whether `decision` admits a revision with these counts. */
  def permits(decision: LedgerDecision, summary: AdmissionSummary): Boolean =
    decision == LedgerDecision.ReviewExclusions || heldBack(summary) == 0

  /** Follow the model's selected dataset revision: a newly selected one
    * resets the ledger and asks for its counts and its ledger; the same one
    * keeps them and follows its admission decision. A revision that is
    * pending again (its verification refused or withdrawn) is no longer
    * waiting to be admitted.
    */
  def sync(ledger: AdmissionLedger, model: AppModel): (AdmissionLedger, Vector[LedgerEffect]) =
    val now = GeometryPanel.selected(model)
    if now.map(_.id) == ledger.shown.map(_.id) then
      val pending = now.exists(_.decision == AdmissionDecision.Pending)
      (ledger.copy(shown = now, admitting = ledger.admitting.filterNot(_ => pending)), none)
    else
      now match
        case None       => (empty, none)
        case Some(spec) =>
          (
            empty.copy(shown = Some(spec), counts = Loading.Waiting, entries = Loading.Waiting),
            Vector(LedgerEffect.RequestCounts(spec.id), LedgerEffect.RequestLedger(spec.id))
          )

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
      case Admit                    => admit(ledger, live)
      case CountsRead(dataset, ans) =>
        if !ledger.shown.exists(_.id == dataset) then (ledger, none)
        else (answered(ledger, ans), none)
      case LedgerRead(dataset, result) =>
        if !ledger.shown.exists(_.id == dataset) then (ledger, none)
        else (ledger.copy(entries = result.fold(Loading.Failed(_), Loading.Ready(_))), none)
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
              if permits(ledger.decision, s) then (cleared, Vector(admitCommand(dataset, c)))
              else (cleared.copy(problem = Some(refusedText(dataset, s))), none)
            case (AdmissionAnswer.Answered(_), _) => (cleared, none)
            case (AdmissionAnswer.Refused(e), _)  =>
              (cleared.copy(problem = Option.when(waiting)(e.message)), none)
            case (AdmissionAnswer.Failed(why), _) =>
              (cleared.copy(problem = Option.when(waiting)(why)), none)

  private def navigate(dataset: DatasetRevision, ref: Option[StudioRef]): LedgerEffect =
    LedgerEffect.App(Intent.Navigate(Location(Perspective.Data, trail(dataset, ref))))

  private def answered(ledger: AdmissionLedger, answer: AdmissionAnswer): AdmissionLedger =
    answer match
      case AdmissionAnswer.Answered(s) => ledger.copy(counts = Loading.Ready(s), refusal = None)
      case AdmissionAnswer.Refused(e)  =>
        ledger.copy(counts = Loading.Failed(e.message), refusal = Some(e))
      case AdmissionAnswer.Failed(why) => ledger.copy(counts = Loading.Failed(why), refusal = None)

  private def refusedText(dataset: DatasetRevision, s: AdmissionSummary): String =
    t(
      LedgerTextId.AdmitRefused,
      dataset.label,
      heldBack(s).toString,
      s.admitted.toString
    )

  private def admitCommand(
      dataset: DatasetRevision,
      content: CanonicalDigest[DatasetRevisionSpec]
  ): LedgerEffect =
    // The backend binds the eyes4s ledger and inventory artifacts (S3.7); the
    // fake has none to bind.
    LedgerEffect.App(
      Intent.Dispatch(Command.Admit(dataset, content, CoreBinding.unbound, CoreBinding.unbound))
    )

  /** Why "Admit as rN" does nothing now, if it does nothing. */
  def blocked(ledger: AdmissionLedger, spec: DatasetRevisionSpec): Option[String] =
    spec.decision match
      case AdmissionDecision.Admitted(_, _) => Some(t(LedgerTextId.AdmittedStatus, spec.id.label))
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
                (ledger.copy(problem = None), Vector(admitCommand(spec.id, content)))
              case AdmissionDecision.Admitted(_, _) => (ledger, none)
