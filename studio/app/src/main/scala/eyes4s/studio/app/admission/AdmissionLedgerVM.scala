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

import eyes4s.studio.app.AppModel
import eyes4s.studio.app.geometry.Loading
import eyes4s.studio.app.importing.{InventorySource, InventorySourceVM}
import eyes4s.studio.app.text.{Format, LedgerText, LedgerTextId}
import eyes4s.studio.core.backend.{
  AdmissionSummary,
  DatasetRevision,
  LedgerEntry,
  RunId,
  TrialDisposition,
  TrialKey
}
import eyes4s.studio.core.document.{DatasetRevisionSpec, OffScreenChoice}
import eyes4s.studio.core.freshness.{RunStanding, StaleReason}
import eyes4s.studio.core.selection.{InventoryKind, LedgerCounts, StudioRef, TallyRegion}

/** How a count row is set: the inventory total, a disposition, the
  * quarantined group, one of its causes (indented), or a reported count.
  */
enum CountStyle derives CanEqual:
  case Total, Plain, Group, Cause, Reported

/** One count of the ledger: what it counts, the backend's number (or "—"
  * when there is none to show, with `note` saying why), and `ref`, which it
  * opens. `detail` qualifies the number ("0 records").
  */
final case class CountRowVM(
    ref: StudioRef,
    label: String,
    value: String,
    detail: Option[String],
    note: Option[String],
    style: CountStyle,
    accessible: String,
    counted: Boolean,
    opened: Boolean
) derives CanEqual

/** A term the ledger defines ("no-fixations", ": the trial has …"). */
final case class DefinitionVM(term: String, text: String) derives CanEqual

/** One trial behind an open count: its label, match item, why it is where it
  * is, and its accessible name. Activating it explains the trial.
  */
final case class TrialRowVM(
    trial: TrialKey,
    ref: StudioRef,
    label: String,
    item: String,
    detail: String,
    accessible: String
) derives CanEqual

/** The trials of the open count, or why they are not listed yet. */
final case class OpenedVM(
    ref: StudioRef,
    title: String,
    rows: Vector[TrialRowVM],
    note: Option[String],
    close: String
) derives CanEqual

/** One admission decision, with what it would do to these counts. */
final case class DecisionVM(value: LedgerDecision, label: String, note: String, selected: Boolean)
    derives CanEqual

/** Everything the admission ledger shows, in the board's order. */
final case class AdmissionLedgerVM(
    title: String,
    empty: Option[String],
    header: String,
    trialsHeader: String,
    rows: Vector[CountRowVM],
    equation: Option[String],
    issues: Vector[String],
    definitions: Vector[DefinitionVM],
    hint: String,
    opened: Option[OpenedVM],
    decisionTitle: String,
    decisionLegend: String,
    decisions: Vector[DecisionVM],
    canDecide: Boolean,
    changes: Option[String],
    consequence: Option[String],
    admit: String,
    canAdmit: Boolean,
    admitNote: Option[String],
    status: Option[String],
    countsSource: String,
    problem: Option[String]
) derives CanEqual

object AdmissionLedgerVM:
  import LedgerTextId.*

  private def t(id: LedgerTextId, args: String*): String = LedgerText(id, args*)

  private def n(value: Int): String = Format.count(value.toLong)

  /** A cause's name as the ledger shows it: its eyes4s code without the
    * family ("overlap").
    */
  def causeName(code: String): String = code.stripPrefix("quarantine.")

  /** What `ref` counts, as its row and the crumb name it. */
  def countLabel(ref: StudioRef): String = ref match
    case StudioRef.InventoryCount(_, kind) =>
      kind match
        case InventoryKind.Inventory   => t(Inventory)
        case InventoryKind.Admitted    => t(Admitted)
        case InventoryKind.Quarantined => t(QuarantinedName)
        case InventoryKind.Cause(code) => causeName(code)
        case InventoryKind.NoFixations => t(NoFixations)
        case InventoryKind.Absent      => t(AbsentTerm)
    case StudioRef.WindowTally(_, _) => t(OutsideScreenName)
    case _                           => ""

  /** "Quarantined · overlap": a count with the count that holds it. */
  def countTitle(ref: StudioRef): String =
    ref.parent.fold(countLabel(ref))(p => t(CrumbCause, countLabel(p), countLabel(ref)))

  /** Why a trial of the ledger is where it is. */
  def detail(entry: LedgerEntry): String = entry.disposition match
    case TrialDisposition.Quarantined(cause) => s"${causeName(cause.code)}: ${cause.message}"
    case TrialDisposition.NoFixations        => t(TrialNoFixations)
    case TrialDisposition.Absent             => t(TrialAbsent)
    case TrialDisposition.Admitted           =>
      if entry.outsideFrame.isEmpty then t(TrialAdmitted)
      else
        t(
          TrialOutside,
          n(entry.outsideFrame.size),
          entry.outsideFrame.map(o => n(o.record)).mkString(", ")
        )

  def trialRow(entry: LedgerEntry): TrialRowVM =
    val label = entry.trial.label
    val why   = detail(entry)
    TrialRowVM(
      entry.trial,
      StudioRef.Trial(entry.trial),
      label,
      entry.item,
      why,
      t(TrialAccessible, label, entry.item, why)
    )

  /** The ledger's view-model. */
  def of(ledger: AdmissionLedger, model: AppModel): AdmissionLedgerVM =
    val spec    = ledger.shown.flatMap(s => model.document.dataset(s.id))
    val summary = ledger.counts.toOption.filter(s => spec.exists(_.id == s.dataset))
    val id      = spec.map(_.id)
    val label   = id.fold("")(_.label)
    val source  = spec.flatMap(_.sources.fixations).fold("fixations.csv")(f => file(f.path.value))
    val inv     = spec.map(InventorySource.vm(_, ledger.inventory))
    val header  = inv.flatMap(_.file).fold(t(CountedFromRecords, source))(f =>
      t(CountedFromInventory, f)
    )
    val open = id.flatMap(AdmissionLedger.opened(model, _))
    val rows = (id, summary) match
      case (Some(d), Some(s)) => countRows(d, s, inv, policyOf(model, s, spec), open)
      case _                  => Vector.empty
    val waiting = spec.fold("")(s => s.id.label)
    AdmissionLedgerVM(
      title = t(Title),
      empty = Option.when(spec.isEmpty)(t(NoDataset)),
      header = header,
      trialsHeader = t(TrialsHeader),
      rows = rows,
      equation = summary.flatMap(equation),
      issues = inv.toVector.flatMap(_.issues.map(_.text)),
      definitions = Vector(
        DefinitionVM(t(NoFixationsTerm), t(NoFixationsDefinition)),
        DefinitionVM(t(AbsentTerm), t(AbsentDefinition, source))
      ),
      hint = t(OpensHint),
      opened = for d <- id; ref <- open yield openedVM(ledger, d, ref, summary),
      decisionTitle = t(DecisionTitle, label),
      decisionLegend = t(DecisionLegend),
      decisions = decisions(ledger, label, summary),
      canDecide = spec.exists(s => !s.decision.isAdmitted) && ledger.admitting.isEmpty,
      changes = for
        s      <- summary
        parent <- spec.flatMap(_.parent)
        if s.history.nonEmpty
      yield t(Changes, parent.label, s.history),
      consequence = spec.filter(!_.decision.isAdmitted).map(s => consequence(model, s.id)),
      admit = t(Admit, label),
      canAdmit = spec.exists(s => AdmissionLedger.blocked(ledger, s).isEmpty),
      admitNote = spec.flatMap(s =>
        if s.decision.isAdmitted then None else AdmissionLedger.blocked(ledger, s)
      ),
      status = spec.filter(_.decision.isAdmitted).map(s => admittedStatus(model, s.id)),
      countsSource = ledger.counts match
        case Loading.Ready(s)    => t(CountsFrom, s.dataset.label)
        case Loading.Failed(why) => t(CountsFailed, waiting, why)
        case _                   => t(CountsWaiting)
      ,
      problem = ledger.problem
    )

  private def file(path: String): String = path.split('/').last

  /** The off-screen policy of the revision the counts were admitted under. */
  private def policyOf(
      model: AppModel,
      s: AdmissionSummary,
      spec: Option[DatasetRevisionSpec]
  ): OffScreenChoice =
    model.document
      .dataset(s.dataset)
      .orElse(spec)
      .fold(OffScreenChoice.ExcludeRecord)(_.admission.offScreen)

  private def countRows(
      d: DatasetRevision,
      s: AdmissionSummary,
      inv: Option[InventorySourceVM],
      policy: OffScreenChoice,
      open: Option[StudioRef]
  ): Vector[CountRowVM] =
    def row(
        kind: InventoryKind,
        label: String,
        style: CountStyle,
        note: Option[String] = None
    ): CountRowVM =
      val ref   = StudioRef.InventoryCount(d, kind)
      val value = LedgerCounts.count(ref, s)
      val shown = value.fold(t(Uncounted))(n)
      val name  = kind match
        case InventoryKind.Cause(code) => t(CauseAccessible, shown, causeName(code))
        case InventoryKind.NoFixations => t(CauseAccessible, shown, t(NoFixations))
        case _                         => t(CountAccessible, countLabel(ref), shown)
      CountRowVM(
        ref,
        label,
        shown,
        None,
        if value.isEmpty then note else None,
        style,
        name,
        value.isDefined,
        open.contains(ref)
      )
    val causes =
      (s.quarantined.map(q => InventoryKind.Cause(q.code) -> causeName(q.code)) ++
        Option.when(s.noFixations > 0)(InventoryKind.NoFixations -> t(NoFixations)))
        .sortBy(_._2)
        .map((kind, name) => row(kind, name, CountStyle.Cause))
    val absent = inv match
      case Some(vm) =>
        row(
          InventoryKind.Absent,
          t(AbsentRow, vm.absentLabel, vm.absentDefinition),
          CountStyle.Plain,
          vm.absentNote
        )
      case None => row(InventoryKind.Absent, t(AbsentTerm), CountStyle.Plain)
    val outside = Option.when(policy == OffScreenChoice.ExcludeRecord) {
      val ref    = StudioRef.WindowTally(d, TallyRegion.OutsideScreen)
      val trials = n(s.window.trialsOutsideScreen)
      CountRowVM(
        ref,
        t(OutsideScreen),
        trials,
        Some(t(OutsideScreenRecords, n(s.window.outsideScreen))),
        None,
        CountStyle.Reported,
        t(CountAccessible, countLabel(ref), trials),
        counted = true,
        opened = open.contains(ref)
      )
    }
    Vector(
      row(
        InventoryKind.Inventory,
        t(Inventory),
        CountStyle.Total,
        inv.map(_.detail)
      ),
      row(InventoryKind.Admitted, t(Admitted), CountStyle.Plain),
      row(InventoryKind.Quarantined, t(Quarantined), CountStyle.Group)
    ) ++ causes ++ Vector(absent) ++ outside

  /** "937 + 17 + 6 = 960.", or what does not add up. Only a joined
    * inventory has all four counts.
    */
  private def equation(s: AdmissionSummary): Option[String] =
    for
      trials <- s.inventoryTrials
      absent <- s.absent
    yield
      val held = AdmissionLedger.heldBack(s)
      val args = Vector(n(s.admitted), n(held), n(absent), n(trials))
      if s.admitted + held + absent == trials then t(Equation, args*)
      else t(EquationMismatch, args*)

  private def openedVM(
      ledger: AdmissionLedger,
      dataset: DatasetRevision,
      ref: StudioRef,
      summary: Option[AdmissionSummary]
  ): OpenedVM =
    val shown   = summary.flatMap(LedgerCounts.count(ref, _)).fold(t(Uncounted))(n)
    val title   = t(OpenedTitle, countTitle(ref), shown)
    val entries = ledger.entries.toOption.flatMap(LedgerCounts.trials(ref, dataset, _))
    val note    = ledger.entries match
      case Loading.Failed(why)           => Some(t(LedgerFailed, dataset.label, why))
      case Loading.Waiting               => Some(t(LedgerWaiting, dataset.label))
      case _ if entries.exists(_.isEmpty) => Some(t(NoTrials))
      case _                             => None
    OpenedVM(ref, title, entries.toVector.flatten.map(trialRow), note, t(Close))

  private def decisions(
      ledger: AdmissionLedger,
      label: String,
      summary: Option[AdmissionSummary]
  ): Vector[DecisionVM] =
    val dash     = t(Uncounted)
    val held     = summary.map(AdmissionLedger.heldBack)
    val absent   = summary.flatMap(_.absent)
    val admitted = summary.fold(dash)(s => n(s.admitted))
    val require  = held match
      case Some(0) => t(RequireCompleteAdmits, label)
      case _       =>
        val refuses = t(RequireCompleteRefuses, label, held.fold(dash)(n))
        absent.fold(refuses)(a => s"$refuses ${t(AbsentJoined, n(a))}")
    val review = absent match
      case Some(a) => t(ReviewExclusionsNote, admitted, held.fold(dash)(n), n(a), label)
      case None    => t(ReviewExclusionsNoInventory, admitted, held.fold(dash)(n), label)
    Vector(
      DecisionVM(
        LedgerDecision.RequireComplete,
        t(RequireComplete),
        require,
        ledger.decision == LedgerDecision.RequireComplete
      ),
      DecisionVM(
        LedgerDecision.ReviewExclusions,
        t(ReviewExclusions),
        review,
        ledger.decision == LedgerDecision.ReviewExclusions
      )
    )

  /** "Run 5". */
  private def runName(run: RunId): String = run.label.capitalize

  /** "Admitting creates dataset r3. Run 5 stays on r2 and is marked stale." */
  private def consequence(model: AppModel, dataset: DatasetRevision): String =
    val stale = model.freshness.pending
      .find(_.dataset == dataset)
      .toVector
      .flatMap(_.wouldStale)
      .flatMap(r => model.document.run(r))
      .map(r => t(WouldStale, runName(r.id), r.dataset.label))
    (t(Creates, dataset.label) +: stale).mkString(" ")

  /** "r3 is admitted. Run 5 (rev 3) used r2 and is now stale." */
  private def admittedStatus(model: AppModel, dataset: DatasetRevision): String =
    val stale = model.freshness.runs.collect {
      case f if f.run.dataset != dataset && (f.standing match
            case RunStanding.Stale(reasons) =>
              reasons.exists {
                case StaleReason.DatasetMoved(_, latest) => latest == dataset
                case _                                   => false
              }
            case _ => false) =>
        t(NowStale, runName(f.run.id), f.run.analysis.label, f.run.dataset.label)
    }
    (t(AdmittedStatus, dataset.label) +: stale :+ t(AdmittedNote)).mkString(" ")
