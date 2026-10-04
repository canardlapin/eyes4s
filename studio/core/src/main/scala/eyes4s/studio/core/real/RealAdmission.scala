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

package eyes4s.studio.core.real

import cats.syntax.all.*
import eyes4s.io.*
import eyes4s.kernel.*
import eyes4s.plan.{
  AdmissionDecision as CoreDecision,
  AdmissionLedger,
  AttributeColumn,
  AttributeKind,
  AttributeValue,
  InventoryTrial,
  OutsideFrame as CoreOutsideFrame,
  SampleCountRule,
  TrialDisposition as CoreDisposition,
  TrialKey as CoreKey,
  WindowSummary,
  WindowTally
}
import eyes4s.studio.core.assets.AssetRegistry
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.{ColumnRole, DatasetRevisionSpec, InventoryMapping, TimeUnit}

/** One dataset revision as eyes4s admitted it, converted for the protocol:
  * the admission summary and every inventory trial's ledger entry, in
  * inventory order. Built once per dataset revision.
  */
final case class AdmittedDataset(
    summary: AdmissionSummary,
    ledger: Vector[LedgerEntry],
    screen: Frame[Unit2D.Px],
    input: eyes4s.plan.StudyInput[CoreKey, Unit2D.Px]
)

/** The admission of a [[DatasetRevisionSpec]] through eyes4s-io (S3.7 slice 1).
  *
  * Every count comes from eyes4s: the inventory join and each trial's
  * disposition from `FixationCsv.admitInventory`, the out-of-window and
  * out-of-screen counts from `WindowTally.window` over each admitted
  * scanpath and the image window of the revision's geometry, and their
  * totals from `WindowSummary.of`. This file only builds eyes4s's inputs from
  * the revision's recorded mapping, units, geometry and admission choice,
  * and converts eyes4s's answer into protocol values. The images found and
  * missing are the host's stimulus registry's ([[AssetRegistry]], S2.10),
  * not eyes4s's.
  */
object RealAdmission:

  /** Admit `spec`'s sources: `fixations` and `trials` are their exact text.
    *
    * Until protocol 1.11 adds a typed admission refusal, every refusal other
    * than the inventory's is `Unavailable` with a locus naming what is
    * missing (a field) or what eyes4s refused (the source and eyes4s's
    * message), never a bare dataset.
    */
  def admit(
      spec: DatasetRevisionSpec,
      fixations: String,
      trials: String,
      assets: AssetRegistry
  ): Either[BackendError, AdmittedDataset] =
    val d                                                                    = spec.id
    def missing[A](field: String)(value: Option[A]): Either[BackendError, A] =
      value.toRight(BackendError.Unavailable(DiagnosticLocus.Field(field)))
    val source = spec.sources.fixations.fold("fixations")(_.path.value)
    for
      _ <- missing("coordinate corrections (not yet served)")(
        Option.when(spec.admission.corrections.isEmpty)(())
      )
      inventory <- missing("trial inventory")(spec.inventory)
      unit      <- missing("time units")(spec.units.time.map(timeUnit))
      table     <- fixationTable(spec, unit)
      columns   <- inventoryColumns(inventory)
      read      <- TrialInventory.read(trials, columns).leftMap(refusal(d, source, _))
      screen    <- Frame
        .screen("screen", spec.geometry.screen.width, spec.geometry.screen.height)
        .leftMap(e => geometry("screen", e))
      window   <- imageWindow(spec, screen)
      imported <- FixationCsv
        .admitInventory(fixations, table, read, screen, admissionPolicy(spec))
        .leftMap(refusal(d, source, _))
      ledger <- FixationEvidence
        .ledger(
          source,
          spec.sources.trials.fold("trials")(_.path.value),
          imported,
          spec.decision.admittedUnder.getOrElse(CoreDecision.ReviewExclusions)
        )
        .leftMap(e =>
          BackendError.Unavailable(DiagnosticLocus.Artifact(s"$source: ${e.message}"))
        )
      response = inventory.column(ColumnRole.Response).map(_.value)
      outside  = outsideFrameByTrial(imported)
      entries <- imported.trials.traverse(entry(_, response, outside))
    yield
      // The admitted trials; an admission that requires a complete input
      // refused before reaching here (eyes4s ReviewExclusions semantics).
      AdmittedDataset(
        summary(spec, imported, ledger, window, assets),
        entries,
        screen,
        eyes4s.plan.StudyInput(imported.fixations.accepted)
      )

  private def geometry(what: String, e: GeometryError): BackendError =
    BackendError.Unavailable(DiagnosticLocus.Field(s"$what: ${e.message}"))

  // ------------------------------------------------------------------ eyes4s inputs

  private def timeUnit(unit: TimeUnit): TimestampUnit = unit match
    case TimeUnit.Milliseconds => TimestampUnit.Milliseconds
    case TimeUnit.Microseconds => TimestampUnit.Microseconds
    case TimeUnit.Seconds      => TimestampUnit.Seconds

  /** The fixation table's declared columns, from the revision's mapping. */
  private def fixationTable(
      spec: DatasetRevisionSpec,
      unit: TimestampUnit
  ): Either[BackendError, FixationTable] =
    val m                                                    = spec.mapping
    def name(role: ColumnRole): Either[BackendError, String] =
      m.column(role)
        .map(_.value)
        .toRight(BackendError.Unavailable(DiagnosticLocus.Field(s"${role.label} column")))
    def table(e: FixationImportError) =
      BackendError.Unavailable(DiagnosticLocus.Field(s"fixation columns: ${e.message}"))
    for
      participant <- name(ColumnRole.Participant)
      phase       <- name(ColumnRole.Phase)
      trial       <- name(ColumnRole.Trial)
      ordinal     <- name(ColumnRole.Ordinal)
      x           <- name(ColumnRole.X)
      y           <- name(ColumnRole.Y)
      onset       <- name(ColumnRole.Onset)
      duration    <- name(ColumnRole.Duration)
      samples     <- name(ColumnRole.SampleCount)
      keys        <- TrialColumns
        .of(participant, phase, trial, m.column(ColumnRole.Occurrence).map(_.value))
        .leftMap(table)
      result <- FixationTable
        .of(
          keys,
          ordinal,
          x,
          y,
          TimeColumns(onset, duration, unit),
          SampleCountRule.PositiveColumn(samples),
          m.column(ColumnRole.Item).map(_.value),
          spec.attributes.core
        )
        .leftMap(table)
    yield result

  /** The inventory's identity, item and attribute columns: the response is
    * a text attribute, beside the declared attributes.
    */
  private def inventoryColumns(
      mapping: InventoryMapping
  ): Either[BackendError, TrialInventoryColumns] =
    def name(role: ColumnRole): Either[BackendError, String] =
      mapping
        .column(role)
        .map(_.value)
        .toRight(
          BackendError.Unavailable(DiagnosticLocus.Field(s"inventory ${role.label} column"))
        )
    def refused(e: FixationImportError) =
      BackendError.Unavailable(DiagnosticLocus.Field(s"inventory columns: ${e.message}"))
    for
      participant <- name(ColumnRole.Participant)
      phase       <- name(ColumnRole.Phase)
      trial       <- name(ColumnRole.Trial)
      item        <- name(ColumnRole.Item)
      keys        <- TrialColumns
        .of(participant, phase, trial, mapping.column(ColumnRole.Occurrence).map(_.value))
        .leftMap(refused)
      columns <- TrialInventoryColumns
        .of(
          keys,
          Some(item),
          mapping
            .column(ColumnRole.Response)
            .map(c => AttributeColumn(c.value, AttributeKind.Text))
            .toVector ++
            mapping.coreAttributes
        )
        .leftMap(refused)
    yield columns

  /** The revision's off-screen policy. Coordinate corrections are not yet
    * served by the real backend (a later S3.7 slice): [[admit]] refuses a
    * revision that records one before it reaches eyes4s.
    */
  private def admissionPolicy(spec: DatasetRevisionSpec): eyes4s.plan.AdmissionPolicy[CoreKey] =
    eyes4s.plan.AdmissionPolicy(spec.admission.offScreen.core, Vector.empty)

  /** The analysis window: the stimulus image's placement on the screen. */
  private def imageWindow(
      spec: DatasetRevisionSpec,
      screen: Frame[Unit2D.Px]
  ): Either[BackendError, Subframe[Unit2D.Px]] =
    val image = spec.geometry.image
    Bounds
      .of[Unit2D.Px](
        image.left.toDouble,
        image.top.toDouble,
        image.left.toDouble + image.width,
        image.top.toDouble + image.height
      )
      .flatMap(Subframe.of(screen, FrameId("window"), _))
      .leftMap(e => geometry("image window", e))

  /** eyes4s's refusal of a source. An inventory refusal is typed; every other
    * keeps eyes4s's message beside the source it concerns.
    */
  private def refusal(
      d: DatasetRevision,
      source: String,
      error: FixationImportError
  ): BackendError =
    error match
      case FixationImportError.Inventory(errors) =>
        BackendError.InventoryRefused(d, errors.toVector.map(InventoryIssue.of))
      case other =>
        BackendError.Unavailable(DiagnosticLocus.Artifact(s"$source: ${other.message}"))

  // ------------------------------------------------------------------ protocol values

  private def text(value: Option[AttributeValue]): Option[String] = value.collect {
    case AttributeValue.Text(v) if v.trim.nonEmpty => v
  }

  /** A trial's identity: participant, phase, trial label and occurrence. */
  private type Identity = (String, String, String, Int)

  private def identityOf(k: CoreKey): Identity =
    (k.participant, k.phase, k.trial, k.occurrence.value)

  /** The admitted records outside the admission frame, by trial (occurrence
    * included, so repeated occurrences keep their own records).
    */
  private def outsideFrameByTrial(
      imported: InventoryImport[Unit2D.Px]
  ): Map[Identity, Vector[OutsideFrame]] =
    val trialOf = imported.fixations.admitted.map(r => r.rowNumber -> r.key).toMap
    imported.fixations.outsideFrame
      .flatMap { (o: CoreOutsideFrame) =>
        trialOf
          .get(o.record)
          .map(k => identityOf(k) -> OutsideFrame(o.record, o.x, o.y, o.frame.name))
      }
      .groupMap(_._1)(_._2)

  /** A trial's ledger entry. A trial with no item, in the inventory or its
    * records, is refused rather than given an empty item.
    */
  private def entry(
      trial: InventoryTrial,
      response: Option[String],
      outside: Map[Identity, Vector[OutsideFrame]]
  ): Either[BackendError, LedgerEntry] =
    val id  = trial.identity
    val key = TrialKey(id.participant, Phase(id.phase), id.trial, id.occurrence.value)
    trial.inventoryItem
      .orElse(trial.recordItems.headOption)
      .toRight(BackendError.Unavailable(DiagnosticLocus.Trial(key)))
      .map(item =>
        LedgerEntry(
          key,
          item,
          response.flatMap(r => text(trial.attributes.get(r))).map(Response(_)),
          TrialDisposition.of(trial.disposition),
          outside
            .getOrElse((id.participant, id.phase, id.trial, id.occurrence.value), Vector.empty)
        )
      )

  private def summary(
      spec: DatasetRevisionSpec,
      imported: InventoryImport[Unit2D.Px],
      ledger: AdmissionLedger[CoreKey],
      window: Subframe[Unit2D.Px],
      assets: AssetRegistry
  ): AdmissionSummary =
    val trials       = imported.trials
    val dispositions = trials.map(_.disposition)
    val quarantined  = dispositions
      .collect { case CoreDisposition.Quarantined(c) => QuarantineCause.of(c).code }
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .toVector
      .sortBy(_._1)
      .map(QuarantineCount(_, _))
    val tallies =
      imported.fixations.accepted.rows.map(t => t.key -> WindowTally.window(window, t.value))
    // Every total is eyes4s's: counts, durations and the source records.
    val totals  = WindowSummary.of(tallies, ledger)
    val images  = assets.summary
    val missing = assets.missing.map { m =>
      val item = m.trials.headOption.flatMap(assets.matchItem).fold(m.file.value)(_.value)
      MissingImage(item, m.trials.map(_.participant).distinct.sorted, m.trials.size)
    }
    AdmissionSummary(
      spec.id,
      if spec.decision.isAdmitted then DatasetState.Admitted else DatasetState.Draft,
      InventoryJoin.Joined(trials.size, dispositions.count(_ == CoreDisposition.Absent)),
      dispositions.count(_ == CoreDisposition.Admitted),
      quarantined,
      dispositions.count(_ == CoreDisposition.NoFixations),
      imported.fixations.sourceRows.size,
      WindowTotals(
        outsideWindow = totals.outsideWindow,
        outsideScreen = totals.outsideScreen,
        total = totals.total,
        trialsOutsideWindow = totals.trialsOutsideWindow,
        trialsOutsideScreen = totals.trialsOutsideScreen,
        trials = totals.trials,
        untallied = totals.untallied,
        sourceRecords = totals.sourceRecords,
        outsideWindowMicros = totals.outsideWindowDuration.toMicros,
        outsideScreenMicros = totals.outsideScreenDuration.toMicros,
        totalMicros = totals.totalDuration.toMicros
      ),
      // One stimulus file per item: the registry's files are the items.
      images.files,
      images.present,
      missing,
      // history is deprecated free text studio no longer reads (S5.8); the
      // real backend writes none.
      ""
    )
