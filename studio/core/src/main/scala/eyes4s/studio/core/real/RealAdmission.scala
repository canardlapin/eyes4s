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

  /** Admit `spec`'s sources: `fixations` and `trials` are their exact text. */
  def admit(
      spec: DatasetRevisionSpec,
      fixations: String,
      trials: String,
      assets: AssetRegistry
  ): Either[BackendError, AdmittedDataset] =
    val d                                                       = spec.id
    def unavailable[A](why: Option[A]): Either[BackendError, A] =
      why.toRight(BackendError.Unavailable(DiagnosticLocus.Dataset(d)))
    for
      _         <- unavailable(Option.when(spec.admission.corrections.isEmpty)(()))
      inventory <- unavailable(spec.inventory)
      unit      <- unavailable(spec.units.time.map(timeUnit))
      table     <- unavailable(fixationTable(spec, unit))
      columns   <- unavailable(inventoryColumns(inventory))
      read      <- TrialInventory.read(trials, columns).leftMap(refusal(d, _))
      screen    <- unavailable(
        Frame.screen("screen", spec.geometry.screen.width, spec.geometry.screen.height).toOption
      )
      window   <- unavailable(imageWindow(spec, screen))
      imported <- FixationCsv
        .admitInventory(fixations, table, read, screen, admissionPolicy(spec))
        .leftMap(refusal(d, _))
    yield
      val response = inventory.column(ColumnRole.Response).map(_.value)
      val trialsOf = imported.trials
      val outside  = outsideFrameByTrial(imported)
      val ledger   = trialsOf.map(entry(_, response, outside))
      // The admitted trials; an admission that requires a complete input
      // refused before reaching here (eyes4s ReviewExclusions semantics).
      AdmittedDataset(
        summary(spec, imported, window, assets),
        ledger,
        screen,
        eyes4s.plan.StudyInput(imported.fixations.accepted)
      )

  // ------------------------------------------------------------------ eyes4s inputs

  private def timeUnit(unit: TimeUnit): TimestampUnit = unit match
    case TimeUnit.Milliseconds => TimestampUnit.Milliseconds
    case TimeUnit.Microseconds => TimestampUnit.Microseconds
    case TimeUnit.Seconds      => TimestampUnit.Seconds

  /** The fixation table's declared columns, from the revision's mapping. */
  private def fixationTable(spec: DatasetRevisionSpec, unit: TimestampUnit) =
    val m                      = spec.mapping
    def name(role: ColumnRole) = m.column(role).map(_.value)
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
      keys  <- TrialColumns.of(participant, phase, trial, name(ColumnRole.Occurrence)).toOption
      table <- FixationTable
        .of(
          keys,
          ordinal,
          x,
          y,
          TimeColumns(onset, duration, unit),
          SampleCountRule.PositiveColumn(samples),
          name(ColumnRole.Item),
          spec.attributes.core
        )
        .toOption
    yield table

  /** The inventory's identity, item and attribute columns: the response is
    * a text attribute, beside the declared attributes.
    */
  private def inventoryColumns(mapping: InventoryMapping) =
    def name(role: ColumnRole) = mapping.column(role).map(_.value)
    for
      participant <- name(ColumnRole.Participant)
      phase       <- name(ColumnRole.Phase)
      trial       <- name(ColumnRole.Trial)
      keys <- TrialColumns.of(participant, phase, trial, name(ColumnRole.Occurrence)).toOption
      columns <- TrialInventoryColumns
        .of(
          keys,
          name(ColumnRole.Item),
          name(ColumnRole.Response).map(AttributeColumn(_, AttributeKind.Text)).toVector ++
            mapping.coreAttributes
        )
        .toOption
    yield columns

  /** The revision's off-screen policy. Coordinate corrections are not yet
    * served by the real backend (a later S3.7 slice): [[admit]] refuses a
    * revision that records one before it reaches eyes4s.
    */
  private def admissionPolicy(spec: DatasetRevisionSpec): eyes4s.plan.AdmissionPolicy[CoreKey] =
    eyes4s.plan.AdmissionPolicy(spec.admission.offScreen.core, Vector.empty)

  /** The analysis window: the stimulus image's placement on the screen. */
  private def imageWindow(spec: DatasetRevisionSpec, screen: Frame[Unit2D.Px]) =
    val image = spec.geometry.image
    (for
      region <- Bounds.of[Unit2D.Px](
        image.left.toDouble,
        image.top.toDouble,
        image.left.toDouble + image.width,
        image.top.toDouble + image.height
      )
      window <- Subframe.of(screen, FrameId("window"), region)
    yield window).toOption

  private def refusal(d: DatasetRevision, error: FixationImportError): BackendError =
    error match
      case FixationImportError.Inventory(errors) =>
        BackendError.InventoryRefused(d, errors.toVector.map(InventoryIssue.of))
      case _ => BackendError.Unavailable(DiagnosticLocus.Dataset(d))

  // ------------------------------------------------------------------ protocol values

  private def key(k: CoreKey): TrialKey =
    TrialKey(k.participant, Phase(k.phase), k.trial, k.occurrence.value)

  private def text(value: Option[AttributeValue]): Option[String] = value.collect {
    case AttributeValue.Text(v) if v.trim.nonEmpty => v
  }

  /** The admitted records outside the admission frame, by trial. */
  private def outsideFrameByTrial(
      imported: InventoryImport[Unit2D.Px]
  ): Map[(String, String, String), Vector[OutsideFrame]] =
    val trialOf = imported.fixations.admitted.map(r => r.rowNumber -> r.key).toMap
    imported.fixations.outsideFrame
      .flatMap { (o: CoreOutsideFrame) =>
        trialOf
          .get(o.record)
          .map(k =>
            (k.participant, k.phase, k.trial) -> OutsideFrame(o.record, o.x, o.y, o.frame.name)
          )
      }
      .groupMap(_._1)(_._2)

  private def entry(
      trial: InventoryTrial,
      response: Option[String],
      outside: Map[(String, String, String), Vector[OutsideFrame]]
  ): LedgerEntry =
    val id = trial.identity
    LedgerEntry(
      TrialKey(id.participant, Phase(id.phase), id.trial, id.occurrence.value),
      trial.inventoryItem.orElse(trial.recordItems.headOption).getOrElse(""),
      response.flatMap(r => text(trial.attributes.get(r))).map(Response(_)),
      TrialDisposition.of(trial.disposition),
      outside.getOrElse((id.participant, id.phase, id.trial), Vector.empty)
    )

  private def summary(
      spec: DatasetRevisionSpec,
      imported: InventoryImport[Unit2D.Px],
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
    val tallies = imported.fixations.accepted.rows.map(t =>
      key(t.key) -> WindowTally.window(window, t.value)
    )
    val totals  = WindowSummary.of(tallies)
    val counted = tallies.collect { case (_, Right(t)) => t }
    val items   = trials.flatMap(t => t.inventoryItem).distinct
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
        sourceRecords = Some(imported.fixations.sourceRows.size),
        outsideWindowMicros = counted.map(_.outsideWindowDuration.toMicros).sum,
        outsideScreenMicros = counted.map(_.outsideScreenDuration.toMicros).sum,
        totalMicros = counted.map(_.totalDuration.toMicros).sum
      ),
      items.size,
      assets.summary.present,
      missing,
      ""
    )
