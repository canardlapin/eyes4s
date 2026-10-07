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
import eyes4s.io.{FixationSourceText, SourceRecordText}
import eyes4s.kernel.Unit2D
import eyes4s.plan.{
  CoordinateProvenance,
  DataRecord,
  PageSize,
  ScanpathPosition,
  TrialKey as CoreKey
}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.{ColumnRole, SourceRole, TimeUnit}
import eyes4s.studio.core.geometry.RecordPositions
import eyes4s.studio.core.selection.{FixationIndex, RecordNumber, StudioRef}

/** A revision's trial views (S3.7 slice 7, protocol 1.6): each admitted
  * fixation as eyes4s's `CoordinateProvenance` gives it, over the revision's
  * plan, admitted input and admission ledger, and (protocol 1.7) each source
  * record as eyes4s-io's `FixationSourceText` pages the ledger's records with
  * their verbatim text. Studio converts units only: microseconds and the
  * file's time unit to milliseconds, scanpath positions from 0 to positions
  * from 1. A record's cells are read as numbers where they are one; its
  * image position and degrees are the kernel's ([[RecordPositions]]).
  */
final class RealTrialViews private (
    private[real] val work: RealPrepared,
    provenance: CoordinateProvenance[CoreKey, Unit2D.Px],
    keys: Map[TrialKey, CoreKey]
):
  private def refused(trial: TrialKey, step: String, reason: String) =
    BackendError.TrialViewRefused(TrialViewError.Study(trial, step, reason))

  /** The trial's scanpath, every fixation placed by the revision's study.
    * A trial of the inventory without an admitted scanpath is `Unavailable`.
    */
  def fixations(trial: TrialKey): Either[BackendError, TrialFixations] =
    for
      key       <- inventory(trial)
      positions <- provenance.positions(key).leftMap(e => refused(trial, "scanpath", e.message))
      fixations <- positions.traverse(fixation(trial, key, _))
      extent    <- temporalExtent(trial, key)
      done      <- TrialFixations
        .of(work.revision, work.dataset, trial, fixations, extent)
        .leftMap(BackendError.TrialViewRefused(_))
    yield done

  private def temporalExtent(
      trial: TrialKey,
      key: CoreKey
  ): Either[BackendError, TrialTemporalExtent] =
    spec.inventory.flatMap(_.duration) match
      case None              => Right(TrialTemporalExtent.Undeclared(trial, work.dataset))
      case Some(declaration) =>
        for
          native <- declaration.core.leftMap(e =>
            refused(trial, "trial duration declaration", e.message)
          )
          source <- spec.sources.trials.toRight(
            refused(trial, "trial duration source", "No inventory source is declared.")
          )
          evidence <- work.admitted.evidence.inventory.toRight(
            refused(
              trial,
              "trial duration evidence",
              "No native inventory evidence is retained."
            )
          )
          row <- evidence
            .trial(eyes4s.plan.TrialIdentity.of(key))
            .toRight(
              refused(
                trial,
                "trial duration evidence",
                "The trial has no retained inventory record."
              )
            )
          window <- native.extent(row).leftMap(e => refused(trial, "trial duration", e.message))
          extent <- window match
            case None =>
              TrialExtentEvidence
                .of(trial, work.dataset, source.path, declaration, row.rows)
                .map(TrialTemporalExtent.Blank.apply)
                .leftMap(BackendError.TrialViewRefused(_))
            case Some(value) =>
              TrialExtentFromStart
                .of(trial, work.dataset, value, source.path, declaration, row.rows)
                .map(TrialTemporalExtent.FromTrialStart.apply)
                .leftMap(BackendError.TrialViewRefused(_))
        yield extent

  /** The fixed 2-degree preview uses the prepared study's own estimator.
    * It preserves the run's initial-fixation, window and weighting policies.
    */
  def preview(trial: TrialKey): Either[BackendError, TrialPreview] =
    val sigmaDegrees = 2.0
    for
      key     <- inventory(trial)
      angular <- work.plan.angularScale.toRight(
        refused(trial, "angular scale", "the recipe declares no pixels per degree")
      )
      sigma <- eyes4s.kernel.Sigma
        .deg(sigmaDegrees)
        .flatMap(angular.sigma)
        .leftMap(e => refused(trial, "bandwidth", e.message))
      density <- work.work
        .estimate(
          key,
          eyes4s.plan.StudyEstimate.Gaussian(sigma, eyes4s.surface.EdgePolicy.Truncate)
        )
        .leftMap(e => refused(trial, "density", e.message))
      levels <- density
        .levels(Vector(0.5, 0.8))
        .leftMap(e => refused(trial, "isolines", e.message))
      geometry = density.geometry
      region <- ScreenRegion
        .of(
          trial,
          geometry.origin.x,
          geometry.origin.y,
          geometry.origin.x + geometry.bounds.width,
          geometry.origin.y + geometry.bounds.height
        )
        .leftMap(BackendError.TrialViewRefused(_))
      order = geometry.yAxis match
        case eyes4s.kernel.YAxis.Down => RowOrder.TopFirst
        case eyes4s.kernel.YAxis.Up   => RowOrder.BottomFirst
      preview <- TrialPreview
        .of(
          work.revision,
          trial,
          sigmaDegrees,
          region,
          density.nx,
          density.ny,
          order,
          density.cells.toVector.map(Some(_)),
          levels.map(_.threshold)
        )
        .leftMap(BackendError.TrialViewRefused(_))
    yield preview

  private def inventory(trial: TrialKey): Either[BackendError, CoreKey] =
    keys.get(trial).toRight {
      if work.admitted.ledger.exists(_.trial == trial) then
        BackendError.Unavailable(DiagnosticLocus.Trial(trial))
      else BackendError.UnknownTrial(work.dataset, trial)
    }

  private def fixation(
      trial: TrialKey,
      key: CoreKey,
      position: ScanpathPosition
  ): Either[BackendError, AdmittedFixation] =
    for
      p <- provenance
        .fixation(key, position)
        .leftMap(e => refused(trial, "fixation", e.message))
      record <- p.record.leftMap(m =>
        refused(trial, "source record", s"fixation ${position.value + 1}: ${m.toString}")
      )
      index <- FixationIndex
        .of(position.value + 1)
        .leftMap(e => refused(trial, "fixation", e.message))
      centre = p.trail.admitted.position
      made <- AdmittedFixation
        .of(
          StudioRef.Fixation(trial, index),
          record.value,
          centre.x,
          centre.y,
          p.span.onset.toMicros / 1000.0,
          p.span.duration.toMicros / 1000.0,
          p.trail.placement
        )
        .leftMap(BackendError.TrialViewRefused(_))
    yield made

  // ------------------------------------------------------------------ source records

  private val spec = work.admitted.spec

  private def records(e: SourceRecordsError) =
    BackendError.SourceRecordsRefused(work.revision, e)
  private def study(step: String)(reason: String) =
    records(SourceRecordsError.Study(step, reason))

  /** The fixation file's text, checked by eyes4s-io against the ledger's source. */
  private lazy val text: Either[BackendError, FixationSourceText] =
    work.admitted.sourceText
      .toRight(
        study("source text")(
          s"${work.revision.label}/${work.dataset.label} has stored parsed records but no verbatim fixation source text."
        )
      )
      .flatMap(checkedText)

  private def checkedText(rawText: String): Either[BackendError, FixationSourceText] =
    FixationSourceText
      .of(rawText, work.admitted.evidence.source)
      .leftMap(e => study("source text")(e.message))

  /** Records `from` to `from + count - 1` of the dataset's fixation file. */
  def sourceRecords(from: Int, count: Int): Either[BackendError, SourceRecordPage] =
    sourceRecordsWith(from, count, text)

  /** An archive lacks verbatim CSV. The host may supply its byte-verified
    * original only for this request; the native ledger checks parsed identity.
    */
  def sourceRecords(
      rawText: String,
      from: Int,
      count: Int
  ): Either[BackendError, SourceRecordPage] =
    sourceRecordsWith(from, count, checkedText(rawText))

  private def sourceRecordsWith(
      from: Int,
      count: Int,
      sourceText: => Either[BackendError, FixationSourceText]
  ): Either[BackendError, SourceRecordPage] =
    val listing                = provenance.records
    def name(role: ColumnRole) = RealAdmission.column(spec, role)
    for
      _ <- Either.cond(
        from >= 1 && count >= 1 && count <= SourceRecordPage.Limit,
        (),
        records(SourceRecordsError.RangeInvalid(from, count, SourceRecordPage.Limit))
      )
      _ <- Either.cond(
        from <= listing.total,
        (),
        records(SourceRecordsError.PastEnd(from, listing.total))
      )
      source <- spec.sources.fixations.toRight(
        BackendError.Unavailable(DiagnosticLocus.Field("fixations source"))
      )
      file  <- sourceText
      x     <- name(ColumnRole.X)
      y     <- name(ColumnRole.Y)
      start <- DataRecord.of(from).leftMap(e => study("record")(e.toString))
      size  <- PageSize.of(count).leftMap(e => study("page")(e.message))
      page  <- file
        .page(listing, x, y, start, size)
        .leftMap(e => study("source records")(e.message))
      (display, scale) <- RecordPositions
        .frames(work.dataset, work.recipe, spec.geometry)
        .leftMap(study("display frames"))
      columns <- (
        name(ColumnRole.Participant),
        name(ColumnRole.Phase),
        name(ColumnRole.Trial),
        name(ColumnRole.Ordinal),
        name(ColumnRole.Onset),
        name(ColumnRole.Duration),
        name(ColumnRole.SampleCount)
      ).tupled
      rows <- page.entries.traverse(row(file, display, x, y, columns, _))
      done <- SourceRecordPage
        .of(
          work.revision,
          work.dataset,
          source,
          display.pixelsPerDegree,
          scale,
          listing.total,
          from,
          count,
          rows
        )
        .leftMap(records)
    yield done

  /** One record's row: its cells as the file states them, its fixation and
    * placement as eyes4s gives them.
    */
  private def row(
      file: FixationSourceText,
      display: eyes4s.studio.core.geometry.DisplayFrames,
      xName: String,
      yName: String,
      columns: (String, String, String, String, String, String, String),
      entry: SourceRecordText[CoreKey, Unit2D.Px]
  ): Either[BackendError, SourceRecordRow] =
    val (participant, phase, trialName, ordinal, onset, duration, samples) = columns
    val n     = entry.view.record.value
    val index = file.header.zipWithIndex.toMap
    for
      _      <- entry.refusal.map(e => study("source text")(e.message)).toLeft(())
      fields <- file.fields(entry.view.record).leftMap(e => study("record")(e.toString))
      cell       = (name: String) => index.get(name).flatMap(fields.lift).map(_.trim)
      double     = (name: String) => cell(name).flatMap(_.toDoubleOption).filter(_.isFinite)
      int        = (name: String) => cell(name).flatMap(_.toIntOption)
      ms         = (name: String) => double(name).map(_ * millis)
      occurrence = RealAdmission.column(spec, ColumnRole.Occurrence).toOption.fold(Some(1))(int)
      trial <- (cell(participant), cell(phase), cell(trialName), occurrence)
        .mapN((p, ph, t, o) => TrialKey(p, Phase(ph), t, o))
        .toRight(study("record")(s"record $n names no trial"))
      record   <- RecordNumber.of(n).leftMap(e => study("record")(e.message))
      fixation <- entry.view.fixation.traverse(f =>
        FixationIndex.of(f.position.value + 1).leftMap(e => study("fixation")(e.message))
      )
      screen <- (double(xName), double(yName)).tupled.traverse((px, py) =>
        PlanePoint.of(n, "screen", px, py).leftMap(records)
      )
      placed <- screen
        .flatTraverse(p => RecordPositions.position(display, n, p.x, p.y))
        .leftMap(records)
      made <- SourceRecordRow
        .of(
          StudioRef.SourceRecord(trial, fixation, SourceRole.Fixations, record),
          int(ordinal),
          ms(onset),
          ms(duration),
          int(samples),
          screen,
          placed.map(_._1),
          placed.map(_._2),
          entry.view.fixation.map(_.trail.placement),
          entry.text
        )
        .leftMap(records)
    yield made

  /** Milliseconds per unit of the file's declared time unit. */
  private val millis: Double = spec.units.time match
    case Some(TimeUnit.Microseconds) => 0.001
    case Some(TimeUnit.Seconds)      => 1000.0
    case _                           => 1.0

object RealTrialViews:
  def of(work: RealPrepared): Either[BackendError, RealTrialViews] =
    CoordinateProvenance
      .of(work.plan, work.admitted.input, Some(work.admitted.evidence))
      .leftMap(e =>
        BackendError.Unavailable(
          DiagnosticLocus.Artifact(s"${work.revision.label}: ${e.message}")
        )
      )
      .map(p =>
        new RealTrialViews(
          work,
          p,
          work.admitted.input.trials.rows.map(r => RealResults.key(r.key) -> r.key).toMap
        )
      )
