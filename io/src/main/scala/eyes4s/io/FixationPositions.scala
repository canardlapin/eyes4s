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

package eyes4s.io

import cats.syntax.all.*
import eyes4s.kernel.*
import eyes4s.plan.{AdmissionPolicy, DataRecord, QuarantineCause, TrialIdentity}

/** One source record's finite position under its declared correction policy.
  * This is a position preview, not an assertion that the whole fixation or
  * trial is admitted. `admissionFailure` records the admission frame's
  * quarantine decision; temporal and whole-trial validation remain the
  * fixation importer's responsibility. Raw fields and coordinates remain
  * unchanged, including when the corrected point lies outside the frame.
  */
final case class FixationPosition[U <: Unit2D] private[io] (
    record: DataRecord,
    trial: TrialIdentity,
    fields: Vector[String],
    raw: Pt[U],
    corrected: Pt[U],
    rule: Option[Int],
    outsideFrame: Boolean,
    admissionFailure: Option[FixationRowError]
)

/** A position that could not be interpreted, returned beside the placed
  * records. The key is absent only when its declared fields cannot be read.
  */
final case class UnplacedFixationPosition private[io] (
    record: DataRecord,
    fields: Vector[String],
    trial: Option[TrialIdentity],
    error: FixationRowError
)

/** One interpreted source position in a named window and optional angular
  * frame. The window membership is half-open. `image` is in the window's
  * local coordinates even for positions outside the window; nothing is
  * clamped to make an outside point fit.
  */
final case class PlacedFixationPosition[U <: Unit2D] private[io] (
    source: FixationPosition[U],
    image: Pt[U],
    windowPlacement: HalfOpenPlacement[U],
    degrees: Option[Pt[Unit2D.Deg]]
):
  def outsideFrame: Boolean  = source.outsideFrame
  def outsideWindow: Boolean = !outsideFrame && !windowPlacement.isInside

/** Native per-trial position counts. Unreadable records remain in
  * `unplaced` rather than being counted as successfully positioned.
  */
final case class FixationPlacementTally private[io] (
    trial: TrialIdentity,
    records: Int,
    outsideWindow: Int,
    outsideFrame: Int
) derives CanEqual

/** Every source data record as an interpreted position or an explicit
  * refusal, in source order within each partition.
  */
final class FixationPositions[U <: Unit2D] private[io] (
    val frame: Frame[U],
    val positions: Vector[FixationPosition[U]],
    val unplaced: Vector[UnplacedFixationPosition]
):
  def records: Int = positions.size + unplaced.size

  /** Project through the named window and declared angular scale. Geometry
    * identity is checked through Agreement; per-record transform refusals
    * are returned as unplaced records with the original fields and key.
    */
  def place(
      window: Subframe[U],
      angular: Option[LinearAngularScale[U]],
      degreesFrame: FrameId
  ): Either[GeometryError, PlacedFixationPositions[U]] =
    for
      _    <- Agreement.frames(frame, window.parent)
      warp <- angular.traverse(_.on(window).flatMap(_.angular(degreesFrame)))
    yield
      val interpreted = positions.map { p =>
        val location = window.locate(p.corrected)
        val local    = location match
          case HalfOpenPlacement.Inside(image) => Right(image)
          case HalfOpenPlacement.Outside(_)    =>
            window
              .enter(p.corrected)
              .toRight(
                FixationRowError.Event(
                  s"Record ${p.record.value} position (${p.corrected.x},${p.corrected.y}) " +
                    s"cannot enter window ${window.frame.id} from frame ${frame.id}."
                )
              )
        val projected = for
          image   <- local
          degrees <- warp.traverse(w =>
            w(image).toRight(
              FixationRowError.Event(
                s"Record ${p.record.value} window position (${image.x},${image.y}) " +
                  s"cannot enter degrees frame $degreesFrame."
              )
            )
          )
        yield PlacedFixationPosition(p, image, location, degrees)
        projected.left.map(e => UnplacedFixationPosition(p.record, p.fields, Some(p.trial), e))
      }
      new PlacedFixationPositions(
        frame,
        window,
        interpreted.collect { case Right(p) => p },
        (unplaced ++ interpreted.collect { case Left(p) => p }).sortBy(_.record.value)
      )

/** A position preview projected through one named window. Counts and the
  * screen measure come from the same interpreted positions as admission,
  * with each outside or unplaced record retained explicitly.
  */
final class PlacedFixationPositions[U <: Unit2D] private[io] (
    val frame: Frame[U],
    val window: Subframe[U],
    val positions: Vector[PlacedFixationPosition[U]],
    val unplaced: Vector[UnplacedFixationPosition]
):
  def records: Int = positions.size + unplaced.size

  lazy val tallies: Vector[FixationPlacementTally] =
    val grouped = positions.groupBy(_.source.trial)
    val known   = positions.map(p => p.source.record.value -> Some(p.source.trial)) ++
      unplaced.map(p => p.record.value -> p.trial)
    known.sortBy(_._1).flatMap(_._2).distinct.map { key =>
      val rows = grouped.getOrElse(key, Vector.empty)
      FixationPlacementTally(
        key,
        rows.size,
        rows.count(_.outsideWindow),
        rows.count(_.outsideFrame)
      )
    }

  /** Every corrected point with unit weight, ready for PointMeasure.binned.
    * Off-frame points contribute to no grid cell, and remain in the preview
    * and its explicit tallies. The empty preview has an empty measure.
    */
  def measure: Either[SurfaceError, PointMeasure[U]] =
    PointMeasure.of(
      frame,
      IArray.from(positions.map(_.source.corrected)),
      IArray.fill(positions.size)(1.0)
    )

object FixationPositions:
  /** Read only the declared trial identity and position columns, using the
    * fixation importer's RFC 4180 reader and shared position interpreter.
    * Other columns are preserved as raw fields, not validated by a position
    * preview. Conflicting corrections leave their records unplaced; the
    * full importer quarantines the trial with the same conflict operands.
    */
  def read[U <: Unit2D](
      contents: String,
      trial: TrialColumns,
      xColumn: String,
      yColumn: String,
      frame: Frame[U],
      policy: AdmissionPolicy[TrialIdentity] = AdmissionPolicy.default[TrialIdentity]
  ): Either[FixationImportError, FixationPositions[U]] =
    val required = trial.names ++ Vector(xColumn, yColumn)
    for
      _     <- TrialColumns.distinct(required)
      table <- FixationCsv.table(contents, required)
      (header, rows) = table
      interpreted <- rows.zipWithIndex.traverse { (raw, index) =>
        // The decoded table includes one header record, so index+1 fits
        // the DataRecord range without guessing or renumbering a row.
        DataRecord
          .of(index + 1)
          .leftMap(_ => FixationImportError.Incomplete(Vector(index + 2)))
          .map { record =>
            val fields = header.zip(raw).toMap
            val key    = trial
              .identity(fields)
              .leftMap(errors =>
                FixationRowError.Key(
                  errors.map((c, v, r) => s"Column '$c' has '$v'; expected $r.").mkString(" ")
                )
              )
            val parsed = for
              k <- key
              _ <- Either.cond(
                raw.size == header.size,
                (),
                FixationRowError.Width(header.size, raw.size)
              )
              position <- FixationPositionStep.read(
                fields,
                xColumn,
                yColumn,
                k,
                frame,
                policy,
                _.participant
              )
              rule <- position.rule.leftMap { (first, second) =>
                FixationRowError.Trial(
                  Vector(index + 2),
                  QuarantineCause.CorrectionConflict(first, second)
                )
              }
            yield FixationPosition(
              record,
              k,
              raw,
              position.raw,
              position.corrected,
              rule,
              !position.insideFrame,
              position.checkAdmission(frame, policy.offScreen).left.toOption
            )
            parsed.leftMap(error => UnplacedFixationPosition(record, raw, key.toOption, error))
          }
      }
    yield new FixationPositions(
      frame,
      interpreted.collect { case Right(p) => p },
      interpreted.collect { case Left(p) => p }
    )
