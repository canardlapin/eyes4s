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

package eyes4s.studio.core.backend

import io.circe.Codec

/** Where eyes4s places one record of a dataset revision's fixation source
  * (protocol `PlacementOf`, S5.5): outside the screen (the admission frame),
  * on the screen but outside the half-open image frame, or inside it.
  */
enum RecordPlacement derives CanEqual, Codec.AsObject:
  case OutsideScreen, OutsideWindow, Inside

/** One record as eyes4s places it: as recorded (`raw`), with the correction
  * rule covering its trial applied (`rule`, by index; `corrected`), in the
  * image frame's own pixels (`image`, also for a position outside it) and in
  * degrees from the image centre, x right and y up, when the revision
  * declares a scale. Screen positions are screen pixels.
  */
final case class PlacedRecord(
    record: Int,
    trial: TrialKey,
    rawX: Double,
    rawY: Double,
    rule: Option[Int],
    correctedX: Double,
    correctedY: Double,
    imageX: Double,
    imageY: Double,
    placement: RecordPlacement,
    degrees: Option[(Double, Double)]
) derives CanEqual,
      Codec.AsObject

/** A record whose position could not be read, with why: returned, not dropped. */
final case class UnplacedSourceRecord(record: Int, reason: String)
    derives CanEqual,
      Codec.AsObject

/** One trial's records and how many eyes4s places outside the image frame
  * and outside the screen: eyes4s's `WindowTally` of the trial.
  */
final case class TrialPlacement(
    trial: TrialKey,
    records: Int,
    outsideWindow: Int,
    outsideScreen: Int
) derives CanEqual,
      Codec.AsObject

/** Every placed record binned by eyes4s on a grid over the screen: counts per
  * cell, row-major with row 0 at the top. A position off the screen falls in
  * no cell.
  */
final case class PlacementDensityGrid(
    columns: Int,
    rows: Int,
    counts: Vector[Double],
    placed: Int
) derives CanEqual,
      Codec.AsObject

/** A dataset revision's placement preview (S5.5): every record of its
  * fixation source placed by eyes4s under the revision's geometry and
  * recorded corrections (a draft's included), each trial's window tally in
  * source order, and the all-trials density.
  */
final case class PlacementPreview(
    dataset: DatasetRevision,
    records: Vector[PlacedRecord],
    unplaced: Vector[UnplacedSourceRecord],
    trials: Vector[TrialPlacement],
    density: PlacementDensityGrid
) derives CanEqual,
      Codec.AsObject:
  /** Each trial's records, in source order. */
  lazy val byTrial: Map[TrialKey, Vector[PlacedRecord]] = records.groupBy(_.trial)

  /** The record numbered `n`, if it was placed. */
  def record(n: Int): Option[PlacedRecord] = records.find(_.record == n)
