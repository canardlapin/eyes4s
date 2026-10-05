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

import cats.syntax.all.*
import io.circe.{Codec, Decoder, DecodingFailure, Encoder}

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

/** Why a placement preview, or one of its parts, was refused. Every case
  * names what it was refused for.
  */
enum PlacementError derives CanEqual:
  /** A tally's counts are negative, or more records lie outside than it has. */
  case TallyCounts(trial: TrialKey, records: Int, outsideWindow: Int, outsideScreen: Int)

  /** The density grid has no cells along an axis. */
  case EmptyGrid(columns: Int, rows: Int)

  /** The density grid lists `counts` values for `columns × rows` cells. */
  case CellCount(columns: Int, rows: Int, counts: Int)

  /** Cell `index` holds `value`, which is not a finite, non-negative count. */
  case CellNotCount(index: Int, value: Double)

  /** The density says it binned `placed` records; the preview places `records`. */
  case PlacedCount(dataset: DatasetRevision, placed: Int, records: Int)

  /** `trial`'s tally disagrees with the records placed for it. */
  case TallyMismatch(
      dataset: DatasetRevision,
      tally: TrialPlacement,
      records: Int,
      outsideWindow: Int,
      outsideScreen: Int
  )

  /** A record of `trial` has no tally, or the trial has two. */
  case TallyMissing(dataset: DatasetRevision, trial: TrialKey)
  case TallyRepeated(dataset: DatasetRevision, trial: TrialKey)

  def message: String = this match
    case TallyCounts(t, n, w, sc) =>
      s"The tally of ${t.label} counts $n records, $w outside the image frame and $sc " +
        "outside the screen."
    case EmptyGrid(c, r)      => s"A placement density of $c × $r cells has none."
    case CellCount(c, r, n)   => s"A placement density lists $n counts for $c × $r cells."
    case CellNotCount(i, v)   => s"Placement density cell $i holds $v, which is not a count."
    case PlacedCount(d, p, n) =>
      s"The placement of ${d.label} bins $p records but places $n."
    case TallyMismatch(d, t, n, w, sc) =>
      s"The placement of ${d.label} tallies ${t.trial.label} as ${t.records} records " +
        s"(${t.outsideWindow} outside the image frame, ${t.outsideScreen} outside the screen); " +
        s"its records are $n ($w, $sc)."
    case TallyMissing(d, t) =>
      s"The placement of ${d.label} places records of ${t.label} without a tally."
    case TallyRepeated(d, t) => s"The placement of ${d.label} tallies ${t.label} twice."

/** One trial's records and how many eyes4s places outside the image frame
  * and outside the screen: eyes4s's `WindowTally` of the trial.
  */
final case class TrialPlacement private (
    trial: TrialKey,
    records: Int,
    outsideWindow: Int,
    outsideScreen: Int
) derives CanEqual

object TrialPlacement:
  /** A tally of non-negative counts, no more records outside than it has. */
  def of(
      trial: TrialKey,
      records: Int,
      outsideWindow: Int,
      outsideScreen: Int
  ): Either[PlacementError, TrialPlacement] =
    Either.cond(
      records >= 0 && outsideWindow >= 0 && outsideScreen >= 0 &&
        outsideWindow.toLong + outsideScreen <= records,
      TrialPlacement(trial, records, outsideWindow, outsideScreen),
      PlacementError.TallyCounts(trial, records, outsideWindow, outsideScreen)
    )

  given Encoder.AsObject[TrialPlacement] =
    Encoder.forProduct4("trial", "records", "outsideWindow", "outsideScreen")(t =>
      (t.trial, t.records, t.outsideWindow, t.outsideScreen)
    )

  given Decoder[TrialPlacement] = Decoder.instance(c =>
    (
      c.get[TrialKey]("trial"),
      c.get[Int]("records"),
      c.get[Int]("outsideWindow"),
      c.get[Int]("outsideScreen")
    ).flatMapN((t, n, w, s) =>
      of(t, n, w, s).leftMap(e => DecodingFailure(e.message, c.history))
    )
  )

/** Every placed record binned by eyes4s on a grid over the screen: counts per
  * cell, row-major with row 0 at the top. A position off the screen falls in
  * no cell.
  */
final case class PlacementDensityGrid private (
    columns: Int,
    rows: Int,
    counts: Vector[Double],
    placed: Int
) derives CanEqual

object PlacementDensityGrid:
  /** `columns × rows` finite, non-negative counts of `placed` records. */
  def of(
      columns: Int,
      rows: Int,
      counts: Vector[Double],
      placed: Int
  ): Either[PlacementError, PlacementDensityGrid] =
    if columns <= 0 || rows <= 0 then Left(PlacementError.EmptyGrid(columns, rows))
    else if counts.size.toLong != columns.toLong * rows then
      Left(PlacementError.CellCount(columns, rows, counts.size))
    else
      counts.iterator.zipWithIndex
        .collectFirst {
          case (v, i) if !v.isFinite || v < 0.0 => PlacementError.CellNotCount(i, v)
        }
        .toLeft(PlacementDensityGrid(columns, rows, counts, placed))

  given Encoder.AsObject[PlacementDensityGrid] =
    Encoder.forProduct4("columns", "rows", "counts", "placed")(d =>
      (d.columns, d.rows, d.counts, d.placed)
    )

  given Decoder[PlacementDensityGrid] = Decoder.instance(c =>
    (
      c.get[Int]("columns"),
      c.get[Int]("rows"),
      c.get[Vector[Double]]("counts"),
      c.get[Int]("placed")
    ).flatMapN((w, h, n, p) =>
      of(w, h, n, p).leftMap(e => DecodingFailure(e.message, c.history))
    )
  )

/** A dataset revision's placement preview (S5.5): every record of its
  * fixation source placed by eyes4s under the revision's geometry and
  * recorded corrections (a draft's included), each trial's window tally in
  * source order, and the all-trials density.
  */
final case class PlacementPreview private (
    dataset: DatasetRevision,
    records: Vector[PlacedRecord],
    unplaced: Vector[UnplacedSourceRecord],
    trials: Vector[TrialPlacement],
    density: PlacementDensityGrid
) derives CanEqual:
  /** Each trial's records, in source order. */
  lazy val byTrial: Map[TrialKey, Vector[PlacedRecord]] = records.groupBy(_.trial)

  /** The record numbered `n`, if it was placed. */
  def record(n: Int): Option[PlacedRecord] = records.find(_.record == n)

object PlacementPreview:
  /** A preview whose tallies are its records': one tally per trial, each
    * counting that trial's records and where they lie, and a density that
    * binned every record placed.
    */
  def of(
      dataset: DatasetRevision,
      records: Vector[PlacedRecord],
      unplaced: Vector[UnplacedSourceRecord],
      trials: Vector[TrialPlacement],
      density: PlacementDensityGrid
  ): Either[PlacementError, PlacementPreview] =
    val byTrial                     = records.groupBy(_.trial)
    val tallied                     = trials.map(_.trial)
    def mismatch(t: TrialPlacement) =
      val rs = byTrial.getOrElse(t.trial, Vector.empty)
      val w  = rs.count(_.placement == RecordPlacement.OutsideWindow)
      val sc = rs.count(_.placement == RecordPlacement.OutsideScreen)
      Option.when((t.records, t.outsideWindow, t.outsideScreen) != (rs.size, w, sc))(
        PlacementError.TallyMismatch(dataset, t, rs.size, w, sc)
      )
    tallied
      .diff(tallied.distinct)
      .headOption
      .map(PlacementError.TallyRepeated(dataset, _))
      .orElse(
        records.iterator
          .map(_.trial)
          .find(t => !tallied.contains(t))
          .map(PlacementError.TallyMissing(dataset, _))
      )
      .orElse(trials.iterator.flatMap(mismatch).nextOption())
      .orElse(
        Option.when(density.placed != records.size)(
          PlacementError.PlacedCount(dataset, density.placed, records.size)
        )
      )
      .toLeft(PlacementPreview(dataset, records, unplaced, trials, density))

  given Encoder.AsObject[PlacementPreview] =
    Encoder.forProduct5("dataset", "records", "unplaced", "trials", "density")(p =>
      (p.dataset, p.records, p.unplaced, p.trials, p.density)
    )

  given Decoder[PlacementPreview] = Decoder.instance(c =>
    (
      c.get[DatasetRevision]("dataset"),
      c.get[Vector[PlacedRecord]]("records"),
      c.get[Vector[UnplacedSourceRecord]]("unplaced"),
      c.get[Vector[TrialPlacement]]("trials"),
      c.get[PlacementDensityGrid]("density")
    ).flatMapN((d, r, u, t, g) =>
      of(d, r, u, t, g).leftMap(e => DecodingFailure(e.message, c.history))
    )
  )
