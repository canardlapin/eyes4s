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
import eyes4s.kernel.Span
import eyes4s.plan.{MapPlacement, OffWindowPolicy, WindowTally}
import eyes4s.studio.core.selection.{FixationIndex, StudioRef}
import io.circe.syntax.*
import io.circe.{Codec, Decoder, DecodingFailure, Encoder, HCursor, Json, JsonObject}

/** Why a trial view (protocol 1.6, S6.2) is refused. Every case names the
  * trial and what it was applied to.
  */
enum TrialViewError derives CanEqual, Codec.AsObject:
  case RecordNotPositive(trial: TrialKey, position: Int, record: Int)
  case PositionNotFinite(trial: TrialKey, record: Int, x: Double, y: Double)
  case OnsetNegative(trial: TrialKey, record: Int, onsetMs: Double)
  case DurationNotPositive(trial: TrialKey, record: Int, durationMs: Double)

  /** The fixations are not the trial's scanpath positions 1, 2, … in order. */
  case PositionOutOfOrder(trial: TrialKey, at: Int, position: Int)
  case OtherTrial(trial: TrialKey, fixation: TrialKey)
  case ExtentInvalid(trial: TrialKey, fromMicros: Long, untilMicros: Long)
  case ExtentRecords(trial: TrialKey, source: String, records: Vector[Int])
  case ExtentDataset(trial: TrialKey, dataset: DatasetRevision, extentDataset: DatasetRevision)
  case SigmaNotPositive(trial: TrialKey, degrees: Double)
  case EmptyGrid(trial: TrialKey, columns: Int, rows: Int)
  case CellCount(trial: TrialKey, columns: Int, rows: Int, cells: Int)
  case CellNotDensity(trial: TrialKey, index: Int, value: Double)
  case LevelNotFinite(trial: TrialKey, index: Int, level: Double)

  /** The preview's covered region has no area or is not finite. */
  case RegionEmpty(trial: TrialKey, left: Double, top: Double, right: Double, bottom: Double)

  /** The study fails the trial (its off-window policy is `FailTrial` and
    * the fixations at `positions` lie outside the window): it has no map.
    */
  case TrialFails(trial: TrialKey, positions: Vector[Int])

  /** eyes4s refused a step of the view (`step`), saying `reason`. */
  case Study(trial: TrialKey, step: String, reason: String)

  def message: String = this match
    case RecordNotPositive(t, p, r) =>
      s"Fixation $p of ${t.label} names record $r; records count from 1."
    case PositionNotFinite(t, r, x, y) =>
      s"Fixation record $r of ${t.label} is at ($x, $y), which is not finite."
    case OnsetNegative(t, r, o)       => s"Fixation record $r of ${t.label} starts at $o ms."
    case DurationNotPositive(t, r, d) =>
      s"Fixation record $r of ${t.label} lasts $d ms; a fixation lasts more than 0 ms."
    case PositionOutOfOrder(t, at, p) =>
      s"Fixation ${at + 1} of ${t.label} is at scanpath position $p."
    case ExtentInvalid(t, from, until) =>
      s"Trial extent of ${t.label} is [$from, $until) μs from trial start; require zero start and a positive end."
    case ExtentDataset(t, d, e) =>
      s"Trial extent of ${t.label} names ${e.label}, but the fixations belong to ${d.label}."
    case ExtentRecords(t, source, records) =>
      s"Trial extent of ${t.label} names inventory $source records $records; require nonempty, increasing data records starting at 2."
    case OtherTrial(t, f)       => s"The fixations of ${t.label} include one of ${f.label}."
    case SigmaNotPositive(t, d) => s"The preview of ${t.label} has σ $d°; it must be positive."
    case EmptyGrid(t, c, r)     => s"The preview of ${t.label} has $c × $r cells."
    case CellCount(t, c, r, n)  =>
      s"The preview of ${t.label} has $n values for $c × $r cells."
    case CellNotDensity(t, i, v) =>
      s"Cell $i of the preview of ${t.label} holds $v, not a finite non-negative density."
    case LevelNotFinite(t, i, l) =>
      s"Isoline level $i of the preview of ${t.label} is $l, which is not finite."
    case RegionEmpty(t, l, tp, r, b) =>
      s"The preview of ${t.label} covers [$l, $r) × [$tp, $b) px, which has no area."
    case TrialFails(t, ps) =>
      s"The study fails ${t.label}: fixation${if ps.size == 1 then "" else "s"} " +
        s"${ps.mkString(", ")} ${
            if ps.size == 1 then "lies" else "lie"
          } outside the analysis " +
        "window, so it has no map."
    case Study(t, step, reason) => s"eyes4s refused the $step of ${t.label}: $reason"

  /** The trial the refusal is about. */
  def trial: TrialKey

/** Which stored row of a grid is at the top of the map: the convention is
  * part of the grid, never assumed by its readers.
  */
enum RowOrder derives CanEqual, Codec.AsObject:

  /** Row 0 is the top edge, rows run downward (screen y down). */
  case TopFirst

  /** Row 0 is the bottom edge, rows run upward, as an eyes4s kernel grid on
    * a frame whose y axis points up.
    */
  case BottomFirst

/** One fixation of a trial's scanpath as eyes4s admitted it (S6.2): its ref
  * (the trial and its scanpath position, from 1), its source record, its
  * centre in screen pixels (origin top-left, y down), its onset and duration
  * in milliseconds, and where the analysis revision's study places it
  * against the map (eyes4s `MapPlacement`).
  */
final case class AdmittedFixation private (
    ref: StudioRef.Fixation,
    record: Int,
    screenX: Double,
    screenY: Double,
    onsetMs: Double,
    durationMs: Double,
    placement: MapPlacement
) derives CanEqual

object AdmittedFixation:
  def of(
      ref: StudioRef.Fixation,
      record: Int,
      screenX: Double,
      screenY: Double,
      onsetMs: Double,
      durationMs: Double,
      placement: MapPlacement
  ): Either[TrialViewError, AdmittedFixation] =
    val trial = ref.trial
    if record < 1 then Left(TrialViewError.RecordNotPositive(trial, ref.index.value, record))
    else if !screenX.isFinite || !screenY.isFinite then
      Left(TrialViewError.PositionNotFinite(trial, record, screenX, screenY))
    else if !onsetMs.isFinite || onsetMs < 0.0 then
      Left(TrialViewError.OnsetNegative(trial, record, onsetMs))
    else if !durationMs.isFinite || durationMs <= 0.0 then
      Left(TrialViewError.DurationNotPositive(trial, record, durationMs))
    else Right(AdmittedFixation(ref, record, screenX, screenY, onsetMs, durationMs, placement))

  given Encoder.AsObject[AdmittedFixation] = Encoder.AsObject.instance(f =>
    JsonObject(
      "trial"      -> f.ref.trial.asJson,
      "position"   -> f.ref.index.value.asJson,
      "record"     -> f.record.asJson,
      "x"          -> f.screenX.asJson,
      "y"          -> f.screenY.asJson,
      "onsetMs"    -> f.onsetMs.asJson,
      "durationMs" -> f.durationMs.asJson,
      "placement"  -> TrialViewCodecs.placement(f.placement)
    )
  )

  given Decoder[AdmittedFixation] = Decoder.instance { c =>
    for
      trial    <- c.get[TrialKey]("trial")
      position <- c.get[Int]("position")
      index    <- FixationIndex.of(position).leftMap(e => DecodingFailure(e.message, c.history))
      record   <- c.get[Int]("record")
      x        <- c.get[Double]("x")
      y        <- c.get[Double]("y")
      onset    <- c.get[Double]("onsetMs")
      duration <- c.get[Double]("durationMs")
      placement <- c.downField("placement").as(using TrialViewCodecs.placementDecoder)
      fixation <- of(StudioRef.Fixation(trial, index), record, x, y, onset, duration, placement)
        .leftMap(e => DecodingFailure(e.message, c.history))
    yield fixation
  }

/** A trial's admitted fixations under an analysis revision (protocol 1.6,
  * S6.2): its scanpath in order, positions 1, 2, …, each placed by the
  * revision's study. Built only through [[TrialFixations.of]].
  */
final case class TrialFixations private (
    revision: AnalysisRevision,
    dataset: DatasetRevision,
    trial: TrialKey,
    fixations: Vector[AdmittedFixation],
    extent: TrialTemporalExtent
) derives CanEqual

object TrialFixations:
  def of(
      revision: AnalysisRevision,
      dataset: DatasetRevision,
      trial: TrialKey,
      fixations: Vector[AdmittedFixation]
  ): Either[TrialViewError, TrialFixations] =
    of(revision, dataset, trial, fixations, TrialTemporalExtent.LegacyMissing(trial))

  def of(
      revision: AnalysisRevision,
      dataset: DatasetRevision,
      trial: TrialKey,
      fixations: Vector[AdmittedFixation],
      extent: TrialTemporalExtent
  ): Either[TrialViewError, TrialFixations] =
    if extent.trial != trial then Left(TrialViewError.OtherTrial(trial, extent.trial))
    else if extent.declaredDataset.exists(_ != dataset) then
      Left(
        TrialViewError.ExtentDataset(trial, dataset, extent.declaredDataset.getOrElse(dataset))
      )
    else
      fixations.zipWithIndex
        .collectFirst {
          case (f, _) if f.ref.trial != trial => TrialViewError.OtherTrial(trial, f.ref.trial)
          case (f, i) if f.ref.index.value != i + 1 =>
            TrialViewError.PositionOutOfOrder(trial, i, f.ref.index.value)
        }
        .toLeft(TrialFixations(revision, dataset, trial, fixations, extent))

  given Encoder.AsObject[TrialFixations] =
    Encoder.forProduct5("revision", "dataset", "trial", "fixations", "extent")(t =>
      (t.revision, t.dataset, t.trial, t.fixations, t.extent)
    )

  given Decoder[TrialFixations] = Decoder.instance { c =>
    (
      c.get[AnalysisRevision]("revision"),
      c.get[DatasetRevision]("dataset"),
      c.get[TrialKey]("trial"),
      c.get[Vector[AdmittedFixation]]("fixations"),
      c.get[Option[TrialTemporalExtent]]("extent")
    ).flatMapN((r, d, t, fs, extent) =>
      of(r, d, t, fs, extent.getOrElse(TrialTemporalExtent.LegacyMissing(t))).leftMap(e =>
        DecodingFailure(e.message, c.history)
      )
    )
  }

/** The half-open region `[left, right) × [top, bottom)` of the screen, in
  * screen pixels (origin top-left, y down), that a grid covers.
  */
final case class ScreenRegion private (left: Double, top: Double, right: Double, bottom: Double)
    derives CanEqual:
  def width: Double  = right - left
  def height: Double = bottom - top

object ScreenRegion:
  def of(
      trial: TrialKey,
      left: Double,
      top: Double,
      right: Double,
      bottom: Double
  ): Either[TrialViewError, ScreenRegion] =
    Either.cond(
      List(left, top, right, bottom).forall(_.isFinite) && right > left && bottom > top,
      ScreenRegion(left, top, right, bottom),
      TrialViewError.RegionEmpty(trial, left, top, right, bottom)
    )

  given Encoder.AsObject[ScreenRegion] =
    Encoder.forProduct4("left", "top", "right", "bottom")(r =>
      (r.left, r.top, r.right, r.bottom)
    )

/** A trial's preview map under an analysis revision (protocol 1.6, S6.2):
  * eyes4s's density of the trial's in-map fixations at `sigmaDegrees` over
  * the revision's grid, not a result of any run. The grid covers `region` of
  * the screen (the analysis window, or the screen when the study has none),
  * which a renderer must draw it over: it is not the image frame unless the
  * window is. `cells` are in row-major order with `order` stating which row
  * is the top; a cell is `None` where the backend holds no value. `levels`
  * are the backend's isoline levels.
  */
final case class TrialPreview private (
    revision: AnalysisRevision,
    trial: TrialKey,
    sigmaDegrees: Double,
    region: ScreenRegion,
    columns: Int,
    rows: Int,
    order: RowOrder,
    cells: Vector[Option[Double]],
    levels: Vector[Double]
) derives CanEqual

object TrialPreview:
  def of(
      revision: AnalysisRevision,
      trial: TrialKey,
      sigmaDegrees: Double,
      region: ScreenRegion,
      columns: Int,
      rows: Int,
      order: RowOrder,
      cells: Vector[Option[Double]],
      levels: Vector[Double]
  ): Either[TrialViewError, TrialPreview] =
    if !sigmaDegrees.isFinite || sigmaDegrees <= 0.0 then
      Left(TrialViewError.SigmaNotPositive(trial, sigmaDegrees))
    else if columns <= 0 || rows <= 0 then Left(TrialViewError.EmptyGrid(trial, columns, rows))
    else if cells.size.toLong != columns.toLong * rows then
      Left(TrialViewError.CellCount(trial, columns, rows, cells.size))
    else
      cells.iterator.zipWithIndex
        .collectFirst {
          case (Some(v), i) if !v.isFinite || v < 0.0 =>
            TrialViewError.CellNotDensity(trial, i, v)
        }
        .orElse(levels.iterator.zipWithIndex.collectFirst {
          case (l, i) if !l.isFinite => TrialViewError.LevelNotFinite(trial, i, l)
        })
        .toLeft(
          TrialPreview(
            revision,
            trial,
            sigmaDegrees,
            region,
            columns,
            rows,
            order,
            cells,
            levels
          )
        )

  given Encoder.AsObject[TrialPreview] = Encoder.forProduct9(
    "revision",
    "trial",
    "sigmaDegrees",
    "region",
    "columns",
    "rows",
    "order",
    "cells",
    "levels"
  )(p =>
    (
      p.revision,
      p.trial,
      p.sigmaDegrees,
      p.region,
      p.columns,
      p.rows,
      p.order,
      p.cells,
      p.levels
    )
  )

  given Decoder[TrialPreview] = Decoder.instance { c =>
    (
      c.get[AnalysisRevision]("revision"),
      c.get[TrialKey]("trial"),
      c.get[Double]("sigmaDegrees"),
      c.downField("region")
        .as[(Double, Double, Double, Double)](using
          Decoder.forProduct4("left", "top", "right", "bottom")(
            (l: Double, t: Double, r: Double, b: Double) => (l, t, r, b)
          )
        ),
      c.get[Int]("columns"),
      c.get[Int]("rows"),
      c.get[RowOrder]("order"),
      c.get[Vector[Option[Double]]]("cells"),
      c.get[Vector[Double]]("levels")
    ).flatMapN { case (r, t, s, (l, tp, rt, b), w, h, o, cs, ls) =>
      ScreenRegion
        .of(t, l, tp, rt, b)
        .flatMap(of(r, t, s, _, w, h, o, cs, ls))
        .leftMap(e => DecodingFailure(e.message, c.history))
    }
  }

/** eyes4s `MapPlacement` and `OffWindowPolicy` on the wire, as the studio
  * writes its enums: `{"InMap":{}}`, `{"OutsideWindow":{"policy":{"Exclude":{}}}}`.
  * `MapPlacement.InWindow` keeps its earlier wire name `InMap`. Protocol 1.10
  * adds `{"TrialFailed":{"tally":{...}}}`: in the window, in a trial the study
  * fails, with the tally of the trial's kept fixations it fails for.
  */
object TrialViewCodecs:
  import ProtocolCodecs.portableLong
  private def tag(name: String, body: JsonObject = JsonObject.empty): Json =
    Json.obj(name -> Json.fromJsonObject(body))

  def policy(p: OffWindowPolicy): Json = tag(p.toString)

  def tally(t: WindowTally): Json = Json.obj(
    "outsideScreen"       -> Json.fromInt(t.outsideScreen),
    "outsideWindow"       -> Json.fromInt(t.outsideWindow),
    "total"               -> Json.fromInt(t.total),
    "outsideScreenMicros" -> t.outsideScreenDuration.toMicros.asJson,
    "outsideWindowMicros" -> t.outsideWindowDuration.toMicros.asJson,
    "totalMicros"         -> t.totalDuration.toMicros.asJson
  )

  val tallyDecoder: Decoder[WindowTally] = Decoder.instance(c =>
    for
      screen       <- c.get[Int]("outsideScreen")
      window       <- c.get[Int]("outsideWindow")
      total        <- c.get[Int]("total")
      screenMicros <- c.get[Long]("outsideScreenMicros")
      windowMicros <- c.get[Long]("outsideWindowMicros")
      totalMicros  <- c.get[Long]("totalMicros")
      tally        <- WindowTally
        .of(
          screen,
          window,
          total,
          Span.micros(screenMicros),
          Span.micros(windowMicros),
          Span.micros(totalMicros)
        )
        .leftMap(e => DecodingFailure(e.message, c.history))
    yield tally
  )

  def placement(p: MapPlacement): Json = p match
    case MapPlacement.OutsideWindow(policy) =>
      tag("OutsideWindow", JsonObject("policy" -> this.policy(policy)))
    case MapPlacement.InWindow       => tag("InMap")
    case MapPlacement.TrialFailed(t) => tag("TrialFailed", JsonObject("tally" -> tally(t)))
    case MapPlacement.OutsideScreen  => tag("OutsideScreen")
    case MapPlacement.DroppedInitial => tag("DroppedInitial")

  private def single(c: HCursor): Decoder.Result[(String, HCursor)] =
    c.keys.map(_.toVector) match
      case Some(Vector(name)) =>
        c.downField(name)
          .success
          .toRight(DecodingFailure(s"no body for $name", c.history))
          .map(name -> _)
      case other => Left(DecodingFailure(s"expected one case, got $other", c.history))

  val policyDecoder: Decoder[OffWindowPolicy] = Decoder.instance(c =>
    single(c).flatMap((name, _) =>
      OffWindowPolicy.values
        .find(_.toString == name)
        .toRight(DecodingFailure(s"unknown off-window policy $name", c.history))
    )
  )

  val placementDecoder: Decoder[MapPlacement] = Decoder.instance(c =>
    single(c).flatMap {
      case ("InMap", _)          => Right(MapPlacement.InWindow)
      case ("TrialFailed", body) =>
        body.downField("tally").as(using tallyDecoder).map(MapPlacement.TrialFailed(_))
      case ("OutsideScreen", _)    => Right(MapPlacement.OutsideScreen)
      case ("DroppedInitial", _)   => Right(MapPlacement.DroppedInitial)
      case ("OutsideWindow", body) =>
        body.downField("policy").as(using policyDecoder).map(MapPlacement.OutsideWindow(_))
      case (name, _) => Left(DecodingFailure(s"unknown map placement $name", c.history))
    }
  )
