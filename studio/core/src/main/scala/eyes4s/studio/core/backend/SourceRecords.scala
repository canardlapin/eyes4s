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
import eyes4s.plan.MapPlacement
import eyes4s.studio.core.document.{Source, SourceRole}
import eyes4s.studio.core.selection.{FixationIndex, RecordNumber, StudioRef}
import io.circe.syntax.*
import io.circe.{Codec, Decoder, DecodingFailure, Encoder, JsonObject}

/** Why a page of source records (protocol 1.7, S6.4) is refused. Every case
  * names what it was applied to.
  */
enum SourceRecordsError derives CanEqual, Codec.AsObject:

  /** A page starts at record `from` (from 1) and holds 1 to `limit` records. */
  case RangeInvalid(from: Int, count: Int, limit: Int)

  /** The page starts after the source's last record. */
  case PastEnd(from: Int, total: Int)

  /** The source is not the dataset's fixation source. */
  case NotFixations(role: SourceRole)

  /** A record's `field` holds `value`, which is not finite. */
  case NotFinite(record: Int, field: String, value: String)

  /** The record at row `at` of the page is `record`, not `expected`. */
  case OutOfOrder(at: Int, record: Int, expected: Int)

  /** A record names its fixation without the placement the study gives it,
    * or a placement without a fixation.
    */
  case PlacementMismatch(record: Int)

  /** The ref names another source than the fixation file. */
  case OtherSource(record: Int, role: SourceRole)

  /** eyes4s refused a step of the view (`step`), saying `reason`. */
  case Study(step: String, reason: String)

  /** A page of `count` records from `from` of `total` holds `rows`: a page
    * holds every record asked for that the file has, so only the last is
    * short and none is empty.
    */
  case RowCount(from: Int, count: Int, total: Int, rows: Int)

  /** The page's pixels per degree is `value`, not a positive finite number. */
  case ScaleNotPositive(value: String)

  /** A record has a screen position, an image position and degrees all or
    * none; these say which it has.
    */
  case PositionsPartial(record: Int, screen: Boolean, image: Boolean, degrees: Boolean)

  def message: String = this match
    case RangeInvalid(f, c, l) =>
      s"A page of source records starts at record $f and holds $c; records count from 1 " +
        s"and a page holds 1 to $l."
    case PastEnd(f, t)      => s"Record $f is past the last of the source's $t records."
    case NotFixations(role) =>
      s"Source records are read from the fixation file, not the ${role.label}."
    case NotFinite(r, f, v)   => s"Record $r's $f is $v, which is not finite."
    case OutOfOrder(a, r, e)  => s"Row ${a + 1} of the page is record $r, not $e."
    case PlacementMismatch(r) =>
      s"Record $r names a fixation without its placement, or a placement without a fixation."
    case OtherSource(r, role) =>
      s"Record $r is a record of the ${role.label}, not the fixations."
    case Study(step, reason)  => s"eyes4s refused the $step of the source records: $reason"
    case RowCount(f, c, t, n) =>
      val due = math.max(0, math.min(c, t - f + 1))
      s"A page of $c records from record $f of $t holds $n; it must hold $due."
    case ScaleNotPositive(v) => s"The page's pixels per degree is $v; it must be positive."
    case PositionsPartial(r, sc, im, dg) =>
      def has(b: Boolean) = if b then "has" else "lacks"
      s"Record $r ${has(sc)} a screen position, ${has(im)} an image position and " +
        s"${has(dg)} degrees; a record has all three or none."

/** Where a page's pixels per degree comes from: the analysis revision's
  * recipe (the plan's declared units), or the dataset's declared geometry
  * when the recipe states none.
  */
enum ScaleSource derives CanEqual, Codec.AsObject:
  case Recipe, Dataset

/** A finite point in a plane, in the unit its holder states. */
final case class PlanePoint private (x: Double, y: Double) derives CanEqual

object PlanePoint:
  def of(
      record: Int,
      field: String,
      x: Double,
      y: Double
  ): Either[SourceRecordsError, PlanePoint] =
    if !x.isFinite then Left(SourceRecordsError.NotFinite(record, s"$field x", x.toString))
    else if !y.isFinite then Left(SourceRecordsError.NotFinite(record, s"$field y", y.toString))
    else Right(PlanePoint(x, y))

  given Encoder.AsObject[PlanePoint] = Encoder.forProduct2("x", "y")(p => (p.x, p.y))
  private[backend] val pair: Decoder[(Double, Double)] =
    Decoder.forProduct2("x", "y")((x: Double, y: Double) => (x, y))

/** A record's centre in the image frame, in image pixels (origin top-left of
  * the image, y down), and whether the image's half-open frame holds it.
  */
final case class ImagePosition(at: PlanePoint, insideImage: Boolean) derives CanEqual

/** One record of the dataset's fixation file under an analysis revision
  * (protocol 1.7, S6.4), as the file states it and as the study reads it.
  *
  *   - `ref`: the record (from 1, header excluded) and its trial; it names
  *     its fixation (its scanpath position) when the record is in an
  *     admitted trial's scanpath.
  *   - `ordinal`, `onsetMs`, `durationMs`, `samples` and `screen`: the
  *     record's cells as numbers, `None` where a cell is not one.
  *   - `image`: the screen centre in the image frame; `degrees`: in degrees
  *     of visual angle from the image's centre, x right and y up, at the
  *     page's linear pixels per degree. Both are eyes4s's; a record has
  *     `screen`, `image` and `degrees` all or none.
  *   - `placement`: where the study places the fixation against the map
  *     (eyes4s `MapPlacement`); `None` for a record no admitted scanpath
  *     holds.
  *   - `line`: the record's verbatim text in the file, without its final
  *     line end. A record may span several physical lines (a quoted field
  *     can hold a line end), so this is the record's text, not one line.
  */
final case class SourceRecordRow private (
    ref: StudioRef.SourceRecord,
    ordinal: Option[Int],
    onsetMs: Option[Double],
    durationMs: Option[Double],
    samples: Option[Int],
    screen: Option[PlanePoint],
    image: Option[ImagePosition],
    degrees: Option[PlanePoint],
    placement: Option[MapPlacement],
    line: String
) derives CanEqual:
  def record: Int                          = ref.record.value
  def trial: TrialKey                      = ref.trial
  def fixation: Option[StudioRef.Fixation] = ref.fixation.map(StudioRef.Fixation(ref.trial, _))

object SourceRecordRow:
  def of(
      ref: StudioRef.SourceRecord,
      ordinal: Option[Int],
      onsetMs: Option[Double],
      durationMs: Option[Double],
      samples: Option[Int],
      screen: Option[PlanePoint],
      image: Option[ImagePosition],
      degrees: Option[PlanePoint],
      placement: Option[MapPlacement],
      line: String
  ): Either[SourceRecordsError, SourceRecordRow] =
    val record                                   = ref.record.value
    def finite(field: String, v: Option[Double]) =
      v.filterNot(_.isFinite).map(x => SourceRecordsError.NotFinite(record, field, x.toString))
    if ref.source != SourceRole.Fixations then
      Left(SourceRecordsError.OtherSource(record, ref.source))
    else if !(screen.isDefined == image.isDefined && image.isDefined == degrees.isDefined) then
      Left(
        SourceRecordsError.PositionsPartial(
          record,
          screen.isDefined,
          image.isDefined,
          degrees.isDefined
        )
      )
    else if ref.fixation.isDefined != placement.isDefined then
      Left(SourceRecordsError.PlacementMismatch(record))
    else
      finite("onset", onsetMs)
        .orElse(finite("duration", durationMs))
        .toLeft(
          SourceRecordRow(
            ref,
            ordinal,
            onsetMs,
            durationMs,
            samples,
            screen,
            image,
            degrees,
            placement,
            line
          )
        )

  given Encoder.AsObject[SourceRecordRow] = Encoder.AsObject.instance(r =>
    JsonObject(
      "trial"      -> r.trial.asJson,
      "record"     -> r.record.asJson,
      "fixation"   -> r.ref.fixation.map(_.value).asJson,
      "ordinal"    -> r.ordinal.asJson,
      "onsetMs"    -> r.onsetMs.asJson,
      "durationMs" -> r.durationMs.asJson,
      "samples"    -> r.samples.asJson,
      "screen"     -> r.screen.asJson,
      "image"      -> r.image
        .map(i =>
          JsonObject(
            "x"           -> i.at.x.asJson,
            "y"           -> i.at.y.asJson,
            "insideImage" -> i.insideImage.asJson
          )
        )
        .asJson,
      "degrees"   -> r.degrees.asJson,
      "placement" -> r.placement.map(TrialViewCodecs.placement).asJson,
      "line"      -> r.line.asJson
    )
  )

  given Decoder[SourceRecordRow] = Decoder.instance { c =>
    def fail(e: String)                   = DecodingFailure(e, c.history)
    def point(field: String, record: Int) =
      c.downField(field)
        .as[Option[(Double, Double)]](using Decoder.decodeOption(using PlanePoint.pair))
        .flatMap(
          _.traverse((x, y) => PlanePoint.of(record, field, x, y).leftMap(e => fail(e.message)))
        )
    for
      trial    <- c.get[TrialKey]("trial")
      record   <- c.get[RecordNumber]("record")
      fixation <- c.get[Option[FixationIndex]]("fixation")
      ordinal  <- c.get[Option[Int]]("ordinal")
      onset    <- c.get[Option[Double]]("onsetMs")
      duration <- c.get[Option[Double]]("durationMs")
      samples  <- c.get[Option[Int]]("samples")
      screen   <- point("screen", record.value)
      image    <- c
        .downField("image")
        .as[Option[(Double, Double, Boolean)]](using
          Decoder.decodeOption(using
            Decoder.forProduct3("x", "y", "insideImage")((x: Double, y: Double, in: Boolean) =>
              (x, y, in)
            )
          )
        )
        .flatMap(
          _.traverse((x, y, in) =>
            PlanePoint
              .of(record.value, "image", x, y)
              .bimap(e => fail(e.message), ImagePosition(_, in))
          )
        )
      degrees   <- point("degrees", record.value)
      placement <- c.get[Option[MapPlacement]]("placement")(using
        Decoder.decodeOption(using TrialViewCodecs.placementDecoder)
      )
      line <- c.get[String]("line")
      row  <- of(
        StudioRef.SourceRecord(trial, fixation, SourceRole.Fixations, record),
        ordinal,
        onset,
        duration,
        samples,
        screen,
        image,
        degrees,
        placement,
        line
      ).leftMap(e => fail(e.message))
    yield row
  }

/** A page of the dataset's fixation file under an analysis revision
  * (protocol 1.7, S6.4): the source (its import name and the SHA-256 of its
  * bytes), the pixels per degree its degrees are at and where that scale
  * comes from, its `total` records, the `count` asked for, and the rows of
  * records `from`, `from + 1`, … in file order: every record asked for that
  * the file has, so a page is short only at the file's end and never empty.
  * Keyed by the revision because a record's placement and its degrees
  * belong to the study. An empty file has no page: every request is
  * `PastEnd(from, 0)`. Built only through [[SourceRecordPage.of]].
  */
final case class SourceRecordPage private (
    revision: AnalysisRevision,
    dataset: DatasetRevision,
    source: Source,
    pixelsPerDegree: Double,
    scaleSource: ScaleSource,
    total: Int,
    from: Int,
    count: Int,
    rows: Vector[SourceRecordRow]
) derives CanEqual

object SourceRecordPage:

  /** The most records one page holds. */
  val Limit: Int = 500

  def of(
      revision: AnalysisRevision,
      dataset: DatasetRevision,
      source: Source,
      pixelsPerDegree: Double,
      scaleSource: ScaleSource,
      total: Int,
      from: Int,
      count: Int,
      rows: Vector[SourceRecordRow]
  ): Either[SourceRecordsError, SourceRecordPage] =
    if source.role != SourceRole.Fixations then
      Left(SourceRecordsError.NotFixations(source.role))
    else if !pixelsPerDegree.isFinite || pixelsPerDegree <= 0.0 then
      Left(SourceRecordsError.ScaleNotPositive(pixelsPerDegree.toString))
    else if from < 1 || count < 1 || count > Limit then
      Left(SourceRecordsError.RangeInvalid(from, count, Limit))
    else if from > total then Left(SourceRecordsError.PastEnd(from, total))
    else if rows.size != math.min(count, total - from + 1) then
      Left(SourceRecordsError.RowCount(from, count, total, rows.size))
    else
      rows.zipWithIndex
        .collectFirst {
          case (r, i) if r.record != from + i =>
            SourceRecordsError.OutOfOrder(i, r.record, from + i)
        }
        .toLeft(
          SourceRecordPage(
            revision,
            dataset,
            source,
            pixelsPerDegree,
            scaleSource,
            total,
            from,
            count,
            rows
          )
        )

  given Encoder.AsObject[SourceRecordPage] =
    Encoder.forProduct9(
      "revision",
      "dataset",
      "source",
      "pixelsPerDegree",
      "scaleSource",
      "total",
      "from",
      "count",
      "rows"
    )(p =>
      (
        p.revision,
        p.dataset,
        p.source,
        p.pixelsPerDegree,
        p.scaleSource,
        p.total,
        p.from,
        p.count,
        p.rows
      )
    )

  given Decoder[SourceRecordPage] = Decoder.instance { c =>
    (
      c.get[AnalysisRevision]("revision"),
      c.get[DatasetRevision]("dataset"),
      c.get[Source]("source"),
      c.get[Double]("pixelsPerDegree"),
      c.get[ScaleSource]("scaleSource"),
      c.get[Int]("total"),
      c.get[Int]("from"),
      c.get[Int]("count"),
      c.get[Vector[SourceRecordRow]]("rows")
    ).flatMapN((r, d, s, ppd, ss, t, f, n, rs) =>
      of(r, d, s, ppd, ss, t, f, n, rs).leftMap(e => DecodingFailure(e.message, c.history))
    )
  }
