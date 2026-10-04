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

import eyes4s.kernel.*
import eyes4s.plan.*

/** Why a source text cannot be read against a ledger, or a page of it built. */
enum SourceTextError[+K] derives CanEqual:
  /** The text is not a CSV table with a header. */
  case Layout(underlying: CsvLayoutError)

  /** The text's decoded records are not the ones the ledger was made from:
    * their digest differs from the ledger's source reference.
    */
  case SourceDigest(label: String, ledger: String, text: String)

  /** The page's listing reads another source than this text, or none. */
  case SourceMismatch(text: String, listing: Option[String])

  /** A position column is not in the text's header. */
  case Column(column: String, header: Vector[String])

  /** An admitted record's position field is not a finite number. */
  case Field(record: DataRecord, column: String, text: String)

  /** Correcting the recorded position does not give the admitted position,
    * bit for bit.
    */
  case RecordedMismatch(
      record: DataRecord,
      recordedX: Double,
      recordedY: Double,
      correctedX: Double,
      correctedY: Double,
      admittedX: Double,
      admittedY: Double
  )

  /** The listing refused the page. */
  case Provenance(underlying: ProvenanceError[K])

  def message: String = this match
    case Layout(e)                         => e.message
    case SourceDigest(label, ledger, text) =>
      s"The text's records have digest $text; the ledger's source '$label' has $ledger."
    case SourceMismatch(text, listing) =>
      s"The text is source ${text}; the listing reads ${listing.getOrElse("no source")}."
    case Column(column, header)      => s"Column '$column' is not in the header $header."
    case Field(record, column, text) =>
      s"Data record ${record.value} has '$text' in column '$column', not a finite number."
    case RecordedMismatch(record, rx, ry, cx, cy, ax, ay) =>
      s"Data record ${record.value} records ($rx, $ry), which corrects to ($cx, $cy); " +
        s"the input holds ($ax, $ay)."
    case Provenance(e) => e.message

object SourceTextError:
  given diagnose[K]: Diagnose[SourceTextError[K], K] =
    Diagnose.derived[SourceTextError[K], K](IoDiagnosticCatalog.sourceText, subject[K])(
      _.message
    )

  /** The record a refusal names. */
  private def subject[K](error: SourceTextError[K]): Vector[Locus[K]] = error match
    case Field(record, _, _)                        => Vector(Locus.Record(record.csv.value))
    case RecordedMismatch(record, _, _, _, _, _, _) => Vector(Locus.Record(record.csv.value))
    case _                                          => Vector.empty

/** One source record with its verbatim text and lines. An admitted record's
  * fixation carries its recorded position (`trail.recorded`), unless its
  * position fields cannot be read or do not correct to the admitted position:
  * then `refusal` says so, naming the record, and the rest of the page is
  * still given.
  */
final case class SourceRecordText[K, U <: Unit2D](
    view: SourceRecordView[K, U],
    text: String,
    lines: LineSpan,
    refusal: Option[SourceTextError[K]]
) derives CanEqual

/** One page of source records with their text: the entries, the ledger's
  * record count and the record the next page starts at.
  */
final case class SourceTextPage[K, U <: Unit2D](
    entries: Vector[SourceRecordText[K, U]],
    total: Int,
    next: Option[DataRecord]
) derives CanEqual

/** The text of a fixation table, checked to be the source an admission
  * ledger was made from: its decoded header and records have the digest of
  * the ledger's `SourceRef`. Pages of a [[SourceRecordListing]] then carry
  * each record's verbatim text, its lines, and for an admitted record the
  * position fields as recorded and their parse, which must correct to the
  * admitted position bit for bit.
  */
final class FixationSourceText private (
    val layout: CsvLayout,
    val header: Vector[String],
    rows: Vector[Vector[String]],
    val source: SourceRef
):
  /** A data record's decoded fields. */
  def fields(record: DataRecord): Either[RecordIdentityError, Vector[String]] =
    rows
      .lift(record.value - 1)
      .toRight(RecordIdentityError.RecordBeyond(record, rows.size))

  /** A page of `listing` with each record's text; positions are read from the
    * `x` and `y` columns.
    */
  def page[K, U <: Unit2D](
      listing: SourceRecordListing[K, U],
      x: String,
      y: String,
      from: DataRecord,
      size: PageSize
  ): Either[SourceTextError[K], SourceTextPage[K, U]] =
    checked(listing)
      .flatMap(_ => listing.page(from, size).left.map(SourceTextError.Provenance.apply))
      .flatMap(withText(listing, x, y, _))

  /** The first page of `listing` with each record's text. */
  def first[K, U <: Unit2D](
      listing: SourceRecordListing[K, U],
      x: String,
      y: String,
      size: PageSize
  ): Either[SourceTextError[K], SourceTextPage[K, U]] =
    checked(listing)
      .flatMap(_ => listing.first(size).left.map(SourceTextError.Provenance.apply))
      .flatMap(withText(listing, x, y, _))

  private def checked[K](listing: SourceRecordListing[K, ?]): Either[SourceTextError[K], Unit] =
    Either.cond(
      listing.source.contains(source),
      (),
      SourceTextError.SourceMismatch(
        source.records.digest,
        listing.source.map(_.records.digest)
      )
    )

  private def column[K](name: String): Either[SourceTextError[K], Int] =
    val index = header.indexOf(name)
    Either.cond(index >= 0, index, SourceTextError.Column(name, header))

  private def withText[K, U <: Unit2D](
      listing: SourceRecordListing[K, U],
      x: String,
      y: String,
      page: RecordPage[K, U]
  ): Either[SourceTextError[K], SourceTextPage[K, U]] =
    for
      xi      <- column[K](x)
      yi      <- column[K](y)
      entries <- page.entries
        .foldLeft[Either[SourceTextError[K], Vector[SourceRecordText[K, U]]]](
          Right(Vector.empty)
        ) { (done, view) =>
          done.flatMap(texts => entry(listing.admission, x, y, xi, yi, view).map(texts :+ _))
        }
    yield SourceTextPage(entries, page.total, page.next)

  private def entry[K, U <: Unit2D](
      admission: Frame[U],
      x: String,
      y: String,
      xi: Int,
      yi: Int,
      view: SourceRecordView[K, U]
  ): Either[SourceTextError[K], SourceRecordText[K, U]] =
    val record = view.record
    def identity[A](e: Either[RecordIdentityError, A]): Either[SourceTextError[K], A] =
      e.left.map(error => SourceTextError.Provenance(ProvenanceError.Identity(error)))
    for
      text   <- identity(layout.verbatim(record))
      lines  <- identity(layout.lines.span(record))
      values <- identity(fields(record))
    yield view.fixation match
      case None    => SourceRecordText(view, text, lines, None)
      case Some(f) =>
        recorded(admission, record, x, y, values.lift(xi), values.lift(yi), f.trail) match
          case Right(r) =>
            val checked = f.copy(trail = f.trail.withRecorded(r))
            SourceRecordText(view.copy(fixation = Some(checked)), text, lines, None)
          case Left(refusal) => SourceRecordText(view, text, lines, Some(refusal))

  private def recorded[K, U <: Unit2D](
      admission: Frame[U],
      record: DataRecord,
      x: String,
      y: String,
      xText: Option[String],
      yText: Option[String],
      trail: CoordinateTrail[U]
  ): Either[SourceTextError[K], RecordedPosition[U]] =
    // The importer's own parse of a position field.
    def parse(column: String, text: Option[String]) =
      text
        .toRight(SourceTextError.Field(record, column, ""))
        .flatMap(t =>
          FixationCsv
            .finite(Map(column -> t), column)
            .left
            .map(_ => SourceTextError.Field(record, column, t))
        )
    for
      px <- parse(x, xText)
      py <- parse(y, yText)
      parsed    = Pt[U](px, py)
      admitted  = trail.admitted.position
      corrected = trail.correction.fold(Some(parsed))(_.correction.correct(admission, parsed))
      _ <- Either.cond(
        corrected.exists(c => FixationSourceText.sameBits(c, admitted)),
        (),
        SourceTextError.RecordedMismatch(
          record,
          px,
          py,
          corrected.fold(Double.NaN)(_.x),
          corrected.fold(Double.NaN)(_.y),
          admitted.x,
          admitted.y
        )
      )
    yield RecordedPosition(
      x,
      y,
      xText.getOrElse(""),
      yText.getOrElse(""),
      FramedPosition(admission.id, parsed)
    )

object FixationSourceText:
  /** The text of the source `source` names; its decoded header and records
    * must have that source's digest.
    */
  def of(
      text: String,
      source: SourceRef
  ): Either[SourceTextError[Nothing], FixationSourceText] =
    for
      layout <- CsvLayout.scan(text).left.map(SourceTextError.Layout.apply)
      rows   <- Rfc4180
        .decode(text)
        .left
        .map(e => SourceTextError.Layout(CsvLayoutError.Csv(e)))
      header = rows.headOption.getOrElse(Vector.empty)
      found  = ArtifactRef.of[Vector[Vector[String]]](SourceRef.digest(header, rows.drop(1)))
      _ <- Either.cond(
        found == source.records,
        (),
        SourceTextError.SourceDigest(source.label, source.records.digest, found.digest)
      )
    yield new FixationSourceText(layout, header, rows.drop(1), source)

  private[io] def sameBits[U <: Unit2D](a: Pt[U], b: Pt[U]): Boolean =
    java.lang.Double.doubleToLongBits(a.x) == java.lang.Double.doubleToLongBits(b.x) &&
      java.lang.Double.doubleToLongBits(a.y) == java.lang.Double.doubleToLongBits(b.y)
