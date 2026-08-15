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

import eyes4s.core.*
import eyes4s.kernel.*

import scala.collection.mutable.ArrayBuffer

/** A delimiter whose field boundary is unambiguous in an RFC-4180-style row. */
enum Delimiter(val character: Char) derives CanEqual:
  case Comma     extends Delimiter(',')
  case Tab       extends Delimiter('\t')
  case Semicolon extends Delimiter(';')
  case Pipe      extends Delimiter('|')

/** Where field names come from. */
enum HeaderMode derives CanEqual:
  case FirstLine
  case Supplied(columns: Vector[String])

/** Numeric unit declared by the source timestamp column. */
enum TimestampUnit(val microsPerUnit: Double, val label: String) derives CanEqual:
  case Microseconds extends TimestampUnit(1.0, "microseconds")
  case Milliseconds extends TimestampUnit(1000.0, "milliseconds")
  case Seconds      extends TimestampUnit(1000000.0, "seconds")

/** The named precision policy for decimal source timestamps. */
enum TimestampRounding derives CanEqual:
  case NearestMicrosecond

/** Static spatial unit carried by the declared x/y columns. */
enum CoordinateUnit[U <: Unit2D] derives CanEqual:
  case Pixels     extends CoordinateUnit[Unit2D.Px]
  case Degrees    extends CoordinateUnit[Unit2D.Deg]
  case Normalized extends CoordinateUnit[Unit2D.Norm]

  def label: String = this match
    case Pixels     => "pixels"
    case Degrees    => "degrees"
    case Normalized => "normalized"

/** The native validity meanings a delimited source may encode. */
enum NativeValidity derives CanEqual:
  case Tracked
  case Blink
  case Lost
  case OffScreen

/** Total, non-overlapping mapping from native validity tokens to meanings. */
final class ValidityCodebook private (
    val tracked: Set[String],
    val blink: Set[String],
    val lost: Set[String],
    val offScreen: Set[String]
) derives CanEqual:
  def tokens: Set[String] = tracked ++ blink ++ lost ++ offScreen

  def decode(raw: String): Option[NativeValidity] =
    val token = raw.trim
    if tracked.contains(token) then Some(NativeValidity.Tracked)
    else if blink.contains(token) then Some(NativeValidity.Blink)
    else if lost.contains(token) then Some(NativeValidity.Lost)
    else if offScreen.contains(token) then Some(NativeValidity.OffScreen)
    else None

object ValidityCodebook:
  def of(
      tracked: Set[String],
      blink: Set[String] = Set.empty,
      lost: Set[String] = Set.empty,
      offScreen: Set[String] = Set.empty
  ): Either[DelimitedSchemaError, ValidityCodebook] =
    val labelled = Vector(
      NativeValidity.Tracked   -> tracked,
      NativeValidity.Blink     -> blink,
      NativeValidity.Lost      -> lost,
      NativeValidity.OffScreen -> offScreen
    )
    val blank = labelled.collectFirst {
      case (meaning, tokens) if tokens.exists(_.trim.isEmpty) => meaning
    }
    val duplicates = labelled
      .flatMap { case (meaning, tokens) => tokens.toVector.map(_.trim -> meaning) }
      .groupBy(_._1)
      .toVector
      .sortBy(_._1)
      .collectFirst {
        case (token, meanings) if meanings.map(_._2).distinct.length > 1 =>
          token -> meanings.map(_._2).distinct
      }
    if tracked.isEmpty then Left(DelimitedSchemaError.NoTrackedValidityToken)
    else if blank.nonEmpty then Left(DelimitedSchemaError.BlankValidityToken(blank.get))
    else
      duplicates match
        case Some((token, meanings)) =>
          Left(DelimitedSchemaError.AmbiguousValidityToken(token, meanings))
        case None =>
          Right(
            new ValidityCodebook(
              tracked.map(_.trim),
              blink.map(_.trim),
              lost.map(_.trim),
              offScreen.map(_.trim)
            )
          )

final case class TimeColumn(
    name: String,
    unit: TimestampUnit,
    rounding: TimestampRounding = TimestampRounding.NearestMicrosecond
) derives CanEqual

final case class PositionColumns[U <: Unit2D](
    x: String,
    y: String,
    unit: CoordinateUnit[U]
) derives CanEqual

final case class ValidityColumn(name: String, codebook: ValidityCodebook) derives CanEqual

final case class PupilColumn(name: String, unit: PupilUnit) derives CanEqual

/** Declarative interpretation of one family of CSV or TSV files.
  *
  * Construction proves that logical columns are named, distinct, and do not
  * confuse a missing token with a validity token.
  */
final class DelimitedSchema[U <: Unit2D] private (
    val delimiter: Delimiter,
    val header: HeaderMode,
    val time: TimeColumn,
    val position: PositionColumns[U],
    val validity: ValidityColumn,
    val pupil: Option[PupilColumn],
    val markers: Vector[String],
    val missingTokens: Set[String],
    val columnMissingTokens: Map[String, Set[String]]
) derives CanEqual:
  def requiredColumns: Vector[String] =
    Vector(time.name, position.x, position.y, validity.name) ++ pupil.map(_.name) ++ markers

  def isMissing(column: String, raw: String): Boolean =
    val token = raw.trim
    missingTokens.contains(token) || columnMissingTokens.get(column).exists(_.contains(token))

object DelimitedSchema:
  def of[U <: Unit2D](
      delimiter: Delimiter,
      header: HeaderMode,
      time: TimeColumn,
      position: PositionColumns[U],
      validity: ValidityColumn,
      pupil: Option[PupilColumn],
      markers: Vector[String] = Vector.empty,
      missingTokens: Set[String] = Set("", "NA", "NaN"),
      columnMissingTokens: Map[String, Set[String]] = Map.empty
  ): Either[DelimitedSchemaError, DelimitedSchema[U]] =
    val names =
      Vector(time.name, position.x, position.y, validity.name) ++ pupil.map(_.name) ++ markers
    val blankIndex = names.indexWhere(_.trim.isEmpty)
    val duplicate  = names.zipWithIndex.collectFirst {
      case (name, second) if names.take(second).exists(_.trim == name.trim) =>
        val first = names.indexWhere(_.trim == name.trim)
        (name, first, second)
    }
    val suppliedHeaderError = header match
      case HeaderMode.FirstLine         => None
      case HeaderMode.Supplied(columns) =>
        if columns.isEmpty then Some(DelimitedSchemaError.EmptySuppliedHeader)
        else
          columns.zipWithIndex.collectFirst {
            case (name, index) if name.trim.isEmpty =>
              DelimitedSchemaError.BlankSuppliedHeader(index)
            case (name, second) if columns.take(second).exists(_.trim == name.trim) =>
              val first = columns.indexWhere(_.trim == name.trim)
              DelimitedSchemaError.DuplicateSuppliedHeader(name, first, second)
          }
    val normalizedMissing       = missingTokens.map(_.trim)
    val normalizedColumnMissing = columnMissingTokens.map { case (column, tokens) =>
      column.trim -> tokens.map(_.trim)
    }
    val unknownMissingColumn = normalizedColumnMissing.keys.toVector.sorted
      .find(column => !names.map(_.trim).contains(column))
    val validityMissing =
      normalizedMissing ++ normalizedColumnMissing.getOrElse(validity.name.trim, Set.empty)
    val overlap = validityMissing.intersect(validity.codebook.tokens).toVector.sorted.headOption

    if blankIndex >= 0 then Left(DelimitedSchemaError.BlankColumnName(blankIndex))
    else
      duplicate match
        case Some((name, first, second)) =>
          Left(DelimitedSchemaError.DuplicateLogicalColumn(name, first, second))
        case None =>
          suppliedHeaderError match
            case Some(error) => Left(error)
            case None        =>
              unknownMissingColumn match
                case Some(column) =>
                  Left(DelimitedSchemaError.UnknownMissingTokenColumn(column))
                case None =>
                  overlap match
                    case Some(token) => Left(DelimitedSchemaError.MissingValidityOverlap(token))
                    case None        =>
                      Right(
                        new DelimitedSchema(
                          delimiter,
                          header match
                            case HeaderMode.FirstLine         => HeaderMode.FirstLine
                            case HeaderMode.Supplied(columns) =>
                              HeaderMode.Supplied(columns.map(_.trim)),
                          time.copy(name = time.name.trim),
                          position.copy(x = position.x.trim, y = position.y.trim),
                          validity.copy(name = validity.name.trim),
                          pupil.map(column => column.copy(name = column.name.trim)),
                          markers.map(_.trim),
                          normalizedMissing,
                          normalizedColumnMissing
                        )
                      )

/** A source line retained whether it is accepted or rejected. */
final case class RawDelimitedRow(
    sourceLine: Int,
    text: String,
    fields: Vector[String],
    diagnostics: Vector[DelimitedDiagnostic]
) derives CanEqual

/** Lossless syntax-level import, before scientific validation. */
final class RawRecording[U <: Unit2D] private[io] (
    val source: String,
    val schema: DelimitedSchema[U],
    val header: Vector[String],
    val rows: Vector[RawDelimitedRow],
    val headerDiagnostics: Vector[DelimitedDiagnostic],
    val sourceDigest: Sha256,
    val nativeMetadata: Vector[(String, String)]
):
  def diagnostics: Vector[DelimitedDiagnostic] =
    headerDiagnostics ++ rows.flatMap(_.diagnostics)

  /** Validate every row, retaining an explicit rejected-row partition. */
  def validate(
      frame: Frame[U],
      clock: ClockId,
      rate: Rate,
      eye: Eye
  ): DelimitedImport[U] =
    Delimited.validate(this, frame, clock, rate, eye)

/** A source row and the sample it successfully produced. */
final case class ImportedRow[U <: Unit2D](
    sourceLine: Int,
    sample: Sample[U],
    markers: Vector[(String, Option[String])],
    nativeFields: Vector[String]
) derives CanEqual

/** A source row retained with every reason it could not become a sample. */
final case class RejectedDelimitedRow(
    row: RawDelimitedRow,
    diagnostics: Vector[DelimitedDiagnostic]
) derives CanEqual

/** Native timestamp interpretation attached to the validated clock identity. */
final case class DelimitedClockSpec(
    clock: ClockId,
    nativeUnit: TimestampUnit,
    rounding: TimestampRounding
) derives CanEqual

/** Validated recording plus complete ingest accounting and source identity. */
final class DelimitedImport[U <: Unit2D] private[io] (
    val raw: RawRecording[U],
    val frame: Frame[U],
    val clock: ClockId,
    val rate: Rate,
    val eye: Eye,
    val recording: Option[Recording[U]],
    val acceptedRows: Vector[ImportedRow[U]],
    val rejectedRows: Vector[RejectedDelimitedRow],
    val diagnostics: Vector[DelimitedDiagnostic]
):
  def sourceDigest: Sha256          = raw.sourceDigest
  def frameSpec: FrameSpec          = frame.spec
  def clockSpec: DelimitedClockSpec =
    DelimitedClockSpec(clock, raw.schema.time.unit, raw.schema.time.rounding)
  def acceptedCount: Int = acceptedRows.length
  def rejectedCount: Int = rejectedRows.length

  /** Every physical data row is accounted for exactly once. */
  def isLosslessPartition: Boolean =
    val accounted = acceptedRows.map(_.sourceLine) ++ rejectedRows.map(_.row.sourceLine)
    acceptedCount + rejectedCount == raw.rows.length &&
    accounted.distinct.length == raw.rows.length &&
    accounted.sorted == raw.rows.map(_.sourceLine).sorted

/** Delimited parsing and validation without effects or thrown parse failures. */
object Delimited:

  def parse[U <: Unit2D](
      source: String,
      contents: String,
      schema: DelimitedSchema[U],
      nativeMetadata: Vector[(String, String)] = Vector.empty
  ): RawRecording[U] =
    val physicalLines = splitPhysicalLines(contents)
    val (header, dataLines, firstDataLine, headerParseDiagnostics) = schema.header match
      case HeaderMode.FirstLine =>
        physicalLines.headOption match
          case None =>
            (
              Vector.empty[String],
              Vector.empty[String],
              2,
              Vector(DelimitedDiagnostic.MissingHeader(source, 1))
            )
          case Some(line) =>
            val parsed = parseLine(source, 1, removeBom(line), schema.delimiter)
            (parsed.fields.map(_.trim), physicalLines.drop(1), 2, parsed.diagnostics)
      case HeaderMode.Supplied(columns) =>
        (columns, physicalLines, 1, Vector.empty)

    val duplicateHeaderDiagnostics = header.zipWithIndex.collect {
      case (name, second) if header.take(second).contains(name) =>
        DelimitedDiagnostic.DuplicateHeader(
          source,
          name,
          header.indexOf(name),
          second
        )
    }
    val missingColumnDiagnostics = schema.requiredColumns.collect {
      case column if !header.contains(column) =>
        DelimitedDiagnostic.MissingRequiredColumn(source, column, header)
    }
    val expected = header.length
    val rows     = dataLines.zipWithIndex.map { case (line, index) =>
      val sourceLine  = firstDataLine + index
      val parsed      = parseLine(source, sourceLine, line, schema.delimiter)
      val cardinality =
        if parsed.fields.length == expected then Vector.empty
        else
          Vector(
            DelimitedDiagnostic.WrongFieldCount(
              source,
              sourceLine,
              expected,
              parsed.fields.length
            )
          )
      RawDelimitedRow(sourceLine, line, parsed.fields, parsed.diagnostics ++ cardinality)
    }

    new RawRecording(
      source,
      schema,
      header,
      rows,
      headerParseDiagnostics ++ duplicateHeaderDiagnostics ++ missingColumnDiagnostics,
      Sha256.ofUtf8(contents),
      nativeMetadata
    )

  private[io] def validate[U <: Unit2D](
      raw: RawRecording[U],
      frame: Frame[U],
      clock: ClockId,
      rate: Rate,
      eye: Eye
  ): DelimitedImport[U] =
    val accepted                         = ArrayBuffer.empty[ImportedRow[U]]
    val rejected                         = ArrayBuffer.empty[RejectedDelimitedRow]
    var previous: Option[(Int, Instant)] = None

    if raw.headerDiagnostics.nonEmpty then
      raw.rows.foreach(row => rejected += RejectedDelimitedRow(row, raw.headerDiagnostics))
    else
      raw.rows.foreach { row =>
        if row.diagnostics.nonEmpty then rejected += RejectedDelimitedRow(row, row.diagnostics)
        else
          parseSample(raw, row, frame) match
            case Left(errors)    => rejected += RejectedDelimitedRow(row, errors)
            case Right(imported) =>
              previous match
                case Some((previousLine, previousTime))
                    if imported.sample.t.toMicros <= previousTime.toMicros =>
                  rejected += RejectedDelimitedRow(
                    row,
                    Vector(
                      DelimitedDiagnostic.NonIncreasingTimestamp(
                        raw.source,
                        row.sourceLine,
                        previousLine,
                        previousTime,
                        imported.sample.t
                      )
                    )
                  )
                case _ =>
                  accepted += imported
                  previous = Some(row.sourceLine -> imported.sample.t)
      }

    val acceptedVector                       = accepted.toVector
    val rejectedVector                       = rejected.toVector
    val (recording, constructionDiagnostics) =
      if acceptedVector.isEmpty then
        (
          None,
          Vector(DelimitedDiagnostic.NoAcceptedRows(raw.source, raw.rows.length))
        )
      else
        Recording.of(
          frame,
          clock,
          rate,
          eye,
          raw.schema.pupil.map(_.unit),
          IArray.from(acceptedVector.map(_.sample))
        ) match
          case Right(value) => Some(value) -> Vector.empty
          case Left(error)  =>
            None -> Vector(DelimitedDiagnostic.RecordingConstructionFailed(raw.source, error))

    val allDiagnostics =
      (raw.headerDiagnostics ++ rejectedVector.flatMap(
        _.diagnostics
      ) ++ constructionDiagnostics).distinct
    new DelimitedImport(
      raw,
      frame,
      clock,
      rate,
      eye,
      recording,
      acceptedVector,
      rejectedVector,
      allDiagnostics
    )

  private final case class ParsedLine(
      fields: Vector[String],
      diagnostics: Vector[DelimitedDiagnostic]
  )

  private def parseLine(
      source: String,
      sourceLine: Int,
      line: String,
      delimiter: Delimiter
  ): ParsedLine =
    val fields      = ArrayBuffer.empty[String]
    val field       = new java.lang.StringBuilder
    val diagnostics = ArrayBuffer.empty[DelimitedDiagnostic]
    var quoted      = false
    var closedQuote = false
    var index       = 0

    def finishField(): Unit =
      fields += field.toString
      field.setLength(0)
      closedQuote = false

    while index < line.length do
      val character = line.charAt(index)
      if quoted then
        if character == '"' then
          if index + 1 < line.length && line.charAt(index + 1) == '"' then
            field.append('"')
            index += 1
          else
            quoted = false
            closedQuote = true
        else field.append(character)
      else if closedQuote then
        if character == delimiter.character then finishField()
        else
          diagnostics += DelimitedDiagnostic.MalformedQuotedField(
            source,
            sourceLine,
            index + 1,
            s"unexpected character '$character' after a closing quote"
          )
          field.append(character)
          closedQuote = false
      else if character == delimiter.character then finishField()
      else if character == '"' then
        if field.length == 0 then quoted = true
        else
          diagnostics += DelimitedDiagnostic.MalformedQuotedField(
            source,
            sourceLine,
            index + 1,
            "quote inside an unquoted field"
          )
          field.append(character)
      else field.append(character)
      index += 1

    if quoted then
      diagnostics += DelimitedDiagnostic.MalformedQuotedField(
        source,
        sourceLine,
        line.length + 1,
        "unterminated quoted field"
      )
    finishField()
    ParsedLine(fields.toVector, diagnostics.toVector)

  private def parseSample[U <: Unit2D](
      raw: RawRecording[U],
      row: RawDelimitedRow,
      frame: Frame[U]
  ): Either[Vector[DelimitedDiagnostic], ImportedRow[U]] =
    val errors = ArrayBuffer.empty[DelimitedDiagnostic]

    def value(column: String): String = row.fields(raw.header.indexOf(column))

    def number(column: String, unit: String): Option[Double] =
      val native = value(column)
      if raw.schema.isMissing(column, native) then
        errors += DelimitedDiagnostic.MissingValue(raw.source, row.sourceLine, column)
        None
      else
        native.trim.toDoubleOption match
          case Some(parsed) if parsed.isFinite => Some(parsed)
          case _                               =>
            errors += DelimitedDiagnostic.InvalidNumber(
              raw.source,
              row.sourceLine,
              column,
              native,
              unit
            )
            None

    val timeValue = number(raw.schema.time.name, raw.schema.time.unit.label).flatMap { parsed =>
      val micros = parsed * raw.schema.time.unit.microsPerUnit
      if !micros.isFinite || micros < Long.MinValue.toDouble || micros > Long.MaxValue.toDouble
      then
        errors += DelimitedDiagnostic.TimestampOutsideRange(
          raw.source,
          row.sourceLine,
          raw.schema.time.name,
          value(raw.schema.time.name),
          raw.schema.time.unit
        )
        None
      else Some(Instant.micros(math.round(micros)))
    }

    val validityNative = value(raw.schema.validity.name)
    val validity       =
      if raw.schema.isMissing(raw.schema.validity.name, validityNative) then
        Some(NativeValidity.Lost)
      else
        raw.schema.validity.codebook.decode(validityNative) match
          case some @ Some(_) => some
          case None           =>
            errors += DelimitedDiagnostic.UnknownValidity(
              raw.source,
              row.sourceLine,
              raw.schema.validity.name,
              validityNative
            )
            None

    val position = validity match
      case Some(NativeValidity.Tracked | NativeValidity.OffScreen) =>
        val x = number(raw.schema.position.x, raw.schema.position.unit.label)
        val y = number(raw.schema.position.y, raw.schema.position.unit.label)
        (x, y) match
          case (Some(xValue), Some(yValue)) => Some(Pt[U](xValue, yValue))
          case _                            => None
      case _ => None

    val pupil = raw.schema.pupil.flatMap { column =>
      val native = value(column.name)
      if raw.schema.isMissing(column.name, native) then None
      else
        native.trim.toDoubleOption match
          case Some(parsed) if parsed.isFinite && parsed > 0.0 => Some(parsed)
          case _                                               =>
            errors += DelimitedDiagnostic.InvalidPupil(
              raw.source,
              row.sourceLine,
              column.name,
              native,
              column.unit
            )
            None
    }

    val gaze = validity.flatMap {
      case NativeValidity.Tracked =>
        position.map { point =>
          if frame.contains(point) then Gaze.Tracked(point, pupil) else Gaze.OffScreen(point)
        }
      case NativeValidity.OffScreen => position.map(Gaze.OffScreen(_))
      case NativeValidity.Blink     => Some(Gaze.Blink[U]())
      case NativeValidity.Lost      => Some(Gaze.Lost[U]())
    }

    if errors.nonEmpty then Left(errors.toVector)
    else
      (timeValue, gaze) match
        case (Some(time), Some(state)) =>
          val markerValues = raw.schema.markers.map { column =>
            val native = value(column)
            column -> Option.when(!raw.schema.isMissing(column, native))(native)
          }
          Right(
            ImportedRow(
              row.sourceLine,
              Sample(time, state, SampleOrigin.Measured),
              markerValues,
              row.fields
            )
          )
        case _ =>
          Left(
            Vector(
              DelimitedDiagnostic.IncompleteSample(
                raw.source,
                row.sourceLine,
                raw.schema.requiredColumns
              )
            )
          )

  private def splitPhysicalLines(contents: String): Vector[String] =
    if contents.isEmpty then Vector.empty
    else
      val split = contents.split("\\n", -1).toVector.map(_.stripSuffix("\r"))
      if contents.endsWith("\n") then split.dropRight(1) else split

  private def removeBom(line: String): String = line.stripPrefix("\ufeff")

end Delimited

enum DelimitedSchemaError derives CanEqual:
  case NoTrackedValidityToken
  case BlankValidityToken(meaning: NativeValidity)
  case AmbiguousValidityToken(token: String, meanings: Vector[NativeValidity])
  case BlankColumnName(index: Int)
  case DuplicateLogicalColumn(name: String, firstIndex: Int, secondIndex: Int)
  case EmptySuppliedHeader
  case BlankSuppliedHeader(index: Int)
  case DuplicateSuppliedHeader(name: String, firstIndex: Int, secondIndex: Int)
  case UnknownMissingTokenColumn(column: String)
  case MissingValidityOverlap(token: String)

  def message: String = this match
    case NoTrackedValidityToken =>
      "A validity codebook requires at least one native token meaning Tracked."
    case BlankValidityToken(meaning) =>
      s"The validity codebook contains a blank token for meaning=$meaning."
    case AmbiguousValidityToken(token, meanings) =>
      s"Native validity token='$token' names multiple meanings=${meanings.mkString("[", ",", "]")}."
    case BlankColumnName(index) =>
      s"Logical delimited column[$index] has a blank name."
    case DuplicateLogicalColumn(name, first, second) =>
      s"Logical delimited column='$name' is duplicated at indices $first and $second."
    case EmptySuppliedHeader =>
      "A supplied delimited header requires at least one field name."
    case BlankSuppliedHeader(index) =>
      s"Supplied delimited header[$index] has a blank name."
    case DuplicateSuppliedHeader(name, first, second) =>
      s"Supplied delimited header='$name' is duplicated at indices $first and $second."
    case UnknownMissingTokenColumn(column) =>
      s"Column-specific missing tokens name unknown logical column='$column'."
    case MissingValidityOverlap(token) =>
      s"Native token='$token' cannot mean both missing and a gaze-validity state."

end DelimitedSchemaError

enum DelimitedDiagnostic derives CanEqual:
  case MissingHeader(source: String, line: Int)
  case MalformedQuotedField(source: String, line: Int, column: Int, detail: String)
  case WrongFieldCount(source: String, line: Int, expected: Int, actual: Int)
  case DuplicateHeader(
      source: String,
      name: String,
      firstIndex: Int,
      secondIndex: Int
  )
  case MissingRequiredColumn(source: String, column: String, header: Vector[String])
  case MissingValue(source: String, line: Int, column: String)
  case InvalidNumber(source: String, line: Int, column: String, value: String, unit: String)
  case TimestampOutsideRange(
      source: String,
      line: Int,
      column: String,
      value: String,
      unit: TimestampUnit
  )
  case UnknownValidity(source: String, line: Int, column: String, value: String)
  case InvalidPupil(
      source: String,
      line: Int,
      column: String,
      value: String,
      unit: PupilUnit
  )
  case NonIncreasingTimestamp(
      source: String,
      line: Int,
      previousLine: Int,
      previous: Instant,
      current: Instant
  )
  case IncompleteSample(source: String, line: Int, columns: Vector[String])
  case NoAcceptedRows(source: String, suppliedRows: Int)
  case RecordingConstructionFailed(source: String, underlying: RecordingError)

  def message: String = this match
    case MissingHeader(source, line) =>
      s"Delimited source='$source' has no header at line=$line."
    case MalformedQuotedField(source, line, column, detail) =>
      s"Delimited source='$source' line=$line character=$column has malformed quoting: $detail."
    case WrongFieldCount(source, line, expected, actual) =>
      s"Delimited source='$source' line=$line has fields=$actual, expected=$expected."
    case DuplicateHeader(source, name, first, second) =>
      s"Delimited source='$source' header='$name' is duplicated at indices $first and $second."
    case MissingRequiredColumn(source, column, header) =>
      s"Delimited source='$source' requires column='$column', header=${header.mkString("[", ",", "]")}."
    case MissingValue(source, line, column) =>
      s"Delimited source='$source' line=$line column='$column' is missing."
    case InvalidNumber(source, line, column, value, unit) =>
      s"Delimited source='$source' line=$line column='$column' value='$value' is not a finite number in $unit."
    case TimestampOutsideRange(source, line, column, value, unit) =>
      s"Delimited source='$source' line=$line column='$column' value='$value' in ${unit.label} is outside microsecond timestamp range."
    case UnknownValidity(source, line, column, value) =>
      s"Delimited source='$source' line=$line column='$column' has unknown validity token='$value'."
    case InvalidPupil(source, line, column, value, unit) =>
      s"Delimited source='$source' line=$line column='$column' has nonpositive or nonfinite pupil value='$value' in unit=$unit."
    case NonIncreasingTimestamp(source, line, previousLine, previous, current) =>
      s"Delimited source='$source' line=$line timestamp=${current.render} does not follow accepted line=$previousLine timestamp=${previous.render}."
    case IncompleteSample(source, line, columns) =>
      s"Delimited source='$source' line=$line could not produce a sample from columns=${columns.mkString("[", ",", "]")}."
    case NoAcceptedRows(source, supplied) =>
      s"Delimited source='$source' produced no accepted samples from suppliedRows=$supplied."
    case RecordingConstructionFailed(source, underlying) =>
      s"Delimited source='$source' could not construct a validated recording: ${underlying.message}"

end DelimitedDiagnostic
