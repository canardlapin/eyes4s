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

/** Bounded rendering for hostile source operands retained by diagnostics. The
  * exact bytes remain available through [[AscSourceLine]].
  */
private[io] object AscDiagnosticText:
  val MaximumCharacters: Int = 160

  def bounded(value: String): String =
    if value.length <= MaximumCharacters then value
    else value.take(MaximumCharacters - 3) + "..."

  def bounded(values: Vector[String]): Vector[String] =
    val rendered = values.mkString(",")
    if rendered.length <= MaximumCharacters then values
    else Vector(bounded(rendered))

/** Validated maximum physical ASC line length. */
final class AscLineLimit private (val bytes: Int)

object AscLineLimit:
  val Default: AscLineLimit = new AscLineLimit(1024 * 1024)

  def of(bytes: Int): Either[AscSourceLineError, AscLineLimit] =
    if bytes <= 0 then Left(AscSourceLineError.NonPositiveLineLimit(bytes))
    else Right(new AscLineLimit(bytes))

/** One physical line after the streaming layer has removed its line terminator.
  *
  * Structural tokens are ASCII. The original bytes remain authoritative so a
  * message payload is not corrupted by an eager or platform-specific decoder.
  */
final class AscSourceLine private (
    val source: String,
    val number: Long,
    val byteOffset: Long,
    val bytes: IArray[Byte],
    val terminator: AscLineTerminator
):
  lazy val diagnosticExcerpt: String =
    val maximumCharacters = 160
    val builder           = new java.lang.StringBuilder
    var index             = 0
    var render            = true
    while index < bytes.length && render do
      val value    = bytes(index) & 0xff
      val rendered =
        if value >= 0x20 && value <= 0x7e then value.toChar.toString
        else
          val digits = "0123456789abcdef"
          s"\\x${digits.charAt(value >>> 4)}${digits.charAt(value & 0x0f)}"
      if builder.length + rendered.length <= maximumCharacters then
        builder.append(rendered): Unit
        index += 1
      else render = false
    if index < bytes.length then builder.append("..."): Unit
    builder.toString

object AscSourceLine:
  def of(
      source: String,
      number: Long,
      byteOffset: Long,
      bytes: IArray[Byte],
      limit: AscLineLimit = AscLineLimit.Default,
      terminator: AscLineTerminator = AscLineTerminator.EndOfFile
  ): Either[AscSourceLineError, AscSourceLine] =
    val normalizedSource = source.trim
    if normalizedSource.isEmpty then Left(AscSourceLineError.BlankSource(source))
    else if number <= 0L then
      Left(AscSourceLineError.NonPositiveLineNumber(normalizedSource, number))
    else if byteOffset < 0L then
      Left(AscSourceLineError.NegativeByteOffset(normalizedSource, byteOffset))
    else if bytes.length > limit.bytes then
      Left(AscSourceLineError.LineTooLong(normalizedSource, number, limit.bytes, bytes.length))
    else
      firstTerminator(bytes) match
        case Some((index, value)) =>
          Left(
            AscSourceLineError.EmbeddedLineTerminator(normalizedSource, number, index, value)
          )
        case None =>
          val retained = IArray.tabulate(bytes.length)(bytes.apply)
          Right(new AscSourceLine(normalizedSource, number, byteOffset, retained, terminator))

  private def firstTerminator(bytes: IArray[Byte]): Option[(Int, Byte)] =
    var index: Int                 = 0
    var found: Option[(Int, Byte)] = None
    while index < bytes.length && found.isEmpty do
      val value = bytes(index)
      if value == '\n'.toByte || value == '\r'.toByte then found = Some(index -> value)
      index += 1
    found

enum AscSourceLineError derives CanEqual:
  case BlankSource(value: String)
  case NonPositiveLineNumber(source: String, number: Long)
  case NegativeByteOffset(source: String, offset: Long)
  case NonPositiveLineLimit(bytes: Int)
  case LineTooLong(source: String, line: Long, limit: Int, actual: Int)
  case EmbeddedLineTerminator(source: String, line: Long, index: Int, value: Byte)

  def message: String = this match
    case BlankSource(value) =>
      s"ASC source='$value' is blank."
    case NonPositiveLineNumber(source, number) =>
      s"ASC source='$source' has nonpositive line number=$number."
    case NegativeByteOffset(source, offset) =>
      s"ASC source='$source' has negative byte offset=$offset."
    case NonPositiveLineLimit(bytes) =>
      s"ASC line limit bytes=$bytes is not positive."
    case LineTooLong(source, line, limit, actual) =>
      s"ASC source='$source' line=$line has bytes=$actual, exceeding limit=$limit."
    case EmbeddedLineTerminator(source, line, index, value) =>
      s"ASC source='$source' line=$line contains terminator byte=${value & 0xff} at index=$index."

end AscSourceLineError

/** Physical delimiter removed by the streaming line reader. */
enum AscLineTerminator derives CanEqual:
  case LineFeed
  case CarriageReturnLineFeed
  case EndOfFile

/** Encoding contract for structural tokens and uninterpreted payload bytes.
  *
  * EyeLink tags are parsed as printable ASCII. Payloads and the complete source
  * line remain byte-preserving. A UTF-8 BOM is structural only at byte zero of
  * the first physical line; elsewhere it is diagnosed and retained as an
  * unknown token.
  */
enum AscEncodingPolicy derives CanEqual:
  case PrintableAsciiStructurePreservePayload

/** Byte range of one whitespace-delimited token. */
final class AscToken private[io] (
    val start: Int,
    val end: Int,
    private val sourceBytes: IArray[Byte]
):
  def bytes: IArray[Byte] = AscBytes.slice(sourceBytes, start, end)

  /** Structural ASCII view. `None` leaves the original bytes intact. */
  lazy val ascii: Option[String] = AscBytes.ascii(sourceBytes, start, end)

/** Exact remainder of a line, normally used for messages and unknown records. */
final class AscPayload private[io] (val bytes: IArray[Byte]):
  def ascii: Option[String] = AscBytes.ascii(bytes, 0, bytes.length)

/** Raw fields following a known record tag, or every field for a sample. */
final class AscFields private[io] (
    val source: AscSourceLine,
    val tokens: Vector[AscToken]
):
  def size: Int = tokens.length

  def token(index: Int): Option[AscToken] = tokens.lift(index)

  /** Exact bytes after a selected token, excluding only the separating ASCII
    * spaces or tabs. Internal and trailing payload whitespace is retained.
    */
  def remainderAfter(index: Int): Option[AscPayload] =
    token(index).map { selected =>
      var start = selected.end
      while start < source.bytes.length && AscBytes.isHorizontalSpace(source.bytes(start)) do
        start += 1
      new AscPayload(AscBytes.slice(source.bytes, start, source.bytes.length))
    }

enum AscBoundaryKind derives CanEqual:
  case Start
  case End

enum AscConfigurationKind derives CanEqual:
  case Samples
  case Events
  case Prescaler
  case VelocityPrescaler
  case Pupil

enum AscNativeEventKind derives CanEqual:
  case FixationStart
  case FixationEnd
  case SaccadeStart
  case SaccadeEnd
  case BlinkStart
  case BlinkEnd
  case FixationUpdate
  case SaccadeUpdate

enum AscKnownRecord derives CanEqual:
  case Message
  case Boundary(kind: AscBoundaryKind)
  case Configuration(kind: AscConfigurationKind)
  case NativeEvent(kind: AscNativeEventKind)
  case Button
  case Input
  case LostData

  def minimumFields: Int = this match
    case Message                                       => 1
    case Boundary(_)                                   => 1
    case Configuration(_)                              => 1
    case NativeEvent(AscNativeEventKind.FixationStart) => 2
    case NativeEvent(AscNativeEventKind.SaccadeStart)  => 2
    case NativeEvent(AscNativeEventKind.BlinkStart)    => 2
    case NativeEvent(_)                                => 3
    case Button                                        => 2
    case Input                                         => 2
    case LostData                                      => 1

/** Lossless lexical classification before block-specific interpretation. */
enum AscRecord:
  case Blank
  case Comment(payload: AscPayload)
  case Sample(fields: AscFields)
  case Message(fields: AscFields)
  case Boundary(kind: AscBoundaryKind, fields: AscFields)
  case Configuration(kind: AscConfigurationKind, fields: AscFields)
  case NativeEvent(kind: AscNativeEventKind, fields: AscFields)
  case Button(fields: AscFields)
  case Input(fields: AscFields)
  case LostData(fields: AscFields)
  case Unknown(token: AscToken, payload: AscPayload)
  case MalformedKnown(kind: AscKnownRecord, fields: AscFields)

/** One physical line is always represented by exactly one result. */
final case class AscLineResult(
    source: AscSourceLine,
    encoding: AscEncodingPolicy,
    record: AscRecord,
    diagnostics: Vector[AscLexicalDiagnostic]
) derives CanEqual

enum AscLexicalDiagnostic derives CanEqual:
  case InvalidRecordToken(
      source: String,
      line: Long,
      column: Int,
      excerpt: String
  )
  case TooFewFields(
      source: String,
      line: Long,
      record: AscKnownRecord,
      expectedAtLeast: Int,
      actual: Int,
      excerpt: String
  )

  def message: String = this match
    case InvalidRecordToken(source, line, column, excerpt) =>
      s"ASC source='$source' line=$line has a record token outside printable ASCII at column=$column; excerpt='$excerpt'."
    case TooFewFields(source, line, record, expected, actual, excerpt) =>
      s"ASC source='$source' line=$line record=$record has fields=$actual, expectedAtLeast=$expected; excerpt='$excerpt'."

end AscLexicalDiagnostic

/** Pure, total lexical classifier for one validated physical line. */
object EyeLinkAscLexer:

  def parse(
      line: AscSourceLine,
      encoding: AscEncodingPolicy = AscEncodingPolicy.PrintableAsciiStructurePreservePayload
  ): AscLineResult =
    val tokens = tokenize(line.bytes, structuralStart(line))
    tokens.headOption match
      case None => AscLineResult(line, encoding, AscRecord.Blank, Vector.empty)
      case Some(first) if isComment(line.bytes, first.start) =>
        AscLineResult(
          line,
          encoding,
          AscRecord.Comment(
            new AscPayload(AscBytes.slice(line.bytes, first.start, line.bytes.length))
          ),
          Vector.empty
        )
      case Some(first) =>
        AscBytes.printableAscii(line.bytes, first.start, first.end) match
          case None =>
            AscLineResult(
              line,
              encoding,
              AscRecord.Unknown(
                first,
                payloadAfter(line.bytes, first.end)
              ),
              Vector(
                AscLexicalDiagnostic.InvalidRecordToken(
                  line.source,
                  line.number,
                  first.start + 1,
                  line.diagnosticExcerpt
                )
              )
            )
          case Some(value) if startsNumeric(value) =>
            AscLineResult(
              line,
              encoding,
              AscRecord.Sample(new AscFields(line, tokens)),
              Vector.empty
            )
          case Some(value) =>
            known(AscBytes.upperAscii(value)) match
              case None =>
                AscLineResult(
                  line,
                  encoding,
                  AscRecord.Unknown(first, payloadAfter(line.bytes, first.end)),
                  Vector.empty
                )
              case Some(kind) =>
                val fields = new AscFields(line, tokens.drop(1))
                if fields.size < kind.minimumFields then
                  val diagnostic = AscLexicalDiagnostic.TooFewFields(
                    line.source,
                    line.number,
                    kind,
                    kind.minimumFields,
                    fields.size,
                    line.diagnosticExcerpt
                  )
                  AscLineResult(
                    line,
                    encoding,
                    AscRecord.MalformedKnown(kind, fields),
                    Vector(diagnostic)
                  )
                else AscLineResult(line, encoding, record(kind, fields), Vector.empty)

  private def record(kind: AscKnownRecord, fields: AscFields): AscRecord = kind match
    case AscKnownRecord.Message               => AscRecord.Message(fields)
    case AscKnownRecord.Boundary(boundary)    => AscRecord.Boundary(boundary, fields)
    case AscKnownRecord.Configuration(config) => AscRecord.Configuration(config, fields)
    case AscKnownRecord.NativeEvent(event)    => AscRecord.NativeEvent(event, fields)
    case AscKnownRecord.Button                => AscRecord.Button(fields)
    case AscKnownRecord.Input                 => AscRecord.Input(fields)
    case AscKnownRecord.LostData              => AscRecord.LostData(fields)

  private def known(value: String): Option[AscKnownRecord] = value match
    case "MSG"        => Some(AscKnownRecord.Message)
    case "START"      => Some(AscKnownRecord.Boundary(AscBoundaryKind.Start))
    case "END"        => Some(AscKnownRecord.Boundary(AscBoundaryKind.End))
    case "SAMPLES"    => Some(AscKnownRecord.Configuration(AscConfigurationKind.Samples))
    case "EVENTS"     => Some(AscKnownRecord.Configuration(AscConfigurationKind.Events))
    case "PRESCALER"  => Some(AscKnownRecord.Configuration(AscConfigurationKind.Prescaler))
    case "VPRESCALER" =>
      Some(AscKnownRecord.Configuration(AscConfigurationKind.VelocityPrescaler))
    case "PUPIL"     => Some(AscKnownRecord.Configuration(AscConfigurationKind.Pupil))
    case "SFIX"      => Some(AscKnownRecord.NativeEvent(AscNativeEventKind.FixationStart))
    case "EFIX"      => Some(AscKnownRecord.NativeEvent(AscNativeEventKind.FixationEnd))
    case "SSACC"     => Some(AscKnownRecord.NativeEvent(AscNativeEventKind.SaccadeStart))
    case "ESACC"     => Some(AscKnownRecord.NativeEvent(AscNativeEventKind.SaccadeEnd))
    case "SBLINK"    => Some(AscKnownRecord.NativeEvent(AscNativeEventKind.BlinkStart))
    case "EBLINK"    => Some(AscKnownRecord.NativeEvent(AscNativeEventKind.BlinkEnd))
    case "FIXUPDATE" => Some(AscKnownRecord.NativeEvent(AscNativeEventKind.FixationUpdate))
    case "SACUPDATE" => Some(AscKnownRecord.NativeEvent(AscNativeEventKind.SaccadeUpdate))
    case "BUTTON"    => Some(AscKnownRecord.Button)
    case "INPUT"     => Some(AscKnownRecord.Input)
    case "LOST_DATA_EVENT" => Some(AscKnownRecord.LostData)
    case _                 => None

  private def startsNumeric(value: String): Boolean =
    def isAsciiDigit(value: Char): Boolean = value >= '0' && value <= '9'
    value.headOption.exists(isAsciiDigit) ||
    (value.length > 1 &&
      (value.charAt(0) == '+' || value.charAt(0) == '-') &&
      isAsciiDigit(value.charAt(1)))

  private def structuralStart(line: AscSourceLine): Int =
    if line.number == 1L && line.byteOffset == 0L && AscBytes.hasUtf8Bom(line.bytes) then 3
    else 0

  private def isComment(bytes: IArray[Byte], start: Int): Boolean =
    bytes(start) == '#'.toByte ||
      (bytes(start) == '*'.toByte && start + 1 < bytes.length && bytes(start + 1) == '*'.toByte)

  private def payloadAfter(bytes: IArray[Byte], end: Int): AscPayload =
    var start = end
    while start < bytes.length && AscBytes.isHorizontalSpace(bytes(start)) do start += 1
    new AscPayload(AscBytes.slice(bytes, start, bytes.length))

  private def tokenize(bytes: IArray[Byte], from: Int): Vector[AscToken] =
    val output = Vector.newBuilder[AscToken]
    var index  = from
    while index < bytes.length do
      while index < bytes.length && AscBytes.isHorizontalSpace(bytes(index)) do index += 1
      if index < bytes.length then
        val start = index
        while index < bytes.length && !AscBytes.isHorizontalSpace(bytes(index)) do index += 1
        output += new AscToken(start, index, bytes)
    output.result()

end EyeLinkAscLexer

private object AscBytes:
  def isHorizontalSpace(value: Byte): Boolean = value == ' '.toByte || value == '\t'.toByte

  def slice(bytes: IArray[Byte], from: Int, until: Int): IArray[Byte] =
    IArray.tabulate(math.max(0, until - from))(index => bytes(from + index))

  def ascii(bytes: IArray[Byte], from: Int, until: Int): Option[String] =
    val builder = new java.lang.StringBuilder(math.max(0, until - from))
    var index   = from
    var valid   = true
    while index < until && valid do
      val value = bytes(index) & 0xff
      if value > 0x7f then valid = false
      else builder.append(value.toChar)
      index += 1
    Option.when(valid)(builder.toString)

  def printableAscii(bytes: IArray[Byte], from: Int, until: Int): Option[String] =
    ascii(bytes, from, until).filter(_.forall(value => value >= 0x21 && value <= 0x7e))

  def upperAscii(value: String): String =
    value.map { character =>
      if character >= 'a' && character <= 'z' then (character - ('a' - 'A')).toChar
      else character
    }

  def hasUtf8Bom(bytes: IArray[Byte]): Boolean =
    bytes.length >= 3 &&
      bytes(0) == 0xef.toByte &&
      bytes(1) == 0xbb.toByte &&
      bytes(2) == 0xbf.toByte
