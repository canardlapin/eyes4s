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

import eyes4s.fs2.*
import eyes4s.kernel.Detector
import eyes4s.kernel.Machine

import _root_.fs2.Pipe
import _root_.fs2.Stream
import _root_.fs2.io.file.Files
import _root_.fs2.io.file.Flags
import _root_.fs2.io.file.Path

final class AscReadChunkSize private (val bytes: Int)

object AscReadChunkSize:
  val Default: AscReadChunkSize = new AscReadChunkSize(64 * 1024)

  def of(bytes: Int): Either[AscStreamConfigurationError, AscReadChunkSize] =
    if bytes <= 0 then Left(AscStreamConfigurationError.NonPositiveReadChunk(bytes))
    else Right(new AscReadChunkSize(bytes))

/** Allocation and source-location policy validated before streaming starts. */
final class AscStreamSettings private (
    val source: String,
    val lineLimit: AscLineLimit,
    val readChunkSize: AscReadChunkSize,
    val encoding: AscEncodingPolicy
)

object AscStreamSettings:
  def of(
      source: String,
      maximumLineBytes: Int,
      readChunkBytes: Int,
      encoding: AscEncodingPolicy = AscEncodingPolicy.PrintableAsciiStructurePreservePayload
  ): Either[AscStreamConfigurationError, AscStreamSettings] =
    val normalized = source.trim
    if normalized.isEmpty then Left(AscStreamConfigurationError.BlankSource(source))
    else
      for
        lineLimit <- AscLineLimit
          .of(maximumLineBytes)
          .left
          .map(error => AscStreamConfigurationError.InvalidLineLimit(maximumLineBytes, error))
        chunkSize <- AscReadChunkSize.of(readChunkBytes)
      yield new AscStreamSettings(normalized, lineLimit, chunkSize, encoding)

enum AscStreamConfigurationError derives CanEqual:
  case BlankSource(value: String)
  case InvalidLineLimit(bytes: Int, cause: AscSourceLineError)
  case NonPositiveReadChunk(bytes: Int)

  def message: String = this match
    case BlankSource(value)             => s"ASC stream source='$value' is blank."
    case InvalidLineLimit(bytes, cause) =>
      s"ASC stream maximum line bytes=$bytes is invalid: ${cause.message}"
    case NonPositiveReadChunk(bytes) =>
      s"ASC stream read chunk bytes=$bytes is not positive."

end AscStreamConfigurationError

enum AscFramingDiagnostic derives CanEqual:
  case LineTooLong(
      source: String,
      line: Long,
      byteOffset: Long,
      limit: Int,
      actual: Long,
      excerpt: String
  )
  case LoneCarriageReturn(
      source: String,
      line: Long,
      byteOffset: Long,
      carriageReturnOffset: Long,
      excerpt: String
  )
  case SourceLineRejected(
      source: String,
      line: Long,
      byteOffset: Long,
      cause: AscSourceLineError
  )

  def message: String = this match
    case LineTooLong(source, line, byteOffset, limit, actual, excerpt) =>
      s"ASC source='$source' line=$line byteOffset=$byteOffset has bytes=$actual, exceeding limit=$limit; excerpt='$excerpt'."
    case LoneCarriageReturn(source, line, byteOffset, carriageReturnOffset, excerpt) =>
      s"ASC source='$source' line=$line byteOffset=$byteOffset has a lone CR at byte=$carriageReturnOffset; expected CRLF or LF; excerpt='$excerpt'."
    case SourceLineRejected(source, line, byteOffset, cause) =>
      s"ASC source='$source' line=$line byteOffset=$byteOffset failed source-line construction: ${cause.message}"

end AscFramingDiagnostic

enum AscFramingEmission:
  case Parsed(value: AscLineResult)
  case Rejected(value: AscFramingDiagnostic)

/** Pure byte framing. The retained buffer never exceeds `maximumLineBytes`;
  * overlong input is counted to its delimiter without being accumulated.
  */
object EyeLinkAscFraming:

  private final case class State(
      lineNumber: Long,
      lineStartOffset: Long,
      nextByteOffset: Long,
      retained: Vector[Byte],
      actualLength: Long,
      pendingCarriageReturn: Boolean,
      carriageReturnOffset: Option[Long]
  )

  def machine(settings: AscStreamSettings): Machine[IArray[Byte], AscFramingEmission] =
    Machine(
      new Detector[State, IArray[Byte], AscFramingEmission]:
        def init: State = State(1L, 0L, 0L, Vector.empty, 0L, false, None)

        def step(
            state: State,
            input: IArray[Byte]
        ): (State, Vector[AscFramingEmission]) =
          var current = state
          val output  = Vector.newBuilder[AscFramingEmission]
          var index   = 0
          while index < input.length do
            val (next, emitted) = consume(settings, current, input(index))
            current = next
            output ++= emitted
            index += 1
          (current, output.result())

        def flush(state: State): Vector[AscFramingEmission] =
          if state.pendingCarriageReturn then
            val (_, output) = rejectAtLoneCarriageReturn(settings, state)
            output
          else if state.actualLength > 0L then
            val (_, output) = finish(settings, state, AscLineTerminator.EndOfFile)
            output
          else Vector.empty
    )

  def whole(
      settings: AscStreamSettings,
      bytes: IArray[Byte]
  ): Vector[AscFramingEmission] = machine(settings).runAll(Vector(bytes))

  private def consume(
      settings: AscStreamSettings,
      state: State,
      value: Byte
  ): (State, Vector[AscFramingEmission]) =
    if state.pendingCarriageReturn then
      if value == '\n'.toByte then
        val consumed = state.copy(nextByteOffset = state.nextByteOffset + 1L)
        finish(settings, consumed, AscLineTerminator.CarriageReturnLineFeed)
      else
        val (nextLine, rejected) = rejectAtLoneCarriageReturn(settings, state)
        val (next, emitted)      = consume(settings, nextLine, value)
        (next, rejected ++ emitted)
    else if value == '\r'.toByte then
      (
        state.copy(
          nextByteOffset = state.nextByteOffset + 1L,
          pendingCarriageReturn = true,
          carriageReturnOffset = Some(state.nextByteOffset)
        ),
        Vector.empty
      )
    else if value == '\n'.toByte then
      val consumed = state.copy(nextByteOffset = state.nextByteOffset + 1L)
      finish(settings, consumed, AscLineTerminator.LineFeed)
    else
      val retained =
        if state.actualLength < settings.lineLimit.bytes.toLong then state.retained :+ value
        else state.retained
      (
        state.copy(
          nextByteOffset = state.nextByteOffset + 1L,
          retained = retained,
          actualLength = state.actualLength + 1L
        ),
        Vector.empty
      )

  private def finish(
      settings: AscStreamSettings,
      state: State,
      terminator: AscLineTerminator
  ): (State, Vector[AscFramingEmission]) =
    val output =
      if state.actualLength > settings.lineLimit.bytes.toLong then
        AscFramingEmission.Rejected(
          AscFramingDiagnostic.LineTooLong(
            settings.source,
            state.lineNumber,
            state.lineStartOffset,
            settings.lineLimit.bytes,
            state.actualLength,
            excerpt(state.retained)
          )
        )
      else
        val bytes = IArray.from(state.retained.toArray)
        AscSourceLine.of(
          settings.source,
          state.lineNumber,
          state.lineStartOffset,
          bytes,
          settings.lineLimit,
          terminator
        ) match
          case Right(line) =>
            AscFramingEmission.Parsed(EyeLinkAscLexer.parse(line, settings.encoding))
          case Left(error) =>
            AscFramingEmission.Rejected(
              AscFramingDiagnostic.SourceLineRejected(
                settings.source,
                state.lineNumber,
                state.lineStartOffset,
                error
              )
            )
    (nextLine(state), Vector(output))

  private def rejectAtLoneCarriageReturn(
      settings: AscStreamSettings,
      state: State
  ): (State, Vector[AscFramingEmission]) =
    if state.actualLength > settings.lineLimit.bytes.toLong then
      finish(settings, state, AscLineTerminator.EndOfFile)
    else
      val diagnostic = AscFramingDiagnostic.LoneCarriageReturn(
        settings.source,
        state.lineNumber,
        state.lineStartOffset,
        state.carriageReturnOffset.getOrElse(state.nextByteOffset - 1L),
        excerpt(state.retained)
      )
      (nextLine(state), Vector(AscFramingEmission.Rejected(diagnostic)))

  private def nextLine(state: State): State =
    State(
      state.lineNumber + 1L,
      state.nextByteOffset,
      state.nextByteOffset,
      Vector.empty,
      0L,
      false,
      None
    )

  private def excerpt(bytes: Vector[Byte]): String =
    val maximumCharacters = 160
    val builder           = new java.lang.StringBuilder
    var index             = 0
    var render            = true
    while index < bytes.length && render do
      val value    = bytes(index) & 0xff
      val rendered =
        if value >= 0x20 && value <= 0x7e then value.toChar.toString
        else f"\\x$value%02x"
      if builder.length + rendered.length <= maximumCharacters then
        builder.append(rendered): Unit
        index += 1
      else render = false
    if index < bytes.length then builder.append("..."): Unit
    builder.toString

end EyeLinkAscFraming

/** FS2 execution of the pure byte framer. */
object EyeLinkAscStreaming:

  def pipe[F[_]](settings: AscStreamSettings): Pipe[F, Byte, AscFramingEmission] =
    input =>
      input
        .through(boundedChunks(settings))
        .through(EyeLinkAscFraming.machine(settings).toPipe[F])

  private[io] def boundedChunks[F[_]](
      settings: AscStreamSettings
  ): Pipe[F, Byte, IArray[Byte]] =
    input =>
      input
        .chunkN(settings.readChunkSize.bytes, allowFewer = true)
        .map(chunk => IArray.tabulate(chunk.size)(chunk.apply))

  /** Resource-safe platform file streaming supplied by FS2's `Files`
    * implementation (JVM files or the platform-appropriate Scala.js backend).
    */
  def readPath[F[_]](
      path: Path,
      settings: AscStreamSettings
  )(using files: Files[F]): Stream[F, AscFramingEmission] =
    files
      .readAll(path, settings.readChunkSize.bytes, Flags.Read)
      .through(pipe(settings))

end EyeLinkAscStreaming
