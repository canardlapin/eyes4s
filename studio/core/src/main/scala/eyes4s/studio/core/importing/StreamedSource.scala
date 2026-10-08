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

package eyes4s.studio.core.importing

import cats.effect.Async
import cats.syntax.all.*
import eyes4s.codec.ByteDigest
import eyes4s.results.Sha256Core
import eyes4s.studio.core.document.{SourcePath, SourceRole}
import eyes4s.studio.core.platform.PlatformFailure
import fs2.{Chunk, Stream}

import java.nio.{ByteBuffer, CharBuffer}
import java.nio.charset.{CodingErrorAction, StandardCharsets}
import scala.util.control.NonFatal

/** Strict incremental source decoding; only bounded byte/character buffers are allocated.
  * The returned text is the decoder's CharSequence, without a whole-file decode copy.
  */
object StreamedSource:
  val ChunkBytes: Int      = 8192
  val MaxSourceBytes: Long = 1024L * 1024 * 1024

  final case class Text(bytes: ByteDigest, value: CharSequence)

  private final class TextSequence(value: java.lang.StringBuilder) extends CharSequence:
    def length(): Int                                   = value.length()
    def charAt(index: Int): Char                        = value.charAt(index)
    def subSequence(start: Int, end: Int): CharSequence = value.substring(start, end)
    override def toString: String                       = value.toString

  private final class Decoder(file: String, maxBytes: Long):
    private val hash    = new Sha256Core.Hasher
    private val decoder = StandardCharsets.UTF_8
      .newDecoder()
      .onMalformedInput(CodingErrorAction.REPORT)
      .onUnmappableCharacter(CodingErrorAction.REPORT)
    private val input                    = ByteBuffer.allocate(ChunkBytes + 4)
    private val output                   = CharBuffer.allocate(ChunkBytes)
    private val text                     = new java.lang.StringBuilder
    private var offset                   = 0L
    private var started                  = false
    private var received                 = 0L
    var problem: Option[SourceReadError] = None

    def feed(chunk: Chunk[Byte]): Unit =
      if chunk.size.toLong > maxBytes - received then
        problem = Some(
          SourceReadError.Unreadable(
            file,
            s"Source exceeds the $maxBytes-byte budget after $received bytes and a ${chunk.size}-byte chunk"
          )
        )
      else feedWithinBudget(chunk)

    private def feedWithinBudget(chunk: Chunk[Byte]): Unit =
      received += chunk.size.toLong
      var i = 0
      while i < chunk.size do
        val byte = chunk(i)
        hash.update(byte)
        input.put(byte)
        i += 1
      if started || input.position() >= 2 then decode(last = false)

    private def decode(last: Boolean): Unit =
      input.flip()
      if !started then
        started = true
        if input.remaining() >= 2 then
          val first  = input.get(0) & 0xff
          val second = input.get(1) & 0xff
          if (first == 0xfe && second == 0xff) || (first == 0xff && second == 0xfe) then
            problem = Some(SourceReadError.Unpreviewable(SniffError.Utf16(file)))
      var more = problem.isEmpty
      while more do
        val result = decoder.decode(input, output, last)
        output.flip()
        text.append(output)
        output.clear()
        if result.isError then
          problem = Some(
            SourceReadError.Unpreviewable(SniffError.NotUtf8(file, offset + input.position()))
          )
        more = result.isOverflow && problem.isEmpty
      offset += input.position()
      input.compact(): Unit

    def finish(): Either[SourceReadError, Text] =
      if problem.isEmpty then decode(last = true)
      if problem.isEmpty then
        var more = true
        while more do
          val result = decoder.flush(output)
          output.flip()
          text.append(output)
          output.clear()
          if result.isError then
            problem = Some(SourceReadError.Unpreviewable(SniffError.NotUtf8(file, offset)))
          more = result.isOverflow && problem.isEmpty
      problem.toLeft(()).flatMap { _ =>
        val hex = hash.finish().iterator.map(b => f"${b & 0xff}%02x").mkString
        ByteDigest
          .parse(hex)
          .left
          .map(e => SourceReadError.Unreadable(file, e.message))
          .map(d => Text(d, new TextSequence(text)))
      }

  private def unreadable(file: String, error: Throwable): SourceReadError = error match
    case PlatformFailure(e) => SourceReadError.Unreadable(file, e.message)
    case other              =>
      SourceReadError.Unreadable(file, Option(other.getMessage).getOrElse(other.toString))

  def text[F[_]: Async](
      file: String,
      bytes: Stream[F, Byte],
      maxBytes: Long = MaxSourceBytes
  ): F[Either[SourceReadError, Text]] =
    if maxBytes < 0 || maxBytes > MaxSourceBytes then
      Async[F].pure(
        Left(
          SourceReadError
            .Unreadable(file, s"Invalid byte budget $maxBytes; supported 0 to $MaxSourceBytes")
        )
      )
    else
      Async[F]
        .defer {
          val decoder = new Decoder(file, maxBytes)
          bytes
            .chunkLimit(ChunkBytes)
            .evalMap { chunk =>
              Async[F].delay {
                decoder.feed(chunk)
                decoder.problem.isEmpty
              }
            }
            .takeThrough(identity)
            .compile
            .drain >> Async[F].delay(decoder.finish())
        }
        .handleErrorWith {
          case NonFatal(e) => Async[F].pure(Left(unreadable(file, e)))
          case other       => Async[F].raiseError(other)
        }

  def preview[F[_]: Async](
      role: SourceRole,
      name: String,
      bytes: Stream[F, Byte]
  ): F[Either[SourceReadError, SniffedSource]] =
    SourcePath.of(name) match
      case Left(e)     => Async[F].pure(Left(SourceReadError.BadPath(e)))
      case Right(path) =>
        val file = path.value.split('/').last
        text(file, bytes).map(_.flatMap { read =>
          CsvSniffer
            .sniff(file, read.value)
            .left
            .map(SourceReadError.Unpreviewable(_))
            .map(p => SniffedSource(role, path, read.bytes, p))
        })

  /** Exact bytes for project storage; reject over-budget chunks before retaining them. */
  def bytes[F[_]: Async](
      file: String,
      source: Stream[F, Byte],
      maxBytes: Long = MaxSourceBytes
  ): F[Either[SourceReadError, IArray[Byte]]] =
    if maxBytes < 0 || maxBytes > MaxSourceBytes then
      Async[F].pure(
        Left(
          SourceReadError
            .Unreadable(file, s"Invalid byte budget $maxBytes; supported 0 to $MaxSourceBytes")
        )
      )
    else
      Async[F]
        .defer {
          val chunks                           = Vector.newBuilder[Chunk[Byte]]
          var count                            = 0L
          var problem: Option[SourceReadError] = None
          val limit                            = maxBytes
          source.chunks
            .evalMap { chunk =>
              Async[F].delay {
                if chunk.size.toLong > limit - count then
                  problem = Some(
                    SourceReadError.Unreadable(
                      file,
                      s"Source exceeds the $limit-byte budget after $count bytes and a ${chunk.size}-byte chunk"
                    )
                  )
                else
                  count += chunk.size.toLong
                  chunks += chunk
                problem.isEmpty
              }
            }
            .takeThrough(identity)
            .compile
            .drain >> Async[F].delay {
            problem.toLeft(()).map { _ =>
              val result = Array.ofDim[Byte](count.toInt)
              var at     = 0
              chunks.result().foreach { chunk =>
                var i = 0
                while i < chunk.size do
                  result(at) = chunk(i)
                  at += 1
                  i += 1
              }
              IArray.unsafeFromArray(result)
            }
          }
        }
        .handleErrorWith {
          case NonFatal(e) => Async[F].pure(Left(unreadable(file, e)))
          case other       => Async[F].raiseError(other)
        }
