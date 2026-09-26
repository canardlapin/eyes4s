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

import cats.effect.kernel.{Resource, Sync}
import cats.syntax.all.*
import org.apache.arrow.memory.{BufferAllocator, RootAllocator}
import org.apache.arrow.vector.*
import org.apache.arrow.vector.ipc.ArrowStreamWriter
import org.apache.arrow.vector.dictionary.DictionaryProvider.MapDictionaryProvider
import org.apache.arrow.vector.types.FloatingPointPrecision
import org.apache.arrow.vector.types.pojo.{ArrowType, Field, FieldType, Schema}
import java.io.OutputStream
import java.nio.channels.Channels
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

enum ArrowExportError derives CanEqual:
  case Limits(memoryBytes: Long, batchRows: Int)
  case Write(path: String, reason: String)
  def message: String = this match
    case Limits(m, b) => s"Arrow memoryBytes=$m and batchRows=$b must be positive."
    case Write(p, r)  => s"Arrow output path=$p failed: $r."

/** JVM-only IPC stream writer. This object owns each file, writer, vector and allocator it opens. */
object ArrowResultExport:

  /** A missing optional Arrow jar or `--add-opens` surfaces as a `LinkageError`
    * (`NoClassDefFoundError`, `ExceptionInInitializerError`). Cats Effect treats those as
    * fatal, so they are converted here, where Arrow is first touched, into an ordinary
    * failure that `write` returns as a typed `Left`.
    */
  private final class ArrowUnavailable(cause: LinkageError)
      extends RuntimeException(cause.toString, cause)

  private def linked[F[_]: Sync, A](thunk: => A): F[A] =
    Sync[F].blocking {
      try thunk
      catch case e: LinkageError => throw new ArrowUnavailable(e)
    }

  def write[F[_]: Sync](
      table: ResultTable,
      path: Path,
      memoryBytes: Long = 64L * 1024 * 1024,
      batchRows: Int = 1024
  ): F[Either[ArrowExportError, Unit]] =
    writeUsing(table, path, memoryBytes, batchRows)(new RootAllocator(_))

  /** `write` with the allocator construction supplied, so a failed Arrow linkage is testable. */
  private[io] def writeUsing[F[_]: Sync](
      table: ResultTable,
      path: Path,
      memoryBytes: Long,
      batchRows: Int
  )(allocate: Long => BufferAllocator): F[Either[ArrowExportError, Unit]] =
    if memoryBytes <= 0 || batchRows <= 0 then
      Sync[F].pure(Left(ArrowExportError.Limits(memoryBytes, batchRows)))
    else
      val resources = for
        allocator <- Resource.fromAutoCloseable(linked(allocate(memoryBytes)))
        stream    <- Resource.fromAutoCloseable(Sync[F].blocking(Files.newOutputStream(path)))
      yield (allocator, stream)
      resources
        .use { (allocator, stream) => writeTo(table, stream, allocator, batchRows) }
        .attempt
        .map(
          _.left.map {
            case e: ArrowUnavailable =>
              ArrowExportError.Write(
                path.toString,
                s"Arrow runtime unavailable (${e.getMessage}); the JVM Arrow writer needs " +
                  "arrow-vector, arrow-memory-unsafe and " +
                  "--add-opens=java.base/java.nio=ALL-UNNAMED"
              )
            case e =>
              ArrowExportError
                .Write(path.toString, Option(e.getMessage).getOrElse(e.getClass.getName))
          }
        )

  /** Borrow allocator; own the stream through writer.close, also on a failed write. */
  private[io] def writeTo[F[_]: Sync](
      table: ResultTable,
      stream: OutputStream,
      allocator: BufferAllocator,
      batchRows: Int
  ): F[Unit] = linked(schemaOf(table)).flatMap(schema =>
    writeWith(table, schema, stream, allocator, batchRows)
  )

  private def schemaOf(table: ResultTable): Schema =
    val columns = ResultColumn(
      "table_sha256",
      ResultColumnType.Utf8,
      false,
      "sha256",
      "linked metadata identity"
    ) +: table.columns
    val fields = columns.map { c =>
      val kind: ArrowType = c.kind match
        case ResultColumnType.Utf8 | ResultColumnType.JsonUtf8 => new ArrowType.Utf8()
        case ResultColumnType.Int64                            => new ArrowType.Int(64, true)
        case ResultColumnType.Float64                          =>
          new ArrowType.FloatingPoint(FloatingPointPrecision.DOUBLE)
        case ResultColumnType.Boolean => new ArrowType.Bool()
      val metadata = Map(
        "unit"    -> c.unit,
        "meaning" -> c.meaning,
        "labels"  -> io.circe.Json.arr(c.labels.map(io.circe.Json.fromString)*).noSpaces
      ).asJava
      new Field(
        c.name,
        new FieldType(c.nullable, kind, null, metadata),
        java.util.Collections.emptyList[Field]()
      )
    }
    new Schema(fields.asJava, Map("eyes4s.result_metadata" -> table.metadata.noSpaces).asJava)

  private def writeWith[F[_]: Sync](
      table: ResultTable,
      schema: Schema,
      stream: OutputStream,
      allocator: BufferAllocator,
      batchRows: Int
  ): F[Unit] =
    val resources = for
      output <- Resource.make(Sync[F].pure(stream))(s => linked(s.close()))
      root   <- Resource.fromAutoCloseable(
        linked(VectorSchemaRoot.create(schema, allocator))
      )
      dictionaries <- Resource.fromAutoCloseable(linked(new MapDictionaryProvider()))
      writer       <- Resource.fromAutoCloseable(
        linked(new ArrowStreamWriter(root, dictionaries, Channels.newChannel(output)))
      )
    yield (root, writer)
    resources.use { (root, writer) =>
      linked {
        writer.start()
        table.rows.grouped(batchRows).foreach { batch =>
          root.allocateNew()
          batch.zipWithIndex.foreach { (row, i) =>
            (ResultCell.Text(table.identity.hex) +: row).zipWithIndex.foreach { (value, j) =>
              val vector = root.getVector(j)
              value match
                case ResultCell.Missing => vector.setNull(i)
                case ResultCell.Text(v) =>
                  vector.asInstanceOf[VarCharVector].setSafe(i, v.getBytes(UTF_8))
                case ResultCell.Integer(v) => vector.asInstanceOf[BigIntVector].setSafe(i, v)
                case ResultCell.Number(v)  => vector.asInstanceOf[Float8Vector].setSafe(i, v)
                case ResultCell.Flag(v)    =>
                  vector.asInstanceOf[BitVector].setSafe(i, if v then 1 else 0)
            }
          }
          root.setRowCount(batch.size)
          writer.writeBatch()
          root.clear()
        }
        writer.end()
      }
    }
