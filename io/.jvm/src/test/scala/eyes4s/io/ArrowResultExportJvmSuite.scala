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

import cats.effect.IO
import cats.syntax.all.*
import eyes4s.examples.BaselineExportGuide
import org.apache.arrow.memory.RootAllocator
import org.apache.arrow.vector.ipc.ArrowStreamReader
import org.apache.arrow.vector.BigIntVector
import java.io.{IOException, OutputStream}
import java.nio.file.Files
import scala.jdk.CollectionConverters.*

class ArrowResultExportJvmSuite extends munit.CatsEffectSuite:
  private def table = BaselineExportGuide.tables
    .fold(e => fail(e), identity)
    .find(_._1 == "point-pointsamples")
    .get
    ._2

  test(
    "IPC preserves nullability, exact signed Int64 and metadata while closing every allocation"
  ) {
    val t = table
    IO.blocking(Files.createTempFile("eyes4s-arrow-", ".arrow"))
      .bracket { path =>
        ArrowResultExport.write[IO](t, path, batchRows = 3).flatMap { result =>
          assertEquals(result, Right(()))
          IO.blocking {
            val allocator = new RootAllocator(64L * 1024 * 1024)
            try
              val reader = new ArrowStreamReader(Files.newInputStream(path), allocator)
              try
                val root = reader.getVectorSchemaRoot
                assertEquals(
                  root.getSchema.getCustomMetadata.get("eyes4s.result_metadata"),
                  t.metadata.noSpaces
                )
                assertEquals(
                  root.getSchema.getFields.asScala.map(_.getName).toVector,
                  Vector("table_sha256") ++ t.columns.map(_.name)
                )
                var rows = 0; var seen = Vector.empty[Long]; var missing = 0
                while reader.loadNextBatch() do
                  val times = root.getVector("time_us").asInstanceOf[BigIntVector]
                  val value = root.getVector("value")
                  for i <- 0 until root.getRowCount do
                    seen = seen :+ times.get(i)
                    if value.isNull(i) then missing += 1
                  rows += root.getRowCount
                assertEquals(rows, 30); assertEquals(missing, 12)
                assert(seen.contains(9007199254740993L)); assert(seen.contains(Long.MaxValue))
              finally reader.close()
              assertEquals(allocator.getAllocatedMemory, 0L)
            finally allocator.close()
          }
        }
      }(p => IO.blocking(Files.deleteIfExists(p)).void)
  }
  test(
    "a failed output closes the writer, stream and all vectors while preserving borrowed allocator ownership"
  ) {
    val allocator = new RootAllocator(64L * 1024 * 1024)
    var closed    = false
    val output    = new OutputStream:
      override def write(value: Int): Unit = throw new IOException("deliberate write failure")
      override def close(): Unit           = closed = true
    ArrowResultExport
      .writeTo[IO](table, output, allocator, 2)
      .attempt
      .flatMap { result =>
        IO {
          assert(result.isLeft); assert(closed); assertEquals(allocator.getAllocatedMemory, 0L)
          val probe = allocator.buffer(8); probe.close()
        }
      }
      .guarantee(IO(allocator.close()))
  }
  test("empty IPC still has the complete schema, and invalid limits do not create files") {
    val t =
      BaselineExportGuide.tables.fold(e => fail(e), identity).find(_._1 == "empty-pairs").get._2
    IO.blocking(Files.createTempFile("eyes4s-empty-arrow-", ".arrow"))
      .bracket { path =>
        for
          written <- ArrowResultExport.write[IO](t, path)
          _ = assertEquals(written, Right(()))
          _ <- IO.blocking {
            val allocator = new RootAllocator(1024 * 1024)
            try
              val reader = new ArrowStreamReader(Files.newInputStream(path), allocator)
              try
                assertEquals(
                  reader.getVectorSchemaRoot.getSchema.getFields.size,
                  t.columns.size + 1
                )
                assert(!reader.loadNextBatch())
              finally reader.close()
            finally allocator.close()
          }
          invalid <- ArrowResultExport.write[IO](
            t,
            path.resolveSibling(path.getFileName.toString + ".absent"),
            memoryBytes = 0
          )
          _ = assert(invalid.left.exists(_.message.contains("0")))
          _ = assert(!Files.exists(path.resolveSibling(path.getFileName.toString + ".absent")))
        yield ()
      }(p => IO.blocking(Files.deleteIfExists(p)).void)
  }
