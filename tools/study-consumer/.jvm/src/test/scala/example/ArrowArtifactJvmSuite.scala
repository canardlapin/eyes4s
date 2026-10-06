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

package example

import cats.effect.IO
import eyes4s.io.*
import io.circe.Json
import java.nio.file.Files
import org.apache.arrow.memory.RootAllocator
import org.apache.arrow.vector.{BigIntVector, VarCharVector}
import org.apache.arrow.vector.ipc.ArrowStreamReader
import org.apache.arrow.vector.types.pojo.Schema

/** The optional Arrow transport from the packaged library with downstream-selected Jackson. */
class ArrowArtifactJvmSuite extends munit.CatsEffectSuite:
  test("the packaged Arrow writer preserves cells and schema with patched Jackson") {
    val table = ResultTable
      .of(
        ResultFamily.PointSamples,
        Vector(
          ResultColumn("time_us", ResultColumnType.Int64, false, "us", "exact time"),
          ResultColumn("label", ResultColumnType.Utf8, true, "label", "source label")
        ),
        Vector(
          Vector(ResultCell.Integer(9007199254740993L), ResultCell.Text("quoted \"label\"")),
          Vector(ResultCell.Integer(Long.MaxValue), ResultCell.Missing)
        ),
        Json.obj("source" -> Json.fromString("packaged Arrow consumer"))
      )
      .fold(e => fail(e.message), identity)
    IO.blocking(Files.createTempFile("eyes4s-consumer-arrow-", ".arrow"))
      .bracket { path =>
        ArrowResultExport.write[IO](table, path, batchRows = 1).flatMap { written =>
          assertEquals(written, Right(()))
          IO.blocking {
            val allocator = new RootAllocator(1024L * 1024)
            try
              val reader = new ArrowStreamReader(Files.newInputStream(path), allocator)
              try
                val root   = reader.getVectorSchemaRoot
                val schema = root.getSchema
                assertEquals(
                  schema.getCustomMetadata.get("eyes4s.result_metadata"),
                  table.metadata.circe.noSpaces
                )
                // Arrow's schema JSON path initializes and exercises Jackson
                // databind, separately from its FlatBuffer IPC representation.
                assertEquals(Schema.fromJSON(schema.toJson), schema)
                val times  = root.getVector("time_us").asInstanceOf[BigIntVector]
                val labels = root.getVector("label").asInstanceOf[VarCharVector]
                assert(reader.loadNextBatch())
                assertEquals(root.getRowCount, 1)
                assertEquals(times.get(0), 9007199254740993L)
                assertEquals(labels.getObject(0).toString, "quoted \"label\"")
                assert(reader.loadNextBatch())
                assertEquals(root.getRowCount, 1)
                assertEquals(times.get(0), Long.MaxValue)
                assert(labels.isNull(0))
                assert(!reader.loadNextBatch())
              finally reader.close()
              assertEquals(allocator.getAllocatedMemory, 0L)
            finally allocator.close()
          }
        }
      }(path => IO.blocking(Files.deleteIfExists(path)).void)
  }
