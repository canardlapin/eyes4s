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

package eyes4s.examples

import cats.effect.{IO, IOApp, ExitCode}
import cats.syntax.all.*
import eyes4s.io.*
import io.circe.Json
import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets.UTF_8

/** Emit actual public-example artifacts for independent offline consumers. */
object BaselineExportMain extends IOApp:
  def run(args: List[String]): IO[ExitCode] = args match
    case directory :: Nil =>
      val root = Paths.get(directory)
      for
        tables <- IO.fromEither(
          BaselineExportGuide.tables.left.map(new IllegalStateException(_))
        )
        _ <- IO.blocking(Files.createDirectories(root))
        _ <- tables.traverse_ { (name, table) =>
          for
            _ <- IO.blocking(
              Files.writeString(root.resolve(name + ".csv"), table.csv.encode, UTF_8)
            )
            _ <- IO.blocking(
              Files.writeString(
                root.resolve(name + ".metadata.json"),
                table.metadata.spaces2 + "\n",
                UTF_8
              )
            )
            written <- ArrowResultExport
              .write[IO](table, root.resolve(name + ".arrow"), batchRows = 3)
            _ <- IO.fromEither(written.left.map(e => new IllegalStateException(e.message)))
          yield ()
        }
        index = Json.arr(
          tables.map((name, t) =>
            Json.obj(
              "name"         -> Json.fromString(name),
              "family"       -> Json.fromString(t.family.toString),
              "rows"         -> Json.fromInt(t.rows.size),
              "table_sha256" -> Json.fromString(t.identity.hex)
            )
          )*
        )
        _ <- IO.blocking(
          Files.writeString(root.resolve("index.json"), index.spaces2 + "\n", UTF_8)
        )
        _ <- IO.println(s"Wrote ${tables.size} CSV/metadata/Arrow artifacts to $root")
      yield ExitCode.Success
    case _ => IO.println("usage: BaselineExportMain OUTPUT_DIRECTORY").as(ExitCode.Error)
