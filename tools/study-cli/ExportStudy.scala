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

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

/** JVM guide launcher; filesystem effects stay outside the pure analysis modules. */
object ExportStudy:
  def main(args: Array[String]): Unit =
    if args.length != 2 then
      System.err.println("Usage: ExportStudy fixation.csv output-directory")
      sys.exit(2)
    else
      val csv = Files.readString(Path.of(args(0)), StandardCharsets.UTF_8)
      StudyGuide.run(csv) match
        case Left(error) =>
          System.err.println(StudyGuide.message(error))
          sys.exit(1)
        case Right(output) =>
          val directory = Path.of(args(1))
          Files.createDirectories(directory)
          Files.writeString(
            directory.resolve("study.json"),
            output.savedPlan,
            StandardCharsets.UTF_8
          )
          Files.writeString(
            directory.resolve("contrasts.csv"),
            output.contrasts.encode,
            StandardCharsets.UTF_8
          )
          println(
            s"Wrote ${output.contrasts.rows.size} contrast rows and a saved plan to $directory"
          )
