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

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, StandardOpenOption}

/** Developer/example CLI; not a hidden process-launching dependency of pure modules. */
object TemplateFitCli:
  def main(args: Array[String]): Unit =
    if args.length != 2 || !Set("fit", "evaluate", "export", "import").contains(args(0)) then
      sys.error("Usage: TemplateFitCli fit|evaluate|export|import DIRECTORY")
    val directory = Path.of(args(1))
    args(0) match
      case "export" =>
        Files.createDirectories(directory)
        val prepared = (for
          split  <- TemplateFitGuide.input().left.map(TemplateFitGuide.message)
          result <- TemplateFitGuide.prepare(split).left.map(TemplateFitGuide.message)
        yield result).fold(sys.error, identity)
        Files.writeString(
          directory.resolve("recipe.json"),
          prepared.savedRecipe,
          UTF_8,
          StandardOpenOption.CREATE_NEW
        )
        Files.writeString(
          directory.resolve("training.csv"),
          prepared.trainingCsv,
          UTF_8,
          StandardOpenOption.CREATE_NEW
        )
        println(s"Exported training-only request and saved recipe to $directory")
      case "fit" =>
        Files.createDirectories(directory)
        val saved = (for
          split  <- TemplateFitGuide.input().left.map(TemplateFitGuide.message)
          recipe <- TemplateFitGuide.saveNative(split).left.map(TemplateFitGuide.message)
          _      <- TemplateFitGuide.evaluateNative(recipe).left.map(TemplateFitGuide.message)
        yield recipe).fold(sys.error, identity)
        Files.writeString(
          directory.resolve("recipe.json"),
          saved,
          UTF_8,
          StandardOpenOption.CREATE_NEW
        )
        println(s"Saved native fitting recipe to $directory; reopen with evaluate")
      case "evaluate" =>
        val result = TemplateFitGuide
          .evaluateNative(
            Files.readString(directory.resolve("recipe.json"), UTF_8)
          )
          .fold(e => sys.error(TemplateFitGuide.message(e)), identity)
        result.rows.foreach(row =>
          println(s"${row.key}\t${row.fold}\t${row.observed}\t${row.result}")
        )
        println(s"held-out MSE = ${result.meanSquaredError}")
      case "import" =>
        val result = TemplateFitGuide
          .evaluate(
            Files.readString(directory.resolve("recipe.json"), UTF_8),
            Files.readString(directory.resolve("coefficients.csv"), UTF_8)
          )
          .fold(e => sys.error(TemplateFitGuide.message(e)), identity)
        result.rows.foreach(row =>
          println(s"${row.key}\t${row.fold}\t${row.observed}\t${row.result}")
        )
        println(s"held-out MSE = ${result.meanSquaredError}")
      case _ => () // validated above; this executable is an effectful example, not a pure API
