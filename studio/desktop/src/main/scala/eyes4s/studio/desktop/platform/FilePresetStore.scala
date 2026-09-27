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

package eyes4s.studio.desktop.platform

import eyes4s.studio.core.importing.{ImportPreset, ImportPresets}
import io.circe.parser.decode
import io.circe.syntax.*

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths, StandardCopyOption}
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/** Import presets (ticket S5.2) as JSON files in one directory, one file per
  * preset, named by the hex of its name's UTF-8 bytes so any name is a safe
  * file name. A write goes to a staging file that is moved into place, so a
  * reader never sees a partial preset.
  */
final class FilePresetStore(val directory: Path):

  private def fileOf(preset: ImportPreset): Path =
    val hex = preset.name.value.getBytes(UTF_8).map(b => f"${b & 0xff}%02x").mkString
    directory.resolve(s"$hex.json")

  /** Write `preset`; the error names the file and the reason. */
  def save(preset: ImportPreset): Either[String, Unit] =
    val target = fileOf(preset)
    try
      Files.createDirectories(directory)
      val staged = Files.createTempFile(directory, ".staging-", ".json")
      Files.writeString(staged, preset.asJson.spaces2, UTF_8)
      Files.move(
        staged,
        target,
        StandardCopyOption.ATOMIC_MOVE,
        StandardCopyOption.REPLACE_EXISTING
      )
      Right(())
    catch case NonFatal(e) => Left(s"$target: ${Option(e.getMessage).getOrElse(e.toString)}")

  /** Every readable preset in file-name order, and a message for each file
    * that is not one (never dropped silently).
    */
  def load: (ImportPresets, Vector[String]) =
    if !Files.isDirectory(directory) then (ImportPresets.empty, Vector.empty)
    else
      val listed =
        try
          val stream = Files.list(directory)
          try
            Right(
              stream.iterator.asScala.toVector
                .filter(p =>
                  val n = p.getFileName.toString
                  n.endsWith(".json") && !n.startsWith(".")
                )
                .sortBy(_.getFileName.toString)
            )
          finally stream.close()
        catch
          case NonFatal(e) =>
            Left(
              s"$directory could not be listed: ${Option(e.getMessage).getOrElse(e.toString)}"
            )
      val files = listed.getOrElse(Vector.empty)
      val read  = files.map(f =>
        (try decode[ImportPreset](Files.readString(f, UTF_8)).left.map(_.getMessage)
        catch case NonFatal(e) => Left(Option(e.getMessage).getOrElse(e.toString))).left
          .map(m => s"$f is not an import preset: $m")
      )
      val (presets, problems) = read.foldLeft((ImportPresets.empty, Vector.empty[String])) {
        case ((acc, errs), Right(p)) =>
          acc.add(p).fold(e => (acc, errs :+ e.message), next => (next, errs))
        case ((acc, errs), Left(e)) => (acc, errs :+ e)
      }
      (presets, listed.left.toOption.toVector ++ problems)

object FilePresetStore:
  /** The user's preset directory: `~/.eyes4s-studio/import-presets`. */
  def userDefault: FilePresetStore =
    FilePresetStore(
      Paths.get(sys.props.getOrElse("user.home", ".")).resolve(".eyes4s-studio/import-presets")
    )
