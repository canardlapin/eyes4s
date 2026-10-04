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

package eyes4s.studio.desktop.figures

import cats.syntax.all.*
import eyes4s.io.csv
import eyes4s.studio.app.figures.{BundleItem, BundleRequest, FigureBundle}
import eyes4s.studio.core.backend.{QueryRow, ResultSummary}
import eyes4s.studio.core.figures.BundleTables

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{AtomicMoveNotSupportedException, Files, Path, StandardCopyOption}
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

/** The files of an export bundle (ticket S9.5), by name: the figure in its
  * format, the result tables as eyes4s CSV (RFC 4180, with the table digest
  * and an explicit `__valid` column after each nullable one), and the
  * methods text as shown. The project snapshot is a directory, written by
  * the project port.
  */
object BundleFiles:

  def assemble(
      request: BundleRequest,
      summary: ResultSummary,
      rows: Vector[QueryRow]
  ): Either[String, Vector[(String, IArray[Byte])]] =
    def utf8(text: String) = IArray.unsafeFromArray(text.getBytes(UTF_8))
    val readme             = "README.txt" -> utf8(FigureBundle.readme(request))
    request.items
      .filterNot(_ == BundleItem.Snapshot)
      .traverse { item =>
        val name = FigureBundle.file(item, request.page, request.format)
        val bytes: Either[String, IArray[Byte]] = item match
          case BundleItem.Figure  => FigureExport.render(request.format, request.page)
          case BundleItem.Results =>
            BundleTables
              .results(request.source, summary, rows)
              .bimap(_.message, t => utf8(t.csv.encode))
          case BundleItem.Participants =>
            BundleTables
              .participants(request.source, summary)
              .bimap(_.message, t => utf8(t.csv.encode))
          case BundleItem.Methods =>
            request.methods
              .map(m => utf8(m + "\n"))
              .toRight("the methods text is not generated")
          case BundleItem.Comparisons => Left(FigureBundle.NoPairRows)
          case BundleItem.Snapshot    => Left("the project snapshot is a directory")
        bytes.bimap(why => s"$name: $why", name -> _)
      }
      .map(_ :+ readme)

/** Writes a bundle so that it appears whole or not at all (ticket S9.5):
  * into a hidden partial folder beside `target`, with the project snapshot
  * if there is one, then moved into place. On any failure the partial folder
  * is removed and `target` never appears.
  */
object BundleWriter:

  /** Copies a project snapshot into a directory, answering once. */
  type Snapshot = (Path, Either[String, Unit] => Unit) => Unit

  def write(
      target: Path,
      files: Vector[(String, IArray[Byte])],
      snapshot: Option[Snapshot],
      done: Either[String, String] => Unit
  ): Unit =
    val partial                 = target.resolveSibling(s".${target.getFileName}.partial")
    def fail(why: String): Unit =
      remove(partial)
      done(Left(why))
    def finish(): Unit =
      try
        try Files.move(partial, target, StandardCopyOption.ATOMIC_MOVE): Unit
        catch case _: AtomicMoveNotSupportedException => Files.move(partial, target): Unit
        done(Right(target.toString))
      catch case NonFatal(e) => fail(reason(e))
    if Files.exists(target) then done(Left(s"$target already exists; choose another folder"))
    else
      val written =
        try
          remove(partial)
          Files.createDirectories(partial)
          files.foreach((name, bytes) => Files.write(partial.resolve(name), Array.from(bytes)))
          Right(())
        catch case NonFatal(e) => Left(reason(e))
      written match
        case Left(why) => fail(why)
        case Right(()) =>
          snapshot match
            case None       => finish()
            case Some(take) =>
              take(
                partial.resolve("project"),
                {
                  case Left(why) => fail(s"project snapshot: $why")
                  case Right(()) => finish()
                }
              )

  /** Removes `dir` and everything under it, if it exists. */
  private def remove(dir: Path): Unit =
    if Files.exists(dir) then
      val all = Files.walk(dir)
      try all.iterator.asScala.toVector.reverse.foreach(Files.delete)
      finally all.close()

  private def reason(e: Throwable): String = Option(e.getMessage).getOrElse(e.toString)
