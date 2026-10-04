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
