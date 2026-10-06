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

package eyes4s.studio.desktop.runtime

import cats.effect.IO
import cats.syntax.all.*
import eyes4s.codec.ByteDigest
import eyes4s.studio.core.assets.{AssetFile, AssetRef, AssetRegistry}
import eyes4s.studio.core.document.{DatasetRevisionSpec, Source, SourceRole}
import eyes4s.studio.core.real.DatasetSources
import eyes4s.studio.core.fixture.GoldenAssets
import eyes4s.studio.app.explore.DisplaySource
import eyes4s.studio.desktop.explore.NavigatorDisplays
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** Native source ports owned by the desktop. Portable backend code sees only bytes and assets. */
object DatasetSourceHosts:
  def stored(project: ProjectPort): DatasetSources[IO] = new DatasetSources[IO]:
    def bytes(dataset: DatasetRevisionSpec, source: Source): IO[Option[IArray[Byte]]] =
      IO.async_[Option[IArray[Byte]]](done =>
        project.readInput(source, value => done(Right(value.toOption)))
      )
    def assets(dataset: DatasetRevisionSpec): IO[Option[AssetRegistry]] =
      IO.async_[Option[AssetRegistry]](done =>
        NavigatorDisplays
          .stored(project)
          .read(
            dataset,
            value =>
              done(Right(value.toOption.flatMap {
                case DisplaySource.Served(registry) => Some(registry)
                case DisplaySource.NotServed        => None
              }))
          )
      )

  /** The bundled, redistributable memory-study inputs at their fixture location.
    * Source digests are still verified by the native backend; missing files stay missing.
    */
  def golden(root: Path): DatasetSources[IO] = new DatasetSources[IO]:
    private def file(source: Source): Path = source.role match
      case SourceRole.Fixations => root.resolve("fixations.csv")
      case SourceRole.Trials    => root.resolve("trials.csv")
    private def read(path: Path): IO[Option[IArray[Byte]]] = IO
      .blocking {
        Option.when(Files.isRegularFile(path))(IArray.from(Files.readAllBytes(path)))
      }
      .handleError(_ => None)
    def bytes(dataset: DatasetRevisionSpec, source: Source): IO[Option[IArray[Byte]]] = read(
      file(source)
    )
    def assets(dataset: DatasetRevisionSpec): IO[Option[AssetRegistry]] =
      dataset.sources.trials match
        case None         => IO.pure(None)
        case Some(source) =>
          val images = root.resolve("stimuli")
          val files  = IO.blocking {
            if !Files.isDirectory(images) then Vector.empty[Path]
            else
              val stream = Files.list(images)
              try
                stream.iterator.asScala
                  .filter(Files.isRegularFile(_))
                  .toVector
                  .sortBy(_.getFileName.toString)
              finally stream.close()
          }
          (
            bytes(dataset, source),
            files.flatMap(_.traverse { path =>
              read(path).map(
                _.flatMap(data =>
                  AssetFile
                    .of(path.getFileName.toString)
                    .toOption
                    .map(name => AssetRef(name, ByteDigest.sha256(data)))
                )
              )
            })
          ).mapN { (inventory, references) =>
            inventory.flatMap { data =>
              if ByteDigest.sha256(data) != source.bytes then None
              else if GoldenAssets.describes(dataset) then
                // This explicitly selected example has build-time display rows
                // bound to these exact inventory bytes. Image availability and
                // digests still come from the files the host actually read.
                GoldenAssets.rows.toOption.flatMap(rows =>
                  AssetRegistry.fromRows(dataset, rows, references.flatten).toOption
                )
              else
                AssetRegistry
                  .fromInventory(dataset, data, references.flatten, Vector.empty)
                  .toOption
            }
          }
