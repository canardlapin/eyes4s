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

package eyes4s.studio.desktop.importing

import cats.effect.IO
import cats.syntax.all.*
import eyes4s.codec.ByteDigest
import eyes4s.studio.app.text.{ImportText, ImportTextId}
import eyes4s.studio.core.bundle.InputKind
import eyes4s.studio.core.document.{Source, SourceRole}
import eyes4s.studio.core.importing.{ImportPreset, ImportPresets, StreamedSource}
import eyes4s.studio.core.platform.{
  FileKind,
  FileRequest,
  FileSystem,
  HostPath,
  Platform as HostPlatform,
  PlatformError
}
import eyes4s.studio.desktop.platform.{DesktopPlatform, PlatformPresetStore}
import eyes4s.studio.desktop.runtime.ProjectPort

/** Selection metadata is supplied by the host that gives its opaque path meaning. */
final case class ChosenSource(path: HostPath, name: String)

/** Import services through the existing portable host seams; the wizard runs these asynchronously. */
final class PlatformImport(
    platform: HostPlatform[IO],
    presets: Either[PlatformError, PlatformPresetStore[IO]],
    project: Option[ProjectPort],
    displayName: HostPath => Either[PlatformError, String] = DesktopPlatform.fileName
) extends ImportPlatform:
  override def files: FileSystem[IO]                              = platform.files
  override def sourceName(path: HostPath): Either[String, String] =
    displayName(path).left.map(_.message)

  def chooseFile(role: SourceRole): IO[Either[String, Option[ChosenSource]]] =
    val title = ImportText(role match
      case SourceRole.Fixations => ImportTextId.DialogFixations
      case SourceRole.Trials    => ImportTextId.DialogTrials)
    FileKind.of(ImportText(ImportTextId.DialogFilter), Vector("csv", "tsv", "txt")) match
      case Left(e)     => IO.pure(Left(e.message))
      case Right(kind) =>
        platform.dialogs
          .chooseOpen(FileRequest(title, Vector(kind), None))
          .map(_.traverse(p => sourceName(p).map(ChosenSource(p, _))))

  def storePreset(preset: ImportPreset): IO[Either[String, Unit]] =
    presets.fold(e => IO.pure(Left(e.message)), _.save(preset))
  override def loadPresets: IO[(ImportPresets, Vector[String])] =
    presets.fold(e => IO.pure((ImportPresets.empty, Vector(e.message))), _.load)

  def importInput(source: Source, path: HostPath): IO[Either[String, Unit]] =
    val name = source.path.value.split('/').last
    project match
      case None       => IO.pure(Left(s"$name: no project is open to store it"))
      case Some(port) =>
        files.readStream(path, StreamedSource.ChunkBytes).flatMap {
          case Left(e)       => IO.pure(Left(e.message))
          case Right(stream) =>
            StreamedSource.bytes[IO](path.value, stream).flatMap {
              case Left(e) => IO.pure(Left(e.message))
              case Right(bytes) if ByteDigest.sha256(bytes) != source.bytes =>
                IO.pure(Left(s"${path.value} changed after it was read; read it again"))
              case Right(bytes) =>
                IO.async_[Either[String, Unit]] { done =>
                  port.importInput(
                    InputKind.Source(source.role),
                    name,
                    bytes,
                    r => done(Right(r))
                  )
                }
            }
        }
