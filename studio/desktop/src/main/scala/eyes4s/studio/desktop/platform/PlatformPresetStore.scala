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

import cats.effect.Sync
import cats.syntax.all.*
import eyes4s.studio.core.importing.{ImportPreset, ImportPresets, SniffedSource}
import eyes4s.studio.core.platform.{FileSystem, HostPath, PlatformError}
import io.circe.parser.decode
import io.circe.syntax.*

import java.nio.charset.StandardCharsets.UTF_8

/** Per-user preset files through the host seam. Independent names remain independent writes. */
final class PlatformPresetStore[F[_]: Sync](
    files: FileSystem[F],
    val directory: HostPath,
    displayName: HostPath => Either[PlatformError, String]
):
  private def fileOf(preset: ImportPreset): Either[PlatformError, HostPath] =
    val hex = preset.name.value.getBytes(UTF_8).map(b => f"${b & 0xff}%02x").mkString
    files.child(directory, s"$hex.json")

  def save(preset: ImportPreset): F[Either[String, Unit]] =
    fileOf(preset).fold(
      e => Sync[F].pure(Left(e.message)),
      target => files.write(target, IArray.from(preset.asJson.spaces2.getBytes(UTF_8)))
        .map(_.left.map(_.message))
    )

  /** Failed entries remain named errors; missing first-use directories are empty. */
  def load: F[(ImportPresets, Vector[String])] =
    files.list(directory).flatMap {
      case Left(_: PlatformError.Missing) => Sync[F].pure((ImportPresets.empty, Vector.empty))
      case Left(e) => Sync[F].pure((ImportPresets.empty, Vector(e.message)))
      case Right(listed) =>
        listed.sortBy(_.value).traverse { path =>
          val read: F[Option[Either[String, ImportPreset]]] = displayName(path) match
            case Left(e) => Sync[F].pure(Some(Left(e.message)))
            case Right(name) if !name.endsWith(".json") || name.startsWith(".") =>
              Sync[F].pure(None)
            case Right(_) => files.read(path).map(_.left.map(_.message).flatMap { bytes =>
              SniffedSource.decodeUtf8(path.value, bytes).left.map(_.message)
                .flatMap(text => decode[ImportPreset](text.toString).left.map(_.getMessage))
            }.left.map(e => s"${path.value} is not an import preset: $e")).map(Some(_))
          read
        }.map(_.flatten.foldLeft((ImportPresets.empty, Vector.empty[String])) {
          case ((presets, errors), Right(p)) =>
            presets.add(p).fold(e => (presets, errors :+ e.message), next => (next, errors))
          case ((presets, errors), Left(e)) => (presets, errors :+ e)
        })
    }
