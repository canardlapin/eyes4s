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

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import eyes4s.studio.core.importing.{ImportPreset, ImportPresets}
import eyes4s.studio.core.platform.{FileSystem, HostPath, PlatformError}

import java.nio.file.Path

/** Compatibility facade for legacy JVM callers; persistence belongs to the platform seam. */
final class FilePresetStore(val directory: Path):
  def on(files: FileSystem[IO]): Either[PlatformError, PlatformPresetStore[IO]] =
    HostPath.of(directory.toString).map(PlatformPresetStore(files, _, DesktopPlatform.fileName))

  /** Non-UI compatibility methods; wizard loads and saves use the effectful store. */
  def save(preset: ImportPreset): Either[String, Unit] =
    on(JvmFileSystem).left.map(_.message).flatMap(_.save(preset).unsafeRunSync())
  def load: (ImportPresets, Vector[String]) =
    on(JvmFileSystem).fold(e => (ImportPresets.empty, Vector(e.message)), _.load.unsafeRunSync())

object FilePresetStore:
  /** Keep the existing per-user directory and readable legacy files. */
  def userDefault: FilePresetStore = FilePresetStore(DesktopPlatform.importPresetDirectory)
