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
import eyes4s.studio.core.platform.{HostPath, PlatformError}
import eyes4s.studio.core.preferences.PreferencesStore

import java.nio.file.Paths

/** Where the desktop keeps the user's preferences file (ticket S2.8): the
  * platform's per-user application-data directory.
  *
  *  - macOS: `~/Library/Application Support/Eyes Studio/preferences.json`;
  *  - Windows: `%APPDATA%\Eyes Studio\preferences.json`;
  *  - elsewhere: `$XDG_CONFIG_HOME/eyes4s-studio/preferences.json`, with
  *    `~/.config` when the variable is unset.
  */
object PreferencesLocation:

  val FileName: String = "preferences.json"

  /** The file for an OS named `osName`, a home directory and an environment. */
  def of(
      osName: String,
      home: String,
      env: String => Option[String]
  ): Either[PlatformError, HostPath] =
    val os  = osName.toLowerCase
    val dir =
      if os.contains("mac") then
        Paths.get(home, "Library", "Application Support", "Eyes Studio")
      else if os.contains("windows") then
        env("APPDATA")
          .filter(_.trim.nonEmpty)
          .fold(Paths.get(home, "AppData", "Roaming", "Eyes Studio"))(
            Paths.get(_, "Eyes Studio")
          )
      else
        env("XDG_CONFIG_HOME")
          .filter(_.trim.nonEmpty)
          .fold(Paths.get(home, ".config", "eyes4s-studio"))(Paths.get(_, "eyes4s-studio"))
    HostPath.of(dir.resolve(FileName).toString)

  /** This JVM's file. */
  def default: Either[PlatformError, HostPath] =
    of(
      sys.props.getOrElse("os.name", ""),
      sys.props.getOrElse("user.home", "."),
      sys.env.get
    )

  /** The store of this JVM's file. */
  def store: Either[PlatformError, PreferencesStore[IO]] =
    default.map(PreferencesStore[IO](JvmFileSystem, _))
