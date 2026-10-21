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

package eyes4s.studio.app.data

import eyes4s.studio.core.document.{Preset, SourceRole}
import eyes4s.studio.core.platform.HostPath

/** Files chosen at first run. Units, geometry and mappings still require confirmation. */
final case class FirstRunImport(
    files: Vector[(SourceRole, HostPath)] = Vector.empty,
    choose: Option[SourceRole] = None,
    preset: Preset = Preset.EncodingRetrieval,
    imageFolder: Option[HostPath] = None,
    chooseImages: Boolean = false
) derives CanEqual

/** Native file metadata; opaque host paths are not parsed by the app. */
final case class FirstRunFile(path: HostPath, name: String, directory: Boolean) derives CanEqual

enum FirstRunDropProblem derives CanEqual:
  case Empty
  case Unsupported(names: Vector[String])
  case SeveralFolders(names: Vector[String])
  case RepeatedRole(role: SourceRole, names: Vector[String])

  def message: String = this match
    case Empty                     => "The drop contains no files."
    case Unsupported(names)        => s"Unsupported source files: ${names.mkString(", ")}."
    case SeveralFolders(names)     => s"Choose one stimulus folder: ${names.mkString(", ")}."
    case RepeatedRole(role, names) =>
      s"Choose one ${role.label} table: ${names.mkString(", ")}."

object FirstRunImport:
  /** Admit the whole drop, never pick one source and discard another. */
  def dropped(files: Vector[FirstRunFile]): Either[FirstRunDropProblem, FirstRunImport] =
    def asciiLower(name: String): String = name.map { c =>
      if c >= 'A' && c <= 'Z' then (c + ('a' - 'A')).toChar else c
    }
    val directories = files.filter(_.directory)
    val tables      = files.filterNot(_.directory)
    val unsupported =
      tables.filterNot(f => Vector(".csv", ".tsv").exists(asciiLower(f.name).endsWith))
    val roles = tables.map(f =>
      (if asciiLower(f.name).startsWith("trials") then SourceRole.Trials
       else SourceRole.Fixations) -> f
    )
    val repeated = SourceRole.values.toVector.flatMap { role =>
      val names = roles.collect { case (`role`, file) => file.name }
      Option.when(names.size > 1)(FirstRunDropProblem.RepeatedRole(role, names))
    }.headOption
    if files.isEmpty then Left(FirstRunDropProblem.Empty)
    else if unsupported.nonEmpty then
      Left(FirstRunDropProblem.Unsupported(unsupported.map(_.name)))
    else if directories.size > 1 then
      Left(FirstRunDropProblem.SeveralFolders(directories.map(_.name)))
    else
      repeated match
        case Some(problem) => Left(problem)
        case None          =>
          Right(
            FirstRunImport(
              roles.map((role, file) => role -> file.path),
              imageFolder = directories.headOption.map(_.path)
            )
          )
