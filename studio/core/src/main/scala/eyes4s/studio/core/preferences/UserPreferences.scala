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

package eyes4s.studio.core.preferences

import cats.Monad
import cats.syntax.all.*
import eyes4s.studio.core.document.{Perspective, SavedLayout, StageAppearance}
import eyes4s.studio.core.platform.{FileSystem, HostPath, PlatformError}
import io.circe.parser.parse
import io.circe.syntax.*
import io.circe.{Codec, Decoder, DecodingFailure, Encoder, Json}

import java.nio.charset.StandardCharsets.UTF_8

/** The appearance the user chose (DESIGN_SPEC section 4; S1.10): a fixed
  * theme, or the platform's. Following the system is a user preference
  * only: a document keeps the effective Light or Dark theme (lead's decision
  * on bead S2.8, 2026-10-04).
  */
enum AppearanceChoice derives CanEqual, Codec.AsObject:
  case Light, Dark, System

/** The projects the user opened last, most recent first: distinct, at most
  * [[RecentProjects.Max]].
  */
final case class RecentProjects private (paths: Vector[HostPath]) derives CanEqual:

  /** `path` opened now: first, and once. */
  def opened(path: HostPath): RecentProjects = RecentProjects.of(path +: paths)

  /** `path` no longer offered (it was moved or deleted). */
  def forget(path: HostPath): RecentProjects = new RecentProjects(paths.filterNot(_ == path))

object RecentProjects:
  val Max: Int = 10

  val empty: RecentProjects = new RecentProjects(Vector.empty)

  /** `paths` in order, repeats and those past [[Max]] dropped. */
  def of(paths: Vector[HostPath]): RecentProjects = new RecentProjects(paths.distinct.take(Max))

/** Per-user settings that outlive any project (ticket S2.8): the
  * appearance, the projects opened last, the stage surround a new project
  * starts with, the user's own layout of each perspective, and the
  * directory exports were last written to. Project settings live in the
  * document, never here. `layouts` holds at most one layout per
  * perspective, in perspective order.
  */
final case class UserPreferences private (
    appearance: AppearanceChoice,
    stage: StageAppearance,
    recent: RecentProjects,
    layouts: Vector[SavedLayout],
    exportDirectory: Option[HostPath]
) derives CanEqual:

  def withAppearance(a: AppearanceChoice): UserPreferences      = copy(appearance = a)
  def withStage(s: StageAppearance): UserPreferences            = copy(stage = s)
  def withRecent(r: RecentProjects): UserPreferences            = copy(recent = r)
  def withExportDirectory(d: Option[HostPath]): UserPreferences = copy(exportDirectory = d)

  /** `layout` as the user's layout of its perspective, replacing any other. */
  def withLayout(layout: SavedLayout): UserPreferences =
    copy(layouts =
      UserPreferences.ordered(layouts.filterNot(_.perspective == layout.perspective) :+ layout)
    )

  /** The user's layout of `perspective`, if one is saved. */
  def layout(perspective: Perspective): Option[SavedLayout] =
    layouts.find(_.perspective == perspective)

object UserPreferences:

  /** The schema version this build writes and reads. */
  val Version: Int = 1

  /** Light, a dark stage (the boards'), no recent projects, no user
    * layouts, no export directory.
    */
  val defaults: UserPreferences =
    new UserPreferences(
      AppearanceChoice.Light,
      StageAppearance.Dark,
      RecentProjects.empty,
      Vector.empty,
      None
    )

  private def ordered(layouts: Vector[SavedLayout]): Vector[SavedLayout] =
    layouts.sortBy(_.perspective.ordinal)

  /** Preferences with these fields: at most one layout per perspective. */
  def of(
      appearance: AppearanceChoice,
      stage: StageAppearance,
      recent: RecentProjects,
      layouts: Vector[SavedLayout],
      exportDirectory: Option[HostPath]
  ): Either[PreferencesError, UserPreferences] =
    val perspectives = layouts.map(_.perspective)
    val repeated     = perspectives.diff(perspectives.distinct).distinct
    Either.cond(
      repeated.isEmpty,
      new UserPreferences(appearance, stage, recent, ordered(layouts), exportDirectory),
      PreferencesError.DuplicateLayouts(repeated)
    )

  private given Codec[HostPath] = Codec.from(
    Decoder.decodeString.emap(HostPath.of(_).left.map(_.message)),
    Encoder.encodeString.contramap(_.value)
  )

  private val fields: Encoder.AsObject[UserPreferences] =
    Encoder.forProduct5("appearance", "stage", "recentProjects", "layouts", "exportDirectory")(
      p => (p.appearance, p.stage, p.recent.paths, p.layouts, p.exportDirectory)
    )

  private val decodeFields: Decoder[UserPreferences] = Decoder.instance { c =>
    for
      appearance <- c.get[AppearanceChoice]("appearance")
      stage      <- c.get[StageAppearance]("stage")
      recent     <- c.get[Vector[HostPath]]("recentProjects")
      layouts    <- c.get[Vector[SavedLayout]]("layouts")
      exportDir  <- c.get[Option[HostPath]]("exportDirectory")
      prefs      <- of(appearance, stage, RecentProjects.of(recent), layouts, exportDir).left
        .map(e => DecodingFailure(e.message, c.history))
    yield prefs
  }

  /** The file's text: `{"version": 1, "preferences": {…}}`. */
  def encode(p: UserPreferences): String =
    Json.obj("version" -> Version.asJson, "preferences" -> fields(p).asJson).spaces2

  /** The preferences a file's text holds. A version this build cannot read
    * is refused as such, never read as another.
    */
  def decode(text: String): Either[PreferencesError, UserPreferences] =
    for
      json    <- parse(text).left.map(e => PreferencesError.NotJson(e.message))
      version <- json.hcursor
        .get[Int]("version")
        .left
        .map(e => PreferencesError.Malformed(e.getMessage))
      _ <- Either.cond(
        version == Version,
        (),
        PreferencesError.UnknownVersion(version, Version)
      )
      prefs <- json.hcursor
        .downField("preferences")
        .as(using decodeFields)
        .left
        .map(e => PreferencesError.Malformed(e.getMessage))
    yield prefs

/** Why a preferences file's content was not used. Every case names what it
  * found.
  */
enum PreferencesError derives CanEqual:
  case NotJson(reason: String)
  case Malformed(reason: String)
  case UnknownVersion(found: Int, supported: Int)
  case DuplicateLayouts(perspectives: Vector[Perspective])

  def message: String = this match
    case NotJson(why)                => s"not JSON: $why"
    case Malformed(why)              => s"not preferences: $why"
    case UnknownVersion(found, ours) =>
      s"schema version $found, but this build reads version $ours"
    case DuplicateLayouts(ps) =>
      s"more than one layout for ${ps.map(_.label).mkString(", ")}"

/** What went wrong reading or writing the preferences file, for the log. The
  * app carries on with defaults (reading) or the preferences in memory
  * (writing).
  */
enum PreferencesProblem derives CanEqual:
  /** The file could not be read. */
  case Unreadable(path: HostPath, error: PlatformError)

  /** The file was read but its content refused; it was kept at `kept`, when
    * that copy could be written, so the next save does not destroy it.
    */
  case Corrupt(path: HostPath, error: PreferencesError, kept: Option[HostPath])

  case Unwritable(path: HostPath, error: PlatformError)

  def message: String = this match
    case Unreadable(p, e) =>
      s"Preferences ${p.value} could not be read (${e.message}); using defaults."
    case Corrupt(p, e, kept) =>
      s"Preferences ${p.value} are unusable (${e.message}); using defaults" +
        kept.fold(".")(k => s"; the file was kept as ${k.value}.")
    case Unwritable(p, e) => s"Preferences ${p.value} could not be saved: ${e.message}"

/** The user's preferences file (ticket S2.8) on a host's [[FileSystem]]: the
  * desktop's lives in Application Support, a web host's wherever its file
  * system puts `path`. Reading never fails: a missing file is the defaults,
  * and an unreadable or corrupt one is the defaults plus a
  * [[PreferencesProblem]] for the log; a corrupt file is first copied
  * beside itself with the suffix `.corrupt`.
  */
final class PreferencesStore[F[_]: Monad](files: FileSystem[F], val path: HostPath):

  /** The preferences, and what went wrong reading them. */
  def load: F[(UserPreferences, Vector[PreferencesProblem])] =
    files.read(path).flatMap {
      case Left(PlatformError.Missing(_)) => (UserPreferences.defaults, Vector.empty).pure[F]
      case Left(e)                        =>
        (UserPreferences.defaults, Vector(PreferencesProblem.Unreadable(path, e))).pure[F]
      case Right(bytes) =>
        UserPreferences.decode(String(IArray.genericWrapArray(bytes).toArray, UTF_8)) match
          case Right(p) => (p, Vector.empty).pure[F]
          case Left(e)  =>
            kept.fold(
              (UserPreferences.defaults, Vector(PreferencesProblem.Corrupt(path, e, None)))
                .pure[F]
            ) { copy =>
              files.write(copy, bytes).map { written =>
                (
                  UserPreferences.defaults,
                  Vector(PreferencesProblem.Corrupt(path, e, written.toOption.map(_ => copy)))
                )
              }
            }
    }

  /** Writes `p`, replacing the file. */
  def save(p: UserPreferences): F[Either[PreferencesProblem, Unit]] =
    files
      .write(path, IArray.unsafeFromArray(UserPreferences.encode(p).getBytes(UTF_8)))
      .map(_.left.map(PreferencesProblem.Unwritable(path, _)))

  /** Where a corrupt file is kept: `path` with `.corrupt` appended. */
  private def kept: Option[HostPath] = HostPath.of(path.value + ".corrupt").toOption
