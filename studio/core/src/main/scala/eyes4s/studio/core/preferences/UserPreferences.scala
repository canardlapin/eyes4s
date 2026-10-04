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

  /** `path` no longer offered (it was moved or deleted). S2.9's project
    * lifecycle calls it when an open finds a recent project gone.
    */
  def forget(path: HostPath): RecentProjects =
    new RecentProjects(paths.filterNot(_ == RecentProjects.normalise(path)))

object RecentProjects:
  val Max: Int = 10

  val empty: RecentProjects = new RecentProjects(Vector.empty)

  /** `paths` in order, each normalised (no trailing separator), repeats and
    * those past [[Max]] dropped. Symbolic links and case-insensitive file
    * systems are not resolved: that needs the host (S2.9).
    */
  def of(paths: Vector[HostPath]): RecentProjects =
    new RecentProjects(paths.map(normalise).distinct.take(Max))

  /** `path` without trailing separators, the root kept as it is. */
  def normalise(path: HostPath): HostPath =
    val trimmed = path.value.reverse.dropWhile(c => c == '/' || c == '\\').reverse
    if trimmed.isEmpty || trimmed == path.value then path
    else HostPath.of(trimmed).getOrElse(path)

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

  /** The file was read but its content refused. It stays as it is; a save
    * copies it beside itself first ([[PreferencesStore.save]]).
    */
  case Corrupt(path: HostPath, error: PreferencesError)

  case Unwritable(path: HostPath, error: PlatformError)

  /** A save would have replaced a file that cannot be read now, so it was
    * not written: what the file holds is kept.
    */
  case NotOverwritten(path: HostPath, error: PlatformError)

  /** A save would have replaced an unusable file, and the copy of it could
    * not be written, so the save was not made.
    */
  case NotKept(path: HostPath, copy: HostPath, error: PlatformError)

  def message: String = this match
    case Unreadable(p, e) =>
      s"Preferences ${p.value} could not be read (${e.message}); using defaults."
    case Corrupt(p, e) =>
      s"Preferences ${p.value} are unusable (${e.message}); using defaults. The file is " +
        "kept, and copied beside itself before it is ever replaced."
    case Unwritable(p, e)     => s"Preferences ${p.value} could not be saved: ${e.message}"
    case NotOverwritten(p, e) =>
      s"Preferences ${p.value} were not saved: the file cannot be read (${e.message}), so " +
        "it is kept as it is."
    case NotKept(p, c, e) =>
      s"Preferences ${p.value} were not saved: the unusable file could not be copied to " +
        s"${c.value} (${e.message})."

/** The user's preferences file (ticket S2.8) on a host's [[FileSystem]]: the
  * desktop's lives in Application Support, a web host's wherever its file
  * system puts `path`.
  *
  * Reading never fails: a missing file is the defaults, and an unreadable,
  * corrupt or newer-version one is the defaults plus a
  * [[PreferencesProblem]] for the log, the file left as it is. A save never
  * destroys a file it cannot use: an unusable file is first copied beside
  * itself as `<file>.corrupt-<stamp>`, `stamp` being the save's time in
  * epoch milliseconds (so successive copies never replace one another), and
  * a file that cannot be read at all is not replaced.
  */
final class PreferencesStore[F[_]: Monad](
    files: FileSystem[F],
    val path: HostPath,
    stamp: F[Long]
):

  private def text(bytes: IArray[Byte]): String =
    String(IArray.genericWrapArray(bytes).toArray, UTF_8)

  /** The preferences, and what went wrong reading them. */
  def load: F[(UserPreferences, Vector[PreferencesProblem])] =
    files.read(path).map {
      case Left(PlatformError.Missing(_)) => (UserPreferences.defaults, Vector.empty)
      case Left(e) => (UserPreferences.defaults, Vector(PreferencesProblem.Unreadable(path, e)))
      case Right(bytes) =>
        UserPreferences.decode(text(bytes)) match
          case Right(p) => (p, Vector.empty)
          case Left(e)  =>
            (UserPreferences.defaults, Vector(PreferencesProblem.Corrupt(path, e)))
    }

  /** Writes `p`, replacing the file, but never destroying one it cannot
    * use: an unusable file is copied aside first, an unreadable one is not
    * replaced. `Right` names the copy made, if one was.
    */
  def save(p: UserPreferences): F[Either[PreferencesProblem, Option[HostPath]]] =
    val bytes = IArray.unsafeFromArray(UserPreferences.encode(p).getBytes(UTF_8))
    def write(kept: Option[HostPath]): F[Either[PreferencesProblem, Option[HostPath]]] =
      files
        .write(path, bytes)
        .map(_.left.map(PreferencesProblem.Unwritable(path, _)).map(_ => kept))
    files.read(path).flatMap {
      case Left(PlatformError.Missing(_)) => write(None)
      case Left(e) => Monad[F].pure(Left(PreferencesProblem.NotOverwritten(path, e)))
      case Right(old) if UserPreferences.decode(text(old)).isRight => write(None)
      case Right(old)                                              =>
        stamp.flatMap { now =>
          HostPath.of(s"${path.value}.corrupt-$now") match
            case Left(e)     => Monad[F].pure(Left(PreferencesProblem.NotKept(path, path, e)))
            case Right(copy) =>
              files.write(copy, old).flatMap {
                case Left(e)  => Monad[F].pure(Left(PreferencesProblem.NotKept(path, copy, e)))
                case Right(_) => write(Some(copy))
              }
        }
    }
