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

package eyes4s.studio.core.platform

import eyes4s.studio.core.bundle.ProjectStore
import fs2.Stream

import scala.concurrent.duration.FiniteDuration

/** A location in the host's own namespace (ticket S0.9): a file-system path on
  * the desktop, a file handle's id in a browser. Studio never parses it; only
  * the host's [[FileSystem]] gives it meaning.
  */
final case class HostPath private (value: String) derives CanEqual

object HostPath:
  def of(value: String): Either[PlatformError, HostPath] =
    if value.trim.isEmpty then Left(PlatformError.BlankPath)
    else
      value.indexWhere(c => c < ' ' || c == '\u007f') match
        case -1    => Right(new HostPath(value))
        case index => Left(PlatformError.ControlCharacter(value.length, index))

  /** Whether `name` names a direct child: not blank, `.` or `..`, and free of
    * separators and control characters. Every [[FileSystem.child]] applies it.
    */
  def validName(name: String): Boolean =
    name.nonEmpty && name != "." && name != ".." &&
      !name.exists(c => c == '/' || c == '\\' || c < ' ' || c == '\u007f')

/** One kind of file a dialog offers, such as "EyeLink fixation CSV" with the
  * extensions `csv` and `tsv`. Extensions are given without a leading dot.
  */
final case class FileKind private (description: String, extensions: Vector[String])
    derives CanEqual

object FileKind:
  def of(description: String, extensions: Vector[String]): Either[PlatformError, FileKind] =
    val bad = extensions.find(e => e.isEmpty || e.exists(c => c == '.' || c == '*' || c == '/'))
    if description.trim.isEmpty || extensions.isEmpty || bad.nonEmpty then
      Left(PlatformError.InvalidFileKind(description, extensions))
    else Right(new FileKind(description, extensions))

/** What an open or save dialog asks for. `suggestedName` is the file name a
  * save dialog proposes; an open dialog ignores it.
  */
final case class FileRequest(
    title: String,
    kinds: Vector[FileKind],
    suggestedName: Option[String]
) derives CanEqual

/** A web or mail address to hand to the host's browser or mail client. Only
  * `https:`, `http:` and `mailto:` addresses are accepted: studio never asks
  * the host to open an arbitrary scheme.
  */
final case class ExternalUrl private (value: String) derives CanEqual

object ExternalUrl:
  val Schemes: Vector[String] = Vector("https://", "http://", "mailto:")

  def of(value: String): Either[PlatformError, ExternalUrl] =
    val lower = value.toLowerCase
    Schemes.find(lower.startsWith) match
      case Some(s) if value.length > s.length && !value.exists(_.isWhitespace) =>
        Right(new ExternalUrl(value))
      case _ => Left(PlatformError.InvalidUrl(value))

/** What [[ExternalOpen]] shows outside studio. */
enum ExternalTarget derives CanEqual:
  /** A web page or mail address, in the host's default application. */
  case Url(url: ExternalUrl)

  /** A file or bundle, revealed in the host's file browser (File › Reveal). */
  case Reveal(path: HostPath)

  def render: String = this match
    case Url(u)    => u.value
    case Reveal(p) => p.value

/** A preference's name: lower-case letters, digits, `.` and `-`, at most 80
  * characters (the limit of the most restrictive host store, Java's).
  */
final case class PreferenceKey private (value: String) derives CanEqual

object PreferenceKey:
  val MaxLength: Int = 80

  def of(value: String): Either[PlatformError, PreferenceKey] =
    val allowed = (c: Char) =>
      (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9') || c == '.' ||
        c == '-'
    if value.nonEmpty && value.length <= MaxLength && value.forall(allowed) then
      Right(new PreferenceKey(value))
    else Left(PlatformError.InvalidPreferenceKey(value))

/** A bundled font face the host must register before the first frame:
  * `resource` is its JVM classpath resource (studio-desktop bundles the faces
  * under `eyes4s/studio/desktop/fonts/`; a web host maps it to the URL it serves the
  * file at) and `family` the family name stylesheets use for it.
  */
final case class FontRequest private (family: String, resource: String) derives CanEqual

object FontRequest:
  def of(family: String, resource: String): Either[PlatformError, FontRequest] =
    if family.trim.isEmpty || resource.trim.isEmpty then
      Left(PlatformError.InvalidFont(family, resource))
    else Right(new FontRequest(family, resource))

/** A wall-clock reading: milliseconds since the Unix epoch and the host's UTC
  * offset at that instant, so studio can show local times without a time-zone
  * database (which Scala.js does not ship).
  */
final case class ClockReading private (epochMillis: Long, utcOffsetMinutes: Int)
    derives CanEqual:
  private def localMinutes: Long =
    Math.floorMod(Math.floorDiv(epochMillis, 60000L) + utcOffsetMinutes, 24L * 60L)

  /** The local hour, 0 to 23. */
  def localHour: Int = (localMinutes / 60L).toInt

  /** The local minute, 0 to 59. */
  def localMinute: Int = (localMinutes % 60L).toInt

object ClockReading:
  /** UTC offsets in use lie within ±18 hours. */
  val MaxOffsetMinutes: Int = 18 * 60

  /** A reading at UTC, whose offset is always valid. */
  def utc(epochMillis: Long): ClockReading = new ClockReading(epochMillis, 0)

  def of(epochMillis: Long, utcOffsetMinutes: Int): Either[PlatformError, ClockReading] =
    if Math.abs(utcOffsetMinutes) > MaxOffsetMinutes then
      Left(PlatformError.InvalidOffset(utcOffsetMinutes))
    else Right(new ClockReading(epochMillis, utcOffsetMinutes))

/** Why a platform service refused. Every case names its operands. */
enum PlatformError derives CanEqual:
  case BlankPath

  /** A control character (such as NUL) at `index` of a path `length` long. */
  case ControlCharacter(length: Int, index: Int)

  /** A path the host's file system cannot represent (a Windows-reserved
    * character, say).
    */
  case InvalidPath(path: HostPath, reason: String)
  case InvalidFileKind(description: String, extensions: Vector[String])
  case InvalidUrl(text: String)
  case InvalidPreferenceKey(text: String)
  case InvalidFont(family: String, resource: String)
  case InvalidOffset(minutes: Int)

  /** A child name that is blank, a separator, `.` or `..`. */
  case InvalidName(directory: HostPath, name: String)
  case Missing(path: HostPath)
  case Unreadable(path: HostPath, reason: String)
  case Unwritable(path: HostPath, reason: String)

  /** The host does not offer this operation at all (a browser that may not
    * read the clipboard, say); see [[HostCapabilities]].
    */
  case Unsupported(service: String, operation: String)
  case FontRejected(font: FontRequest, reason: String)
  case OpenFailed(target: ExternalTarget, reason: String)
  case PreferenceFailed(key: PreferenceKey, reason: String)

  def message: String = this match
    case BlankPath                       => "A host path may not be blank."
    case ControlCharacter(length, index) =>
      s"A host path may not hold a control character (index $index of $length)."
    case InvalidPath(path, reason)      => s"${path.value} is not a path on this host: $reason."
    case InvalidFileKind(d, extensions) =>
      s"File kind '$d' with extensions [${extensions.mkString(", ")}] needs a description " +
        "and at least one extension without '.', '*' or '/'."
    case InvalidUrl(text) =>
      s"'$text' is not an address studio opens (${ExternalUrl.Schemes.mkString(", ")})."
    case InvalidPreferenceKey(text) =>
      s"'$text' is not a preference key (a-z, 0-9, '.', '-'; at most ${PreferenceKey.MaxLength})."
    case InvalidFont(family, resource) =>
      s"Font family '$family' at '$resource' needs a family and a resource."
    case InvalidOffset(minutes)   => s"A UTC offset of $minutes minutes is out of range."
    case InvalidName(dir, name)   => s"'$name' is not a name inside ${dir.value}."
    case Missing(path)            => s"${path.value} does not exist."
    case Unreadable(path, reason) => s"Cannot read ${path.value}: $reason."
    case Unwritable(path, reason) => s"Cannot write ${path.value}: $reason."
    case Unsupported(service, op) => s"This host's $service does not support $op."
    case FontRejected(font, why)  => s"Font ${font.family} (${font.resource}) not loaded: $why."
    case OpenFailed(target, why)  => s"Cannot open ${target.render}: $why."
    case PreferenceFailed(key, why) => s"Preference ${key.value}: $why."

/** The host's user files: import sources, export targets, project bundles
  * (DESIGN_SPEC section 13). Distinct from a bundle's own storage, which is a
  * [[ProjectStore]] this service opens.
  */
trait FileSystem[F[_]]:

  /** The location of `name` inside `directory`; pure, touches nothing. */
  def child(directory: HostPath, name: String): Either[PlatformError, HostPath]

  /** The whole file. A directory is [[PlatformError.Unreadable]]. */
  def read(path: HostPath): F[Either[PlatformError, IArray[Byte]]]

  /** The file as a stream of chunks of at most `chunkSize` bytes, for sources
    * too large to hold whole (EyeLink ASC files). Opening is checked first and
    * refused as a value; a read failing mid-stream raises a [[PlatformFailure]].
    */
  def readStream(path: HostPath, chunkSize: Int): F[Either[PlatformError, Stream[F, Byte]]]

  /** Replace the file's bytes, creating it and its parent directories if
    * needed. A directory, the root included, is [[PlatformError.Unwritable]].
    */
  def write(path: HostPath, bytes: IArray[Byte]): F[Either[PlatformError, Unit]]

  /** The directory's direct children, in the order of their `value`. A file
    * is [[PlatformError.Unreadable]]; a path that does not exist is
    * [[PlatformError.Missing]].
    */
  def list(directory: HostPath): F[Either[PlatformError, Vector[HostPath]]]

  /** The bundle at `path` as a [[ProjectStore]]; two calls on one path are
    * two views of the same bundle. A host without local files
    * (`HostCapabilities.localFiles` false) answers
    * [[PlatformError.Unsupported]].
    */
  def project(path: HostPath): F[Either[PlatformError, ProjectStore[F]]]

/** File choosers. `None` is the user cancelling, never an error. */
trait Dialogs[F[_]]:
  def chooseOpen(request: FileRequest): F[Option[HostPath]]
  def chooseSave(request: FileRequest): F[Option[HostPath]]

  /** A directory, such as an `.eyes` project bundle, under `title`. */
  def chooseDirectory(title: String): F[Option[HostPath]]

/** A platform error raised in a stream that has no other way to report it
  * ([[FileSystem.readStream]] after opening).
  */
final case class PlatformFailure(error: PlatformError) extends RuntimeException(error.message)

/** The system clipboard, as plain text. */
trait Clipboard[F[_]]:

  /** The clipboard's text, `None` when it holds none. */
  def readText: F[Either[PlatformError, Option[String]]]
  def writeText(text: String): F[Either[PlatformError, Unit]]

/** Registration of studio's bundled fonts with the host's renderer. */
trait Fonts[F[_]]:
  def register(font: FontRequest): F[Either[PlatformError, Unit]]

/** A scheduled task; cancelling after it ran does nothing. */
trait Scheduled[F[_]]:
  def cancel: F[Unit]

/** The wall clock and delayed work (autosave debounce, the status bar's
  * clock). Tasks run off the UI thread; a shell moves their results to it.
  */
trait Scheduler[F[_]]:
  def now: F[ClockReading]

  /** Run `task` once, `delay` from now, unless cancelled first. */
  def after(delay: FiniteDuration)(task: F[Unit]): F[Scheduled[F]]

/** Hands a target to the host's browser, mail client or file browser. */
trait ExternalOpen[F[_]]:
  def open(target: ExternalTarget): F[Either[PlatformError, Unit]]

/** Per-user preferences that outlive a project (recent projects, window
  * placement, theme). Project settings live in the document, never here.
  */
trait Preferences[F[_]]:
  def get(key: PreferenceKey): F[Either[PlatformError, Option[String]]]
  def put(key: PreferenceKey, value: String): F[Either[PlatformError, Unit]]
  def remove(key: PreferenceKey): F[Either[PlatformError, Unit]]

/** What the host's window system offers, so the app can choose a variant
  * rather than fail: a native menu bar (macOS) or an in-window one, several
  * windows or one, a local file system, clipboard reads.
  */
final case class HostCapabilities(
    nativeMenuBar: Boolean,
    multipleWindows: Boolean,
    localFiles: Boolean,
    clipboardRead: Boolean
) derives CanEqual

/** Every platform service a shell provides (DESIGN_SPEC section 13, S0.9;
  * docs/studio/PORTING.md). studio-core and studio-app reach the host only
  * through these; the JVM implementations live in studio-desktop and
  * [[InMemoryPlatform]] is the portable one.
  */
final case class Platform[F[_]](
    capabilities: HostCapabilities,
    files: FileSystem[F],
    dialogs: Dialogs[F],
    clipboard: Clipboard[F],
    fonts: Fonts[F],
    scheduler: Scheduler[F],
    external: ExternalOpen[F],
    preferences: Preferences[F]
)

object Scheduler:
  import cats.effect.Temporal
  import cats.syntax.all.*

  /** A scheduler on the effect's own timer. `offset` gives the host's UTC
    * offset, in minutes, at an epoch millisecond: studio-desktop reads the
    * JVM's zone rules, a browser `Date.getTimezoneOffset`.
    */
  def temporal[F[_]: Temporal](offset: Long => Int): Scheduler[F] = new Scheduler[F]:
    def now: F[ClockReading] =
      Temporal[F].realTime.flatMap { t =>
        val millis = t.toMillis
        Temporal[F].fromEither(
          ClockReading
            .of(millis, offset(millis))
            .leftMap(e => new IllegalStateException(e.message))
        )
      }

    def after(delay: FiniteDuration)(task: F[Unit]): F[Scheduled[F]] =
      Temporal[F].start(Temporal[F].sleep(delay) >> task).map { fiber =>
        new Scheduled[F]:
          def cancel: F[Unit] = fiber.cancel
      }
