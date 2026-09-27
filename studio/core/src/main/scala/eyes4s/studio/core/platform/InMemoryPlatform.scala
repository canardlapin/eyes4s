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

import cats.effect.{Ref, Sync}
import cats.syntax.all.*
import eyes4s.studio.core.bundle.{InMemoryProjectStore, ProjectStore}

import scala.concurrent.duration.*

/** The portable [[Platform]] (ticket S0.9): every service held in memory, for
  * headless tests and as the reference a new host's services are checked
  * against (`PlatformConformance`). Its controls script what a user or the
  * passage of time would do.
  *
  *  - Files live under `/`-separated paths; writing a file makes its parent
  *    directories exist.
  *  - Dialogs answer from [[answer]], one answer per dialog, `None` (cancel)
  *    when none is queued, and record every request.
  *  - The clock is virtual: it starts at `start` and moves only by
  *    [[advance]], which runs every task that falls due, in due order.
  *  - External opens and font registrations are recorded.
  */
final class InMemoryPlatform[F[_]: Sync] private (state: Ref[F, InMemoryPlatform.State[F]]):
  import InMemoryPlatform.*

  /** Queue the next dialog's answer. */
  def answer(choice: Option[HostPath]): F[Unit] =
    state.update(s => s.copy(answers = s.answers :+ choice))

  /** Every dialog request, in order, `true` for a save dialog. */
  def requests: F[Vector[(FileRequest, Boolean)]] = state.get.map(_.requests)

  /** Every target handed to [[ExternalOpen]], in order. */
  def opened: F[Vector[ExternalTarget]] = state.get.map(_.opened)

  /** Every font registered, in order. */
  def registered: F[Vector[FontRequest]] = state.get.map(_.fonts)

  /** Move the virtual clock by `by`, running each task due on the way in
    * order of its due time (then of scheduling).
    */
  def advance(by: FiniteDuration): F[Unit] =
    state.get.map(_.clock + by.toMillis).flatMap { target =>
      def step: F[Unit] =
        state
          .modify { s =>
            s.tasks.filter(_.due <= target).minByOption(t => (t.due, t.id)) match
              case None    => (s.copy(clock = target), None)
              case Some(t) =>
                (s.copy(clock = t.due, tasks = s.tasks.filterNot(_.id == t.id)), Some(t.run))
          }
          .flatMap(_.fold(Sync[F].unit)(run => run >> step))
      step
    }

  private val files: FileSystem[F] = new FileSystem[F]:
    def child(directory: HostPath, name: String): Either[PlatformError, HostPath] =
      if name.isEmpty || name == "." || name == ".." || name.exists(c => c == '/' || c == '\\')
      then Left(PlatformError.InvalidName(directory, name))
      else HostPath.of(s"${directory.value.stripSuffix("/")}/$name")

    def read(path: HostPath): F[Either[PlatformError, IArray[Byte]]] =
      state.get.map(_.files.get(path.value).toRight(PlatformError.Missing(path)))

    def write(path: HostPath, bytes: IArray[Byte]): F[Either[PlatformError, Unit]] =
      state.modify { s =>
        if s.directories(path.value) then
          (s, Left(PlatformError.Unwritable(path, "a directory")))
        else
          (
            s.copy(
              files = s.files.updated(path.value, bytes),
              directories = s.directories ++ parents(path.value)
            ),
            Right(())
          )
      }

    def list(directory: HostPath): F[Either[PlatformError, Vector[HostPath]]] =
      state.get.map { s =>
        val dir = directory.value.stripSuffix("/")
        if !s.directories(dir) then Left(PlatformError.Missing(directory))
        else
          val names = (s.files.keySet ++ s.directories).toVector
            .filter(p => parents(p).lastOption.contains(dir))
            .sorted
          names.traverse(HostPath.of)
      }

    def project(path: HostPath): F[Either[PlatformError, ProjectStore[F]]] =
      state.get.map(_.bundles.get(path.value)).flatMap {
        case Some(store) => Sync[F].pure(Right(store))
        case None        =>
          InMemoryProjectStore.create[F].flatMap { fresh =>
            state.modify { s =>
              s.bundles.get(path.value) match
                case Some(store) => (s, Right(store))
                case None        =>
                  (
                    s.copy(
                      bundles = s.bundles.updated(path.value, fresh),
                      directories = s.directories ++ parents(path.value) + path.value
                    ),
                    Right(fresh)
                  )
            }
          }
      }

  private val dialogs: Dialogs[F] = new Dialogs[F]:
    private def ask(request: FileRequest, save: Boolean): F[Option[HostPath]] =
      state.modify { s =>
        val next = s.copy(answers = s.answers.drop(1), requests = s.requests :+ (request, save))
        (next, s.answers.headOption.flatten)
      }
    def chooseOpen(request: FileRequest): F[Option[HostPath]] = ask(request, save = false)
    def chooseSave(request: FileRequest): F[Option[HostPath]] = ask(request, save = true)

  private val clipboard: Clipboard[F] = new Clipboard[F]:
    def readText: F[Either[PlatformError, Option[String]]] = state.get.map(s => Right(s.clip))
    def writeText(text: String): F[Either[PlatformError, Unit]] =
      state.update(_.copy(clip = Some(text))).as(Right(()))

  private val fonts: Fonts[F] = new Fonts[F]:
    def register(font: FontRequest): F[Either[PlatformError, Unit]] =
      state.update(s => s.copy(fonts = s.fonts :+ font)).as(Right(()))

  private val scheduler: Scheduler[F] = new Scheduler[F]:
    def now: F[ClockReading] = state.get.map(s => ClockReading.utc(s.clock))

    def after(delay: FiniteDuration)(task: F[Unit]): F[Scheduled[F]] =
      state.modify { s =>
        val id = s.nextTask
        (
          s.copy(
            tasks = s.tasks :+ Task(id, s.clock + delay.toMillis, task),
            nextTask = id + 1
          ),
          new Scheduled[F]:
            def cancel: F[Unit] =
              state.update(t => t.copy(tasks = t.tasks.filterNot(_.id == id)))
        )
      }

  private val external: ExternalOpen[F] = new ExternalOpen[F]:
    def open(target: ExternalTarget): F[Either[PlatformError, Unit]] =
      state.update(s => s.copy(opened = s.opened :+ target)).as(Right(()))

  private val preferences: Preferences[F] = new Preferences[F]:
    def get(key: PreferenceKey): F[Either[PlatformError, Option[String]]] =
      state.get.map(s => Right(s.prefs.get(key)))
    def put(key: PreferenceKey, value: String): F[Either[PlatformError, Unit]] =
      state.update(s => s.copy(prefs = s.prefs.updated(key, value))).as(Right(()))
    def remove(key: PreferenceKey): F[Either[PlatformError, Unit]] =
      state.update(s => s.copy(prefs = s.prefs - key)).as(Right(()))

  /** The services, with every capability on. */
  val platform: Platform[F] = Platform(
    HostCapabilities(
      nativeMenuBar = false,
      multipleWindows = true,
      localFiles = true,
      clipboardRead = true
    ),
    files,
    dialogs,
    clipboard,
    fonts,
    scheduler,
    external,
    preferences
  )

object InMemoryPlatform:

  private final case class Task[F[_]](id: Long, due: Long, run: F[Unit])

  private final case class State[F[_]](
      files: Map[String, IArray[Byte]],
      directories: Set[String],
      bundles: Map[String, ProjectStore[F]],
      answers: Vector[Option[HostPath]],
      requests: Vector[(FileRequest, Boolean)],
      clip: Option[String],
      fonts: Vector[FontRequest],
      clock: Long,
      tasks: Vector[Task[F]],
      nextTask: Long,
      opened: Vector[ExternalTarget],
      prefs: Map[PreferenceKey, String]
  )

  /** Every proper ancestor of a `/`-separated path, outermost first; the
    * root is `""`.
    */
  private def parents(path: String): Vector[String] =
    val segments = path.stripSuffix("/").split('/').toVector
    segments.indices.drop(1).map(n => segments.take(n).mkString("/")).toVector

  /** A platform whose file system holds only the root `/` and whose clock
    * reads `start` (epoch milliseconds, UTC).
    */
  def create[F[_]: Sync](start: Long = 0L): F[InMemoryPlatform[F]] =
    Ref
      .of[F, State[F]](
        State(
          Map.empty,
          Set(""),
          Map.empty,
          Vector.empty,
          Vector.empty,
          None,
          Vector.empty,
          start,
          Vector.empty,
          0L,
          Vector.empty,
          Map.empty
        )
      )
      .map(new InMemoryPlatform(_))
