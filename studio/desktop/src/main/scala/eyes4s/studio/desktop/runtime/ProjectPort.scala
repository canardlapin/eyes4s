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
import cats.effect.std.Queue
import cats.effect.unsafe.IORuntime
import eyes4s.studio.core.bundle.InputKind
import eyes4s.studio.core.command.JournalEntry
import eyes4s.studio.core.document.Source
import eyes4s.studio.core.session.{ProjectSession, SaveReceipt}

import scala.annotation.unused

/** The project a window saves into (tickets S1.8 and S2.4a/b): the app's
  * `Journal` and `Persist` effects, performed in the order the model emits
  * them. A save's outcome is handed back to `done`.
  */
trait ProjectPort:
  def journal(entry: JournalEntry): Unit
  def save(done: Either[String, SaveReceipt] => Unit): Unit
  def close(): Unit

  /** Copy an imported file's bytes into the project, listed at the next save
    * (S5.2): queued before the journal entry of the command that names it.
    * A port that cannot store inputs refuses, naming the file.
    */
  def importInput(
      @unused kind: InputKind,
      name: String,
      @unused bytes: IArray[Byte],
      done: Either[String, Unit] => Unit
  ): Unit = done(Left(s"$name: this project cannot store imported files"))

  /** Read a dataset source's stored bytes back from the project (the
    * column-mapping pane's re-map). `done` is called once, on any thread; a
    * port that cannot read inputs refuses, naming the file.
    */
  def readInput(source: Source, done: Either[String, IArray[Byte]] => Unit): Unit =
    done(Left(s"${source.path.value}: this project cannot read its inputs"))

/** A [[ProjectPort]] on a studio-core [[ProjectSession]]: every operation
  * joins one queue, which a single fibre drains, so a journal entry is
  * performed before the save that follows it. `done` runs on that fibre;
  * the caller moves it to the UI thread.
  */
final class SessionPort private (
    val session: ProjectSession[IO],
    queue: Queue[IO, IO[Unit]],
    release: IO[Unit],
    report: String => Unit
)(using runtime: IORuntime)
    extends ProjectPort:

  private val closed = java.util.concurrent.atomic.AtomicBoolean(false)

  /** Queue `op`; after [[close]] it is refused and reported, never lost silently. */
  private def enqueue(op: IO[Unit], what: String): Unit =
    if closed.get then report(s"$what after the project port closed")
    else queue.offer(op).unsafeRunSync()

  /** A refused journal entry (an undo the session's history does not hold)
    * is reported, not fatal: the next save writes the document as the
    * session has it.
    */
  def journal(entry: JournalEntry): Unit =
    enqueue(
      session
        .perform(entry)
        .flatMap(_.fold(e => IO(report(s"journal $entry: ${e.message}")), _ => IO.unit)),
      s"journal $entry"
    )

  override def importInput(
      kind: InputKind,
      name: String,
      bytes: IArray[Byte],
      done: Either[String, Unit] => Unit
  ): Unit =
    if closed.get then done(Left("the project is closed"))
    else
      enqueue(
        session
          .importInput(kind, name, bytes)
          .flatMap(r => IO(done(r.left.map(_.message).map(_ => ())))),
        s"import $name"
      )

  /** Queued after the imports before it, so a file just imported reads back. */
  override def readInput(source: Source, done: Either[String, IArray[Byte]] => Unit): Unit =
    if closed.get then done(Left("the project is closed"))
    else
      enqueue(
        session.readInput(source).flatMap(r => IO(done(r.left.map(_.message)))),
        s"read ${source.path.value}"
      )

  def save(done: Either[String, SaveReceipt] => Unit): Unit =
    if closed.get then done(Left("the project is closed"))
    else enqueue(session.save.flatMap(r => IO(done(r.left.map(_.message)))), "save")

  /** Finish the queued operations, then stop. The session stays open. */
  def close(): Unit = if closed.compareAndSet(false, true) then
    val drained = IO.deferred[Unit].flatMap(d => queue.offer(d.complete(()).void) *> d.get)
    drained.unsafeRunSync()
    release.unsafeRunSync()

object SessionPort:

  def start(session: ProjectSession[IO], report: String => Unit = System.err.println)(using
      runtime: IORuntime
  ): SessionPort =
    val resources =
      for
        queue <- cats.effect.Resource.eval(Queue.unbounded[IO, IO[Unit]])
        _     <- queue.take
          .flatMap(_.handleError(e => report(String.valueOf(e))))
          .foreverM
          .background
      yield queue
    val (queue, release) = resources.allocated.unsafeRunSync()
    new SessionPort(session, queue, release, report)
