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
import eyes4s.studio.core.bundle.{Inclusion, InputEntry, InputKind, InputStatus, SharingOptions}
import eyes4s.studio.core.command.JournalEntry
import eyes4s.studio.core.document.Source
import eyes4s.studio.core.session.{ProjectSession, SaveReceipt}
import eyes4s.studio.desktop.platform.FileProjectStore

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

  /** Copy the project as last saved into the empty directory `to` (an export
    * bundle's snapshot, S9.5), with the stimulus images when `images`.
    */
  def snapshot(
      @unused to: java.nio.file.Path,
      @unused images: Boolean,
      done: Either[String, Unit] => Unit
  ): Unit = done(Left("this project is not saved in a bundle, so it has no snapshot"))

  /** The inputs the project stores (S5.7: its stimulus images); a port that
    * cannot list them refuses.
    */
  def storedInputs(done: Either[String, Vector[InputEntry]] => Unit): Unit =
    done(Left("this project cannot list its stored files"))

  /** Check every stored input's bytes against its digest (S2.5). A port that
    * cannot check answers `None`: its inputs stay unchecked, so nothing is
    * blocked on their account.
    */
  def checkInputs(done: Either[String, Option[Vector[InputStatus]]] => Unit): Unit =
    done(Right(None))

  /** Put `source`'s exact bytes back in the project (S2.5); a port that
    * cannot store inputs refuses, naming the file.
    */
  def restoreInput(
      source: Source,
      @unused bytes: IArray[Byte],
      done: Either[String, Unit] => Unit
  ): Unit = done(Left(s"${source.path.value}: this project cannot store imported files"))

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

  /** Guards [[closed]] and the queue together: an operation is either queued
    * before [[close]]'s drain marker, so it runs, or refused.
    */
  private val gate = new Object

  /** Queue `op`; after [[close]] it is refused, never lost silently: a read,
    * an import or a save answers its caller through `refused` (its `done`,
    * with the refusal); a journal entry, which has no caller, is reported.
    */
  private def enqueue(
      op: IO[Unit],
      what: String,
      refused: Option[String => Unit] = None
  ): Unit =
    val queued = gate.synchronized {
      if closed.get then false
      else
        queue.offer(op).unsafeRunSync()
        true
    }
    if !queued then
      refused.fold(report(s"$what after the project port closed"))(_("the project is closed"))

  /** `op`'s result for `done`; a raised error answers too, as its message, so
    * a caller waiting on `done` is never left waiting.
    */
  private def answering[A](
      op: IO[Either[String, A]],
      done: Either[String, A] => Unit
  ): IO[Unit] =
    op.attempt.flatMap(r =>
      IO(done(r.left.map(e => Option(e.getMessage).getOrElse(e.toString)).flatten))
    )

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
    enqueue(
      answering(
        session.importInput(kind, name, bytes).map(_.left.map(_.message).map(_ => ())),
        done
      ),
      s"import $name",
      Some(reason => done(Left(reason)))
    )

  /** Queued after the imports before it, so a file just imported is listed. */
  override def storedInputs(done: Either[String, Vector[InputEntry]] => Unit): Unit =
    enqueue(
      answering(session.inputs.map(Right(_)), done),
      "list stored inputs",
      Some(reason => done(Left(reason)))
    )

  /** Queued after the imports and restores before it. */
  override def checkInputs(done: Either[String, Option[Vector[InputStatus]]] => Unit): Unit =
    enqueue(
      answering(session.checkInputs.map(s => Right(Some(s))), done),
      "check stored inputs",
      Some(reason => done(Left(reason)))
    )

  override def restoreInput(
      source: Source,
      bytes: IArray[Byte],
      done: Either[String, Unit] => Unit
  ): Unit =
    enqueue(
      answering(session.restoreInput(source, bytes).map(_.left.map(_.message)), done),
      s"restore ${source.path.value}",
      Some(reason => done(Left(reason)))
    )

  /** Queued after the imports before it, so a file just imported reads back. */
  override def readInput(source: Source, done: Either[String, IArray[Byte]] => Unit): Unit =
    enqueue(
      answering(session.readInput(source).map(_.left.map(_.message)), done),
      s"read ${source.path.value}",
      Some(reason => done(Left(reason)))
    )

  override def snapshot(
      to: java.nio.file.Path,
      images: Boolean,
      done: Either[String, Unit] => Unit
  ): Unit =
    val sharing = SharingOptions(
      Inclusion.Included,
      if images then Inclusion.Included else Inclusion.Withheld
    )
    enqueue(
      answering(
        FileProjectStore
          .at[IO](to)
          .flatMap(session.share(_, sharing))
          .map(_.left.map(_.message).map(_ => ())),
        done
      ),
      s"snapshot to $to",
      Some(reason => done(Left(reason)))
    )

  def save(done: Either[String, SaveReceipt] => Unit): Unit =
    enqueue(
      answering(session.save.map(_.left.map(_.message)), done),
      "save",
      Some(reason => done(Left(reason)))
    )

  /** Finish the queued operations, then stop. The session stays open. */
  def close(): Unit =
    val drained = gate.synchronized {
      if !closed.compareAndSet(false, true) then None
      else
        val d = IO.deferred[Unit].unsafeRunSync()
        queue.offer(d.complete(()).void).unsafeRunSync()
        Some(d)
    }
    drained.foreach { d =>
      d.get.unsafeRunSync()
      release.unsafeRunSync()
    }

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
