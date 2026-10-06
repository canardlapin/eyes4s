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
import cats.effect.unsafe.IORuntime
import cats.effect.unsafe.implicits.global
import eyes4s.codec.ByteDigest
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.document.{DocumentSamples, Source, SourcePath, SourceRole, Theme}
import eyes4s.studio.core.command.{Command, JournalEntry}
import eyes4s.studio.core.session.ProjectSession

import java.util.concurrent.{CompletableFuture, Executors, TimeUnit}
import scala.collection.mutable
import scala.concurrent.ExecutionContext
import scala.util.chaining.*

/** Every read, import and save on a [[SessionPort]] answers its `done`, so
  * no caller waits forever (a trial key check waiting on a read would show
  * "checking" for good): an operation that raises answers with its message,
  * and one refused because the port closed answers with the refusal.
  */
class SessionPortSuite extends munit.FunSuite:

  /** A store whose reads raise, as a failing disk does. */
  private final class RaisingReads(inner: ProjectStore[IO]) extends ProjectStore[IO]:
    /** Set once the session is open: it reads the bundle while it opens. */
    @volatile var raising: Boolean     = false
    @volatile var beforeRead: IO[Unit] = IO.unit
    def read(path: BundlePath)         =
      IO.defer(
        beforeRead.flatMap(_ =>
          if raising then IO.raiseError(RuntimeException("disk gone")) else inner.read(path)
        )
      )
    def list                                                           = inner.list
    def write(lock: WriterLock, path: BundlePath, bytes: IArray[Byte]) =
      inner.write(lock, path, bytes)
    def delete(lock: WriterLock, path: BundlePath) = inner.delete(lock, path)
    def readManifest                               = inner.readManifest
    def swapManifest(lock: WriterLock, expected: Option[ByteDigest], next: IArray[Byte]) =
      inner.swapManifest(lock, expected, next)
    def acquire(owner: LockOwner)  = inner.acquire(owner)
    def release(lock: WriterLock)  = inner.release(lock)
    def readSidecar(file: Sidecar) = inner.readSidecar(file)
    def appendSidecar(lock: WriterLock, file: Sidecar, bytes: IArray[Byte]) =
      inner.appendSidecar(lock, file, bytes)
    def replaceSidecar(lock: WriterLock, file: Sidecar, bytes: IArray[Byte]) =
      inner.replaceSidecar(lock, file, bytes)
    def removeSidecar(lock: WriterLock, file: Sidecar) = inner.removeSidecar(lock, file)

  private val stores = mutable.ArrayBuffer.empty[RaisingReads]

  private def session(raising: Boolean = true): ProjectSession[IO] =
    val doc   = DocumentSamples.t1
    val owner = LockOwner.of("SessionPortSuite").fold(e => fail(e.toString), identity)
    (for
      store   <- InMemoryProjectStore.create[IO]
      created <- ProjectSession.create(
        stores.addOne(RaisingReads(store)).last,
        owner,
        doc,
        SharingOptions.complete,
        BundleSamples.inputsFor(doc)
      )
    yield created.fold(e => fail(e.message), identity))
      .unsafeRunSync()
      .tap(_ => stores.last.raising = raising)

  private val source = Source(
    SourceRole.Fixations,
    SourcePath.of("fixations.csv").fold(e => fail(e.message), identity),
    ByteDigest.sha256(IArray.from("a,b\n1,2\n".getBytes("UTF-8"))),
    None
  )

  /** Read `source` through `port`; the answer, or a failure after 10 s. */
  private def answer(port: SessionPort): Either[String, IArray[Byte]] =
    val done = CompletableFuture[Either[String, IArray[Byte]]]()
    port.readInput(source, r => done.complete(r): Unit)
    try done.get(10, TimeUnit.SECONDS)
    catch case _: java.util.concurrent.TimeoutException => fail("readInput never answered")

  test("a read that raises answers with the error's message") {
    val s    = session()
    val port = SessionPort.start(s, _ => ())
    try assertEquals(answer(port).left.toOption, Some("disk gone"))
    finally
      port.close()
      s.close.unsafeRunSync(): Unit
  }

  test("a read refused because the port closed answers with the refusal") {
    val s        = session()
    val reported = mutable.ArrayBuffer.empty[String]
    val port     = SessionPort.start(s, reported += _)
    port.close()
    try
      assertEquals(answer(port), Left("the project is closed"))
      // A caller was answered, so there is nothing to report.
      assertEquals(reported.toVector, Vector.empty)
      // A journal entry has no caller: its refusal is reported.
      port.journal(eyes4s.studio.core.command.JournalEntry.Undo)
      assertEquals(reported.toVector, Vector("journal Undo after the project port closed"))
    finally s.close.unsafeRunSync(): Unit
  }

  test(
    "a source callback on the sole IO compute thread queues and answers without blocking that runtime"
  ) {
    val executor = Executors.newSingleThreadExecutor(r =>
      val thread = Thread(r, "session-port-single-compute")
      thread.setDaemon(true)
      thread
    )
    val runtime = IORuntime
      .builder()
      .setCompute(
        ExecutionContext.fromExecutor(executor),
        () => { executor.shutdownNow(): Unit }
      )
      .build()
    val bytes = IArray.from("a,b\n1,2\n".getBytes("UTF-8"))
    val s     = (for
      store   <- InMemoryProjectStore.create[IO]
      session <- ProjectSession
        .create(
          store,
          LockOwner.of("single compute source callback").toOption.get,
          DocumentSamples.t1,
          SharingOptions.complete,
          BundleSamples.inputsFor(DocumentSamples.t1)
        )
        .map(_.fold(e => fail(e.message), identity))
      _ <- session
        .importInput(InputKind.Source(SourceRole.Fixations), "fixations.csv", bytes)
        .map(_.fold(e => fail(e.message), identity))
    yield session).unsafeRunSync()(using runtime)
    val port = SessionPort.start(s, _ => ())(using runtime)
    val done = CompletableFuture[Either[Throwable, Either[String, IArray[Byte]]]]()
    IO.async_[Either[String, IArray[Byte]]](callback =>
      port.readInput(source, answer => callback(Right(answer)))
    ).unsafeRunAsync(answer => done.complete(answer): Unit)(using runtime)
    try
      val answer = done.get(5, TimeUnit.SECONDS).fold(throw _, identity)
      assertEquals(answer.map(Vector.from(_)), Right(Vector.from(bytes)))
    finally
      // A broken bridge must fail within the test's deadline, even if it
      // blocked this owned runtime. The executor is daemon and interrupted.
      if done.isDone then
        port.close()
        s.close.unsafeRunSync()(using runtime): Unit
      runtime.shutdown()
  }

  test(
    "FIFO journal-before-save drains on interrupted first close and permits repeated callback close"
  ) {
    val s     = session(raising = false)
    val store = stores.last
    val bytes = IArray.from("a,b\n1,2\n".getBytes("UTF-8"))
    s.importInput(InputKind.Source(SourceRole.Fixations), "fixations.csv", bytes)
      .unsafeRunSync()
      .fold(e => fail(e.message), identity): Unit
    val entered     = IO.deferred[Unit].unsafeRunSync()
    val releaseRead = IO.deferred[Unit].unsafeRunSync()
    store.beforeRead = entered.complete(()).void.flatMap(_ => releaseRead.get)
    val port  = SessionPort.start(s, _ => ())
    val read  = CompletableFuture[Either[String, IArray[Byte]]]()
    val saved = CompletableFuture[Either[String, eyes4s.studio.core.session.SaveReceipt]]()
    def closing(): (Thread, CompletableFuture[Boolean]) =
      val done   = CompletableFuture[Boolean]()
      val thread = Thread(() =>
        try { port.close(); done.complete(Thread.currentThread().isInterrupted): Unit }
        catch case failure: Throwable => done.completeExceptionally(failure): Unit
      )
      thread.setDaemon(true)
      thread.start()
      (thread, done)
    try
      port.readInput(
        source,
        answer =>
          port.close() // A repeated close from the draining worker cannot wait for itself.
          read.complete(answer): Unit
      )
      entered.get.unsafeRunSync()
      port.journal(JournalEntry.Apply(Command.SetTheme(Theme.Dark)))
      port.save(answer => saved.complete(answer): Unit)
      val first = closing()
      // Observe closure through the public refusal callback. Any save that
      // wins acceptance first is harmless and remains ahead of the drain.
      val refused  = CompletableFuture[String]()
      val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(5)
      while !refused.isDone && System.nanoTime() < deadline do
        port.save(_.left.foreach(reason => refused.complete(reason): Unit))
        Thread.`yield`()
      assertEquals(refused.get(5, TimeUnit.SECONDS), "the project is closed")
      first._1.interrupt()
      val second = closing()
      assertEquals(second._2.get(5, TimeUnit.SECONDS), false)
      assert(
        !first._2.isDone,
        "interrupting the first close must not abandon accepted operations"
      )
      assert(!read.isDone)
      assert(!saved.isDone)
      releaseRead.complete(()).unsafeRunSync(): Unit
      assertEquals(read.get(5, TimeUnit.SECONDS).map(Vector.from(_)), Right(Vector.from(bytes)))
      assert(saved.get(5, TimeUnit.SECONDS).isRight)
      assertEquals(
        first._2.get(5, TimeUnit.SECONDS),
        true,
        "the caller's interrupt flag is preserved"
      )
      store.beforeRead = IO.unit
      val reopened =
        ProjectBundle.open(store).unsafeRunSync().fold(e => fail(e.message), identity)
      assertEquals(reopened.document.presentation.theme, Theme.Dark)
    finally
      releaseRead.complete(()).unsafeRunSync(): Unit
      store.beforeRead = IO.unit
      port.close()
      s.close.unsafeRunSync(): Unit
  }
