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
import cats.effect.unsafe.implicits.global
import eyes4s.codec.ByteDigest
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.document.{DocumentSamples, Source, SourcePath, SourceRole}
import eyes4s.studio.core.session.ProjectSession

import java.util.concurrent.{CompletableFuture, TimeUnit}
import scala.collection.mutable
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
    @volatile var raising: Boolean = false
    def read(path: BundlePath)     =
      if raising then IO.raiseError(RuntimeException("disk gone")) else inner.read(path)
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

  private def session(): ProjectSession[IO] =
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
      .tap(_ => stores.last.raising = true)

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
