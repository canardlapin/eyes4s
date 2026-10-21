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

package eyes4s.studio.core.session

import cats.effect.{IO, Resource}
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.command.{Command, JournalEntry}
import eyes4s.studio.core.document.{DocumentSamples, SourceRole, Theme}
import java.nio.charset.StandardCharsets.UTF_8
import scala.concurrent.duration.*

class SessionSaveAsSuite extends SessionConformance:
  def bundle: Resource[IO, IO[ProjectStore[IO]]] =
    Resource.eval(InMemoryProjectStore.create[IO]).map(IO.pure)

  private def bytes(text: String): IArray[Byte] = IArray.from(text.getBytes(UTF_8))
  private val fixes    = bytes("participant,phase,trial,x,y\nP01,Encoding,enc_01,1,2\n")
  private val trials   = bytes("participant,phase,trial\nP01,Encoding,enc_01\n")
  private val document =
    BundleSamples.withSources(withoutJobs(DocumentSamples.t1), fixes, trials)
  private val inputs = Vector(SourceRole.Fixations -> fixes, SourceRole.Trials -> trials).map {
    (role, b) =>
      val source = document.datasets.last.sources.entries.find(_.role == role).get
      BundleSamples.right(
        InputEntry.of(
          InputKind.Source(role),
          source.path.value.split('/').last,
          source.bytes,
          b.length.toLong
        )
      )
  }
  private val archivePath =
    BundleSamples.right(BundlePath.in(BundleArea.Runs, "retained/archive.bin"))
  private val archiveBytes = bytes("retained immutable archive payload")

  private def saved: IO[(ProjectSession[IO], ProjectStore[IO])] =
    for
      store   <- InMemoryProjectStore.create[IO]
      lock    <- ok(store.acquire(alice))
      _       <- ok(store.write(lock, archivePath, archiveBytes))
      _       <- ok(store.release(lock))
      session <- ok(
        ProjectSession.create(store, alice, document, SharingOptions.complete, inputs)
      )
      _ <- ok(
        session.importInput(InputKind.Source(SourceRole.Fixations), "fixations.csv", fixes)
      )
      _ <- ok(session.importInput(InputKind.Source(SourceRole.Trials), "trials.csv", trials))
      _ <- ok(session.save)
    yield (session, store)

  test(
    "Save As copies unsaved edits, new inputs, retained entries and live undo; the source stays edited"
  ) {
    for
      source <- saved
      (session, store) = source
      _     <- ok(session.perform(JournalEntry.Apply(Command.SetTheme(Theme.Dark))))
      image <- ok(
        session.importInput(InputKind.StimulusImage, "new.png", bytes("new image bytes"))
      )
      edited        <- session.document
      priorSaved    <- session.saved
      priorManifest <- ok(store.readManifest)
      priorJournal  <- ok(store.readSidecar(Sidecar.Journal))
      target        <- InMemoryProjectStore.create[IO]
      result        <- ok(session.saveAs(target))
      (copied, receipt) = result
      copiedDoc     <- copied.document
      copiedInputs  <- copied.inputs
      payload       <- ok(target.read(archivePath))
      check         <- copied.checkInputs
      _             <- ok(copied.perform(JournalEntry.UndoView))
      undone        <- copied.document
      _             <- ok(copied.save)
      _             <- ok(copied.close)
      reopened      <- ok(ProjectSession.open(target, bob))
      back          <- reopened.session.document
      _             <- ok(reopened.session.close)
      sourceDoc     <- session.document
      sourceSaved   <- session.saved
      dirty         <- session.unsaved
      afterManifest <- ok(store.readManifest)
      afterJournal  <- ok(store.readSidecar(Sidecar.Journal))
      _             <- ok(session.close)
    yield
      assertEquals(copiedDoc, edited)
      assertEquals(copiedInputs.contains(image), true)
      assertEquals(payload.toVector, archiveBytes.toVector)
      assert(check.forall(_.isInstanceOf[InputStatus.Present]), check)
      assertEquals(undone, priorSaved)
      assertEquals(back, undone)
      assertEquals(sourceDoc, edited)
      assertEquals(sourceSaved, priorSaved)
      assert(dirty)
      assertEquals(afterManifest.toVector, priorManifest.toVector)
      assertEquals(afterJournal.toVector, priorJournal.toVector)
      assert(receipt.swapped)
  }

  test("a nonempty destination is refused without altering its entry or keeping its writer") {
    for
      source <- saved
      (session, _) = source
      target <- InMemoryProjectStore.create[IO]
      lock   <- ok(target.acquire(bob))
      _      <- ok(target.write(lock, archivePath, archiveBytes))
      _      <- ok(target.release(lock))
      result <- session.saveAs(target)
      held   <- ok(target.acquire(bob))
      back   <- ok(target.read(archivePath))
      _      <- ok(target.release(held))
      _      <- ok(session.close)
    yield
      assert(
        result.left.toOption.exists {
          case SessionError.Bundle(_, BundleError.TargetNotEmpty(false, entries)) =>
            entries == Vector(archivePath)
          case _ => false
        },
        result.toString
      )
      assertEquals(back.toVector, archiveBytes.toVector)
  }

  test(
    "a failed entry write releases the destination and leaves the source manifest and edits alone"
  ) {
    for
      source <- saved
      (session, store) = source
      _        <- ok(session.perform(JournalEntry.Apply(Command.SetTheme(Theme.Dark))))
      before   <- ok(store.readManifest)
      target   <- InMemoryProjectStore.create[IO].flatMap(FaultyStore.over)
      _        <- target.arm(1, Fault.Before, Failure.Report)
      result   <- session.saveAs(target)
      manifest <- target.readManifest
      lock     <- ok(target.acquire(bob))
      _        <- ok(target.release(lock))
      after    <- ok(store.readManifest)
      dirty    <- session.unsaved
      _        <- ok(session.close)
    yield
      assert(result.isLeft)
      assertEquals(manifest, Left(StoreError.NoManifest))
      assertEquals(after.toVector, before.toVector)
      assert(dirty)
  }

  test("a source cannot be its own destination") {
    for
      source <- saved
      (session, store) = source
      result <- session.saveAs(store)
      _      <- ok(session.close)
    yield assert(result.left.toOption.exists(_.isInstanceOf[SessionError.WriterRefused]))
  }

  test("journal-write trouble remains in the successful copy receipt") {
    for
      source <- saved
      (session, _) = source
      target <- InMemoryProjectStore.create[IO].flatMap(FaultyStore.over)
      _ <- target.armNamed(s"replace ${Sidecar.Journal.fileName}", Fault.Before, Failure.Report)
      result <- ok(session.saveAs(target))
      (copied, receipt) = result
      _ <- ok(copied.close)
      _ <- ok(session.close)
    yield assert(receipt.journalReconcile.nonEmpty || receipt.journalReset.nonEmpty)
  }

  test("cancelling a copy never leaves its new writer inaccessible to the caller") {
    for
      source <- saved
      (session, _) = source
      target    <- InMemoryProjectStore.create[IO].flatMap(FaultyStore.over)
      paused    <- target.pause(1, Fault.Before)
      fiber     <- session.saveAs(target).start
      _         <- paused.reached.get.timeout(10.seconds)
      canceling <- fiber.cancel.start
      _         <- IO.sleep(20.millis)
      _         <- paused.gate.complete(())
      _         <- canceling.join
      outcome   <- fiber.join
      lock      <- ok(target.acquire(bob))
      _         <- ok(target.release(lock))
      _         <- ok(session.close)
    yield assert(outcome.isCanceled)
  }

  test("an existing recovery journal makes a destination nonempty even without a manifest") {
    for
      source <- saved
      (session, _) = source
      target <- InMemoryProjectStore.create[IO]
      lock   <- ok(target.acquire(bob))
      prior = bytes("existing recovery journal")
      _       <- ok(target.replaceSidecar(lock, Sidecar.Journal, prior))
      _       <- ok(target.release(lock))
      refused <- session.saveAs(target)
      after   <- ok(target.readSidecar(Sidecar.Journal))
      _       <- ok(session.close)
    yield
      assertEquals(
        refused.left.toOption,
        Some(SessionError.CopyDestinationState(Vector(Sidecar.Journal)))
      )
      assertEquals(after.toVector, prior.toVector)
  }

  test("the first Save As retains an untitled project's undo history") {
    val base    = withoutJobs(DocumentSamples.t1)
    val history = (
      eyes4s.studio.core.command.History
        .start(base)
        .perform(JournalEntry.Apply(Command.SetTheme(Theme.Dark)))
      )
      .fold(e => fail(e.message), identity)
      .history
    for
      target <- InMemoryProjectStore.create[IO]
      copied <- ok(
        ProjectSession.createWithHistory(
          target,
          alice,
          history,
          SharingOptions.complete,
          BundleSamples.inputsFor(base)
        )
      ).map(_._1)
      before <- copied.document
      _      <- ok(copied.perform(JournalEntry.UndoView))
      after  <- copied.document
      _      <- ok(copied.close)
    yield
      assertEquals(before, history.document)
      assertEquals(after, base)
  }

  test("the first Save As refuses retained recovery bytes and releases its writer") {
    val history  = eyes4s.studio.core.command.History.start(withoutJobs(DocumentSamples.t1))
    val recovery = bytes("existing recovery evidence")
    for
      target <- InMemoryProjectStore.create[IO]
      lock   <- ok(target.acquire(alice))
      _      <- ok(target.replaceSidecar(lock, Sidecar.Journal, recovery))
      _      <- ok(target.release(lock))
      answer <- ProjectSession.createWithHistory(
        target,
        alice,
        history,
        SharingOptions.complete,
        BundleSamples.inputsFor(history.document)
      )
      after    <- ok(target.readSidecar(Sidecar.Journal))
      released <- ok(target.acquire(alice))
      _        <- ok(target.release(released))
    yield
      assertEquals(
        answer.left.toOption,
        Some(SessionError.CopyDestinationState(Vector(Sidecar.Journal)))
      )
      assertEquals(after.toVector, recovery.toVector)
  }
