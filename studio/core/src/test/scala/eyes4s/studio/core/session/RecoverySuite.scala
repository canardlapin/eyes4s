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
import cats.syntax.all.*
import eyes4s.codec.ByteDigest
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.command.*
import eyes4s.studio.core.document.DocumentSamples.{t1, t2}
import eyes4s.studio.core.document.{DocumentGen, RunLifecycle, StudioDocument, Theme}
import org.scalacheck.Gen
import org.scalacheck.rng.Seed

import java.nio.charset.StandardCharsets.UTF_8

/** The autosave journal and crash recovery (ticket S2.4b; S2.4a's
  * RecoverySuite), run against every [[ProjectStore]]: [[RecoverySuite]] here
  * and the file store's suite in studio-desktop.
  */
abstract class RecoveryConformance extends SessionConformance:

  private val base = withoutJobs(t2)

  private val every3 = SessionOptions.of(3).fold(e => fail(e.message), identity)

  private def text(bytes: IArray[Byte]): String = String(Array.from(bytes), UTF_8)

  /** A session entry that never imports sources: an import needs its bytes
    * copied into the bundle before a save, which a generated source has not.
    */
  private def entryFor(h: History): Gen[JournalEntry] =
    CommandGen.entry(h).retryUntil {
      case JournalEntry.Apply(_: Command.ImportSources) => false
      case _                                            => true
    }

  /** Up to `n` generated entries on `session`, each drawn for the history it
    * meets (seeded, so a failure reproduces), with a save after every
    * `saveEvery`-th. Refused entries are skipped. Returns the entries the
    * session performed.
    */
  private def walk(
      session: ProjectSession[IO],
      n: Int,
      seed: Long,
      saveEvery: Int
  ): IO[Vector[JournalEntry]] =
    (1 to n).toVector
      .foldLeftM((Seed(seed), Vector.empty[JournalEntry])) { case ((s, done), i) =>
        for
          h <- session.history
          (entry, next) = entryFor(h).pureApply(Gen.Parameters.default, s) -> s.next
          result <- session.perform(entry).flatMap {
            case Right(_)                         => IO.pure(true)
            case Left(SessionError.Command(_, _)) => IO.pure(false)
            case Left(other)                      =>
              IO.raiseError(AssertionError(s"entry $i ($entry): ${other.message}"))
          }
          _ <- ok(session.save).whenA(saveEvery > 0 && i % saveEvery == 0 && result)
        yield (next, if result then done :+ entry else done)
      }
      .map(_._2)

  for
    (n, saveEvery) <- Vector((1, 0), (5, 0), (17, 0), (40, 0), (40, 7))
    seed           <- Vector(1L, 2L, 3L, 4L)
  do
    test(
      s"E2E-12: a crash after $n generated entries (seed $seed" +
        (if saveEvery > 0 then s", saving every $saveEvery" else "") +
        ") recovers the pre-crash document exactly"
    ) {
      bundle.use { views =>
        val document = DocumentGen.document.pureApply(Gen.Parameters.default, Seed(seed))
        for
          faulty    <- views.flatMap(FaultyStore.over)
          session   <- created(faulty, document, every3)
          performed <- walk(session, n, seed, saveEvery)
          preCrash  <- session.document
          unsaved   <- session.unsaved
          _         <- die(faulty)
          opened    <- reopen(views)
          _         <- opened.report.journal match
            case JournalFinding.Pending(Recovery.Offered(offer)) =>
              IO(assertEquals(offer.recovered, preCrash)) *>
                IO(assertEquals(offer.torn, None)) *>
                ok(opened.session.accept(offer)).void
            case JournalFinding.Clean =>
              // Nothing unsaved: the last entry was followed by a save, or
              // every entry was refused.
              IO(assert(!unsaved, "unsaved work but no recovery offered"))
            case other => IO(fail(s"unexpected journal $other"))
          recovered <- opened.session.document
          _         <- ok(opened.session.close)
          again     <- reopen(views)
          stored    <- again.session.document
          _         <- ok(again.session.close)
        yield
          assert(n == 1 || performed.nonEmpty)
          assertEquals(recovered, preCrash)
          assertEquals(stored, preCrash)
          assertEquals(again.report.journal, JournalFinding.Clean)
      }
    }

  test(
    "the recovery offer lists what it restores, the runs to resubmit and the checkpoint check"
  ) {
    bundle.use { views =>
      for
        faulty  <- views.flatMap(FaultyStore.over)
        every4  <- IO.fromEither(SessionOptions.of(4).leftMap(e => AssertionError(e.message)))
        session <- created(faulty, t2, every4)
        _       <- performAll(session, CommandSamples.session)
        live    <- session.document
        _       <- die(faulty)
        opened  <- reopen(views)
      yield opened.report.journal match
        case JournalFinding.Pending(Recovery.Offered(offer)) =>
          assertEquals(offer.saved, base)
          assertEquals(offer.recovered, live)
          assertEquals(
            offer.restores.map(r => (r.seq, r.label)),
            Vector(
              1 -> "SetPanelSelection (Reporting · no rerun)",
              2 -> "SetTheme (View only)",
              3 -> "Undo",
              4 -> "Redo",
              5 -> "Undo view change",
              6 -> "SaveAndRun (Analysis · rerun)"
            )
          )
          val started = live.runs.last
          assertEquals(started.state, RunLifecycle.Running)
          assertEquals(
            offer.unsubmitted.filter(_.sinceSave),
            Vector(
              UnsubmittedRun(started.id, started.analysis, started.dataset, sinceSave = true)
            )
          )
          assert(offer.unsubmitted.forall(u => live.job(u.run).isEmpty))
          assertEquals(offer.drift, DriftCheck.Checked(Vector(4), 2))
        case other => fail(s"expected an offer, got $other")
    }
  }

  test(
    "replay never re-issues effects: accepting performs nothing and keeps the run unsubmitted"
  ) {
    bundle.use { views =>
      for
        faulty  <- views.flatMap(FaultyStore.over)
        session <- created(faulty, t2)
        applied <- performAll(session, CommandSamples.session)
        _       <- die(faulty)
        opened  <- reopen(views)
        offer   <- opened.report.journal match
          case JournalFinding.Pending(Recovery.Offered(o)) => IO.pure(o)
          case other => IO.raiseError(AssertionError(other.toString))
        receipt   <- ok(opened.session.accept(offer))
        recovered <- opened.session.document
        _         <- ok(opened.session.close)
      yield
        // The live session asked for the run; the recovery did not.
        assert(applied.last.step.effects.exists {
          case Effect.RequestRun(_, _, _) => true
          case _                          => false
        })
        assert(receipt.swapped)
        val run = recovered.runs.last
        assertEquals(run.state, RunLifecycle.Running)
        assertEquals(recovered.job(run.id), None)
        assert(offer.unsubmitted.exists(_.run == run.id))
    }
  }

  test("a torn final journal line is tolerated: the entries before it are restored") {
    bundle.use { views =>
      for
        faulty  <- views.flatMap(FaultyStore.over)
        session <- created(faulty, t2)
        _       <- performAll(session, CommandSamples.session.take(5))
        five    <- session.document
        // The sixth entry's append is torn by the crash.
        _       <- faulty.arm(1, Fault.Torn, Failure.Crash)
        torn    <- session.perform(CommandSamples.session(5)).attempt
        _       <- die(faulty)
        opened  <- reopen(views)
        journal <- ok(views.flatMap(_.readSidecar(Sidecar.Journal)))
      yield
        assertEquals(torn.left.toOption, Some(Crashed))
        assert(!text(journal).endsWith("\n"), "the journal ends in a torn line")
        opened.report.journal match
          case JournalFinding.Pending(Recovery.Offered(offer)) =>
            assertEquals(offer.recovered, five)
            assertEquals(offer.restores.size, 5)
            assert(offer.torn.isDefined)
          case other => fail(s"expected an offer, got $other")
    }
  }

  test("an undo of an edit made before the last save is journaled as its inverse") {
    bundle.use { views =>
      for
        faulty  <- views.flatMap(FaultyStore.over)
        session <- created(faulty, t2)
        _       <- performAll(session, CommandSamples.session.take(2))
        _       <- ok(session.save)
        undone  <- ok(session.perform(JournalEntry.Undo))
        _       <- ok(session.perform(JournalEntry.UndoView))
        live    <- session.document
        journal <- ok(faulty.readSidecar(Sidecar.Journal))
        _       <- die(faulty)
        opened  <- reopen(views)
      yield
        assertEquals(undone.journal, JournalWrite.Appended(1))
        val lines = text(journal).split("\n").toVector
        assertEquals(lines.size, 3)
        assert(lines(1).contains("\"Apply\""), lines(1))
        assert(lines(2).contains("\"Apply\""), lines(2))
        opened.report.journal match
          case JournalFinding.Pending(Recovery.Offered(offer)) =>
            assertEquals(offer.recovered, live)
            assertEquals(offer.recovered.science, base.science)
            assertEquals(offer.recovered.presentation, base.presentation)
          case other => fail(s"expected an offer, got $other")
    }
  }

  test("a failed journal write is reported, and the next write rewrites the journal whole") {
    bundle.use { views =>
      for
        faulty  <- views.flatMap(FaultyStore.over)
        session <- created(faulty, t2)
        first   <- ok(session.perform(CommandSamples.session(0)))
        _       <- faulty.arm(1, Fault.Before, Failure.Report)
        failed  <- ok(session.perform(CommandSamples.session(1)))
        next    <- ok(session.perform(CommandSamples.session(2)))
        live    <- session.document
        _       <- die(faulty)
        opened  <- reopen(views)
      yield
        assertEquals(first.journal, JournalWrite.Appended(1))
        assert(failed.journal.isInstanceOf[JournalWrite.Failed], failed.journal)
        assertEquals(next.journal, JournalWrite.Rewritten(4))
        opened.report.journal match
          case JournalFinding.Pending(Recovery.Offered(offer)) =>
            assertEquals(offer.recovered, live)
          case other => fail(s"expected an offer, got $other")
    }
  }

  test("a cancel request changes nothing replayable and is not journaled") {
    bundle.use { views =>
      for
        store   <- views
        session <- created(store, t2)
        _       <- performAll(session, CommandSamples.session)
        run     <- session.document.map(_.runs.last.id)
        cancel  <- session.perform(JournalEntry.Apply(Command.CancelRun(run)))
        _       <- ok(session.close)
      yield
        // This session holds no job handle for the run it requested.
        assertEquals(
          cancel.left.toOption,
          Some(
            SessionError.Command(
              JournalEntry.Apply(Command.CancelRun(run)),
              CommandError.NoJobHandle(run)
            )
          )
        )
    }
  }

  test(
    "while a recovery is pending, edits and saves are refused; declining archives the journal"
  ) {
    bundle.use { views =>
      for
        faulty  <- views.flatMap(FaultyStore.over)
        session <- created(faulty, t2)
        _       <- performAll(session, CommandSamples.session.take(2))
        journal <- ok(faulty.readSidecar(Sidecar.Journal))
        _       <- die(faulty)
        opened  <- reopen(views)
        digest = ByteDigest.sha256(journal)
        edit     <- opened.session.perform(CommandSamples.session(0))
        save     <- opened.session.save
        wrong    <- opened.session.decline(ByteDigest.sha256(BundleSamples.utf8("other")))
        archived <- ok(opened.session.decline(digest))
        twice    <- opened.session.decline(digest)
        doc      <- opened.session.document
        store    <- views
        copy     <- ok(store.read(archived))
        gone     <- store.readSidecar(Sidecar.Journal)
        _        <- ok(opened.session.close)
        again    <- reopen(views)
        _        <- ok(again.session.close)
      yield
        assertEquals(edit.map(_ => ()), Left(SessionError.RecoveryPending(digest)))
        assertEquals(save.map(_ => ()), Left(SessionError.RecoveryPending(digest)))
        assert(wrong.left.exists(_.isInstanceOf[SessionError.OtherRecovery]), wrong)
        assertEquals(twice, Left(SessionError.NoRecoveryPending(digest)))
        assertEquals(archived.value, s"cache/journals/${digest.hex}.jsonl")
        assertEquals(ByteDigest.sha256(copy), digest)
        assertEquals(gone, Left(StoreError.NoSidecar(Sidecar.Journal)))
        assertEquals(doc, base)
        assertEquals(again.report.journal, JournalFinding.Clean)
    }
  }

  test(
    "a journal whose checkpoint disagrees is unreplayable: it can be declined, not accepted"
  ) {
    bundle.use { views =>
      for
        store   <- views
        session <- created(store, t2)
        _       <- ok(session.close)
        lines   <- IO.fromEither(
          (
            CommandJournal.start(base),
            CommandJournal.entry(1, JournalEntry.Apply(Command.SetTheme(Theme.Dark))),
            CommandJournal.checkpoint(1, t1)
          ).mapN((a, b, c) => Vector(a, b, c).map(_ + "\n").mkString)
            .leftMap(e => AssertionError(e.message))
        )
        lock    <- ok(store.acquire(owner("a forger")))
        _       <- ok(store.replaceSidecar(lock, Sidecar.Journal, BundleSamples.utf8(lines)))
        _       <- ok(store.release(lock))
        opened  <- reopen(views)
        pending <- opened.session.pending
        digest = ByteDigest.sha256(BundleSamples.utf8(lines))
        fake   = RecoveryOffer(
          digest,
          base,
          base,
          History.start(base),
          Vector.empty,
          Vector.empty,
          DriftCheck.NoCheckpoint(0),
          None
        )
        accepted <- opened.session.accept(fake)
        declined <- opened.session.decline(digest)
        _        <- ok(opened.session.close)
      yield
        pending match
          case Some(Recovery.Unreplayable(d, JournalError.Drift(3, 1, _, _))) =>
            assertEquals(d, digest)
          case other => fail(s"expected an unreplayable journal, got $other")
        assert(accepted.left.exists(_.isInstanceOf[SessionError.NotReplayable]), accepted)
        assert(declined.isRight, declined)
    }
  }

  test("a journal over an older document is superseded and archived on open") {
    bundle.use { views =>
      for
        store   <- views
        session <- created(store, t2)
        _       <- ok(session.close)
        lines   <- IO.fromEither(
          (
            CommandJournal.start(withoutJobs(t1)),
            CommandJournal.entry(1, JournalEntry.Apply(Command.SetTheme(Theme.Dark)))
          ).mapN((a, b) => Vector(a, b).map(_ + "\n").mkString)
            .leftMap(e => AssertionError(e.message))
        )
        lock   <- ok(store.acquire(owner("an older session")))
        _      <- ok(store.replaceSidecar(lock, Sidecar.Journal, BundleSamples.utf8(lines)))
        _      <- ok(store.release(lock))
        opened <- reopen(views)
        doc    <- opened.session.document
        gone   <- store.readSidecar(Sidecar.Journal)
        _      <- ok(opened.session.close)
      yield
        opened.report.journal match
          case JournalFinding.Superseded(path) =>
            assertEquals(
              path.value,
              s"cache/journals/${ByteDigest.sha256(BundleSamples.utf8(lines)).hex}.jsonl"
            )
          case other => fail(s"expected a superseded journal, got $other")
        assertEquals(doc, base)
        assertEquals(gone, Left(StoreError.NoSidecar(Sidecar.Journal)))
    }
  }

  test("closing keeps the journal: unsaved work is offered at the next open") {
    bundle.use { views =>
      for
        store   <- views
        session <- created(store, t2)
        _       <- performAll(session, CommandSamples.session.take(2))
        live    <- session.document
        _       <- ok(session.close)
        opened  <- reopen(views)
        _       <- ok(opened.session.close)
      yield opened.report.journal match
        case JournalFinding.Pending(Recovery.Offered(offer)) =>
          assertEquals(offer.recovered, live)
        case other => fail(s"expected an offer, got $other")
    }
  }

  test("session options refuse a checkpoint interval below 1") {
    assertEquals(SessionOptions.of(0), Left(SessionError.CheckpointInterval(0)))
    assertEquals(SessionOptions.of(1).map(_.checkpointEvery), Right(1))
  }

/** The in-memory store passes the recovery conformance suite. */
class RecoverySuite extends RecoveryConformance:
  def bundle: Resource[IO, IO[ProjectStore[IO]]] =
    Resource.eval(InMemoryProjectStore.create[IO]).map(IO.pure)
