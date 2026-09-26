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
import eyes4s.studio.core.document.{StudioDocument, Theme}

/** Faults beyond the save (S2.4a/b review): crashes at the journal append,
  * at every step of accepting and of declining a recovery, and of archiving
  * a superseded journal; a journal written on a manifest that no longer
  * opens; the runs every open reports as orphaned; and the journal rewrite a
  * save makes after a failed autosave. Run against every [[ProjectStore]].
  */
abstract class RecoveryFaultConformance extends SessionConformance:

  private val base   = withoutJobs(t2)
  private val edits  = CommandSamples.session
  private val faults = Vector(Fault.Before, Fault.After)

  private def replayed(n: Int): StudioDocument =
    edits
      .take(n)
      .foldLeft(History.start(base))((h, e) =>
        h.perform(e).fold(x => fail(x.toString), _.history)
      )
      .document

  /** t2 with `n` story entries performed and never saved, then a crash. */
  private def crashedAfter(views: IO[ProjectStore[IO]], n: Int): IO[Unit] =
    for
      faulty  <- views.flatMap(FaultyStore.over)
      session <- created(faulty, t2)
      _       <- performAll(session, edits.take(n))
      _       <- die(faulty)
    yield ()

  private def offerOf(report: OpenReport): IO[RecoveryOffer] = report.journal match
    case JournalFinding.Pending(Recovery.Offered(o)) => IO.pure(o)
    case other => IO.raiseError(AssertionError(s"expected an offer, got $other"))

  /** Open, accept any offer, and return the document; then close. */
  private def settle(views: IO[ProjectStore[IO]]): IO[StudioDocument] =
    for
      opened <- reopen(views)
      _      <- opened.report.journal match
        case JournalFinding.Pending(Recovery.Offered(o)) => ok(opened.session.accept(o)).void
        case _                                           => IO.unit
      doc <- opened.session.document
      _   <- ok(opened.session.close)
    yield doc

  for fault <- faults do
    test(
      s"a crash ${fault.toString.toLowerCase} a journal append recovers the entries the journal holds"
    ) {
      bundle.use { views =>
        for
          faulty  <- views.flatMap(FaultyStore.over)
          session <- created(faulty, t2)
          _       <- performAll(session, edits.take(2))
          _       <- faulty.arm(1, fault, Failure.Crash)
          crashed <- session.perform(edits(2)).attempt
          _       <- die(faulty)
          opened  <- reopen(views)
          offer   <- offerOf(opened.report)
          _       <- ok(opened.session.close)
        yield
          assertEquals(crashed.left.toOption, Some(Crashed))
          val expected = if fault == Fault.After then 3 else 2
          assertEquals(offer.restores.size, expected)
          assertEquals(offer.recovered, replayed(expected))
      }
    }

  /** The mutations accepting the six-entry offer makes. */
  private val acceptSteps: IO[Vector[String]] =
    bundle.use { views =>
      for
        _      <- crashedAfter(views, edits.size)
        faulty <- views.flatMap(FaultyStore.over)
        opened <- ok(ProjectSession.open(faulty, bob))
        offer  <- offerOf(opened.report)
        _      <- faulty.reset
        _      <- ok(opened.session.accept(offer))
        steps  <- faulty.mutations
      yield steps
    }

  for fault <- faults do
    test(
      s"a crash ${fault.toString.toLowerCase} each step of accepting a recovery loses nothing"
    ) {
      acceptSteps.flatMap { all =>
        assertEquals(all.takeRight(2), Vector("swap manifest", "replace project.journal"))
        assert(!all.dropRight(1).contains("replace project.journal"), all)
        (1 to all.size).toVector.traverse_ { k =>
          bundle.use { views =>
            val clue = s"crash ${fault.toString.toLowerCase} accept step $k (${all(k - 1)})"
            for
              _       <- crashedAfter(views, edits.size)
              faulty  <- views.flatMap(FaultyStore.over)
              opened  <- ok(ProjectSession.open(faulty, bob))
              offer   <- offerOf(opened.report)
              _       <- faulty.arm(k, fault, Failure.Crash)
              crashed <- opened.session.accept(offer).attempt
              _       <- die(faulty)
              doc     <- settle(views)
              again   <- reopen(views)
              _       <- ok(again.session.close)
            yield
              assertEquals(crashed.left.toOption, Some(Crashed), clue)
              assertEquals(doc, replayed(edits.size), clue)
              assertEquals(again.report.journal, JournalFinding.Clean, clue)
          }
        }
      }
    }

  for
    fault <- faults
    step  <- Vector(1, 2)
  do
    test(
      s"a crash ${fault.toString.toLowerCase} decline step $step keeps the journal or its archive"
    ) {
      bundle.use { views =>
        for
          _       <- crashedAfter(views, 2)
          faulty  <- views.flatMap(FaultyStore.over)
          opened  <- ok(ProjectSession.open(faulty, bob))
          offer   <- offerOf(opened.report)
          _       <- faulty.arm(step, fault, Failure.Crash)
          crashed <- opened.session.decline(offer.journal).attempt
          _       <- die(faulty)
          next    <- reopen(views)
          redo    <- next.report.journal match
            case JournalFinding.Pending(r) => ok(next.session.decline(r.journal)).map(Some(_))
            case _                         => IO.pure(None)
          doc      <- next.session.document
          _        <- ok(next.session.close)
          store    <- views
          archived <- store.read(
            BundlePath
              .in(BundleArea.Journals, s"archive/${offer.journal.hex}.jsonl")
              .fold(e => fail(e.message), identity)
          )
        yield
          assertEquals(crashed.left.toOption, Some(Crashed))
          // Only a crash after the removal leaves nothing pending.
          assertEquals(redo.isEmpty, step == 2 && fault == Fault.After)
          assertEquals(doc, base)
          assertEquals(archived.map(ByteDigest.sha256), Right(offer.journal))
      }
    }

  for
    fault <- faults
    step  <- Vector(1, 2)
  do
    test(
      s"a crash ${fault.toString.toLowerCase} step $step of archiving a superseded journal is harmless"
    ) {
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
          lock    <- ok(store.acquire(owner("an older session")))
          _       <- ok(store.replaceSidecar(lock, Sidecar.Journal, BundleSamples.utf8(lines)))
          _       <- ok(store.release(lock))
          faulty  <- views.flatMap(FaultyStore.over)
          _       <- faulty.arm(step, fault, Failure.Crash)
          crashed <- ProjectSession.open(faulty, bob).attempt
          _       <- die(faulty)
          next    <- reopen(views)
          doc     <- next.session.document
          _       <- ok(next.session.close)
          after   <- reopen(views)
          _       <- ok(after.session.close)
        yield
          assertEquals(crashed.left.toOption, Some(Crashed))
          next.report.journal match
            case JournalFinding.Superseded(_) => assert(!(step == 2 && fault == Fault.After))
            case JournalFinding.Clean         => assert(step == 2 && fault == Fault.After)
            case other                        => fail(s"unexpected journal $other")
          assertEquals(doc, base)
          assertEquals(after.report.journal, JournalFinding.Clean)
      }
    }

  test(
    "a journal written on a manifest that no longer opens is kept and reported, never superseded"
  ) {
    bundle.use { views =>
      for
        store   <- views
        session <- created(store, t2)
        before  <- ok(store.readManifest)
        _       <- performAll(session, edits)
        _       <- ok(session.save)
        current <- ok(store.readManifest)
        // Work after the save, journaled on the saved (soon damaged) document.
        _        <- ok(session.perform(JournalEntry.Apply(Command.SetTheme(Theme.Dark))))
        journal  <- ok(store.readSidecar(Sidecar.Journal))
        _        <- ok(session.close)
        newParts <- IO.fromEither(
          (ProjectBundle.readManifest(current), ProjectBundle.readManifest(before)).tupled
            .map((n, o) => n.parts.all.map(_.path).diff(o.parts.all.map(_.path)))
            .leftMap(e => AssertionError(e.message))
        )
        vandal <- ok(store.acquire(owner("disk fault")))
        _      <- ok(store.write(vandal, newParts.head, BundleSamples.utf8("{}")))
        _      <- ok(store.release(vandal))
        opened <- reopen(views)
        kept   <- ok(store.readSidecar(Sidecar.Journal))
        digest = ByteDigest.sha256(journal)
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
        edit     <- opened.session.perform(JournalEntry.Apply(Command.SetTheme(Theme.Dark)))
        archived <- ok(opened.session.decline(digest))
        _        <- ok(opened.session.close)
      yield
        assert(opened.report.source.isInstanceOf[ManifestSource.Previous], opened.report.source)
        opened.report.journal match
          case JournalFinding.Pending(Recovery.JournalOnDamagedManifest(d, problem)) =>
            assertEquals(d, digest)
            assertEquals(ManifestSource.Previous(problem), opened.report.source)
          case other => fail(s"expected a journal on a damaged manifest, got $other")
        assertEquals(ByteDigest.sha256(kept), digest)
        assert(accepted.left.exists(_.isInstanceOf[SessionError.DamagedBase]), accepted)
        assertEquals(edit.map(_ => ()), Left(SessionError.RecoveryPending(digest)))
        assertEquals(archived.value, s"journals/archive/${digest.hex}.jsonl")
    }
  }

  test("every open lists the runs the saved document shows running, as orphaned") {
    bundle.use { views =>
      for
        store   <- views
        session <- created(store, t2)
        _       <- performAll(session, edits)
        _       <- ok(session.save)
        saved   <- session.document
        _       <- ok(session.close)
        first   <- reopen(views)
        _       <- ok(first.session.close)
        second  <- reopen(views)
        _       <- ok(second.session.close)
      yield
        val expected =
          saved.running.map(r => UnsubmittedRun(r.id, r.analysis, r.dataset, sinceSave = false))
        assert(expected.nonEmpty)
        assert(expected.exists(_.run == saved.runs.last.id))
        assertEquals(first.report.journal, JournalFinding.Clean)
        assertEquals(first.report.orphaned, expected)
        assertEquals(second.report.orphaned, expected)
    }
  }

  test(
    "after a failed autosave, a save rewrites the journal before its swap, so recovery is exact"
  ) {
    bundle.use { views =>
      for
        faulty  <- views.flatMap(FaultyStore.over)
        session <- created(faulty, t2)
        _       <- ok(session.perform(edits(0)))
        _       <- faulty.arm(1, Fault.Before, Failure.Report)
        failed  <- ok(session.perform(edits(1)))
        live    <- session.document
        _       <- faulty.reset
        _       <- faulty.armNamed("swap manifest", Fault.Before, Failure.Crash)
        crashed <- session.save.attempt
        steps   <- faulty.mutations
        _       <- die(faulty)
        opened  <- reopen(views)
        offer   <- offerOf(opened.report)
        _       <- ok(opened.session.close)
      yield
        assert(failed.journal.isInstanceOf[JournalWrite.Failed], failed.journal)
        assertEquals(crashed.left.toOption, Some(Crashed))
        assertEquals(steps.head, "replace project.journal")
        assertEquals(offer.recovered, live)
        assertEquals(offer.restores.size, 2)
    }
  }

  test("if that rewrite fails too, the save still completes and reports the narrower window") {
    bundle.use { views =>
      for
        faulty  <- views.flatMap(FaultyStore.over)
        session <- created(faulty, t2)
        _       <- ok(session.perform(edits(0)))
        _       <- faulty.arm(1, Fault.Before, Failure.Report)
        _       <- ok(session.perform(edits(1)))
        live    <- session.document
        _       <- faulty.reset
        _       <- faulty.armNamed("replace project.journal", Fault.Before, Failure.Report)
        receipt <- ok(session.save)
        _       <- ok(session.close)
        opened  <- reopen(views)
        doc     <- opened.session.document
        _       <- ok(opened.session.close)
      yield
        assert(receipt.journalReconcile.isDefined, receipt)
        assert(receipt.swapped)
        assertEquals(receipt.journalReset, None)
        assertEquals(doc, live)
    }
  }

  test(
    "the narrower window: a crash before the swap then recovers only what the journal holds"
  ) {
    bundle.use { views =>
      for
        faulty  <- views.flatMap(FaultyStore.over)
        session <- created(faulty, t2)
        _       <- ok(session.perform(edits(0)))
        _       <- faulty.arm(1, Fault.Before, Failure.Report)
        _       <- ok(session.perform(edits(1)))
        _       <- faulty.reset
        _       <- faulty.armNamed("replace project.journal", Fault.Before, Failure.Report)
        _       <- faulty.armNamed("swap manifest", Fault.Before, Failure.Crash)
        crashed <- session.save.attempt
        _       <- die(faulty)
        opened  <- reopen(views)
        offer   <- offerOf(opened.report)
        _       <- ok(opened.session.close)
      yield
        assertEquals(crashed.left.toOption, Some(Crashed))
        assertEquals(offer.restores.size, 1)
        assertEquals(offer.recovered, replayed(1))
    }
  }

/** The in-memory store passes the recovery fault suite. */
class RecoveryFaultSuite extends RecoveryFaultConformance:
  def bundle: Resource[IO, IO[ProjectStore[IO]]] =
    Resource.eval(InMemoryProjectStore.create[IO]).map(IO.pure)
