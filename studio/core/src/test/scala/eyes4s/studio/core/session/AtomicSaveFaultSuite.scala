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
import eyes4s.studio.core.command.CommandSamples
import eyes4s.studio.core.document.DocumentSamples.t2
import eyes4s.studio.core.document.StudioDocument

/** Atomic save and the single-writer lock (ticket S2.4a), run against every
  * [[ProjectStore]]: [[AtomicSaveFaultSuite]] here and the file store's suite
  * in studio-desktop.
  *
  * The fault tests kill (or fail) a save at every one of its store
  * mutations, before and after the store performs it, then reopen the bundle
  * as a new process. The project must open valid, with verified science, at
  * the old document or the new one and never a mix; when it opens at the old
  * one, the autosave journal must restore the new one.
  */
abstract class AtomicSaveFaultConformance extends SessionConformance:

  /** t2 edited by the story session: a figure edit, view changes, undo and
    * redo, then Save & run. Its new parts are an analysis revision and a run.
    */
  private val edits = CommandSamples.session

  /** t2 as its bundle holds it: without session-only job handles. */
  private val base = withoutJobs(t2)

  /** A bundle holding t2 with the story session performed and unsaved, over
    * a fault-injecting store; the session and the edited document.
    */
  private def edited(
      views: IO[ProjectStore[IO]]
  ): IO[(FaultyStore, ProjectSession[IO], StudioDocument)] =
    for
      faulty  <- views.flatMap(FaultyStore.over)
      session <- created(faulty, t2)
      _       <- performAll(session, edits)
      after   <- session.document
    yield (faulty, session, after)

  /** The mutations one save makes, from a run with no fault. */
  private val steps: IO[Vector[String]] =
    bundle.use { views =>
      for
        (faulty, session, _) <- edited(views)
        _                    <- faulty.reset
        _                    <- ok(session.save)
        steps                <- faulty.mutations
      yield steps
    }

  test(
    "a save writes the new parts, keeps the previous manifest, swaps, then resets the journal"
  ) {
    steps.map { s =>
      assert(s.size >= 4, s)
      assert(s.dropRight(3).forall(_.startsWith("write ")), s)
      assert(s.dropRight(3).exists(_.startsWith("write runs/")), s)
      assertEquals(
        s.takeRight(3),
        Vector("replace project.previous.json", "swap manifest", "replace project.journal")
      )
    }
  }

  for fault <- Vector(Fault.Before, Fault.After)
  do
    test(
      s"a crash ${fault.toString.toLowerCase} each save step reopens at the old or the new document, never a mix"
    ) {
      steps.flatMap { all =>
        val swap = all.indexOf("swap manifest") + 1
        (1 to all.size).toVector.traverse_ { k =>
          bundle.use { views =>
            val clue = s"crash ${fault.toString.toLowerCase} step $k (${all(k - 1)})"
            for
              (faulty, session, after) <- edited(views)
              _                        <- faulty.arm(k, fault, Failure.Crash)
              crashed                  <- session.save.attempt
              _                        <- die(faulty)
              opened                   <- reopen(views)
              reopened                 <- opened.session.document
              expectNew = k > swap || (k == swap && fault == Fault.After)
              restored <- opened.report.journal match
                case JournalFinding.Pending(Recovery.Offered(offer)) =>
                  ok(opened.session.accept(offer)) *> opened.session.document
                case _ => opened.session.document
              _      <- ok(opened.session.close)
              again  <- reopen(views)
              final_ <- again.session.document
              _      <- ok(again.session.close)
            yield
              assertEquals(crashed.left.toOption, Some(Crashed), clue)
              assert(opened.report.science.verified, clue)
              assertEquals(opened.report.source, ManifestSource.Current, clue)
              assertEquals(reopened, if expectNew then after else base, clue)
              opened.report.journal match
                case JournalFinding.Pending(Recovery.Offered(offer)) =>
                  assert(!expectNew, clue)
                  assertEquals(offer.recovered, after, clue)
                case JournalFinding.Superseded(_) =>
                  // The swap happened; the journal reset did not.
                  assert(expectNew, clue)
                case JournalFinding.Clean => assert(expectNew, clue)
                case other                => fail(s"$clue: unexpected journal $other")
              assertEquals(restored, after, clue)
              assertEquals(final_, after, clue)
              assertEquals(again.report.journal, JournalFinding.Clean, clue)
          }
        }
      }
    }

  for fault <- Vector(Fault.Before, Fault.After)
  do
    test(
      s"a store error ${fault.toString.toLowerCase} each save step leaves the session able to save"
    ) {
      steps.flatMap { all =>
        (1 to all.size).toVector.traverse_ { k =>
          bundle.use { views =>
            val clue = s"error ${fault.toString.toLowerCase} step $k (${all(k - 1)})"
            for
              (faulty, session, after) <- edited(views)
              _                        <- faulty.arm(k, fault, Failure.Report)
              first                    <- session.save
              _                        <- faulty.reset
              retry                    <- session.save
              _                        <- ok(session.close)
              opened                   <- reopen(views)
              reopened                 <- opened.session.document
              _                        <- ok(opened.session.close)
            yield
              // Only the journal reset may fail without failing the save; a
              // swap the store reported as failed but performed is read back.
              first match
                case Left(_) =>
                  assert(!(fault == Fault.After && all(k - 1) == "swap manifest"), clue)
                case Right(receipt) =>
                  assert(
                    all(k - 1) == "replace project.journal" && receipt.journalReset.isDefined ||
                      fault == Fault.After && all(k - 1) == "swap manifest",
                    clue
                  )
              assert(retry.isRight, s"$clue: $retry")
              assertEquals(reopened, after, clue)
              assertEquals(opened.report.journal, JournalFinding.Clean, clue)
          }
        }
      }
    }

  test("the last valid manifest is kept: a damaged project opens at the previous save") {
    bundle.use { views =>
      for
        store   <- views
        session <- created(store, t2)
        before  <- ok(store.readManifest)
        _       <- performAll(session, edits)
        after   <- session.document
        _       <- ok(session.save)
        kept    <- ok(store.readSidecar(Sidecar.PreviousManifest))
        current <- ok(store.readManifest)
        _       <- ok(session.close)
        // Damage a part only the new manifest names, as a disk fault would.
        newParts <- IO.fromEither(
          (ProjectBundle.readManifest(current), ProjectBundle.readManifest(before)).tupled
            .map((n, o) => n.parts.all.map(_.path).diff(o.parts.all.map(_.path)))
            .leftMap(e => AssertionError(e.message))
        )
        vandal <- ok(store.acquire(owner("disk fault")))
        _      <- ok(store.write(vandal, newParts.head, BundleSamples.utf8("{}")))
        _      <- ok(store.release(vandal))
        opened <- reopen(views)
        doc    <- opened.session.document
        saved  <- ok(opened.session.save)
        _      <- ok(opened.session.close)
        again  <- reopen(views)
        _      <- ok(again.session.close)
      yield
        assertEquals(ByteDigest.sha256(kept), ByteDigest.sha256(before))
        opened.report.source match
          case ManifestSource.Previous(BundleError.PartLength(path, _, _)) =>
            assertEquals(path, newParts.head)
          case other => fail(s"expected the previous manifest, got $other")
        assertEquals(doc, base)
        assert(after != base)
        assert(saved.swapped)
        assertEquals(again.report.source, ManifestSource.Current)
    }
  }

  test("a second writer is refused with a message naming the holder (E2E-19)") {
    bundle.use { views =>
      for
        store   <- views
        session <- created(store, t2)
        other   <- views
        refused <- ProjectSession.open(other, bob)
        _       <- ok(session.close)
        granted <- reopen(views, bob)
        _       <- ok(granted.session.close)
      yield refused match
        case Left(e @ SessionError.WriterRefused(requester, holder)) =>
          assertEquals(requester, bob)
          assertEquals(holder, Some(alice))
          assertEquals(
            e.message,
            "Eyes Studio · bob cannot open the project for writing: it is open in " +
              "Eyes Studio · alice. Close it there, or open a copy."
          )
        case other => fail(s"expected a refusal, got $other")
    }
  }

  test("a closed session refuses every write and names its owner") {
    bundle.use { views =>
      for
        store   <- views
        session <- created(store, t2)
        _       <- ok(session.close)
        save    <- session.save
        perform <- session.perform(edits.head)
        again   <- session.close
      yield
        assertEquals(save, Left(SessionError.Closed(alice)))
        assertEquals(perform, Left(SessionError.Closed(alice)))
        assertEquals(again, Left(SessionError.Closed(alice)))
    }
  }

  test("create refuses a bundle that already holds a project, and open refuses an empty one") {
    bundle.use { views =>
      for
        store  <- views
        empty  <- ProjectSession.open(store, alice)
        first  <- created(store, t2)
        _      <- ok(first.close)
        second <- ProjectSession.create(
          store,
          bob,
          t2,
          SharingOptions.complete,
          BundleSamples.inputsFor(t2)
        )
        lockFree <- reopen(views)
        _        <- ok(lockFree.session.close)
      yield
        assertEquals(empty.map(_ => ()), Left(SessionError.NoProject))
        assertEquals(second.map(_ => ()), Left(SessionError.ProjectExists))
    }
  }

  test("saving an unchanged document swaps nothing and writes no part") {
    bundle.use { views =>
      for
        store   <- views
        session <- created(store, t2)
        receipt <- ok(session.save)
        unsaved <- session.unsaved
        _       <- ok(session.close)
      yield
        assert(!receipt.swapped)
        assertEquals(receipt.written, Vector.empty)
        assertEquals(receipt.journalReset, None)
        assert(!unsaved)
    }
  }

/** The in-memory store passes the atomic-save conformance suite. */
class AtomicSaveFaultSuite extends AtomicSaveFaultConformance:
  def bundle: Resource[IO, IO[ProjectStore[IO]]] =
    Resource.eval(InMemoryProjectStore.create[IO]).map(IO.pure)
