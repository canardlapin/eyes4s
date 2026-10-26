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
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.command.{Command, JournalEntry}
import eyes4s.studio.core.document.{DocumentSamples, PanelLetter}
import java.nio.charset.StandardCharsets.UTF_8

/** Exercise the same recovery/deletion contract against memory and disk. */
abstract class SavedPartRetentionConformance extends SessionConformance:
  private val base  = withoutJobs(DocumentSamples.t2)
  private val panel = PanelLetter.of("A").fold(e => fail(e.toString), identity)
  private def title(session: ProjectSession[IO], value: String): IO[Unit] =
    ok(
      session.perform(
        JournalEntry.Apply(Command.RetitlePanel(base.figures.head.id, panel, value))
      )
    ).void
  private def bytes(s: String): IArray[Byte]                   = IArray.from(s.getBytes(UTF_8))
  private def path(area: BundleArea, name: String): BundlePath =
    BundlePath.in(area, name).fold(e => fail(e.message), identity)
  private def manifest(store: ProjectStore[IO]): IO[ProjectManifest] =
    ok(store.readManifest).flatMap(b =>
      IO.fromEither(ProjectBundle.readManifest(b).leftMap(e => new AssertionError(e.message)))
    )

  private def prepared(store: ProjectStore[IO]): IO[(ProjectSession[IO], Vector[BundlePath])] =
    for
      session <- created(store, base)
      first   <- manifest(store)
      _       <- title(session, "second save")
      _       <- ok(session.save)
      second  <- manifest(store)
      retired = first.parts.all.map(_.path).filterNot(second.parts.all.map(_.path).toSet)
      _ <- IO(assert(retired.nonEmpty))
      _ <- title(session, "third save")
    yield (session, retired)

  test(
    "repeated saves retain current and previous parts and bound changing figure generations"
  ) {
    bundle.use { views =>
      for
        store   <- views
        session <- created(store, base)
        _ <- (1 to 12).toVector.traverse_(n => title(session, s"save $n") *> ok(session.save))
        current   <- manifest(store)
        prevBytes <- ok(store.readSidecar(Sidecar.PreviousManifest))
        previous  <- ok(ProjectBundle.openFrom(store, prevBytes))
        paths     <- ok(store.list)
        _         <- ok(session.close)
        opened    <- reopen(views)
        _         <- ok(opened.session.close)
      yield {
        assertEquals(paths.filter(_.area == BundleArea.Figures).size, base.figures.size + 1)
        assertEquals(
          paths.toSet,
          (current.parts.all ++ previous.manifest.parts.all).map(_.path).toSet
        )
        assertEquals(opened.report.source, ManifestSource.Current)
        assert(opened.report.science.verified)
      }
    }
  }

  test(
    "collection removes only retired manifest parts and preserves unrelated files and archives"
  ) {
    bundle.use { views =>
      for
        store              <- views
        (session, retired) <- prepared(store)
        _                  <- ok(session.close)
        lock               <- ok(store.acquire(alice))
        extras = Vector(
          path(BundleArea.Figures, "unindexed.json"),
          path(BundleArea.Inputs, "original.csv"),
          path(BundleArea.Runs, "archive/payload.bin"),
          path(BundleArea.Cache, "keep.bin")
        )
        _        <- extras.traverse_(p => ok(store.write(lock, p, bytes(p.value))))
        _        <- ok(store.release(lock))
        reopened <- reopen(views)
        offer = reopened.report.journal match
          case JournalFinding.Pending(Recovery.Offered(value)) => value
          case other => fail(s"missing recovery $other")
        receipt   <- ok(reopened.session.accept(offer))
        remaining <- ok(store.list)
        _         <- ok(reopened.session.close)
      yield {
        assertEquals(receipt.retention, PartRetention.Collected(retired, Vector.empty))
        assert(extras.forall(remaining.contains))
        assert(!retired.exists(remaining.contains))
      }
    }
  }

  test("an archived journal pins every old part even when its contents are unreadable") {
    bundle.use { views =>
      for
        store        <- views
        (session, _) <- prepared(store)
        before       <- ok(store.list)
        _            <- ok(session.close)
        lock         <- ok(store.acquire(alice))
        archived = path(BundleArea.Journals, "archive/unknown.jsonl")
        _      <- ok(store.write(lock, archived, bytes("unreadable historical base")))
        _      <- ok(store.release(lock))
        opened <- reopen(views)
        offer = opened.report.journal match
          case JournalFinding.Pending(Recovery.Offered(value)) => value
          case other => fail(s"missing recovery $other")
        receipt <- ok(opened.session.accept(offer))
        after   <- ok(store.list)
        _       <- ok(opened.session.close)
      yield {
        assertEquals(receipt.retention, PartRetention.ArchivedJournals(Vector(archived)))
        assert(before.forall(after.contains))
      }
    }
  }

  test("a failed journal reset saves successfully but deletes nothing") {
    bundle.use { views =>
      for
        faulty       <- views.flatMap(FaultyStore.over)
        (session, _) <- prepared(faulty)
        before       <- ok(faulty.list)
        _            <- faulty.armNamed("replace project.journal", Fault.Before, Failure.Report)
        receipt      <- ok(session.save)
        after        <- ok(faulty.list)
        mutations    <- faulty.mutations
        _            <- ok(session.close)
      yield {
        assert(receipt.journalReset.nonEmpty)
        assert(receipt.retention.isInstanceOf[PartRetention.Deferred])
        assert(before.forall(after.contains))
        assert(!mutations.exists(_.startsWith("delete ")))
      }
    }
  }

  test("a retired part reused by the new current save is preserved") {
    bundle.use { views =>
      for
        store        <- views
        (session, _) <- prepared(store)
        _            <- title(session, base.figures.head.panels.head.title)
        _            <- ok(session.save)
        _            <- ok(session.close)
        reopened     <- reopen(views)
        current      <- reopened.session.document
        _            <- ok(reopened.session.close)
      yield assertEquals(current.figures, base.figures)
    }
  }

  test("a part needed only by the retained previous save survives collection") {
    bundle.use { views =>
      for
        store        <- views
        (session, _) <- prepared(store)
        _ <- ok(session.perform(JournalEntry.Apply(Command.DeleteFigure(base.figures.last.id))))
        _ <- ok(session.save)
        prev   <- ok(store.readSidecar(Sidecar.PreviousManifest))
        opened <- ok(ProjectBundle.openFrom(store, prev))
        _      <- ok(session.close)
      yield assertEquals(opened.document.figures.size, base.figures.size)
    }
  }

  test("a malformed retired manifest defers collection without invalidating a saved document") {
    bundle.use { views =>
      for
        store        <- views
        (session, _) <- prepared(store)
        before       <- ok(store.list)
        _            <- ok(session.close)
        lock         <- ok(store.acquire(alice))
        _            <- ok(
          store.replaceSidecar(
            lock,
            Sidecar.PreviousManifest,
            bytes("broken previous manifest")
          )
        )
        _      <- ok(store.release(lock))
        opened <- reopen(views)
        offer = opened.report.journal match
          case JournalFinding.Pending(Recovery.Offered(value)) => value
          case other => fail(s"missing recovery $other")
        receipt <- ok(opened.session.accept(offer))
        after   <- ok(store.list)
        _       <- ok(opened.session.close)
      yield {
        assert(receipt.retention.isInstanceOf[PartRetention.Deferred])
        assert(before.forall(after.contains))
      }
    }
  }

  for fault <- Vector(Fault.Before, Fault.After) do
    test(s"reported deletion failure $fault does not invalidate the successful save") {
      bundle.use { views =>
        for
          faulty             <- views.flatMap(FaultyStore.over)
          (session, retired) <- prepared(faulty)
          expected           <- session.document
          _       <- faulty.armNamed(s"delete ${retired.head.value}", fault, Failure.Report)
          receipt <- ok(session.save)
          _       <- ok(session.close)
          opened  <- reopen(views)
          actual  <- opened.session.document
          _       <- ok(opened.session.close)
        yield {
          assertEquals(actual, expected)
          assert(receipt.retention.warnings.nonEmpty)
          assertEquals(opened.report.journal, JournalFinding.Clean)
        }
      }
    }
    test(s"crash $fault retired-part deletion preserves both current and previous recovery") {
      bundle.use { views =>
        for
          faulty             <- views.flatMap(FaultyStore.over)
          (session, retired) <- prepared(faulty)
          expected           <- session.document
          _      <- faulty.armNamed(s"delete ${retired.head.value}", fault, Failure.Crash)
          result <- session.save.attempt
          _      <- die(faulty)
          store  <- views
          previousBytes <- ok(store.readSidecar(Sidecar.PreviousManifest))
          previous      <- ok(ProjectBundle.openFrom(store, previousBytes))
          opened        <- reopen(views)
          actual        <- opened.session.document
          _             <- ok(opened.session.close)
        yield {
          assertEquals(result.left.toOption, Some(Crashed))
          assertEquals(actual, expected)
          assert(previous.science.verified)
          assertEquals(opened.report.source, ManifestSource.Current)
        }
      }
    }

class SavedPartRetentionSuite extends SavedPartRetentionConformance:
  def bundle: Resource[IO, IO[ProjectStore[IO]]] =
    Resource.eval(InMemoryProjectStore.create[IO]).map(IO.pure)
