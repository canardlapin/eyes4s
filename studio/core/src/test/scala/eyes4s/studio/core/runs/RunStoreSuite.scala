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

package eyes4s.studio.core.runs

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import eyes4s.codec.{ArtifactName, ByteDigest, CanonicalDigest}
import eyes4s.studio.core.backend.RunId
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.command.{Command, History}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.document.DocumentSamples.{t1, t3}
import eyes4s.studio.core.fixture.StoryMoments
import munit.CatsEffectSuite
import org.scalacheck.Gen
import org.scalacheck.rng.Seed

import java.nio.charset.StandardCharsets.UTF_8

/** The run store (ticket S2.6): archives under `runs/<id>/archive/`, read
  * lazily, kept while any reachable figure binds their run, and pruned only
  * with confirmation.
  */
class RunStoreSuite extends CatsEffectSuite:

  private def ok[E, A](io: IO[Either[E, A]]): IO[A] =
    io.flatMap(_.fold(e => IO.raiseError(new AssertionError(e.toString)), IO.pure))

  private def right[E, A](e: Either[E, A]): A =
    e.fold(err => throw new AssertionError(err.toString), identity)

  private val me = right(LockOwner.of("RunStoreSuite"))

  private def bytes(text: String): IArray[Byte] = IArray.unsafeFromArray(text.getBytes(UTF_8))
  private def name(text: String): ArtifactName  = right(ArtifactName.of(text))
  private val result                            = name("study-result.json")
  private val density                           = name("density/σ2.bin")
  private val copy                              = name("density/σ2-copy.bin")

  /** A fake archive for `run`: what eyes4s would hand over, as opaque bytes. */
  private def archiveOf(run: RunRef): RunArchive =
    right(
      RunArchive.of(
        run,
        Vector(
          result  -> bytes(s"result of ${run.id.label}"),
          density -> bytes(s"density of ${run.id.label} " * 8),
          copy    -> bytes(s"density of ${run.id.label} " * 8)
        )
      )
    )

  private def same(a: IArray[Byte], b: IArray[Byte]): Boolean = a.toSeq == b.toSeq

  private def sameArchive(a: RunArchive, b: RunArchive): Boolean =
    a.index == b.index && a.files.map(_._2.toSeq) == b.files.map(_._2.toSeq)

  private def completed(run: RunRef): RunRef = run.copy(state = RunLifecycle.Completed)

  /** t3 after run 8 completed: runs 5–8 exist; Figure 1 binds run 7,
    * Figure 2 binds run 5; the view shows run 7.
    */
  private val afterRun8: StudioDocument =
    right(
      StudioDocument.of(
        t3.datasets,
        t3.analyses,
        t3.draft,
        t3.runs.map(r => if r.id == StoryMoments.run8 then completed(r) else r),
        t3.reporting,
        t3.figures,
        t3.presentation,
        Vector.empty
      )
    )

  private val figure1 = right(FigureId.of(1))
  private val figure2 = right(FigureId.of(2))

  private def runRef(document: StudioDocument, run: RunId): RunRef = document.run(run).get

  /** A store, its lock, and the run store over it. */
  private def fresh: IO[(ProjectStore[IO], WriterLock, RunStore[IO])] =
    for
      store <- InMemoryProjectStore.create[IO]
      lock  <- ok(store.acquire(me))
    yield (store, lock, RunStore(store))

  /** A store that records every entry read. */
  private final class Recording(
      underlying: ProjectStore[IO],
      reads: Ref[IO, Vector[BundlePath]]
  ) extends ProjectStore[IO]:
    def read(path: BundlePath) = reads.update(_ :+ path) *> underlying.read(path)
    def list                   = underlying.list
    def write(lock: WriterLock, path: BundlePath, b: IArray[Byte]) =
      underlying.write(lock, path, b)
    def delete(lock: WriterLock, path: BundlePath) = underlying.delete(lock, path)
    def readManifest                               = underlying.readManifest
    def swapManifest(lock: WriterLock, e: Option[ByteDigest], n: IArray[Byte]) =
      underlying.swapManifest(lock, e, n)
    def acquire(owner: LockOwner)                                 = underlying.acquire(owner)
    def release(lock: WriterLock)                                 = underlying.release(lock)
    def readSidecar(file: Sidecar)                                = underlying.readSidecar(file)
    def appendSidecar(l: WriterLock, f: Sidecar, b: IArray[Byte]) =
      underlying.appendSidecar(l, f, b)
    def replaceSidecar(l: WriterLock, f: Sidecar, b: IArray[Byte]) =
      underlying.replaceSidecar(l, f, b)
    def removeSidecar(l: WriterLock, f: Sidecar) = underlying.removeSidecar(l, f)

  /** A store whose deletes fail after `allowed` of them succeed. */
  private final class FailingDeletes(underlying: ProjectStore[IO], left: Ref[IO, Int])
      extends ProjectStore[IO]:
    def read(path: BundlePath)                                     = underlying.read(path)
    def list                                                       = underlying.list
    def write(lock: WriterLock, path: BundlePath, b: IArray[Byte]) =
      underlying.write(lock, path, b)
    def delete(lock: WriterLock, path: BundlePath) =
      left.getAndUpdate(_ - 1).flatMap { n =>
        if n > 0 then underlying.delete(lock, path)
        else IO.pure(Left(StoreError.Unwritable(path.value, "injected fault")))
      }
    def readManifest = underlying.readManifest
    def swapManifest(lock: WriterLock, e: Option[ByteDigest], n: IArray[Byte]) =
      underlying.swapManifest(lock, e, n)
    def acquire(owner: LockOwner)                                 = underlying.acquire(owner)
    def release(lock: WriterLock)                                 = underlying.release(lock)
    def readSidecar(file: Sidecar)                                = underlying.readSidecar(file)
    def appendSidecar(l: WriterLock, f: Sidecar, b: IArray[Byte]) =
      underlying.appendSidecar(l, f, b)
    def replaceSidecar(l: WriterLock, f: Sidecar, b: IArray[Byte]) =
      underlying.replaceSidecar(l, f, b)
    def removeSidecar(l: WriterLock, f: Sidecar) = underlying.removeSidecar(l, f)

  /** Store the archive of every completed run of `document`. */
  private def putAll(
      runs: RunStore[IO],
      lock: WriterLock,
      document: StudioDocument
  ): IO[Unit] =
    document.runs
      .filter(_.state == RunLifecycle.Completed)
      .traverse_(r => ok(runs.put(lock, archiveOf(r))))

  // --- Acceptance: figure rebinding ----------------------------------------

  test("a figure bound to run 5 still renders after runs 6-8 exist") {
    for
      (_, lock, runs) <- fresh
      // t1: run 5 is the only run, and Figure 2 binds it.
      run5 = runRef(t1, StoryMoments.run5)
      _      <- ok(runs.put(lock, archiveOf(run5)))
      before <- ok(runs.figure(t1, figure2))
      // Runs 6, 7 and 8 follow; run 8 is the latest and run 7 is shown.
      _       <- putAll(runs, lock, afterRun8)
      after   <- ok(runs.figure(afterRun8, figure2))
      storage <- ok(runs.storage(RetentionBasis.of(afterRun8)))
      plan = right(PrunePlan.everything(storage))
      _      <- ok(runs.prune(lock, plan.confirm, RetentionBasis.of(afterRun8)))
      pruned <- ok(runs.figure(afterRun8, figure2))
      run8   <- runs.load(runRef(afterRun8, StoryMoments.run8))
    yield
      assertEquals(before.run, StoryMoments.run5)
      assertEquals(after.run, StoryMoments.run5)
      assert(sameArchive(after, archiveOf(run5)))
      assert(same(after.bytes(result).get, bytes("result of run 5")))
      assertEquals(plan.runs, Vector(StoryMoments.run8))
      assert(sameArchive(pruned, archiveOf(run5)))
      assertEquals(
        run8.left.map(_.isInstanceOf[RunStoreError.NoArchive]),
        Left(true),
        "only the unbound, unshown run 8 was pruned"
      )
  }

  test("a figure renders from its bound run, never the latest one") {
    for
      (_, lock, runs) <- fresh
      _               <- putAll(runs, lock, afterRun8)
      one             <- ok(runs.figure(afterRun8, figure1))
      two             <- ok(runs.figure(afterRun8, figure2))
      missing         <- runs.figure(afterRun8, right(FigureId.of(3)))
    yield
      assertEquals(one.run, StoryMoments.run7)
      assertEquals(two.run, StoryMoments.run5)
      assertEquals(
        missing,
        Left(RunStoreError.UnknownFigure(right(FigureId.of(3)), Vector(figure1, figure2)))
      )
  }

  // --- Acceptance: pruning never removes a figure-bound run -----------------

  test("a figure-bound run cannot be planned for pruning") {
    for
      (_, lock, runs) <- fresh
      _               <- putAll(runs, lock, afterRun8)
      storage         <- ok(runs.storage(RetentionBasis.of(afterRun8)))
    yield
      assertEquals(
        PrunePlan.of(storage, Vector(StoryMoments.run5)),
        Left(
          RunStoreError.KeptRun(
            StoryMoments.run5,
            Vector(KeepReason.Figure(figure2, BindingSource.Current))
          )
        )
      )
      assertEquals(
        PrunePlan.of(storage, Vector(StoryMoments.run7)),
        Left(
          RunStoreError.KeptRun(
            StoryMoments.run7,
            Vector(KeepReason.Figure(figure1, BindingSource.Current), KeepReason.Shown)
          )
        )
      )
      assertEquals(storage.kept.map(_.run), Vector(StoryMoments.run5, StoryMoments.run7))
      assertEquals(storage.prunable.map(_.run), Vector(StoryMoments.run8))
  }

  test("a run bound after the plan was confirmed refuses the whole prune and deletes nothing") {
    val rebound = right(
      History
        .start(afterRun8)
        .apply(Command.BindFigure(figure2, StoryMoments.run8, afterRun8.figures(1).reporting))
    ).history
    for
      (store, lock, runs) <- fresh
      _                   <- putAll(runs, lock, afterRun8)
      storage             <- ok(runs.storage(RetentionBasis.of(afterRun8)))
      plan = right(PrunePlan.everything(storage))
      before  <- ok(store.list)
      refused <- runs.prune(lock, plan.confirm, RetentionBasis.of(rebound.document))
      after   <- ok(store.list)
    yield
      assertEquals(plan.runs, Vector(StoryMoments.run8))
      assertEquals(
        refused,
        Left(
          RunStoreError.KeptRun(
            StoryMoments.run8,
            Vector(KeepReason.Figure(figure2, BindingSource.Current))
          )
        )
      )
      assertEquals(after, before)
  }

  test("a run a figure bound before an undoable rebind is kept for undo") {
    // Rebinding Figure 2 from run 5 to run 8 leaves run 5 bound only in the
    // document that undo returns to.
    val history = right(
      History
        .start(afterRun8)
        .apply(Command.BindFigure(figure2, StoryMoments.run8, afterRun8.figures(1).reporting))
    ).history
    val undone = right(history.undo).history
    for
      (_, lock, runs) <- fresh
      _               <- putAll(runs, lock, afterRun8)
      current         <- ok(runs.storage(RetentionBasis.of(history.document)))
      withUndo        <- ok(runs.storage(RetentionBasis.of(history)))
      // After the undo, run 8 is bound only in the document redo returns to.
      withRedo <- ok(runs.storage(RetentionBasis.of(undone)))
    yield
      assertEquals(
        current.row(StoryMoments.run5).map(_.isInstanceOf[RunRetention.Prunable]),
        Some(true)
      )
      assertEquals(
        withUndo.row(StoryMoments.run5),
        Some(
          RunRetention.Kept(
            current.row(StoryMoments.run5).get.record,
            Vector(KeepReason.Figure(figure2, BindingSource.Undo))
          )
        )
      )
      assertEquals(
        withRedo.row(StoryMoments.run8).collect { case RunRetention.Kept(_, r) => r },
        Some(Vector(KeepReason.Figure(figure2, BindingSource.Undo)))
      )
  }

  test("the saved and previous documents' figures keep their runs") {
    val unbound = right(
      History.start(afterRun8).apply(Command.DeleteFigure(figure2))
    ).history.document
    for
      (_, lock, runs) <- fresh
      _               <- putAll(runs, lock, afterRun8)
      alone           <- ok(runs.storage(RetentionBasis.of(unbound)))
      saved           <- ok(runs.storage(RetentionBasis.of(unbound).withSaved(afterRun8)))
      previous        <- ok(runs.storage(RetentionBasis.of(unbound).withPrevious(t1)))
    yield
      assert(alone.row(StoryMoments.run5).exists(_.isInstanceOf[RunRetention.Prunable]))
      assertEquals(
        saved.row(StoryMoments.run5).collect { case RunRetention.Kept(_, r) => r },
        Some(Vector(KeepReason.Figure(figure2, BindingSource.Saved)))
      )
      assertEquals(
        previous.row(StoryMoments.run5).collect { case RunRetention.Kept(_, r) => r },
        Some(Vector(KeepReason.Figure(figure2, BindingSource.Previous)))
      )
  }

  test("the shown, running and ready runs are kept") {
    val basis = RetentionBasis.of(t3).withReady(StoryMoments.run6)
    assertEquals(basis.reasons(StoryMoments.run7).contains(KeepReason.Shown), true)
    assertEquals(basis.reasons(StoryMoments.run8), Vector(KeepReason.Running))
    assertEquals(basis.reasons(StoryMoments.run6), Vector(KeepReason.Ready))
    assertEquals(basis.runs, Vector(5, 6, 7, 8).map(RunId(_)))
  }

  test("property: pruning everything prunable leaves every figure's archive loadable") {
    (1 to 120).toVector.traverse_ { seed =>
      val document = DocumentGen.document.pureApply(Gen.Parameters.default, Seed(seed.toLong))
      for
        (_, lock, runs) <- fresh
        _               <- putAll(runs, lock, document)
        basis = RetentionBasis.of(document)
        storage <- ok(runs.storage(basis))
        _       <- PrunePlan
          .everything(storage)
          .toOption
          .traverse_(plan => ok(runs.prune(lock, plan.confirm, basis)))
        after   <- ok(runs.records)
        figures <- document.figures.traverse(f => runs.figure(document, f.id).map(f -> _))
      yield
        val stored = after.map(_.run)
        figures.foreach { (f, rendered) =>
          val run = document.run(f.run).get
          if run.state == RunLifecycle.Completed then
            assert(
              rendered.exists(sameArchive(_, archiveOf(run))),
              s"seed $seed, ${f.id.label}: $rendered"
            )
        }
        assertEquals(stored, storage.kept.map(_.run), s"seed $seed")
    }
  }

  // --- Lazy loading ----------------------------------------------------------

  test("opening a project reads no archive; an index or an entry reads only itself") {
    for
      store <- InMemoryProjectStore.create[IO]
      lock  <- ok(store.acquire(me))
      _     <- ok(RunStore(store).put(lock, archiveOf(runRef(afterRun8, StoryMoments.run5))))
      // The project's own parts and manifest.
      encoded = right(
        ProjectBundle.encode(
          afterRun8,
          SharingOptions.complete,
          BundleSamples.inputsFor(afterRun8)
        )
      )
      _     <- ok(ProjectBundle.save(store, lock, None, encoded))
      reads <- Ref.of[IO, Vector[BundlePath]](Vector.empty)
      recording = Recording(store, reads)
      runs      = RunStore(recording)
      run5      = runRef(afterRun8, StoryMoments.run5)
      _       <- ok(ProjectBundle.open(recording))
      onOpen  <- reads.getAndSet(Vector.empty)
      index   <- ok(runs.index(run5))
      onIndex <- reads.getAndSet(Vector.empty)
      one     <- ok(runs.entry(index, result))
      onEntry <- reads.getAndSet(Vector.empty)
      _       <- ok(runs.records)
      onList  <- reads.getAndSet(Vector.empty)
      indexPath   = right(ArchivePaths.index(StoryMoments.run5))
      resultEntry = index.entry(result).get
    yield
      assert(onOpen.nonEmpty)
      assert(onOpen.forall(ArchivePaths.owner(_).isEmpty), onOpen)
      assertEquals(onIndex, Vector(indexPath))
      assertEquals(
        onEntry,
        Vector(right(ArchivePaths.content(StoryMoments.run5, resultEntry.sha256)))
      )
      assert(same(one, bytes("result of run 5")))
      assertEquals(onList, Vector(indexPath), "listing sizes reads indexes only")
  }

  // --- Sizes -----------------------------------------------------------------

  test(
    "sizes: an archive is its index and its distinct entries; totals split kept and prunable"
  ) {
    for
      (store, lock, runs) <- fresh
      _                   <- putAll(runs, lock, afterRun8)
      storage             <- ok(runs.storage(RetentionBasis.of(afterRun8)))
      run5Bytes           <- storage
        .row(StoryMoments.run5)
        .get
        .record
        .files
        .traverse(p => ok(store.read(p)))
    yield
      val run5 = storage.row(StoryMoments.run5).get.record
      // density and its copy share one stored file.
      assertEquals(run5.files.size, 3)
      assertEquals(run5.size, Some(run5Bytes.map(_.length.toLong).sum))
      assertEquals(storage.bytes, storage.rows.flatMap(_.record.size).sum)
      assertEquals(storage.keptBytes + storage.prunableBytes, storage.bytes)
      assertEquals(storage.prunableBytes, storage.row(StoryMoments.run8).get.record.size.get)
  }

  test("a prune reports the bytes it frees and deletes entries before the index") {
    for
      (store, lock, runs) <- fresh
      _                   <- putAll(runs, lock, afterRun8)
      basis = RetentionBasis.of(afterRun8)
      storage <- ok(runs.storage(basis))
      plan = right(PrunePlan.everything(storage))
      report <- ok(runs.prune(lock, plan.confirm, basis))
      left   <- ok(store.list)
    yield
      assertEquals(report.runs, Vector(StoryMoments.run8))
      assertEquals(report.freedBytes, plan.bytes)
      assertEquals(plan.bytes, storage.prunableBytes)
      assertEquals(report.deleted.last, right(ArchivePaths.index(StoryMoments.run8)))
      assertEquals(report.deleted.toSet, storage.row(StoryMoments.run8).get.record.files.toSet)
      assert(left.forall(p => ArchivePaths.owner(p) != Some(StoryMoments.run8)), left)
  }

  // --- Interrupted writes and prunes -------------------------------------------

  test("an interrupted prune leaves a prunable archive; the next plan finishes it") {
    for
      (store, lock, _) <- fresh
      left             <- Ref.of[IO, Int](1)
      runs = RunStore(FailingDeletes(store, left))
      _ <- putAll(runs, lock, afterRun8)
      basis = RetentionBasis.of(afterRun8)
      storage <- ok(runs.storage(basis))
      plan = right(PrunePlan.everything(storage))
      failed  <- runs.prune(lock, plan.confirm, basis)
      halfway <- ok(runs.storage(basis))
      stale   <- runs.prune(lock, plan.confirm, basis)
      _       <- left.set(10)
      again = right(PrunePlan.everything(halfway))
      _    <- ok(runs.prune(lock, again.confirm, basis))
      done <- ok(runs.records)
    yield
      assert(failed.isLeft)
      // The index survived the failure, so the archive is still listed, prunable.
      assert(halfway.row(StoryMoments.run8).exists(_.isInstanceOf[RunRetention.Prunable]))
      assert(halfway.row(StoryMoments.run8).exists(_.record.isInstanceOf[ArchiveRecord.Stored]))
      assertEquals(stale, Left(RunStoreError.PlanOutdated(StoryMoments.run8)))
      assertEquals(done.map(_.run), Vector(StoryMoments.run5, StoryMoments.run7))
  }

  test("entries without an index are incomplete: prunable unless the run is kept") {
    val run5 = runRef(afterRun8, StoryMoments.run5)
    val run8 = runRef(afterRun8, StoryMoments.run8)
    for
      (store, lock, runs) <- fresh
      stray5 = right(ArchivePaths.content(run5.id, ByteDigest.sha256(bytes("partial"))))
      stray8 = right(ArchivePaths.content(run8.id, ByteDigest.sha256(bytes("partial"))))
      _       <- ok(store.write(lock, stray5, bytes("partial")))
      _       <- ok(store.write(lock, stray8, bytes("partial")))
      storage <- ok(runs.storage(RetentionBasis.of(afterRun8)))
      plan = right(PrunePlan.everything(storage))
      _    <- ok(runs.prune(lock, plan.confirm, RetentionBasis.of(afterRun8)))
      left <- ok(store.list)
    yield
      assertEquals(
        storage.rows.map(_.record),
        Vector(
          ArchiveRecord.Incomplete(run5.id, Vector(stray5)),
          ArchiveRecord.Incomplete(run8.id, Vector(stray8))
        )
      )
      assertEquals(storage.row(run5.id).map(_.isInstanceOf[RunRetention.Kept]), Some(true))
      assertEquals(plan.runs, Vector(run8.id))
      assert(plan.sizeUnknown)
      assertEquals(left, Vector(stray5))
  }

  // --- Integrity ------------------------------------------------------------------

  test("only a completed run's archive can be stored") {
    val running = runRef(t3, StoryMoments.run8)
    assertEquals(
      RunArchive.of(running, Vector(result -> bytes("x"))).left.map(identity),
      Left(RunStoreError.NotCompleted(StoryMoments.run8, RunLifecycle.Running))
    )
    val run5 = runRef(t3, StoryMoments.run5)
    assertEquals(
      RunArchive.of(run5, Vector.empty).left.map(identity),
      Left(RunStoreError.EmptyArchive(StoryMoments.run5))
    )
    assertEquals(
      RunArchive
        .of(run5, Vector(result -> bytes("a"), result -> bytes("b")))
        .left
        .map(identity),
      Left(RunStoreError.DuplicateEntries(StoryMoments.run5, Vector(result)))
    )
  }

  test("storing the same archive twice writes nothing; another archive for the run collides") {
    val run5  = runRef(afterRun8, StoryMoments.run5)
    val other = right(RunArchive.of(run5, Vector(result -> bytes("another result"))))
    for
      (store, lock, runs) <- fresh
      first               <- ok(runs.put(lock, archiveOf(run5)))
      second              <- ok(runs.put(lock, archiveOf(run5)))
      collided            <- runs.put(lock, other)
    yield
      assertEquals(second, first)
      assert(
        collided.left.exists {
          case RunStoreError.Bundle(BundleError.Collision(path, _, _)) =>
            path == right(ArchivePaths.index(run5.id))
          case _ => false
        },
        collided
      )
  }

  test("a damaged entry, a missing archive and another run's archive are refused") {
    val run5  = runRef(afterRun8, StoryMoments.run5)
    val run7  = runRef(afterRun8, StoryMoments.run7)
    val bound = run5.copy(
      archive =
        CoreBinding.Bound(right(CanonicalDigest.parse[ResultArchiveArtifact]("ab" * 32)))
    )
    for
      (store, lock, runs) <- fresh
      _                   <- ok(runs.put(lock, archiveOf(run5)))
      missing             <- runs.load(run7)
      mismatch            <- runs.load(bound)
      index               <- ok(runs.index(run5))
      entry = index.entry(result).get
      path  = right(ArchivePaths.content(run5.id, entry.sha256))
      _       <- ok(store.delete(lock, path))
      gone    <- runs.entry(index, result)
      _       <- ok(store.write(lock, path, bytes("result of run 6")))
      damaged <- runs.entry(index, result)
      unknown <- runs.entry(index, name("nothing.json"))
    yield
      assertEquals(
        missing,
        Left(RunStoreError.NoArchive(run7.id, right(ArchivePaths.index(run7.id))))
      )
      assertEquals(
        mismatch,
        Left(RunStoreError.ArchiveMismatch(run5.id, bound.archive, run5.archive))
      )
      assertEquals(gone, Left(RunStoreError.MissingEntry(run5.id, result, path)))
      assertEquals(
        damaged,
        Left(
          RunStoreError.EntryDigest(
            run5.id,
            result,
            entry.sha256,
            ByteDigest.sha256(bytes("result of run 6"))
          )
        )
      )
      assertEquals(
        unknown,
        Left(RunStoreError.UnknownEntry(run5.id, name("nothing.json"), index.names))
      )
  }

  test("a plan names only stored runs and at least one") {
    for
      (_, lock, runs) <- fresh
      _               <- putAll(runs, lock, afterRun8)
      storage         <- ok(runs.storage(RetentionBasis.of(afterRun8)))
    yield
      assertEquals(PrunePlan.of(storage, Vector.empty), Left(RunStoreError.NothingToPrune))
      assertEquals(
        PrunePlan.of(storage, Vector(StoryMoments.run6)),
        Left(
          RunStoreError.NotInStorage(
            StoryMoments.run6,
            Vector(StoryMoments.run5, StoryMoments.run7, StoryMoments.run8)
          )
        )
      )
  }

  test(
    "the archive index round-trips through its versioned codec, schema studio.run-archive@1"
  ) {
    val index = archiveOf(runRef(afterRun8, StoryMoments.run5)).index
    val json  = right(ArchiveIndex.encode(index))
    assertEquals(ArchiveIndex.decode(json), Right(index))
    assertEquals(
      json.hcursor.downField("schema").get[String]("name"),
      Right("studio.run-archive")
    )
    assertEquals(index.names, Vector(copy, density, result).sortBy(_.value))
  }

  test("an error names its operands") {
    assert(
      RunStoreError
        .KeptRun(StoryMoments.run5, Vector(KeepReason.Figure(figure2, BindingSource.Current)))
        .message
        .contains("Figure 2 binds it in the open document"),
      "kept"
    )
    assert(RunStoreError.PlanOutdated(StoryMoments.run8).message.contains("run 8"))
  }
