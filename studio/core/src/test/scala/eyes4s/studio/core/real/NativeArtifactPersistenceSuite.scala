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

package eyes4s.studio.core.real

import cats.effect.{IO, Ref}
import cats.syntax.all.*
import eyes4s.codec.{ArtifactName, ByteDigest}
import eyes4s.studio.core.assets.AssetRegistry
import eyes4s.studio.core.artifacts.*
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.command.{Command, JournalEntry, Reducer}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.StoryMoments
import eyes4s.studio.core.runs.{
  ArchivePaths,
  ArchiveRecord,
  RunArchive,
  RunStore,
  RunStoreError
}
import eyes4s.studio.core.session.ProjectSession
import java.nio.charset.StandardCharsets.UTF_8
import munit.CatsEffectSuite

/** Real library bytes, a session writer, and a backend-independent cold reopen. */
class NativeArtifactPersistenceSuite extends CatsEffectSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private def ok[E, A](value: IO[Either[E, A]]): IO[A] = value.map(get)
  private val owner = get(LockOwner.of("native artifact persistence"))
  private val run   = RunId(1)
  private val fixes =
    "participant,phase,trial,occurrence,ordinal,onset_ms,duration_ms,x,y,sample_count\n" +
      "P01,Encoding,enc_01,1,1,0,100,600,300,1\n" +
      "P01,Encoding,enc_02,1,1,0,100,620,320,1\n" +
      "P01,Retrieval,ret_01,1,1,0,100,604,305,1\n"
  private val trials =
    "participant,phase,trial,occurrence,item,response,display_kind,image_file\n" +
      "P01,Encoding,enc_01,1,item-a,,image,item-a.png\n" +
      "P01,Encoding,enc_02,1,item-b,,image,item-b.png\n" +
      "P01,Retrieval,ret_01,1,item-a,Remembered,image,item-a.png\n"
  private lazy val sample   = get(StoryMoments.t2)
  private lazy val prepared =
    val original = sample.datasets.last
    val byRole   = Map(SourceRole.Fixations -> fixes, SourceRole.Trials -> trials)
    val sources  = get(
      Sources.of(
        original.sources.entries.map(source =>
          source.copy(
            bytes = ByteDigest.sha256(IArray.from(byRole(source.role).getBytes(UTF_8))),
            semantic = None
          )
        )
      )
    )
    val spec   = original.copy(parent = None, sources = sources)
    val assets = get(
      AssetRegistry.of(spec.id, sources.trials.get.bytes, spec.geometry.screen, Vector.empty)
    )
    val admitted = get(RealAdmission.admit(spec, fixes, trials, assets))
    val recipe   = sample.analyses.last.recipe.copy(
      grid = get(GridSize.of(8, 8)),
      scales = get(ScaleSet.of(Vector(get(Sigma.of(1)), get(Sigma.of(2)))))
    )
    get(RealPrepared.of(AnalysisRevision(1), spec.id, recipe, admitted))
  private lazy val exported = get(
    NativeArtifacts.build(
      run,
      RealStudyBackend
        .RealRun(prepared, get(prepared.work.run), RealStudyBackend.RunOrigin.Computed),
      NativeArtifactBudget.Default
    )
  )
  private def document(
      lifecycle: RunLifecycle = RunLifecycle.Completed,
      declaredSource: Option[SemanticIdentity] = None,
      units: Option[DeclaredUnits] = None,
      explicitFamilies: Boolean = false
  ): StudioDocument =
    val original = prepared.admitted.spec
    val spec     = original.copy(
      units = units.getOrElse(original.units),
      sources = get(
        Sources.of(
          original.sources.entries.map(entry =>
            if entry.role == SourceRole.Fixations then entry.copy(semantic = declaredSource)
            else entry
          )
        )
      )
    )
    val analysis = AnalysisRevisionSpec(
      prepared.revision,
      spec.id,
      CoreBinding.unbound,
      prepared.recipe,
      sample.analyses.last.studio.copy(preset = Preset.Custom)
    )
    val analyses =
      if explicitFamilies then Vector(analysis, analysis.copy(id = AnalysisRevision(2)))
      else Vector(analysis)
    val families = Option.when(explicitFamilies) {
      get(AnalysisFamilyRegistry.of(
        Vector(get(AnalysisFamily.of(FamilySamples.a, "A")), get(AnalysisFamily.of(FamilySamples.b, "B"))),
        Vector(get(AnalysisFamilyOwner.of(analysis.id, FamilySamples.a)),
          get(AnalysisFamilyOwner.of(AnalysisRevision(2), FamilySamples.b))),
        analyses.map(_.id)
      ))
    }
    get(
      StudioDocument.of(
        Vector(spec),
        analyses,
        None,
        Vector(RunRef(run, analysis.id, spec.id, lifecycle, CoreBinding.unbound)),
        Vector.empty,
        Vector.empty,
        PresentationState.default,
        Vector.empty,
        families
      )
    )
  private def sourceEntries(store: ProjectStore[IO], lock: WriterLock): IO[Vector[InputEntry]] =
    Vector(SourceRole.Fixations -> fixes, SourceRole.Trials -> trials).traverse {
      (role, text) =>
        val name = prepared.admitted.spec.sources.entries
          .find(_.role == role)
          .get
          .path
          .value
          .split('/')
          .last
        ok(
          ProjectBundle.importInput(
            store,
            lock,
            InputKind.Source(role),
            name,
            IArray.from(text.getBytes(UTF_8))
          )
        )
    }
  private def create(
      store: ProjectStore[IO],
      state: RunLifecycle = RunLifecycle.Completed,
      declaredSource: Option[SemanticIdentity] = None,
      units: Option[DeclaredUnits] = None,
      explicitFamilies: Boolean = false
  ): IO[ProjectSession[IO]] =
    for
      lock    <- ok(store.acquire(owner))
      entries <- sourceEntries(store, lock).guarantee(store.release(lock).void)
      session <- ok(
        ProjectSession.create(
          store,
          owner,
          document(state, declaredSource, units, explicitFamilies),
          SharingOptions.complete,
          entries
        )
      )
    yield session

  private def bind(session: ProjectSession[IO]): IO[Unit] =
    session
      .perform(JournalEntry.Apply(Command.BindCompletedArtifacts(exported.facts)))
      .map(get)
      .void

  private final class ControlledStore(
      inner: ProjectStore[IO],
      failIndex: Ref[IO, Boolean],
      reads: Ref[IO, Vector[BundlePath]],
      corruptPath: Option[BundlePath] = None
  ) extends ProjectStore[IO]:
    def read(path: BundlePath) = reads
      .update(_ :+ path)
      .flatMap(_ => inner.read(path))
      .map(
        _.map(bytes =>
          if corruptPath.contains(path) then IArray.from(Vector.from(bytes) :+ 0.toByte)
          else bytes
        )
      )
    def list                                                           = inner.list
    def write(lock: WriterLock, path: BundlePath, bytes: IArray[Byte]) =
      failIndex.get.flatMap(fail =>
        if fail && ArchivePaths.isIndex(path) then
          IO.pure(Left(StoreError.Unwritable(path.value, "injected index failure")))
        else inner.write(lock, path, bytes)
      )
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

  test(
    "writer-owned storage handles completion ordering, changes no history, then cold load verifies saved facts"
  ) {
    for
      store   <- InMemoryProjectStore.create[IO]
      session <- create(store, RunLifecycle.Running)
      initial <- session.history
      facts   <- NativeArtifactSink.project(session).store(exported).map(get)
      stored  <- session.history
      _ = assertEquals(stored, initial, "storage must not invent a terminal journal event")
      _ <- ok(
        session.perform(
          JournalEntry.Apply(
            Command.RecordRunOutcome(run, RunLifecycle.Completed, CoreBinding.unbound)
          )
        )
      )
      _            <- bind(session)
      _            <- ok(session.save)
      _            <- ok(session.close)
      reopened     <- ok(ProjectSession.open(store, owner))
      restored     <- reopened.session.loadNativeArtifacts(run).map(get)
      savedHistory <- reopened.session.history
      optional     <- reopened.session
        .findNativeArtifacts(savedHistory.document.run(run).get)
        .map(get)
      _ <- ok(reopened.session.close)
    yield
      assertEquals(facts, exported.facts)
      assertEquals(restored.facts, facts)
      assertEquals(restored.files.sortBy(_._1.value), exported.files.sortBy(_._1.value))
      assertEquals(restored.facts.inputCanonical, exported.facts.inputCanonical)
      assertEquals(optional.map(_.facts), Some(facts))
  }

  test("unknown and failed targets refuse before writing any archive bytes") {
    for
      store   <- InMemoryProjectStore.create[IO]
      session <- create(store, RunLifecycle.Failed)
      before  <- ok(store.list)
      refused <- session.storeNativeArtifacts(exported)
      after   <- ok(store.list)
      _       <- ok(session.close)
    yield
      assert(refused.isLeft)
      assertEquals(after, before)
  }

  test("genuinely absent archives stay optional for both unbound and older bound runs") {
    for
      store       <- InMemoryProjectStore.create[IO]
      session     <- create(store)
      initial     <- session.history
      absent      <- session.findNativeArtifacts(initial.document.run(run).get).map(get)
      _           <- bind(session)
      bound       <- session.history
      stillAbsent <- session.findNativeArtifacts(bound.document.run(run).get).map(get)
      stale       <- session.findNativeArtifacts(initial.document.run(run).get)
      after       <- session.history
      _           <- ok(session.close)
    yield
      assertEquals(absent, None)
      assertEquals(stillAbsent, None)
      assert(stale.isLeft)
      assertEquals(after, bound)
  }

  test(
    "contradictory declared fixation source refuses before storage, matching identity stores"
  ) {
    val other        = get(SemanticIdentity.of("ffeeddccbbaa9988"))
    val matchingSpec = document(declaredSource = Some(exported.facts.source)).datasets.head
    val assets       = get(
      AssetRegistry.of(
        matchingSpec.id,
        matchingSpec.sources.trials.get.bytes,
        matchingSpec.geometry.screen,
        Vector.empty
      )
    )
    val admitted     = get(RealAdmission.admit(matchingSpec, fixes, trials, assets))
    val matchingWork =
      get(RealPrepared.of(prepared.revision, matchingSpec.id, prepared.recipe, admitted))
    val matchingExport = get(
      NativeArtifacts.build(
        run,
        RealStudyBackend.RealRun(
          matchingWork,
          get(matchingWork.work.run),
          RealStudyBackend.RunOrigin.Computed
        ),
        NativeArtifactBudget.Default
      )
    )
    for
      store         <- InMemoryProjectStore.create[IO]
      session       <- create(store, declaredSource = Some(other))
      initial       <- session.history
      before        <- ok(store.list)
      refused       <- session.storeNativeArtifacts(exported)
      after         <- ok(store.list)
      unchanged     <- session.history
      _             <- ok(session.close)
      matchingStore <- InMemoryProjectStore.create[IO]
      matching      <- create(matchingStore, declaredSource = Some(exported.facts.source))
      accepted      <- matching.storeNativeArtifacts(matchingExport).map(get)
      _             <- ok(matching.close)
    yield
      assert(refused.left.toOption.exists(_.message.contains("fixation source semantic")))
      assertEquals(after, before)
      assertEquals(unchanged, initial)
      assertEquals(accepted, matchingExport.facts)
  }

  test(
    "an index-write failure leaves metadata unbound and explicit orphan bytes; retry is idempotent"
  ) {
    for
      inner     <- InMemoryProjectStore.create[IO]
      failIndex <- Ref.of[IO, Boolean](true)
      reads     <- Ref.of[IO, Vector[BundlePath]](Vector.empty)
      store = new ControlledStore(inner, failIndex, reads)
      session        <- create(store)
      before         <- session.history
      manifestBefore <- ok(store.readManifest)
      refused        <- session.storeNativeArtifacts(exported)
      after          <- session.history
      manifestAfter  <- ok(store.readManifest)
      orphan         <- ok(RunStore(store).records)
      refusedOrphan  <- session.findNativeArtifacts(before.document.run(run).get)
      _              <- failIndex.set(false)
      stored         <- session.storeNativeArtifacts(exported).map(get)
      again          <- session.storeNativeArtifacts(exported).map(get)
      _              <- ok(session.close)
    yield
      assert(refused.left.toOption.exists(_.message.contains("injected index failure")))
      assertEquals(after, before)
      assertEquals(Vector.from(manifestAfter), Vector.from(manifestBefore))
      assert(orphan.exists(_.isInstanceOf[ArchiveRecord.Incomplete]))
      assert(refusedOrphan.left.toOption.exists(_.message.contains("orphan archive paths")))
      assertEquals(stored, exported.facts)
      assertEquals(again, stored)
  }

  test("bounded cold read checks index totals before reading full native entries") {
    for
      inner     <- InMemoryProjectStore.create[IO]
      failIndex <- Ref.of[IO, Boolean](false)
      reads     <- Ref.of[IO, Vector[BundlePath]](Vector.empty)
      store = new ControlledStore(inner, failIndex, reads)
      session <- create(store)
      _       <- session.storeNativeArtifacts(exported).map(get)
      _       <- bind(session)
      _       <- reads.set(Vector.empty)
      limited <- session.loadNativeArtifacts(
        run,
        get(NativeArtifactBudget.of(maxTotalBytes = 1))
      )
      visited <- reads.get
      _       <- ok(session.close)
    yield
      assert(limited.left.toOption.exists(_.isInstanceOf[NativeArtifactError.Budget]))
      assertEquals(visited, Vector(get(ArchivePaths.index(run))))
  }

  test("a legacy unbound stored index collides explicitly and cannot be silently rebound") {
    for
      store  <- InMemoryProjectStore.create[IO]
      lock   <- ok(store.acquire(owner))
      inputs <- sourceEntries(store, lock)
      encoded = get(ProjectBundle.encode(document(), SharingOptions.complete, inputs))
      _ <- ok(ProjectBundle.save(store, lock, None, encoded))
      legacy = get(
        RunArchive.of(
          document().run(run).get,
          exported.files.map((name, bytes) => name -> IArray.from(bytes))
        )
      )
      _       <- ok(RunStore(store).put(lock, legacy))
      _       <- ok(store.release(lock))
      session <- ok(ProjectSession.open(store, owner))
      refused <- session.session.storeNativeArtifacts(exported)
      still   <- session.session.document
      _       <- ok(session.session.close)
    yield
      assert(refused.left.toOption.exists(_.message.contains("already holds")))
      assertEquals(still.run(run).get.archive, CoreBinding.unbound[ResultArchiveArtifact])
  }

  private def seeded(
      files: Vector[(ArtifactName, Vector[Byte])],
      extraBodies: Vector[Vector[Byte]] = Vector.empty
  ): IO[Either[NativeArtifactError, Option[NativeArtifactPackage]]] =
    val bound = get(
      Reducer.run(document(), Command.BindCompletedArtifacts(exported.facts))
    ).document
    for
      store  <- InMemoryProjectStore.create[IO]
      lock   <- ok(store.acquire(owner))
      inputs <- sourceEntries(store, lock)
      encoded = get(ProjectBundle.encode(bound, SharingOptions.complete, inputs))
      _ <- ok(ProjectBundle.save(store, lock, None, encoded))
      archive = get(
        RunArchive.of(
          bound.run(run).get,
          files.map((name, bytes) => name -> IArray.from(bytes))
        )
      )
      _ <- ok(RunStore(store).put(lock, archive))
      _ <- extraBodies.traverse_ { body =>
        val bytes = IArray.from(body)
        ok(store.write(lock, get(ArchivePaths.content(run, ByteDigest.sha256(bytes))), bytes))
      }
      _       <- ok(store.release(lock))
      opened  <- ok(ProjectSession.open(store, owner))
      initial <- opened.session.history
      found   <- opened.session.findNativeArtifacts(bound.run(run).get)
      after   <- opened.session.history
      _       <- ok(opened.session.close)
      _ = assertEquals(after, initial)
    yield found

  test("finder recognizes a legacy index and refuses a present incomplete native closure") {

    for
      legacy     <- seeded(Vector(get(ArtifactName.of("legacy-summary")) -> Vector(1.toByte)))
      incomplete <- seeded(exported.files.filterNot(_._1 == NativeArtifactPackage.InputName))
    yield
      assertEquals(legacy, Right(None))
      assert(incomplete.isLeft)
  }

  test("a legacy archive can name its arbitrary entry manifest without becoming native") {
    seeded(Vector(get(ArtifactName.of("manifest")) -> Vector(1.toByte))).map(found =>
      assertEquals(found, Right(None))
    )
  }

  test("an omitted native facts entry left as an actual undeclared blob refuses as orphan") {
    val body = exported.bytes(NativeArtifactPackage.FactsName).get
    val path = get(ArchivePaths.content(run, ByteDigest.sha256(IArray.from(body))))
    seeded(exported.files.filterNot(_._1 == NativeArtifactPackage.FactsName), Vector(body)).map(
      found =>
        assert(
          found.left.toOption.exists(error =>
            error.message.contains("orphan archive paths") && error.message.contains(path.value)
          ),
          clues(found)
        )
    )
  }

  test("cold optional finder refuses corrupt native entry bytes and changes no history") {
    for
      inner     <- InMemoryProjectStore.create[IO]
      session   <- create(inner)
      _         <- session.storeNativeArtifacts(exported).map(get)
      _         <- bind(session)
      _         <- ok(session.save)
      _         <- ok(session.close)
      failIndex <- Ref.of[IO, Boolean](false)
      reads     <- Ref.of[IO, Vector[BundlePath]](Vector.empty)
      path = get(
        ArchivePaths.content(
          run,
          exported.archive.index.entry(NativeArtifactPackage.ResultName).get.sha256
        )
      )
      store = new ControlledStore(inner, failIndex, reads, Some(path))
      opened  <- ok(ProjectSession.open(store, owner))
      initial <- opened.session.history
      found   <- opened.session.findNativeArtifacts(initial.document.run(run).get)
      after   <- opened.session.history
      _       <- ok(opened.session.close)
    yield
      val entry = exported.archive.index.entry(NativeArtifactPackage.ResultName).get
      assertEquals(
        found,
        Left(
          NativeArtifactError.persistence(
            run,
            "load archive entries",
            RunStoreError.EntryLength(run, entry.name, entry.length, entry.length + 1).message
          )
        )
      )
      assertEquals(after, initial)
  }

  test("dataset definition mismatch refuses before any storage or history mutation") {
    for
      store     <- InMemoryProjectStore.create[IO]
      session   <- create(store, units = Some(DeclaredUnits(Some(TimeUnit.Microseconds))))
      initial   <- session.history
      before    <- ok(store.list)
      rejected  <- session.storeNativeArtifacts(exported)
      after     <- ok(store.list)
      unchanged <- session.history
      _         <- ok(session.close)
    yield
      assert(rejected.left.toOption.exists(_.message.contains("definition")))
      assertEquals(after, before)
      assertEquals(unchanged, initial)
  }

  test("native prevalidation, terminal binding and cold restore retain independent owners") {
    for
      store <- InMemoryProjectStore.create[IO]
      session <- create(store, RunLifecycle.Running, explicitFamilies = true)
      initial <- session.history
      _ <- session.storeNativeArtifacts(exported).map(get)
      stored <- session.history
      _ = assertEquals(stored, initial)
      _ <- ok(session.perform(JournalEntry.Apply(
        Command.RecordRunOutcome(run, RunLifecycle.Completed, CoreBinding.unbound)
      )))
      _ <- bind(session)
      _ <- ok(session.save)
      saved <- session.document
      _ <- ok(session.close)
      reopened <- ok(ProjectSession.open(store, owner))
      restored <- reopened.session.document
      _ <- reopened.session.loadNativeArtifacts(run).map(get)
      _ <- ok(reopened.session.close)
    yield
      assertEquals(saved.analysisFamilies, initial.document.analysisFamilies)
      assertEquals(restored.analysisFamilies, initial.document.analysisFamilies)
      assertEquals(restored.familyOf(AnalysisRevision(1)), Some(FamilySamples.a))
      assertEquals(restored.familyOf(AnalysisRevision(2)), Some(FamilySamples.b))
  }
