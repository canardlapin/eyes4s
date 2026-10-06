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

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import eyes4s.codec.ByteDigest
import eyes4s.plan.TrialKeyDefinitions
import eyes4s.studio.app.{AppEffect, AppModel, Intent, Notice, PreparedDesign, ProjectName}
import eyes4s.studio.core.assets.AssetRegistry
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.*
import eyes4s.studio.core.execution.*
import eyes4s.studio.core.fixture.StoryMoments
import eyes4s.studio.core.preview.*
import eyes4s.studio.core.real.{DatasetSources, RealAdmission}
import eyes4s.studio.core.session.ProjectSession
import eyes4s.studio.desktop.platform.{FileProjectStore, TempDirs}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.*

/** Execution safety through the native desktop services and the same app
  * commands as Save & run. Fixtures declare blank displays and finite,
  * nonoverlapping fixations; every preview count comes from eyes4s.
  */
class NativeExecutionSafetySuite extends munit.CatsEffectSuite:
  override val munitIOTimeout: Duration         = 90.seconds
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private def ok[E, A](value: IO[Either[E, A]]): IO[A] = value.map(get)
  private val revision                                 = StoryMoments.rev5

  private final case class Fixture(
      document: StudioDocument,
      data: DatasetRevisionSpec,
      bytes: Map[SourceRole, IArray[Byte]],
      registry: AssetRegistry
  ):
    val sources: DatasetSources[IO] = new DatasetSources[IO]:
      def bytes(dataset: DatasetRevisionSpec, source: Source) =
        IO.pure(Fixture.this.bytes.get(source.role))
      def assets(dataset: DatasetRevisionSpec) = IO.pure(Some(registry))

  private def fixture(participants: Int = 1, items: Int = 2): Fixture =
    val story  = get(StoryMoments.t2)
    val trials = (for
      person <- 1 to participants
      phase  <- Vector("Encoding", "Retrieval")
      item   <- 1 to items
    yield s"P$person,$phase,${
        if phase == "Encoding" then "enc" else "ret"
      }_$item,1,item-$item,blank,,${if phase == "Retrieval" then "Remembered" else ""}").toVector
    val fixations = (for
      person  <- 1 to participants
      phase   <- Vector("Encoding", "Retrieval")
      item    <- 1 to items
      ordinal <- 1 to 8
    yield s"P$person,$phase,${if phase == "Encoding" then "enc" else "ret"}_$item,1,$ordinal,${600 + item * 20 + ordinal},${500 + ordinal * 3},${(ordinal - 1) * 200},100,50").toVector
    val bytes = Map(
      SourceRole.Trials -> IArray.from(
        ("participant,phase,trial,occurrence,item,display_kind,image_file,response" +: trials)
          .mkString("", "\n", "\n")
          .getBytes(UTF_8)
      ),
      SourceRole.Fixations -> IArray.from(
        ("participant,phase,trial,occurrence,ordinal,x,y,onset_ms,duration_ms,sample_count" +: fixations)
          .mkString("", "\n", "\n")
          .getBytes(UTF_8)
      )
    )
    val old = story.dataset(StoryMoments.r3).getOrElse(fail("no declared fixture geometry"))
    val inventory = get(
      InventoryMapping.withDisplays(
        old.inventory.get,
        Some(
          DisplayColumns(
            get(ColumnName.of("display_kind")),
            Some(get(ColumnName.of("image_file")))
          )
        )
      )
    )
    val data = old.copy(
      parent = None,
      sources = get(Sources.of(bytes.toVector.map { (role, contents) =>
        Source(
          role,
          get(SourcePath.of(if role == SourceRole.Trials then "trials.csv"
          else "fixations.csv")),
          ByteDigest.sha256(contents),
          None
        )
      })),
      inventory = Some(inventory),
      decision = AdmissionDecision.Admitted(
        Some(eyes4s.plan.AdmissionDecision.RequireComplete),
        CoreBinding.unbound,
        CoreBinding.unbound
      )
    )
    val registry = get(
      AssetRegistry.fromInventory(data, bytes(SourceRole.Trials), Vector.empty, Vector.empty)
    )
    val admitted = get(
      RealAdmission.admit(
        data,
        String(Array.from(bytes(SourceRole.Fixations)), UTF_8),
        String(Array.from(bytes(SourceRole.Trials)), UTF_8),
        registry
      )
    )
    assertEquals(admitted.input.trials.rows.size, participants * items * 2)
    val oldBase = story.analysis(StoryMoments.rev4).get
    val recipe  = oldBase.recipe.copy(
      layout = DefinitionRef.fromCore(TrialKeyDefinitions.trialLayout),
      grid = get(GridSize.of(32, 24)),
      scales = get(ScaleSet.of(Vector(get(Sigma.of(2.0)))))
    )
    val base  = oldBase.copy(recipe = recipe, plan = CoreBinding.unbound)
    val draft = get(Draft.between(revision, base, recipe.copy(grid = get(GridSize.of(40, 30)))))
    val document = get(
      StudioDocument.of(
        Vector(data),
        Vector(base),
        Some(draft),
        Vector.empty,
        story.reporting,
        Vector.empty,
        PresentationState.default,
        Vector.empty
      )
    )
    Fixture(document, data, bytes, registry)

  private final class Harness(
      val session: StudioSession,
      val model: AtomicReference[AppModel],
      val inbox: LinkedBlockingQueue[Intent],
      val effects: DesktopEffects
  ):
    def dispatch(intent: Intent, perform: Boolean = true): IO[Vector[AppEffect]] = IO.blocking {
      val (next, emitted) = AppModel.update(model.get(), intent)
      model.set(next)
      if perform then emitted.foreach(effects.perform(_, inbox.put))
      emitted
    }
    def raw: IO[Intent] = IO
      .interruptible(inbox.take())
      .timeoutTo(
        15.seconds,
        for
          jobs    <- session.backend.jobs
          tracked <- session.service.jobs
          current <- session.service.requested
          problems = effects.problems.collect {
            case problem: EffectProblem.Refused => problem.message
            case problem: EffectProblem.Failed  => problem.message
          }
          intent <- IO.raiseError[Intent](
            new AssertionError(
              s"No native event: runs=${model.get().document.runs.map(r => (r.id, r.analysis, r.state))}; backend=$jobs; tracked=$tracked; requested=$current; problems=$problems"
            )
          )
        yield intent
      )
    def take: IO[Intent]                                = raw.flatTap(dispatch(_).void)
    def until(predicate: Intent => Boolean): IO[Intent] = take.flatMap { intent =>
      if predicate(intent) then IO.pure(intent) else until(predicate)
    }
    def prepared: IO[PreviewReady] =
      val budget = get(PreviewBudget.of(PreviewBudget.MaximumPages))
      def finish(
          events: Vector[Either[BackendError, PreviewEvent]],
          id: Option[PreviewId]
      ): IO[PreviewReady] =
        events.collectFirst { case Left(error) => error }.foreach(e => fail(e.message))
        events.collectFirst { case Right(PreviewEvent.Ready(ready)) => ready } match
          case Some(ready) => IO.pure(ready)
          case None        =>
            val cursor = id
              .orElse(events.collectFirst { case Right(PreviewEvent.Initial(id, _, _)) => id })
              .get
            session.backend
              .continuePreview(cursor, budget)
              .compile
              .toVector
              .flatMap(finish(_, Some(cursor)))
      session.backend
        .previewCounting(revision, budget)
        .compile
        .toVector
        .flatMap(finish(_, None))

  private def harness(
      document: StudioDocument,
      sources: DatasetSources[IO],
      port: Option[ProjectPort] = None
  ): Resource[IO, Harness] =
    val model = AtomicReference(
      AppModel.open(document, Some(get(ProjectName.of("Native execution safety"))))
    )
    val inbox = LinkedBlockingQueue[Intent]()
    Resource.make(IO.blocking {
      val session = StudioSession.start(document, sources, e => inbox.put(Intent.Execution(e)))
      session.bindDocument(() => model.get().document)
      val effects =
        DesktopEffects(session, (_, _) => (), _ => (), _ => (), f => f(), project = port)
      new Harness(session, model, inbox, effects)
    })(h => IO.blocking(h.session.close()))

  private def stored(
      f: Fixture,
      root: Path
  ): Resource[IO, (ProjectStore[IO], SessionPort, Vector[InputEntry])] =
    val owner = get(LockOwner.of("NativeExecutionSafetySuite"))
    for
      store   <- Resource.eval(FileProjectStore.at[IO](root))
      entries <- Resource.eval(for
        lock    <- ok(store.acquire(owner))
        entries <- f.data.sources.entries.traverse(s =>
          ok(
            ProjectBundle.importInput(
              store,
              lock,
              InputKind.Source(s.role),
              s.path.value,
              f.bytes(s.role)
            )
          )
        )
        _ <- ok(
          ProjectBundle.save(
            store,
            lock,
            None,
            get(ProjectBundle.encode(f.document, SharingOptions.complete, entries))
          )
        )
        _ <- ok(store.release(lock))
      yield entries)
      project <- Resource.make(ok(ProjectSession.open(store, owner)).map(_.session))(p =>
        ok(p.close)
      )
      port <- Resource.make(IO.blocking(SessionPort.start(project)))(p =>
        IO.blocking(p.close())
      )
    yield (store, port, entries)

  test(
    "native runtime refusal: changed stored source after preview blocks Save & run before any run or job"
  ) {
    val f = fixture()
    TempDirs.resource("eyes4s-native-source-safety-").use { directory =>
      val root = directory.resolve("project.eyes")
      stored(f, root).use { (_, port, entries) =>
        // The asset port is a truthful blank-display registry. Source reads
        // and checks still go through the actual stored project port.
        val fromProject = DatasetSourceHosts.stored(port)
        val sources     = new DatasetSources[IO]:
          def bytes(dataset: DatasetRevisionSpec, source: Source) =
            fromProject.bytes(dataset, source)
          def assets(dataset: DatasetRevisionSpec) = IO.pure(Some(f.registry))
        harness(f.document, sources, Some(port)).use { h =>
          for
            _     <- h.dispatch(Intent.CheckInputs)
            _     <- h.until(_.isInstanceOf[Intent.InputsChecked])
            ready <- h.prepared
            _     <- h.dispatch(
              Intent.DesignPrepared(
                PreparedDesign(
                  ready,
                  f.document.draft.get.recipe(f.document.analyses.head.recipe)
                )
              )
            )
            input   = entries.find(_.kind == InputKind.Source(SourceRole.Fixations)).get
            changed = IArray.from(
              (String(Array.from(f.bytes(SourceRole.Fixations)), UTF_8) + "\n").getBytes(UTF_8)
            )
            _ <- IO.blocking(
              Files.write(root.resolve(input.path.get.value), Array.from(changed))
            )
            emitted <- h.dispatch(Intent.Dispatch(Command.SaveAndRun(None)))
            checked <- h.until(_.isInstanceOf[Intent.InputsChecked])
            jobs    <- h.session.backend.jobs
            tracked <- h.session.service.jobs
          yield
            assert(emitted.exists(_.isInstanceOf[AppEffect.CheckInputs]))
            checked match
              case Intent.InputsChecked(_, statuses) =>
                assert(
                  statuses.contains(InputStatus.Changed(input, ByteDigest.sha256(changed)))
                )
              case other => fail(s"unexpected native source check $other")
            assert(h.model.get().notice.exists(_.isInstanceOf[Notice.SourcesBlocked]))
            assertEquals(h.model.get().document, f.document)
            assertEquals(h.model.get().document.presentation.shownRun, None)
            assertEquals(jobs, Vector.empty)
            assertEquals(tracked, Vector.empty)
        }
      }
    }
  }

  Vector("tampered", "stale").foreach { kind =>
    test(
      s"native runtime refusal: $kind receipt fails the exact requested run and allocates no job"
    ) {
      val f = fixture()
      harness(f.document, f.sources).use { h =>
        for
          receipt <- h.prepared
          recipe   = f.document.draft.get.recipe(f.document.analyses.head.recipe)
          supplied =
            if kind == "tampered" then
              get(
                PreviewReady.of(
                  receipt.id,
                  receipt.stamp,
                  receipt.candidates,
                  receipt.counts,
                  receipt.diagnostics :+ StudioDiagnostic(
                    "test.forged",
                    DiagnosticLevel.Warning,
                    DiagnosticOrigin.Host,
                    Vector(DiagnosticLocus.Revision(revision)),
                    "forged receipt"
                  ),
                  receipt.recipe
                )
              )
            else receipt
          _ <- h.dispatch(Intent.DesignPrepared(PreparedDesign(supplied, recipe)))
          _ <-
            if kind == "stale" then
              h.dispatch(
                Intent.Dispatch(
                  Command.ChangeRecipe(RecipeChange.Grid(recipe.grid, get(GridSize.of(48, 32))))
                )
              ).void
            else IO.unit
          emitted <- h.dispatch(Intent.Dispatch(Command.SaveAndRun(None)), perform = false)
          requested = h.model.get().document.runs.last.id
          _ <- IO {
            // A second matching reservation makes exact-run handling observable:
            // the refused request must fail its own run, never the latest one.
            val saved     = h.model.get().document
            val unrelated = saved.runs.last.copy(id = RunId(99))
            val extra     = get(
              StudioDocument.of(
                saved.datasets,
                saved.analyses,
                saved.draft,
                saved.runs :+ unrelated,
                saved.reporting,
                saved.figures,
                saved.presentation,
                saved.jobs
              )
            )
            h.model.set(AppModel.open(extra, h.model.get().project))
            // Send the adversarial old receipt through the real desktop effect
            // path after the actual command has reserved its requested run.
            h.effects.perform(
              AppEffect.Execution(ExecutionEffect.SubmitPreview(supplied, Some(requested))),
              h.inbox.put
            )
          }
          refused <- h.until(_.isInstanceOf[Intent.PreparedRefused])
          jobs    <- h.session.backend.jobs
          tracked <- h.session.service.jobs
        yield
          assert(emitted.exists(_.isInstanceOf[AppEffect.Execution]))
          refused match
            case Intent.PreparedRefused(
                  `supplied`,
                  ExecutionError.Backend(error),
                  Some(`requested`)
                ) =>
              assertEquals(
                error.code,
                if kind == "tampered" then "studio-backend.tampered-preview"
                else "studio-backend.stale-preview"
              )
            case other => fail(s"wrong native refusal identity $other")
          assertEquals(
            h.model.get().document.run(requested).map(_.state),
            Some(RunLifecycle.Failed)
          )
          assertEquals(
            h.model.get().document.run(RunId(99)).map(_.state),
            Some(RunLifecycle.Running)
          )
          assertEquals(h.model.get().document.presentation.shownRun, None)
          assertEquals(jobs, Vector.empty)
          assertEquals(tracked, Vector.empty)
      }
    }
  }

  test(
    "native runtime cancellation settles the command's run without showing or replacing a result"
  ) {
    val f = fixture(participants = 20, items = 4)
    harness(f.document, f.sources).use { h =>
      for
        emitted <- h.dispatch(Intent.Dispatch(Command.SaveAndRun(None)))
        requested = h.model.get().document.runs.last.id
        started <- h.until {
          case Intent.Execution(ExecutionEvent.Changed(job)) => !job.phase.isTerminal
          case _                                             => false
        }
        job = started match
          case Intent.Execution(ExecutionEvent.Changed(job)) => job
          case other                                         => fail(s"no native job $other")
        cancelled <- h.dispatch(Intent.CancelJob(job.id))
        terminal  <- h.until {
          case Intent.Execution(ExecutionEvent.Changed(job)) => job.phase.isTerminal
          case _                                             => false
        }
        backend <- ok(h.session.backend.job(job.id))
      yield
        assert(emitted.exists(_.isInstanceOf[AppEffect.Execution]))
        assertEquals(job.run, requested)
        assertEquals(h.model.get().document.job(requested), None)
        assertEquals(cancelled, Vector(AppEffect.Execution(ExecutionEffect.Cancel(job.id))))
        terminal match
          case Intent.Execution(ExecutionEvent.Changed(done)) =>
            assert(done.phase.isInstanceOf[JobPhase.Cancelled], done)
          case other => fail(s"no native cancellation outcome $other")
        assert(backend.state match
          case JobState.Finished(_: JobOutcome.Cancelled) => true
          case _                                          => false)
        assert(
          h.model
            .get()
            .document
            .run(requested)
            .exists(_.state.isInstanceOf[RunLifecycle.Cancelled])
        )
        assertEquals(h.model.get().document.presentation.shownRun, None)
        assertEquals(h.model.get().jobs.shelf.shown, None)
        assertEquals(h.model.get().jobs.shelf.pending, None)
    }
  }

  test(
    "native runtime supersession: late old completion events cannot replace a newer draft's ready run"
  ) {
    val f = fixture()
    harness(f.document, f.sources).use { h =>
      // Delay delivery to the app, not native execution. Every held event
      // comes from the actual first job; the native single-job slot is free
      // before the newer command submits. No outcomes are constructed here.
      def holdUntilReady(run: RunId, held: Vector[Intent] = Vector.empty): IO[Vector[Intent]] =
        h.raw.flatMap { intent =>
          val next = held :+ intent
          intent match
            case Intent.Execution(ExecutionEvent.Ready(value)) if value.run == run =>
              IO.pure(next)
            case _ => holdUntilReady(run, next)
        }
      for
        _ <- h.dispatch(Intent.Dispatch(Command.SaveAndRun(None)))
        oldRun = h.model.get().document.runs.last.id
        held    <- holdUntilReady(oldRun)
        oldJobs <- h.session.backend.jobs
        _ = assertEquals(
          h.model.get().document.run(oldRun).map(_.state),
          Some(RunLifecycle.Running)
        )
        saved = h.model.get().document.analyses.last.recipe
        _ <- h.dispatch(
          Intent.Dispatch(
            Command.ChangeRecipe(RecipeChange.Grid(saved.grid, get(GridSize.of(48, 32))))
          )
        )
        _ <- h.dispatch(Intent.Dispatch(Command.SaveAndRun(None)))
        newRun   = h.model.get().document.runs.last.id
        newStamp = AppModel.requestedStamp(h.model.get().document).get
        ready <- h.until {
          case Intent.Execution(ExecutionEvent.Ready(value)) => value.run == newRun
          case _                                             => false
        }
        _ = assertEquals(
          h.model.get().document.run(oldRun).map(_.state),
          Some(RunLifecycle.Running)
        )
        _ = assertEquals(h.model.get().jobs.shelf.pending.map(_.run), Some(newRun))
        _         <- held.traverse_(h.dispatch(_).void)
        requested <- h.session.service.requested
        jobs      <- h.session.service.jobs
      yield
        assert(newRun != oldRun)
        assert(
          oldJobs
            .find(_.run == oldRun)
            .exists(_.state match
              case JobState.Finished(_: JobOutcome.Completed) => true
              case _                                          => false)
        )
        assert(held.exists {
          case Intent.Execution(ExecutionEvent.Changed(job)) =>
            job.run == oldRun && job.phase.isInstanceOf[JobPhase.Succeeded]
          case _ => false
        })
        ready match
          case Intent.Execution(ExecutionEvent.Ready(value)) =>
            assertEquals(value.stamp, newStamp)
          case other => fail(s"no new native completion $other")
        assertEquals(
          h.model.get().document.run(oldRun).map(_.state),
          Some(RunLifecycle.Completed)
        )
        assertEquals(
          h.model.get().document.run(newRun).map(_.state),
          Some(RunLifecycle.Completed)
        )
        assertEquals(requested, Some(newStamp))
        assertEquals(h.model.get().jobs.shelf.required, Some(newStamp))
        assertEquals(h.model.get().jobs.shelf.pending.map(_.run), Some(newRun))
        assertEquals(h.model.get().document.presentation.shownRun, None)
        assertEquals(h.model.get().jobs.shelf.shown, None)
        assertEquals(
          jobs.find(_.run == newRun).map(_.phase.isInstanceOf[JobPhase.Succeeded]),
          Some(true)
        )
    }
  }
