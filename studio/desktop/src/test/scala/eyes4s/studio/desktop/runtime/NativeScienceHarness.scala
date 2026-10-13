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
import eyes4s.codec.ByteDigest
import eyes4s.plan.TrialKeyDefinitions
import eyes4s.studio.app.{AppEffect, AppModel, Intent, ProjectName}
import eyes4s.studio.core.assets.AssetRegistry
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.StoryMoments
import eyes4s.studio.core.preview.*
import eyes4s.studio.core.real.{DatasetSources, RealAdmission, RealPrepared}
import java.nio.charset.StandardCharsets.UTF_8
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.*

/** Execution safety through the native desktop services and the same app
  * commands as Save & run. Fixtures declare blank displays and finite,
  * nonoverlapping fixations; every preview count comes from eyes4s.
  */
private[runtime] object NativeScienceHarness extends munit.Assertions:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  val revision                                  = StoryMoments.rev5

  final case class Fixture(
      document: StudioDocument,
      data: DatasetRevisionSpec,
      bytes: Map[SourceRole, IArray[Byte]],
      registry: AssetRegistry
  ):
    val sources: DatasetSources[IO] = new DatasetSources[IO]:
      def bytes(dataset: DatasetRevisionSpec, source: Source) =
        IO.pure(Fixture.this.bytes.get(source.role))
      def assets(dataset: DatasetRevisionSpec) = IO.pure(Some(registry))

  def direct(f: Fixture, document: StudioDocument): RealPrepared =
    val admitted = get(
      RealAdmission.admit(
        f.data,
        String(Array.from(f.bytes(SourceRole.Fixations)), UTF_8),
        String(Array.from(f.bytes(SourceRole.Trials)), UTF_8),
        f.registry
      )
    )
    val analysis = document.analyses.last
    get(RealPrepared.of(analysis.id, f.data.id, analysis.recipe, admitted))

  /** `repeated`: P1 encodes item 1 a second time, as trial enc_9, so its
    * retrieval query has two matched references.
    */
  def fixture(
      participants: Int = 1,
      items: Int = 2,
      repeated: Boolean = false,
      imagery: Boolean = false,
      identical: Boolean = false,
      reversePositions: Boolean = false,
      emptyMaps: Boolean = false
  ): Fixture =
    val story  = get(StoryMoments.t2)
    val again  = Vector("P1,Encoding,enc_9,1,item-1,blank,,").filter(_ => repeated)
    val trials = again ++ (for
      person <- 1 to participants
      phase  <- Vector("Encoding", if imagery then "Imagery" else "Retrieval")
      item   <- 1 to items
    yield s"P$person,$phase,${
        if phase == "Encoding" then "enc" else "ret"
      }_$item,1,item-$item,blank,,${if phase != "Encoding" then "Remembered" else ""}").toVector
    val againFixations = (1 to 8).toVector
      .filter(_ => repeated)
      .map(o => s"P1,Encoding,enc_9,1,$o,${640 + o},${520 + o * 3},${(o - 1) * 200},100,50")
    val fixations = againFixations ++ (for
      person  <- 1 to participants
      phase   <- Vector("Encoding", if imagery then "Imagery" else "Retrieval")
      item    <- 1 to items
      ordinal <- 1 to 8
      position = if reversePositions then 9 - ordinal else ordinal
      x = if emptyMaps then 50 else 600 + (if identical then 0 else item * 180) + position
      y = if emptyMaps then 50 else 500 + position * 3
    yield s"P$person,$phase,${if phase == "Encoding" then "enc" else "ret"}_$item,1,$ordinal,$x,$y,${(ordinal - 1) * 400},${position * 20},50").toVector
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
    assertEquals(
      admitted.input.trials.rows.size,
      participants * items * 2 + (if repeated then 1 else 0)
    )
    val oldBase = story.analysis(StoryMoments.rev4).get
    val recipe  = oldBase.recipe.copy(
      layout = DefinitionRef.fromCore(TrialKeyDefinitions.trialLayout),
      phases = PhasePair(Phase(if imagery then "Imagery" else "Retrieval"), Phase.Encoding),
      grid = get(GridSize.of(32, 24)),
      scales = get(ScaleSet.of(Vector(get(Sigma.of(2.0))))),
      matched = MatchedChoice.RequireOne
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

  final class Harness(
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

  def harness(
      document: StudioDocument,
      sources: DatasetSources[IO]
  )(using cats.effect.unsafe.IORuntime): Resource[IO, Harness] =
    val model = AtomicReference(
      AppModel.open(document, Some(get(ProjectName.of("Adversarial native science"))))
    )
    val inbox = LinkedBlockingQueue[Intent]()
    Resource.make(IO.blocking {
      val session = StudioSession.start(document, sources, e => inbox.put(Intent.Execution(e)))
      session.bindDocument(() => model.get().document)
      val effects =
        DesktopEffects(session, (_, _) => (), _ => (), _ => (), f => f(), project = None)
      new Harness(session, model, inbox, effects)
    })(h => IO.blocking(h.session.close()))
