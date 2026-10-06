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

import cats.effect.IO
import eyes4s.design.WorkQuanta
import eyes4s.plan.StudyInput
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.{DefinitionRef, InitialFixationChoice, UnmatchedChoice}
import eyes4s.studio.core.fixture.{GoldenAssets, StoryMoments}
import eyes4s.codec.ByteDigest
import eyes4s.studio.core.document.{Sources, SourceRole}
import java.nio.charset.StandardCharsets
import eyes4s.studio.core.preview.*
import munit.CatsEffectSuite

class RealPreviewCursorSuite extends CatsEffectSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private lazy val document                     = get(StoryMoments.t2)
  private lazy val recipe                       =
    get(document.analysis(StoryMoments.rev4).toRight("no analysis")).recipe
      .copy(layout = DefinitionRef.fromCore(eyes4s.plan.TrialKeyDefinitions.trialLayout))
  private lazy val admitted =
    val original = get(document.dataset(StoryMoments.r3).toRight("no dataset"))
    val displays = get(GoldenAssets.rows)
    val focal    = displays
      .find(row =>
        row.trial.phase == recipe.phases.focal && displays.exists(reference =>
          reference.trial.phase == recipe.phases.reference && reference.trial.participant == row.trial.participant && reference.item == row.item
        )
      )
      .getOrElse(fail("no focal with a declared reference"))
    val reference = displays
      .find(row =>
        row.trial.participant == focal.trial.participant && row.item == focal.item && row.trial.phase == recipe.phases.reference
      )
      .getOrElse(fail("no reference"))
    val selected  = Vector(focal, reference)
    val fixations =
      "participant,phase,trial,occurrence,ordinal,onset_ms,duration_ms,x,y,sample_count\n" +
        selected
          .map(row =>
            s"${row.trial.participant},${row.trial.phase.label},${row.trial.trial},${row.trial.occurrence},1,0,100,600,300,1"
          )
          .mkString("\n") + "\n"
    val inventory =
      "participant,phase,trial,occurrence,item,response,display_kind,image_file\n" +
        selected
          .map(row =>
            s"${row.trial.participant},${row.trial.phase.label},${row.trial.trial},${row.trial.occurrence},${row.item},Remembered,${row.kind},${row.file}"
          )
          .mkString("\n") + "\n"
    val texts   = Map(SourceRole.Fixations -> fixations, SourceRole.Trials -> inventory)
    val sources = get(
      Sources.of(
        original.sources.entries.map(source =>
          source.copy(bytes =
            ByteDigest.sha256(IArray.from(texts(source.role).getBytes(StandardCharsets.UTF_8)))
          )
        )
      )
    )
    val spec = original.copy(sources = sources)
    get(RealAdmission.admit(spec, fixations, inventory, get(GoldenAssets.registry(spec))))
  private lazy val configured =
    get(RealPrepared.configure(StoryMoments.rev4, StoryMoments.r3, recipe, admitted))
  private val onePage = get(PreviewBudget.of(1))

  private def finish(
      service: RealPreview[IO],
      id: PreviewId,
      limit: Int = 100
  ): IO[Vector[PreviewEvent]] =
    service.continue(id, onePage).compile.toVector.flatMap { responses =>
      val events = responses.map(get)
      if events.exists { case PreviewEvent.Ready(_) => true; case _ => false } then
        IO.pure(events)
      else if limit == 0 then IO.raiseError(new AssertionError("preview did not finish"))
      else finish(service, id, limit - 1).map(events ++ _)
    }

  test("native pages are retained, bounded and count exactly the direct cursor") {
    assertEquals(configured.preview.counts, None)
    val direct = get(configured.work.counts)
    for
      service <- RealPreview.create[IO]
      opening <- service.begin(configured, onePage).take(1).compile.toVector
      id = get(opening.head) match
        case PreviewEvent.Initial(id, _, _) => id
        case event                          => fail(s"expected Initial, got $event")
      events   <- finish(service, id)
      repeated <- service.continue(id, onePage).compile.toVector
    yield
      val progress = events.collect { case value: PreviewEvent.CountingWork => value }
      assert(progress.nonEmpty)
      progress.foreach(p =>
        assert(p.workUnits >= 0 && p.workUnits <= WorkQuanta.default.pairs.value)
      )
      assertEquals(
        progress.map(_.visitedScheduleWork),
        progress.map(_.visitedScheduleWork).sorted
      )
      assert(progress.exists(_.design == PairDesign.Matched))
      assert(progress.exists(_.design == PairDesign.Control))
      val ready = events
        .collectFirst { case PreviewEvent.Ready(receipt) => receipt }
        .getOrElse(fail("no receipt"))
      assertEquals(ready.counts.eligiblePairsPerScale, direct.pairRowsPerScale)
      assertEquals(ready.counts.eligiblePairs, direct.totalPairs)
      assertEquals(ready.counts.eligibleQueries.value.toLong, direct.eligibleQueries)
      assertEquals(ready.recipe, Some(recipe))
      assertEquals(ready.stamp, get(RealPreview.stamp(configured)))
      assertEquals(repeated, Vector(Right(PreviewEvent.Ready(ready))))
      // A tiny count traverses cardinality assembly between the two schedules.
      // Its work units exceed schedule work; that assembly must not inflate visits.
      assert(progress.map(_.workUnits.toLong).sum >= progress.last.visitedScheduleWork)
  }

  test("cardinality diagnostics spend work without claiming schedule visits") {
    val focalOnly =
      configured.admitted.input.trials.filter(_.key.phase == recipe.phases.focal.label)
    val subset = configured.admitted.copy(
      input = StudyInput(focalOnly),
      ledger = configured.admitted.ledger.filter(_.trial.phase == recipe.phases.focal)
    )
    val refused = get(
      RealPrepared.configure(
        configured.revision,
        configured.dataset,
        recipe.copy(unmatched = UnmatchedChoice.Refuse),
        subset
      )
    )
    for
      service <- RealPreview.create[IO]
      opening <- service.begin(refused, onePage).take(1).compile.toVector
      id = get(opening.head) match
        case PreviewEvent.Initial(id, _, _) => id
        case event                          => fail(s"expected Initial, got $event")
      events <- finish(service, id)
      receipt = events
        .collectFirst { case PreviewEvent.Ready(value) => value }
        .getOrElse(fail("no receipt"))
      submission <- service.accept(receipt)(_ => IO.pure(Right(receipt.stamp)))
    yield
      val progress = events.collect { case value: PreviewEvent.CountingWork => value }
      val changes  = progress
        .sliding(2)
        .collect { case Vector(before, after) =>
          after.workUnits > 0 && after.visitedScheduleWork == before.visitedScheduleWork
        }
        .toVector
      assert(
        changes.contains(true),
        "native cardinality assembly must not inflate schedule work"
      )
      assert(progress.map(_.workUnits.toLong).sum > progress.last.visitedScheduleWork)
      val ready = events
        .collectFirst { case PreviewEvent.Ready(receipt) => receipt }
        .getOrElse(fail("no receipt"))
      assertEquals(ready.counts.unmatchedQueries.value, 1)
      assertEquals(ready.counts.eligibleQueries.value, 0)
      assert(ready.diagnostics.exists(_.level == DiagnosticLevel.Error))
      assert(submission.isLeft, "a native pairing refusal must prevent submission")
      assert(submission.left.toOption.exists(_.message.contains("preview submission")))
  }

  test("receipts reject premature, unknown, tampered and stale submissions") {
    val direct = get(configured.work.counts)
    for
      service <- RealPreview.create[IO]
      opening <- service.begin(configured, onePage).take(1).compile.toVector
      initial = get(opening.head) match
        case event: PreviewEvent.Initial => event
        case event                       => fail(s"expected Initial, got $event")
      counts = get(
        PreviewCounts.of(
          direct.pairRowsPerScale,
          direct.totalPairs,
          direct.eligibleQueries.toInt,
          direct.cardinality.unmatched.size,
          0
        )
      )
      forgedEarly = get(
        PreviewReady.of(
          initial.id,
          initial.stamp,
          initial.candidates,
          counts,
          Vector.empty,
          Some(recipe)
        )
      )
      early  <- service.accept(forgedEarly)(_ => IO.pure(Right(initial.stamp)))
      events <- finish(service, initial.id)
      ready = events
        .collectFirst { case PreviewEvent.Ready(receipt) => receipt }
        .getOrElse(fail("no ready"))
      changedCounts = get(
        PreviewCounts.of(
          counts.eligiblePairsPerScale + 1,
          counts.eligiblePairs + 1,
          counts.eligibleQueries.value,
          counts.unmatchedQueries.value,
          counts.ambiguousMatches
        )
      )
      tampered = get(
        PreviewReady.of(
          ready.id,
          ready.stamp,
          ready.candidates,
          changedCounts,
          ready.diagnostics,
          ready.recipe
        )
      )
      changedRecipe = get(
        PreviewReady.of(
          ready.id,
          ready.stamp,
          ready.candidates,
          ready.counts,
          ready.diagnostics,
          Some(recipe.copy(initialFixations = InitialFixationChoice.DropFirst))
        )
      )
      changedId = get(
        PreviewReady.of(
          PreviewId(999),
          ready.stamp,
          ready.candidates,
          ready.counts,
          ready.diagnostics,
          ready.recipe
        )
      )
      badCounts <- service.accept(tampered)(_ => IO.pure(Right(ready.stamp)))
      badRecipe <- service.accept(changedRecipe)(_ => IO.pure(Right(ready.stamp)))
      unknown   <- service.accept(changedId)(_ => IO.pure(Right(ready.stamp)))
      stale     <- service.accept(ready)(_ =>
        IO.pure(Right(ready.stamp.copy(dataset = DatasetRevision(999))))
      )
      accepted <- service.accept(ready)(_ => IO.pure(Right(ready.stamp)))
    yield
      assertEquals(
        early.left.toOption.map(_.code),
        Some("studio-backend.preview-work-not-ready")
      )
      assertEquals(badCounts, Left(BackendError.TamperedPreview(tampered, ready)))
      assertEquals(badRecipe, Left(BackendError.TamperedPreview(changedRecipe, ready)))
      assertEquals(unknown.left.toOption.map(_.code), Some("studio-backend.unknown-preview"))
      assertEquals(
        stale,
        Left(
          BackendError.StalePreview(
            ready.id,
            ready.stamp,
            ready.stamp.copy(dataset = DatasetRevision(999))
          )
        )
      )
      val held = get(accepted)
      assert(
        held.work eq configured.work,
        "submission must reuse the retained native preparation"
      )
  }

  test("an invalidated old cursor cannot publish current rows for a reused draft id") {
    val changed = get(
      RealPrepared.configure(
        configured.revision,
        configured.dataset,
        recipe.copy(initialFixations = InitialFixationChoice.DropFirst),
        admitted
      )
    )
    val request                                                  = get(PageRequest.of(0, 10))
    def idOf(events: Vector[Either[BackendError, PreviewEvent]]) = get(events.head) match
      case PreviewEvent.Initial(id, _, _) => id
      case other                          => fail(s"expected Initial, got $other")
    for
      service     <- RealPreview.create[IO]
      old         <- service.begin(configured, onePage).take(1).compile.toVector
      _           <- service.invalidate(Set(configured.revision))
      _           <- finish(service, idOf(old))
      invalidated <- service.rows(configured.revision, request)
      fresh       <- service.begin(changed, onePage).take(1).compile.toVector
      _           <- service.continue(idOf(old), onePage).compile.toVector
      notReady    <- service.rows(configured.revision, request)
      freshEvents <- finish(service, idOf(fresh))
      current     <- service.rows(configured.revision, request)
    yield
      assert(invalidated.isLeft && notReady.isLeft)
      assert(current.isRight)
      val receipt = freshEvents
        .collectFirst { case PreviewEvent.Ready(value) => value }
        .getOrElse(fail("no fresh Ready"))
      assertEquals(receipt.recipe, Some(changed.recipe))
  }
