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

import cats.effect.IO
import cats.syntax.all.*
import eyes4s.plan.{ResultInspection, ResultRef, ScaleContrast, StudyDesign}
import eyes4s.studio.app.{AppEffect, Intent, PreparedDesign}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.*
import eyes4s.studio.core.execution.*
import eyes4s.studio.core.figures.MethodsReads
import scala.concurrent.duration.*

/** Small adversarial studies execute through Save & run and the native services.
  * Their oracles prepare and evaluate the same declared study directly in eyes4s.
  */
class AdversarialScienceSuite extends munit.CatsEffectSuite:
  import NativeScienceHarness.{Fixture as NativeFixture, *}
  override val munitIOTimeout: Duration                = 120.seconds
  private val SpatialScoreTolerance                    = 1e-12
  private def get[E, A](value: Either[E, A]): A        = value.fold(e => fail(s"$e"), identity)
  private def ok[E, A](value: IO[Either[E, A]]): IO[A] = value.map(get)

  private def execute(h: Harness): IO[RunId] =
    for
      emitted <- h.dispatch(Intent.Dispatch(Command.SaveAndRun(None)))
      _   = assert(emitted.exists(_.isInstanceOf[AppEffect.Execution]))
      run = h.model.get().document.runs.last.id
      _ <- h.until {
        case Intent.Execution(ExecutionEvent.Ready(value)) => value.run == run
        case _                                             => false
      }
    yield run

  private def rows(h: Harness, run: RunId): IO[Vector[QueryRow]] =
    ok(MethodsReads.queryRows[IO](h.session.reads.queries, run))

  private def qualify(f: NativeFixture, h: Harness, run: RunId): IO[Vector[QueryRow]] =
    for
      served <- rows(h, run)
      prepared = direct(f, h.model.get().document)
      result <- IO.blocking(get(prepared.work.run))
    yield
      val inspection = get(
        ResultInspection.study(
          prepared.plan,
          result,
          prepared.admitted.input,
          Some(prepared.admitted.evidence)
        )
      )
      val keys = prepared.admitted.input.trials.rows
        .map(_.key)
        .filter(_.phase == prepared.recipe.phases.focal.label)
      assertEquals(
        served.map(r => (r.query.participant, r.query.trial, r.query.occurrence)).toSet,
        keys.map(k => (k.participant, k.trial, k.occurrence.value)).toSet
      )
      assertEquals(served.size, keys.size)
      served.foreach { row =>
        val key = keys
          .find(k =>
            k.participant == row.query.participant && k.trial == row.query.trial && k.occurrence.value == row.query.occurrence
          )
          .get
        inspection.scales.zipWithIndex.foreach { (scale, index) =>
          val contrast = scale.contrast match
            case ScaleContrast.Rows(listing) =>
              listing.get(ResultRef.ContrastRow(index, key)).get.outcome
            case other => fail(s"no direct contrast rows: $other")
          row.status match
            case QueryStatus.Contributing(ms, bs, ds) =>
              assertEquals(ds(index), get(contrast).value.value)
              Vector(StudyDesign.Matched -> ms(index), StudyDesign.Control -> bs(index))
                .foreach { (design, value) =>
                  val reduction =
                    scale.reductions(design).get(ResultRef.Reduction(index, design, key)).get
                  assertEquals(value, get(reduction.outcome).value.value)
                }
            case status if status.isFailed =>
              assert(contrast.isLeft, s"${row.query}: $contrast")
            case other => fail(s"unexpected adversarial outcome $other")
        }
      }
      served

  private def run(f: NativeFixture): IO[Vector[QueryRow]] = harness(f.document, f.sources).use {
    h =>
      execute(h).flatMap(qualify(f, h, _))
  }

  test("identical weighted maps give D zero through the command route and direct library") {
    run(fixture(identical = true)).map { rows =>
      assertEquals(rows.size, 2)
      rows.foreach { row =>
        row.status match
          case QueryStatus.Contributing(m, b, d) =>
            assertEquals(m, b)
            assertEquals(d, Vector(0.0))
          case other => fail(s"identical maps failed: $other")
      }
    }
  }

  test("empty maps fail explicitly through the command route and direct library") {
    run(fixture(emptyMaps = true)).map { rows =>
      assertEquals(rows.size, 2)
      assert(rows.forall(_.status.isFailed), rows.toString)
    }
  }

  test("reordering fixed weighted positions preserves every spatial score") {
    for
      original  <- run(fixture())
      reordered <- run(fixture(reversePositions = true))
    yield
      assertEquals(reordered.map(_.query), original.map(_.query))
      // The estimator's accumulation order may change its final rounding.
      original.zip(reordered).foreach { (a, b) =>
        (a.status, b.status) match
          case (QueryStatus.Contributing(am, ab, ad), QueryStatus.Contributing(bm, bb, bd)) =>
            (am ++ ab ++ ad).zip(bm ++ bb ++ bd).foreach { (x, y) =>
              assertEqualsDouble(x, y, SpatialScoreTolerance)
            }
          case other => fail(s"reordered maps did not contribute: $other")
      }
  }

  test("duplicate matched references block the native command and the direct plan") {
    val f = fixture(repeated = true)
    harness(f.document, f.sources).use { h =>
      for
        _     <- h.dispatch(Intent.SwitchPerspective(Perspective.Analysis))
        ready <- h.prepared
        recipe = f.document.draft.get.recipe(f.document.analyses.head.recipe)
        _ <- h.dispatch(Intent.DesignPrepared(PreparedDesign(ready, recipe)))
        before = h.model.get()
        design = eyes4s.studio.app.analysis.ResolvedDesign.empty.copy(
          target = eyes4s.studio.app.analysis.ResolvedDesign.target(before),
          preview = eyes4s.studio.app.analysis.DesignPreview.Ready(ready)
        )
        card = eyes4s.studio.app.analysis.Preflight.vm(design, before).card
        _    = assert(!card.enabled)
        _    = assertEquals(card.run, None)
        // A forced submission still reaches the native cardinality guard.
        emitted <- h.dispatch(Intent.Dispatch(Command.SaveAndRun(None)))
        requested = h.model.get().document.runs.last.id
        refused <- h.until(_.isInstanceOf[Intent.PreparedRefused])
        jobs    <- h.session.backend.jobs
      yield
        val directPrepared = direct(f, f.document)
        assert(directPrepared.pairingRefusal.nonEmpty)
        val findings = ready.diagnostics.filter(_.code == "study-finding.matched-cardinality")
        assertEquals(
          findings.flatMap(_.affected).toSet,
          Set(
            TrialKey("P1", Phase.Retrieval, "ret_1", 1),
            TrialKey("P1", Phase.Encoding, "enc_1", 1),
            TrialKey("P1", Phase.Encoding, "enc_9", 1)
          )
        )
        assert(emitted.exists(_.isInstanceOf[AppEffect.Execution]), emitted.toString)
        refused match
          case Intent.PreparedRefused(
                `ready`,
                ExecutionError.Backend(error),
                Some(`requested`)
              ) =>
            assertEquals(error.code, "studio-backend.unavailable")
          case other => fail(s"wrong native cardinality refusal: $other")
        assertEquals(
          h.model.get().document.run(requested).map(_.state),
          Some(RunLifecycle.Failed)
        )
        assertEquals(
          h.model.get().document.run(requested).map(_.archive),
          Some(CoreBinding.unbound)
        )
        assertEquals(h.model.get().document.presentation.shownRun, None)
        assertEquals(jobs, Vector.empty)
    }
  }

  test("an unrelated participant changes none of the original participant's results") {
    for
      original <- run(fixture())
      expanded <- run(fixture(participants = 2))
    yield
      assertEquals(expanded.filter(_.query.participant == "P1"), original)
      assertEquals(expanded.count(_.query.participant == "P2"), original.size)
  }

  test("blank display imagery retains item-specific matches and distinct control references") {
    val f = fixture(items = 3, imagery = true)
    harness(f.document, f.sources).use { h =>
      for
        id     <- execute(h)
        served <- qualify(f, h, id)
        pairs  <- ok(h.session.reads.pairRows(id, 0, get(PageRequest.of(0, 100))))
      yield
        assertEquals(served.size, 3)
        served.foreach { row =>
          assertEquals(row.query.phase, Phase("Imagery"))
          assertEquals(row.matched.map(_.trial), Some(row.query.trial.replace("ret_", "enc_")))
          assertEquals(row.controls, Some(2))
        }
        val matched = pairs.rows.filter(_.design == PairDesign.Matched)
        assertEquals(matched.size, 3)
        assertEquals(matched.map(_.referenceItem).toSet, Set("item-1", "item-2", "item-3"))
        assertEquals(pairs.rows.count(_.design == PairDesign.Control), 6)
    }
  }

  test("presentation commands and map palettes change no native scientific value") {
    val f = fixture()
    harness(f.document, f.sources).use { h =>
      for
        id     <- execute(h)
        before <- qualify(f, h, id)
        science = h.model.get().document.science
        grid <- ok(h.session.reads.mapGrid(id, 0, before.head.query))
        opacity = get(MapOpacity.of(0.2))
        opacityEffects <- h.dispatch(Intent.Dispatch(Command.SetMapOpacity(opacity)))
        themeEffects   <- h.dispatch(Intent.Dispatch(Command.SetTheme(Theme.Dark)))
        after          <- qualify(f, h, id)
        afterGrid      <- ok(h.session.reads.mapGrid(id, 0, before.head.query))
      yield
        import eyes4s.studio.app.maps.*
        val map = get(
          MapGrid.of(
            MapId(id, before.head.query, get(eyes4s.studio.core.selection.ScaleIndex.of(0))),
            grid.columns,
            grid.rows,
            grid.order,
            grid.cells.map(Some(_)),
            grid.levels.map(_.threshold)
          )
        )
        val mass = MapRaster.render(map, ColourLimits.spanning(MapPalette.Mass, Vector(map)))
        val difference =
          MapRaster.render(map, ColourLimits.spanning(MapPalette.Difference, Vector(map)))
        assertNotEquals(Vector.from(mass.argb), Vector.from(difference.argb))
        assertEquals(afterGrid, grid)
        assertEquals(after, before)
        assertEquals(h.model.get().document.science, science)
        assertEquals(h.model.get().document.presentation.mapOpacity, opacity)
        assert(!(opacityEffects ++ themeEffects).exists(_.isInstanceOf[AppEffect.Execution]))
    }
  }

  test("delayed cancellation of an old native job cannot overwrite a newer native result") {
    val f = fixture(participants = 20, items = 4)
    harness(f.document, f.sources).use { h =>
      def holdCancelled(held: Vector[Intent] = Vector.empty): IO[Vector[Intent]] =
        h.raw.flatMap { intent =>
          val next = held :+ intent
          intent match
            case Intent.Execution(ExecutionEvent.Changed(job))
                if job.phase.isInstanceOf[JobPhase.Cancelled] =>
              IO.pure(next)
            case _ => holdCancelled(next)
        }
      for
        _ <- h.dispatch(Intent.Dispatch(Command.SaveAndRun(None)))
        oldRun = h.model.get().document.runs.last.id
        started <- h.until {
          case Intent.Execution(ExecutionEvent.Changed(job)) => !job.phase.isTerminal
          case _                                             => false
        }
        job = started match
          case Intent.Execution(ExecutionEvent.Changed(job)) => job
          case other => fail(s"no started native job: $other")
        _    <- h.dispatch(Intent.CancelJob(job.id))
        held <- holdCancelled()
        saved = h.model.get().document.analyses.last.recipe
        _ <- h.dispatch(
          Intent.Dispatch(
            Command.ChangeRecipe(RecipeChange.Grid(saved.grid, get(GridSize.of(48, 32))))
          )
        )
        newRun  <- execute(h)
        before  <- qualify(f, h, newRun)
        _       <- h.dispatch(Intent.ShowRun(newRun))
        _       <- held.traverse_(h.dispatch(_).void)
        after   <- rows(h, newRun)
        backend <- ok(h.session.backend.job(job.id))
      yield
        assertNotEquals(newRun, oldRun)
        assertEquals(after, before)
        assertEquals(h.model.get().document.presentation.shownRun, Some(newRun))
        assertEquals(h.model.get().jobs.shelf.shown, Some(newRun))
        assertEquals(
          h.model.get().document.run(newRun).map(_.state),
          Some(RunLifecycle.Completed)
        )
        assert(
          h.model
            .get()
            .document
            .run(oldRun)
            .exists(_.state.isInstanceOf[RunLifecycle.Cancelled])
        )
        assert(backend.state match
          case JobState.Finished(_: JobOutcome.Cancelled) => true
          case _                                          => false)
    }
  }
