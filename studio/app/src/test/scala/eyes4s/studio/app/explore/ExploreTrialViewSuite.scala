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

package eyes4s.studio.app.explore

import eyes4s.plan.{MapPlacement, OffWindowPolicy}
import eyes4s.studio.app.geometry.Loading
import eyes4s.studio.app.maps.MapId
import eyes4s.studio.app.nav.Place
import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.core.assets.DisplayKind
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.fixture.{GoldenAssets, MockStudy, StoryMoment, StoryMoments}
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.selection.StudioRef

import scala.concurrent.{ExecutionContext, Future}

/** Explore's trial view, headless (ticket S6.2; Explore.dc.html, centre), on
  * the fake backend at t2: P17 enc_03 under rev 4 (run 7, data r3), its 13
  * admitted fixations with one outside the window, and its σ 2° preview.
  */
class ExploreTrialViewSuite extends munit.FunSuite:
  import StoryMoments.r3

  private given ExecutionContext = ExecutionContext.global

  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private def model: AppModel = StoryModels.t2Explore
  private val rev4            = AnalysisRevision(4)
  private val enc03           = StoryModels.p17enc03
  private val ret09           = MockStudy.key("P17", "ret_09")

  private def served(
      trial: TrialKey
  ): Future[(Either[String, TrialFixations], Either[String, TrialPreview])] =
    for
      session <- HeadlessSession.open(StoryMoment.T2)
      fs      <- session.trialFixations(rev4, trial)
      p       <- session.trialPreview(rev4, trial)
      _       <- session.close
    yield (fs.left.map(_.message), p.left.map(_.message))

  private def displays(m: AppModel): DisplaySource =
    DisplaySource.Served(ok(GoldenAssets.registry(m.document.dataset(r3).get)))

  /** The view synced to `m` with the backend's answers read. */
  private def loaded(
      m: AppModel,
      answers: (Either[String, TrialFixations], Either[String, TrialPreview]),
      display: DisplaySource
  ): ExploreTrialView =
    val (synced, effects) = ExploreTrialView.sync(ExploreTrialView.empty, m)
    val trial             = TrialsNavigator.selected(m).get
    assertEquals(
      effects,
      Vector(
        TrialViewEffect.RequestFixations(rev4, trial, 1),
        TrialViewEffect.RequestPreview(rev4, trial, 1),
        TrialViewEffect.RequestDisplays(rev4, m.document.dataset(r3).get, 1)
      )
    )
    Vector(
      TrialViewIntent.FixationsRead(rev4, trial, 1, answers._1),
      TrialViewIntent.PreviewRead(rev4, trial, 1, answers._2),
      TrialViewIntent.DisplaysRead(rev4, 1, Right(display))
    ).foldLeft(synced)((v, i) => ExploreTrialView.update(v, i)._1)

  test("t2: P17 enc_03 under rev 4, neutral marks, the preview map labelled not a result") {
    served(enc03).map { answers =>
      val view = loaded(model, answers, displays(model))
      val vm   = ExploreTrialViewVM.of(view)
      assertEquals(vm.title, "P17 · enc_03 · beach-042")
      assertEquals(
        vm.toggles.map(t => (t.label, t.on, t.enabled)),
        Vector(("Points", true, true), ("Order", true, true), ("Map", true, true))
      )
      assertEquals(vm.mapLabel, Some("Map: preview · σ 2° · not a result"))
      assertEquals(vm.hint, "Canvas focused · arrows move · Enter selects · Esc clears")
      val shown = vm.shown.getOrElse(fail("nothing shown"))
      assertEquals((shown.trial, shown.display.kind), (enc03, DisplayKind.Image))
      assertEquals(shown.fixations.size, 13)
      // The map is the preview, identified as one: never a run's result.
      val grid = shown.map.getOrElse(fail("no preview map"))
      assertEquals(grid.map, MapId.Preview(rev4, enc03))
      assertEquals(grid.map.address, None)
      assertEquals((grid.columns, grid.rows), (64, 48))
      assertEquals(
        vm.legend.map(_.label),
        Vector(
          "Fixation · marker area ∝ duration",
          "Outside the analysis window · excluded from the map",
          "Order between fixation centres, not measured saccades",
          "Image",
          "Preview density · σ 2° · not a result"
        )
      )
      assertEquals((vm.note, vm.retry, vm.mapNote), (None, None, None))
    }
  }

  test("the outside-window fixation is flagged by its placement, from the backend") {
    served(enc03).map { answers =>
      val shown =
        ExploreTrialViewVM.of(loaded(model, answers, displays(model))).shown.get
      val out = shown.fixations.filterNot(_.placement == MapPlacement.InMap)
      assertEquals(
        out.map(f => (f.ref.index.value, f.placement)),
        Vector((10, MapPlacement.OutsideWindow(OffWindowPolicy.Exclude)))
      )
    }
  }

  test("the toggles turn the marks, the order lines and the map off and on") {
    served(enc03).map { answers =>
      val view                    = loaded(model, answers, displays(model))
      def toggled(t: TrialToggle) =
        ExploreTrialView.update(view, TrialViewIntent.Switch(t))._1
      val noPoints = ExploreTrialViewVM.of(toggled(TrialToggle.Points))
      assertEquals(noPoints.shown.map(_.points), Some(false))
      assert(!noPoints.legend.exists(_.swatch == LegendSwatch.Fixation), noPoints.legend)
      assertEquals(noPoints.toggles.head.accessible, "Points, off")
      val noOrder = ExploreTrialViewVM.of(toggled(TrialToggle.Order))
      assertEquals(noOrder.shown.map(_.order), Some(false))
      assert(!noOrder.legend.exists(_.swatch == LegendSwatch.OrderLine))
      val noMap = ExploreTrialViewVM.of(toggled(TrialToggle.Map))
      assertEquals((noMap.shown.flatMap(_.map), noMap.mapLabel), (None, None))
      assert(!noMap.legend.exists(_.swatch == LegendSwatch.PreviewMap))
      // Twice is back on.
      val back = ExploreTrialView
        .update(toggled(TrialToggle.Map), TrialViewIntent.Switch(TrialToggle.Map))
        ._1
      assertEquals(ExploreTrialViewVM.of(back).mapLabel, vm(view).mapLabel)
    }
  }

  private def vm(v: ExploreTrialView) = ExploreTrialViewVM.of(v)

  test("a preview the backend cannot give disables Map and says why; a retry asks again") {
    served(enc03).map { answers =>
      val view = loaded(model, (answers._1, Left("no preview for this trial")), displays(model))
      val v    = vm(view)
      assertEquals(v.toggles.last.enabled, false)
      assertEquals(v.mapLabel, None)
      assertEquals(v.mapNote, Some("Map preview not available: no preview for this trial"))
      assertEquals(v.shown.flatMap(_.map), None)
      assertEquals(v.retry, Some("Retry"))
      val (again, effects) = ExploreTrialView.update(view, TrialViewIntent.Retry)
      assertEquals(effects, Vector(TrialViewEffect.RequestPreview(rev4, enc03, 2)))
      assertEquals(again.fixations, view.fixations)
      // The first ask's late answer is ignored.
      val late = ExploreTrialView
        .update(
          again,
          TrialViewIntent.PreviewRead(rev4, enc03, 1, answers._2)
        )
        ._1
      assertEquals(late, again)
    }
  }

  test("following Explore: a new trial asks again; the same trial and revision asks nothing") {
    served(enc03).map { answers =>
      val view = loaded(model, answers, displays(model))
      assertEquals(ExploreTrialView.sync(view, model), (view, Vector.empty))
      val explored =
        AppModel.update(model, Intent.Explain(Place.At(StudioRef.Trial(ret09))))._1
      val (moved, effects) = ExploreTrialView.sync(view, explored)
      assertEquals(moved.trial, Some(ret09))
      assertEquals(
        effects.map(_.productPrefix),
        Vector("RequestFixations", "RequestPreview", "RequestDisplays")
      )
      // An answer for the previous trial no longer applies.
      val stale = ExploreTrialView
        .update(
          moved,
          TrialViewIntent.FixationsRead(rev4, enc03, moved.ask, answers._1)
        )
        ._1
      assertEquals(stale, moved)
      assertEquals(moved.points && moved.order && moved.map, true)
    }
  }

  test("an absent trial has no fixations: the view says so and offers Retry") {
    served(ret09).map { answers =>
      val explored =
        AppModel.update(model, Intent.Explain(Place.At(StudioRef.Trial(ret09))))._1
      val view = loaded(explored, answers, displays(explored))
      val v    = ExploreTrialViewVM.of(view)
      assertEquals(v.shown, None)
      assert(
        v.note.exists(_.startsWith("The fixations of P17 · ret_09 are not available: ")),
        v.note
      )
      assertEquals(v.retry, Some("Retry"))
    }
  }

  test("display kinds not served: the trial is drawn on its screen without its display") {
    served(enc03).map { answers =>
      val view = loaded(model, answers, DisplaySource.NotServed)
      val v    = vm(view)
      assertEquals(v.shown.map(_.display.kind), Some(DisplayKind.Unknown))
      assertEquals(v.title, "P17 · enc_03")
      assert(v.note.exists(_.startsWith("Display kinds of r3 are not served")), v.note)
      assertEquals(v.retry, None)
    }
  }

  test("nothing to show: no trial on Explore's trail, or no run shown") {
    val first           = StoryModels.firstRun
    val (none, effects) = ExploreTrialView.sync(ExploreTrialView.empty, first)
    assertEquals(effects, Vector.empty)
    assertEquals(
      ExploreTrialViewVM.of(none).note,
      Some("Choose a trial in the Trials navigator to explore it.")
    )
    assertEquals(ExploreTrialView.shownRevision(model), Some(rev4))
  }
