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

package eyes4s.studio.core.fixture

import cats.effect.IO
import eyes4s.plan.{MapPlacement, OffWindowPolicy}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.OffWindowChoice
import eyes4s.studio.core.selection.{FixationIndex, StudioRef}
import io.circe.syntax.*
import munit.CatsEffectSuite

/** The trial views of protocol 1.6 (S6.2) on the fake backend at t2: P17
  * enc_03's 13 admitted fixations (fixation 6 is fixations.csv record 7,214
  * at screen (1148, 456), onset 2160 ms, 412 ms; FIXTURE.md), one outside
  * the analysis window, and its σ 2° preview from eyes4s's own smoother.
  * Positions and timing are checked against fixations.csv itself in the JVM
  * suite
  * (`TrialViewsJvmSuite`), placement against the fixture README's
  * half-open window.
  */
class TrialViewsSuite extends CatsEffectSuite:

  private val rev4  = AnalysisRevision(4)
  private val r3    = DatasetRevision(3)
  private val enc03 = MockStudy.key("P17", "enc_03")
  private val ret09 = MockStudy.key("P17", "ret_09")

  /** A density over the grid sums to one, to accumulated rounding. */
  private val MassTolerance: Double = 1e-9

  private def fake = FakeStudyBackend.create[IO](StoryMoment.T2)

  /** The fixture README's window: the image frame, half-open. */
  private def insideWindow(x: Double, y: Double): Boolean =
    x >= 448 && x < 1472 && y >= 156 && y < 924

  test("a trial the study fails marks its in-window fixations TrialFailed, with its tally") {
    val failing = Vector(
      MapPlacement.InWindow                                 -> 200.0,
      MapPlacement.OutsideWindow(OffWindowPolicy.FailTrial) -> 100.0,
      MapPlacement.OutsideScreen                            -> 50.0,
      MapPlacement.InWindow                                 -> 25.5
    )
    val tally = eyes4s.plan.WindowTally
      .of(
        1,
        1,
        4,
        eyes4s.kernel.Span.micros(50000L),
        eyes4s.kernel.Span.micros(100000L),
        eyes4s.kernel.Span.micros(375500L)
      )
      .fold(e => fail(e.message), identity)
    assertEquals(
      FakeTrialViews.trialPlacements(failing),
      Right(
        Vector(
          MapPlacement.TrialFailed(tally),
          MapPlacement.OutsideWindow(OffWindowPolicy.FailTrial),
          MapPlacement.OutsideScreen,
          MapPlacement.TrialFailed(tally)
        )
      )
    )
    // A trial the window policy keeps is placed as it was.
    val kept = failing.map((p, d) =>
      (if p == MapPlacement.OutsideWindow(OffWindowPolicy.FailTrial) then
         MapPlacement.OutsideWindow(OffWindowPolicy.Exclude)
       else p) -> d
    )
    assertEquals(FakeTrialViews.trialPlacements(kept), Right(kept.map(_._1)))
  }

  test("under FailTrial, enc_03 fails: its trial view and its source records say so, tallied") {
    // Revision 4's study with its off-window policy turned to FailTrial;
    // fixation 10 (record 7,218) lies left of the image, outside the window.
    val (recipe, geometry) =
      FakeTrialViews.study(StoryMoment.T2, rev4).fold(e => fail(e.message), identity)
    val failing = recipe.copy(offWindow = Some(OffWindowChoice.FailTrial))
    val view    = FakeTrialViews
      .under(failing, geometry, rev4, r3, enc03)
      .fold(e => fail(e.message), identity)
    val outside = view.fixations(9)
    assertEquals(outside.placement, MapPlacement.OutsideWindow(OffWindowPolicy.FailTrial))
    val tallies = view.fixations.patch(9, Nil, 1).map(_.placement).map {
      case MapPlacement.TrialFailed(t) => t
      case other                       => fail(s"expected TrialFailed, got $other")
    }
    assertEquals(tallies.distinct.size, 1)
    val tally = tallies.head
    assertEquals((tally.outsideScreen, tally.outsideWindow, tally.total), (0, 1, 13))
    def micros(ms: Double) = math.round(ms * 1000.0)
    assertEquals(tally.outsideWindowDuration.toMicros, micros(outside.durationMs))
    assertEquals(
      tally.totalDuration.toMicros,
      view.fixations.map(f => micros(f.durationMs)).sum
    )
    // The same placements on the trial's source records, in one page.
    val source = StorySeed
      .document(StoryMoment.T2)
      .toOption
      .flatMap(_.dataset(r3))
      .flatMap(_.sources.fixations)
      .getOrElse(fail("no fixation source"))
    val page = FakeSourceRecords
      .serve(rev4, r3, failing, geometry, source, view.fixations.head.record, 13)
      .fold(e => fail(e.message), identity)
    assertEquals(page.rows.map(_.record), view.fixations.map(_.record))
    assertEquals(page.rows.map(_.placement), view.fixations.map(f => Some(f.placement)))
    // Revision 4 itself excludes the fixation and fails nothing.
    val kept = FakeTrialViews
      .under(recipe, geometry, rev4, r3, enc03)
      .fold(e => fail(e.message), identity)
    assert(kept.fixations.forall(!_.placement.isInstanceOf[MapPlacement.TrialFailed]))
  }

  test("P17 enc_03: 13 fixations in scanpath order; fixation 6 is record 7,214") {
    fake.flatMap(_.trialFixations(rev4, enc03)).map { result =>
      val view = result.fold(e => fail(e.message), identity)
      assertEquals((view.revision, view.dataset, view.trial), (rev4, r3, enc03))
      assertEquals(view.fixations.size, 13)
      // Records count up through the trial (fixations.csv is ordinal-sorted here).
      assertEquals(view.fixations.map(_.record), (7209 to 7221).toVector)
      assertEquals(
        view.fixations.map(_.ref.index.value),
        (1 to 13).toVector
      )
      // FIXTURE.md's focus fixation.
      val six = view.fixations(5)
      assertEquals(
        (six.record, six.screenX, six.screenY, six.onsetMs, six.durationMs),
        (7214, 1148.0, 456.0, 2160.0, 412.0)
      )
      // Placement: in the map unless outside the half-open window (rev 4
      // excludes off-window fixations); fixation 10 is left of the image.
      assertEquals(
        view.fixations.map(_.placement),
        view.fixations.map(f =>
          if insideWindow(f.screenX, f.screenY) then MapPlacement.InWindow
          else MapPlacement.OutsideWindow(OffWindowPolicy.Exclude)
        )
      )
      assertEquals(
        view.fixations.filterNot(_.placement == MapPlacement.InWindow).map(_.ref.index.value),
        Vector(10)
      )
      // The view crosses the wire unchanged.
      assertEquals(view.asJson.as[TrialFixations], Right(view))
    }
  }

  test("P17 enc_03's preview is eyes4s's σ 2° density of its in-map fixations") {
    fake.flatMap(_.trialPreview(rev4, enc03)).map { result =>
      val preview = result.fold(e => fail(e.message), identity)
      assertEquals(
        (preview.trial, preview.sigmaDegrees, preview.columns, preview.rows, preview.order),
        (enc03, 2.0, 64, 48, RowOrder.TopFirst)
      )
      // The grid covers rev 4's analysis window, in screen pixels.
      assertEquals(
        (preview.region.left, preview.region.top, preview.region.right, preview.region.bottom),
        (448.0, 156.0, 1472.0, 924.0)
      )
      val cells = preview.cells.map(_.getOrElse(fail("a cell without a value")))
      assert(cells.forall(_ >= 0.0))
      assertEqualsDouble(cells.sum, 1.0, MassTolerance)
      // Two highest-density levels, the half-mass one the higher.
      assertEquals(preview.levels.size, 2)
      assert(preview.levels(0) > preview.levels(1), preview.levels)
      // Row 0 is the top: the densest cell is in the upper half, where
      // P17 enc_03's fixations cluster (screen y 294 to 741).
      val peak = cells.indexOf(cells.max)
      assert(peak / 64 < 30, peak)
      assertEquals(preview.asJson.as[TrialPreview], Right(preview))
    }
  }

  test("refusals: a trial without fixations, a revision on other data, an unknown revision") {
    for
      backend     <- fake
      absent      <- backend.trialFixations(rev4, ret09)
      noMap       <- backend.trialPreview(rev4, ret09)
      onR2        <- backend.trialFixations(AnalysisRevision(3), enc03)
      unknown     <- backend.trialPreview(AnalysisRevision(99), enc03)
      stranger    <- backend.trialFixations(rev4, MockStudy.key("P99", "enc_01"))
      strangerMap <- backend.trialPreview(rev4, MockStudy.key("P99", "enc_01"))
    yield
      // A trial outside r3's inventory is refused by name, not as no data.
      val p99 = MockStudy.key("P99", "enc_01")
      assertEquals(stranger, Left(BackendError.UnknownTrial(r3, p99)))
      assertEquals(strangerMap, Left(BackendError.UnknownTrial(r3, p99)))
      assertEquals(absent, Left(BackendError.Unavailable(DiagnosticLocus.Trial(ret09))))
      assertEquals(noMap, Left(BackendError.Unavailable(DiagnosticLocus.Trial(ret09))))
      assertEquals(
        onR2,
        Left(BackendError.Unavailable(DiagnosticLocus.Revision(AnalysisRevision(3))))
      )
      assert(
        unknown match
          case Left(BackendError.UnknownRevision(r, _)) => r == AnalysisRevision(99)
          case _                                        => false
        ,
        unknown
      )
  }

  test("the study is the fake's own moment's: rev 5 is a saved recipe at t3, a draft at t2") {
    val rev5 = AnalysisRevision(5)
    for
      atT2 <- fake.flatMap(_.trialFixations(rev5, enc03))
      t3   <- FakeStudyBackend.create[IO](StoryMoment.T3)
      atT3 <- t3.trialFixations(rev5, enc03)
      t1   <- FakeStudyBackend.create[IO](StoryMoment.T1)
      atT1 <- t1.trialFixations(rev4, enc03)
    yield
      assertEquals(atT2, Left(BackendError.Unavailable(DiagnosticLocus.Revision(rev5))))
      assertEquals(atT3.map(_.fixations.size), Right(13))
      assert(
        atT1 match
          case Left(BackendError.UnknownRevision(r, _)) => r == rev4
          case _                                        => false
        ,
        atT1
      )
  }

  test("the views refuse invalid fixations and grids, naming what failed") {
    val fails = TrialViewError.TrialFails(enc03, Vector(10))
    assertEquals(
      BackendError.TrialViewRefused(fails).message,
      "The study fails P17 · enc_03: fixation 10 lies outside the analysis window, so it " +
        "has no map."
    )
    assertEquals(
      BackendError.TrialViewRefused(fails).diagnostic.subject,
      Vector(DiagnosticLocus.Trial(enc03))
    )
    def ref(i: Int, trial: TrialKey = enc03): StudioRef.Fixation =
      StudioRef.Fixation(trial, FixationIndex.of(i).toOption.get)
    def fix(i: Int, record: Int = 1, x: Double = 1.0, onset: Double = 0.0, d: Double = 10.0) =
      AdmittedFixation.of(ref(i), record, x, 2.0, onset, d, MapPlacement.InWindow)
    assertEquals(fix(1, record = 0), Left(TrialViewError.RecordNotPositive(enc03, 1, 0)))
    assertEquals(
      fix(1, x = Double.NaN).left.map(_.productPrefix),
      Left("PositionNotFinite")
    )
    assertEquals(fix(1, onset = -1.0), Left(TrialViewError.OnsetNegative(enc03, 1, -1.0)))
    assertEquals(fix(1, d = 0.0), Left(TrialViewError.DurationNotPositive(enc03, 1, 0.0)))
    val one = fix(1).toOption.get
    val two = fix(2).toOption.get
    assertEquals(
      TrialFixations.of(rev4, r3, enc03, Vector(two, one)),
      Left(TrialViewError.PositionOutOfOrder(enc03, 0, 2))
    )
    val other = AdmittedFixation
      .of(ref(1, ret09), 1, 1.0, 2.0, 0.0, 10.0, MapPlacement.InWindow)
      .toOption
      .get
    assertEquals(
      TrialFixations.of(rev4, r3, enc03, Vector(other)),
      Left(TrialViewError.OtherTrial(enc03, ret09))
    )
    val region = ScreenRegion.of(enc03, 448.0, 156.0, 1472.0, 924.0).toOption.get
    assertEquals(
      ScreenRegion.of(enc03, 10.0, 0.0, 10.0, 5.0),
      Left(TrialViewError.RegionEmpty(enc03, 10.0, 0.0, 10.0, 5.0))
    )
    def grid(cells: Vector[Option[Double]], levels: Vector[Double] = Vector.empty) =
      TrialPreview.of(rev4, enc03, 2.0, region, 2, 1, RowOrder.TopFirst, cells, levels)
    assertEquals(grid(Vector(Some(0.5))), Left(TrialViewError.CellCount(enc03, 2, 1, 1)))
    assertEquals(
      grid(Vector(Some(0.5), Some(-0.1))),
      Left(TrialViewError.CellNotDensity(enc03, 1, -0.1))
    )
    assertEquals(
      grid(Vector(Some(0.5), None), Vector(Double.PositiveInfinity)),
      Left(TrialViewError.LevelNotFinite(enc03, 0, Double.PositiveInfinity))
    )
    assertEquals(
      TrialPreview
        .of(rev4, enc03, 0.0, region, 1, 1, RowOrder.TopFirst, Vector(None), Vector.empty),
      Left(TrialViewError.SigmaNotPositive(enc03, 0.0))
    )
    // Decoding validates again.
    val wire = one.asJson.mapObject(_.add("durationMs", (-5.0).asJson))
    assert(wire.as[AdmittedFixation].isLeft)
    assert(
      grid(Vector(Some(0.5), None)).toOption.get.asJson
        .mapObject(_.add("columns", 3.asJson))
        .as[TrialPreview]
        .isLeft
    )
  }
