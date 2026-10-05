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
import cats.syntax.all.*
import eyes4s.studio.core.backend.*
import io.circe.syntax.*
import munit.CatsEffectSuite

/** A run's density grids (protocol `MapGridOf`, UI-E) on the fake at t2:
  * run 7's map of a trial at a scale is eyes4s's density of its in-map
  * fixations at the recipe's σ, over rev 4's 64 × 48 grid on the analysis
  * window, with the levels enclosing 50% and 90% of the mass.
  */
class MapGridSuite extends CatsEffectSuite:

  private val run7  = RunId(7)
  private val ret07 = MockStudy.key("P17", "ret_07")
  private val enc03 = MockStudy.key("P17", "enc_03")
  private val ret09 = MockStudy.key("P17", "ret_09")

  /** The focus scale: σ 2° is rev 4's third scale. */
  private val Focus = 2

  /** A density over the grid sums to one, to accumulated rounding. */
  private val MassTolerance: Double = 1e-9

  private def fake = FakeStudyBackend.create[IO](StoryMoment.T2)

  private def grid(backend: FakeStudyBackend[IO], scale: Int, trial: TrialKey) =
    backend.mapGrid(run7, scale, trial).map(_.fold(e => fail(e.message), identity))

  test("P17 ret_07 at σ 2°: rev 4's grid on the window, mass one, levels at 50% and 90%") {
    for
      backend <- fake
      g       <- grid(backend, Focus, ret07)
    yield
      assertEquals(
        (g.run, g.address, g.sigmaDegrees, g.columns, g.rows, g.order),
        (run7, ResultAddress.Estimation(Focus, ret07), 2.0, 64, 48, RowOrder.TopFirst)
      )
      assertEquals(
        (g.region.left, g.region.top, g.region.right, g.region.bottom),
        (448.0, 156.0, 1472.0, 924.0)
      )
      assertEquals((g.cellWidthPx, g.cellHeightPx), (16.0, 16.0))
      // The kernel's cell size under the declared angular scale.
      val degrees = g.cellDegrees.getOrElse(fail("no angular cell size"))
      assertEqualsDouble(degrees.width, degrees.height, 0.0)
      assert(degrees.width > 0.4 && degrees.width < 0.5, degrees)
      assertEquals(g.cells.size, 64 * 48)
      assert(g.cells.forall(_ >= 0.0))
      assertEqualsDouble(g.cells.sum, 1.0, MassTolerance)
      assertEquals(g.levels.map(_.coverage), Vector(0.5, 0.9))
      assert(g.levels(0).threshold > g.levels(1).threshold, g.levels)
      // The half-mass region holds at least half the mass, and only just.
      val half = g.cells.filter(_ >= g.levels(0).threshold).sum
      assert(half >= 0.5 && half < 0.6, half)
      assertEquals(g.asJson.as[DensityGrid], Right(g))
  }

  test("each scale is the recipe's σ: a wider kernel flattens the same trial's peak") {
    for
      backend <- fake
      grids   <- (0 to 3).toVector.traverse(grid(backend, _, ret07))
    yield
      assertEquals(grids.map(_.sigmaDegrees), Vector(0.5, 1.0, 2.0, 4.0))
      val peaks = grids.map(_.cells.max)
      assertEquals(peaks, peaks.sorted.reverse)
      assertEquals(peaks.distinct.size, 4)
  }

  /** P17 ret_07's matched M at σ 0.5°, 1°, 2° and 4° on the real fixture,
    * as an independent numpy reimplementation of the pipeline reproduced it
    * (review of S0.7b's SCORES.json, to 6 dp). fixture.json's scores are
    * the board's illustrative numbers, not these, so they are no oracle.
    */
  private val RealMatchedM: Vector[Double] = Vector(0.463138, 0.823414, 0.936849, 0.974504)

  /** SCORES.json's rounding: six decimal places. */
  private val ScoreTolerance: Double = 5e-7

  test("the grids are the run's maps: ret_07 · enc_03's cosine is the real fixture's M") {
    def cosine(a: Vector[Double], b: Vector[Double]): Double =
      a.lazyZip(b).map(_ * _).sum / math.sqrt(a.map(x => x * x).sum * b.map(x => x * x).sum)
    for
      backend <- fake
      scores  <- (0 to 3).toVector.traverse(s =>
        (grid(backend, s, ret07), grid(backend, s, enc03)).mapN((q, m) =>
          cosine(q.cells, m.cells)
        )
      )
    yield scores.zip(RealMatchedM).foreach((c, m) => assertEqualsDouble(c, m, ScoreTolerance))
  }

  test("a missing density preserves the cause's affected trials and remedy") {
    val cause     = ProtocolSamples.diagnostic
    val refusal   = BackendError.NoDensity(run7, ResultAddress.Estimation(Focus, ret07), cause)
    val displayed = refusal.diagnostic
    assertEquals(displayed.affected, cause.affected)
    assertEquals(displayed.category, cause.category)
    assertEquals(displayed.remedy, cause.remedy)
    assertEquals(displayed.origin, cause.origin)
    assert(displayed.subject.contains(DiagnosticLocus.Run(run7)))
    assert(
      displayed.subject.contains(
        DiagnosticLocus.Address(ResultAddress.Estimation(Focus, ret07))
      )
    )
  }

  test("refusals name the run and estimation: unknown run, scale, trial; no scanpath") {
    val stranger = MockStudy.key("P99", "enc_01")
    for
      backend  <- fake
      noRun    <- backend.mapGrid(RunId(99), Focus, ret07)
      noScale  <- backend.mapGrid(run7, 9, ret07)
      notTrial <- backend.mapGrid(run7, Focus, stranger)
      noPath   <- backend.mapGrid(run7, Focus, ret09)
    yield
      assert(noRun.left.exists(_.isInstanceOf[BackendError.UnknownRun]), noRun)
      assertEquals(
        noScale,
        Left(BackendError.UnknownReference(run7, ResultAddress.Estimation(9, ret07)))
      )
      assertEquals(
        notTrial,
        Left(BackendError.UnknownReference(run7, ResultAddress.Estimation(Focus, stranger)))
      )
      val none = BackendError.NoDensity(
        run7,
        ResultAddress.Estimation(Focus, ret09),
        StudioDiagnostic(
          FakeTrialViews.NoDensityCode,
          DiagnosticLevel.Error,
          DiagnosticOrigin.Host,
          Vector(DiagnosticLocus.Trial(ret09)),
          "the trial has no admitted scanpath"
        )
      )
      assertEquals(noPath, Left(none))
      assertEquals(
        none.message,
        "run 7 has no density for the estimation of P17 · ret_09 at scale 2: the trial has " +
          "no admitted scanpath"
      )
      assertEquals(none.code, "studio-backend.no-density")
      assertEquals(
        none.diagnostic.subject,
        Vector(
          DiagnosticLocus.Run(run7),
          DiagnosticLocus.Address(ResultAddress.Estimation(Focus, ret09)),
          DiagnosticLocus.Trial(ret09)
        )
      )
  }

  test("a grid refuses a bad shape, cell, level or angular size, naming the estimation") {
    val at     = ResultAddress.Estimation(Focus, ret07)
    val region =
      DensityGrid.region(run7, Focus, ret07, 448.0, 156.0, 1472.0, 924.0).toOption.get
    def of(
        cells: Vector[Double] = Vector(0.75, 0.25),
        levels: Vector[DensityLevel] = Vector(DensityLevel(0.5, 0.75)),
        degrees: Option[CellDegrees] = None,
        sigma: Double = 2.0,
        columns: Int = 2
    ) =
      DensityGrid
        .of(
          run7,
          Focus,
          ret07,
          sigma,
          region,
          columns,
          1,
          RowOrder.TopFirst,
          degrees,
          cells,
          levels
        )
    assert(of().isRight)
    assertEquals(of(sigma = 0.0), Left(DensityGridError.SigmaNotPositive(run7, at, 0.0)))
    assertEquals(of(columns = 0), Left(DensityGridError.EmptyGrid(run7, at, 0, 1)))
    assertEquals(of(Vector(1.0)), Left(DensityGridError.CellCount(run7, at, 2, 1, 1)))
    assertEquals(
      of(Vector(1.0, -0.5)),
      Left(DensityGridError.CellNotDensity(run7, at, 1, -0.5))
    )
    assertEquals(
      of(Vector(1.0, Double.NaN)).left.map(_.productPrefix),
      Left("CellNotDensity")
    )
    assertEquals(
      of(levels = Vector(DensityLevel(0.5, 0.75), DensityLevel(1.5, 0.1))),
      Left(DensityGridError.LevelOutOfRange(run7, at, 1, 1.5, 0.1))
    )
    assertEquals(
      of(levels = Vector(DensityLevel(0.0, 0.1))),
      Left(DensityGridError.LevelOutOfRange(run7, at, 0, 0.0, 0.1))
    )
    assertEquals(
      of(levels = Vector(DensityLevel(0.5, -0.1))),
      Left(DensityGridError.LevelOutOfRange(run7, at, 0, 0.5, -0.1))
    )
    assertEquals(
      of(degrees = Some(CellDegrees(0.5, 0.0))),
      Left(DensityGridError.CellDegreesNotPositive(run7, at, 0.5, 0.0))
    )
    assertEquals(
      DensityGrid.region(run7, Focus, ret07, 10.0, 0.0, 10.0, 5.0),
      Left(DensityGridError.RegionEmpty(run7, at, 10.0, 0.0, 10.0, 5.0))
    )
    assertEquals(
      DensityGridError.CellNotDensity(run7, at, 1, -0.5).message,
      "Cell 1 of the density grid of estimation of P17 · ret_07 at scale 2 in run 7 holds " +
        "-0.5, which is not a density."
    )
    // Decoding validates again.
    val good = of().toOption.get
    assert(good.asJson.mapObject(_.add("columns", 3.asJson)).as[DensityGrid].isLeft)
    assert(
      good.asJson
        .mapObject(
          _.add(
            "region",
            io.circe.Json.obj(
              "left"   -> 1.0.asJson,
              "top"    -> 1.0.asJson,
              "right"  -> 1.0.asJson,
              "bottom" -> 2.0.asJson
            )
          )
        )
        .as[DensityGrid]
        .isLeft
    )
  }
