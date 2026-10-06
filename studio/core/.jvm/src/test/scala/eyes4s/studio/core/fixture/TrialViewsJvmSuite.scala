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
import eyes4s.kernel.*
import eyes4s.studio.core.backend.*
import eyes4s.surface.{EdgePolicy, Smoother}
import munit.CatsEffectSuite

/** The protocol 1.6 trial views (S6.2) checked against fixtures/studio-golden's
  * own text (`GoldenCsv`, JVM test scope): P17 enc_03's fixations are
  * fixations.csv's records, in ordinal order, and its preview equals eyes4s's
  * Gaussian smoother called here on the in-window records, cell for cell.
  */
class TrialViewsJvmSuite extends CatsEffectSuite:

  private val rev4  = AnalysisRevision(4)
  private val enc03 = MockStudy.key("P17", "enc_03")

  /** Cell values are the same computation's: they agree to rounding. */
  private val CellTolerance: Double = 1e-12

  private def fake = FakeStudyBackend.create[IO](StoryMoment.T2)

  /** fixations.csv's data records of `trial`, numbered from 1 (header excluded). */
  private def csv(trial: TrialKey): Vector[(Int, Array[String])] =
    GoldenCsv.fixations.linesIterator.toVector
      .drop(1)
      .zipWithIndex
      .map((line, i) => (i + 1, line.split(",", -1)))
      .filter((_, r) =>
        r(0) == trial.participant && r(1) == trial.phase.label && r(2) == trial.trial
      )
      .sortBy((_, r) => r(4).toInt)

  /** The fixture README's window: the image frame, half-open. */
  private def insideWindow(x: Double, y: Double): Boolean =
    x >= 448 && x < 1472 && y >= 156 && y < 924

  test("P17 enc_03's fixations are fixations.csv's records, in ordinal order") {
    fake.flatMap(_.trialFixations(rev4, enc03)).map { result =>
      val view = result.fold(e => fail(e.message), identity)
      assertEquals(
        view.fixations.map(f => (f.record, f.screenX, f.screenY, f.onsetMs, f.durationMs)),
        csv(enc03)
          .map((n, r) => (n, r(5).toDouble, r(6).toDouble, r(7).toDouble, r(8).toDouble))
      )
    }
  }

  test("P17 enc_03's preview equals eyes4s's smoother on its in-window records") {
    fake.flatMap(_.trialPreview(rev4, enc03)).map { result =>
      val preview = result.fold(e => fail(e.message), identity)
      // The recipe's window and grid, 35 px/° (σ 2° = 70 px), duration weights.
      val (grid, sigma, window) = (for
        screen <- Frame.screen("screen", 1920, 1080)
        region <- Bounds.of[Unit2D.Px](448.0, 156.0, 1472.0, 924.0)
        window <- Subframe.of(screen, FrameId("window"), region)
        grid   <- Grid.of(GridId("preview"), window.frame, 64, 48)
        sigma  <- Sigma.px(70.0)
      yield (grid, sigma, window)).fold(e => fail(e.message), identity)
      val inside = csv(enc03).collect {
        case (_, r) if insideWindow(r(5).toDouble, r(6).toDouble) =>
          (Pt[Unit2D.Px](r(5).toDouble - 448.0, r(6).toDouble - 156.0), r(8).toDouble)
      }
      assertEquals(inside.size, 12)
      val measure = PointMeasure
        .of(window.frame, IArray.from(inside.map(_._1)), IArray.from(inside.map(_._2)))
        .fold(e => fail(e.toString), identity)
      val expected = Smoother
        .gaussian(sigma, EdgePolicy.Truncate)
        .density(measure, grid)
        .fold(e => fail(e.message), identity)
      val cells = preview.cells.map(_.getOrElse(fail("a cell without a value")))
      assertEquals(cells.size, expected.values.length)
      cells.zip(expected.values.toVector).zipWithIndex.foreach { case ((a, b), i) =>
        assertEqualsDouble(a, b, CellTolerance, s"cell $i")
      }
      val levels =
        MassLevels.of(expected, Vector(0.5, 0.8)).fold(e => fail(e.message), identity)
      // The native occupancy path uses seconds; this independent CSV oracle
      // uses milliseconds. Normalized masses agree to the same cell tolerance.
      assertEquals(preview.levels.size, levels.size)
      preview.levels.zip(levels).foreach { (actual, expected) =>
        assertEqualsDouble(
          actual,
          expected.threshold,
          CellTolerance,
          s"coverage ${expected.coverage}"
        )
      }
    }
  }
