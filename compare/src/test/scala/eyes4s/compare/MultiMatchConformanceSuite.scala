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

package eyes4s.compare

import eyes4s.compare.MultiMatchPythonFixtures.*
import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px

class MultiMatchConformanceSuite extends munit.FunSuite:

  /** The reference and eyes4s both use IEEE-754 Double operations. This allows
    * only accumulated rounding error, not algorithmic disagreement.
    */
  private val MultimatchGazeTolerance = 1e-12

  private val frame =
    Frame.screen("multimatch-python-fixture", screenWidth, screenHeight).toOption.get
  private val clock = ClockId("multimatch-python-fixture")

  private def scanpath(fixations: Vector[Fixation]): Scanpath[Px] =
    var onsetMicros = 0L
    val events      = fixations.map { fixation =>
      val durationMicros = math.round(fixation.durationSeconds * 1000000.0)
      val span           = Interval
        .of(
          clock,
          Instant.micros(onsetMicros),
          Instant.micros(onsetMicros + durationMicros)
        )
        .toOption
        .get
      onsetMicros += durationMicros + 10000L
      Event.Fixation[Px](span, Pt[Px](fixation.x, fixation.y), 1.0, 1)
    }
    Scanpath.of(frame, clock, IArray.from(events)).toOption.get

  cases.foreach { fixture =>
    test(s"MultiMatch agrees with multimatch_gaze for ${fixture.name}") {
      val actual = MultiMatch[Px]
        .compare(scanpath(fixture.left), scanpath(fixture.right))
        .toOption
        .get
      val expected = fixture.expected

      assertEqualsDouble(
        actual.shape,
        expected.shape,
        MultimatchGazeTolerance,
        clue(fixture.name)
      )
      assertEqualsDouble(
        actual.direction,
        expected.direction,
        MultimatchGazeTolerance,
        clue(fixture.name)
      )
      assertEqualsDouble(
        actual.length,
        expected.length,
        MultimatchGazeTolerance,
        clue(fixture.name)
      )
      assertEqualsDouble(
        actual.position,
        expected.position,
        MultimatchGazeTolerance,
        clue(fixture.name)
      )
      assertEqualsDouble(
        actual.duration,
        expected.duration,
        MultimatchGazeTolerance,
        clue(fixture.name)
      )
    }
  }

end MultiMatchConformanceSuite
