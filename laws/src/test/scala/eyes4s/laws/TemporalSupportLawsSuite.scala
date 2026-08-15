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

package eyes4s.laws

import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Deg

import org.scalacheck.Gen

import scala.compiletime.testing.typeCheckErrors

class TemporalSupportLawsSuite extends munit.DisciplineSuite:

  private val frame = Frame.angular("temporal-support-laws", 20.0, 20.0).toOption.get
  private val clock = ClockId("temporal-support-laws")

  private def sample(index: Int): Sample[Deg] =
    Sample(
      Instant.millis(index.toLong),
      Gaze.Tracked(Pt[Deg](index.toDouble / 2.0, 0.0), None)
    )

  private val fixed = Recording
    .of(
      frame,
      clock,
      Rate.Fixed(Hz(1000.0).toOption.get),
      Eye.Left,
      None,
      IArray.tabulate(8)(sample)
    )
    .toOption
    .get
    .occupancy
    .toOption
    .get

  private val irregularRecording = Recording
    .of(
      frame,
      clock,
      Rate.Irregular,
      Eye.Left,
      None,
      IArray(sample(0), sample(3), sample(10))
    )
    .toOption
    .get

  private val capped = irregularRecording
    .occupancy(
      TemporalSupport.Voronoi(
        MaximumSupportGap.atMost(Span.millis(4)).toOption.get,
        EdgeSupport.Censored
      )
    )
    .toOption
    .get

  private val held = irregularRecording
    .occupancy(
      TemporalSupport.ForwardHold(
        MaximumSupportGap.Unlimited,
        EdgeSupport.PreviousInterval
      )
    )
    .toOption
    .get

  checkAll(
    "Recording.temporalSupport",
    TemporalSupportLaws.accounting(
      Gen.oneOf(fixed, capped, held),
      Tolerance(absolute = 1e-12, relative = 1e-12)
    )
  )

  test("raw temporal-support scalars cannot bypass their smart constructors") {
    val errors = typeCheckErrors("""
      import eyes4s.core.*
      import eyes4s.kernel.*
      TemporalSupport.Fixed(Span.zero)
      MaximumSupportGap.AtMost(Span.millis(-1))
      EdgeSupport.Fixed(Span.millis(-1))
    """)
    assertEquals(errors.length, 3, clue(errors.map(_.message)))
  }

end TemporalSupportLawsSuite
