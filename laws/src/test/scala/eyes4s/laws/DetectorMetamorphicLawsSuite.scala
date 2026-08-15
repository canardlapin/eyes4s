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

import eyes4s.core.Event
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Deg

class DetectorMetamorphicLawsSuite extends munit.DisciplineSuite:

  checkAll(
    "I-DT.metamorphic",
    DetectorMetamorphicLaws.idt(Detectors.idt[Deg])
  )

  test("the fixed JVM and JavaScript fixture has the same expected event summary") {
    val extent  = Extent.square[Deg](1.0).toOption.get
    val minimum = MinimumEventDuration.of(Span.millis(5)).toOption.get
    val events  = Detectors
      .idt(extent, minimum, ClockId("metamorphic-tracker"))
      .runAll(DetectorMetamorphicLaws.referenceInput)
      .map {
        case Right(fixation: Event.Fixation[Deg]) => fixation
        case Right(other)                         => fail(s"unexpected event: $other")
        case Left(error)                          => fail(error.message)
      }

    assertEquals(events.length, 2)
    assertEquals(
      events.map(event => event.span.onset.toMicros -> event.span.offset.toMicros),
      Vector(0L -> 12000L, 17000L -> 30000L)
    )
    assertEqualsDouble(events.head.centre.x, -0.05, 1e-12)
    assertEqualsDouble(events.last.centre.x, 6.046153846153846, 1e-12)
  }

end DetectorMetamorphicLawsSuite
