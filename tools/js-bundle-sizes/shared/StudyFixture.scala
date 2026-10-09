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

package probe

import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

object StudyFixture:
  def checked[E, A](value: Either[E, A]): A =
    value.fold(error => throw new IllegalArgumentException(error.toString), identity)
  val frame                        = checked(Frame.screen("bundle-probe", 2, 2))
  val grid                         = checked(Grid.over(frame, 2, 2))
  private def trial(phase: String) =
    val clock    = ClockId("bundle-probe-" + phase)
    val interval = checked(Interval.of(clock, Instant.micros(0), Instant.micros(1000)))
    val fixation = checked(Event.Fixation.withoutDispersion(interval, Pt[Px](0.5, 0.5), 1))
    Trial(StudyKey("p1", "a", phase), (), checked(Scanpath.of(frame, clock, IArray(fixation))))
  val input = StudyInput(Trials(Vector(trial("encode"), trial("recall"))))
  val plan  = checked(
    StudyPlan.cosine[Px](
      input.reference,
      grid,
      "recall",
      "encode",
      Weight.Duration,
      Vector(StudyEstimate.Binned[Px]()),
      FailurePolicy.RequireAll
    )
  )
