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
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import org.scalacheck.Gen

class StudyCountsLawSuite extends munit.DisciplineSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(e => fail(s"$e"), identity)
  private val frame                         = get(Frame.screen("count-laws", 2, 2))
  private val grid                          = get(Grid.over(frame, 2, 2))
  private def trial(key: StudyKey)          =
    val clock    = ClockId(key.toString)
    val fixation = get(
      Event.Fixation.withoutDispersion(
        get(Interval.of(clock, Instant.micros(0L), Instant.micros(1000L))),
        Pt[Px](0.5, 0.5),
        1
      )
    )
    Trial(key, (), get(Scanpath.of(frame, clock, IArray(fixation))))

  private val cases = for
    n      <- Gen.chooseNum(1, 4)
    m      <- Gen.chooseNum(1, 4)
    scales <- Gen.chooseNum(1, 3)
  yield
    val focal = Vector.tabulate(n)(i => StudyKey("p", i.toString, "recall"))
    val refs  = Vector.tabulate(m)(i => StudyKey("p", i.toString, "encode"))
    val input = StudyInput(Trials((focal ++ refs).map(trial)))
    val plan  = get(
      StudyPlan.of(
        input.reference,
        StudyKey.layout(DefinitionId.studyLayout),
        grid,
        "recall",
        "encode",
        Weight.Duration,
        Vector.tabulate(scales)(i =>
          StudyEstimate.Gaussian(get(Sigma.px(i + 1.0)), eyes4s.surface.EdgePolicy.Truncate)
        ),
        FailurePolicy.RequireAll,
        StudyMethod.cosine[Px](DefinitionId.cosine),
        ()
      )
    )
    val matched = math.min(n, m).toLong
    StudyCountsLaws.Case(
      get(CountCursor.of(plan, get(plan.prepare(input)))),
      matched,
      n.toLong * m - matched,
      n.toLong,
      n.toLong + m,
      scales
    )

  checkAll("preview", StudyCountsLaws.counts(cases))
