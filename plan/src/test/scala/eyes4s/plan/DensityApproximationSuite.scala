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

package eyes4s.plan

import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.surface.*

/** Independent point-kernel sums pin coarse grids and near-zero corrected scores. */
class DensityApproximationSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)
  private def key(value: StudyKey): String =
    s"${value.participant}/${value.stimulus}/${value.phase}"
  private val DirectSumTolerance = 2e-12
  val frame                      = Frame.of[Px](
    FrameId("public-density-crop"),
    get(Bounds.of[Px](-1.0, -1.0, 7.0, 5.0)),
    YAxis.Down
  )
  val trials = DensityApproximationInputs.items.map { item =>
    val studyKey = StudyKey(item.participant, item.stimulus, item.phase)
    val clock    = ClockId("density:" + key(studyKey))
    val fixes    = item.points.zipWithIndex.map { (point, index) =>
      val onset = index * 1000000L
      get(
        Event.Fixation.withoutDispersion(
          get(
            Interval
              .of(clock, Instant.micros(onset), Instant.micros(onset + item.durations(index)))
          ),
          Pt[Px](point._1, point._2),
          1
        )
      )
    }
    Trial(studyKey, (), get(Scanpath.of(frame, clock, IArray.from(fixes))))
  }
  val input = StudyInput(Trials(trials))

  DensityApproximationReference.rows.groupBy(row => (row.nx, row.sigma)).foreach {
    case ((nx, sigma), expected) =>
      test(s"duration-weighted native direct-sum oracle nx=$nx sigma=$sigma") {
        val grid = get(Grid.over(frame, nx, nx * 3 / 4))
        val plan = get(
          StudyPlan.of(
            input.reference,
            StudyKey.layout(DefinitionId.studyLayout),
            grid,
            "near-midpoint",
            "encode",
            Weight.Duration,
            Vector(StudyEstimate.Gaussian(get(Sigma.px(sigma)), EdgePolicy.Truncate)),
            FailurePolicy.RequireAll,
            StudyMethod.cosine[Px](DefinitionId.cosine),
            ()
          )
        )
        val scale = get(plan.run(input)).scales.head
        val rows  = get(scale.contrast).rows.map(row => key(row.key) -> row).toMap
        assertEquals(rows.keySet, expected.map(_.id).toSet)
        expected.foreach { oracle =>
          val row     = rows(oracle.id)
          val matched = row.matched.getOrElse(fail(s"missing matched=${oracle.id}"))
          val control = row.control.getOrElse(fail(s"missing control=${oracle.id}"))
          assertEquals(control.contributing, 2)
          assertEqualsDouble(
            get(matched.result).value,
            oracle.matched,
            DirectSumTolerance,
            oracle.id
          )
          assertEqualsDouble(
            get(control.result).value,
            oracle.control,
            DirectSumTolerance,
            oracle.id
          )
          assertEqualsDouble(
            get(row.difference).value,
            oracle.difference,
            DirectSumTolerance,
            oracle.id
          )
        }
      }
  }
