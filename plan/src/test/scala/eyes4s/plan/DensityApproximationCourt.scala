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

/** Opt-in public qualification producer. It executes the native study plan and
  * emits every selected map, pair identity and corrected score for an independent
  * external oracle. Default tests do not run this workload.
  */
object DensityApproximationCourt:
  private def get[E, A](value: Either[E, A]): A =
    value.fold(error => throw new IllegalStateException(error.toString), identity)

  private def quote(value: String): String =
    "\"" + value.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n") + "\""

  private def array(values: Iterable[String]): String = values.mkString("[", ",", "]")
  private def key(value: StudyKey): String            =
    s"${value.participant}/${value.stimulus}/${value.phase}"

  def main(arguments: Array[String]): Unit =
    val smoke = arguments.contains("--smoke")
    val frame = Frame.of[Px](
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
    val input  = StudyInput(Trials(trials))
    val byKey  = trials.map(trial => trial.key -> trial.value).toMap
    val sizes  = if smoke then Vector(8, 32) else Vector(8, 16, 32, 64, 128)
    val sigmas = if smoke then Vector(0.8) else Vector(0.2, 0.4, 0.8, 1.6)
    val phases =
      if smoke then Vector("near-midpoint") else Vector("matched", "near-midpoint", "mismatch")
    for
      nx    <- sizes
      sigma <- sigmas
      phase <- phases
    do
      val ny        = nx * 3 / 4
      val grid      = get(Grid.over(frame, nx, ny))
      val bandwidth = get(Sigma.px(sigma))
      val plan      = get(
        StudyPlan.of(
          input.reference,
          StudyKey.layout(DefinitionId.studyLayout),
          grid,
          phase,
          "encode",
          Weight.Duration,
          Vector(StudyEstimate.Gaussian(bandwidth, EdgePolicy.Truncate)),
          FailurePolicy.RequireAll,
          StudyMethod.cosine[Px](DefinitionId.cosine),
          ()
        )
      )
      val result = get(plan.run(input))
      val scale  = result.scales.head
      val maps   = scale.estimation.map { (identity, value) =>
        val mass    = get(value)
        val measure = get(byKey(identity).occupancy(Weight.Duration))
        val raw = get(Smoother.gaussian(bandwidth, EdgePolicy.Truncate).smooth(measure, grid))
        "{" + s"\"id\":${quote(key(identity))},\"input_total\":${measure.total},\"raw_total\":${raw.sum},\"cells\":" +
          array(mass.values.iterator.map(_.toString).toVector) + "}"
      }
      def pairs(design: StudyDesign): String =
        array(scale.analyses.source(design).rows.map { row =>
          "{" + s"\"left\":${quote(key(row.left))},\"right\":${quote(key(row.right))},\"value\":${get(row.result).value}" + "}"
        })
      val contrast = get(scale.contrast).rows.map { row =>
        val matched =
          row.matched.getOrElse(throw new IllegalStateException(s"missing matched=${row.key}"))
        val control =
          row.control.getOrElse(throw new IllegalStateException(s"missing controls=${row.key}"))
        "{" + s"\"id\":${quote(key(row.key))},\"matched\":${get(matched.result).value},\"control\":${get(control.result).value},\"difference\":${get(row.difference).value},\"controls\":${control.contributing}" + "}"
      }
      val xs = grid.centres.take(nx).iterator.map(_.x.toString).toVector
      val ys = (0 until ny).map(i =>
        get(grid.cellCentre(i * nx).toRight(s"missing row=$i")).y.toString
      )
      println(
        "DENSITY_COURT={" +
          s"\"source_sha256\":${quote(DensityApproximationInputs.inputSha256)},\"input\":${quote(input.reference.digest)},\"nx\":$nx,\"ny\":$ny,\"sigma\":$sigma,\"phase\":${quote(phase)}," +
          s"\"x\":${array(xs)},\"y\":${array(ys)},\"maps\":${array(maps)},\"matched_pairs\":${pairs(StudyDesign.Matched)},\"control_pairs\":${pairs(StudyDesign.Control)},\"contrast\":${array(contrast)},\"excluded\":" +
          array(scale.excludedPhases.map(value => quote(key(value)))) + "}"
      )
