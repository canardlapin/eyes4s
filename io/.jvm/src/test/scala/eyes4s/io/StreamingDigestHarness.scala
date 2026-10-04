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

package eyes4s.io

import eyes4s.codec.StudyResultCodecs
import eyes4s.compare.Similarity
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import eyes4s.surface.EdgePolicy

/** The child half of `StreamingDigestJvmSuite`: builds a large synthetic
  * study result and either digests it (`streamed`), which describes the
  * archive one row at a time, or encodes its whole archive (`whole`), the
  * first step of the digest before the archive was described. The suite runs
  * both in a JVM whose heap holds the result but not its encoded archive.
  *
  * Prints `EYES4S_DIGEST=<hex> values=<n>` and exits 0 when it finishes.
  */
object StreamingDigestHarness:
  val marker = "EYES4S_DIGEST="

  /** Cells per map, participants (each with two stimuli, recalled and
    * encoded) and Gaussian scales: 30,000 x 240 x 2 = 14.4 million doubles,
    * about 115 MB as arrays.
    */
  val columns      = 200
  val rows         = 150
  val participants = 60
  val sigmas       = Vector(2.0, 6.0)

  private def get[E, A](e: Either[E, A]): A =
    e.fold(x => throw new IllegalStateException(s"$x"), identity)

  def result: StudyResult[StudyKey, Px, Similarity, SignedDifference] =
    val frame  = get(Frame.screen("streaming-digest", columns, rows))
    val grid   = get(Grid.over(frame, columns, rows))
    val trials = for
      p     <- 0 until participants
      s     <- Vector("a", "b")
      phase <- Vector("recall", "encode")
    yield
      val key   = StudyKey(s"p$p", s, phase)
      val clock = ClockId(s"p$p/$s/$phase")
      val fixes = (0 until 4).map { i =>
        val x = (p * 37 + i * 53 + (if s == "a" then 11 else 97))        % columns + 0.5
        val y = (p * 17 + i * 29 + (if phase == "recall" then 3 else 5)) % rows + 0.5
        get(
          Event.Fixation.withoutDispersion(
            get(
              Interval.of(clock, Instant.micros(i * 2000L), Instant.micros(i * 2000L + 1000L))
            ),
            Pt[Px](x, y),
            1
          )
        )
      }
      Trial(key, (), get(Scanpath.of(frame, clock, IArray.from(fixes))))
    val input = StudyInput(Trials(trials.toVector))
    val plan  = get(
      StudyPlan.cosine[Px](
        input.reference,
        grid,
        "recall",
        "encode",
        Weight.Duration,
        sigmas.map(s => StudyEstimate.Gaussian(get(Sigma.px(s)), EdgePolicy.Truncate)),
        FailurePolicy.RequireAll
      )
    )
    get(plan.run(input))

  def main(arguments: Array[String]): Unit =
    val built   = result
    val values  = built.scales.map(_.estimation.map(_._2.fold(_ => 0, _.values.length)).sum).sum
    val results = StudyResultCodecs.cosine[Px]
    val hex     = arguments.headOption match
      case Some("streamed") => get(results.codec.digest(built)).sha256.hex
      case Some("whole")    =>
        val document = get(results.codec.encode(built))
        s"whole-${document.hashCode}"
      case other =>
        throw new IllegalArgumentException(s"expected streamed or whole, got $other")
    println(s"$marker$hex values=$values")
