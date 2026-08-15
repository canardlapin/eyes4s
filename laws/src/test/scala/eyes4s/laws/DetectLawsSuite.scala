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
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Deg

import org.scalacheck.Gen

class DetectLawsSuite extends munit.DisciplineSuite:

  private val frame  = Frame.angular("filter-laws", 100.0, 100.0).toOption.get
  private val width  = WindowHalfWidth.of(2).toOption.get
  private val policy = WindowObservationPolicy.RequireTracked
  private val seed   =
    (0 until 17).map { index =>
      Sample(
        Instant.millis(index.toLong),
        Gaze.Tracked(Pt[Deg](index.toDouble, 0.0), Some(index.toDouble))
      )
    }.toVector
  private val sampling = RegularSampling.from(seed).toOption.get

  private val regularSamples = Gen.listOfN(seed.length, Gen.choose(-40.0, 40.0)).map { xs =>
    xs.zipWithIndex.map { case (x, index) =>
      Sample(
        Instant.millis(index.toLong),
        Gaze.Tracked(Pt[Deg](x, 0.0), Some(index.toDouble))
      )
    }
  }

  checkAll(
    "Median.sampleSequence",
    SampleSequenceLaws.exactTimestamps(
      regularSamples,
      Filter.median(frame, width, policy)
    )
  )

  checkAll(
    "SavitzkyGolay.sampleSequence",
    SampleSequenceLaws.exactTimestamps(
      regularSamples,
      Filter.savitzkyGolay(frame, width, policy, sampling)
    )
  )

end DetectLawsSuite
