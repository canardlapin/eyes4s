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

package eyes4s.codec

import eyes4s.kernel.Provenance
import eyes4s.kernel.Unit2D.Px

/** Run explicitly; timings are observations, never a flaky correctness threshold. */
object DensityPrefixBenchmark:
  private def get[E, A](value: Either[E, A]): A =
    value.fold(e => throw new AssertionError(e.toString), identity)

  def main(args: Array[String]): Unit =
    val result = get(
      StudyResultCodecs.cosine[Px].codec.parse(StudyResultFixtures.resultVersionOne)
    )
    val mass = get(result.scales.head.estimation.head._2)
    // The G2 cohort has 12,213 queries. Each explicit expected-unmatched
    // assignment contributes several typed context parameters.
    val context = result.description :+ ("explicit-selection" -> Vector.tabulate(12213)(i =>
      Provenance.Param.Text(s"participant/phase/trial/occurrence/$i")
    ))
    val maps                           = 300
    def timed[A](f: => A): (A, Double) =
      val start = System.nanoTime()
      val value = f
      (value, (System.nanoTime() - start) / 1e9)
    (1 to 3).foreach { _ =>
      get(DensityDigest.of(mass, context))
      get(DensityDigest.continued(get(DensityDigest.prepare(mass.grid, context)), mass))
    }
    val (expected, fullSeconds) = timed(
      Vector.fill(maps)(get(DensityDigest.of(mass, context)).sha256)
    )
    val (actual, prefixSeconds) = timed {
      val prefix = get(DensityDigest.prepare(mass.grid, context))
      Vector.fill(maps)(get(DensityDigest.continued(prefix, mass)))
    }
    assert(actual == expected, "Every map must retain its full canonical digest")
    println(
      io.circe.Json
        .obj(
          "schema"              -> io.circe.Json.fromInt(1),
          "maps"                -> io.circe.Json.fromInt(maps),
          "explicitParameters"  -> io.circe.Json.fromInt(12213),
          "cellsPerMap"         -> io.circe.Json.fromInt(mass.values.length),
          "fullSeconds"         -> io.circe.Json.fromDoubleOrNull(fullSeconds),
          "sharedPrefixSeconds" -> io.circe.Json.fromDoubleOrNull(prefixSeconds),
          "allDigestsIdentical" -> io.circe.Json.True,
          "digest"              -> io.circe.Json.fromString(actual.head.hex)
        )
        .noSpaces
    )
