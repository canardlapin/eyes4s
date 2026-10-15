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

import eyes4s.codec.*
import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import org.scalacheck.Gen

private[laws] object ScanpathMethodLawFixture:
  val codec: VersionedCodec[ScanpathMethod[Px]] = new ScanpathMethodCodec[Px].codec

class ScanpathMethodCodecLawSuite extends munit.DisciplineSuite:
  private def get[E, A](value: Either[E, A]): A = value.toOption.get
  private val multiMatch                        = for
    minimum <- Gen.oneOf(MultiMatchCardinality.values.toVector)
    limit   <- Gen.oneOf(1L, 1234567L, 9007199254740993L, Long.MaxValue)
  yield ScanpathMethod.MultiMatch[Px](minimum, get(AlignmentCellLimit.of(limit)))
  private val overlap = for
    times <- Gen.nonEmptyListOf(
      Gen.oneOf(Long.MinValue, Long.MaxValue, 9007199254740993L, 0L, -1L)
    )
    threshold <- Gen.oneOf(0.0, 0.125, 3.5)
    distance  <- Gen.oneOf(FixationGroundDistance.values.toVector)
    endpoint  <- Gen.oneOf(TrajectoryEndpoint.values.toVector)
    missing   <- Gen.oneOf(MissingOverlapPolicy.values.toVector)
  yield ScanpathMethod.Overlap(
    get(
      FixationOverlapSpec.of(
        ClockId("generated queries"),
        times.toVector.map(Instant.micros),
        get(OverlapThreshold.of[Px](threshold)),
        distance,
        endpoint,
        missing
      )
    )
  )
  private val transport = for
    x      <- Gen.oneOf(0.125, 12.5)
    y      <- Gen.oneOf(0.25, 2.5)
    time   <- Gen.oneOf(1L, 9007199254740993L, Long.MaxValue)
    work   <- Gen.oneOf(1L, 9007199254740993L, Long.MaxValue)
    lambda <- Gen.oneOf(0.125, 1.0, 2.5)
    weight <- Gen.oneOf(0.0, 0.25, 1.0)
  yield ScanpathMethod.Transport(
    get(
      FixationTransportConfig
        .of[Px](x, y, Span.micros(time), weight, lambda, 321, 1e-8, 1234, work)
    )
  )
  private val methods: Gen[ScanpathMethod[Px]] = Gen.oneOf(multiMatch, overlap, transport)
  private def same(left: ScanpathMethod[Px], right: ScanpathMethod[Px]): Boolean =
    left.id == right.id && left.parameters == right.parameters && left.parameterHash == right.parameterHash && left.properties == right.properties
  checkAll(
    "scanpath method",
    CodecLaws.roundTrip(ScanpathMethodLawFixture.codec, methods, same)
  )
