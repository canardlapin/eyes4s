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
import eyes4s.kernel.*
import eyes4s.plan.*
import org.scalacheck.Gen

private[laws] object EpochCodecFixture:
  val markerSchema: DefinitionId = DefinitionId.of("example.marker", 1).toOption.get
  val codec: VersionedCodec[EpochPlan[String]] =
    EpochCodecs.of(VersionedCodec.string(markerSchema))

class EpochCodecLawSuite extends munit.DisciplineSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val indices     = Gen.oneOf(0L, 1L, 9007199254740993L, Long.MaxValue)
  private val occurrences = Gen.frequency(
    1 -> Gen.const(Occurrence.RequireUnique),
    1 -> Gen.const(Occurrence.First),
    1 -> Gen.const(Occurrence.Last),
    2 -> indices.map(n => Occurrence.Nth(get(NonNegativeLong.of(n))))
  )
  private val cases = for
    key      <- Gen.oneOf("", "start", "message λ", "emoji 😀")
    selected <- occurrences
    from     <- Gen.oneOf(-9007199254740993L, -10L, 0L, 9007199254740993L)
    length   <- Gen.oneOf(0L, 1L, 7L, 100L)
    width    <- Gen.oneOf(1L, 3L, 9007199254740993L, Long.MaxValue)
    finalBin <- Gen.oneOf(FinalBin.values.toVector)
  yield get(
    EpochPlan.of(
      MarkSelector(key, selected),
      get(Window.of(Span.micros(from), Span.micros(from + length))),
      get(PositiveSpan.of(Span.micros(width))),
      finalBin
    )
  )

  checkAll(
    "epoch plan",
    CodecLaws.roundTrip(
      EpochCodecFixture.codec,
      cases,
      (a, b) =>
        a.anchor == b.anchor && a.window == b.window && a.binWidth == b.binWidth && a.finalBin == b.finalBin
    )
  )
