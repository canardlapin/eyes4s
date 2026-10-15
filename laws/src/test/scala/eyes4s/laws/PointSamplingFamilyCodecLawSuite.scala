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
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import eyes4s.surface.*
import org.scalacheck.Gen

private[laws] object PointFamilyCodecFixture:
  private def get[E, A](v: Either[E, A]): A = v.toOption.get
  private def id(name: String)              = get(DefinitionId.of(name, 1))
  val layout: StudyLayout[StudyKey]         = StudyKey.layout(id("example.point-layout"))
  val keys: VersionedCodec[StudyKey]        = StudyCodecs.key(id("example.point-key"))
  val codec: PointSamplingFamilyCodec[StudyKey, Px] = new PointSamplingFamilyCodec(layout, keys)
  val clock: ClockId                                = ClockId("point-law-clock")
  private val frame                                 = get(Frame.screen("point-law-frame", 2, 1))
  private val grid                                  = get(Grid.over(frame, 2, 1))
  private val path                                  = get(
    Scanpath.of(
      frame,
      clock,
      IArray(
        get(
          Event.Fixation.withoutDispersion(
            get(Interval.of(clock, Instant.micros(0), Instant.micros(10))),
            Pt[Px](0.5, 0.5),
            1
          )
        )
      )
    )
  )
  val source: StudyInput[StudyKey, Px] = StudyInput(
    Trials(Vector("a" -> "one", "a" -> "two", "b" -> "one").map { (stimulus, phase) =>
      Trial(StudyKey("person λ", stimulus, phase), (), path)
    })
  )
  val templates: Trials[StudyKey, Unit, Signed[Px]] = Trials(
    Vector("a" -> Vector(1.0, 2.0), "b" -> Vector(3.0, 1.0)).map { (stimulus, cells) =>
      val values = IArray.from(cells)
      Trial(
        StudyKey("person λ", stimulus, "template"),
        (),
        get(Surface.signed(grid, values, Provenance.raw(ContentHash.of(values))))
      )
    }
  )

class PointSamplingFamilyCodecLawSuite extends munit.DisciplineSuite:
  import PointFamilyCodecFixture.*
  private def get[E, A](v: Either[E, A]): A        = v.fold(e => fail(e.toString), identity)
  private val controls: Gen[PointControlSelection] = Gen.oneOf(
    PointControlSelection.Disabled,
    PointControlSelection.Candidates(Selection.All),
    PointControlSelection.Candidates(
      Selection.BottomK(get(PairLimit.of(1)), Seed(Long.MinValue), SampleId("law λ"))
    )
  )
  private val plans = for
    normalization <- Gen.oneOf(DensityNormalization.values.toVector)
    lookup        <- Gen.oneOf(DensityLookupPolicy.values.toVector)
    endpoint      <- Gen.oneOf(TrajectoryEndpoint.values.toVector)
    queries       <- Gen.oneOf(
      Vector.empty[Long],
      Vector(0L, 0L, 5L, 10L),
      Vector(Long.MinValue, 9007199254740993L, Long.MaxValue)
    )
    finalEndpoint <- Gen.oneOf(PointBinEndpoint.values.toVector)
    withBins      <- Gen.oneOf(true, false)
    selection     <- controls
    policy        <- Gen.oneOf(FailurePolicy.RequireAll, get(FailurePolicy.successfulOnly(1)))
  yield
    val bins = Option.when(withBins)(
      get(PointBins.of(clock, Vector(0L, 5L, 10L).map(Instant.micros), finalEndpoint))
    )
    val spec = get(
      PointSamplingSpec.of(
        clock,
        queries.map(Instant.micros),
        bins,
        normalization,
        lookup,
        endpoint,
        selection,
        policy,
        policy
      )
    )
    PointSamplingPlan.of(layout, source, templates, spec)
  checkAll(
    "point-sampling plan",
    CodecLaws.roundTrip(
      codec.plans,
      plans,
      (a, b) =>
        a.description == b.description && a.inputHash == b.inputHash && a.planHash == b.planHash
    )
  )
  checkAll(
    "point-sampling result",
    CodecLaws.roundTrip(
      codec.results,
      plans.map(PointSamplingArchive.run),
      (a, b) =>
        a.plan.description == b.plan.description && a.result.rows.map(
          _.points.map(_.value)
        ) == b.result.rows.map(_.points.map(_.value))
    )
  )
