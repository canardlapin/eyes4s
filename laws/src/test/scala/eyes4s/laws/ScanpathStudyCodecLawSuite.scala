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
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import org.scalacheck.Gen

private[laws] object ScanpathStudyLawFixture:
  private def get[E, A](value: Either[E, A]): A = value.toOption.get
  val frame  = get(Frame.screen("scanpath-study-laws", 100, 100))
  val clock  = ClockId("scanpath-study-laws")
  val layout = StudyKey.layout(DefinitionId.studyLayout)
  val codec  =
    new ScanpathStudyCodec[StudyKey, Px](layout, StudyCodecs.key(DefinitionId.studyKey))
  private val multiMatch: Gen[ScanpathMethod[Px]] = for
    minimum <- Gen.oneOf(MultiMatchCardinality.values.toVector)
    limit   <- Gen.oneOf(1L, 100L, 9007199254740993L, Long.MaxValue)
  yield ScanpathMethod.MultiMatch[Px](minimum, get(AlignmentCellLimit.of(limit)))
  private val overlap: Gen[ScanpathMethod[Px]] = for
    times <- Gen.nonEmptyListOf(
      Gen.oneOf(Long.MinValue, Long.MaxValue, 0L, 1000L, 9007199254740993L)
    )
    threshold <- Gen.oneOf(0.0, 0.5, 3.5)
    distance  <- Gen.oneOf(FixationGroundDistance.values.toVector)
    endpoint  <- Gen.oneOf(TrajectoryEndpoint.values.toVector)
    missing   <- Gen.oneOf(MissingOverlapPolicy.values.toVector)
  yield ScanpathMethod.Overlap(
    get(
      FixationOverlapSpec.of(
        clock,
        times.toVector.map(Instant.micros),
        get(OverlapThreshold.of[Px](threshold)),
        distance,
        endpoint,
        missing
      )
    )
  )
  private val transport: Gen[ScanpathMethod[Px]] = for
    time       <- Gen.oneOf(1000L, 9007199254740993L, Long.MaxValue)
    work       <- Gen.oneOf(1L, 1000000L, Long.MaxValue)
    iterations <- Gen.oneOf(1, 10000)
    lambda     <- Gen.oneOf(0.125, 0.5, 2.0)
    tolerance  <- Gen.oneOf(1e-8, 1e-12)
    weight     <- Gen.oneOf(0.0, 0.25, 1.0)
  yield ScanpathMethod.Transport(
    get(
      FixationTransportConfig
        .of[Px](20, 20, Span.micros(time), weight, lambda, iterations, tolerance, 100, work)
    )
  )
  val plans: Gen[ScanpathStudyPlan[StudyKey, Px]] = for
    method     <- Gen.oneOf(multiMatch, overlap, transport)
    fixations  <- Gen.choose(1, 3)
    shift      <- Gen.choose(1, 20)
    duration   <- Gen.oneOf(1L, 500L, 999L)
    duplicate  <- Gen.oneOf(false, true)
    allMatches <- Gen.oneOf(false, true)
    successful <- Gen.oneOf(false, true)
    minimum    <- Gen.choose(1, 2)
    sample     <- Gen.oneOf(false, true)
    seed       <- Gen.oneOf(Long.MinValue, Long.MaxValue, 9007199254740993L, 0L)
    candidates <- Gen.oneOf(64L, 9007199254740993L, Long.MaxValue)
    selected   <- Gen.oneOf(16, Int.MaxValue)
  yield
    def path(offset: Int) = get(
      Scanpath.of(
        frame,
        clock,
        IArray.from((0 until fixations).map { index =>
          get(
            Event.Fixation.withoutDispersion(
              get(
                Interval.of(
                  clock,
                  Instant.micros(index * 1000L),
                  Instant.micros(index * 1000L + duration)
                )
              ),
              Pt[Px](offset + index * 2.0 + 1, (index % 2) * 3.0 + 1),
              1
            )
          )
        })
      )
    )
    val base = Vector("a", "b").zipWithIndex.flatMap { (item, index) =>
      Vector("focal", "reference").map { phase =>
        Trial(
          StudyKey("p", item, phase),
          (),
          path(index * 20 + (if phase == "focal" then shift else 0))
        )
      }
    }
    val rows     = if duplicate then base :+ base.head else base
    val controls = if sample then
      Selection.BottomK(get(PairLimit.of(1)), Seed(seed), SampleId("λ"))
    else Selection.All
    get(
      ScanpathStudyPlan.of(
        layout,
        StudyInput(Trials(rows)),
        frame,
        "focal",
        "reference",
        method,
        if allMatches then StudyPairing.version1 else StudyPairing.default,
        controls,
        if successful then get(FailurePolicy.successfulOnly(minimum))
        else FailurePolicy.RequireAll,
        get(PairScheduleBudget.of(100, candidates, selected))
      )
    )

class ScanpathStudyCodecLawSuite extends munit.DisciplineSuite:
  import ScanpathStudyLawFixture.*
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  checkAll(
    "scanpath study plan",
    CodecLaws.roundTrip(
      codec.plans,
      plans,
      (a, b) =>
        a.description == b.description && a.inputHash == b.inputHash && a.planHash == b.planHash
    )
  )
  checkAll(
    "scanpath study result",
    CodecLaws.roundTrip(
      codec.results,
      plans.map(plan => get(ScanpathStudyArchive.run(plan))),
      (a, b) =>
        a.plan.description == b.plan.description &&
          codec.resultJson(a.result) == codec.resultJson(b.result)
    )
  )
