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

import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import scala.compiletime.testing.typeCheckErrors

class RunStampSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val inputs                            = StudyInputCodecs.study[Px]
  private val studies                           = StudyCodecs.cosine[Px]
  private val input                             = StudyResultFixtures.clean
  private val plan = StudyResultFixtures.cosinePlan(input, FailurePolicy.RequireAll)

  test("full canonical documents round-trip with the same stamp") {
    val stamp        = get(RunStamp.of(plan, input, studies.codec, inputs.input))
    val decodedPlan  = get(studies.codec.decode(get(studies.codec.encode(plan))))
    val decodedInput = get(inputs.input.decode(get(inputs.input.encode(input))))
    val restored     = get(RunStamp.of(decodedPlan, decodedInput, studies.codec, inputs.input))
    assertEquals(stamp.check(restored, Vector.empty), Right(()))
    assertEquals(stamp.plan.display, get(studies.codec.digest(plan)).display)
    assertEquals(stamp.input.display, get(inputs.input.digest(input)).display)
  }

  test("same-description extension plans still have distinct canonical stamps") {
    import StudyResultFixtures.{TwoComponent, TwoDifference}
    val original = StudyResultFixtures.twoComponentMethod
    val method   = new StudyMethod[Double, Px, TwoComponent, TwoDifference](
      original.id,
      original.name,
      _ => Vector.empty,
      original.execution,
      original.descriptor
    )(using original.mean, original.difference)
    val plans = new StudyCodec(
      StudyResultFixtures.twoComponentStudy.schema,
      StudyKey.layout(DefinitionId.studyLayout),
      studies.keys,
      method,
      StudyResultFixtures.twoComponentStudy.parameters
    )
    def make(gain: Double) = get(
      StudyPlan.of(
        input.reference,
        plans.layout,
        StudyResultFixtures.gridOver(input),
        "recall",
        "encode",
        Weight.Duration,
        Vector(StudyEstimate.Binned[Px]()),
        FailurePolicy.RequireAll,
        method,
        gain
      )
    )
    val first  = make(1.0)
    val second = make(2.0)
    assertEquals(first.description, second.description)
    assertEquals(first.diff(second), Vector.empty)
    val saved   = get(RunStamp.of(first, input, plans.codec, inputs.input))
    val current = get(RunStamp.of(second, input, plans.codec, inputs.input))
    saved.check(current, first.diff(second)) match
      case Left(RunStampError.ChangedPlan(reported, actual, changes)) =>
        assert(reported.sameAs(saved.plan))
        assert(actual.sameAs(current.plan))
        assertEquals(changes, Vector.empty)
      case other => fail(s"accepted changed full plan: $other")
  }

  test("input dispersion evidence changes the stamp even when the legacy identity agrees") {
    val frame = get(Frame.screen("stamp-evidence", 10, 10))
    val clock = ClockId("stamp-evidence")
    val span  = get(Interval.of(clock, Instant.micros(0), Instant.micros(1000)))
    def evidence(dispersion: Double) =
      val fixation =
        get(Event.Fixation.of(span, Pt[Px](5, 5), dispersion, DispersionMethod.RmsRadius, 2))
      StudyInput(
        Trials(
          Vector(
            Trial(
              StudyKey("p", "a", "recall"),
              (),
              get(Scanpath.of(frame, clock, IArray(fixation)))
            )
          )
        )
      )
    val first  = evidence(0.1)
    val second = evidence(0.2)
    assertEquals(first.reference, second.reference)
    val p       = StudyResultFixtures.cosinePlan(first, FailurePolicy.RequireAll)
    val saved   = get(RunStamp.of(p, first, studies.codec, inputs.input))
    val current = get(RunStamp.of(p, second, studies.codec, inputs.input))
    saved.check(current, Vector.empty) match
      case Left(error @ RunStampError.ChangedInput(reported, actual)) =>
        assert(reported.sameAs(saved.input))
        assert(actual.sameAs(current.input))
        assert(error.message.contains(reported.display))
        assert(error.message.contains(actual.display))
      case other => fail(s"accepted changed evidence: $other")
  }

  test("plan refusal retains the human-readable changes and both canonical operands") {
    val changed = StudyResultFixtures.cosinePlan(
      input,
      FailurePolicy.RequireAll,
      Vector(StudyEstimate.Binned[Px]())
    )
    val saved   = get(RunStamp.of(plan, input, studies.codec, inputs.input))
    val current = get(RunStamp.of(changed, input, studies.codec, inputs.input))
    saved.check(current, plan.diff(changed)) match
      case Left(error @ RunStampError.ChangedPlan(reported, actual, changes)) =>
        assertEquals(changes, plan.diff(changed))
        assert(changes.nonEmpty)
        assert(error.message.contains(reported.display))
        assert(error.message.contains(actual.display))
      case other => fail(s"unexpected $other")
  }

  test("unsupported canonical documents are refused rather than assigned an identity") {
    val schema      = get(DefinitionId.of("test.stamp-json", 1))
    val codec       = VersionedCodec.of[io.circe.Json](schema)(identity)(Right(_))
    val unsupported = io.circe.Json.fromBigInt(BigInt(2).pow(64))
    assert(RunStamp.of(unsupported, io.circe.Json.Null, codec, codec).isLeft)
    assert(RunStamp.of(io.circe.Json.Null, unsupported, codec, codec).isLeft)
  }

  test("plan and input digest substitutions are rejected at compile time") {
    assert(typeCheckErrors("""
      import eyes4s.codec.*
      val stamp: RunStamp[String, Int] = ???
      val input: CanonicalDigest[Int] = ???
      stamp.plan.sameAs(input)
    """).nonEmpty)
    assert(typeCheckErrors("""
      import eyes4s.codec.*
      val reported: RunStamp[String, Int] = ???
      val reversed: RunStamp[Int, String] = ???
      reported.check(reversed, Vector.empty)
    """).nonEmpty)
  }
