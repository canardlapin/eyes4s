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

package consumer

import eyes4s.laws.*
import eyes4s.laws.Generators.given
import eyes4s.codec.*
import eyes4s.core.*
import eyes4s.detect.SampleClass
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Deg, Norm, Px}
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.rng.Seed
import io.circe.Json

class PublicLawsSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private def draw[A](gen: Gen[A]): Vector[A]   = Vector.tabulate(30) { i =>
    gen(Gen.Parameters.default.withSize(10), Seed(317L + i))
      .getOrElse(fail("published generator discarded every candidate"))
  }

  test(
    "published arbitrary instances produce checked domains with deterministic diverse evidence"
  ) {
    val intervals = draw(summon[Arbitrary[Interval]].arbitrary)
    assert(intervals.forall(i => i.offset.toMicros >= i.onset.toMicros))
    assert(intervals.distinct.size > 1)
    assert(draw(summon[Arbitrary[Window]].arbitrary).forall(_.width.toMicros >= 0))
    assert(draw(summon[Arbitrary[ClockId]].arbitrary).forall(_.name.nonEmpty))
    assertEquals(draw(summon[Arbitrary[Overlap]].arbitrary).toSet, Overlap.values.toSet)
    assert(
      draw(summon[Arbitrary[Bounds[Norm]]].arbitrary).forall(b => b.width > 0 && b.height > 0)
    )
    assert(draw(summon[Arbitrary[Perspective]].arbitrary).forall(_.distance.toMm > 0))
    assert(draw(summon[Arbitrary[Scanpath[Norm]]].arbitrary).forall(_.n > 0))
    val frame = get(Frame.screen("generator-consumer", 12, 8))
    assert(
      draw(Generators.genGrid(frame)).forall(g => g.frame == frame && g.nx >= 2 && g.ny >= 2)
    )
  }

  test(
    "the public decoder decorator forwards every family and counts each refused document once"
  ) {
    val base     = get(ArtifactDecoders.study[Px])
    val counted  = ManifestLaws.counting(base)
    val refusals = Vector(
      counted.plan(Json.Null).isLeft,
      counted.input(Json.Null).isLeft,
      counted.ledger(Json.Null).isLeft,
      counted.result(Json.Null).isLeft,
      counted.recording(Json.Null, _ => None).isLeft,
      counted.recordingInput(Json.Null).isLeft,
      counted.temporalInput(Json.Null, _ => None).isLeft,
      counted.recordingPlan(Json.Null).isLeft,
      counted.recordingResult(Json.Null).isLeft,
      counted.temporalPlan(Json.Null).isLeft,
      counted.temporalResult(Json.Null).isLeft
    )
    assert(refusals.forall(identity)); assertEquals(counted.calls, 11)
  }

  test("validation observations preserve event class timing and equality under shifting") {
    val span =
      get(Interval.lasting(ClockId("validation-consumer"), Instant.epoch, Span.millis(20)))
    val events: Vector[Event[Deg]] = Vector(
      get(Event.Fixation.of(span, Pt[Deg](1, 2), 0.0, DispersionMethod.RmsRadius, 1)),
      get(Event.Saccade.of(span, Pt[Deg](1, 2), Pt[Deg](3, 4), None)),
      get(Event.Blink.of[Deg](span)),
      get(Event.Pursuit.of(span, IArray(Pt[Deg](1, 2), Pt[Deg](3, 4))))
    )
    val projected = events.map(ValidationEvent.fromEvent)
    assertEquals(
      projected.map(_.label),
      Vector(SampleClass.Fixation, SampleClass.Saccade, SampleClass.Blink, SampleClass.Pursuit)
    )
    projected.zip(events).foreach { (value, event) =>
      assertEquals(value.hashCode, ValidationEvent.fromEvent(event).hashCode)
      assertEquals(value.shift(Span.millis(5)).span.onset, Instant.millis(5))
    }
    val messages = Vector(
      DetectorValidationError.LabelCountMismatch(17, 19).message -> Vector(
        "referenceCount=17",
        "predictedCount=19"
      ),
      SyntheticGenerationError.InvalidClockScale(-17.25).message -> Vector("scale=-17.25"),
      ValidationArtifactError
        .InvalidBuildField("revision-X", "bad-X", "40 hexadecimal digits")
        .message -> Vector("revision-X", "bad-X", "40 hexadecimal digits")
    )
    messages.foreach { (message, operands) =>
      operands.foreach(o => assert(message.contains(o), message))
    }
  }
