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

import eyes4s.core.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

class PublicPlanSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)

  test("diagnostic helpers preserve operands and prepend contextual subjects") {
    val original = Diagnostic.of(TimeError.ClockMismatch(ClockId("left-X"), ClockId("right-X")))
    val contextual =
      original.within(Locus.Field("timestamp-X")).withSeverity(DiagnosticSeverity.Warning)
    assertEquals(contextual.subject, Vector(Locus.Field("timestamp-X")) ++ original.subject)
    assertEquals(contextual.operands, original.operands)
    assertEquals(contextual.severity, DiagnosticSeverity.Warning)
    DiagnosticCatalog.families.foreach { family =>
      assertEquals(family.label(-1), None)
      assertEquals(family.label(family.labels.size), None)
      assertEquals(family.labels.indices.map(family.label).toVector, family.labels.map(Some(_)))
      assert(family.toString.contains(family.name))
    }
    val ExactDouble(value) = ExactDouble(-0.0)
    assertEquals(
      java.lang.Double.doubleToLongBits(value),
      java.lang.Double.doubleToLongBits(-0.0)
    )
    val error = EpochError.DurationOverflow(
      "trial-X",
      get(Window.lasting(Span.micros(1))),
      BigInt(Long.MaxValue) + 1
    )
    assert(error.message.contains("trial-X") && error.message.contains("9223372036854775808"))
  }

  test(
    "recording channels retain sample counts and the public stepwise summoner returns its instance"
  ) {
    val frame     = get(Frame.screen("channel-count", 2, 2))
    val recording = get(
      Recording.of(
        frame,
        ClockId("channel-clock"),
        Rate.Irregular,
        Eye.Left,
        None,
        IArray(Sample(Instant.epoch, Gaze.Tracked(Pt[Px](1, 1), None)))
      )
    )
    assertEquals(RecordingChannels.Monocular(recording).size, 1)
    given instance: Stepwise[Int, String, Nothing, Int] with
      def stage(cursor: Int): String = s"cursor-$cursor"
      def advance(
          cursor: Int,
          quanta: eyes4s.design.WorkQuanta
      ): Either[Nothing, WorkStep[String, Int, Int]] =
        Right(WorkStep.Done(1, cursor))
    val selected = Stepwise[Int, String, Nothing, Int]
    assert(selected eq instance)
    assertEquals(selected.stage(17), "cursor-17")
    assertEquals(
      selected.advance(17, eyes4s.design.WorkQuanta.default),
      Right(WorkStep.Done(1, 17))
    )
  }

  test("a bound parameter field returns the selected domain value as well as its encoding") {
    val info = get(
      ParameterInfo.of(
        "name-X",
        1,
        "Selected name",
        FieldKind.Text
      )
    )
    val descriptor = new ParameterDescriptor[String, String, String](
      info,
      value => if value.nonEmpty then Right(value) else Left("empty name"),
      identity
    )
    val field = descriptor.bind[(Int, String)](_._2)(Provenance.Param.Text.apply)
    assertEquals(field.value((19, "selected-X")).toString, "selected-X")
    assertEquals(field.encoded((19, "selected-X")), Provenance.Param.Text("selected-X"))
    assert(descriptor.parse("").left.exists(_.message.contains("name-X")))
  }
