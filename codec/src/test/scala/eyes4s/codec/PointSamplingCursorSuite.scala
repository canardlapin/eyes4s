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

import eyes4s.compare.ComparisonQuantum
import eyes4s.design.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import eyes4s.pointcodecconsumer.PointSamplingFixture
import io.circe.Json
import scala.compiletime.testing.typeCheckErrors

class PointSamplingCursorSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private def id(value: String)                 = get(DefinitionId.of(value, 1))
  private val codec                             = new PointSamplingCodec[StudyKey, Px](
    id("example.point-plan"),
    id("example.point-result"),
    StudyKey.layout(id("example.point-layout")),
    StudyCodecs.key(id("example.point-key"))
  )
  private val saved             = get(io.circe.parser.parse(PointSamplingFixture.versionOne))
  private val expected          = get(saved.hcursor.downField("value").get[Json]("result"))
  private def plan              = get(codec.archive.decode(saved)).plan
  private def quantum(cap: Int) =
    WorkQuanta(get(PairQuantum.of(cap)), ComparisonQuantum.default)

  test("every quantum reproduces the independently pinned legacy result payload") {
    for cap <- Vector(1, 2, Int.MaxValue) do
      val p = plan; val q = quantum(cap); val initial = p.work
      var work = initial; var result: Option[PointSamplingResult[StudyKey, Px]] = None
      var preparationUnits = 0; var samplingUnits = 0; var steps = 0
      assertEquals(initial.stage, PointSamplingStage.PreparingWholeStudy)
      assertEquals(initial.processedTrials, 0)
      while result.isEmpty && steps <= p.source.trials.rows.size + 1 do
        val stage = work.stage; val before = work.processedTrials
        work.advance(q) match
          case WorkStep.More(worked, units, next) =>
            assertEquals(worked, stage)
            stage match
              case PointSamplingStage.PreparingWholeStudy =>
                assertEquals(units, 1); preparationUnits += units
                assertEquals(next.processedTrials, 0)
                assertEquals(next.stage, PointSamplingStage.SamplingWholeTrials)
              case PointSamplingStage.SamplingWholeTrials =>
                assert(units > 0 && units <= cap); samplingUnits += units
                assertEquals(next.processedTrials, before + units)
            assertEquals(work.processedTrials, before)
            assertEquals(next.totalTrials, initial.totalTrials)
            work = next
          case WorkStep.Done(units, found) =>
            assertEquals(stage, PointSamplingStage.SamplingWholeTrials)
            assert(units > 0 && units <= cap); samplingUnits += units
            assertEquals(before + units, initial.totalTrials)
            result = Some(found)
        steps += 1
      val completed = result.getOrElse(fail("work did not terminate"))
      assertEquals(preparationUnits, 1)
      assertEquals(samplingUnits, p.source.trials.rows.size)
      assertEquals(get(codec.resultJson(completed)), expected)
      assertEquals(initial.processedTrials, 0)
      assertEquals(get(codec.resultJson(initial.complete(q))), expected)
      assertEquals(get(codec.resultJson(get(Stepwise.complete(p.work, q)))), expected)
  }

  test("an Int.MaxValue trial quantum remains safe after partial sampling") {
    val p = plan; assert(p.source.trials.rows.size >= 3)
    p.work.advance(quantum(1)) match
      case WorkStep.More(PointSamplingStage.PreparingWholeStudy, 1, ready) =>
        ready.advance(quantum(1)) match
          case WorkStep.More(PointSamplingStage.SamplingWholeTrials, 1, partial) =>
            partial.advance(quantum(Int.MaxValue)) match
              case WorkStep.Done(units, result) =>
                assertEquals(units, p.source.trials.rows.size - 1)
                assertEquals(get(codec.resultJson(result)), expected)
              case other => fail(other.toString)
          case other => fail(other.toString)
      case other => fail(other.toString)
  }

  test("an external caller cannot manufacture the prepared state or cursor position") {
    assertEquals(
      typeCheckErrors(
        "type Work = eyes4s.plan.PointSamplingCursor[eyes4s.plan.StudyKey,eyes4s.kernel.Unit2D.Px]; val value: Option[Work] = None"
      ),
      Nil
    )
    val errors = typeCheckErrors(
      "new eyes4s.plan.PointSamplingCursor[eyes4s.plan.StudyKey,eyes4s.kernel.Unit2D.Px](null,None,0,Vector.empty)"
    )
    assert(errors.nonEmpty)
    assert(errors.exists(_.message.contains("PointSamplingCursor")))
  }
