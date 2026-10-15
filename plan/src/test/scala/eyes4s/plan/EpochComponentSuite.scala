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

package eyes4s.plan

import eyes4s.kernel.*

class EpochComponentSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val schema                            = get(DefinitionId.of("example.marker", 1))
  private def plan(
      occurrence: Occurrence = Occurrence.RequireUnique,
      from: Long = -10,
      until: Long = 20,
      width: Long = 7,
      finalBin: FinalBin = FinalBin.IncludeShortFinal
  ) =
    get(
      EpochPlan.of(
        MarkSelector("start", occurrence),
        get(Window.of(Span.micros(from), Span.micros(until))),
        get(PositiveSpan.of(Span.micros(width))),
        finalBin
      )
    )

  test("epoch inspection explains every saved field and remains a component") {
    val value     = plan()
    val inspected = get(EpochComponent.inspect(value, schema))
    assertEquals(EpochComponent.component, AnalysisComponent.Epochs)
    assertEquals(inspected.description, EpochComponent.description(value, schema))
    assertEquals(
      inspected.fields.map(_.info.id),
      Vector(
        "markerSchema",
        "anchor.kind",
        "anchor.occurrence",
        "window",
        "binWidth",
        "finalBin"
      )
    )
    assert(inspected.fields.forall(_.info.meaning.nonEmpty))
    assert(inspected.views.forall(_.version == 1))
    assertEquals(inspected.execution, ExecutionCapability.SynchronousWholeOperation)
  }

  test("signed offsets widths and Nth indices retain all64bits in inspection") {
    val n           = 9007199254740993L
    val value       = plan(Occurrence.Nth(get(NonNegativeLong.of(n))), n, n + 7, n)
    val description = get(EpochComponent.inspect(value, schema)).description.toMap
    import Provenance.Param.Text
    assertEquals(description("anchor.occurrence"), Vector(Text("Nth"), Text(n.toString)))
    assertEquals(description("window"), Vector(Text(n.toString), Text((n + 7).toString)))
    assertEquals(description("binWidth"), Vector(Text(n.toString)))
  }

  test("all occurrence and final-bin policies are named independently") {
    Vector(
      Occurrence.RequireUnique,
      Occurrence.First,
      Occurrence.Last,
      Occurrence.Nth(get(NonNegativeLong.of(0)))
    ).foreach { selected =>
      FinalBin.values.foreach { finalBin =>
        val value     = plan(selected, finalBin = finalBin)
        val inspected = get(EpochComponent.inspect(value, schema))
        assertEquals(
          inspected.description.toMap.apply("finalBin"),
          Vector(Provenance.Param.Text(finalBin.toString))
        )
        val occurrence = inspected.views.find(_.id.value == "anchor.occurrence").get.kind
        assert(occurrence match
          case FieldKind.Variant(cases) =>
            cases.map(_.token) == Vector("RequireUnique", "First", "Last", "Nth")
          case _ => false)
      }
    }
  }

  test("empty windows are inspected without tightening the computation contract") {
    val value = plan(from = 0, until = 0, width = 1, finalBin = FinalBin.RequireExactDivision)
    val inspected = get(EpochComponent.inspect(value, schema))
    assertEquals(
      inspected.description.toMap.apply("window"),
      Vector(Provenance.Param.Text("0"), Provenance.Param.Text("0"))
    )
  }
