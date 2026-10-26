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

package eyes4s.studio.core.backend

import eyes4s.studio.core.document.ReportingId
import eyes4s.studio.core.selection.{ReportGroup, ScaleIndex, StudioRef}
import io.circe.syntax.*

class ReportViewLookupSuite extends munit.FunSuite:
  private val run    = RunId(7)
  private val spec   = ReportingId.of("lookup").toOption.get
  private val groups = Vector(None, Some(Response("old")), Some(Response("new")))
  private val rows   = for
    group  <- groups
    role   <- ReportRole.values.toVector
    person <- Vector("P02", "P01", "P03")
  yield ReportParticipantView(
    group,
    role,
    person,
    5,
    Some(0.25),
    None,
    StudioRef.ReportParticipant(
      run,
      spec,
      ScaleIndex.first,
      group.fold(ReportGroup.Whole)(ReportGroup.Level(_)),
      role,
      person
    )
  )
  private def report(values: Vector[ReportParticipantView]) =
    ReportView(
      run,
      spec,
      0,
      Vector.empty,
      values,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      Vector.empty
    )

  test(
    "participant lookup distinguishes role, response and participant; missing stays absent"
  ) {
    val view = report(rows)
    rows.foreach(p => assertEquals(view.participant(p.group, p.role, p.participant), Some(p)))
    assertEquals(view.participant(None, ReportRole.Difference, "absent"), None)
    assertEquals(view.participant(Some(Response("other")), ReportRole.Matched, "P01"), None)
  }

  test("unchecked duplicate rows retain first-match semantics across codec round trips") {
    val first     = rows.head
    val duplicate = first.copy(queries = 99, value = Some(9.0))
    val view      = report(rows :+ duplicate)
    assertEquals(view.participant(first.group, first.role, first.participant), Some(first))
    val decoded = view.asJson.as[ReportView].toOption.get
    assertEquals(decoded.participants, rows :+ duplicate)
    assertEquals(decoded.participant(first.group, first.role, first.participant), Some(first))
  }

  test("copying a served view replaces its cached lookup without altering the original") {
    val view  = report(rows)
    val first = rows.head
    assertEquals(view.participant(first.group, first.role, first.participant), Some(first))
    val changed = first.copy(value = None, absence = Some(ReportAbsence.NotRecorded))
    val edited  = view.copy(participants = changed +: rows.tail)
    assertEquals(edited.participant(first.group, first.role, first.participant), Some(changed))
    assertEquals(view.participant(first.group, first.role, first.participant), Some(first))
  }
