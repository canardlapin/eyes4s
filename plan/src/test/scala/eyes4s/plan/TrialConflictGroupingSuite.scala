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

class TrialConflictGroupingSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(error => fail(s"$error"), identity)

  private def row(occurrence: Int, item: String, record: Int, trial: String = "t1") =
    get(
      TrialConflictRow.of(
        get(TrialIdentity.of("p1", "encoding", trial, get(TrialOccurrence.of(occurrence)))),
        item,
        get(CsvRecord.of(record))
      )
    )

  test("an empty fold has no groups") {
    assertEquals(TrialConflictGrouping.group(Vector.empty), Vector.empty)
  }

  test("one parsed trial key is an ok singleton") {
    val result = TrialConflictGrouping.group(Vector(row(1, "beach", 2))).head
    assertEquals(result.keys.size, 1)
    assertEquals(result.classification, TrialConflictClassification.Ok)
  }

  test("the incremental fold preserves record order and chooses the canonical representative") {
    val rows  = Vector(row(2, "beach", 7), row(1, "beach", 4), row(1, "beach", 2))
    val group =
      rows.foldLeft(TrialConflictGrouping.empty)((state, next) => state.add(next)).groups
    val result = group.head
    assertEquals(result.representative.occurrence.value, 1)
    assertEquals(result.representative.item, "beach")
    assertEquals(result.records.map(_.value), Vector(2, 4, 7))
    assertEquals(
      result.classification,
      TrialConflictClassification.OccurrenceConflict(
        Vector(get(TrialOccurrence.of(1)), get(TrialOccurrence.of(2)))
      )
    )
  }

  test("permuted and chunked input produces the same canonical groups") {
    val rows    = Vector(row(2, "beach", 7), row(1, "beach", 4), row(1, "beach", 2))
    val onePass = TrialConflictGrouping.group(rows.reverse)
    val chunked = rows
      .take(1)
      .foldLeft(TrialConflictGrouping.empty)((state, next) => state.add(next))
      .add(rows(1))
      .add(rows(2))
      .groups
    assertEquals(onePass, chunked)
  }

  test("an item conflict takes precedence when occurrences also disagree") {
    val result = TrialConflictGrouping.group(Vector(row(2, "b", 3), row(1, "a", 2))).head
    assertEquals(result.representative.item, "a")
    assertEquals(
      result.classification,
      TrialConflictClassification.ItemConflict(Vector("a", "b"))
    )
  }

  test("equal parsed trial keys are ok even across multiple records") {
    val parsed = Vector("01", "1").map(raw => get(TrialOccurrence.of(raw.toInt)))
    val rows   = parsed.zip(Vector(5, 2)).map { case (occurrence, record) =>
      get(
        TrialConflictRow.of(
          get(TrialIdentity.of("p1", "encoding", "t1", occurrence)),
          "beach",
          get(CsvRecord.of(record))
        )
      )
    }
    val result = TrialConflictGrouping.group(rows).head
    assertEquals(result.classification, TrialConflictClassification.Ok)
    assertEquals(result.records.map(_.value), Vector(2, 5))
  }

  test("a blank item is refused at the typed input boundary") {
    val identity = get(TrialIdentity.of("p1", "encoding", "t1", TrialOccurrence.first))
    assertEquals(
      TrialConflictRow.of(identity, "", get(CsvRecord.of(2))),
      Left(PlanError.BlankKeyField("item"))
    )
  }

  test("participants, phases and labels remain separate in an immutable fold") {
    val original = row(1, "beach", 2)
    val others   = Vector(
      ("p2", "encoding", "t1"),
      ("p1", "retrieval", "t1"),
      ("p1", "encoding", "t2")
    ).zipWithIndex.map { case ((participant, phase, trial), index) =>
      val identity = get(TrialIdentity.of(participant, phase, trial, TrialOccurrence.first))
      val key      = get(identity.withItem("street"))
      val parsed   = TrialConflictRow.from(key, get(CsvRecord.of(index + 3)))
      assertEquals(parsed.identity, identity)
      parsed
    }
    val first    = TrialConflictGrouping.empty.add(original)
    val combined = others.foldLeft(first)((state, next) => state.add(next)).groups
    assertEquals(first.groups.map(_.records.map(_.value)), Vector(Vector(2)))
    assertEquals(combined.size, 4)
    assert(combined.forall(_.classification == TrialConflictClassification.Ok))
    assertEquals(combined.flatMap(_.records.map(_.value)).sorted, Vector(2, 3, 4, 5))
  }
