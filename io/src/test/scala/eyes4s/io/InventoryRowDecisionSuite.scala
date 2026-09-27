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

package eyes4s.io

import cats.data.NonEmptyVector
import eyes4s.plan.*

class InventoryRowDecisionSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val columns                           = get(
    TrialInventoryColumns.of(
      get(TrialColumns.of("p", "phase", "trial", Some("occurrence"))),
      Some("item"),
      Vector(
        AttributeColumn("text", AttributeKind.Text),
        AttributeColumn("count", AttributeKind.Integer),
        AttributeColumn("value", AttributeKind.Number)
      )
    )
  )
  private val header =
    Vector("p", "phase", "trial", "occurrence", "item", "text", "count", "value")
  private val invalid = Vector(" ", "", "", "0", " ", "kept", "x", "NaN")
  private def defects(number: Int): Vector[InventoryError] = Vector(
    InventoryError.Field(number, "p", " ", "a non-blank value"),
    InventoryError.Field(number, "phase", "", "a non-blank value"),
    InventoryError.Field(number, "trial", "", "a non-blank value"),
    InventoryError.Field(number, "occurrence", "0", "a positive integer occurrence"),
    InventoryError.Field(number, "item", " ", "a non-blank item"),
    InventoryError.Field(number, "count", "x", "a decimal integer"),
    InventoryError.Field(number, "value", "NaN", "a finite decimal number")
  )
  private def text(rows: Vector[Vector[String]]) =
    (header +: rows).map(_.mkString(",")).mkString("\n") + "\n"

  test(
    "shared row decision retains every defect in declared order and uses absolute record numbers"
  ) {
    Vector(2, 37, 12000).foreach { n =>
      assertEquals(TrialInventory.parseRow(header, invalid, n, columns), Left(defects(n)))
    }
  }

  test("wrong width precedes all field interpretation errors") {
    val short = invalid.dropRight(1)
    assertEquals(
      TrialInventory.parseRow(header, short, 37, columns),
      Left(Vector(InventoryError.Width(37, 8, 7)))
    )
    assertEquals(
      TrialInventory.parseRow(header, invalid :+ "extra", 38, columns),
      Left(Vector(InventoryError.Width(38, 8, 9)))
    )
  }

  test("valid records preserve identity and text lexemes while parsing typed attributes") {
    val raw = Vector(" p ", "f", "t", "01", " item ", "", "+0002", "1e1")
    val row = get(TrialInventory.parseRow(header, raw, 71, columns))
    assertEquals(row.number, 71)
    assertEquals(row.identity, get(TrialIdentity.of(" p ", "f", "t", TrialOccurrence.first)))
    assertEquals(row.item, Some(" item "))
    assertEquals(
      row.attributes.entries,
      Vector(
        "text"  -> AttributeValue.Blank,
        "count" -> AttributeValue.Integer(2L),
        "value" -> AttributeValue.Number(10.0)
      )
    )
    val reorderedHeader = Vector("unused") ++ header.reverse
    val reorderedRaw    = Vector("ignored") ++ raw.reverse
    assertEquals(
      TrialInventory.parseRow(reorderedHeader, reorderedRaw, 71, columns),
      Right(row)
    )
    val minimal = get(TrialInventoryColumns.of(get(TrialColumns.of("p", "phase", "trial"))))
    val noItem  = get(
      TrialInventory.parseRow(Vector("p", "phase", "trial"), Vector("p", "f", "t"), 72, minimal)
    )
    assertEquals(noItem.identity.occurrence, TrialOccurrence.first)
    assertEquals(noItem.item, None)
    assertEquals(noItem.attributes, Attributes.empty)
  }

  test("whole inventory reports all row errors before conflicts in first-record order") {
    def valid(p: String, occurrence: String, item: String) =
      Vector(p, "f", "t", occurrence, item, "", "1", "1.0")
    val rows = Vector(
      valid("z", "1", "one"),
      valid("a", "1", "one"),
      valid("z", "2", "two"),
      valid("a", "3", "three"),
      invalid,
      Vector("short")
    )
    val expected = defects(6) ++ Vector(
      InventoryError.Width(7, 8, 1),
      InventoryError.Conflict("z", "f", "t", Vector(2, 4), Vector("occurrence", "item")),
      InventoryError.Conflict("a", "f", "t", Vector(3, 5), Vector("occurrence", "item"))
    )
    assertEquals(
      TrialInventory.read(text(rows), columns),
      Left(
        FixationImportError.Inventory(get(NonEmptyVector.fromVector(expected).toRight("empty")))
      )
    )
  }

  test("duplicate records collapse by parsed values while original source rows remain intact") {
    val rows = Vector(
      Vector("p", "f", "t", "01", "item", "", "01", "1.0"),
      Vector("p", "f", "t", "1", "item", "", "+1", "1e0")
    )
    val read = get(TrialInventory.read(text(rows), columns))
    assertEquals(read.rows, rows)
    assertEquals(read.trials.size, 1)
    assertEquals(read.trials.head.records, Vector(2, 3))
    assertEquals(
      read.trials.head.attributes.entries,
      Vector(
        "text"  -> AttributeValue.Blank,
        "count" -> AttributeValue.Integer(1L),
        "value" -> AttributeValue.Number(1.0)
      )
    )
  }
