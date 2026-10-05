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

  test(
    "each shared conflict field compares parsed values, including numeric spelling and blankness"
  ) {
    import TrialInventory.ConflictField.*
    val base   = Vector("p", "f", "t", "1", "item", "", "1", "0.0")
    val first  = get(TrialInventory.parseRow(header, base, 2, columns))
    val fields =
      Vector(Occurrence, Item, Attribute("text"), Attribute("count"), Attribute("value"))
    val changes = Vector(3 -> "2", 4 -> "other", 5 -> "text", 6 -> "2", 7 -> "1")
    changes.zipWithIndex.foreach { case ((column, value), changed) =>
      val other = get(TrialInventory.parseRow(header, base.updated(column, value), 3, columns))
      fields.zipWithIndex.foreach { (field, index) =>
        assertEquals(TrialInventory.conflicts(first, other, field), index == changed)
        assertEquals(TrialInventory.conflicts(other, first, field), index == changed)
      }
    }
    val equivalent = get(
      TrialInventory.parseRow(
        header,
        base.updated(3, "01").updated(6, "+01").updated(7, "-0.0"),
        3,
        columns
      )
    )
    fields.foreach(field => assert(!TrialInventory.conflicts(first, equivalent, field)))
    val blank =
      get(TrialInventory.parseRow(header, base.updated(6, "").updated(7, ""), 3, columns))
    assert(TrialInventory.conflicts(first, blank, Attribute("count")))
    assert(TrialInventory.conflicts(first, blank, Attribute("value")))
  }

  test(
    "field decisions preserve the previous distinct-value group rule and declaration order"
  ) {
    val base         = Vector("p", "f", "t", "1", "item", "", "1", "0.0")
    val alternatives = Vector(
      base,
      base.updated(3, "01").updated(6, "+1").updated(7, "-0.0"),
      base.updated(3, "2"),
      base.updated(4, "other"),
      base.updated(5, "text"),
      base.updated(6, "2"),
      base.updated(7, "1"),
      base.updated(6, "").updated(7, "")
    )
    for a <- alternatives; b <- alternatives; c <- alternatives do
      val raws = Vector(a, b, c)
      val rows = raws.zipWithIndex.map((raw, i) =>
        get(TrialInventory.parseRow(header, raw, i + 2, columns))
      )
      // This is the pre-extraction algorithm: compare distinct parsed values
      // across the group, independently of the shared pairwise predicate.
      val expected = columns.trial.occurrence.filter(_ =>
        rows.map(_.identity.occurrence.value).distinct.size > 1
      ) ++
        columns.item.filter(_ => rows.map(_.item).distinct.size > 1) ++
        columns.attributes
          .map(_.name)
          .filter(name => rows.map(_.attributes.get(name)).distinct.size > 1)
      val actual = TrialInventory.read(text(raws), columns)
      if expected.isEmpty then assertEquals(get(actual).trials.head.records, Vector(2, 3, 4))
      else
        assertEquals(
          actual,
          Left(
            FixationImportError.Inventory(
              NonEmptyVector.one(
                InventoryError.Conflict("p", "f", "t", Vector(2, 3, 4), expected.toVector)
              )
            )
          )
        )
  }
