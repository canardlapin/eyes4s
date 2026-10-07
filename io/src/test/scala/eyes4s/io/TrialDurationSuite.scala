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

import eyes4s.plan.{AttributeColumn, AttributeKind, AttributeValue, InventoryError}

class TrialDurationSuite extends munit.FunSuite:
  private def ok[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val trialColumns                     = ok(TrialColumns.of("who", "phase", "trial"))
  private def declaration(unit: TimestampUnit) = ok(TrialDurationFromStart.of("elapsed", unit))
  private def read(values: Vector[String], unit: TimestampUnit = TimestampUnit.Seconds) =
    val columns = ok(
      TrialInventoryColumns.of(
        trialColumns,
        Some("item"),
        Vector.empty,
        duration = Some(declaration(unit))
      )
    )
    val text =
      "who,phase,trial,item,elapsed\n" + values.map(v => s"P,Encoding,t,item,$v\n").mkString
    (text, TrialInventory.read(text, columns))

  test("explicit units produce exact half-open native extents and preserve source cells") {
    Vector(
      TimestampUnit.Seconds      -> "5",
      TimestampUnit.Milliseconds -> "5000",
      TimestampUnit.Microseconds -> "5000000"
    ).foreach { (unit, raw) =>
      val (_, result) = read(Vector(raw), unit)
      val inventory   = ok(result)
      val extent      =
        ok(declaration(unit).extent(inventory.trials.head)).getOrElse(fail("no extent"))
      assertEquals(extent.from.toMicros, 0L)
      assertEquals(extent.until.toMicros, 5000000L)
      assertEquals(inventory.rows, Vector(Vector("P", "Encoding", "t", "item", raw)))
      assertEquals(
        inventory.trials.head.attributes.get("elapsed"),
        Some(AttributeValue.Text(raw))
      )
    }
  }

  test("unsafe JSON-sized integer duration remains exact source text and native microseconds") {
    val raw       = "9007199254740993"
    val inventory = ok(read(Vector(raw), TimestampUnit.Microseconds)._2)
    assertEquals(
      ok(declaration(TimestampUnit.Microseconds).extent(inventory.trials.head))
        .map(_.width.toMicros),
      Some(9007199254740993L)
    )
    assertEquals(inventory.rows.head.last, raw)
  }

  test("blank timing is explicitly absent rather than zero duration") {
    val inventory = ok(read(Vector(""))._2)
    assertEquals(declaration(TimestampUnit.Seconds).extent(inventory.trials.head), Right(None))
    assertEquals(inventory.trials.head.attributes.get("elapsed"), Some(AttributeValue.Blank))
  }

  test("malformed and nonpositive timing names the original record, column and cell") {
    Vector("zero", "0", "-1", "NaN", "1e100").foreach { raw =>
      val result = read(Vector(raw))._2
      val errors = result.swap.toOption
        .collect { case FixationImportError.Inventory(es) => es.toVector }
        .getOrElse(fail(result.toString))
      assert(
        errors.exists {
          case InventoryError.Field(2, "elapsed", value, _) => value == raw; case _ => false
        },
        errors
      )
    }
  }

  test("equal duplicate text retains every source row and source record") {
    val inventory = ok(read(Vector("5", "5"))._2)
    assertEquals(inventory.rows, Vector.fill(2)(Vector("P", "Encoding", "t", "item", "5")))
    assertEquals(inventory.trials.size, 1)
    assertEquals(inventory.trials.head.records, Vector(2, 3))
    assertEquals(
      ok(declaration(TimestampUnit.Seconds).extent(inventory.trials.head))
        .map(_.width.toMicros),
      Some(5000000L)
    )
  }

  test("different duration text conflicts even when rounded extents agree; blank is distinct") {
    Vector(Vector("5", "5.0"), Vector("5", "6"), Vector("", "5")).foreach { cells =>
      val result = read(cells)._2
      val errors = result.swap.toOption
        .collect { case FixationImportError.Inventory(es) => es.toVector }
        .getOrElse(fail(result.toString))
      assert(
        errors.exists {
          case InventoryError.Conflict("P", "Encoding", "t", records, columns) =>
            records == Vector(2, 3) && columns == Vector("elapsed");
          case _ => false
        },
        errors
      )
    }
  }

  test("duration declarations refuse missing headers and lossy attribute kinds") {
    val columns = ok(
      TrialInventoryColumns.of(
        trialColumns,
        Some("item"),
        Vector.empty,
        duration = Some(declaration(TimestampUnit.Seconds))
      )
    )
    assert(TrialInventory.read("who,phase,trial,item\nP,Encoding,t,item\n", columns).isLeft)
    Vector(AttributeKind.Number, AttributeKind.Integer).foreach { kind =>
      assert(
        TrialInventoryColumns
          .of(
            trialColumns,
            Some("item"),
            Vector(AttributeColumn("elapsed", kind)),
            Some(declaration(TimestampUnit.Seconds))
          )
          .isLeft
      )
    }
  }

  test("native timestamp rounding policy is named and shared with fixation parsing") {
    assertEquals(
      declaration(TimestampUnit.Milliseconds).parse("1.2345", 7).map(_.map(_.width.toMicros)),
      Right(Some(1235L))
    )
    assert(declaration(TimestampUnit.Microseconds).parse("0.1", 7).isLeft)
  }
