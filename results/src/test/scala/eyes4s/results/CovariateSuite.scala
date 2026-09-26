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

package eyes4s.results

import eyes4s.plan.*

/** Trial covariates read from a trial inventory's attributes: joined by
  * trial key, typed by declaration, blank cells not recorded and cells of
  * another type unparsed; and the small value and table helpers.
  */
class CovariateSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(x => fail(s"$x"), identity)

  private val memory     = get(CovariateName.of("memory"))
  private val confidence = get(CovariateName.of("confidence"))
  private val seen       = get(CovariateName.of("seen"))
  private val levels     = get(Levels.of(Vector("Remembered", "Forgotten")))
  private val schema     = get(
    CovariateSchema.of(
      Vector(
        Covariate(memory, CovariateType.Categorical(levels)),
        Covariate(confidence, CovariateType.Numeric(get(NumericUnit.of("rating")))),
        Covariate(seen, CovariateType.Binary)
      )
    )
  )
  private val columns = Vector(
    AttributeColumn("memory", AttributeKind.Text),
    AttributeColumn("confidence", AttributeKind.Integer),
    AttributeColumn("seen", AttributeKind.Integer)
  )

  private def trialIdentity(p: String, trial: String) =
    get(TrialIdentity.of(p, "recall", trial, TrialOccurrence.first))

  private def trial(
      id: TrialIdentity,
      row: Int,
      item: Option[String],
      values: Vector[AttributeValue]
  ) = get(
    InventoryTrial.of(
      id,
      Vector(row),
      item,
      get(Attributes.of(columns.map(_.name).zip(values))),
      Vector.empty,
      Vector.empty,
      TrialDisposition.Absent
    )
  )

  private val ledger = get(
    InventoryLedger.of(
      SourceRef.of("trials.csv", Vector("participant", "trial", "item"), Vector.empty),
      Vector("participant", "trial", "item") ++ columns.map(_.name),
      columns,
      Vector(
        trial(
          trialIdentity("p1", "t1"),
          2,
          Some("a"),
          Vector(
            AttributeValue.Text("Remembered"),
            AttributeValue.Integer(4),
            AttributeValue.Integer(1)
          )
        ),
        trial(
          trialIdentity("p1", "t2"),
          3,
          Some("b"),
          Vector(AttributeValue.Text("Unsure"), AttributeValue.Blank, AttributeValue.Integer(7))
        ),
        // No item is resolved: the trial cannot be a study key and is left out.
        trial(
          trialIdentity("p1", "t3"),
          4,
          None,
          Vector(
            AttributeValue.Text("Forgotten"),
            AttributeValue.Integer(2),
            AttributeValue.Integer(0)
          )
        )
      ),
      Vector.empty,
      Vector.empty,
      Vector.empty,
      SampleCountRule.PositiveColumn("samples")
    )
  )

  test("a ledger's inventory is read by trial key, typed, with blanks and bad cells distinct") {
    val table = get(CovariateTable.fromLedger(schema, ledger))
    val t1    = get(trialIdentity("p1", "t1").withItem("a"))
    val t2    = get(trialIdentity("p1", "t2").withItem("b"))
    assertEquals(table.rows.map(_.key), Vector(t1, t2))
    assertEquals(table.value(t1, memory), Value.Present(CovariateValue.Level("Remembered")))
    assertEquals(table.value(t1, confidence), Value.Present(CovariateValue.Number(4.0)))
    assertEquals(table.value(t1, seen), Value.Present(CovariateValue.Flag(true)))
    assertEquals(table.value(t2, memory), Value.Missing(Absence.Unparsed))
    assertEquals(table.value(t2, confidence), Value.Missing(Absence.NotRecorded))
    assertEquals(table.value(t2, seen), Value.Missing(Absence.Unparsed))
    assertEquals(
      get(table.row(t2).toRight("row")).unparsed,
      Vector(memory -> "Unsure", seen -> "7")
    )
    // A key the inventory does not declare is not recorded.
    val absent = get(trialIdentity("p9", "t9").withItem("z"))
    assertEquals(table.value(absent, memory), Value.Missing(Absence.NotRecorded))
    assertEquals(get(CovariateTable.fromLedger(schema, ledger)), table)
    assertEquals(get(CovariateTable.fromLedger(schema, ledger)).hashCode, table.hashCode)
  }

  test("every covariate must be an inventory column of a kind that can hold it") {
    val unknown = get(
      CovariateSchema.of(Vector(Covariate(get(CovariateName.of("mood")), CovariateType.Binary)))
    )
    assertEquals(
      CovariateTable.fromLedger(unknown, ledger),
      Left(CovariateError.UnknownAttribute("mood", columns.map(_.name)))
    )
    val numeric = get(
      CovariateSchema.of(
        Vector(Covariate(memory, CovariateType.Numeric(get(NumericUnit.of("rating")))))
      )
    )
    assertEquals(
      CovariateTable.fromLedger(numeric, ledger),
      Left(CovariateError.IncompatibleKind("memory", "numeric(rating)", "Text"))
    )
    assert(CovariateType.Binary.admits(AttributeKind.Integer))
    assert(!CovariateType.Binary.admits(AttributeKind.Number))
    assertEquals(CovariateType.raw(AttributeValue.Number(2.5)), "2.5")
    assertEquals(CovariateType.raw(AttributeValue.Blank), "")
  }

  test("declarations refuse blank names, repeated levels and repeated covariates") {
    assert(CovariateName.of(" ").isLeft)
    assertEquals(memory.toString, "memory")
    assert(Levels.of(Vector("a", "a")).isLeft)
    assert(Levels.of(Vector.empty).isLeft)
    assertEquals(levels.rank("Forgotten"), Some(1))
    assertEquals(levels.rank("Unsure"), None)
    assert(NumericUnit.of("").isLeft)
    assert(CovariateSchema.of(schema.covariates ++ schema.covariates.take(1)).isLeft)
    assertEquals(FlagTerm.Binary(seen).render, "covariate:seen")
    val key = StudyKey("p1", "a", "recall")
    assertEquals(
      CovariateTable.of(schema, Vector(key -> Attributes.empty, key -> Attributes.empty)),
      Left(CovariateError.DuplicateKey(key))
    )
  }

  test("values say why they are missing, and fold without a default") {
    val present: Value[Double] = Value.Present(2.0)
    val missing: Value[Double] = Value.Missing(Absence.EmptyGroup)
    assertEquals(present.absence, None)
    assertEquals(missing.absence, Some(Absence.EmptyGroup))
    assertEquals(present.fold(_ => -1.0)(_ * 2), 4.0)
    assertEquals(missing.fold(_.toString)(_.toString), "EmptyGroup")
  }

  test("table JSON reads members and arrays, and a digest prints as hex") {
    val doc = get(TableJson.parse("""{"a":[1,2],"a":[3]}""").left.map(_.toString))
    assertEquals(doc.field("a"), Some(TableJson.arr(TableJson.integer(3))))
    assertEquals(doc.field("b"), None)
    val digest = TableDigest.ofUtf8("abc")
    assertEquals(digest.toString, digest.hex)
    assertEquals(digest.hashCode, TableDigest.ofUtf8("abc").hashCode)
    assert(digest.hex.startsWith("ba7816bf"))
  }

  test("a report hashes like the report it equals") {
    val binding = ReportBinding(
      get(BindingDigest.parse("plan", "a" * 64)),
      get(BindingDigest.parse("input", "b" * 64)),
      get(BindingDigest.parse("result", "c" * 64)),
      None
    )
    val table = get(QueryTable.of(0, Vector("value"), CovariateSchema.empty, Vector.empty))
    val spec  = get(
      ReportSpec.of(
        get(ReportId.of("empty")),
        0,
        get(ReportSelection.of(Vector(Role.Difference), Vector("value")))
      )
    )
    val a = get(Report.reduce(spec, table, binding))
    val b = get(Report.reduce(spec, table, binding))
    assertEquals(a, b)
    assertEquals(a.hashCode, b.hashCode)
    assertEquals(a.cells.map(_.estimate), Vector(Value.Missing(Absence.EmptyGroup)))
  }
