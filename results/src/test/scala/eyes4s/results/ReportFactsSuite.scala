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

import eyes4s.plan.{
  CellKey,
  ContrastKey,
  Fact,
  FactError,
  FactSlot,
  FactSource,
  FactValue,
  GroupCell,
  MethodsFacts,
  StudyKey
}

/** The reporting facts of a methods text, read from reports over a
  * hand-built query table whose every count is worked out here.
  */
class ReportFactsSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(e => fail(s"$e"), identity)

  private val hex     = (c: Char) => c.toString * 64
  private val binding = ReportBinding(
    get(BindingDigest.parse("plan", hex('a'))),
    get(BindingDigest.parse("input", hex('b'))),
    get(BindingDigest.parse("result", hex('c'))),
    None
  )
  private val memory     = get(CovariateName.of("memory"))
  private val levels     = get(Levels.of(Vector("Remembered", "Forgotten")))
  private val memoryTerm = LevelTerm.Categorical(memory, levels)
  private val schema     =
    get(CovariateSchema.of(Vector(Covariate(memory, CovariateType.Categorical(levels)))))
  private val difference = get(ReportSelection.of(Vector(Role.Difference), Vector("value")))
  private val id         = get(ReportId.of("by-memory"))

  private def query(p: String, item: String, remembered: Boolean): Query[StudyKey] = get(
    Query.of(
      StudyKey(p, item, "recall"),
      p,
      item,
      "recall",
      1,
      Vector(
        memory -> Value.Present(
          CovariateValue.Level(if remembered then "Remembered" else "Forgotten")
        )
      ),
      Vector.empty,
      Vector.empty,
      RoleOutcome.NotStored,
      RoleOutcome.NotStored,
      RoleOutcome.Scored(Vector(0.25))
    )
  )

  // Queries per participant and group: p1 Remembered 2, Forgotten 1; p2
  // Remembered 1, Forgotten 2; p3 Forgotten 1.
  private val table = get(
    QueryTable.of(
      2,
      Vector("value"),
      schema,
      Vector(
        query("p1", "a", true),
        query("p1", "b", true),
        query("p1", "c", false),
        query("p2", "a", true),
        query("p2", "e", false),
        query("p2", "f", false),
        query("p3", "d", false)
      )
    )
  )

  private def report(
      groupBy: Vector[Grouping],
      reduce: ReducePolicy = ReducePolicy.default,
      contrast: Option[LevelContrast] = None
  ) = get(
    Report.reduce(
      get(
        ReportSpec
          .of(id, 2, difference, None, groupBy, reduce, contrast, Spread.StandardDeviation)
      ),
      table,
      binding
    )
  )
  private val byMemory        = Vector(Grouping.ByLevel(memoryTerm))
  private val contrast        = Some(LevelContrast(memoryTerm, "Remembered", "Forgotten"))
  private def atLeast(n: Int) = ReducePolicy.ParticipantMeans(get(MinimumQueries.of(n)))

  private def fact(slot: FactSlot, source: FactSource, value: FactValue) =
    get(Fact.of(slot, source, value))
  private def key(group: String, participant: Option[String] = None) =
    CellKey(Vector("covariate:memory" -> group), "difference", "value", 2, participant)
  private val cells = FactSource.ReportCells(Vector(key("Remembered"), key("Forgotten")))
  private def cell(p: String, group: String, queries: Int) =
    GroupCell(p, group, queries, FactSource.ReportCells(Vector(key(group, Some(p)))))
  private val spec = FactSource.ReportSpec("by-memory")

  test("a grouped report with a contrast and no minimum: n, paired n, sizes, smallest") {
    val facts = get(ReportFacts.of(report(byMemory, contrast = contrast), "memory"))
    assertEquals(
      facts,
      Vector(
        fact(FactSlot.ReportingSpec, spec, FactValue.Label("memory")),
        fact(
          FactSlot.GroupN("Remembered"),
          FactSource.ReportCells(Vector(key("Remembered"))),
          FactValue.Count(2)
        ),
        fact(
          FactSlot.GroupN("Forgotten"),
          FactSource.ReportCells(Vector(key("Forgotten"))),
          FactValue.Count(3)
        ),
        fact(
          FactSlot.PairedN,
          FactSource.ReportContrast(
            ContrastKey(Vector.empty, "covariate:memory", "Remembered", "Forgotten", 2)
          ),
          FactValue.Count(2)
        ),
        fact(FactSlot.GroupSizeRange, cells, FactValue.Range(1, 2)),
        fact(
          FactSlot.SmallestGroups,
          cells,
          FactValue.Breakdown(
            Vector(
              cell("p2", "Remembered", 1),
              cell("p1", "Forgotten", 1),
              cell("p3", "Forgotten", 1)
            )
          )
        )
      )
    )
    // They are a valid set of methods facts.
    assert(MethodsFacts.of(facts).isRight)
  }

  test("a minimum above one is stated, with the cells it left out, and n counts the rest") {
    val facts = get(ReportFacts.of(report(byMemory, atLeast(2), contrast), "memory"))
    assertEquals(
      facts,
      Vector(
        fact(FactSlot.ReportingSpec, spec, FactValue.Label("memory")),
        fact(FactSlot.MinimumQueries, spec, FactValue.Count(2)),
        fact(
          FactSlot.GroupN("Remembered"),
          FactSource.ReportCells(Vector(key("Remembered"))),
          FactValue.Count(1)
        ),
        fact(
          FactSlot.GroupN("Forgotten"),
          FactSource.ReportCells(Vector(key("Forgotten"))),
          FactValue.Count(1)
        ),
        fact(
          FactSlot.PairedN,
          FactSource.ReportContrast(
            ContrastKey(Vector.empty, "covariate:memory", "Remembered", "Forgotten", 2)
          ),
          FactValue.Count(0)
        ),
        fact(FactSlot.GroupSizeRange, cells, FactValue.Range(1, 2)),
        fact(
          FactSlot.BelowMinimumQueries,
          cells,
          FactValue.Breakdown(
            Vector(
              cell("p2", "Remembered", 1),
              cell("p1", "Forgotten", 1),
              cell("p3", "Forgotten", 1)
            )
          )
        )
      )
    )
    // A minimum of one is no minimum: the smallest groups are stated instead.
    val one = get(ReportFacts.of(report(byMemory, atLeast(1)), "memory")).map(_.slot)
    assert(one.contains(FactSlot.SmallestGroups) && !one.contains(FactSlot.MinimumQueries), one)
  }

  test("an ungrouped report has no group n; its one group is 'all'") {
    val facts = get(ReportFacts.of(report(Vector.empty), "all trials"))
    val all   =
      FactSource.ReportCells(Vector(CellKey(Vector.empty, "difference", "value", 2, None)))
    assertEquals(
      facts.map(_.slot),
      Vector(FactSlot.ReportingSpec, FactSlot.GroupSizeRange, FactSlot.SmallestGroups)
    )
    assertEquals(
      facts.find(_.slot == FactSlot.GroupSizeRange).map(_.value),
      Some(FactValue.Range(1, 3))
    )
    assertEquals(
      facts.find(_.slot == FactSlot.SmallestGroups).map(_.value),
      Some(
        FactValue.Breakdown(
          Vector(
            GroupCell(
              "p3",
              "all",
              1,
              FactSource.ReportCells(
                Vector(CellKey(Vector.empty, "difference", "value", 2, Some("p3")))
              )
            )
          )
        )
      )
    )
    assertEquals(facts.find(_.slot == FactSlot.GroupSizeRange).map(_.source), Some(all))
    assertEquals(ReportFacts.level(GroupKey(Vector("a" -> "x", "b" -> "y"))), "x · y")
  }

  test("pooled queries have no minimum; a role or component the report lacks gives no cells") {
    val pooled = get(ReportFacts.of(report(byMemory, ReducePolicy.PooledQueries), "memory"))
    assert(pooled.exists(_.slot == FactSlot.SmallestGroups), pooled)
    val control = get(ReportFacts.of(report(byMemory), "memory", role = Some(Role.Control)))
    assertEquals(control.map(_.slot), Vector(FactSlot.ReportingSpec))
  }

  test("a blank display name is refused as the methods facts refuse it") {
    assertEquals(
      ReportFacts.of(report(byMemory), " "),
      Left(FactError.BlankLabel("reportingSpec"))
    )
  }
