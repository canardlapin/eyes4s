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

import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

/** The design facts of a run, read from hand-built query tables and from a
  * completed study whose control reductions give each query's controls.
  */
class RunFactsSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(e => fail(s"$e"), identity)

  private val arithmetic = DiagnosticCode("contrast-row", "arithmetic")
  private val offWindow  = DiagnosticCode("study-contrast", "off-window")

  private def query(item: String, difference: RoleOutcome): Query[StudyKey] = get(
    Query.of(
      StudyKey("p1", item, "recall"),
      "p1",
      item,
      "recall",
      1,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      RoleOutcome.NotStored,
      RoleOutcome.NotStored,
      difference
    )
  )
  private val scored                       = RoleOutcome.Scored(Vector(0.5))
  private def failed(code: DiagnosticCode) = RoleOutcome.Failed(code, "failed")

  // Three scored, two arithmetic failures, one off-window, one without a row.
  private val table = get(
    QueryTable.of(
      0,
      Vector("value"),
      CovariateSchema.empty,
      Vector(
        query("a", scored),
        query("b", scored),
        query("c", scored),
        query("d", failed(arithmetic)),
        query("e", failed(offWindow)),
        query("f", failed(arithmetic)),
        query("g", RoleOutcome.NotStored)
      )
    )
  )

  private def run(slot: FactSlot, total: QueryTotal, n: Long) =
    get(Fact.of(slot, FactSource.Run(total), FactValue.Count(n)))
  private def code(c: DiagnosticCode) = get(FactCode.of(c.render))

  test("a query table's totals: eligible, contributing, failed by code, unmatched") {
    assertEquals(
      get(RunFacts.of(table)),
      Vector(
        run(FactSlot.EligibleQueries, QueryTotal.Eligible, 6),
        run(FactSlot.ContributingQueries, QueryTotal.Contributing, 3),
        run(FactSlot.FailedQueries, QueryTotal.Failed, 3),
        run(FactSlot.FailureCause(code(arithmetic)), QueryTotal.Failure(code(arithmetic)), 2),
        run(FactSlot.FailureCause(code(offWindow)), QueryTotal.Failure(code(offWindow)), 1),
        run(FactSlot.UnmatchedQueries, QueryTotal.Unmatched, 1)
      )
    )
    // The failures sum to the failed queries, so the totals make methods facts.
    assert(MethodsFacts.of(get(RunFacts.of(table))).isRight)
    assertEquals(code(offWindow).label, "off-window")
  }

  test(
    "the controls of the compared queries, most first; a query without a reduction has none"
  ) {
    val selected = Map("a" -> 19, "b" -> 19, "c" -> 18, "d" -> 19, "e" -> 18)
    assertEquals(
      get(RunFacts.controls(table, k => selected.get(k.stimulus))),
      Some(
        get(
          Fact.of(
            FactSlot.ControlsPerQuery,
            FactSource.Run(QueryTotal.ControlsPerQuery),
            FactValue.Controls(
              Vector(
                ControlCount(19, 3, None),
                ControlCount(18, 2, None),
                ControlCount(0, 1, None)
              )
            )
          )
        )
      )
    )
    val none = get(
      QueryTable.of(
        0,
        Vector("value"),
        CovariateSchema.empty,
        Vector(query("g", RoleOutcome.NotStored))
      )
    )
    assertEquals(get(RunFacts.controls(none, _ => Some(3))), None)
  }

  // ---------------------------------------------------------------- a stored study

  private val frame                = get(Frame.screen("run", 2, 2))
  private val grid                 = get(Grid.over(frame, 2, 2))
  private def clock(key: StudyKey) =
    ClockId(key.participant + "/" + key.stimulus + "/" + key.phase)
  private def trial(key: StudyKey, xs: Vector[(Double, Double)]) =
    val fixes = xs.zipWithIndex.map { case ((x, y), i) =>
      get(
        Event.Fixation.withoutDispersion(
          get(
            Interval
              .of(clock(key), Instant.micros(i * 1000L), Instant.micros(i * 1000L + 1000L))
          ),
          Pt[Px](x, y),
          1
        )
      )
    }
    Trial(key, (), get(Scanpath.of(frame, clock(key), IArray.from(fixes))))

  // p1 views three items and p2 two, so each p1 query has two controls and
  // each p2 query one.
  private val layout = Vector(
    ("p1", "a", Vector(0.5 -> 0.5, 1.5 -> 0.5), Vector(0.5 -> 0.5, 0.5 -> 1.5)),
    ("p1", "b", Vector(1.5 -> 1.5, 1.5 -> 0.5), Vector(1.5 -> 1.5, 0.5 -> 0.5)),
    ("p1", "c", Vector(0.5 -> 1.5, 0.5 -> 1.5), Vector(0.5 -> 1.5, 1.5 -> 1.5)),
    ("p2", "a", Vector(0.5 -> 0.5, 0.5 -> 0.5), Vector(0.5 -> 0.5, 1.5 -> 0.5)),
    ("p2", "b", Vector(1.5 -> 0.5, 1.5 -> 1.5), Vector(1.5 -> 0.5, 0.5 -> 1.5))
  )
  private val input = StudyInput(
    Trials(layout.flatMap { (p, i, recall, encode) =>
      Vector(trial(StudyKey(p, i, "recall"), recall), trial(StudyKey(p, i, "encode"), encode))
    })
  )
  private val plan = get(
    StudyPlan.cosine(
      input.reference,
      grid,
      "recall",
      "encode",
      Weight.Duration,
      Vector(StudyEstimate.Binned()),
      FailurePolicy.RequireAll
    )
  )
  private val result  = get(plan.run(input))
  private val binding = ReportBinding(
    get(BindingDigest.parse("plan", "a" * 64)),
    get(BindingDigest.parse("input", "b" * 64)),
    get(BindingDigest.parse("result", "c" * 64)),
    None
  )

  test(
    "a completed study: every query contributes, with its participant's other items as controls"
  ) {
    val source = get(ReportSource.study(plan, input, result, None, binding))
    val facts  = get(RunFacts.study(result, get(source.queries(0))))
    assertEquals(
      facts.map(f => (f.slot, f.value)),
      Vector(
        FactSlot.EligibleQueries     -> FactValue.Count(5),
        FactSlot.ContributingQueries -> FactValue.Count(5),
        FactSlot.FailedQueries       -> FactValue.Count(0),
        FactSlot.UnmatchedQueries    -> FactValue.Count(0),
        FactSlot.ControlsPerQuery    -> FactValue.Controls(
          Vector(ControlCount(2, 3, None), ControlCount(1, 2, None))
        )
      )
    )
  }

  test("run and report facts of one study make one methods text") {
    val source = get(ReportSource.study(plan, input, result, None, binding))
    val report = get(
      Report.evaluate(
        get(
          ReportSpec.of(
            get(ReportId.of("all")),
            0,
            get(ReportSelection.of(Vector(Role.Difference), Vector("value")))
          )
        ),
        source
      )
    )
    val facts = get(
      MethodsFacts.of(
        get(RunFacts.study(result, get(source.queries(0)))) ++
          get(ReportFacts.of(report, "all queries"))
      )
    )
    val text   = StudyText.methods(plan, facts)
    val design = text.clauses.filter(_.topic == ClauseTopic.Design).map(_.text)
    assertEquals(
      design,
      Vector(
        "5 queries were eligible; 5 contributed, 0 failed and 0 had no admitted matched trial.",
        "Of the compared queries, 3 had 2 controls and 2 had 1 control."
      )
    )
    val reporting = text.clauses.filter(_.topic == ClauseTopic.Reporting).map(_.text)
    assertEquals(
      reporting,
      Vector(
        "Results were reported by all queries.",
        "Per participant, groups held 2–3 queries.",
        "1 participant-group cell had the fewest queries (p2 · all · 2 queries)."
      )
    )
    val stated = text.tokens.collect { case t: Token.Fact => t.slot }.toSet
    assertEquals(stated, facts.facts.map(_.slot).toSet)
  }
