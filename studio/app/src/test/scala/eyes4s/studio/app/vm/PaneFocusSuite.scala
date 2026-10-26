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

package eyes4s.studio.app.vm

import eyes4s.studio.app.StoryModels
import eyes4s.studio.app.compare.{
  CompareSummary,
  CompareSummaryVM,
  ExplainVM,
  ParticipantsNavigatorVM,
  ScaleChoice
}
import munit.FunSuite

class PaneFocusSuite extends FunSuite:
  private val model            = StoryModels.t3Summary
  private def pane(id: String) = model.layout.panes.find(_.id.value == id).get
  private val summary          = CompareSummaryVM.of(CompareSummary.empty, model)
  private val nav              = ParticipantsNavigatorVM("", "", None, Vector.empty)

  test(
    "a served plot replaces its container while an unread plot keeps a reachable placeholder"
  ) {
    val decl   = pane("compare.participant-plot")
    val spoken = FocusStop(A11yRole.Region, "Served plot, two groups; selected participant")
    val focus  = PaneFocus.served(decl, Vector(spoken))
    val order  = A11y.tabOrder(model, focus = id => Option.when(id == decl.id)(focus))
    assertEquals(order.count(_ == spoken), 1)
    assert(!order.contains(A11y.pane(decl)))
    assertEquals(focus.container, None)
    assertEquals(PaneFocus.served(decl, Vector.empty).stops, Vector(A11y.pane(decl)))
  }

  test("participant table keeps the available selected scale, Explain and its served table") {
    val decl  = pane("compare.participant-table")
    val table = FocusStop(A11yRole.Table, "Participant summary: served rows")
    val vm    = summary.copy(
      scales = Vector(ScaleChoice(StoryModels.sigma2, "sigma two", true, true, None)),
      explain = Some(ExplainVM("Explain participant", "keeps the grouping", Vector.empty))
    )
    val plan = ResultsFocus.pane(decl, vm, nav, Vector(table))
    assertEquals(plan.container, None)
    assertEquals(
      plan.stops,
      Vector(
        FocusStop(A11yRole.ToggleButton, "sigma two"),
        FocusStop(A11yRole.Button, "Explain participant keeps the grouping"),
        table
      )
    )
    val pending = vm.copy(scales = vm.scales.map(_.copy(available = false)), explain = None)
    assertEquals(ResultsFocus.pane(decl, pending, nav, Vector(table)).stops, Vector(table))
  }

  test("the navigator retains exactly one composite stop while its values are pending") {
    val decl = pane("compare.participants")
    val plan = ResultsFocus.pane(decl, summary, nav, Vector.empty)
    assertEquals(plan.stops, Vector(FocusStop(A11yRole.List, decl.accessibleName)))
    assert(plan.container.nonEmpty)
  }
