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

package eyes4s.studio.app.layout

import eyes4s.studio.app.text.{MessageId, Messages}
import eyes4s.studio.app.vm.Menus
import eyes4s.studio.app.{AppModel, Intent, StoryModels}

/** Every plot has a Table twin (ticket S10.5; DESIGN_SPEC §2, §10): in every
  * layout of every perspective, each Plot pane has a Table sibling in its
  * own group, its keyboard equivalent, and the tab menu offers "Show table"
  * on the plot and "Show plot" on the table. Each exception is named.
  */
class TableTwinSuite extends munit.FunSuite:

  /** Plots without a twin in a layout, and why. */
  private val exceptions: Map[(String, String), String] = Map(
    ("explore", "explore.small-multiples") ->
      "S6.7 (Explore small multiples) is not built: the pane is a placeholder; its Table twin comes with it",
    ("compare.query", "compare.scale-profile") ->
      "the board (Main.dc.html) shows the scale profile's table in Summary only; open for the lead (S10.5 F3)"
  )

  private def excepted(layout: PerspectiveLayout, decl: PaneDecl): Boolean =
    exceptions.contains((layout.id.value, decl.id.value))

  private val plots: Vector[(PerspectiveLayout, PaneDecl)] =
    for
      layout <- StudioLayouts.spec.all
      decl   <- layout.panes if decl.kind == PaneKind.Plot
    yield (layout, decl)

  test("every plot pane has a Table twin in its own group") {
    assert(plots.size >= 9, plots.map(_._2.id.value))
    val missing = plots.collect {
      case (layout, decl)
          if StudioLayouts.twin(layout, decl.id).isEmpty && !excepted(layout, decl) =>
        s"${layout.id}: ${decl.id.value}"
    }
    assertEquals(missing, Vector.empty[String])
    plots.foreach { (layout, decl) =>
      StudioLayouts.twin(layout, decl.id).foreach { table =>
        assertEquals(table.kind, PaneKind.Table, decl.id.value)
        // The relation is mutual.
        assertEquals(
          StudioLayouts.twin(layout, table.id).map(_.id),
          Some(decl.id),
          table.id.value
        )
      }
    }
    // An exception that gains a twin must leave the list.
    exceptions.keys.foreach((layout, id) =>
      assert(
        plots.exists((l, d) =>
          l.id.value == layout && d.id.value == id && StudioLayouts.twin(l, d.id).isEmpty
        ),
        s"$layout: $id"
      )
    )
  }

  test("a twin is a sibling in the same group, by convention or by name, never across groups") {
    import cats.data.NonEmptyVector
    def decl(id: String, kind: PaneKind) = PaneDecl(new PaneId(id), PaneTitle.Fixed(id), kind)
    def grp(ds: PaneDecl*)               =
      LayoutNode.Group(NonEmptyVector.fromVectorUnsafe(ds.toVector), 0, false)
    val (a, aTable) = (decl("t.a", PaneKind.Plot), decl("t.a.table", PaneKind.Table))
    val (b, bTable) = (decl("t.b", PaneKind.Plot), decl("t.b.table", PaneKind.Table))
    val root        = LayoutNode.Split(
      Axis.Horizontal,
      NonEmptyVector.of(grp(a, aTable, b) -> 0.5, grp(bTable) -> 0.5)
    )
    val layout = PerspectiveLayout(
      new LayoutId("t"),
      eyes4s.studio.core.document.Perspective.Explore,
      root,
      eyes4s.studio.app.text.MessageId.TabShowTable
    )
    assertEquals(StudioLayouts.twin(layout, a.id), Some(aTable))
    assertEquals(StudioLayouts.twin(layout, aTable.id), Some(a))
    // b's table is in another group: not a twin, either way.
    assertEquals(StudioLayouts.twin(layout, b.id), None)
    assertEquals(StudioLayouts.twin(layout, bTable.id), None)
    // The scale ladder's table is the Pairs table, by name.
    val compare = StudioLayouts.spec.layout(new LayoutId("compare.query")).get
    assertEquals(
      StudioLayouts.twin(compare, new PaneId("compare.contrast")).map(_.id.value),
      Some("compare.pairs")
    )
  }

  test("the tab menu offers Show table on a plot and Show plot on its table, both ways") {
    val m: AppModel = StoryModels.t2Compare
    val layout      = m.layout
    val shown       = layout.panes.filter(_.kind == PaneKind.Plot)
    assert(shown.exists(_.id.value == "compare.contrast"), shown)
    shown.foreach { plot =>
      StudioLayouts.twin(layout, plot.id).foreach { table =>
        val onPlot  = Menus.tab(m, plot.id).map(a => (a.label, a.intent))
        val onTable = Menus.tab(m, table.id).map(a => (a.label, a.intent))
        assert(
          onPlot
            .contains((Messages.english(MessageId.TabShowTable), Intent.FocusPane(table.id))),
          s"${plot.id.value}: $onPlot"
        )
        assert(
          onTable
            .contains((Messages.english(MessageId.TabShowPlot), Intent.FocusPane(plot.id))),
          s"${table.id.value}: $onTable"
        )
      }
    }
  }
