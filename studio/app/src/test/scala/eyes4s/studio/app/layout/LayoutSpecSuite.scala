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

import eyes4s.studio.app.nav.Place
import eyes4s.studio.app.StoryModels
import eyes4s.studio.core.document.Perspective

/** Every pane tab on the boards is declared, in the boards' order, with the
  * boards' fixed titles (ticket S1.0); the shell rules of DESIGN_SPEC
  * sections 3 and 10 hold for every layout.
  */
class LayoutSpecSuite extends munit.FunSuite:
  import StudioLayouts.*

  /** Each board's tabs in reading order (the docs/studio/design boards,
    * rendered with the boards' default state), with the pane that shows it.
    */
  private val boards: Vector[(String, PerspectiveLayout, Vector[(String, String)])] = Vector(
    (
      "DataEmpty.dc.html",
      dataFirstRun,
      Vector(
        "Sources"   -> "data.sources",
        "Import"    -> "data.import",
        "Checklist" -> "data.checklist"
      )
    ),
    (
      "Data.dc.html",
      dataVerify,
      Vector(
        "Sources"               -> "data.sources",
        "Column mapping"        -> "data.column-mapping",
        "Trial metadata"        -> "data.trial-metadata",
        "Admission"             -> "data.admission",
        "Records outside frame" -> "data.outside-frame",
        "Geometry"              -> "data.geometry"
      )
    ),
    (
      "Explore.dc.html",
      explore,
      Vector(
        "Trials"                   -> "explore.trials",
        "Items"                    -> "explore.items",
        "P17 · enc_03 · beach-042" -> "explore.trial-view",
        "Table"                    -> "explore.trial-view.table",
        "Small multiples"          -> "explore.small-multiples",
        "Timeline"                 -> "explore.timeline",
        "Table"                    -> "explore.timeline.table",
        "Source records"           -> "explore.source-records",
        "trials.csv"               -> "explore.trial-inventory",
        "Inspector"                -> "explore.inspector"
      )
    ),
    (
      "Analysis.dc.html",
      analysis,
      Vector(
        "Analyses"             -> "analysis.analyses",
        "Recipe · Draft rev 5" -> "analysis.recipe",
        "Diff vs rev 4"        -> "analysis.diff",
        "Description"          -> "analysis.description",
        "Resolved design"      -> "analysis.resolved-design",
        "Table"                -> "analysis.resolved-design.table",
        "Preflight"            -> "analysis.preflight",
        "Diagnostics"          -> "analysis.diagnostics"
      )
    ),
    (
      "Main.dc.html",
      compareQuery,
      Vector(
        "Queries"                          -> "compare.queries",
        "Items"                            -> "compare.items",
        "Query P17 · ret_07 · beach-042"   -> "compare.query-trial",
        "Table"                            -> "compare.query-trial.table",
        "Matched P17 · enc_03 · beach-042" -> "compare.reference-trial",
        "Table"                            -> "compare.reference-trial.table",
        "Contrast"                         -> "compare.contrast",
        "Pairs table"                      -> "compare.pairs",
        "Scale profile"                    -> "compare.scale-profile",
        "Inspector"                        -> "compare.inspector"
      )
    ),
    (
      "Results.dc.html",
      compareSummary,
      Vector(
        "Participants"                 -> "compare.participants",
        "Participants · D by response" -> "compare.participant-plot",
        "Table"                        -> "compare.participant-plot.table",
        "Scale profile"                -> "compare.scale-profile",
        "Table"                        -> "compare.scale-profile.table",
        "Participant table · σ 2°"     -> "compare.participant-table",
        "Query table"                  -> "compare.query-table",
        "Reporting"                    -> "compare.reporting"
      )
    ),
    (
      "Figures.dc.html",
      figures,
      Vector(
        "Figures"           -> "figures.figures",
        "Figure 1"          -> "figures.page",
        "Table"             -> "figures.page.table",
        "methods.md"        -> "figures.methods",
        "Diff vs generated" -> "figures.methods-diff",
        "Panel D"           -> "figures.panel"
      )
    )
  )

  test("every board pane is declared, in the board's order, and nothing else is") {
    for (board, layout, tabs) <- boards do
      assertEquals(layout.panes.map(_.id.value), tabs.map(_._2), board)
  }

  test("fixed titles are the boards' words; computed titles are declared dynamic") {
    for
      (board, layout, tabs) <- boards
      (tab, id)             <- tabs
    do
      val pane = layout.panes.find(_.id.value == id).get
      pane.title match
        case PaneTitle.Fixed(words) => assertEquals(words, tab, s"$board $id")
        case PaneTitle.Dynamic(_)   => assertNotEquals(tab, "", s"$board $id")
  }

  test("every board is covered, and every declared layout has a board") {
    assertEquals(boards.map(_._2.id).toSet, spec.all.map(_.id).toSet)
    assertEquals(spec.perspectives.map(_._1), Perspective.values.toVector)
  }

  test("the declaration is sound: ids parse, weights positive, selections in range") {
    assertEquals(problems(spec), Vector.empty)
    for l <- spec.all do
      assertEquals(LayoutId.of(l.id.value), Right(l.id))
      l.panes.foreach(p => assertEquals(PaneId.of(p.id.value), Right(p.id)))
    assertEquals(PaneId.of("Bad Id").left.map(_.message).isLeft, true)
  }

  test("every perspective starts with a navigator group on the left") {
    for l <- spec.all do
      assert(l.groups.head.navigator, l.id)
      assert(l.groups.head.panes.forall(_.kind == PaneKind.Navigator), l.id)
      assertEquals(l.groups.count(_.navigator), 1, l.id)
  }

  test("every plot's group has a Table tab (the keyboard twin)") {
    for
      l <- spec.all
      g <- l.groups
      if g.panes.exists(_.kind == PaneKind.Plot)
    do assert(g.panes.exists(_.kind == PaneKind.Table), s"${l.id} ${g.panes.head.id}")
  }

  test("a pane shared by two layouts is one declaration (one live view per PaneId)") {
    val all = spec.all.flatMap(_.panes)
    all.groupBy(_.id).foreach { (id, decls) =>
      assertEquals(decls.distinct.size, 1, id)
    }
    val shared = compareSummary.panes.map(_.id).intersect(compareQuery.panes.map(_.id))
    assertEquals(shared.map(_.value), Vector("compare.scale-profile"))
  }

  test("Compare: summary layout at the Summary crumb, query layout below it") {
    val t = StoryModels.queryTrail
    assertEquals(compareLayout(Vector.empty), CompareLayout.Summary)
    assertEquals(compareLayout(t.take(1)), CompareLayout.Summary)
    for n <- 2 to t.size do assertEquals(compareLayout(t.take(n)), CompareLayout.Query, n)
    assertEquals(layoutFor(Perspective.Compare, t, hasData = true), compareQuery)
    assertEquals(layoutFor(Perspective.Compare, t.take(1), hasData = true), compareSummary)
  }

  test("Data shows the first-run layout until a dataset exists") {
    assertEquals(
      layoutFor(Perspective.Data, Vector(Place.NewProject), hasData = false),
      dataFirstRun
    )
    assertEquals(layoutFor(Perspective.Data, Vector.empty, hasData = true), dataVerify)
  }

  test("every layout names a status-bar hint and the diagnostics pane is Analysis's") {
    assert(analysis.pane(diagnostics).isDefined)
    assertEquals(spec.all.map(_.hint).distinct.size, spec.all.size)
  }
