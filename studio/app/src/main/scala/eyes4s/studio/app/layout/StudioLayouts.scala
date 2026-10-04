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

import cats.data.NonEmptyVector
import eyes4s.studio.app.nav.Place
import eyes4s.studio.app.text.MessageId
import eyes4s.studio.core.document.Perspective

/** The studio's perspectives and default pane layouts, pane for pane as the
  * boards in docs/studio/design/ draw them (LayoutSpecSuite checks every
  * board tab against this declaration).
  *
  * Every perspective has a navigator group on the left; every plot's group
  * has a Table tab (DESIGN_SPEC sections 3 and 10).
  */
object StudioLayouts:
  import PaneKind.*

  // Literal ids, checked through PaneId.of and LayoutId.of by LayoutSpecSuite.
  private def pane(id: String, title: String, kind: PaneKind): PaneDecl =
    PaneDecl(new PaneId(id), PaneTitle.Fixed(title), kind)

  private def dynamic(id: String, generic: String, kind: PaneKind): PaneDecl =
    PaneDecl(new PaneId(id), PaneTitle.Dynamic(generic), kind)

  private def table(of: String): PaneDecl = pane(s"$of.table", "Table", Table)

  private def group(first: PaneDecl, rest: PaneDecl*): LayoutNode =
    LayoutNode.Group(NonEmptyVector(first, rest.toVector), 0, navigator = false)

  private def navigator(first: PaneDecl, rest: PaneDecl*): LayoutNode =
    LayoutNode.Group(NonEmptyVector(first, rest.toVector), 0, navigator = true)

  private def split(axis: Axis, first: (LayoutNode, Double), rest: (LayoutNode, Double)*) =
    LayoutNode.Split(axis, NonEmptyVector(first, rest.toVector))

  /** Navigator | centre | right, the shell's three columns. */
  private def columns(nav: LayoutNode, centre: LayoutNode, right: LayoutNode): LayoutNode =
    split(Axis.Horizontal, nav -> 0.18, centre -> 0.6, right -> 0.22)

  private def layout(id: String, perspective: Perspective, root: LayoutNode, hint: MessageId) =
    PerspectiveLayout(new LayoutId(id), perspective, root, hint)

  // --- Data -------------------------------------------------------------------

  private val sources = pane("data.sources", "Sources", Navigator)

  /** The pane that hosts the import wizard on the selected dataset revision. */
  val columnMapping: PaneId = new PaneId("data.column-mapping")

  /** The pane that hosts the admission ledger of the selected revision. */
  val admission: PaneId = new PaneId("data.admission")

  /** DataEmpty.dc.html: no dataset yet. */
  val dataFirstRun: PerspectiveLayout = layout(
    "data.first-run",
    Perspective.Data,
    columns(
      navigator(sources),
      group(pane("data.import", "Import", Start)),
      group(pane("data.checklist", "Checklist", Inspector))
    ),
    MessageId.HintDataFirstRun
  )

  /** Data.dc.html: verify an import. */
  val dataVerify: PerspectiveLayout = layout(
    "data.verify",
    Perspective.Data,
    columns(
      navigator(sources),
      split(
        Axis.Vertical,
        group(
          PaneDecl(columnMapping, PaneTitle.Fixed("Column mapping"), Form),
          pane("data.trial-metadata", "Trial metadata", Table)
        ) -> 0.55,
        group(
          PaneDecl(admission, PaneTitle.Fixed("Admission"), Form),
          pane("data.outside-frame", "Records outside frame", Table)
        ) -> 0.45
      ),
      group(pane("data.geometry", "Geometry", Form))
    ),
    MessageId.HintDataVerify
  )

  // --- Explore ------------------------------------------------------------------

  /** The panes that host the trials navigator (S6.1): its tree, and its items. */
  val trials: PaneId = new PaneId("explore.trials")
  val items: PaneId  = new PaneId("explore.items")

  /** Explore.dc.html. */
  val explore: PerspectiveLayout = layout(
    "explore",
    Perspective.Explore,
    columns(
      navigator(
        PaneDecl(trials, PaneTitle.Fixed("Trials"), Navigator),
        PaneDecl(items, PaneTitle.Fixed("Items"), Navigator)
      ),
      split(
        Axis.Vertical,
        group(
          dynamic("explore.trial-view", "Trial view", Plot),
          table("explore.trial-view"),
          pane("explore.small-multiples", "Small multiples", Plot)
        )                                                                            -> 0.55,
        group(pane("explore.timeline", "Timeline", Plot), table("explore.timeline")) -> 0.2,
        group(
          pane("explore.source-records", "Source records", Table),
          dynamic("explore.trial-inventory", "Trial inventory", Table)
        ) -> 0.25
      ),
      group(pane("explore.inspector", "Inspector", Inspector))
    ),
    MessageId.HintExplore
  )

  // --- Analysis -----------------------------------------------------------------

  /** Where the failed jobs chip leads (S1.4). */
  val diagnostics: PaneId = new PaneId("analysis.diagnostics")

  /** The resolved-design table (S7.5). */
  val resolvedDesign: PaneId = new PaneId("analysis.resolved-design")

  /** The pane that hosts the preflight findings and the run card (S7.6). */
  val preflight: PaneId = new PaneId("analysis.preflight")

  /** Analysis.dc.html. */
  val analysis: PerspectiveLayout = layout(
    "analysis",
    Perspective.Analysis,
    columns(
      navigator(pane("analysis.analyses", "Analyses", Navigator)),
      split(
        Axis.Vertical,
        group(
          dynamic("analysis.recipe", "Recipe", Form),
          dynamic("analysis.diff", "Diff", Text),
          pane("analysis.description", "Description", Text)
        ) -> 0.6,
        group(
          // A region (the board's section) holding the table's own focus stop.
          PaneDecl(resolvedDesign, PaneTitle.Fixed("Resolved design"), Form),
          table("analysis.resolved-design")
        ) -> 0.4
      ),
      group(
        pane("analysis.preflight", "Preflight", Inspector),
        PaneDecl(diagnostics, PaneTitle.Fixed("Diagnostics"), Table)
      )
    ),
    MessageId.HintAnalysis
  )

  // --- Compare ------------------------------------------------------------------

  /** Shared by both Compare layouts: one live view. */
  private val scaleProfile = pane("compare.scale-profile", "Scale profile", Plot)

  /** Results.dc.html: study → participants. */
  val compareSummary: PerspectiveLayout = layout(
    "compare.summary",
    Perspective.Compare,
    columns(
      navigator(pane("compare.participants", "Participants", Navigator)),
      split(
        Axis.Vertical,
        split(
          Axis.Horizontal,
          group(
            dynamic("compare.participant-plot", "Participant plot", Plot),
            table("compare.participant-plot")
          )                                                   -> 0.6,
          group(scaleProfile, table("compare.scale-profile")) -> 0.4
        ) -> 0.55,
        group(
          dynamic("compare.participant-table", "Participant table", Table),
          pane("compare.query-table", "Query table", Table)
        ) -> 0.45
      ),
      group(pane("compare.reporting", "Reporting", Inspector))
    ),
    MessageId.HintCompareSummary
  )

  /** Main.dc.html: query → pair → maps. */
  val compareQuery: PerspectiveLayout = layout(
    "compare.query",
    Perspective.Compare,
    columns(
      navigator(
        pane("compare.queries", "Queries", Navigator),
        pane("compare.items", "Items", Navigator)
      ),
      split(
        Axis.Vertical,
        split(
          Axis.Horizontal,
          group(
            dynamic("compare.query-trial", "Query trial", Plot),
            table("compare.query-trial")
          ) -> 0.5,
          group(
            dynamic("compare.reference-trial", "Reference trial", Plot),
            table("compare.reference-trial")
          ) -> 0.5
        ) -> 0.6,
        group(
          pane("compare.contrast", "Contrast", Plot),
          pane("compare.pairs", "Pairs table", Table),
          scaleProfile
        ) -> 0.4
      ),
      group(pane("compare.inspector", "Inspector", Inspector))
    ),
    MessageId.HintCompareQuery
  )

  // --- Figures ------------------------------------------------------------------

  /** Figures.dc.html. */
  val figures: PerspectiveLayout = layout(
    "figures",
    Perspective.Figures,
    columns(
      navigator(pane("figures.figures", "Figures", Navigator)),
      split(
        Axis.Vertical,
        group(dynamic("figures.page", "Figure", Plot), table("figures.page")) -> 0.7,
        group(
          pane("figures.methods", "methods.md", Text),
          pane("figures.methods-diff", "Diff vs generated", Text)
        ) -> 0.3
      ),
      group(dynamic("figures.panel", "Panel", Inspector))
    ),
    MessageId.HintFigures
  )

  /** Every perspective, in switcher order, with its default layout first. */
  val spec: LayoutSpec = LayoutSpec(
    Vector(
      Perspective.Data     -> NonEmptyVector.of(dataVerify, dataFirstRun),
      Perspective.Explore  -> NonEmptyVector.one(explore),
      Perspective.Analysis -> NonEmptyVector.one(analysis),
      Perspective.Compare  -> NonEmptyVector.of(compareQuery, compareSummary),
      Perspective.Figures  -> NonEmptyVector.one(figures)
    )
  )

  /** Compare shows the summary layout at the Summary crumb (or with no
    * trail) and the query layout at any deeper crumb.
    */
  def compareLayout(trail: Vector[Place]): CompareLayout = trail.lastOption match
    case None | Some(Place.Summary(_)) => CompareLayout.Summary
    case _                             => CompareLayout.Query

  /** The layout a perspective shows: Data's first-run layout until a dataset
    * exists, and Compare's by trail depth.
    */
  def layoutFor(
      perspective: Perspective,
      trail: Vector[Place],
      hasData: Boolean
  ): PerspectiveLayout =
    perspective match
      case Perspective.Data     => if hasData then dataVerify else dataFirstRun
      case Perspective.Explore  => explore
      case Perspective.Analysis => analysis
      case Perspective.Compare  =>
        compareLayout(trail) match
          case CompareLayout.Summary => compareSummary
          case CompareLayout.Query   => compareQuery
      case Perspective.Figures => figures

  /** Every structural rule a declaration must satisfy; empty when sound. */
  def problems(spec: LayoutSpec): Vector[LayoutError] =
    val missing = Perspective.values.toVector
      .filter(p => spec.layouts(p).isEmpty)
      .map(LayoutError.NoLayouts(_))
    val perLayout = spec.all.flatMap { l =>
      val ids        = l.panes.map(_.id)
      val duplicates =
        ids.diff(ids.distinct).distinct.map(p => LayoutError.DuplicatePane(l.id.value, p.value))
      val badIds =
        (l.id.value +: ids.map(_.value)).filterNot(Ids.valid).map(LayoutError.BadId(_))
      val selections = l.groups.collect {
        case g if g.selected < 0 || g.selected >= g.panes.length =>
          LayoutError.SelectedOutOfRange(g.panes.head.id.value, g.selected, g.panes.length)
      }
      def weights(node: LayoutNode): Vector[LayoutError] = node match
        case LayoutNode.Split(_, children) =>
          children.toVector.flatMap { (child, w) =>
            (if w > 0 && !w.isNaN && !w.isInfinite then Vector.empty
             else Vector(LayoutError.BadWeight(l.id.value, w))) ++ weights(child)
          }
        case _ => Vector.empty
      duplicates ++ badIds ++ selections ++ weights(l.root)
    }
    val conflicting = spec.all
      .flatMap(l => l.panes.map(p => (p, l.id.value)))
      .groupBy(_._1.id)
      .toVector
      .sortBy(_._1.value)
      .collect {
        case (id, uses) if uses.map(_._1).distinct.size > 1 =>
          LayoutError.ConflictingPane(id.value, uses.map(_._2).distinct)
      }
    missing ++ perLayout ++ conflicting
