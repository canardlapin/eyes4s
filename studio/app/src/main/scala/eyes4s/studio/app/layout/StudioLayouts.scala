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

import eyes4s.studio.app.text.{RecordText, RecordTextId}
import cats.data.NonEmptyVector
import eyes4s.studio.app.nav.Place
import eyes4s.studio.app.text.MessageId
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.core.selection.StudioRef

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
    PaneDecl(PaneId(id), PaneTitle.Fixed(title), kind)

  private def dynamic(id: String, generic: String, kind: PaneKind): PaneDecl =
    PaneDecl(PaneId(id), PaneTitle.Dynamic(generic), kind)

  private def table(of: String): PaneDecl = pane(s"$of.table", "Table", Table)

  private def group(first: PaneDecl, rest: PaneDecl*): LayoutNode =
    LayoutNode.Group(NonEmptyVector(first, rest.toVector), 0, navigator = false)

  private def navigator(first: PaneDecl, rest: PaneDecl*): LayoutNode =
    LayoutNode.Group(NonEmptyVector(first, rest.toVector), 0, navigator = true)

  /** A group that opens minimized, a rail at its edge. */
  private def rail(first: PaneDecl, rest: PaneDecl*): LayoutNode =
    LayoutNode.Group(
      NonEmptyVector(first, rest.toVector),
      0,
      navigator = false,
      collapsed = true
    )

  /** Shares of the space along `axis`. */
  private def split(axis: Axis, first: (LayoutNode, Double), rest: (LayoutNode, Double)*) =
    LayoutNode.Split(
      axis,
      NonEmptyVector(first, rest.toVector).map((n, w) => (n, CellSize.Share(w)))
    )

  /** Panes stacked top to bottom, by the golden's heights (any unit). */
  private def stack(first: (LayoutNode, Double), rest: (LayoutNode, Double)*) =
    split(Axis.Vertical, first, rest*)

  /** The v4 golden's three columns: the navigator and the right column at
    * fixed widths in pixels, the centre taking the rest of the window.
    */
  private def columns(
      nav: (LayoutNode, Double),
      centre: LayoutNode,
      right: (LayoutNode, Double)
  ): LayoutNode =
    LayoutNode.Split(
      Axis.Horizontal,
      NonEmptyVector.of(
        nav._1   -> CellSize.Pixels(nav._2),
        centre   -> CellSize.Share(1.0),
        right._1 -> CellSize.Pixels(right._2)
      )
    )

  /** The navigator column (248 px in the golden) and the inspector (300 px). */
  private val NavPx       = 248.0
  private val InspectorPx = 300.0

  private def layout(id: String, perspective: Perspective, root: LayoutNode, hint: MessageId) =
    PerspectiveLayout(LayoutId(id), perspective, root, hint)

  // --- Data -------------------------------------------------------------------

  /** The pane that hosts the Sources navigator: sources, displays, repairs (S5.7). */
  val sources: PaneId               = PaneId("data.sources")
  private val sourcesPane: PaneDecl = PaneDecl(sources, PaneTitle.Fixed("Sources"), Navigator)

  /** The pane that hosts the import wizard on the selected dataset revision. */
  val columnMapping: PaneId = PaneId("data.column-mapping")

  /** The selected import draft's geometry, coordinated with column mapping. */
  val dataGeometry: PaneId = PaneId("data.geometry")

  /** The trial metadata and issue pages of that same selected import draft. */
  val trialMetadata: PaneId = PaneId("data.trial-metadata")
  val dataIssues: PaneId    = PaneId("data.issues")

  /** The pane that hosts the admission ledger of the selected revision. */
  val admission: PaneId = PaneId("data.admission")

  /** Trials whose source records a geometry count opens. */
  val outsideFrame: PaneId = PaneId("data.outside-frame")

  /** The Figures perspective's methods.md text and its diff (S9.4). */
  val methods: PaneId     = PaneId("figures.methods")
  val methodsDiff: PaneId = PaneId("figures.methods-diff")

  /** The panes that host Compare's Queries and Items navigators (S8.1). */
  val compareQueries: PaneId = PaneId("compare.queries")
  val compareItems: PaneId   = PaneId("compare.items")

  /** The pane that hosts Results' pair (S8.2; overhaul v4): the query's and
    * the reference's stages side by side, their legend and the control strip.
    */
  val pair: PaneId = PaneId("compare.pair")

  /** The pane that hosts Compare's scale ladder and contrast readout (S8.3). */
  val contrast: PaneId = PaneId("compare.contrast")

  /** The Results summary's panes: the Δ strip, the slopegraphs and the
    * participant table.
    */
  val participants: PaneId     = PaneId("compare.participants")
  val participantPlot: PaneId  = PaneId("compare.participant-plot")
  val participantTable: PaneId = PaneId("compare.participant-table")
  val reporting: PaneId        = PaneId("compare.reporting")

  /** The pane that hosts Compare's why-this-reference inspector (S8.4). */
  val compareInspector: PaneId = PaneId("compare.inspector")

  /** The pane that hosts Compare's reporting editor (S8.7). */
  val compareReporting: PaneId = PaneId("compare.reporting")

  val firstRunImport: PaneId    = PaneId("data.import")
  val firstRunChecklist: PaneId = PaneId("data.checklist")

  /** DataEmpty.dc.html: no dataset yet. */
  val dataFirstRun: PerspectiveLayout = layout(
    "data.first-run",
    Perspective.Data,
    columns(
      navigator(sourcesPane) -> NavPx,
      group(pane("data.import", "Import", Start)),
      group(pane("data.checklist", "Checklist", Inspector)) -> InspectorPx
    ),
    MessageId.HintDataFirstRun
  )

  /** Data.dc.html: verify an import. The board's columns: Sources 252 px and
    * Geometry 372 px, which the geometry panel's two thumbnails and its counts
    * need.
    */
  val dataVerify: PerspectiveLayout = layout(
    "data.verify",
    Perspective.Data,
    columns(
      navigator(sourcesPane) -> NavPx,
      // One page at a time in the centre (the golden's): Column mapping
      // first, then Admission, which a trail at Admission brings forward.
      group(
        PaneDecl(columnMapping, PaneTitle.Fixed("Column mapping"), Form),
        PaneDecl(admission, PaneTitle.Fixed("Admission"), Form),
        PaneDecl(trialMetadata, PaneTitle.Fixed("Trial metadata"), Form),
        PaneDecl(dataIssues, PaneTitle.Fixed("Data issues"), Form),
        pane("data.outside-frame", "Records outside frame", Table)
      ),
      // 372 px until Port 8 fits the geometry form to the golden's 300.
      group(PaneDecl(dataGeometry, PaneTitle.Fixed("Geometry"), Form)) -> 372.0
    ),
    MessageId.HintDataVerify
  )

  // --- Explore ------------------------------------------------------------------

  /** The panes that host the trials navigator (S6.1): its tree, and its items. */
  val trials: PaneId = PaneId("explore.trials")
  val items: PaneId  = PaneId("explore.items")

  /** The pane that hosts Explore's trial view (S6.2). */
  val trialView: PaneId = PaneId("explore.trial-view")

  /** The pane that hosts Explore's source records table (S6.4). */
  val sourceRecords: PaneId = PaneId("explore.source-records")

  /** The pane that hosts Explore's fixation inspector (S6.5). */
  val exploreInspector: PaneId = PaneId("explore.inspector")

  /** The panes that host Explore's timeline and its table (S6.3). */
  val timeline: PaneId      = PaneId("explore.timeline")
  val timelineTable: PaneId = PaneId("explore.timeline.table")

  /** The Results drill's navigator (overhaul v4): the trials of the pair. */
  val pairTrials: PaneId = PaneId("compare.pair-trials")

  /** The drill's centre and inspector, shared by Results' drill and Data ›
    * Trials (one live view each).
    */
  private def drillColumns(nav: LayoutNode) =
    columns(
      nav -> NavPx,
      // The golden's heights: the stage 440, the timeline 102, records 248.
      stack(
        group(
          PaneDecl(trialView, PaneTitle.Dynamic("Trial view"), Plot),
          table("explore.trial-view")
        ) -> 440.0,
        group(
          PaneDecl(timeline, PaneTitle.Fixed("Timeline"), Plot),
          PaneDecl(timelineTable, PaneTitle.Fixed("Table"), Table)
        ) -> 102.0,
        // The golden's records pane alone: small multiples and the trial
        // inventory were placeholders the overhaul drops.
        group(
          pane("explore.source-records", "Source records", Table)
            .copy(described = Some(RecordText(RecordTextId.TableName)))
        ) -> 248.0
      ),
      group(
        PaneDecl(exploreInspector, PaneTitle.Dynamic("Inspector"), Inspector)
      ) -> InspectorPx
    )

  /** data-trials.html (overhaul v4): participants → trials, a trial's stage,
    * timeline and records, no run's results.
    */
  val dataTrials: PerspectiveLayout = layout(
    "data.trials",
    Perspective.Data,
    // The golden's navigator is the trials tree; the Items pane stays beside
    // it, so a matched item can still be browsed.
    drillColumns(
      navigator(
        PaneDecl(trials, PaneTitle.Fixed("Trials"), Navigator),
        PaneDecl(items, PaneTitle.Fixed("Items"), Navigator)
      )
    ),
    MessageId.HintDataTrials
  )

  /** Explore.dc.html; explore.html (overhaul v4): the Results drill. */
  val explore: PerspectiveLayout = layout(
    "explore",
    Perspective.Results,
    drillColumns(
      navigator(PaneDecl(pairTrials, PaneTitle.Fixed("Trials in this pair"), Navigator))
    ),
    MessageId.HintExplore
  )

  // --- Analysis -----------------------------------------------------------------

  /** Where the failed jobs chip leads (S1.4). */
  val diagnostics: PaneId = PaneId("analysis.diagnostics")

  /** The preset and first-analysis controls. */
  val recipe: PaneId            = PaneId("analysis.recipe")
  val recipeDiff: PaneId        = PaneId("analysis.diff")
  val recipeDescription: PaneId = PaneId("analysis.description")

  /** The resolved-design table (S7.5). */
  val resolvedDesign: PaneId = PaneId("analysis.resolved-design")

  /** The pane that hosts the preflight findings and the run card (S7.6). */
  val preflight: PaneId = PaneId("analysis.preflight")

  val analyses: PaneId = PaneId("analysis.analyses")

  /** Analysis.dc.html. */
  val analysis: PerspectiveLayout = layout(
    "analysis",
    Perspective.Analysis,
    columns(
      navigator(PaneDecl(analyses, PaneTitle.Fixed("Analyses"), Navigator)) -> NavPx,
      // The golden's heights: the recipe 459, the resolved design 332.
      stack(
        group(
          dynamic(recipe.value, "Recipe", Form),
          dynamic(recipeDiff.value, "Diff", Text),
          pane(recipeDescription.value, "Description", Form)
        ) -> 459.0,
        group(
          // A region (the board's section) holding the table's own focus stop.
          PaneDecl(resolvedDesign, PaneTitle.Fixed("Resolved design"), Form),
          table("analysis.resolved-design")
        ) -> 332.0
      ),
      group(
        pane("analysis.preflight", "Preflight", Inspector),
        PaneDecl(diagnostics, PaneTitle.Fixed("Diagnostics"), Table)
      ) -> InspectorPx
    ),
    MessageId.HintAnalysis
  )

  // --- Compare ------------------------------------------------------------------

  /** Results.dc.html: study → participants. */
  val compareSummary: PerspectiveLayout = layout(
    "compare.summary",
    Perspective.Results,
    columns(
      navigator(pane("compare.participants", "Participants", Navigator)) -> 232.0,
      // The golden's heights: the plot 412, the participant table 379. The
      // slopegraphs carry Δ by σ, so the summary has no scale profile; a
      // participant's queries are the drill's (overhaul v4).
      stack(
        group(
          dynamic("compare.participant-plot", "Participant plot", Plot),
          table("compare.participant-plot")
        )                                                                  -> 412.0,
        group(dynamic("compare.participant-table", "Participants", Table)) -> 379.0
      ),
      // The inspector is closed by default here (overhaul decision 5).
      rail(pane("compare.reporting", "Reporting", Inspector)) -> InspectorPx
    ),
    MessageId.HintCompareSummary
  )

  /** Main.dc.html: query → pair → maps. */
  val compareQuery: PerspectiveLayout = layout(
    "compare.query",
    Perspective.Results,
    columns(
      navigator(
        pane("compare.queries", "Queries", Navigator),
        pane("compare.items", "Items", Navigator)
      ) -> 264.0,
      // The golden's heights: the pair 421, across scales 370.
      stack(
        group(PaneDecl(pair, PaneTitle.Fixed("Pair"), Plot), table(pair.value)) -> 421.0,
        // Across scales and its Table (the pairs and scores by scale); the
        // scale profile is the summary's.
        group(
          pane("compare.contrast", "Across scales", Plot),
          pane("compare.pairs", "Pairs table", Table)
        ) -> 370.0
      ),
      group(pane("compare.inspector", "Inspector", Inspector)) -> InspectorPx
    ),
    MessageId.HintCompareQuery
  )

  // --- Figures ------------------------------------------------------------------

  /** Figures.dc.html. */
  val figures: PerspectiveLayout = layout(
    "figures",
    Perspective.Report,
    columns(
      navigator(pane("figures.figures", "Figures", Navigator)) -> NavPx,
      // The golden's Report: the figure page alone; Methods is its own page.
      group(
        dynamic("figures.page", "Figure", Plot),
        table("figures.page"),
        pane(methods.value, "Methods", Text),
        pane(methodsDiff.value, "Diff vs generated", Text)
      ),
      // The panel inspector is closed by default (overhaul decision 5).
      rail(dynamic("figures.panel", "Panel", Inspector)) -> InspectorPx
    ),
    MessageId.HintFigures
  )

  /** Every perspective, in switcher order, with its default layout first. */
  /** Plots whose Table twin is not their `<id>.table` (S10.5): the scale
    * ladder's table is the Pairs table, its pairs and scores by scale.
    */
  private val namedTwins: Map[String, String] = Map("compare.contrast" -> "compare.pairs")

  /** The id of `plot`'s Table twin, by convention or by name, in any layout. */
  def twinTable(plot: String): String = namedTwins.getOrElse(plot, s"$plot.table")

  /** The plot whose Table twin is `table`, if `table` is one. */
  def twinPlot(table: String): Option[String] =
    namedTwins
      .collectFirst { case (plot, t) if t == table => plot }
      .orElse(Option.when(table.endsWith(".table"))(table.stripSuffix(".table")))

  /** The Table twin of plot `plot`, or the plot of table `table`, in
    * `layout`: `<id>.table` by convention, else the named pairs above. Only
    * a sibling in the same group is a twin.
    */
  def twin(layout: PerspectiveLayout, pane: PaneId): Option[PaneDecl] =
    layout.pane(pane).flatMap { decl =>
      val wanted: Option[String] = decl.kind match
        case PaneKind.Plot  => Some(namedTwins.getOrElse(pane.value, s"${pane.value}.table"))
        case PaneKind.Table =>
          namedTwins
            .collectFirst { case (plot, table) if table == pane.value => plot }
            .orElse(
              Option.when(pane.value.endsWith(".table"))(pane.value.stripSuffix(".table"))
            )
        case _ => None
      for
        id    <- wanted
        group <- layout.groupOf(pane)
        other <- group.panes.find(_.id.value == id)
      yield other
    }

  val spec: LayoutSpec = LayoutSpec(
    Vector(
      Perspective.Data     -> NonEmptyVector.of(dataVerify, dataFirstRun, dataTrials),
      Perspective.Analysis -> NonEmptyVector.one(analysis),
      Perspective.Results  -> NonEmptyVector.of(compareQuery, compareSummary, explore),
      Perspective.Report   -> NonEmptyVector.one(figures)
    )
  )

  /** Results shows the summary layout at the Summary crumb, the drill (a
    * trial, its fixations and records: Explore before v4) once the trail
    * reaches a trial, fixation or record, or a participant reached outside a
    * summary, and the pair layout at any other crumb. With no trail there is
    * no summary yet (Results' root trail is the summary once one exists), so
    * it shows the drill's trial browser.
    */
  def compareLayout(trail: Vector[Place]): CompareLayout =
    val fromSummary = trail.exists {
      case Place.Summary(_) | Place.Group(_, _) => true
      case _                                    => false
    }
    trail.lastOption match
      case None                   => CompareLayout.Drill
      case Some(Place.Summary(_)) => CompareLayout.Summary
      case Some(Place.At(ref))    =>
        ref match
          case StudioRef.Trial(_) | StudioRef.Fixation(_, _) |
              StudioRef.SourceRecord(_, _, _, _) | StudioRef.TrialGroup(_, _) =>
            CompareLayout.Drill
          case StudioRef.Participant(_) if !fromSummary => CompareLayout.Drill
          case _                                        => CompareLayout.Query
      case _ => CompareLayout.Query

  /** Whether `perspective` at `trail` shows a trial drill: Results' drill
    * below a pair, or Data › Trials (overhaul v4).
    */
  def drilling(perspective: Perspective, trail: Vector[Place]): Boolean =
    (perspective == Perspective.Results && compareLayout(trail) == CompareLayout.Drill) ||
      (perspective == Perspective.Data && dataTrialsAt(trail))

  /** Whether a Data trail is in Data › Trials. */
  def dataTrialsAt(trail: Vector[Place]): Boolean =
    trail.contains(Place.DataView(eyes4s.studio.app.nav.DataSection.Trials))

  /** The layout a perspective shows: Data's first-run layout until a dataset
    * exists, and Compare's by trail depth.
    */
  def layoutFor(
      perspective: Perspective,
      trail: Vector[Place],
      hasData: Boolean
  ): PerspectiveLayout =
    perspective match
      case Perspective.Data =>
        if !hasData then dataFirstRun
        else if dataTrialsAt(trail) then dataTrials
        else dataVerify
      case Perspective.Analysis => analysis
      case Perspective.Results  =>
        compareLayout(trail) match
          case CompareLayout.Summary => compareSummary
          case CompareLayout.Query   => compareQuery
          case CompareLayout.Drill   => explore
      case Perspective.Report => figures

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
          children.toVector.flatMap { (child, size) =>
            val w = size.value
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
