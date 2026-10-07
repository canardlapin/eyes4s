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

package eyes4s.studio.app.figures

import cats.syntax.all.*
import eyes4s.studio.app.compare.{CompareSummary, SummaryAnswer}
import eyes4s.studio.app.explore.DisplaySource
import eyes4s.studio.app.layout.StudioLayouts
import eyes4s.studio.app.nav.{Location, Place}
import eyes4s.studio.app.plot.{ParticipantLines, PlotSource}
import eyes4s.studio.app.text.Format
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.backend.{
  AnalysisRevision,
  DatasetRevision,
  DensityGrid,
  RunId,
  ReportView,
  TrialFixations,
  TrialKey
}
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.*
import eyes4s.studio.core.figures.{FigureSource, RebindProposal, ReferenceScores}
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}

/** The journal widths of a figure page (Figures board: "Page 183 mm ·
  * two-column").
  */
enum PageWidth(val mm: Int, val columns: Int) derives CanEqual:
  case SingleColumn extends PageWidth(89, 1)
  case TwoColumn    extends PageWidth(183, 2)

  def label: String = this match
    case SingleColumn => s"$mm mm · one-column"
    case TwoColumn    => s"$mm mm · two-column"

/** The space between panel columns on the page. */
object PageLayout:
  val GutterMm: Int = 5

  /** Each panel's width on a page of `width`: the page's columns share it. */
  def panelMm(width: PageWidth): Int =
    (width.mm - GutterMm * (width.columns - 1)) / width.columns

/** The page zoom: a percentage of the board's 3.5 px per millimetre. */
final case class Zoom private (percent: Int) derives CanEqual:
  def pxPerMm: Double = Zoom.BasePxPerMm * percent / 100.0

  /** "100% · 3.5 px/mm". */
  def label: String = s"$percent% · ${Format.decimal(pxPerMm, 1)} px/mm"

  def in: Zoom  = Zoom.Levels.find(_ > percent).fold(this)(Zoom(_))
  def out: Zoom = Zoom.Levels.findLast(_ < percent).fold(this)(Zoom(_))

object Zoom:
  val BasePxPerMm: Double = 3.5
  val Levels: Vector[Int] = Vector(50, 75, 100, 150, 200)
  val Default: Zoom       = Zoom(100)

/** Which of the page's two tabs is in front. */
enum ComposerTab derives CanEqual:
  case Figure, Table

/** A user action or platform fact the Figures panes dispatch. */
enum ComposerIntent derives CanEqual:
  /** A navigator, stale-notice or rebind action (S9.1). */
  case Binding(intent: FigureIntent)
  case SelectFigure(figure: FigureId)
  case SelectPanel(figure: FigureId, panel: PanelLetter)

  /** "New figure": the latest current run, the first reporting spec, and
    * the two whole-run templates (participant D, scale profile).
    */
  case NewFigure

  /** Begin a figure with one chosen template, letter A. */
  case NewFigureWith(kind: NewPanel)

  /** Add panel (bead bd-01M44PBFHM4CWTXJAKQYMVV7RY): a `kind` panel, last,
    * in the shown figure ([[AddPanel]]), which the trail then shows.
    */
  case AddPanelOf(kind: NewPanel)
  case RemoveSelectedPanel
  case MoveSelectedPanelEarlier
  case MoveSelectedPanelLater
  case SetWidth(width: PageWidth)
  case ZoomIn
  case ZoomOut
  case ShowTab(tab: ComposerTab)

  /** "Open in Compare": the figure's run, at the selected panel. */
  case OpenInCompare

  /** Appearance · view only (S9.2b), of the shown figure. */
  case SetTextSize(size: FigureTextSize)
  case SetParticipantLines(lines: ParticipantLines)

  /** Set a panel's width, within [[FigureAppearance.MinPanelMm]] and the page. */
  case SetPanelWidth(panel: PanelLetter, mm: Int)
  case IncludeImages(include: Boolean)

  /** "Greyscale check": show the page as it prints in greyscale (S9.3). */
  case SetGreyscale(on: Boolean)

  /** Export (S9.3): the format, then "Export figure…"; the platform answers
    * with where the file went, or why it did not.
    */
  case ChooseFormat(format: ExportFormat)
  case Export
  case Exported(answer: Either[String, String])
  case SummaryRead(run: RunId, answer: SummaryAnswer)
  case ReportRead(
      run: RunId,
      spec: ReportingSpec,
      scale: ScaleIndex,
      answer: Either[String, ReportView]
  )
  case ReferencesRead(
      run: RunId,
      scale: ScaleIndex,
      query: TrialKey,
      answer: Either[String, ReferenceScores]
  )
  case DisplaysRead(dataset: DatasetRevision, answer: Either[String, DisplaySource])

  /** A run's density grid for panel C. */
  case MapRead(
      run: RunId,
      scale: ScaleIndex,
      trial: TrialKey,
      answer: Either[String, DensityGrid]
  )

  /** A gaze panel's fixations (S6.2 trialFixations). */
  case FixationsRead(
      revision: AnalysisRevision,
      trial: TrialKey,
      answer: Either[String, TrialFixations]
  )

  /** The methods.md pane (S9.4). */
  case Methods(intent: MethodsIntent)

  /** The export bundle (S9.5): choose its files, export it, its answer. */
  case ToggleBundle(item: BundleItem)
  case ExportBundle
  case BundleExported(answer: Either[String, String])

/** What the composer asks of the app and the platform. */
enum ComposerEffect derives CanEqual:
  case App(intent: Intent)

  /** The figure binding's request (S9.1's `RequestStatus`). */
  case Binding(effect: FigureEffect)

  /** The run's result summary (panels D and E). */
  case RequestSummary(run: RunId)
  case RequestReport(run: RunId, spec: ReportingSpec, scale: ScaleIndex)

  /** A query's contrast, matched pair and control pairs (panel C). */
  case RequestReferences(run: RunId, scale: ScaleIndex, query: TrialKey)

  /** What each trial of the revision displayed (panels A and B). */
  case RequestDisplays(dataset: DatasetRevisionSpec)

  /** Read a gaze panel's fixations under the bound analysis revision. */
  case RequestFixations(revision: AnalysisRevision, trial: TrialKey)

  /** Read one density grid that panel C names. */
  case RequestMap(run: RunId, scale: ScaleIndex, trial: TrialKey)

  /** Export `page` as `format` under the suggested file name `name`. */
  case ExportFigure(format: ExportFormat, page: PageVM, name: String)

  /** Read the admission and query facts the methods text cites (S9.4). */
  case RequestMethods(run: RunId, dataset: DatasetRevision)

  /** Write the export bundle (S9.5). */
  case WriteBundle(request: BundleRequest)

/** One read the composer asked for, so it is asked once. */
enum ComposerRead derives CanEqual:
  case Summary(run: RunId)
  case Report(run: RunId, spec: ReportingSpec, scale: ScaleIndex)
  case References(run: RunId, scale: ScaleIndex, query: TrialKey)
  case Displays(dataset: DatasetRevision)
  case Fixations(revision: AnalysisRevision, trial: TrialKey)
  case Grid(run: RunId, scale: ScaleIndex, trial: TrialKey)
  case Methods(run: RunId)

/** One panel on the page. */
final case class PanelVM(
    letter: PanelLetter,
    title: String,
    selected: Boolean,
    widthMm: Int,
    body: PanelBody,
    ref: StudioRef
) derives CanEqual

/** The inspector's Appearance and Export controls (S9.2b). */
final case class AppearanceVM(
    textSizes: Vector[(FigureTextSize, String, Boolean)],
    lines: Vector[(ParticipantLines, String, Boolean)],
    panelWidthMm: Option[(PanelLetter, Int, String)],
    includeImages: (String, Boolean)
) derives CanEqual

/** The figure formats export writes (Figures board: "Figure format SVG PDF PNG"). */
enum ExportFormat(val label: String, val extension: String) derives CanEqual:
  case Svg extends ExportFormat("SVG", "svg")
  case Pdf extends ExportFormat("PDF", "pdf")
  case Png extends ExportFormat("PNG", "png")

/** The inspector's Export section (S9.3). */
final case class ExportVM(
    formats: Vector[(ExportFormat, String, Boolean)],
    action: String,
    status: Option[String]
) derives CanEqual

/** The page of the shown figure. */
final case class PageVM(
    figure: FigureId,
    title: String,
    width: PageWidth,
    widthLabel: String,
    zoom: String,
    pxPerMm: Double,
    panels: Vector[PanelVM],
    tab: ComposerTab,
    table: Option[Either[String, PlotSource]],
    openInCompare: String,
    caption: String,
    stamp: String,
    textPt: Int,
    appearance: AppearanceVM,
    greyscale: Boolean,
    exporting: ExportVM,
    addPanel: AddPanelVM,
    panelEditing: PanelEditingVM
) derives CanEqual

/** Everything the Figures perspective shows. */
final case class ComposerVM(
    figures: FiguresVM,
    newFigure: String,
    startFigure: AddPanelVM,
    widths: Vector[(PageWidth, String, Boolean)],
    page: Option[PageVM],
    methods: Option[MethodsVM],
    bundle: Option[BundleVM],
    problem: Option[String]
) derives CanEqual

/** The figure composer (ticket S9.2a; Figures.dc.html, page): the Figures
  * navigator with New figure, the page at a journal width and zoom, the
  * panels from their templates ([[PanelTemplate]]), the Table tab of the
  * selected panel and Open in Compare; the figure binding (S9.1) is its
  * own part. The shown figure and panel are the Figures trail's. Every value
  * a panel shows is the backend's, read for the figure's bound run (never
  * the shown or latest one). Pure; a host performs the effects.
  */
final case class FigureComposer private (
    binding: FigureBinding,
    width: PageWidth,
    zoom: Zoom,
    tab: ComposerTab,
    summaries: Map[RunId, SummaryAnswer],
    references: Map[(RunId, ScaleIndex, TrialKey), Either[String, ReferenceScores]],
    displays: Map[DatasetRevision, Either[String, DisplaySource]],
    maps: Map[(RunId, ScaleIndex, TrialKey), Either[String, DensityGrid]],
    asked: Set[ComposerRead],
    problem: Option[String],
    appearance: Map[FigureId, FigureAppearance],
    greyscale: Boolean,
    format: ExportFormat,
    exported: Option[String],
    methods: FigureMethods,
    bundle: Set[BundleItem],
    bundled: Option[String],
    fixations: Map[(AnalysisRevision, TrialKey), Either[String, TrialFixations]],
    reports: Map[(RunId, ReportingSpec, ScaleIndex), Either[String, ReportView]] = Map.empty
) derives CanEqual:
  def appearanceOf(figure: FigureId): FigureAppearance =
    appearance.getOrElse(figure, FigureAppearance.default)

  /** The same composer, asking `read` again at the next sync if it failed. */
  private[figures] def retrying(read: ComposerRead, failed: Boolean): FigureComposer =
    if failed then copy(asked = asked - read) else this

object FigureComposer:
  /** Methods counts and participants.csv follow the first participant panel's declared scale,
    * or the first estimation scale when the figure has no participant panel.
    */
  def reportingScale(source: FigureSource): Either[String, ScaleIndex] =
    source.figure.panels.map(PanelTemplate.of).collectFirst {
      case PanelTemplate.ParticipantD(sigma) => sigma
    } match
      case None        => Right(ScaleIndex.first)
      case Some(sigma) =>
        FigurePanels
          .scaleIndex(source.bound.analysis.recipe.scales, sigma)
          .toRight(ComposerText.notComputed(source.run.id, sigma))

  private def reportingAnswer(
      c: FigureComposer,
      source: FigureSource
  ): Option[Either[String, ReportView]] =
    reportingScale(source) match
      case Left(why)    => Some(Left(why))
      case Right(scale) => c.reports.get((source.run.id, source.reporting, scale))

  val empty: FigureComposer = FigureComposer(
    FigureBinding.empty,
    PageWidth.TwoColumn,
    Zoom.Default,
    ComposerTab.Figure,
    Map.empty,
    Map.empty,
    Map.empty,
    Map.empty,
    Set.empty,
    None,
    Map.empty,
    false,
    ExportFormat.Svg,
    None,
    FigureMethods.empty,
    BundleItem.Default,
    None,
    Map.empty
  )

  private val none: Vector[ComposerEffect] = Vector.empty

  /** The figure the Figures trail is on, else the document's first. */
  def shownFigure(model: AppModel): Option[FigureId] =
    model.navigation
      .trail(Perspective.Figures)
      .collect { case Place.Figure(f) => f }
      .lastOption
      .filter(f => model.document.figures.exists(_.id == f))
      .orElse(model.document.figures.headOption.map(_.id))

  /** The panel the Figures trail is on, in the shown figure. */
  def shownPanel(model: AppModel): Option[PanelLetter] =
    val figure = shownFigure(model)
    model.navigation
      .trail(Perspective.Figures)
      .collect {
        case Place.At(StudioRef.FigurePanel(f, letter)) if figure.contains(f) => letter
      }
      .lastOption

  private def figureTrail(figure: FigureId, panel: Option[PanelLetter]): Location =
    Location(
      Perspective.Figures,
      Vector(Place.Figures, Place.Figure(figure)) ++
        panel.map(l => Place.At(StudioRef.FigurePanel(figure, l)))
    )

  /** The reads the shown figure needs and has not asked for. */
  private def reads(
      c: FigureComposer,
      model: AppModel
  ): Vector[(ComposerRead, ComposerEffect)] =
    shownFigure(model)
      .flatMap(FigureSource.of(model.document, _).toOption)
      .toVector
      .flatMap { s =>
        val run    = s.run.id
        val scales = s.bound.analysis.recipe.scales
        Vector(
          ComposerRead.Summary(run) -> ComposerEffect.RequestSummary(run),
          ComposerRead.Methods(run) -> ComposerEffect.RequestMethods(run, s.bound.dataset.id)
        ) ++
          (for
            i     <- scales.values.indices.toVector
            scale <- ScaleIndex.of(i).toOption.toVector
            spec  <- (Vector(s.reporting) ++ CompareSummary
              .overall(s.reporting)
              .toVector).distinct
          yield ComposerRead
            .Report(run, spec, scale) -> ComposerEffect.RequestReport(run, spec, scale)) ++
          s.figure.panels.flatMap { p =>
            PanelTemplate.of(p) match
              case PanelTemplate.DensityMaps(sigma, query) =>
                FigurePanels.scaleIndex(scales, sigma).toVector.flatMap { i =>
                  val maps = c.references.get((run, i, query)) match
                    case Some(Right(scores)) =>
                      FigurePanels
                        .mapTrials(query, scores)
                        .map(t =>
                          ComposerRead.Grid(run, i, t) -> ComposerEffect.RequestMap(run, i, t)
                        )
                    case _ => Vector.empty
                  (ComposerRead.References(run, i, query) ->
                    ComposerEffect.RequestReferences(run, i, query)) +: maps
                }
              case PanelTemplate.Gaze(trial) =>
                val revision = s.bound.analysis.id
                Vector(
                  ComposerRead.Displays(s.bound.dataset.id) ->
                    ComposerEffect.RequestDisplays(s.bound.dataset),
                  ComposerRead.Fixations(revision, trial) ->
                    ComposerEffect.RequestFixations(revision, trial)
                )
              case _ => Vector.empty
          }
      }
      .distinctBy(_._1)
      .filterNot((r, _) => c.asked.contains(r))

  /** Follows the model: the binding follows the shown figure, and the shown
    * figure's reads are asked for once.
    */
  def sync(c: FigureComposer, model: AppModel): (FigureComposer, Vector[ComposerEffect]) =
    val figure           = shownFigure(model)
    val (binding, bound) =
      if figure.isEmpty || c.binding.selected == figure then (c.binding, Vector.empty)
      else FigureBinding.update(c.binding, model, FigureIntent.Select(figure.get))
    val (asked, next) = asking(c.copy(binding = binding), model)
    (asked, bound.map(lift) ++ next)

  /** Ask every currently-needed read once. A references answer may reveal the
    * three trials panel C must read, so this is also used after that answer.
    */
  private def asking(
      c: FigureComposer,
      model: AppModel
  ): (FigureComposer, Vector[ComposerEffect]) =
    val next = reads(c, model)
    (c.copy(asked = c.asked ++ next.map(_._1)), next.map(_._2))

  private def lift(e: FigureEffect): ComposerEffect = e match
    case FigureEffect.App(i) => ComposerEffect.App(i)
    case other               => ComposerEffect.Binding(other)

  def update(
      c: FigureComposer,
      model: AppModel,
      intent: ComposerIntent
  ): (FigureComposer, Vector[ComposerEffect]) =
    import ComposerIntent.*
    intent match
      case Binding(i) =>
        val (b, effects) = FigureBinding.update(c.binding, model, i)
        val navigate     = i match
          case FigureIntent.Select(f) =>
            Vector(ComposerEffect.App(Intent.Navigate(figureTrail(f, None))))
          case _ => none
        (c.copy(binding = b), effects.map(lift) ++ navigate)
      case SelectFigure(f) =>
        (
          c.copy(problem = None),
          Vector(ComposerEffect.App(Intent.Navigate(figureTrail(f, None))))
        )
      case SelectPanel(f, letter) =>
        (
          c.copy(problem = None),
          Vector(ComposerEffect.App(Intent.Navigate(figureTrail(f, Some(letter)))))
        )
      case NewFigure                => newFigure(c, model)
      case NewFigureWith(kind)      => newFigure(c, model, Some(kind))
      case AddPanelOf(kind)         => addPanel(c, model, kind)
      case RemoveSelectedPanel      => editPanel(c, model, None)
      case MoveSelectedPanelEarlier => editPanel(c, model, Some(-1))
      case MoveSelectedPanelLater   => editPanel(c, model, Some(1))
      case SetWidth(w)              =>
        // A narrower page holds no panel wider than itself.
        val clamped = c.appearance.view
          .mapValues(a => a.copy(widthsMm = a.widthsMm.view.mapValues(_.min(w.mm)).toMap))
          .toMap
        (c.copy(width = w, appearance = clamped), none)
      case ZoomIn        => (c.copy(zoom = c.zoom.in), none)
      case ZoomOut       => (c.copy(zoom = c.zoom.out), none)
      case ShowTab(tab)  => (c.copy(tab = tab), none)
      case OpenInCompare => openInCompare(c, model)
      // A read that failed is asked again at the next sync; its failure
      // shows until then.
      case SummaryRead(r, a) =>
        val failed = a match
          case SummaryAnswer.Answered(_) => false
          case _                         => true
        (
          c.copy(summaries = c.summaries.updated(r, a))
            .retrying(ComposerRead.Summary(r), failed),
          none
        )
      case ReportRead(r, spec, scale, a) =>
        val checked = a.flatMap(view =>
          Either.cond(
            view.run == r && view.reporting == spec.id && view.scale == scale.value,
            view,
            s"The report for ${r.label}, ${spec.id.value}, scale ${scale.value} answered " +
              s"${view.run.label}, ${view.reporting.value}, scale ${view.scale}."
          )
        )
        (
          c.copy(reports = c.reports.updated((r, spec, scale), checked))
            .retrying(ComposerRead.Report(r, spec, scale), checked.isLeft),
          none
        )
      case ReferencesRead(r, s, q, a) =>
        asking(
          c.copy(references = c.references.updated((r, s, q), a))
            .retrying(ComposerRead.References(r, s, q), a.isLeft),
          model
        )
      case MapRead(r, s, t, a) =>
        (
          c.copy(maps = c.maps.updated((r, s, t), a))
            .retrying(ComposerRead.Grid(r, s, t), a.isLeft),
          none
        )
      case FixationsRead(r, t, a) =>
        (
          c.copy(fixations = c.fixations.updated((r, t), a))
            .retrying(ComposerRead.Fixations(r, t), a.isLeft),
          none
        )
      case DisplaysRead(d, a) =>
        (
          c.copy(displays = c.displays.updated(d, a))
            .retrying(ComposerRead.Displays(d), a.isLeft),
          none
        )
      case Methods(i) =>
        val source = shownFigure(model).flatMap(FigureSource.of(model.document, _).toOption)
        val (methods, show) = FigureMethods.update(
          c.methods,
          source,
          source.flatMap(s => c.summaries.get(s.run.id)),
          source.flatMap(s => reportingAnswer(c, s)),
          i
        )
        val next = i match
          case MethodsIntent.FactsRead(r, a) =>
            c.retrying(ComposerRead.Methods(r), a.isLeft)
          case _ => c
        val focus = show.toVector.map {
          case FigureMethods.Show.Text => Intent.FocusPane(StudioLayouts.methods)
          case FigureMethods.Show.Diff => Intent.FocusPane(StudioLayouts.methodsDiff)
        }
        (next.copy(methods = methods), focus.map(ComposerEffect.App(_)))
      case SetTextSize(size)          => (restyle(c, model)(_.copy(text = size)), none)
      case SetParticipantLines(lines) => (restyle(c, model)(_.copy(lines = lines)), none)
      case SetGreyscale(on)           => (c.copy(greyscale = on), none)
      case ChooseFormat(f)            => (c.copy(format = f, exported = None), none)
      case Exported(answer)           =>
        (
          c.copy(exported =
            Some(answer.fold(ComposerText.exportFailed, ComposerText.exportedTo))
          ),
          none
        )
      case ToggleBundle(item) =>
        val next = if c.bundle.contains(item) then c.bundle - item else c.bundle + item
        (c.copy(bundle = next, bundled = None), none)
      case BundleExported(answer) =>
        (c.copy(bundled = Some(FigureBundle.exported(answer))), none)
      case ExportBundle =>
        val v = view(c, model)
        (for
          s      <- shownFigure(model).flatMap(FigureSource.of(model.document, _).toOption)
          page   <- v.page
          bundle <- v.bundle
          written = bundle.written
          if written.nonEmpty
        yield ComposerEffect.WriteBundle(
          BundleRequest(
            s,
            page,
            c.format,
            v.methods.flatMap(_.text.toOption),
            written,
            c.appearanceOf(s.figure.id).includeImages,
            FigureBundle.folder(page),
            bundle.rows.collect {
              case r if r.chosen && r.unavailable.isDefined => r.file -> r.unavailable.get
            },
            reportingScale(s).toOption
          )
        )).fold((c, none))(e => (c.copy(bundled = None), Vector(e)))
      case Export =>
        view(c, model).page.fold((c, none)) { p =>
          (
            c.copy(exported = None),
            Vector(ComposerEffect.ExportFigure(c.format, p, ComposerText.fileName(p, c.format)))
          )
        }
      case IncludeImages(include) => (restyle(c, model)(_.copy(includeImages = include)), none)
      case SetPanelWidth(panel, mm) =>
        val bounded = mm.max(FigureAppearance.MinPanelMm).min(c.width.mm)
        (restyle(c, model)(a => a.copy(widthsMm = a.widthsMm.updated(panel, bounded))), none)

  /** The shown figure's appearance, changed by `f`. */
  private def restyle(c: FigureComposer, model: AppModel)(
      f: FigureAppearance => FigureAppearance
  ): FigureComposer =
    shownFigure(model).fold(c)(fig =>
      c.copy(appearance = c.appearance.updated(fig, f(c.appearanceOf(fig))))
    )

  private def newFigure(
      c: FigureComposer,
      model: AppModel,
      kind: Option[NewPanel] = None
  ): (FigureComposer, Vector[ComposerEffect]) =
    val document = model.document
    val made     = for
      run    <- RebindProposal.target(model.freshness).toRight(ComposerText.NoRun)
      rep    <- document.reporting.headOption.toRight(ComposerText.NoReporting)
      scales <- document
        .analysis(run.analysis)
        .map(_.recipe.scales.values)
        .toRight(ComposerText.NoRun)
      first  <- scales.headOption.toRight(ComposerText.NoRun)
      a      <- PanelLetter.of("A").left.map(_.message)
      b      <- PanelLetter.of("B").left.map(_.message)
      next   <- document.nextFigureId.left.map(_.message)
      panels <- kind match
        case Some(template) =>
          AddPanel.firstSpec(model, run.id, rep.id, template).map(Vector(_))
        case None =>
          Right(
            Vector(
              PanelSpec(
                a,
                s"Participant D, ${first.render}",
                PanelScale.At(first),
                PanelSelection.AllQueries
              ),
              PanelSpec(b, "Scale profile", PanelScale.AllScales, PanelSelection.AllQueries)
            )
          )
    yield (
      next,
      Command.CreateFigure(run.id, rep.id, panels),
      panels.headOption.filter(_ => kind.isDefined).map(_.letter)
    )
    made match
      case Left(why)                       => (c.copy(problem = Some(why)), none)
      case Right((next, create, selected)) =>
        (
          c.copy(problem = None),
          Vector(
            ComposerEffect.App(Intent.Dispatch(create)),
            ComposerEffect.App(
              Intent.Navigate(
                figureTrail(
                  next,
                  selected
                )
              )
            )
          )
        )

  private def addPanel(
      c: FigureComposer,
      model: AppModel,
      kind: NewPanel
  ): (FigureComposer, Vector[ComposerEffect]) =
    val made = for
      id     <- shownFigure(model).toRight(ComposerText.NoFigure)
      figure <- model.document.figures.find(_.id == id).toRight(ComposerText.NoFigure)
      add    <- AddPanel.command(model, figure, shownPanel(model), kind)
    yield add
    made match
      case Left(why)  => (c.copy(problem = Some(why)), none)
      case Right(add) =>
        (
          c.copy(problem = None),
          Vector(
            ComposerEffect.App(Intent.Dispatch(add)),
            ComposerEffect.App(Intent.Navigate(figureTrail(add.figure, Some(add.panel.letter))))
          )
        )

  private def editPanel(
      c: FigureComposer,
      model: AppModel,
      movement: Option[Int]
  ): (FigureComposer, Vector[ComposerEffect]) =
    val selected = for
      id     <- shownFigure(model)
      figure <- model.document.figures.find(_.id == id)
      letter <- shownPanel(model)
      index  <- Option.when(figure.panels.exists(_.letter == letter))(
        figure.panels.indexWhere(_.letter == letter)
      )
    yield (figure, letter, index)
    selected match
      case None => (c.copy(problem = Some(PanelEditing.NoSelection)), none)
      case Some((figure, letter, index)) =>
        movement match
          case None if figure.panels.size == 1 =>
            (c.copy(problem = Some(PanelEditing.LastPanel)), none)
          case None =>
            val remaining = figure.panels.filterNot(_.letter == letter)
            val next      = remaining.lift(index.min(remaining.size - 1)).map(_.letter)
            (
              c.copy(problem = None),
              Vector(
                ComposerEffect.App(Intent.Dispatch(Command.RemovePanel(figure.id, letter))),
                ComposerEffect.App(Intent.Navigate(figureTrail(figure.id, next)))
              )
            )
          case Some(offset) =>
            val target = index + offset
            if target < 0 || target >= figure.panels.size then (c.copy(problem = None), none)
            else
              (
                c.copy(problem = None),
                Vector(
                  ComposerEffect.App(
                    Intent.Dispatch(Command.MovePanel(figure.id, letter, target))
                  )
                )
              )

  private def openInCompare(
      c: FigureComposer,
      model: AppModel
  ): (FigureComposer, Vector[ComposerEffect]) =
    shownFigure(model).map(FigureSource.of(model.document, _)) match
      case None           => (c, none)
      case Some(Left(e))  => (c.copy(problem = Some(e.message)), none)
      case Some(Right(s)) =>
        val run  = s.run.id
        val show = Option
          .when(!model.document.presentation.shownRun.contains(run))(
            ComposerEffect.App(Intent.Dispatch(Command.ShowRun(Some(run))))
          )
          .toVector
        val panel = shownPanel(model).flatMap(l => s.figure.panels.find(_.letter == l))
        val go    = panel.map(PanelTemplate.of) match
          case Some(PanelTemplate.DensityMaps(sigma, query)) =>
            FigurePanels
              .scaleIndex(s.bound.analysis.recipe.scales, sigma)
              .fold(summaryOf(s.reporting.id))(i =>
                Intent.Explain(Place.At(StudioRef.QueryContrast(run, i, query)))
              )
          case Some(PanelTemplate.Gaze(trial)) =>
            Intent.Explain(Place.At(StudioRef.Trial(trial)))
          case _ => summaryOf(s.reporting.id)
        (c.copy(problem = None), show :+ ComposerEffect.App(go))

  private def summaryOf(reporting: ReportingId): Intent =
    Intent.Navigate(Location(Perspective.Compare, Vector(Place.Summary(reporting))))

  // -------------------------------------------------------------------------
  // The view
  // -------------------------------------------------------------------------

  def view(c: FigureComposer, model: AppModel): ComposerVM =
    val source = shownFigure(model).map(f => FigureSource.of(model.document, f))
    val page   = source.map {
      case Left(e)  => Left(e.message)
      case Right(s) => Right(pageOf(c, model, s))
    }
    val methods = source
      .flatMap(_.toOption)
      .map(s =>
        FigureMethods.view(c.methods, s, c.summaries.get(s.run.id), reportingAnswer(c, s))
      )
    val bundle = for
      s <- source.flatMap(_.toOption)
      p <- page.flatMap(_.toOption)
    yield FigureBundle.view(
      c.bundle,
      s,
      p,
      c.format,
      c.summaries.get(s.run.id),
      reportingAnswer(c, s),
      methods,
      c.appearanceOf(s.figure.id).includeImages,
      c.bundled
    )
    ComposerVM(
      FigureBinding.view(c.binding, model),
      "New figure",
      AddPanel.firstChoices(model),
      PageWidth.values.toVector.map(w => (w, w.label, w == c.width)),
      page.flatMap(_.toOption),
      methods,
      bundle,
      c.problem.orElse(page.flatMap(_.left.toOption))
    )

  private def pageOf(c: FigureComposer, model: AppModel, s: FigureSource): PageVM =
    val selected                     = shownPanel(model)
    val look                         = c.appearanceOf(s.figure.id)
    def widthOf(letter: PanelLetter) =
      look.widthsMm.getOrElse(letter, PageLayout.panelMm(c.width)).min(c.width.mm)
    val panels = s.figure.panels.map { p =>
      PanelVM(
        p.letter,
        p.title,
        selected.contains(p.letter),
        widthOf(p.letter),
        body(c, s, PanelTemplate.of(p)),
        StudioRef.FigurePanel(s.figure.id, p.letter)
      )
    }
    val appearance = AppearanceVM(
      FigureTextSize.values.toVector.map(t => (t, t.label, t == look.text)),
      ParticipantLines.values.toVector.map(l => (l, l.label, l == look.lines)),
      selected.map(l => (l, widthOf(l), s"${widthOf(l)} mm")),
      ("project snapshot includes images", look.includeImages)
    )
    PageVM(
      s.figure.id,
      s.figure.id.label,
      c.width,
      ComposerText.page(c.width),
      c.zoom.label,
      c.zoom.pxPerMm,
      panels,
      c.tab,
      selected.flatMap(l => s.figure.panels.find(_.letter == l)).map(p => table(c, s, p)),
      "Open in Compare",
      FigureCaption.figure(s),
      FigureCaption.stamp(s),
      look.text.pt,
      appearance,
      c.greyscale,
      ExportVM(
        ExportFormat.values.toVector.map(f => (f, f.label, f == c.format)),
        "Export figure…",
        c.exported
      ),
      AddPanel.view(model, s.figure, selected),
      PanelEditing.view(s.figure, selected)
    )

  private def summaryOf(c: FigureComposer, run: RunId) =
    c.summaries.get(run) match
      case None                            => Left(PanelBody.Waiting(ComposerText.reading(run)))
      case Some(SummaryAnswer.Answered(r)) =>
        Either.cond(
          r.run == run,
          r,
          PanelBody.Unavailable(s"The summary of ${run.label} answered ${r.run.label}.")
        )
      case Some(SummaryAnswer.Refused(e))     => Left(PanelBody.Unavailable(e.message))
      case Some(SummaryAnswer.Failed(reason)) => Left(PanelBody.Unavailable(reason))

  private def reportOf(
      c: FigureComposer,
      run: RunId,
      spec: ReportingSpec,
      scale: ScaleIndex
  ): Either[PanelBody, ReportView] =
    c.reports.get((run, spec, scale)) match
      case None              => Left(PanelBody.Waiting(ComposerText.reading(run)))
      case Some(Left(why))   => Left(PanelBody.Unavailable(why))
      case Some(Right(view)) => Right(view)

  /** A panel's body from its template and what the backend answered. */
  def body(c: FigureComposer, s: FigureSource, template: PanelTemplate): PanelBody =
    val run    = s.run.id
    val scales = s.bound.analysis.recipe.scales
    template match
      case PanelTemplate.ParticipantD(sigma) =>
        FigurePanels.scaleIndex(scales, sigma) match
          case None    => PanelBody.Unavailable(ComposerText.notComputed(run, sigma))
          case Some(i) =>
            (for
              r     <- summaryOf(c, run)
              view  <- reportOf(c, run, s.reporting, i)
              label <- r.scales
                .lift(i.value)
                .toRight(
                  PanelBody.Unavailable(s"The summary of ${run.label} has no scale ${i.value}.")
                )
            yield (label, view)).fold(
              identity,
              (label, view) =>
                FigurePanels
                  .participantD(
                    view,
                    label,
                    s.reporting.weighting,
                    c.appearanceOf(s.figure.id).lines
                  )
                  .fold(PanelBody.Unavailable(_), PanelBody.Plot(_))
            )
      case PanelTemplate.ScaleProfile =>
        (for
          r       <- summaryOf(c, run)
          overall <- CompareSummary
            .overall(s.reporting)
            .toRight(PanelBody.Unavailable("The overall reporting spec could not be formed."))
          grouped <- scales.values.indices.toVector.traverse(i =>
            ScaleIndex
              .of(i)
              .left
              .map(e => PanelBody.Unavailable(e.message): PanelBody)
              .flatMap(reportOf(c, run, s.reporting, _))
          )
          whole <- scales.values.indices.toVector.traverse(i =>
            ScaleIndex
              .of(i)
              .left
              .map(e => PanelBody.Unavailable(e.message): PanelBody)
              .flatMap(reportOf(c, run, overall, _))
          )
        yield (r, grouped, whole)).fold(
          identity,
          (r, grouped, whole) =>
            FigurePanels
              .scaleProfile(grouped, whole, scales, r.scales)
              .fold(PanelBody.Unavailable(_), PanelBody.Plot(_))
        )
      case PanelTemplate.DensityMaps(sigma, query) =>
        FigurePanels.scaleIndex(scales, sigma) match
          case None    => PanelBody.Unavailable(ComposerText.notComputed(run, sigma))
          case Some(i) =>
            c.references.get((run, i, query)) match
              case None                => PanelBody.Waiting(ComposerText.reading(run))
              case Some(Left(e))       => PanelBody.Unavailable(e)
              case Some(Right(scores)) =>
                PanelBody.Maps(
                  FigurePanels.densityMaps(
                    run,
                    i,
                    sigma,
                    query,
                    scores,
                    t => c.maps.get((run, i, t)),
                    s.bound.analysis.recipe.weighting
                  )
                )
      case PanelTemplate.Gaze(trial) =>
        c.displays.get(s.bound.dataset.id) match
          case None         => PanelBody.Waiting(ComposerText.displays(s.bound.dataset.id))
          case Some(answer) =>
            val registry = answer.map {
              case DisplaySource.Served(r) => Some(r)
              case DisplaySource.NotServed => None
            }
            val role =
              if trial.phase == s.bound.analysis.recipe.phases.focal then GazeRole.Query
              else GazeRole.Matched
            PanelBody.Gaze(
              FigurePanels.gaze(
                trial,
                registry,
                c.fixations.get((s.bound.analysis.id, trial)),
                s.bound.dataset.geometry.screen,
                role
              )
            )
      case PanelTemplate.NoTemplate(scale, selection) =>
        PanelBody.Unavailable(ComposerText.noTemplate(scale, selection))

  /** The Table tab of a panel: its values with their refs, or why it has none. */
  def table(c: FigureComposer, s: FigureSource, panel: PanelSpec): Either[String, PlotSource] =
    PanelTemplate.of(panel) match
      case PanelTemplate.DensityMaps(sigma, query) =>
        for
          i <- FigurePanels
            .scaleIndex(s.bound.analysis.recipe.scales, sigma)
            .toRight(ComposerText.notComputed(s.run.id, sigma))
          scores <- c.references
            .get((s.run.id, i, query))
            .toRight(ComposerText.reading(s.run.id))
            .flatMap(identity)
          source <- FigurePanels.referenceTable(s.run.id, i, query, scores)
        yield source
      case PanelTemplate.Gaze(_) => Left(ComposerText.noTable(panel.letter))
      case other                 =>
        body(c, s, other) match
          case PanelBody.Plot(vm)                    => Right(vm.source)
          case PanelBody.Waiting(why)                => Left(why)
          case PanelBody.Unavailable(why)            => Left(why)
          case PanelBody.Gaze(_) | PanelBody.Maps(_) => Left(ComposerText.noTable(panel.letter))

/** The composer's English text. */
object ComposerText:
  /** "figure-1.svg". */
  def fileName(page: PageVM, format: ExportFormat): String =
    s"figure-${page.figure.number}.${format.extension}"

  def exportedTo(where: String): String = s"Exported to $where."
  def exportFailed(why: String): String = s"The figure was not exported: $why"

  val NoRun: String       = "A new figure needs a completed, current run; there is none."
  val NoReporting: String = "A new figure needs a reporting spec; the project has none."
  val NoFigure: String    = "Add panel needs a figure; the project has none."

  /** "Page 183 mm · two-column". */
  def page(width: PageWidth): String = s"Page ${width.label}"

  def reading(run: RunId): String = s"Reading ${run.label}'s results…"

  def displays(dataset: DatasetRevision): String =
    s"Reading what data ${dataset.label}'s trials displayed…"

  def notComputed(run: RunId, sigma: Sigma): String =
    s"${run.label} does not compute ${sigma.render}."

  // Follow-up bd-01M42K7ZNCC5J9R9RPR7H6ZHNT: the gaze panels' fixations.
  def noTable(letter: PanelLetter): String =
    s"Panel ${letter.value} has no table yet: its fixations are listed when the backend " +
      "serves the trial-fixations view (S6.2)."

  def noTemplate(scale: PanelScale, selection: PanelSelection): String =
    val at = scale match
      case PanelScale.Unscaled  => "no scale"
      case PanelScale.At(s)     => s.render
      case PanelScale.AllScales => "every scale"
    val of = selection match
      case PanelSelection.Trial(k)               => k.label
      case PanelSelection.QueryWithReferences(q) => s"${q.label} with its references"
      case PanelSelection.AllQueries             => "all queries"
    s"No panel template draws $of at $at."
