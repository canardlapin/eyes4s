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

import eyes4s.studio.app.compare.SummaryAnswer
import eyes4s.studio.app.explore.DisplaySource
import eyes4s.studio.app.layout.StudioLayouts
import eyes4s.studio.app.nav.{Location, Place}
import eyes4s.studio.app.plot.{ParticipantLines, PlotSource}
import eyes4s.studio.app.text.Format
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.backend.{DatasetRevision, RunId, TrialKey}
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
  case ReferencesRead(
      run: RunId,
      scale: ScaleIndex,
      query: TrialKey,
      answer: Either[String, ReferenceScores]
  )
  case DisplaysRead(dataset: DatasetRevision, answer: Either[String, DisplaySource])

  /** The methods.md pane (S9.4). */
  case Methods(intent: MethodsIntent)

/** What the composer asks of the app and the platform. */
enum ComposerEffect derives CanEqual:
  case App(intent: Intent)

  /** The figure binding's request (S9.1's `RequestStatus`). */
  case Binding(effect: FigureEffect)

  /** The run's result summary (panels D and E). */
  case RequestSummary(run: RunId)

  /** A query's contrast, matched pair and control pairs (panel C). */
  case RequestReferences(run: RunId, scale: ScaleIndex, query: TrialKey)

  /** What each trial of the revision displayed (panels A and B). */
  case RequestDisplays(dataset: DatasetRevisionSpec)

  /** Export `page` as `format` under the suggested file name `name`. */
  case ExportFigure(format: ExportFormat, page: PageVM, name: String)

  /** Read the admission and query facts the methods text cites (S9.4). */
  case RequestMethods(run: RunId, dataset: DatasetRevision)

/** One read the composer asked for, so it is asked once. */
enum ComposerRead derives CanEqual:
  case Summary(run: RunId)
  case References(run: RunId, scale: ScaleIndex, query: TrialKey)
  case Displays(dataset: DatasetRevision)
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
    exporting: ExportVM
) derives CanEqual

/** Everything the Figures perspective shows. */
final case class ComposerVM(
    figures: FiguresVM,
    newFigure: String,
    widths: Vector[(PageWidth, String, Boolean)],
    page: Option[PageVM],
    methods: Option[MethodsVM],
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
    asked: Set[ComposerRead],
    problem: Option[String],
    appearance: Map[FigureId, FigureAppearance],
    greyscale: Boolean,
    format: ExportFormat,
    exported: Option[String],
    methods: FigureMethods
) derives CanEqual:
  def appearanceOf(figure: FigureId): FigureAppearance =
    appearance.getOrElse(figure, FigureAppearance.default)

  /** The same composer, asking `read` again at the next sync if it failed. */
  private[figures] def retrying(read: ComposerRead, failed: Boolean): FigureComposer =
    if failed then copy(asked = asked - read) else this

object FigureComposer:
  val empty: FigureComposer = FigureComposer(
    FigureBinding.empty,
    PageWidth.TwoColumn,
    Zoom.Default,
    ComposerTab.Figure,
    Map.empty,
    Map.empty,
    Map.empty,
    Set.empty,
    None,
    Map.empty,
    false,
    ExportFormat.Svg,
    None,
    FigureMethods.empty
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
          s.figure.panels.flatMap { p =>
            PanelTemplate.of(p) match
              case PanelTemplate.DensityMaps(sigma, query) =>
                FigurePanels
                  .scaleIndex(scales, sigma)
                  .map(i =>
                    ComposerRead.References(run, i, query) ->
                      ComposerEffect.RequestReferences(run, i, query)
                  )
                  .toVector
              case PanelTemplate.Gaze(_) =>
                Vector(
                  ComposerRead.Displays(s.bound.dataset.id) ->
                    ComposerEffect.RequestDisplays(s.bound.dataset)
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
    val next = reads(c, model)
    (
      c.copy(binding = binding, asked = c.asked ++ next.map(_._1)),
      bound.map(lift) ++ next.map(_._2)
    )

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
      case NewFigure   => newFigure(c, model)
      case SetWidth(w) =>
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
      case ReferencesRead(r, s, q, a) =>
        (
          c.copy(references = c.references.updated((r, s, q), a))
            .retrying(ComposerRead.References(r, s, q), a.isLeft),
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
      model: AppModel
  ): (FigureComposer, Vector[ComposerEffect]) =
    val document = model.document
    val made     = for
      run    <- RebindProposal.target(model.freshness).toRight(ComposerText.NoRun)
      rep    <- document.reporting.headOption.toRight(ComposerText.NoReporting)
      scales <- document
        .analysis(run.analysis)
        .map(_.recipe.scales.values)
        .toRight(ComposerText.NoRun)
      first <- scales.headOption.toRight(ComposerText.NoRun)
      a     <- PanelLetter.of("A").left.map(_.message)
      b     <- PanelLetter.of("B").left.map(_.message)
      next  <- document.nextFigureId.left.map(_.message)
    yield (
      next,
      Command.CreateFigure(
        run.id,
        rep.id,
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
    )
    made match
      case Left(why)             => (c.copy(problem = Some(why)), none)
      case Right((next, create)) =>
        (
          c.copy(problem = None),
          Vector(
            ComposerEffect.App(Intent.Dispatch(create)),
            ComposerEffect.App(Intent.Navigate(figureTrail(next, None)))
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
      .map(s => FigureMethods.view(c.methods, s, c.summaries.get(s.run.id)))
    ComposerVM(
      FigureBinding.view(c.binding, model),
      "New figure",
      PageWidth.values.toVector.map(w => (w, w.label, w == c.width)),
      page.flatMap(_.toOption),
      methods,
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
      )
    )

  private def summaryOf(c: FigureComposer, run: RunId) =
    c.summaries.get(run) match
      case None                            => Left(PanelBody.Waiting(ComposerText.reading(run)))
      case Some(SummaryAnswer.Answered(r)) => Right(r)
      case Some(SummaryAnswer.Refused(e))  => Left(PanelBody.Unavailable(e.message))
      case Some(SummaryAnswer.Failed(reason)) => Left(PanelBody.Unavailable(reason))

  /** A panel's body from its template and what the backend answered. */
  def body(c: FigureComposer, s: FigureSource, template: PanelTemplate): PanelBody =
    val run    = s.run.id
    val scales = s.bound.analysis.recipe.scales
    template match
      case PanelTemplate.ParticipantD(sigma) =>
        FigurePanels.scaleIndex(scales, sigma) match
          case None    => PanelBody.Unavailable(ComposerText.notComputed(run, sigma))
          case Some(i) =>
            summaryOf(c, run).fold(
              identity,
              r =>
                FigurePanels
                  .participantD(r, s.reporting.id, i, c.appearanceOf(s.figure.id).lines)
                  .fold(PanelBody.Unavailable(_), PanelBody.Plot(_))
            )
      case PanelTemplate.ScaleProfile =>
        summaryOf(c, run).fold(
          identity,
          r =>
            FigurePanels
              .scaleProfile(r, s.reporting.id, scales)
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
                PanelBody.Maps(FigurePanels.densityMaps(run, i, sigma, query, scores))
      case PanelTemplate.Gaze(trial) =>
        c.displays.get(s.bound.dataset.id) match
          case None         => PanelBody.Waiting(ComposerText.displays(s.bound.dataset.id))
          case Some(answer) =>
            val registry = answer.map {
              case DisplaySource.Served(r) => Some(r)
              case DisplaySource.NotServed => None
            }
            PanelBody.Gaze(FigurePanels.gaze(trial, registry))
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
