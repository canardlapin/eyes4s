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

package eyes4s.studio.app.compare

import eyes4s.kernel.Span
import eyes4s.plan.{MapPlacement, WindowTally}
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.app.maps.LimitsScope
import eyes4s.studio.app.text.{Format, WhyText, WhyTextId}
import eyes4s.studio.app.vm.{A11yRole, FocusStop}
import eyes4s.studio.core.backend.{
  DatasetRevision,
  LedgerEntry,
  PairDesign,
  QueryRow,
  QueryStatus,
  ResultSummary,
  TrialDisposition,
  TrialKey
}
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.{
  AnalysisRevisionSpec,
  ControlChoice,
  DatasetRevisionSpec,
  InitialFixationChoice,
  MapOpacity,
  MatchedChoice,
  OffWindowChoice,
  Perspective,
  Recipe,
  ReportingFilter,
  ReportingId,
  ReportingSpec,
  ReportingWeight,
  StageAppearance,
  StudioDocument,
  UnmatchedChoice,
  WeightChoice
}
import eyes4s.studio.core.selection.{StudioRef, TrialGrouping}

/** The backend's answer for a dataset revision's whole admission ledger. */
enum LedgerAnswer derives CanEqual:
  case Answered(entries: Vector[LedgerEntry])
  case Failed(reason: String)

enum WhyEffect derives CanEqual:
  /** Read every entry of `dataset`'s admission ledger. */
  case ReadLedger(dataset: DatasetRevision)

enum WhyIntent derives CanEqual:
  case LedgerRead(dataset: DatasetRevision, answer: LedgerAnswer)

  /** Whether the trial panels' maps share their colour limits (View only). */
  case ChooseLimits(scope: LimitsScope)

  /** Read the ledger again after a failed read. */
  case Retry

/** One line of an inspector section: its label, its value, and the ref the
  * value traces to when it states a number.
  */
final case class InspectorFact(label: String, value: String, ref: Option[StudioRef])
    derives CanEqual

/** Why the reference panel's trial is the query's reference: the sentence
  * the prepared design yields for it, and the facts beneath it.
  */
final case class WhyVM(explanation: String, facts: Vector[InspectorFact]) derives CanEqual

/** A read-only inspector section: its title, its kind of change and its facts. */
final case class InspectorSection(title: String, kind: String, facts: Vector[InspectorFact])
    derives CanEqual

/** One choice of a segmented control and whether it is the one chosen. */
final case class SegmentVM[A](value: A, label: String, chosen: Boolean) derives CanEqual

/** The Appearance section ("View only"): the stage surround, whether the
  * panels' colour limits are shared, and the maps' opacity.
  */
final case class AppearanceVM(
    title: String,
    kind: String,
    stageLabel: String,
    stages: Vector[SegmentVM[StageAppearance]],
    limitsLabel: String,
    limits: Vector[SegmentVM[LimitsScope]],
    opacityLabel: String,
    opacity: Double,
    opacityText: String
) derives CanEqual

/** Compare's inspector (Main.dc.html, inspector): why this reference, the
  * analysis, the reporting, the appearance. `status` says why there is no
  * explanation yet; `edit` opens Analysis; `retry` is offered after a failed
  * ledger read.
  */
final case class CompareInspectorVM(
    title: String,
    status: Option[String],
    why: Option[WhyVM],
    analysis: Option[InspectorSection],
    edit: (String, Intent),
    reporting: InspectorSection,
    appearance: AppearanceVM,
    retry: Option[String]
) derives CanEqual

/** What the inspector reads of the Compare views it sits beside: the trial
  * panels (their focus, the inspected pair and the trials' content), the
  * shown run and its summary, and the reporting spec Compare is in.
  */
final case class InspectorInputs(
    panels: TrialPanels,
    shown: Option[ShownRun],
    summary: Option[ResultSummary],
    reporting: Option[ReportingId]
) derives CanEqual

/** Compare's "Why this reference?" inspector (ticket S8.4; Main.dc.html,
  * inspector). The explanation is generated from the prepared design: the
  * run's analysis revision's recipe chooses one template per matching and
  * control choice, and the run's query row and the dataset's admission
  * ledger fill it in. Outside-window figures are eyes4s's `WindowTally` of
  * the trials' placed fixations. The analysis and reporting sections are
  * read-only; the appearance section dispatches View-only commands. The
  * only state is the ledger read and the colour-limit scope. Pure; a host
  * performs the effects.
  */
final case class WhyReference(
    dataset: Option[DatasetRevision],
    ledger: Option[LedgerAnswer],
    limits: LimitsScope
) derives CanEqual

object WhyReference:
  import WhyTextId.*

  val empty: WhyReference = WhyReference(None, None, LimitsScope.Shared)

  /** The dataset revision the shown run was made on, per the document. */
  def datasetOf(m: AppModel, shown: Option[ShownRun]): Option[DatasetRevision] =
    shown.flatMap(s => m.document.run(s.run)).map(_.dataset)

  /** Follows the shown run's dataset: a new dataset's ledger is read once. */
  def sync(
      s: WhyReference,
      dataset: Option[DatasetRevision]
  ): (WhyReference, Vector[WhyEffect]) =
    if dataset == s.dataset then (s, Vector.empty)
    else
      (s.copy(dataset = dataset, ledger = None), dataset.toVector.map(WhyEffect.ReadLedger(_)))

  def update(s: WhyReference, intent: WhyIntent): (WhyReference, Vector[WhyEffect]) =
    intent match
      case WhyIntent.LedgerRead(d, a) if s.dataset.contains(d) && s.ledger.isEmpty =>
        (s.copy(ledger = Some(a)), Vector.empty)
      case WhyIntent.LedgerRead(_, _)    => (s, Vector.empty)
      case WhyIntent.ChooseLimits(scope) => (s.copy(limits = scope), Vector.empty)
      case WhyIntent.Retry               =>
        (s.ledger, s.dataset) match
          case (Some(LedgerAnswer.Failed(_)), Some(d)) =>
            (s.copy(ledger = None), Vector(WhyEffect.ReadLedger(d)))
          case _ => (s, Vector.empty)

  /** The intent that sets the stage surround. */
  def stageIntent(stage: StageAppearance): Intent = Intent.Dispatch(Command.SetStage(stage))

  /** The intent that sets the maps' opacity, if `value` is an opacity. */
  def opacityIntent(value: Double): Option[Intent] =
    MapOpacity.of(value).toOption.map(o => Intent.Dispatch(Command.SetMapOpacity(o)))

  /** The intent of Edit in Analysis. */
  val editIntent: Intent = Intent.SwitchPerspective(Perspective.Analysis)

  /** The controls inside the inspector's own focus stop, in Tab order: Edit
    * in Analysis, each segmented control's chosen segment, the opacity
    * slider, and Retry while it is offered.
    */
  def focusStops(vm: CompareInspectorVM): Vector[FocusStop] =
    val a                                                        = vm.appearance
    def chosen[A](label: String, segments: Vector[SegmentVM[A]]) =
      segments
        .find(_.chosen)
        .orElse(segments.headOption)
        .map(c => FocusStop(A11yRole.RadioButton, s"$label: ${c.label}"))
    Vector(FocusStop(A11yRole.Button, vm.edit._1)) ++
      chosen(a.stageLabel, a.stages) ++
      chosen(a.limitsLabel, a.limits) ++
      Vector(FocusStop(A11yRole.Slider, a.opacityLabel)) ++
      vm.retry.map(FocusStop(A11yRole.Button, _))

  /** The view-model of `doc` (the document's recipe, reporting specs and
    * presentation) beside the Compare views `in`.
    */
  def vm(s: WhyReference, doc: StudioDocument, in: InspectorInputs): CompareInspectorVM =
    val spec    = in.shown.flatMap(r => doc.analysis(r.revision))
    val dataset = spec.flatMap(a => doc.dataset(a.dataset))
    val focus   = in.panels.focus.filter(f => in.shown.exists(_.run == f.run))
    val status  = (focus, in.shown) match
      case (None, _)                            => Some(WhyText(NoQuery))
      case (Some(_), Some(r)) if r.rows.isEmpty => Some(WhyText(Reading))
      case _                                    => None
    val why = for
      f      <- focus
      shown  <- in.shown
      row    <- shown.rows.find(_.query == f.query)
      recipe <- spec.map(_.recipe)
    yield explain(s, in.panels, f, row, recipe)
    CompareInspectorVM(
      WhyText(Title),
      status,
      why,
      in.shown.map(r =>
        spec.fold(
          InspectorSection(
            WhyText(AnalysisTitle, r.revision.label),
            WhyText(AnalysisKind),
            Vector(InspectorFact("", WhyText(NoAnalysis, r.revision.label), None))
          )
        )(analysis(_, dataset))
      ),
      (WhyText(EditInAnalysis), editIntent),
      reporting(doc, in),
      appearance(s, doc),
      Option.when(s.ledger.exists {
        case LedgerAnswer.Failed(_) => true
        case _                      => false
      })(WhyText(Retry))
    )

  // --- Why this reference? -------------------------------------------------------------

  private def explain(
      s: WhyReference,
      panels: TrialPanels,
      f: PanelFocus,
      row: QueryRow,
      recipe: Recipe
  ): WhyVM =
    val focal               = recipe.phases.focal
    val refPhase            = recipe.phases.reference
    val p                   = f.query.participant
    val (design, reference) =
      TrialPanels.referenceOf(f, Vector(row)).getOrElse((PairDesign.Matched, row.matched))
    val entries   = s.ledger.collect { case LedgerAnswer.Answered(es) => es }
    val inspected = panels.inspected.collect {
      case (ref, Some(PairAnswer.Answered(eyes4s.studio.core.backend.Inspection.Pair(_, i, _))))
          if ref == f.pair(design, reference) =>
        i
    }
    val refItem = design match
      case PairDesign.Matched => Some(row.item)
      case PairDesign.Control =>
        inspected.orElse(entries.flatMap(_.find(_.trial == reference)).map(_.item))
    val sentence = row.status match
      case QueryStatus.NotAdmitted(d) => WhyText(QueryNotAdmitted, disposition(d))
      case QueryStatus.NoMatch(_)     =>
        recipe.unmatched match
          case UnmatchedChoice.ReportNoMatch =>
            WhyText(NoMatchReport, refPhase.label, p, row.item)
          case UnmatchedChoice.Refuse => WhyText(NoMatchRefuse, refPhase.label, p, row.item)
      case _ =>
        design match
          case PairDesign.Matched =>
            val args = Vector(refPhase.label, p, row.item, focal.label.toLowerCase)
            recipe.matched match
              case MatchedChoice.RequireOne     => WhyText(MatchedRequireOne, args*)
              case MatchedChoice.SameOccurrence => WhyText(MatchedSameOccurrence, args*)
              case MatchedChoice.Select(pick)   =>
                WhyText(MatchedSelect, (args :+ pick.render)*)
              case MatchedChoice.MeanOfAll => WhyText(MatchedMeanOfAll, args*)
          case PairDesign.Control =>
            val pool = recipe.controls match
              case ControlChoice.SameSelection =>
                WhyText(ControlSameSelection, refPhase.label, p)
              case ControlChoice.AllOccurrences =>
                WhyText(ControlAllOccurrences, refPhase.label, p)
            val used = row.controls.fold(WhyText(ControlsUncounted))(n =>
              WhyText(ControlsCounted, n.toString)
            )
            s"$pool $used"
    val contrast = f.contrast
    val facts    = Vector(
      InspectorFact(
        WhyText(Participant),
        if reference.participant == p then WhyText(SameParticipant, reference.participant)
        else WhyText(OtherParticipant, reference.participant),
        Some(StudioRef.Participant(reference.participant))
      ),
      InspectorFact(
        WhyText(PhaseOccurrence),
        occurrence(reference, refItem, entries),
        Some(StudioRef.Trial(reference))
      ),
      InspectorFact(
        WhyText(Item),
        refItem.getOrElse(WhyText(ControlsUnknown)),
        Some(StudioRef.Trial(reference))
      ),
      InspectorFact(
        WhyText(Controls),
        row.controls.fold(WhyText(ControlsUnknown))(n => WhyText(ControlsValue, n.toString)),
        Some(contrast)
      ),
      InspectorFact(
        WhyText(Excluded),
        excluded(s, p, refPhase, entries),
        s.dataset.map(d => StudioRef.TrialGroup(d, TrialGrouping.PhaseOf(p, refPhase)))
      ),
      InspectorFact(
        WhyText(OutsideReference),
        outside(panels, reference),
        Some(StudioRef.Trial(reference))
      ),
      InspectorFact(
        WhyText(OutsideQuery),
        outside(panels, f.query),
        Some(StudioRef.Trial(f.query))
      )
    )
    WhyVM(sentence, facts)

  /** "Encoding · 1 of 1": the reference's occurrence, of the occurrences of
    * its item the ledger lists for its participant and phase.
    */
  private def occurrence(
      reference: TrialKey,
      item: Option[String],
      entries: Option[Vector[LedgerEntry]]
  ): String =
    val of = for
      i  <- item
      es <- entries
      n = es.count(e =>
        e.trial.participant == reference.participant &&
          e.trial.phase == reference.phase && e.item == i
      )
      if n > 0
    yield n
    of.fold(WhyText(OccurrenceOnly, reference.phase.label, reference.occurrence.toString))(n =>
      WhyText(OccurrenceOf, reference.phase.label, reference.occurrence.toString, n.toString)
    )

  /** The participant's reference-phase trials the ledger did not admit: the
    * candidates no pair can use.
    */
  private def excluded(
      s: WhyReference,
      participant: String,
      phase: eyes4s.studio.core.backend.Phase,
      entries: Option[Vector[LedgerEntry]]
  ): String =
    (s.ledger, entries) match
      case (_, Some(es)) =>
        val candidates =
          es.filter(e => e.trial.participant == participant && e.trial.phase == phase)
        val out = candidates.filter(_.disposition != TrialDisposition.Admitted)
        if out.isEmpty then WhyText(ExcludedNone, candidates.size.toString, phase.label)
        else
          WhyText(
            ExcludedSome,
            out.size.toString,
            candidates.size.toString,
            phase.label,
            out
              .map(e => WhyText(ExcludedTrial, e.trial.trial, disposition(e.disposition)))
              .mkString(", ")
          )
      case (Some(LedgerAnswer.Failed(why)), _) => WhyText(ExcludedUnreadable, why)
      case _                                   => WhyText(ExcludedReading)

  private def disposition(d: TrialDisposition): String = d match
    case TrialDisposition.Admitted       => WhyText(DispositionAdmitted)
    case TrialDisposition.Quarantined(c) => WhyText(DispositionQuarantined, c.message)
    case TrialDisposition.NoFixations    => WhyText(DispositionNoFixations)
    case TrialDisposition.Absent         => WhyText(DispositionAbsent)

  /** "1 of 12 fix · 4% dur": eyes4s's window tally of the trial's fixations
    * as the panels read them under the run's revision.
    */
  private def outside(panels: TrialPanels, trial: TrialKey): String =
    panels.contents.collectFirst {
      case (key, answer) if key.trial == trial => answer
    } match
      case None | Some(None)    => WhyText(OutsideReading)
      case Some(Some(Left(_)))  => WhyText(OutsideUnknown)
      case Some(Some(Right(c))) =>
        tally(c.fixations).fold(WhyText(OutsideUnknown)) { t =>
          WhyText(
            OutsideValue,
            t.outsideWindow.toString,
            t.total.toString,
            t.outsideWindowShare.fold(WhyText(OutsideUnknown))(Format.percent)
          )
        }

  /** The trial's [[WindowTally]], from its fixations' placements. */
  def tally(fixations: Vector[ContentFixation]): Option[WindowTally] =
    def sum(fs: Vector[ContentFixation]) = Span.millis(fs.map(_.durationMs.toLong).sum)
    val screen = fixations.filter(_.placement == MapPlacement.OutsideScreen)
    val window = fixations.filter(_.placement match
      case MapPlacement.OutsideWindow(_) => true
      case _                             => false)
    WindowTally
      .of(screen.size, window.size, fixations.size, sum(screen), sum(window), sum(fixations))
      .toOption

  // --- Analysis ------------------------------------------------------------------------------

  private def analysis(
      spec: AnalysisRevisionSpec,
      dataset: Option[DatasetRevisionSpec]
  ): InspectorSection =
    val r      = spec.recipe
    val method = r.method.definition.name.split('.').last.capitalize
    val window = r.window.fold(WhyText(WindowWhole)) { w =>
      val (wd, ht) = (whole(w.xMax - w.xMin), whole(w.yMax - w.yMin))
      val image    = dataset.map(_.geometry.image)
      val box      =
        if image.exists(i =>
            i.left.toDouble == w.xMin && i.top.toDouble == w.yMin &&
              i.width.toDouble == w.xMax - w.xMin && i.height.toDouble == w.yMax - w.yMin
          )
        then WhyText(WindowImageFrame, wd, ht)
        else WhyText(WindowBox, wd, ht)
      r.offWindow match
        case Some(OffWindowChoice.FailTrial) => WhyText(OffFails, box)
        case _                               => box
    }
    InspectorSection(
      WhyText(AnalysisTitle, spec.id.label),
      WhyText(AnalysisKind),
      Vector(
        InspectorFact(
          WhyText(Representation),
          r.weighting match
            case WeightChoice.Duration => WhyText(DurationWeighted)
            case WeightChoice.Uniform  => WhyText(CountWeighted)
          ,
          None
        ),
        InspectorFact(WhyText(Window), window, None),
        InspectorFact(
          WhyText(InitialFixation),
          r.initialFixations match
            case InitialFixationChoice.KeepAll                 => WhyText(KeepAll)
            case InitialFixationChoice.DropFirst               => WhyText(DropFirst)
            case InitialFixationChoice.DropLeadingNearCross(c) =>
              WhyText(DropNearCross, whole(c.degrees))
          ,
          None
        ),
        InspectorFact(
          WhyText(Smoother),
          WhyText(Gaussian, r.scales.values.map(s => s"${whole(s.degrees)}°").mkString(", ")),
          None
        ),
        InspectorFact(WhyText(Grid), grid(r, dataset), None),
        InspectorFact(
          WhyText(MetricControls),
          r.controls match
            case ControlChoice.SameSelection  => WhyText(MetricSameSelection, method)
            case ControlChoice.AllOccurrences => WhyText(MetricAllOccurrences, method)
          ,
          None
        )
      )
    )

  /** "64×48 · cell 0.46°": the cell size is recipe arithmetic Studio does
    * until CR6d's plan descriptors serve it (bd-01M3DH0S5ZKFCN47MHDM0XWW10),
    * as the methods text does.
    */
  private def grid(r: Recipe, dataset: Option[DatasetRevisionSpec]): String =
    val cols = r.grid.columns.toString
    val rows = r.grid.rows.toString
    val size = r.window
      .map(w => (w.xMax - w.xMin, w.yMax - w.yMin))
      .orElse(
        dataset.map(d => (d.geometry.screen.width.toDouble, d.geometry.screen.height.toDouble))
      )
    (size, r.angularScale) match
      case (Some((w, h)), Some(ppd)) =>
        val (x, y) =
          (
            Format.decimal(w / r.grid.columns / ppd.value, 2),
            Format.decimal(h / r.grid.rows / ppd.value, 2)
          )
        WhyText(GridCell, cols, rows, if x == y then s"$x°" else s"$x × $y°")
      case _ => WhyText(GridOnly, cols, rows)

  /** `2` not `2.0`. */
  private def whole(v: Double): String =
    if v == math.rint(v) && math.abs(v) < 1e15 then v.toLong.toString else v.toString

  // --- Reporting -----------------------------------------------------------------------------

  private def reporting(doc: StudioDocument, in: InspectorInputs): InspectorSection =
    val title = (WhyText(ReportingTitle), WhyText(ReportingKind))
    in.reporting.flatMap(id => doc.reporting.find(_.id == id)) match
      case None =>
        InspectorSection(
          title._1,
          title._2,
          Vector(InspectorFact("", WhyText(NoReporting), None))
        )
      case Some(spec) => InspectorSection(title._1, title._2, reportingFacts(spec, in.summary))

  private def reportingFacts(
      spec: ReportingSpec,
      summary: Option[ResultSummary]
  ): Vector[InspectorFact] =
    val minimum = spec.minimumPerGroup.fold(WhyText(MinimumOff))(n =>
      WhyText(MinimumOn, n.queries.toString)
    )
    val filters =
      if spec.filters.isEmpty then WhyText(NoFilters)
      else
        spec.filters
          .map {
            case ReportingFilter.Keep(a, vs) =>
              WhyText(FilterKeep, a.label, vs.values.mkString(", "))
            case ReportingFilter.OutsideWindowAtMost(share) =>
              WhyText(FilterOutside, Format.percent(share.value))
          }
          .mkString("; ")
    Vector(
      InspectorFact(WhyText(Spec), spec.name, None),
      InspectorFact(
        WhyText(GroupBy),
        spec.groupBy.fold(WhyText(NoGrouping))(_.label),
        None
      ),
      InspectorFact(WhyText(Filters), filters, None),
      InspectorFact(
        WhyText(MinimumPerGroup),
        summary.fold(minimum)(r =>
          WhyText(GroupRange, minimum, r.groupNMinimum.toString, r.groupNMaximum.toString)
        ),
        None
      ),
      InspectorFact(
        WhyText(Summary),
        spec.weighting match
          case ReportingWeight.ParticipantMeans => WhyText(ParticipantMeans)
          case ReportingWeight.PooledQueries    => WhyText(PooledQueries)
        ,
        None
      )
    )

  // --- Appearance ----------------------------------------------------------------------------

  private def appearance(s: WhyReference, doc: StudioDocument): AppearanceVM =
    val p = doc.presentation
    AppearanceVM(
      WhyText(AppearanceTitle),
      WhyText(ViewOnly),
      WhyText(Stage),
      StageAppearance.values.toVector.map(st =>
        SegmentVM(
          st,
          WhyText(st match
            case StageAppearance.Dark  => StageDark
            case StageAppearance.Mid   => StageMid
            case StageAppearance.Light => StageLight),
          p.stage == st
        )
      ),
      WhyText(Limits),
      LimitsScope.values.toVector.map(l =>
        SegmentVM(
          l,
          WhyText(l match
            case LimitsScope.Shared   => LimitsShared
            case LimitsScope.PerPanel => LimitsPerPanel),
          s.limits == l
        )
      ),
      WhyText(Opacity),
      p.mapOpacity.value,
      WhyText(OpacityValue, Format.decimal(p.mapOpacity.value, 2))
    )
