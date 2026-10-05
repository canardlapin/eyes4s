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

import eyes4s.studio.app.AppModel
import eyes4s.studio.app.admission.AdmissionLedgerVM
import eyes4s.studio.app.explore.TrialsNavigatorVM
import eyes4s.studio.app.nav.{DataSection, Place}
import eyes4s.studio.app.text.{
  DesignText,
  Format,
  GeometryText,
  GeometryTextId,
  LedgerText,
  LedgerTextId,
  MessageId,
  Messages,
  SourcesText,
  SummaryText
}
import eyes4s.studio.core.backend.{
  DatasetRevision,
  PairDesign,
  ResultAddress,
  RunId,
  StageKind,
  TrialKey
}
import eyes4s.studio.core.document.{
  FigureId,
  PanelLetter,
  PanelScale,
  Perspective,
  Preset,
  RecipeField,
  ReportingId,
  SourceRole
}
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef, TallyRegion}

/** The words for typed places and refs, resolved against one model: item
  * names from the ledger, file names from the dataset, σ from the run's
  * recipe. Every lookup has a fallback, so labelling is total.
  */
final class Labels(model: AppModel, messages: Messages):
  import MessageId.*

  private def doc = model.document

  def perspective(p: Perspective): String = messages(p match
    case Perspective.Data     => PerspectiveData
    case Perspective.Explore  => PerspectiveExplore
    case Perspective.Analysis => PerspectiveAnalysis
    case Perspective.Compare  => PerspectiveCompare
    case Perspective.Figures  => PerspectiveFigures)

  def stage(kind: StageKind): String = messages(kind match
    case StageKind.Estimating  => StageEstimating
    case StageKind.Comparing   => StageComparing
    case StageKind.Reducing    => StageReducing
    case StageKind.Contrasting => StageContrasting)

  def design(d: PairDesign): String = messages(d match
    case PairDesign.Matched => DesignMatched
    case PairDesign.Control => DesignControl)

  def run(id: RunId): String = messages(JobTitle, id.number.toString)

  def plural(n: Int, one: MessageId, many: MessageId): String =
    messages(if n == 1 then one else many, n.toString)

  /** "by retrieval response": the spec's name as a phrase. */
  def reporting(id: ReportingId): String =
    doc.reporting.find(_.id == id).fold(id.value) { spec =>
      val name = spec.name
      if name.isEmpty then name else s"${name.head.toLower}${name.tail}"
    }

  def lineage(preset: Preset): String = messages(preset match
    case Preset.EncodingRetrieval => LineageEncodingRetrieval
    case Preset.Recognition       => LineageRecognition
    case Preset.Custom            => LineageCustom
    case Preset.PerceptionImagery => LineagePerceptionImagery)

  def section(s: DataSection): String = messages(s match
    case DataSection.Sources       => SectionSources
    case DataSection.ColumnMapping => SectionColumnMapping
    case DataSection.TrialMetadata => SectionTrialMetadata
    case DataSection.Admission     => SectionAdmission
    case DataSection.Geometry      => SectionGeometry)

  /** A recipe field as a form label ("Scales"). */
  def field(f: RecipeField): String =
    val words = f.label
    if words.isEmpty then words else s"${words.head.toUpper}${words.tail}"

  /** The file a source role was imported from ("fixations.csv"): in the
    * shown run's dataset, else the latest dataset.
    */
  def sourceFile(role: SourceRole): String =
    val dataset = doc.presentation.shownRun
      .flatMap(doc.run)
      .flatMap(r => doc.dataset(r.dataset))
      .orElse(doc.datasets.lastOption)
    val source = dataset.flatMap(d =>
      role match
        case SourceRole.Fixations => d.sources.fixations
        case SourceRole.Trials    => d.sources.trials
    )
    source.fold(role.label)(_.path.value.split('/').last)

  /** "σ 2°", from the run's recipe; "scale 2" when the run is unknown. */
  def sigma(run: RunId, scale: ScaleIndex): String =
    doc
      .run(run)
      .flatMap(r => doc.analysis(r.analysis))
      .flatMap(_.recipe.scales.values.lift(scale.value))
      .fold(messages(ScaleNumber, scale.value.toString))(_.render)

  private def figureRun(figure: FigureId): Option[RunId] =
    doc.figures.find(_.id == figure).map(_.run)

  private def trialWithItem(key: TrialKey): String =
    model.items.item(key).fold(key.trial)(item => messages(CrumbQuery, key.trial, item))

  /** A crumb's words. `current` adds the pair's design ("· matched"), which
    * names the panel shown only at that depth.
    */
  def place(p: Place, current: Boolean): String = p match
    case Place.NewProject  => messages(CrumbNewProject)
    case Place.Dataset(id) =>
      val pending = doc.dataset(id).exists(!_.decision.isAdmitted)
      messages(if pending then CrumbDatasetDraft else CrumbDataset, id.label)
    case Place.Source(role)       => sourceFile(role)
    case Place.DataView(section)  => this.section(section)
    case Place.Analyses           => messages(CrumbAnalyses)
    case Place.Lineage(preset)    => lineage(preset)
    case Place.Revision(revision) =>
      if doc.draft.exists(_.id == revision) then messages(CrumbDraftRevision, revision.label)
      else messages(CrumbRevision, revision.label)
    case Place.Field(f)           => field(f)
    case Place.Summary(reporting) => messages(CrumbSummary, this.reporting(reporting))
    case Place.Group(_, group)    => group.label
    case Place.Figures            => messages(CrumbFigures)
    case Place.Figure(figure)     => figure.label
    case Place.At(ref)            => crumb(ref, current)

  private def crumb(ref: StudioRef, current: Boolean): String = ref match
    case StudioRef.Participant(p)         => p
    case StudioRef.Trial(key)             => key.trial
    case StudioRef.Fixation(trial, index) =>
      messages(CrumbFixation, trial.trial, index.value.toString)
    case StudioRef.SourceRecord(_, _, role, r) =>
      messages(CrumbRecord, sourceFile(role), Format.count(r.value.toLong))
    case StudioRef.ParticipantSummary(_, _, _, _, p) => p
    case StudioRef.GroupCell(_, _, _, group)         => group.label
    case StudioRef.ReportCell(_, _, _, group, role)  =>
      s"${SummaryText.reportGroup(group)} · ${SummaryText.roleName(role)}"
    case StudioRef.ReportParticipant(_, _, _, _, _, p)                => p
    case StudioRef.ReportContrast(_, _, _, role, minuend, subtrahend) =>
      s"${minuend.label} − ${subtrahend.label} · ${SummaryText.roleName(role)}"
    case StudioRef.ReportQueryRange(_, reporting, scale, role) =>
      SummaryText(
        eyes4s.studio.app.text.SummaryTextId.ReportQueryRangeCrumb,
        SummaryText.roleName(role)
      )
    case StudioRef.FigurePanel(_, letter)              => messages(CrumbPanel, letter.value)
    case StudioRef.WindowTally(dataset, region)        => tally(dataset, region)
    case StudioRef.TrialPlacementTally(dataset, trial) =>
      s"${dataset.label} · ${trial.label} · ${GeometryText(GeometryTextId.PlacementTally)}"
    case StudioRef.DesignTally(revision, count) => DesignText.tally(revision, count)
    case StudioRef.DisplayTally(dataset, count) => SourcesText.tally(dataset, count)
    case StudioRef.QueryTally(run, count)       => SummaryText.tally(run, count)
    case StudioRef.ReportTally(run, reporting, _, role, count) =>
      SummaryText.reportTally(run, reporting.value, role, count)
    case StudioRef.InventoryCount(_, _) => AdmissionLedgerVM.countLabel(ref)
    case StudioRef.TrialGroup(_, group) => TrialsNavigatorVM.groupLabel(group)
    case StudioRef.Result(run, address) =>
      val scale = address.scale
      address.value match
        case ResultAddress.ContrastRow(_, key)             => trialWithItem(key)
        case ResultAddress.PairRow(_, d, focal, reference) =>
          if current then messages(CrumbPairWithDesign, focal.trial, reference.trial, design(d))
          else messages(CrumbPair, focal.trial, reference.trial)
        case ResultAddress.Estimation(_, key) =>
          messages(CrumbMap, key.trial, sigma(run, scale))
        case ResultAddress.Reduction(_, d, key) =>
          messages(CrumbReduction, key.trial, design(d), sigma(run, scale))

  /** "r3 · Outside image frame" (S5.5's geometry counts). */
  private def tally(dataset: DatasetRevision, region: TallyRegion): String =
    val what = region match
      case TallyRegion.OutsideWindow => GeometryTextId.OutsideWindowTitle
      case TallyRegion.OutsideScreen => GeometryTextId.OutsideScreenTitle
    s"${dataset.label} · ${GeometryText(what)}"

  /** A status-bar path of places ("fixations.csv › Admission"). */
  def path(places: Vector[Place]): String =
    places.map(place(_, current = false)).mkString(messages(PathSeparator))

  private def panel(figure: FigureId, letter: PanelLetter): String =
    val scale = doc.figures
      .find(_.id == figure)
      .flatMap(_.panels.find(_.letter == letter))
      .map(_.scale)
    val runText = figureRun(figure).fold("")(r => r.label)
    scale match
      case Some(PanelScale.At(s)) =>
        messages(PathPanel, figure.label, letter.value, runText, s.render)
      case Some(PanelScale.AllScales) =>
        messages(PathPanel, figure.label, letter.value, runText, messages(AllScales))
      case _ => messages(PathPanelUnscaled, figure.label, letter.value, runText)

  /** One selected ref as the status bar shows it ("P17 › ret_07 × enc_03
    * (matched) · σ 2°").
    */
  def selected(ref: StudioRef): String =
    val sep = messages(PathSeparator)
    ref match
      case StudioRef.Participant(p) => p
      case StudioRef.Trial(key)     => key.participant + sep + key.trial
      case StudioRef.Fixation(t, i) =>
        t.participant + sep + t.trial + sep + messages(PathFixation, i.value.toString)
      case StudioRef.SourceRecord(t, fixation, role, r) =>
        val record = messages(PathRecord, sourceFile(role), Format.count(r.value.toLong))
        val head   = t.participant + sep + t.trial
        fixation.fold(messages(PathDetail, head, record)) { i =>
          messages(PathDetail, head + sep + messages(PathFixation, i.value.toString), record)
        }
      case StudioRef.ParticipantSummary(run, reporting, scale, group, p) =>
        group.fold(messages(PathSummary, p, this.reporting(reporting), sigma(run, scale)))(g =>
          messages(PathSummaryGroup, p, this.reporting(reporting), g.label, sigma(run, scale))
        )
      case StudioRef.GroupCell(run, reporting, scale, group) =>
        messages(PathGroupCell, group.label, this.reporting(reporting), sigma(run, scale))
      case StudioRef.ReportCell(run, reporting, scale, group, role) =>
        SummaryText.reportCell(group, role, this.reporting(reporting), sigma(run, scale))
      case StudioRef.ReportParticipant(run, reporting, scale, group, role, p) =>
        SummaryText
          .reportParticipant(p, group, role, this.reporting(reporting), sigma(run, scale))
      case StudioRef.ReportContrast(run, reporting, scale, role, minuend, subtrahend) =>
        SummaryText.reportContrast(
          minuend.label,
          subtrahend.label,
          role,
          this.reporting(reporting),
          sigma(run, scale)
        )
      case StudioRef.ReportQueryRange(run, reporting, scale, role) =>
        SummaryText.reportQueryRange(role, this.reporting(reporting), sigma(run, scale))
      case StudioRef.TrialPlacementTally(dataset, trial) =>
        s"${dataset.label} · ${trial.label} · ${GeometryText(GeometryTextId.PlacementTally)}"

      case StudioRef.FigurePanel(figure, letter)  => panel(figure, letter)
      case StudioRef.WindowTally(dataset, region) => tally(dataset, region)
      case StudioRef.DesignTally(revision, count) => DesignText.tally(revision, count)
      case StudioRef.DisplayTally(dataset, count) => SourcesText.tally(dataset, count)
      case StudioRef.QueryTally(run, count)       => SummaryText.tally(run, count)
      case StudioRef.ReportTally(run, reporting, _, role, count) =>
        SummaryText.reportTally(run, this.reporting(reporting), role, count)
      case StudioRef.InventoryCount(dataset, _) =>
        LedgerText(LedgerTextId.PathCount, dataset.label, AdmissionLedgerVM.countTitle(ref))
      case StudioRef.TrialGroup(dataset, group) =>
        LedgerText(LedgerTextId.PathCount, dataset.label, TrialsNavigatorVM.groupLabel(group))
      case StudioRef.Result(run, address) =>
        val s = sigma(run, address.scale)
        address.value match
          case ResultAddress.PairRow(_, d, focal, reference) =>
            messages(PathPair, focal.participant, focal.trial, reference.trial, design(d), s)
          case ResultAddress.ContrastRow(_, key) =>
            messages(PathDetail, key.participant + sep + key.trial, s)
          case ResultAddress.Estimation(_, key) =>
            messages(PathDetail, key.participant + sep + messages(PathMap, key.trial), s)
          case ResultAddress.Reduction(_, d, key) =>
            messages(
              PathDetail,
              key.participant + sep + messages(PathReduction, key.trial, design(d)),
              s
            )
