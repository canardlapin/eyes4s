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

import eyes4s.studio.app.nav.Place
import eyes4s.studio.app.vm.{A11yRole, FocusStop}
import eyes4s.studio.app.text.{
  Format,
  PanelText,
  PanelTextId,
  SummaryText,
  SummaryTextId,
  TrialText,
  TrialTextId
}
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.plan.{MapPlacement, OffWindowPolicy}
import eyes4s.studio.app.plot.{
  ColumnFormat,
  ColumnId,
  PlotColumn,
  PlotRow,
  PlotSource,
  PlotSourceError,
  PlotValue
}
import eyes4s.studio.core.assets.{AssetLink, AssetRef, Display}
import eyes4s.studio.core.backend.{
  AnalysisRevision,
  BackendError,
  Inspection,
  PairDesign,
  QueryRow,
  QueryStatus,
  ResultAddress,
  RunId,
  TrialKey
}
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}

/** The role a trial panel shows its trial in (DESIGN_SPEC section 5): the
  * query, or the reference by its design.
  */
enum PanelRole derives CanEqual:
  case Query, Matched, Control

  def label: String = this match
    case Query   => PanelText(PanelTextId.Query)
    case Matched => PanelText(PanelTextId.Matched)
    case Control => PanelText(PanelTextId.Control)

object PanelRole:
  def of(design: PairDesign): PanelRole = design match
    case PairDesign.Matched => Matched
    case PairDesign.Control => Control

/** What Compare's trail points the panels at: a query of a run at a scale,
  * and the reference the trail names, if it names one.
  */
final case class PanelFocus(
    run: RunId,
    scale: ScaleIndex,
    query: TrialKey,
    reference: Option[(PairDesign, TrialKey)]
) derives CanEqual:

  def contrast: StudioRef = StudioRef.QueryContrast(run, scale, query)

  def pair(design: PairDesign, reference: TrialKey): StudioRef =
    StudioRef.Pair(run, scale, design, query, reference)

/** The backend's answer for an inspected pair. */
enum PairAnswer derives CanEqual:
  case Answered(inspection: Inspection)
  case Refused(error: BackendError)
  case Failed(reason: String)

/** The run Compare shows and what the panels read of it: its analysis
  * revision and its query rows. A focus on any other run is no focus.
  */
final case class ShownRun(run: RunId, revision: AnalysisRevision, rows: Vector[QueryRow])
    derives CanEqual

/** A trial's content under one analysis revision: the identity of what the
  * panels read, so a revision's content never stands in for another's.
  */
final case class ContentKey(revision: AnalysisRevision, trial: TrialKey) derives CanEqual

enum PanelsIntent derives CanEqual:
  case PairRead(pair: StudioRef, answer: PairAnswer)
  case ContentRead(key: ContentKey, answer: Either[ContentError, TrialContent])
  case Underlay(on: Boolean)

  /** Ask again for what failed: the pair's score and unreadable content. */
  case Retry

enum PanelsEffect derives CanEqual:
  /** Inspect `pair`, the reference panel's pair, at `address`. */
  case InspectPair(run: RunId, address: ResultAddress, pair: StudioRef)

  /** Read a trial's content under an analysis revision. */
  case ReadContent(key: ContentKey)

/** What a panel's stage shows: the trial's content, or why not yet or not. */
enum PanelContent derives CanEqual:
  case Reading
  case Shown(content: TrialContent)
  case Unavailable(reason: String)

/** Compare's query and reference trial panels (ticket S8.2; Main.dc.html,
  * panels): the query the Compare trail is at, and its reference, which is
  * the pair the trail names or else the query's matched reference. The
  * inspected pair's score is asked of the backend once per pair; the
  * query's contrast comes from the run's query rows. Pure; a host performs
  * the effects.
  */
final case class TrialPanels(
    focus: Option[PanelFocus],
    inspected: Option[(StudioRef, Option[PairAnswer])],
    underlay: Boolean,
    contents: Map[ContentKey, Option[Either[ContentError, TrialContent]]]
) derives CanEqual

/** One panel's readout: what it reports, its value, and the ref it traces to. */
final case class PanelReadout(text: String, ref: StudioRef) derives CanEqual

/** One trial panel: its role pill, title, trial, the readout it carries,
  * what its stage shows and its fixation count.
  */
final case class TrialPanelVM(
    role: PanelRole,
    title: String,
    trial: TrialKey,
    readout: PanelReadout,
    content: PanelContent,
    count: String
) derives CanEqual

/** The reference panel's extras: the way back to the matched reference when
  * a control is shown, and the matched reference's identity, always shown.
  */
final case class ReferenceExtras(back: Option[(String, Intent)], matched: String)
    derives CanEqual

/** The query's remembered image: the matched reference's stored image, which
  * the query panel underlays when `shown`.
  */
final case class Remembered(asset: AssetRef, shown: Boolean) derives CanEqual

/** The panels' view-model. `rememberedNote` says the remembered image is
  * not shown when the underlay is on but the query has no stored remembered
  * image to underlay (S4.3b); `retry` is offered while something failed.
  */
final case class TrialPanelsVM(
    empty: Option[String],
    query: Option[TrialPanelVM],
    reference: Option[TrialPanelVM],
    extras: Option[ReferenceExtras],
    underlay: Boolean,
    remembered: Option[Remembered],
    rememberedNote: Option[String],
    retry: Option[String]
) derives CanEqual

object TrialPanels:

  val empty: TrialPanels = TrialPanels(None, None, false, Map.empty)

  /** Where Compare's trail points, if it is in the `shown` run. */
  def focusIn(m: AppModel, shown: Option[ShownRun]): Option[PanelFocus] =
    focusOf(m).filter(f => shown.exists(_.run == f.run))

  /** Where Compare's trail points: its last query contrast or pair. */
  def focusOf(m: AppModel): Option[PanelFocus] =
    m.navigation.trail(Perspective.Compare).reverseIterator.collectFirst {
      case Place.At(StudioRef.QueryContrast(run, scale, key)) =>
        PanelFocus(run, scale, key, None)
      case Place.At(StudioRef.Pair(run, scale, design, focal, reference)) =>
        PanelFocus(run, scale, focal, Some((design, reference)))
    }

  /** The reference the panel shows under `focus`: the trail's pair, else the
    * query's matched reference from the run's rows.
    */
  def referenceOf(focus: PanelFocus, rows: Vector[QueryRow]): Option[(PairDesign, TrialKey)] =
    focus.reference.orElse(
      rows.find(_.query == focus.query).flatMap(_.matched.map(PairDesign.Matched -> _))
    )

  /** Follows the model and the shown run: a new reference pair is inspected
    * once, and each trial the panels need is read once per revision. A trail
    * in another run than the one shown is no focus.
    */
  def sync(
      s: TrialPanels,
      m: AppModel,
      shown: Option[ShownRun]
  ): (TrialPanels, Vector[PanelsEffect]) =
    val focus = focusIn(m, shown)
    val rows  = shown.fold(Vector.empty[QueryRow])(_.rows)
    val pair  = focus.flatMap(f => referenceOf(f, rows).map((d, r) => (f, d, r)))
    // The pair the reference panel shows is inspected once; its answer is
    // `None` while it is asked.
    val (inspected, inspect) = pair match
      case Some((f, design, reference)) =>
        val ref = f.pair(design, reference)
        if s.inspected.exists(_._1 == ref) then (s.inspected, Vector.empty)
        else
          (
            Some((ref, None)),
            Vector(
              PanelsEffect.InspectPair(
                f.run,
                ResultAddress.PairRow(f.scale.value, design, f.query, reference),
                ref
              )
            )
          )
      case None => (None, Vector.empty)
    // The trials the panels need, under the shown run's revision: the query,
    // its reference and its matched reference, whose image is the query's
    // remembered image. Any other content is let go.
    val needed = (for
      f   <- focus
      run <- shown
    yield Vector(
      Some(f.query),
      pair.map(_._3),
      rows.find(_.query == f.query).flatMap(_.matched)
    ).flatten.distinct.map(ContentKey(run.revision, _))).getOrElse(Vector.empty)
    val kept     = s.contents.filter((k, _) => needed.contains(k))
    val missing  = needed.filterNot(kept.contains)
    val reads    = missing.map(PanelsEffect.ReadContent(_))
    val contents = kept ++ missing.map(_ -> None)
    (TrialPanels(focus, inspected, s.underlay, contents), inspect ++ reads)

  /** A backend answer or a user action. An answer is kept only while the
    * panels still wait on it; any other is stale. Retry forgets what failed,
    * so the next sync asks for it again.
    */
  def update(s: TrialPanels, intent: PanelsIntent): TrialPanels = intent match
    case PanelsIntent.PairRead(pair, answer) =>
      if s.inspected.contains((pair, None)) then s.copy(inspected = Some((pair, Some(answer))))
      else s
    case PanelsIntent.ContentRead(key, answer) =>
      if s.contents.get(key).contains(None) then
        s.copy(contents = s.contents.updated(key, Some(answer)))
      else s
    case PanelsIntent.Underlay(on) => s.copy(underlay = on)
    case PanelsIntent.Retry        =>
      s.copy(
        inspected = s.inspected.filterNot((_, a) => a.exists(failed)),
        contents = s.contents.filterNot((_, a) => a.exists(unreadable))
      )

  private def failed(a: PairAnswer): Boolean = a match
    case PairAnswer.Answered(_) => false
    case _                      => true

  private def unreadable(a: Either[ContentError, TrialContent]): Boolean = a match
    case Left(ContentError.Unreadable(_, _)) => true
    case _                                   => false

  /** The panels' view-model. `rows` are the run's query rows and `scales`
    * the run's scale labels; titles take items from the rows and, for a
    * control, from the inspected pair.
    */
  def vm(s: TrialPanels, shown: Option[ShownRun], scales: Vector[String]): TrialPanelsVM =
    (s.focus.filter(f => shown.exists(_.run == f.run)), shown) match
      case (Some(f), Some(run)) => focused(s, f, run, scales)
      case _                    =>
        TrialPanelsVM(
          Some(PanelText(PanelTextId.NoQuery)),
          None,
          None,
          None,
          s.underlay,
          None,
          None,
          None
        )

  private def focused(
      s: TrialPanels,
      f: PanelFocus,
      shown: ShownRun,
      scales: Vector[String]
  ): TrialPanelsVM =
    val rows                    = shown.rows
    def contentKey(k: TrialKey) = ContentKey(shown.revision, k)
    val row                     = rows.find(_.query == f.query)
    val sigma                   = scales.lift(f.scale.value).getOrElse(f.scale.value.toString)
    val query                   = TrialPanelVM(
      PanelRole.Query,
      title(f.query, row.map(_.item)),
      f.query,
      PanelReadout(
        PanelText(
          PanelTextId.ContrastReadout,
          sigma,
          row.fold(PanelText(PanelTextId.Reading))(said(f.scale))
        ),
        f.contrast
      ),
      contentOf(s, contentKey(f.query)),
      countOf(s, contentKey(f.query))
    )
    val reference = referenceOf(f, rows).map { (design, key) =>
      val ref    = f.pair(design, key)
      val answer = s.inspected.collect { case (`ref`, Some(a)) => a }
      val item   = design match
        case PairDesign.Matched => row.map(_.item)
        case PairDesign.Control =>
          answer.collect { case PairAnswer.Answered(Inspection.Pair(_, item, _)) => item }
      TrialPanelVM(
        PanelRole.of(design),
        title(key, item),
        key,
        PanelReadout(PanelText(PanelTextId.PairReadout, sigma, scoreOf(answer)), ref),
        contentOf(s, contentKey(key)),
        countOf(s, contentKey(key))
      )
    }
    val extras = row.flatMap { r =>
      r.matched.map { key =>
        val matched = f.pair(PairDesign.Matched, key)
        ReferenceExtras(
          Option.when(reference.exists(_.role == PanelRole.Control))(
            (PanelText(PanelTextId.BackToMatched), Intent.Explain(Place.At(matched)))
          ),
          PanelText(PanelTextId.MatchedIs, title(key, Some(r.item)))
        )
      }
    }
    val remembered = row
      .flatMap(_.matched)
      .flatMap(key => s.contents.get(contentKey(key)).flatten)
      .collect { case Right(c) => c.display.display }
      .collect { case Display.Image(AssetLink.Present(asset)) =>
        Remembered(asset, s.underlay)
      }
    val failure = s.inspected.flatMap(_._2).exists(failed) ||
      s.contents.values.flatten.exists(unreadable)
    TrialPanelsVM(
      None,
      Some(query),
      reference,
      extras,
      s.underlay,
      remembered,
      Option.when(s.underlay && remembered.isEmpty)(TrialText(TrialTextId.RememberedHidden)),
      Option.when(failure)(PanelText(PanelTextId.Retry))
    )

  /** The controls inside the query panel's own focus stop: its underlay
    * toggle, while it shows a query.
    */
  def queryStops(vm: TrialPanelsVM): Vector[FocusStop] =
    vm.query.toVector.map(_ =>
      FocusStop(A11yRole.ToggleButton, PanelText(PanelTextId.Underlay))
    )

  /** The controls inside the reference panel's own focus stop: the way back
    * to the matched reference, while a control is shown.
    */
  def referenceStops(vm: TrialPanelsVM): Vector[FocusStop] =
    vm.extras.flatMap(_.back).toVector.map((label, _) => FocusStop(A11yRole.Button, label))

  /** A trial's fixations as its Table tab lists them, each row the
    * fixation's ref; every value as the content source served it.
    */
  def fixationTable(content: TrialContent): Either[PlotSourceError, PlotSource] =
    import PanelTextId.*
    def column(id: String, header: PanelTextId, format: ColumnFormat) =
      ColumnId.of(id).map(PlotColumn(_, PanelText(header), format))
    def placed(p: MapPlacement): String = p match
      case MapPlacement.InWindow                                 => PanelText(InMap)
      case MapPlacement.DroppedInitial                           => PanelText(DroppedInitial)
      case MapPlacement.OutsideScreen                            => PanelText(OutsideScreen)
      case MapPlacement.OutsideWindow(OffWindowPolicy.Exclude)   => PanelText(OutsideExcluded)
      case MapPlacement.OutsideWindow(OffWindowPolicy.FailTrial) => PanelText(OutsideFails)
      case MapPlacement.TrialFailed(t)                           =>
        PanelText(InWindowTrialFails, t.outsideWindow.toString, t.total.toString)
    for
      index    <- column("fixation", FixationHeader, ColumnFormat.Count)
      x        <- column("x", XHeader, ColumnFormat.Decimal(1))
      y        <- column("y", YHeader, ColumnFormat.Decimal(1))
      onset    <- column("onset", OnsetHeader, ColumnFormat.Count)
      duration <- column("duration", DurationHeader, ColumnFormat.Count)
      place    <- column("placement", PlacementHeader, ColumnFormat.Label)
      source   <- PlotSource(
        PanelText(TableCaption, content.display.trial.label),
        Vector(index, x, y, onset, duration, place),
        content.fixations.map(f =>
          PlotRow(
            f.ref,
            Vector(
              PlotValue.Number(f.index.value.toDouble),
              PlotValue.Number(f.screenX),
              PlotValue.Number(f.screenY),
              PlotValue.Number(f.onsetMs.toDouble),
              PlotValue.Number(f.durationMs.toDouble),
              PlotValue.Text(placed(f.placement))
            )
          )
        )
      )
    yield source

  private def contentOf(s: TrialPanels, key: ContentKey): PanelContent =
    s.contents.get(key).flatten match
      case None            => PanelContent.Reading
      case Some(Right(c))  => PanelContent.Shown(c)
      case Some(Left(err)) => PanelContent.Unavailable(err.message)

  private def countOf(s: TrialPanels, key: ContentKey): String =
    s.contents
      .get(key)
      .flatten
      .collect { case Right(c) => c.fixations.size }
      .fold("")(n => PanelText(PanelTextId.Fixations, n.toString))

  private def title(key: TrialKey, item: Option[String]): String =
    item.fold(PanelText(PanelTextId.TitleNoItem, key.participant, key.trial))(i =>
      PanelText(PanelTextId.Title, key.participant, key.trial, i)
    )

  private def said(scale: ScaleIndex)(r: QueryRow): String = r.status match
    case QueryStatus.Contributing(_, _, d) =>
      d.lift(scale.value).fold(SummaryText(SummaryTextId.NotApplicable))(Format.signed(_, 2))
    case other => statusWord(other)

  private def statusWord(status: QueryStatus): String = status match
    case QueryStatus.Contributing(_, _, _) => SummaryText(SummaryTextId.StatusContributing)
    case QueryStatus.Failed(_) | QueryStatus.FailedAtScales(_) =>
      SummaryText(SummaryTextId.StatusFailed)
    case QueryStatus.NoMatch(_)     => SummaryText(SummaryTextId.StatusNoMatch)
    case QueryStatus.NotAdmitted(_) => SummaryText(SummaryTextId.StatusNotAdmitted)

  private def scoreOf(answer: Option[PairAnswer]): String = answer match
    case None => PanelText(PanelTextId.Reading)
    case Some(PairAnswer.Answered(Inspection.Pair(_, _, score)))   => Format.decimal(score, 2)
    case Some(PairAnswer.Answered(Inspection.Unscored(_, status))) =>
      PanelText(PanelTextId.Unscored, statusWord(status))
    case Some(PairAnswer.Answered(other)) =>
      PanelText(
        PanelTextId.NotAPairScore,
        other match
          case Inspection.Contrast(_, _, _, _) => PanelText(PanelTextId.KindContrast)
          case Inspection.Reduction(_, _, _)   => PanelText(PanelTextId.KindReduction)
          case _                               => PanelText(PanelTextId.KindOther)
      )
    case Some(PairAnswer.Refused(e))  => PanelText(PanelTextId.Unscored, e.message)
    case Some(PairAnswer.Failed(why)) => PanelText(PanelTextId.Unscored, why)
