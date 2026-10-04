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
import eyes4s.studio.app.text.{Format, PanelText, PanelTextId, SummaryText, SummaryTextId}
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.backend.{
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

enum PanelsIntent derives CanEqual:
  case PairRead(pair: StudioRef, answer: PairAnswer)
  case Underlay(on: Boolean)

enum PanelsEffect derives CanEqual:
  /** Inspect `pair`, the reference panel's pair, at `address`. */
  case InspectPair(run: RunId, address: ResultAddress, pair: StudioRef)

/** Compare's query and reference trial panels (ticket S8.2; Main.dc.html,
  * panels): the query the Compare trail is at, and its reference, which is
  * the pair the trail names or else the query's matched reference. The
  * inspected pair's score is asked of the backend once per pair; the
  * query's contrast comes from the run's query rows. Pure; a host performs
  * the effects.
  */
final case class TrialPanels(
    focus: Option[PanelFocus],
    inspected: Option[(StudioRef, PairAnswer)],
    underlay: Boolean
) derives CanEqual

/** One panel's readout: what it reports, its value, and the ref it traces to. */
final case class PanelReadout(text: String, ref: StudioRef) derives CanEqual

/** One trial panel: its role pill, title, trial, the readout it carries. */
final case class TrialPanelVM(
    role: PanelRole,
    title: String,
    trial: TrialKey,
    readout: PanelReadout
) derives CanEqual

/** The reference panel's extras: the way back to the matched reference when
  * a control is shown, and the matched reference's identity, always shown.
  */
final case class ReferenceExtras(back: Option[(String, Intent)], matched: String)
    derives CanEqual

final case class TrialPanelsVM(
    empty: Option[String],
    query: Option[TrialPanelVM],
    reference: Option[TrialPanelVM],
    extras: Option[ReferenceExtras],
    underlay: Boolean
) derives CanEqual

object TrialPanels:

  val empty: TrialPanels = TrialPanels(None, None, false)

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
      rows.find(_.query == focus.query).map(r => (PairDesign.Matched, r.matched))
    )

  /** Follows the model and the run's query rows: a new reference pair is
    * inspected once.
    */
  def sync(
      s: TrialPanels,
      m: AppModel,
      rows: Vector[QueryRow]
  ): (TrialPanels, Vector[PanelsEffect]) =
    val focus = focusOf(m)
    val pair  = focus.flatMap(f => referenceOf(f, rows).map((d, r) => (f, d, r)))
    val next  = s.copy(focus = focus)
    pair match
      case Some((f, design, reference)) =>
        val ref = f.pair(design, reference)
        if s.inspected.exists(_._1 == ref) then (next, Vector.empty)
        else
          (
            next.copy(inspected = None),
            Vector(
              PanelsEffect.InspectPair(
                f.run,
                ResultAddress.PairRow(f.scale.value, design, f.query, reference),
                ref
              )
            )
          )
      case None => (next.copy(inspected = None), Vector.empty)

  /** A backend answer or a user action. An answer is kept only for a pair
    * of the focused query; any other is stale.
    */
  def update(s: TrialPanels, intent: PanelsIntent): TrialPanels = intent match
    case PanelsIntent.PairRead(pair, answer) =>
      val ofFocus = (s.focus, pair) match
        case (Some(f), StudioRef.Pair(run, scale, _, focal, _)) =>
          run == f.run && scale == f.scale && focal == f.query
        case _ => false
      if ofFocus then s.copy(inspected = Some((pair, answer))) else s
    case PanelsIntent.Underlay(on) => s.copy(underlay = on)

  /** The panels' view-model. `rows` are the run's query rows and `scales`
    * the run's scale labels; titles take items from the rows and, for a
    * control, from the inspected pair.
    */
  def vm(s: TrialPanels, rows: Vector[QueryRow], scales: Vector[String]): TrialPanelsVM =
    s.focus match
      case None =>
        TrialPanelsVM(Some(PanelText(PanelTextId.NoQuery)), None, None, None, s.underlay)
      case Some(f) =>
        val row   = rows.find(_.query == f.query)
        val sigma = scales.lift(f.scale.value).getOrElse(f.scale.value.toString)
        val query = TrialPanelVM(
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
          )
        )
        val reference = referenceOf(f, rows).map { (design, key) =>
          val ref    = f.pair(design, key)
          val answer = s.inspected.collect { case (`ref`, a) => a }
          val item   = design match
            case PairDesign.Matched => row.map(_.item)
            case PairDesign.Control =>
              answer.collect { case PairAnswer.Answered(Inspection.Pair(_, item, _)) => item }
          TrialPanelVM(
            PanelRole.of(design),
            title(key, item),
            key,
            PanelReadout(PanelText(PanelTextId.PairReadout, sigma, scoreOf(answer)), ref)
          )
        }
        val extras = row.map { r =>
          val matched = f.pair(PairDesign.Matched, r.matched)
          ReferenceExtras(
            Option.when(reference.exists(_.role == PanelRole.Control))(
              (PanelText(PanelTextId.BackToMatched), Intent.Explain(Place.At(matched)))
            ),
            PanelText(PanelTextId.MatchedIs, title(r.matched, Some(r.item)))
          )
        }
        TrialPanelsVM(None, Some(query), reference, extras, s.underlay)

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
    case QueryStatus.Failed(_)             => SummaryText(SummaryTextId.StatusFailed)
    case QueryStatus.NoMatch(_)            => SummaryText(SummaryTextId.StatusNoMatch)
    case QueryStatus.NotAdmitted(_)        => SummaryText(SummaryTextId.StatusNotAdmitted)

  private def scoreOf(answer: Option[PairAnswer]): String = answer match
    case None => PanelText(PanelTextId.Reading)
    case Some(PairAnswer.Answered(Inspection.Pair(_, _, score)))   => Format.decimal(score, 2)
    case Some(PairAnswer.Answered(Inspection.Unscored(_, status))) =>
      PanelText(PanelTextId.Unscored, statusWord(status))
    case Some(PairAnswer.Answered(other)) =>
      PanelText(PanelTextId.Unscored, other.productPrefix)
    case Some(PairAnswer.Refused(e))  => PanelText(PanelTextId.Unscored, e.message)
    case Some(PairAnswer.Failed(why)) => PanelText(PanelTextId.Unscored, why)
