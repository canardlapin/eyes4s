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

package eyes4s.studio.app.explore

import eyes4s.studio.app.AppModel
import eyes4s.studio.app.geometry.Loading
import eyes4s.studio.app.text.{Format, NavigatorText, NavigatorTextId}
import eyes4s.studio.app.vm.{A11yRole, FocusStop}
import eyes4s.studio.core.assets.{AssetRegistry, DisplayKind, DisplayState}
import eyes4s.studio.core.backend.{DatasetRevision, LedgerEntry, TrialDisposition, TrialKey}
import eyes4s.studio.core.selection.{StudioRef, TrialGrouping}

/** How a row of the navigator is set: a participant, one of its phases, the
  * collapsed run of a phase's trials, a trial, or an item.
  */
enum NavigatorRowKind derives CanEqual:
  case Participant, Phase, Range, Trial, Item

/** A trial's glyph: what its display showed, a display whose image is
  * missing, or a trial eyes4s did not admit.
  */
enum TrialGlyph derives CanEqual:
  case Image, Blank, BlankWithCross, Cue, Unknown, MissingAsset, NotAdmitted

/** One row of a navigator pane. `activate` is what Enter or a double click
  * does: open or close a group, or explore a trial. `ref` is what the row's
  * figures and words describe.
  */
final case class NavigatorRowVM(
    kind: NavigatorRowKind,
    depth: Int,
    label: String,
    item: String,
    detail: String,
    glyph: Option[TrialGlyph],
    warn: Boolean,
    open: Option[Boolean],
    selected: Boolean,
    accessible: String,
    activate: Option[NavigatorIntent],
    ref: Option[StudioRef]
) derives CanEqual

/** A glyph of the legend, with its name. */
final case class LegendVM(glyph: TrialGlyph, label: String) derives CanEqual

/** Everything one navigator pane (Trials or Items) shows. */
final case class NavigatorPaneVM(
    title: String,
    filterLabel: String,
    filterPrompt: String,
    filter: String,
    list: String,
    rows: Vector[NavigatorRowVM],
    note: Option[String],
    retry: Option[String],
    legend: Vector[LegendVM],
    footer: String
) derives CanEqual

object TrialsNavigatorVM:
  import NavigatorTextId.*

  private def t(id: NavigatorTextId, args: String*): String = NavigatorText(id, args*)

  private def n(value: Int): String = Format.count(value.toLong)

  /** "Image", "Blank + cross": a glyph's name in the legend. */
  def glyphName(glyph: TrialGlyph): String = glyph match
    case TrialGlyph.Image          => t(KindImage)
    case TrialGlyph.Blank          => t(KindBlank)
    case TrialGlyph.BlankWithCross => t(KindBlankWithCross)
    case TrialGlyph.Cue            => t(KindCue)
    case TrialGlyph.Unknown        => t(KindUnknown)
    case TrialGlyph.MissingAsset   => t(KindMissing)
    case TrialGlyph.NotAdmitted    => t(KindNotAdmitted)

  private def kindGlyph(kind: DisplayKind): TrialGlyph = kind match
    case DisplayKind.Image                  => TrialGlyph.Image
    case DisplayKind.Blank                  => TrialGlyph.Blank
    case DisplayKind.BlankWithFixationCross => TrialGlyph.BlankWithCross
    case DisplayKind.Cue                    => TrialGlyph.Cue
    case DisplayKind.Unknown                => TrialGlyph.Unknown

  /** A trial's glyph: not admitted, else its display (a missing asset as
    * its own state); none until the displays are read.
    */
  def glyph(entry: LedgerEntry, displays: Option[AssetRegistry]): Option[TrialGlyph] =
    if entry.disposition != TrialDisposition.Admitted then Some(TrialGlyph.NotAdmitted)
    else
      displays
        .flatMap(_.display(entry.trial))
        .map(d =>
          d.state match
            case DisplayState.MissingAsset(_, _) => TrialGlyph.MissingAsset
            case _                               => kindGlyph(d.kind)
        )

  /** The missing file `entry`'s display names, if its asset is missing. */
  private def missingFile(entry: LedgerEntry, displays: Option[AssetRegistry]) =
    displays
      .flatMap(_.display(entry.trial))
      .collect(d => d.state match { case DisplayState.MissingAsset(_, f) => f.value })

  private def isMissing(entry: LedgerEntry, displays: Option[AssetRegistry]): Boolean =
    missingFile(entry, displays).isDefined

  /** A trial's short status ("absent · no records"), if it has one. */
  def status(entry: LedgerEntry, displays: Option[AssetRegistry]): Option[String] =
    val disposition = entry.disposition match
      case TrialDisposition.Admitted       => None
      case TrialDisposition.Absent         => Some(t(StatusAbsent))
      case TrialDisposition.NoFixations    => Some(t(StatusNoFixations))
      case TrialDisposition.Quarantined(c) =>
        Some(t(StatusQuarantined, c.code.stripPrefix("quarantine.")))
    val missing = Option.when(isMissing(entry, displays))(t(StatusImageMissing))
    Option((disposition.toVector ++ missing).mkString(" · ")).filter(_.nonEmpty)

  /** A trial's status in words, for its accessible name. */
  private def longStatus(
      entry: LedgerEntry,
      displays: Option[AssetRegistry],
      inventory: String
  ): Vector[String] =
    val disposition = entry.disposition match
      case TrialDisposition.Admitted       => None
      case TrialDisposition.Absent         => Some(t(LongAbsent, inventory))
      case TrialDisposition.NoFixations    => Some(t(LongNoFixations))
      case TrialDisposition.Quarantined(c) => Some(t(LongQuarantined, c.message))
    disposition.toVector ++ missingFile(entry, displays).map(t(LongImageMissing, _))

  /** "ret_07", or "ret_07 occ 2" for a later occurrence. */
  def trialName(key: TrialKey): String =
    if key.occurrence == 1 then key.trial else t(Occurrence, key.trial, key.occurrence.toString)

  private def itemName(item: String): String = if item.isEmpty then t(NoItem) else item

  /** "ret_01–20": a run of trials, the last named by what differs. */
  def range(first: TrialKey, last: TrialKey): String =
    val a    = trialName(first)
    val b    = trialName(last)
    val cut  = a.lastIndexOf('_')
    val tail =
      if cut >= 0 && b.length > cut && b.take(cut + 1) == a.take(cut + 1) then b.drop(cut + 1)
      else b
    t(Range, a, tail)

  private def matches(entry: LedgerEntry, filter: String): Boolean =
    val f = filter.trim.toLowerCase
    f.isEmpty || Vector(entry.trial.participant, entry.trial.trial, entry.item)
      .exists(_.toLowerCase.contains(f))

  /** "P17 · Retrieval", "beach-042": what a group of trials names. */
  def groupLabel(group: TrialGrouping): String = group match
    case TrialGrouping.PhaseOf(participant, phase) => t(GroupLabel, participant, phase.label)
    case TrialGrouping.MatchedOn(item)             => itemName(item)

  /** "1 image missing", "2 images missing". */
  private def imagesMissing(count: Int): String =
    if count == 1 then t(ImageMissing, n(count)) else t(ImagesMissing, n(count))

  /** The served registry, if the displays were read and served, with the
    * document's repairs applied (S5.7); a repair that no longer applies
    * leaves the registry as served.
    */
  private def repaired(
      nav: TrialsNavigator,
      model: AppModel
  ): Option[Either[String, AssetRegistry]] =
    nav.displays.toOption.collect { case DisplaySource.Served(r) =>
      r.withRelinks(model.document.relinks.of(r.dataset)).left.map(_.message)
    }

  /** The served registry with the document's repairs; a repair that cannot
    * be applied is said in the panes' note, never swallowed.
    */
  private def registry(nav: TrialsNavigator, model: AppModel): Option[AssetRegistry] =
    repaired(nav, model).flatMap(_.toOption)

  private def openness(open: Boolean): String = if open then t(Expanded) else t(Collapsed)

  /** The status of the panes, when they cannot list everything: still
    * reading, or a read that failed.
    */
  private def notes(nav: TrialsNavigator, model: AppModel): (Option[String], Option[String]) =
    val label = nav.dataset.fold("")(_.id.label)
    val read  = nav.entries match
      case Loading.Waiting | Loading.Idle if nav.dataset.isDefined => Some(t(Reading, label))
      case Loading.Failed(why) => Some(t(ReadFailed, label, why))
      case _                   => None
    val shown = nav.displays match
      case Loading.Failed(why)                    => Some(t(DisplaysFailed, label, why))
      case Loading.Ready(DisplaySource.NotServed) => Some(t(DisplaysNotServed, label))
      case _ => repaired(nav, model).flatMap(_.left.toOption).map(t(DisplaysFailed, label, _))
    val failed = (nav.entries, nav.displays) match
      case (Loading.Failed(_), _) | (_, Loading.Failed(_)) => true
      case _                                               => false
    val note = Option((read.toVector ++ shown).mkString(" ")).filter(_.nonEmpty)
    (
      if nav.dataset.isEmpty then Some(t(NoDataset)) else note,
      Option.when(failed)(t(Retry))
    )

  private def inventoryFile(nav: TrialsNavigator): String =
    nav.dataset
      .flatMap(_.sources.trials)
      .fold(t(InventoryFallback))(_.path.value.split('/').last)

  private def trialRow(
      entry: LedgerEntry,
      depth: Int,
      label: String,
      displays: Option[AssetRegistry],
      inventory: String,
      selected: Option[TrialKey]
  ): NavigatorRowVM =
    val words = Vector(s"$label ${entry.item}".trim) ++ longStatus(entry, displays, inventory)
    NavigatorRowVM(
      NavigatorRowKind.Trial,
      depth,
      label,
      itemName(entry.item),
      status(entry, displays).getOrElse(""),
      glyph(entry, displays),
      entry.disposition != TrialDisposition.Admitted || isMissing(entry, displays),
      None,
      selected.contains(entry.trial),
      words.mkString(", "),
      Some(NavigatorIntent.OpenTrial(entry.trial)),
      Some(StudioRef.Trial(entry.trial))
    )

  private def legend(entries: Vector[LedgerEntry], displays: Option[AssetRegistry]) =
    val shown = entries.flatMap(glyph(_, displays)).toSet
    TrialGlyph.values.toVector.filter(shown.contains).map(g => LegendVM(g, glyphName(g)))

  /** A participant's figures: its trials, then those quarantined, those
    * with no fixations (as the admission ledger counts them apart), absent
    * and missing their image, when there are any.
    */
  private def participantDetail(
      entries: Vector[LedgerEntry],
      displays: Option[AssetRegistry]
  ): String =
    val quarantined = entries.count(e =>
      e.disposition match
        case TrialDisposition.Quarantined(_) => true
        case _                               => false
    )
    val noFixations = entries.count(_.disposition == TrialDisposition.NoFixations)
    val absent      = entries.count(_.disposition == TrialDisposition.Absent)
    val missing     = entries.count(isMissing(_, displays))
    (Vector(t(ParticipantDetail, n(entries.size))) ++
      Option.when(quarantined > 0)(t(Quarantined, n(quarantined))) ++
      Option.when(noFixations > 0)(t(NoFixationsCount, n(noFixations))) ++
      Option.when(absent > 0)(t(Absent, n(absent))) ++
      Option.when(missing > 0)(imagesMissing(missing))).mkString(" · ")

  /** The kind every trial of `entries` showed, when they agree. */
  private def commonKind(
      entries: Vector[LedgerEntry],
      displays: Option[AssetRegistry]
  ): Option[DisplayKind] =
    displays.flatMap { r =>
      val kinds = entries.flatMap(e => r.display(e.trial).map(_.kind)).distinct
      Option
        .when(kinds.size == 1 && entries.forall(e => r.display(e.trial).isDefined))(kinds.head)
    }

  /** The Trials pane: participant → phase → trial.
    *
    * Board parity (Explore.dc.html, left): the board's per-trial fixation
    * count (`{{t.nf}}`, "enc_03 · 13") is not shown. It waits on serving
    * fixation timing from the backend (bead bd-01M425ARKH0V5Z124VJQNFKTBT);
    * the ledger entries hold no fixation count.
    */
  def trials(nav: TrialsNavigator, model: AppModel): NavigatorPaneVM =
    val (note, retry)                    = notes(nav, model)
    val entries                          = nav.entries.toOption.getOrElse(Vector.empty)
    val displays                         = registry(nav, model)
    val selected                         = TrialsNavigator.selected(model)
    val inventory                        = inventoryFile(nav)
    val filtering                        = nav.filter.trim.nonEmpty
    val label                            = nav.dataset.fold("")(_.id.label)
    val dataset: Option[DatasetRevision] = nav.dataset.map(_.id)
    val participants                     = entries.map(_.trial.participant).distinct
    val rows                             = participants.flatMap { p =>
      val all    = entries.filter(_.trial.participant == p)
      val shown  = all.filter(matches(_, nav.filter))
      val group  = NavigatorGroup.Participant(p)
      val open   = filtering || TrialsNavigator.isOpen(nav, group, model)
      val detail =
        if filtering then t(Matching, n(shown.size)) else participantDetail(all, displays)
      val header = NavigatorRowVM(
        NavigatorRowKind.Participant,
        0,
        p,
        "",
        detail,
        None,
        false,
        Some(open),
        false,
        t(GroupAccessible, p, detail, openness(open)),
        Some(NavigatorIntent.Toggle(group)),
        Some(StudioRef.Participant(p))
      )
      val phases = shown.map(_.trial.phase).distinct.flatMap { phase =>
        val inPhase = shown.filter(_.trial.phase == phase)
        val pg      = NavigatorGroup.PhaseOf(p, phase)
        val pref    = dataset.map(StudioRef.TrialGroup(_, TrialGrouping.PhaseOf(p, phase)))
        val popen   = filtering || TrialsNavigator.isOpen(nav, pg, model)
        val head    = commonKind(inPhase, displays).fold(
          t(PhaseHeaderMixed, phase.label, n(inPhase.size))
        )(k =>
          t(PhaseHeader, phase.label, n(inPhase.size), glyphName(kindGlyph(k)).toLowerCase)
        )
        val phaseRow = NavigatorRowVM(
          NavigatorRowKind.Phase,
          1,
          head,
          "",
          "",
          None,
          false,
          Some(popen),
          false,
          t(GroupAccessible, p, head, openness(popen)),
          Some(NavigatorIntent.Toggle(pg)),
          pref
        )
        val body =
          if popen then
            inPhase.map(e => trialRow(e, 2, trialName(e.trial), displays, inventory, selected))
          else
            val admitted = inPhase.count(_.disposition == TrialDisposition.Admitted)
            val span     = range(inPhase.head.trial, inPhase.last.trial)
            val count    = t(RangeAdmitted, n(admitted))
            val rangeRow = NavigatorRowVM(
              NavigatorRowKind.Range,
              2,
              span,
              count,
              "",
              commonKind(inPhase, displays).map(kindGlyph),
              false,
              Some(false),
              false,
              t(GroupAccessible, span, count, openness(false)),
              Some(NavigatorIntent.Toggle(pg)),
              pref
            )
            val exceptions = inPhase
              .filter(e => e.disposition != TrialDisposition.Admitted || isMissing(e, displays))
            rangeRow +: exceptions
              .map(e => trialRow(e, 2, trialName(e.trial), displays, inventory, selected))
        phaseRow +: body
      }
      if filtering && shown.isEmpty then Vector.empty
      else header +: (if open then phases else Vector.empty)
    }
    val empty =
      Option.when(filtering && rows.isEmpty && entries.nonEmpty)(t(NoMatch, nav.filter))
    NavigatorPaneVM(
      t(TrialsTitle),
      t(TrialsFilter),
      t(TrialsFilterPrompt),
      nav.filter,
      t(TrialsList, label),
      rows,
      note.orElse(empty),
      retry,
      legend(entries, displays),
      t(Footer)
    )

  /** The Items pane: each match item, opening to the trials matched on it. */
  def items(nav: TrialsNavigator, model: AppModel): NavigatorPaneVM =
    val (note, retry)                    = notes(nav, model)
    val entries                          = nav.entries.toOption.getOrElse(Vector.empty)
    val displays                         = registry(nav, model)
    val selected                         = TrialsNavigator.selected(model)
    val inventory                        = inventoryFile(nav)
    val label                            = nav.dataset.fold("")(_.id.label)
    val dataset: Option[DatasetRevision] = nav.dataset.map(_.id)
    val f                                = nav.itemFilter.trim.toLowerCase
    val byItem                           = entries
      .groupBy(_.item)
      .toVector
      .filter((item, _) => f.isEmpty || item.toLowerCase.contains(f))
      .sortBy(_._1)
    val rows = byItem.flatMap { (item, trials) =>
      val group   = NavigatorGroup.Item(item)
      val open    = TrialsNavigator.isOpen(nav, group, model)
      val missing = trials.count(isMissing(_, displays))
      val detail  = (Vector(t(ItemDetail, n(trials.size))) ++
        Option.when(missing > 0)(imagesMissing(missing))).mkString(" · ")
      val name = itemName(item)
      NavigatorRowVM(
        NavigatorRowKind.Item,
        0,
        name,
        "",
        detail,
        None,
        missing > 0,
        Some(open),
        false,
        t(GroupAccessible, name, detail, openness(open)),
        Some(NavigatorIntent.Toggle(group)),
        dataset.map(StudioRef.TrialGroup(_, TrialGrouping.MatchedOn(item)))
      ) +: (if open then
              trials.map(e =>
                trialRow(
                  e,
                  1,
                  t(TrialOf, e.trial.participant, trialName(e.trial)),
                  displays,
                  inventory,
                  selected
                )
              )
            else Vector.empty)
    }
    val empty = Option.when(f.nonEmpty && rows.isEmpty && entries.nonEmpty)(
      t(NoMatch, nav.itemFilter)
    )
    NavigatorPaneVM(
      t(ItemsTitle),
      t(ItemsFilter),
      t(ItemsFilter),
      nav.itemFilter,
      t(ItemsList, label),
      rows,
      note.orElse(empty),
      retry,
      legend(entries, displays),
      t(Footer)
    )

  /** A pane's own focus stops, after the pane's: its filter, its list, and
    * Retry after a failed read.
    */
  def focusStops(vm: NavigatorPaneVM): Vector[FocusStop] =
    Vector(FocusStop(A11yRole.TextField, vm.filterLabel), FocusStop(A11yRole.List, vm.list)) ++
      vm.retry.map(FocusStop(A11yRole.Button, _))
