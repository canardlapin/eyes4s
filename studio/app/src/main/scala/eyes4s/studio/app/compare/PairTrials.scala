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

import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.app.explore.ExploreRole
import eyes4s.studio.app.nav.{DataSection, Location, Place}
import eyes4s.studio.app.text.{Format, Messages}
import eyes4s.studio.core.backend.{PairDesign, PairRowEntry, PairScoreState, QueryRow, TrialKey}
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.core.selection.StudioRef

/** The words of the drill's pair navigator (overhaul v4, `explore.html`). */
enum PairTrialsTextId derives CanEqual:
  case Query, Matched, Controls, Count, AllTrials, Spoken, NoPair, Reading

object PairTrialsText:
  def english(id: PairTrialsTextId): String =
    import PairTrialsTextId.*
    id match
      case Query     => "Query"
      case Matched   => "Matched reference"
      case Controls  => "Controls · cosine at {0}"
      case Count     => "{0}"
      case AllTrials => "All {0} trials"
      case Spoken    => "{0}, {1} {2}, {3}"
      case NoPair    => "Choose a pair in Results to list its trials."
      case Reading   => "Reading the pair's trials…"

  def apply(id: PairTrialsTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)

/** One trial of the pair: its role, item and cosine at the pair's σ (none
  * for the query), whether the drill shows it, whether the cursor is on it,
  * and what opening it does.
  */
final case class PairTrialRow(
    trial: TrialKey,
    role: ExploreRole,
    item: String,
    value: String,
    shown: Boolean,
    cursor: Boolean,
    spoken: String,
    open: Intent
) derives CanEqual

/** A section of the pair navigator: its heading and rows. */
final case class PairTrialSection(heading: String, rows: Vector[PairTrialRow]) derives CanEqual

/** What the drill's pair navigator shows. */
final case class PairTrialsVM(
    count: String,
    status: Option[String],
    sections: Vector[PairTrialSection],
    all: Option[(String, Intent)]
) derives CanEqual:
  def rows: Vector[PairTrialRow] = sections.flatMap(_.rows)

  def cursorText: Option[String] = rows.find(_.cursor).map(_.spoken)

/** The pair navigator's cursor (the trial under it). */
final case class PairTrials(cursor: Option[TrialKey]) derives CanEqual

object PairTrials:
  import PairTrialsTextId.*

  val empty: PairTrials = PairTrials(None)

  /** The trial the drill shows: the trail's last trial, fixation or record. */
  def shownTrial(m: AppModel): Option[TrialKey] =
    m.location.trail.reverseIterator.collectFirst {
      case Place.At(StudioRef.Trial(k))                 => k
      case Place.At(StudioRef.Fixation(k, _))           => k
      case Place.At(StudioRef.SourceRecord(k, _, _, _)) => k
      case Place.At(StudioRef.Pair(_, _, _, _, ref))    => ref
    }

  /** The pair's trials: the query, its matched reference and its controls by
    * cosine, highest first, each as eyes4s served its pair row. Nothing is
    * computed but the order.
    */
  def vm(
      nav: PairTrials,
      m: AppModel,
      focus: Option[PanelFocus],
      entries: Option[Vector[PairRowEntry]],
      queries: Vector[QueryRow],
      scaleLabel: Option[String]
  ): PairTrialsVM =
    (focus, entries) match
      case (None, _)       => PairTrialsVM("", Some(PairTrialsText(NoPair)), Vector.empty, None)
      case (Some(_), None) =>
        PairTrialsVM("", Some(PairTrialsText(Reading)), Vector.empty, None)
      case (Some(f), Some(es)) =>
        val own   = es.filter(_.query == f.query)
        val trail = m.location.trail
        // The trail above the pair: the summary, group, participant, query.
        val above = trail.takeWhile {
          case Place.At(StudioRef.Pair(_, _, _, _, _)) => false
          case _                                       => true
        }
        val shown                   = shownTrial(m)
        def cosine(e: PairRowEntry) = e.score match
          case PairScoreState.Scored(v) => Some(v)
          case _                        => None
        def open(design: PairDesign, ref: TrialKey, trial: TrialKey) =
          Intent.Navigate(
            Location(
              Perspective.Results,
              above ++ Vector(
                Place.At(f.pair(design, ref)),
                Place.At(StudioRef.Trial(trial))
              )
            )
          )
        def roleWord(r: ExploreRole) = r match
          case ExploreRole.Query   => PairTrialsText(Query)
          case ExploreRole.Matched => PairTrialsText(Matched)
          case ExploreRole.Control => PairTrialsText(Controls, scaleLabel.getOrElse(""))
        def row(trial: TrialKey, role: ExploreRole, item: String, value: String, go: Intent) =
          PairTrialRow(
            trial,
            role,
            item,
            value,
            shown.contains(trial),
            false,
            PairTrialsText(Spoken, roleWord(role), trial.trial, item, value).stripSuffix(", "),
            go
          )
        val matched  = own.filter(_.design == PairDesign.Matched)
        val controls =
          own.filter(_.design == PairDesign.Control).sortBy(e => -cosine(e).getOrElse(-2.0))
        // The pair the trail is on: the query opens under it.
        val (design, ref) = f.reference
          .orElse(matched.headOption.map(e => (e.design, e.reference)))
          .getOrElse((PairDesign.Matched, f.query))
        val query = row(
          f.query,
          ExploreRole.Query,
          queries.find(_.query == f.query).fold("")(_.item),
          "",
          open(design, ref, f.query)
        )
        def scored(es: Vector[PairRowEntry], role: ExploreRole) = es.map(e =>
          row(
            e.reference,
            role,
            e.referenceItem,
            cosine(e).fold("")(Format.decimal(_, 2)),
            open(e.design, e.reference, e.reference)
          )
        )
        val sections = Vector(
          PairTrialSection(PairTrialsText(Query), Vector(query)),
          PairTrialSection(PairTrialsText(Matched), scored(matched, ExploreRole.Matched)),
          PairTrialSection(
            PairTrialsText(Controls, scaleLabel.getOrElse("")),
            scored(controls, ExploreRole.Control)
          )
        ).filter(_.rows.nonEmpty)
        val all     = sections.flatMap(_.rows)
        val home    = nav.cursor.filter(k => all.exists(_.trial == k)).orElse(shown)
        val cursorK =
          home.filter(k => all.exists(_.trial == k)).orElse(all.headOption.map(_.trial))
        val placed = sections.map(s =>
          s.copy(rows = s.rows.map(r => r.copy(cursor = cursorK.contains(r.trial))))
        )
        val dataset = m.document.run(f.run).map(_.dataset)
        PairTrialsVM(
          PairTrialsText(Count, all.size.toString),
          None,
          placed,
          dataset.map(d =>
            (
              PairTrialsText(AllTrials, f.query.participant),
              Intent.Navigate(
                Location(
                  Perspective.Data,
                  Vector(
                    Place.Dataset(d),
                    Place.DataView(DataSection.Trials),
                    Place.At(StudioRef.Participant(f.query.participant))
                  )
                )
              )
            )
          )
        )

  /** A key on the navigator: arrows move the cursor, Enter opens its trial. */
  def key(nav: PairTrials, vm: PairTrialsVM, key: NavigatorKey): (PairTrials, Option[Intent]) =
    val rows         = vm.rows
    val current      = rows.indexWhere(_.cursor).max(0)
    def move(i: Int) =
      (nav.copy(cursor = rows.lift(i.max(0).min(rows.size - 1)).map(_.trial)), None)
    if rows.isEmpty then (nav.copy(cursor = None), None)
    else
      key match
        case NavigatorKey.Up       => move(current - 1)
        case NavigatorKey.Down     => move(current + 1)
        case NavigatorKey.First    => move(0)
        case NavigatorKey.Last     => move(rows.size - 1)
        case NavigatorKey.Activate =>
          (nav.copy(cursor = Some(rows(current).trial)), Some(rows(current).open))
        case _ => (nav, None)
