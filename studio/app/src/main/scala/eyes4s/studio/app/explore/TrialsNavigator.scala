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

import eyes4s.studio.app.geometry.Loading
import eyes4s.studio.app.nav.Place
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.assets.AssetRegistry
import eyes4s.studio.core.backend.{DatasetRevision, LedgerEntry, Phase, TrialKey}
import eyes4s.studio.core.document.{DatasetRevisionSpec, Perspective}
import eyes4s.studio.core.selection.{Lineage, StudioRef}

/** A group of the navigator that opens and closes: a participant, one
  * participant's phase, or an item of the Items pane.
  */
enum NavigatorGroup derives CanEqual:
  case Participant(participant: String)
  case PhaseOf(participant: String, phase: Phase)
  case Item(item: String)

/** A user action or platform fact the navigator's panes dispatch. */
enum NavigatorIntent derives CanEqual:
  /** Open a closed group, or close an open one. */
  case Toggle(group: NavigatorGroup)

  /** The Trials pane's filter text (participant, trial or item). */
  case Filter(text: String)

  /** The Items pane's filter text. */
  case FilterItems(text: String)

  /** A trial was activated: explore it. */
  case OpenTrial(trial: TrialKey)

  /** Read the trials and displays again, after a read failed. */
  case Retry

  /** The backend's whole ledger of `dataset`, asked for by ask `ask`. */
  case EntriesRead(
      dataset: DatasetRevision,
      ask: Int,
      result: Either[String, Vector[LedgerEntry]]
  )

  /** The trial displays of `dataset` (its asset registry), asked for by ask
    * `ask`.
    */
  case DisplaysRead(dataset: DatasetRevision, ask: Int, result: Either[String, AssetRegistry])

/** What the platform or the app must do after a navigator update. */
enum NavigatorEffect derives CanEqual:
  case App(intent: Intent)

  /** Read `dataset`'s whole ledger; the answer is [[NavigatorIntent.EntriesRead]]. */
  case RequestEntries(dataset: DatasetRevision, ask: Int)

  /** Read the trial displays of `dataset`; the answer is
    * [[NavigatorIntent.DisplaysRead]].
    */
  case RequestDisplays(dataset: DatasetRevisionSpec, ask: Int)

/** Explore's trials navigator (ticket S6.1; Explore.dc.html, left): the
  * trials of the latest admitted dataset revision as a participant → phase →
  * trial tree, with each trial's display kind and its status (quarantined,
  * absent, no-fixations, image missing), and the same trials by match item.
  *
  * Every trial and its status are the backend ledger's entries
  * ([[LedgerEntry]]); display kinds and missing images are the dataset's
  * asset registry. Nothing is counted here beyond the rows listed: a
  * participant's figures are the number of its ledger entries with each
  * disposition. Activating a trial explores it ([[Intent.Explain]]); the
  * trial Explore's trail is on is the selected one, and the groups that
  * hold it are open unless closed by hand.
  *
  * `ask` numbers the reads of `dataset`; an answer to an earlier ask is
  * ignored.
  */
final case class TrialsNavigator(
    dataset: Option[DatasetRevisionSpec],
    ask: Int,
    entries: Loading[Vector[LedgerEntry]],
    displays: Loading[AssetRegistry],
    toggled: Map[NavigatorGroup, Boolean],
    filter: String,
    itemFilter: String
) derives CanEqual

object TrialsNavigator:

  val empty: TrialsNavigator =
    TrialsNavigator(None, 0, Loading.Idle, Loading.Idle, Map.empty, "", "")

  private val none: Vector[NavigatorEffect] = Vector.empty

  /** The dataset revision Explore shows: the latest admitted one. */
  def shown(model: AppModel): Option[DatasetRevisionSpec] =
    model.document.datasets.filter(_.decision.isAdmitted).maxByOption(_.id.number)

  /** The trial Explore's trail is on, if any: the trial of its last place
    * (a trial, or a fixation or record within one).
    */
  def selected(model: AppModel): Option[TrialKey] =
    model.navigation.trail(Perspective.Explore).reverseIterator.collectFirst {
      Function.unlift {
        case Place.At(ref) =>
          (ref +: Lineage.structural.ancestors(ref)).collectFirst { case StudioRef.Trial(k) =>
            k
          }
        case _ => None
      }
    }

  /** Whether `group` is open: as toggled by hand, else open when it holds
    * the selected trial.
    */
  def isOpen(navigator: TrialsNavigator, group: NavigatorGroup, model: AppModel): Boolean =
    navigator.toggled.getOrElse(group, holdsSelected(group, selected(model)))

  private def holdsSelected(group: NavigatorGroup, trial: Option[TrialKey]): Boolean =
    trial.exists(k =>
      group match
        case NavigatorGroup.Participant(p)    => k.participant == p
        case NavigatorGroup.PhaseOf(p, phase) => k.participant == p && k.phase == phase
        case NavigatorGroup.Item(_)           => false
    )

  private def askFor(navigator: TrialsNavigator, spec: DatasetRevisionSpec) =
    val next = navigator.ask + 1
    (
      navigator.copy(
        dataset = Some(spec),
        ask = next,
        entries = Loading.Waiting,
        displays = Loading.Waiting
      ),
      Vector(
        NavigatorEffect.RequestEntries(spec.id, next),
        NavigatorEffect.RequestDisplays(spec, next)
      )
    )

  /** Follow the model's latest admitted dataset revision: a new one resets
    * the navigator (keeping its filters) and asks for its trials and
    * displays; the same one keeps them.
    */
  def sync(
      navigator: TrialsNavigator,
      model: AppModel
  ): (TrialsNavigator, Vector[NavigatorEffect]) =
    shown(model) match
      case None => (empty.copy(ask = navigator.ask), none)
      case Some(spec) if navigator.dataset.exists(_.id == spec.id) =>
        (navigator.copy(dataset = Some(spec)), none)
      case Some(spec) =>
        askFor(
          empty.copy(
            ask = navigator.ask,
            filter = navigator.filter,
            itemFilter = navigator.itemFilter
          ),
          spec
        )

  /** The Elm-style update: pure; effects are data. */
  def update(
      navigator: TrialsNavigator,
      model: AppModel,
      intent: NavigatorIntent
  ): (TrialsNavigator, Vector[NavigatorEffect]) =
    import NavigatorIntent.*
    intent match
      case Toggle(group) =>
        val open = isOpen(navigator, group, model)
        (navigator.copy(toggled = navigator.toggled.updated(group, !open)), none)
      case Filter(text)      => (navigator.copy(filter = text), none)
      case FilterItems(text) => (navigator.copy(itemFilter = text), none)
      case OpenTrial(key)    =>
        (
          navigator,
          Vector(NavigatorEffect.App(Intent.Explain(Place.At(StudioRef.Trial(key)))))
        )
      case Retry =>
        navigator.dataset.fold((navigator, none))(askFor(navigator, _))
      case EntriesRead(dataset, n, result) =>
        if !answers(navigator, dataset, n) then (navigator, none)
        else (navigator.copy(entries = result.fold(Loading.Failed(_), Loading.Ready(_))), none)
      case DisplaysRead(dataset, n, result) =>
        if !answers(navigator, dataset, n) then (navigator, none)
        else (navigator.copy(displays = result.fold(Loading.Failed(_), Loading.Ready(_))), none)

  private def answers(navigator: TrialsNavigator, dataset: DatasetRevision, n: Int): Boolean =
    navigator.dataset.exists(_.id == dataset) && navigator.ask == n
