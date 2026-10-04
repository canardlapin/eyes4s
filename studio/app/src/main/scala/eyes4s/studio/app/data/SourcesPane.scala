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

package eyes4s.studio.app.data

import eyes4s.codec.ByteDigest
import eyes4s.studio.app.explore.DisplaySource
import eyes4s.studio.app.geometry.{GeometryPanel, Loading}
import eyes4s.studio.app.nav.{Location, Place}
import eyes4s.studio.app.text.{Format, SourcesText, SourcesTextId}
import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.core.assets.{AssetFile, AssetRef, AssetRegistry, DisplayKind}
import eyes4s.studio.core.backend.DatasetRevision
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.{DatasetRevisionSpec, Perspective, SourceRole}
import eyes4s.studio.core.selection.{DisplayCount, InventoryKind, StudioRef}

/** A user action or platform fact of the Sources pane. */
enum SourcesIntent derives CanEqual:
  /** The displays of `dataset`, asked for by ask `ask`. */
  case RegistryRead(dataset: DatasetRevision, ask: Int, result: Either[String, DisplaySource])

  /** Read the displays again after a failed read. */
  case Retry

  /** Repair…: locate the first missing image file. */
  case Repair

  /** The platform stored the chosen file `name` (its bytes' SHA-256 `sha256`)
    * for the missing `file` of `dataset`.
    */
  case Located(dataset: DatasetRevision, file: AssetFile, name: AssetFile, sha256: ByteDigest)

  /** The platform could not locate or store a file for `file`. */
  case NotLocated(file: AssetFile, reason: String)

  /** Show the trials whose image is missing. */
  case ShowTrials

/** What the platform must do after an update of the Sources pane. */
enum SourcesEffect derives CanEqual:
  /** Read `spec`'s trial displays: its asset registry, or not served. */
  case ReadRegistry(spec: DatasetRevisionSpec, ask: Int)

  /** Ask the user for the image to show as `file` of `dataset`, store its
    * bytes in the project, and answer [[SourcesIntent.Located]] or
    * [[SourcesIntent.NotLocated]].
    */
  case Locate(dataset: DatasetRevision, file: AssetFile)

  /** Dispatch an app intent: a document command or a navigation. */
  case App(intent: Intent)

/** The Data perspective's Sources pane (ticket S5.7; Data.dc.html, left):
  * the selected dataset revision's source files, what each trial displayed
  * by kind and phase, and its missing image files with Repair….
  *
  * The displays are the revision's asset registry, with the document's
  * repairs applied ([[AssetRegistry.withRelinks]]); nothing is counted but
  * the registry's own trials. A repair stores the chosen bytes in the project
  * and records [[Command.RelinkAsset]]: the inventory's name, the stored
  * bytes' digest.
  */
final case class SourcesPane(
    dataset: Option[DatasetRevisionSpec],
    ask: Int,
    registry: Loading[DisplaySource],
    note: Option[String]
) derives CanEqual

object SourcesPane:

  val empty: SourcesPane = SourcesPane(None, 0, Loading.Idle, None)

  private val none: Vector[SourcesEffect] = Vector.empty

  private def t(id: SourcesTextId, args: String*): String = SourcesText(id, args*)

  private def read(pane: SourcesPane): (SourcesPane, Vector[SourcesEffect]) =
    pane.dataset match
      case Some(spec) =>
        val next = pane.ask + 1
        (
          pane.copy(ask = next, registry = Loading.Waiting),
          Vector(SourcesEffect.ReadRegistry(spec, next))
        )
      case None => (pane.copy(registry = Loading.Idle), none)

  /** Follow the Data perspective's dataset revision: another one (or a
    * changed one) is read again.
    */
  def sync(pane: SourcesPane, model: AppModel): (SourcesPane, Vector[SourcesEffect]) =
    val now = GeometryPanel.selected(model)
    if now == pane.dataset then (pane, none)
    else read(pane.copy(dataset = now, note = None))

  /** The registry as the document has repaired it, when it is served. */
  def registry(pane: SourcesPane, model: AppModel): Option[Either[String, AssetRegistry]] =
    pane.registry.toOption.collect { case DisplaySource.Served(r) =>
      r.withRelinks(model.document.relinks.of(r.dataset)).left.map(_.message)
    }

  def update(
      pane: SourcesPane,
      model: AppModel,
      intent: SourcesIntent
  ): (SourcesPane, Vector[SourcesEffect]) =
    import SourcesIntent.*
    intent match
      case RegistryRead(d, n, result) =>
        if !pane.dataset.exists(_.id == d) || n != pane.ask then (pane, none)
        else
          (
            pane.copy(registry = result.fold(Loading.Failed(_), Loading.Ready(_))),
            none
          )
      case Retry =>
        pane.registry match
          case Loading.Failed(_) => read(pane)
          case _                 => (pane, none)
      case Repair =>
        val target = for
          spec <- pane.dataset
          r    <- registry(pane, model).flatMap(_.toOption)
          m    <- r.missing.headOption
        yield SourcesEffect.Locate(spec.id, m.file)
        (pane.copy(note = None), target.toVector)
      case Located(d, file, name, sha) =>
        if !pane.dataset.exists(_.id == d) then (pane, none)
        else
          (
            pane.copy(note = Some(t(SourcesTextId.Repaired, file.value, name.value))),
            Vector(
              SourcesEffect.App(
                Intent.Dispatch(Command.RelinkAsset(d, file, Some(AssetRef(name, sha))))
              )
            )
          )
      case NotLocated(file, reason) =>
        (pane.copy(note = Some(t(SourcesTextId.RepairFailed, file.value, reason))), none)
      case ShowTrials =>
        val first = registry(pane, model)
          .flatMap(_.toOption)
          .flatMap(_.missing.flatMap(_.trials).headOption)
        (
          pane,
          first.toVector.map(trial =>
            SourcesEffect.App(
              Intent.Navigate(
                Location(Perspective.Explore, Vector(Place.At(StudioRef.Trial(trial))))
              )
            )
          )
        )

/** One source of the revision: its file, kind, what is stored, and a count
  * with the refs its numbers name.
  */
final case class SourceRowVM(
    name: String,
    kind: String,
    stored: String,
    count: Option[String],
    refs: Vector[StudioRef]
) derives CanEqual

/** One line of "What each trial displayed": a kind in a phase (or the kinds
  * no trial showed), how many trials, and the refs of its numbers.
  */
final case class DisplayRowVM(
    kinds: Vector[DisplayKind],
    label: String,
    count: String,
    shown: Boolean,
    refs: Vector[StudioRef]
) derives CanEqual

/** The missing image files and what Repair… and Show… do. */
final case class MissingVM(
    title: String,
    body: String,
    repair: String,
    showTrials: String,
    files: Vector[AssetFile],
    refs: Vector[StudioRef]
) derives CanEqual

/** What the Sources pane shows. */
final case class SourcesVM(
    empty: Option[String],
    sources: Vector[SourceRowVM],
    displaysTitle: String,
    displays: Vector[DisplayRowVM],
    status: Option[String],
    retry: Boolean,
    missing: Option[MissingVM],
    note: Option[String]
) derives CanEqual

object SourcesVM:
  import SourcesTextId.*

  private def t(id: SourcesTextId, args: String*): String = SourcesText(id, args*)

  private def count(n: Int): String = Format.count(n.toLong)

  def of(pane: SourcesPane, model: AppModel): SourcesVM =
    pane.dataset match
      case None =>
        SourcesVM(
          Some(t(NoDataset)),
          Vector.empty,
          t(DisplaysTitle),
          Vector.empty,
          None,
          false,
          None,
          None
        )
      case Some(spec) =>
        val id                                = spec.id
        val read                              = SourcesPane.registry(pane, model)
        val registry                          = read.flatMap(_.toOption)
        def tally(c: DisplayCount): StudioRef = StudioRef.DisplayTally(id, c)
        val files                             = spec.sources.entries.map { s =>
          val name = s.path.value.split('/').last
          s.role match
            case SourceRole.Fixations =>
              SourceRowVM(name, t(KindFixations), t(Stored), None, Vector.empty)
            case SourceRole.Trials =>
              SourceRowVM(
                name,
                t(KindInventory),
                t(Stored),
                registry.map(r => t(InventoryTrials, count(r.trials.size))),
                registry.toVector.map(_ =>
                  StudioRef.InventoryCount(id, InventoryKind.Inventory)
                )
              )
        }
        val stimuli = SourceRowVM(
          t(Stimuli),
          t(KindImages),
          t(Stored),
          Some(registry.fold(t(ImagesNotServed)) { r =>
            val s = r.summary
            t(ImagesFound, count(s.present), count(s.files))
          }),
          registry.toVector.flatMap(_ =>
            Vector(tally(DisplayCount.ImagesFound), tally(DisplayCount.ImagesNamed))
          )
        )
        val displays = registry.toVector.flatMap { r =>
          val shown   = r.trials.filterNot(_.isMissing)
          val phases  = r.trials.map(_.trial.phase).distinct
          val counted = for
            kind  <- DisplayKind.values.toVector
            phase <- phases
            n = shown.count(d => d.kind == kind && d.trial.phase == phase)
            if n > 0
          yield DisplayRowVM(
            Vector(kind),
            t(ShownRow, SourcesText.kind(kind), phase.label),
            count(n),
            true,
            Vector(tally(DisplayCount.Shown(kind, phase)))
          )
          val unseen =
            DisplayKind.values.toVector.filterNot(k => counted.exists(_.kinds == Vector(k)))
          counted ++ Option
            .when(unseen.nonEmpty)(
              DisplayRowVM(
                unseen,
                t(NoneShown, unseen.map(SourcesText.kind).mkString(" · ")),
                count(0),
                false,
                for
                  k <- unseen
                  p <- phases
                yield tally(DisplayCount.Shown(k, p))
              )
            )
            .toVector
        }
        val missing = registry.flatMap { r =>
          val absent = r.missing
          Option.when(absent.nonEmpty) {
            val trials = absent.flatMap(_.trials)
            val byPart = trials
              .groupBy(_.participant)
              .toVector
              .sortBy(_._1)
              .map((p, ts) => t(MissingTrialsOf, p, ts.map(_.trial).sorted.mkString(", ")))
            val phases = trials.map(_.phase).distinct
            MissingVM(
              if absent.size == 1 then t(MissingTitleOne)
              else t(MissingTitle, count(absent.size)),
              t(
                MissingBody,
                absent.map(_.file.value).mkString(", "),
                (count(trials.size) +: phases.map(_.label)).mkString(" "),
                byPart.mkString("; ")
              ),
              t(Repair),
              t(ShowTrials, count(trials.size)),
              absent.map(_.file),
              Vector(tally(DisplayCount.MissingFiles), tally(DisplayCount.MissingTrials))
            )
          }
        }
        SourcesVM(
          None,
          files :+ stimuli,
          t(DisplaysTitle),
          displays,
          pane.registry match
            case Loading.Waiting   => Some(t(Reading))
            case Loading.Failed(e) => Some(t(Unreadable, e))
            case _                 => read.flatMap(_.left.toOption).map(t(Unreadable, _)),
          pane.registry match
            case Loading.Failed(_) => true
            case _                 => false,
          missing,
          pane.note
        )
