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

package eyes4s.studio.app.analysis

import eyes4s.studio.app.{AppModel, Intent}
import eyes4s.studio.app.nav.{Location, Place}
import eyes4s.studio.app.text.{AnalysesText, AnalysesTextId}
import eyes4s.studio.app.vm.{A11yRole, FocusStop}
import eyes4s.studio.core.backend.{AnalysisRevision, DatasetRevision, RunId}
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.{
  AnalysisFamilyId,
  Perspective,
  Preset,
  RevisionName,
  StudioDocument
}
import eyes4s.studio.core.preset.InitialRecipe
import eyes4s.studio.core.freshness.RunStanding

/** History rows retain their document identities; the navigator never runs a study. */
final case class AnalysisHistoryRow(
    revision: AnalysisRevision,
    dataset: DatasetRevision,
    run: Option[RunId],
    label: String,
    detail: String,
    selected: Boolean,
    open: Intent
) derives CanEqual:
  def accessible: String = s"$label · $detail"

final case class AnalysisHistoryGroup(
    family: AnalysisFamilyId,
    name: String,
    heading: String,
    rows: Vector[AnalysisHistoryRow]
) derives CanEqual:
  def accessible: String = AnalysesText(AnalysesTextId.FamilyLabel, name, family.label)

final case class AnalysisCurrentRun(run: RunId, label: String, show: Option[Intent])
    derives CanEqual

final case class AnalysesNavigatorVM(
    groups: Vector[AnalysisHistoryGroup],
    create: Option[Intent],
    current: Option[AnalysisCurrentRun],
    currentLabel: String,
    note: String
) derives CanEqual

object AnalysesNavigator:
  /** New analyses use admitted-data defaults, retaining the legacy first-analysis
    * path. Independent families are provisional until Save & run.
    */
  def create(document: StudioDocument): Option[Command] =
    if document.draft.nonEmpty then None
    else if document.analyses.isEmpty then
      PresetPicker.command(document, Preset.EncodingRetrieval)
    else
      for
        data    <- document.latestAdmitted
        family  <- document.nextFamilyId.toOption
        _       <- document.nextAnalysisId.toOption
        initial <- InitialRecipe.of(data, Preset.EncodingRetrieval).toOption
        name    <- RevisionName
          .of(AnalysesText(AnalysesTextId.NewFamilyName, family.value.toString))
          .toOption
      yield Command.StartFamily(name.value, data.id, initial._1, initial._2.copy(name = name))

  def selected(model: AppModel): Option[AnalysisRevision] =
    AnalysisSelection.selected(model).map(_.id)

  def vm(model: AppModel): AnalysesNavigatorVM =
    import AnalysesTextId.*
    val document  = model.document
    val chosen    = selected(model)
    val chosenRun = model.navigation
      .trail(Perspective.Analysis)
      .reverseIterator
      .collectFirst { case Place.Run(run) => run }
    def open(revision: AnalysisRevision): Intent =
      Intent.Navigate(
        Location(
          Perspective.Analysis,
          AnalysisSelection.trail(document, revision)
        )
      )
    val saved = document.analyses.reverse.flatMap { analysis =>
      val runs    = model.freshness.runs.filter(_.run.analysis == analysis.id).reverse
      val entries = if runs.isEmpty then
        Vector(
          AnalysisHistoryRow(
            analysis.id,
            analysis.dataset,
            None,
            analysis.id.label,
            AnalysesText(NotRun, analysis.dataset.label),
            chosen.contains(analysis.id),
            open(analysis.id)
          )
        )
      else
        runs.map { entry =>
          val state = entry.standing match
            case RunStanding.Current       => AnalysesText(Current)
            case RunStanding.Stale(_)      => AnalysesText(Stale)
            case RunStanding.Running       => AnalysesText(Running)
            case RunStanding.Cancelled(at) =>
              at.fold(AnalysesText(Cancelled))(stage =>
                AnalysesText(CancelledAt, stage.toString)
              )
            case RunStanding.Failed => AnalysesText(Failed)
          AnalysisHistoryRow(
            analysis.id,
            entry.run.dataset,
            Some(entry.run.id),
            s"${analysis.id.label} · ${entry.run.id.label}",
            s"data ${entry.run.dataset.label} · $state",
            chosen
              .contains(analysis.id) && chosenRun.fold(entry == runs.head)(entry.run.id == _),
            Intent.Navigate(
              Location(
                Perspective.Analysis,
                AnalysisSelection.trail(document, analysis.id) :+ Place.Run(entry.run.id)
              )
            )
          )
        }
      document.familyOf(analysis.id).toVector.flatMap(family => entries.map(family -> _))
    }
    val draft = document.draftContext.toVector.flatMap { context =>
      document
        .familyOf(context.id)
        .toVector
        .map(family =>
          family -> AnalysisHistoryRow(
            context.id,
            context.dataset,
            None,
            AnalysesText(Draft, context.id.label),
            AnalysesText(NotRun, context.dataset.label),
            chosen.contains(context.id),
            open(context.id)
          )
        )
    }
    val rows         = draft ++ saved
    val families     = rows.map(_._1).distinct
    val chosenFamily = chosen.flatMap(document.familyOf)
    val current      = model.freshness.runs
      .findLast(entry =>
        entry.standing == RunStanding.Current &&
          chosenFamily.exists(id => document.familyOf(entry.run.analysis).contains(id))
      )
      .map { entry =>
        val run = entry.run.id
        AnalysisCurrentRun(
          run,
          AnalysesText(CurrentRun, entry.run.analysis.label, run.label),
          Option.when(
            !document.presentation.shownRun.contains(run) && model.jobs.ready
              .exists(_.run == run)
          )(Intent.ShowRun(run))
        )
      }
    val names = families.map(family =>
      family -> AnalysisSelection.name(document, family).getOrElse(family.label)
    )
    AnalysesNavigatorVM(
      names.map { (family, name) =>
        val heading =
          if names.count(_._2 == name) > 1 then AnalysesText(FamilyLabel, name, family.label)
          else name
        AnalysisHistoryGroup(
          family,
          name,
          heading,
          rows.collect { case (`family`, row) => row }
        )
      },
      create(document).map(_ => Intent.NewAnalysis),
      current,
      current.fold(AnalysesText(NoCurrentRun))(_.label),
      if document.draft.nonEmpty then AnalysesText(HeldDraft) else AnalysesText(Immutable)
    )

  def focusStops(view: AnalysesNavigatorVM): Vector[FocusStop] =
    Option
      .when(view.create.isDefined)(
        FocusStop(A11yRole.Button, AnalysesText(AnalysesTextId.NewAnalysis))
      )
      .toVector ++
      view.current
        .flatMap(_.show)
        .map(_ => FocusStop(A11yRole.Button, AnalysesText(AnalysesTextId.ShowCurrent)))
        .toVector ++
      view.groups.flatMap(_.rows.map(row => FocusStop(A11yRole.ToggleButton, row.accessible)))
