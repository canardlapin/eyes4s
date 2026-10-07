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
import eyes4s.studio.core.document.{Perspective, Preset, StudioDocument}
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

final case class AnalysisHistoryGroup(name: String, rows: Vector[AnalysisHistoryRow])
    derives CanEqual
final case class AnalysesNavigatorVM(
    groups: Vector[AnalysisHistoryGroup],
    create: Option[Intent],
    note: String
) derives CanEqual

object AnalysesNavigator:
  /** Only the supported initial-analysis command is offered here. Actual recipe
    * changes create later drafts; the empty-draft invariant is preserved.
    */
  def create(document: StudioDocument): Option[Command] =
    Option
      .when(document.analyses.isEmpty && document.draft.isEmpty)(
        PresetPicker.command(document, Preset.EncodingRetrieval)
      )
      .flatten

  def selected(model: AppModel): Option[AnalysisRevision] =
    ResolvedDesign.target(model).map(_.revision)

  def vm(model: AppModel): AnalysesNavigatorVM =
    import AnalysesTextId.*
    val document  = model.document
    val chosen    = selected(model)
    val chosenRun = model.navigation
      .trail(Perspective.Analysis)
      .reverseIterator
      .collectFirst { case Place.Run(run) => run }
    def open(revision: AnalysisRevision, preset: Preset): Intent =
      Intent.Navigate(
        Location(
          Perspective.Analysis,
          Vector(Place.Analyses, Place.Lineage(preset), Place.Revision(revision))
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
            open(analysis.id, analysis.studio.preset)
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
                Vector(
                  Place.Analyses,
                  Place.Lineage(analysis.studio.preset),
                  Place.Revision(analysis.id),
                  Place.Run(entry.run.id)
                )
              )
            )
          )
        }
      entries.map(analysis.studio.name.value -> _)
    }
    val draft = document.draftContext.toVector.map { context =>
      context.studio.name.value -> AnalysisHistoryRow(
        context.id,
        context.dataset,
        None,
        AnalysesText(Draft, context.id.label),
        AnalysesText(NotRun, context.dataset.label),
        chosen.contains(context.id),
        open(context.id, context.studio.preset)
      )
    }
    val rows  = draft ++ saved
    val names = rows.map(_._1).distinct
    AnalysesNavigatorVM(
      names.map(name => AnalysisHistoryGroup(name, rows.collect { case (`name`, row) => row })),
      create(document).map(_ => Intent.NewAnalysis),
      if document.draft.nonEmpty then AnalysesText(HeldDraft) else AnalysesText(Immutable)
    )

  def focusStops(view: AnalysesNavigatorVM): Vector[FocusStop] =
    Option
      .when(view.create.isDefined)(
        FocusStop(A11yRole.Button, AnalysesText(AnalysesTextId.NewAnalysis))
      )
      .toVector ++
      view.groups.flatMap(_.rows.map(row => FocusStop(A11yRole.ToggleButton, row.accessible)))
