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

import eyes4s.plan.{MapPlacement, OffWindowPolicy}
import eyes4s.studio.app.geometry.Loading
import eyes4s.studio.app.maps.{MapGrid, MapId}
import eyes4s.studio.app.text.{ExploreText, ExploreTextId, Format}
import eyes4s.studio.app.vm.{A11yRole, FocusStop}
import eyes4s.studio.core.assets.{Display, DisplayKind, TrialDisplay}
import eyes4s.studio.core.backend.{AdmittedFixation, TrialKey, TrialPreview}
import eyes4s.studio.core.document.ScreenSize

/** A toolbar toggle: its label, state and whether it can act. */
final case class ToggleVM(
    toggle: TrialToggle,
    label: String,
    on: Boolean,
    enabled: Boolean,
    accessible: String
) derives CanEqual

/** What a legend entry shows beside its words. */
enum LegendSwatch derives CanEqual:
  case Fixation, OutsideWindow, OutsideScreen, DroppedInitial, OrderLine, PreviewMap
  case Display(kind: DisplayKind)
  case MissingAsset

/** One entry of the trial view's legend. */
final case class LegendEntryVM(swatch: LegendSwatch, label: String) derives CanEqual

/** The trial to draw: its display on its screen, its admitted fixations,
  * the toggles' choices, and the preview map when Map is on and the backend
  * served one.
  */
final case class ShownTrialVM(
    trial: TrialKey,
    display: TrialDisplay,
    screen: ScreenSize,
    fixations: Vector[AdmittedFixation],
    points: Boolean,
    order: Boolean,
    map: Option[MapGrid]
) derives CanEqual

/** Everything Explore's trial view shows. */
final case class ExploreTrialViewVM(
    title: String,
    toggles: Vector[ToggleVM],
    mapLabel: Option[String],
    mapNote: Option[String],
    hint: String,
    legendTitle: String,
    legend: Vector[LegendEntryVM],
    note: Option[String],
    retry: Option[String],
    shown: Option[ShownTrialVM]
) derives CanEqual

object ExploreTrialViewVM:
  import ExploreTextId.*

  private def t(id: ExploreTextId, args: String*): String = ExploreText(id, args*)

  /** "2", "2.5": a bandwidth in degrees as the board writes it. */
  def degrees(value: Double): String =
    if value == math.rint(value) then value.toLong.toString else Format.decimal(value, 1)

  /** The preview label of `preview`: "Map: preview · σ 2° · not a result". */
  def previewLabel(preview: TrialPreview): String = t(MapPreview, degrees(preview.sigmaDegrees))

  /** The preview as a map grid: identified as a preview, never a run's map. */
  def previewGrid(preview: TrialPreview): Option[MapGrid] =
    MapGrid
      .of(
        MapId.Preview(preview.revision, preview.trial),
        preview.columns,
        preview.rows,
        preview.order,
        preview.cells,
        preview.levels
      )
      .toOption

  private def kindName(kind: DisplayKind): String =
    TrialsNavigatorVM.glyphName(kind match
      case DisplayKind.Image                  => TrialGlyph.Image
      case DisplayKind.Blank                  => TrialGlyph.Blank
      case DisplayKind.BlankWithFixationCross => TrialGlyph.BlankWithCross
      case DisplayKind.Cue                    => TrialGlyph.Cue
      case DisplayKind.Unknown                => TrialGlyph.Unknown)

  /** The display of `trial`: the served registry's, else an unknown display
    * on the dataset's screen at its image placement.
    */
  private def displayOf(
      view: ExploreTrialView,
      trial: TrialKey
  ): Option[(TrialDisplay, ScreenSize)] =
    view.displays.toOption.collect { case DisplaySource.Served(r) => r } match
      case Some(registry) => registry.display(trial).map(_ -> registry.screen)
      case None           =>
        view.dataset.map(d =>
          TrialDisplay(
            trial,
            None,
            Display.Unknown(None),
            d.geometry.image
          ) -> d.geometry.screen
        )

  private def legend(
      view: ExploreTrialView,
      shown: Option[ShownTrialVM]
  ): Vector[LegendEntryVM] =
    shown.toVector.flatMap { s =>
      val placements = s.fixations.map(_.placement).distinct
      val marks      =
        Option
          .when(s.points && s.fixations.nonEmpty)(
            LegendEntryVM(LegendSwatch.Fixation, t(Fixation))
          )
          .toVector ++
          (if !s.points then Vector.empty
           else
             placements.collect {
               case MapPlacement.OutsideWindow(OffWindowPolicy.Exclude) =>
                 LegendEntryVM(LegendSwatch.OutsideWindow, t(OutsideWindowExcluded))
               case MapPlacement.OutsideWindow(OffWindowPolicy.FailTrial) =>
                 LegendEntryVM(LegendSwatch.OutsideWindow, t(OutsideWindowFails))
               case MapPlacement.OutsideScreen =>
                 LegendEntryVM(LegendSwatch.OutsideScreen, t(OutsideScreen))
               case MapPlacement.DroppedInitial =>
                 LegendEntryVM(LegendSwatch.DroppedInitial, t(DroppedInitial))
             })
      val order = Option.when(s.order && s.fixations.size > 1)(
        LegendEntryVM(LegendSwatch.OrderLine, t(OrderLines))
      )
      val display =
        if s.display.isMissing then LegendEntryVM(LegendSwatch.MissingAsset, t(MissingAsset))
        else LegendEntryVM(LegendSwatch.Display(s.display.kind), kindName(s.display.kind))
      val map = s.map
        .zip(view.preview.toOption)
        .map((_, p) =>
          LegendEntryVM(LegendSwatch.PreviewMap, t(PreviewMap, degrees(p.sigmaDegrees)))
        )
      marks ++ order ++ Vector(display) ++ map
    }

  /** The view-model of `view` in `model`. */
  def of(view: ExploreTrialView): ExploreTrialViewVM =
    val trial   = view.trial
    val preview = view.preview.toOption
    val title   = trial.fold("")(k =>
      displayOf(view, k).flatMap(_._1.item) match
        case Some(item) => t(Title, k.participant, k.trial, item.value)
        case None       => t(TitleNoItem, k.participant, k.trial)
    )
    val shown = for
      k                 <- trial
      fixations         <- view.fixations.toOption
      (display, screen) <- displayOf(view, k)
    yield ShownTrialVM(
      k,
      display,
      screen,
      fixations.fixations,
      view.points,
      view.order,
      if view.map then preview.flatMap(previewGrid) else None
    )
    val label = trial.fold("")(_.label)
    val note  =
      if trial.isEmpty then Some(t(NoTrial))
      else if view.revision.isEmpty then Some(t(NoRevision))
      else
        val reading = view.fixations match
          case Loading.Waiting     => Some(t(Reading, label))
          case Loading.Failed(why) => Some(t(ReadFailed, label, why))
          case _                   => None
        val shownDisplay = view.displays match
          case Loading.Failed(why)                    => Some(t(DisplaysFailed, label, why))
          case Loading.Ready(DisplaySource.NotServed) =>
            Some(t(DisplaysNotServed, view.dataset.fold("")(_.id.label)))
          case _ => None
        Option((reading.toVector ++ shownDisplay).mkString(" ")).filter(_.nonEmpty)
    val failed = Vector(view.fixations, view.preview, view.displays).exists {
      case Loading.Failed(_) => true
      case _                 => false
    }
    val mapNote = view.preview match
      case Loading.Failed(why) => Some(t(MapNotServed, why))
      case _                   => None
    // The toggles are view choices, always available but for Map when the
    // backend could not give a preview.
    def toggle(tg: TrialToggle, id: ExploreTextId, enabled: Boolean) =
      val name = t(id)
      val on   = view.isOn(tg)
      ToggleVM(tg, name, on, enabled, if on then t(ToggleOn, name) else t(ToggleOff, name))
    ExploreTrialViewVM(
      title,
      Vector(
        toggle(TrialToggle.Points, Points, true),
        toggle(TrialToggle.Order, Order, true),
        toggle(TrialToggle.Map, Map, mapNote.isEmpty)
      ),
      Option.when(view.map)(preview.map(previewLabel)).flatten,
      mapNote,
      t(CanvasHint),
      t(LegendTitle),
      legend(view, shown),
      note,
      Option.when(failed)(t(Retry)),
      shown
    )

  /** The pane's own focus stops after the pane's, in Tab order: each enabled
    * toggle, then Retry after a failed read.
    */
  def focusStops(vm: ExploreTrialViewVM): Vector[FocusStop] =
    vm.toggles.filter(_.enabled).map(t => FocusStop(A11yRole.ToggleButton, t.accessible)) ++
      vm.retry.map(FocusStop(A11yRole.Button, _))
