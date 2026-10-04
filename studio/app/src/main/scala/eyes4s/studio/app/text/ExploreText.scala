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

package eyes4s.studio.app.text

/** Explore's trial view strings (ticket S6.2; Explore.dc.html, centre), kept
  * apart from the other catalogues so the view adds its own ids. Templates
  * name their arguments by position, as [[Messages]] does.
  */
enum ExploreTextId derives CanEqual:
  // --- The toolbar --------------------------------------------------------------
  case Title, TitleNoItem, Points, Order, Map, ToggleOn, ToggleOff, MapPreview
  case MapNotServed, CanvasHint

  // --- What the view cannot show ------------------------------------------------------
  case NoTrial, NoRevision, Reading, ReadFailed, DisplaysFailed, DisplaysNotServed, Retry
  case SkippedMark, PreviewUndrawable

  // --- The legend ------------------------------------------------------------------------
  case LegendTitle, Fixation, OutsideWindowExcluded, OutsideWindowFails, OutsideScreen
  case DroppedInitial, OrderLines, PreviewMap, MissingAsset

/** Explore's trial view strings in the board's wording. */
object ExploreText:
  import ExploreTextId.*

  /** The reference English template of `id`. */
  def english(id: ExploreTextId): String = id match
    case Title        => "{0} · {1} · {2}"
    case TitleNoItem  => "{0} · {1}"
    case Points       => "Points"
    case Order        => "Order"
    case Map          => "Map"
    case ToggleOn     => "{0}, on"
    case ToggleOff    => "{0}, off"
    case MapPreview   => "Map: preview · σ {0}° · not a result"
    case MapNotServed => "Map preview not available: {0}"
    case CanvasHint   => "Canvas focused · arrows move · Enter selects · Esc clears"

    case NoTrial    => "Choose a trial in the Trials navigator to explore it."
    case NoRevision =>
      "No analysis run is shown: the trial's fixations are placed by an analysis revision."
    case Reading           => "Reading the fixations of {0}…"
    case ReadFailed        => "The fixations of {0} are not available: {1}"
    case DisplaysFailed    => "The display of {0} is not available: {1}"
    case DisplaysNotServed =>
      "Display kinds of {0} are not served for this project yet: the trial is drawn on its " +
        "screen without its display."
    case Retry             => "Retry"
    case SkippedMark       => "Fixation {0} lasts {1} ms, too short to draw; it is not shown."
    case PreviewUndrawable => "The preview of {0} cannot be drawn: {1}"

    case LegendTitle           => "Legend"
    case Fixation              => "Fixation · marker area ∝ duration"
    case OutsideWindowExcluded => "Outside the analysis window · excluded from the map"
    case OutsideWindowFails    => "Outside the analysis window · the trial fails"
    case OutsideScreen         => "Outside the screen"
    case DroppedInitial        => "Dropped by the initial-fixation policy"
    case OrderLines            => "Order between fixation centres, not measured saccades"
    case PreviewMap            => "Preview density · σ {0}° · not a result"
    case MissingAsset          => "Missing asset"

  /** `id`'s English template with its arguments filled. */
  def apply(id: ExploreTextId, args: String*): String =
    Messages.fill(english(id), args.toVector)
