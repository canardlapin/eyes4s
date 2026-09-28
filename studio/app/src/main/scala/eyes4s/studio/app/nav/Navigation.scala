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

package eyes4s.studio.app.nav

import eyes4s.studio.core.backend.{AnalysisRevision, DatasetRevision, Response}
import eyes4s.studio.core.document.{
  FigureId,
  Perspective,
  Preset,
  RecipeField,
  ReportingId,
  SourceRole
}
import eyes4s.studio.core.selection.StudioRef

/** A part of the Data perspective a crumb can name. */
enum DataSection derives CanEqual:
  case Sources, ColumnMapping, TrialMetadata, Admission, Geometry

/** One crumb of a trail, or one step of a status-bar path: a typed place,
  * never a string (DESIGN_SPEC section 3's trail grammar). Results places
  * carry the reporting spec and group, so drilling down from the summary
  * keeps them ("Summary · by retrieval response › Remembered › P17 › …").
  */
enum Place derives CanEqual:
  case NewProject
  case Dataset(dataset: DatasetRevision)
  case Source(role: SourceRole)
  case DataView(section: DataSection)
  case Analyses

  /** The analysis family a revision belongs to ("Reinstatement · Enc→Ret"). */
  case Lineage(preset: Preset)
  case Revision(revision: AnalysisRevision)
  case Field(field: RecipeField)
  case Summary(reporting: ReportingId)
  case Group(reporting: ReportingId, group: Response)

  /** Anything a StudioRef names: participant, query, pair, map, fixation,
    * record, figure panel.
    */
  case At(ref: StudioRef)
  case Figures
  case Figure(figure: FigureId)

object Place:

  /** The perspective that shows `trail`'s last place. Fixation and record
    * places cross into Explore; a participant belongs to Compare when the
    * trail came from a summary, and to Explore otherwise.
    */
  def home(trail: Vector[Place]): Option[Perspective] =
    val fromSummary = trail.exists {
      case Summary(_) | Group(_, _) => true
      case _                        => false
    }
    trail.lastOption.map {
      case NewProject | Dataset(_) | Source(_) | DataView(_) => Perspective.Data
      case Analyses | Lineage(_) | Revision(_) | Field(_)    => Perspective.Analysis
      case Summary(_) | Group(_, _)                          => Perspective.Compare
      case Figures | Figure(_)                               => Perspective.Figures
      case At(ref)                                           =>
        ref match
          case StudioRef.Trial(_) | StudioRef.Fixation(_, _) |
              StudioRef.SourceRecord(_, _, _, _) =>
            Perspective.Explore
          case StudioRef.Participant(_) =>
            if fromSummary then Perspective.Compare else Perspective.Explore
          case StudioRef.FigurePanel(_, _) => Perspective.Figures
          case StudioRef.WindowTally(_, _) | StudioRef.InventoryCount(_, _) => Perspective.Data
          case _                           => Perspective.Compare
    }

/** Where the user is: a perspective and its trail. */
final case class Location(perspective: Perspective, trail: Vector[Place]) derives CanEqual

/** Back/forward history across perspectives (S3.4) and the trail each
  * perspective last showed. The current location is the document's
  * perspective with its trail; the history holds the locations left.
  */
final case class Navigation private (
    back: List[Location],
    trails: Map[Perspective, Vector[Place]],
    forward: List[Location]
) derives CanEqual:

  def trail(perspective: Perspective): Vector[Place] =
    trails.getOrElse(perspective, Vector.empty)

  def at(perspective: Perspective): Location = Location(perspective, trail(perspective))

  def canGoBack: Boolean    = back.nonEmpty
  def canGoForward: Boolean = forward.nonEmpty

  /** Move from `from` to `to`: `from` becomes the next back step and the
    * forward steps are dropped. Moving to where one is changes nothing.
    */
  def go(from: Location, to: Location): Navigation =
    if from == to then this
    else Navigation((from :: back).take(Navigation.Depth), remember(to), Nil)

  /** The location one step back, and the history after taking it. */
  def goBack(from: Location): Option[(Location, Navigation)] = back match
    case to :: rest =>
      Some((to, Navigation(rest, remember(to), (from :: forward).take(Navigation.Depth))))
    case Nil => None

  def goForward(from: Location): Option[(Location, Navigation)] = forward match
    case to :: rest =>
      Some((to, Navigation((from :: back).take(Navigation.Depth), remember(to), rest)))
    case Nil => None

  private def remember(to: Location) = trails.updated(to.perspective, to.trail)

object Navigation:
  /** History steps kept in each direction. */
  val Depth: Int = 100

  /** No history; each perspective starts at its root trail. */
  def start(trails: Map[Perspective, Vector[Place]]): Navigation =
    Navigation(Nil, trails, Nil)
