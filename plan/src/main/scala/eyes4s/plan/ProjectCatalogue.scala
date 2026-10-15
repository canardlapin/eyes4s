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

package eyes4s.plan

import cats.Eq
import cats.data.NonEmptyVector
import eyes4s.design.{Session, SessionKey}
import eyes4s.kernel.*

/** Geometry is needed when deriving angular coordinates from pixel records.
  * Already measured coordinates and fixed features require no new calibration.
  */
enum ProjectCoordinates:
  case Native, DegreesFromPixels

/** An observed epoch binding names both the selected trial and its recording.
  * A planned timeline cannot be supplied to the evidence query.
  */
final case class ProjectAnchor[K, M](
    trial: K,
    recording: SessionKey,
    selector: MarkSelector[M],
    observedClock: ClockId,
    maximumUncertainty: SyncErrorMagnitude
):
  private[plan] def requirement: ProjectRequirement[K, M] =
    ProjectRequirement.ObservedAnchor(
      trial,
      recording,
      selector,
      observedClock,
      maximumUncertainty
    )

/** Supplied checked scanpaths/maps do not require the source recording to
  * remain in the project. Derivation from recordings keeps those keys mandatory.
  */
enum ProjectInputOrigin:
  case Supplied
  case Recordings(keys: NonEmptyVector[SessionKey])

/** Supplied windows carry their own measured support. Resolving a new event
  * alignment requires observed marks and adequate directed synchronization.
  */
enum ProjectTemporalWindows[K, M]:
  case Supplied()
  case Observed(anchors: NonEmptyVector[ProjectAnchor[K, M]])

enum ProjectTemplateInput:
  case FixedFeatures
  case MeanMap(grid: SessionKey)

/** Resource bindings for every shipped analysis and the epoch component.
  * A recording-derived input uses a nonempty list, so an empty binding cannot
  * make it appear ready. Supplied checked inputs are identified explicitly. Identifiers and installed schema
  * requirements are supplied by the application's registered recipes.
  */
enum ProjectWorkflow[K, M]:
  case FixationStudy(
      input: ProjectInputOrigin,
      grid: SessionKey,
      coordinates: ProjectCoordinates
  )
  case EventRecording(
      recording: SessionKey,
      areas: SessionKey,
      anchors: Vector[ProjectAnchor[K, M]]
  )
  case TemporalStudy(
      input: ProjectInputOrigin,
      grid: SessionKey,
      coordinates: ProjectCoordinates,
      windows: ProjectTemporalWindows[K, M]
  )
  case Repetition(
      input: ProjectInputOrigin,
      grid: SessionKey,
      coordinates: ProjectCoordinates
  )
  case PointSampling(
      input: ProjectInputOrigin,
      grid: SessionKey,
      coordinates: ProjectCoordinates
  )
  case TemplateFit(input: ProjectTemplateInput)
  case Decomposition(grid: SessionKey)
  case ScanpathComparison(
      input: ProjectInputOrigin,
      coordinates: ProjectCoordinates
  )
  case Epochs(anchors: NonEmptyVector[ProjectAnchor[K, M]])

  def kind: Either[AnalysisComponent, AnalysisKind] = this match
    case FixationStudy(_, _, _)    => Right(AnalysisKind.FixationStudy)
    case EventRecording(_, _, _)   => Right(AnalysisKind.EventRecording)
    case TemporalStudy(_, _, _, _) => Right(AnalysisKind.TemporalStudy)
    case Repetition(_, _, _)       => Right(AnalysisKind.Repetition)
    case PointSampling(_, _, _)    => Right(AnalysisKind.PointSampling)
    case TemplateFit(_)            => Right(AnalysisKind.TemplateFit)
    case Decomposition(_)          => Right(AnalysisKind.Decomposition)
    case ScanpathComparison(_, _)  => Right(AnalysisKind.ScanpathComparison)
    case Epochs(_)                 => Left(AnalysisComponent.Epochs)

  /** Derive the complete project-resource requirements from the typed binding.
    * This does not fit maps, detect events, or claim numerical success.
    */
  def analysis(
      id: DefinitionId,
      schemas: Vector[DefinitionId] = Vector.empty
  ): ProjectAnalysis[K, M] =
    def records(origin: ProjectInputOrigin): Vector[ProjectRequirement[K, M]] = origin match
      case ProjectInputOrigin.Supplied         => Vector.empty
      case ProjectInputOrigin.Recordings(keys) =>
        keys.toVector.map(ProjectRequirement.Recording.apply)
    def geometry(coordinates: ProjectCoordinates): Vector[ProjectRequirement[K, M]] =
      coordinates match
        case ProjectCoordinates.Native            => Vector.empty
        case ProjectCoordinates.DegreesFromPixels =>
          Vector(ProjectRequirement.ViewingGeometry())
    def spatial(
        keys: ProjectInputOrigin,
        grid: SessionKey,
        coordinates: ProjectCoordinates
    ): Vector[ProjectRequirement[K, M]] =
      records(keys) ++ Vector(ProjectRequirement.Grid(grid)) ++ geometry(coordinates)
    val required: Vector[ProjectRequirement[K, M]] = this match
      case FixationStudy(keys, grid, coordinates) => spatial(keys, grid, coordinates)
      case EventRecording(key, areas, anchors)    =>
        Vector(
          ProjectRequirement.Recording(key),
          ProjectRequirement.ViewingGeometry(),
          ProjectRequirement.Areas(areas)
        ) ++ anchors.map(_.requirement)
      case TemporalStudy(keys, grid, coordinates, windows) =>
        spatial(keys, grid, coordinates) ++ (windows match
          case ProjectTemporalWindows.Supplied()        => Vector.empty
          case ProjectTemporalWindows.Observed(anchors) => anchors.toVector.map(_.requirement))
      case Repetition(keys, grid, coordinates)             => spatial(keys, grid, coordinates)
      case PointSampling(keys, grid, coordinates)          => spatial(keys, grid, coordinates)
      case TemplateFit(ProjectTemplateInput.FixedFeatures) => Vector.empty
      case TemplateFit(ProjectTemplateInput.MeanMap(grid)) =>
        Vector(ProjectRequirement.Grid(grid))
      case Decomposition(grid)                   => Vector(ProjectRequirement.Grid(grid))
      case ScanpathComparison(keys, coordinates) => records(keys) ++ geometry(coordinates)
      case Epochs(anchors)                       => anchors.toVector.map(_.requirement)
    ProjectAnalysis(id, required ++ schemas.map(ProjectRequirement.Schema.apply))

/** One resource query over typed workflow bindings. Catalogue order and repeated
  * configured entries are retained, including component entries.
  */
object ProjectCatalogue:
  def query[K, M: Eq, U <: Unit2D](
      session: Session[U],
      evidence: ProjectEvidence[K, M],
      entries: Vector[(DefinitionId, ProjectWorkflow[K, M], Vector[DefinitionId])]
  ): ProjectAvailability[K, M] =
    ProjectPrerequisites.query(
      session,
      evidence,
      entries.map((id, workflow, schemas) => workflow.analysis(id, schemas))
    )
