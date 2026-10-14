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
import eyes4s.core.Viewing
import eyes4s.design.{Session, SessionKey}
import eyes4s.kernel.*

/** Project resources required by a named analysis. Requirements are data, not
  * captured predicates. Synchronization is directed from observed marks to the
  * recording clock; equal nominal clocks need no fitted conversion.
  */
enum ProjectRequirement[K, M]:
  case Recording(key: SessionKey)
  case ViewingGeometry()
  case Areas(key: SessionKey)
  case Grid(key: SessionKey)
  case ObservedAnchor(
      trial: K,
      recording: SessionKey,
      selector: MarkSelector[M],
      observedClock: ClockId,
      maximumUncertainty: SyncErrorMagnitude
  )
  case Schema(id: DefinitionId)

/** A catalogue entry retains its analysis identity and complete requirement
  * list. The query preserves catalogue and requirement order, including repeated
  * entries; it does not silently merge two differently configured analyses.
  */
final case class ProjectAnalysis[K, M](
    id: DefinitionId,
    requirements: Vector[ProjectRequirement[K, M]]
)

/** Actual observed evidence accompanying a checked spatial Session. Planned
  * timelines have no admission path here. Schema identities include versions;
  * a different installed version does not satisfy a requested one.
  */
final case class ProjectEvidence[K, M](
    viewing: Option[Viewing],
    observed: Map[K, ObservedTimeline[M]],
    synchronization: Vector[SyncEvidence],
    schemas: Set[DefinitionId]
)

enum ProjectFinding[K, M]:
  case MissingRecording(key: SessionKey)
  case MissingViewing()
  case MissingAreas(key: SessionKey)
  case MissingGrid(key: SessionKey)
  case MissingObservedTimeline(trial: K, selector: MarkSelector[M], expected: ClockId)
  case Anchor(underlying: EpochError[K, M])
  case MissingSynchronization(
      trial: K,
      selector: MarkSelector[M],
      source: ClockId,
      target: ClockId,
      maximum: SyncErrorMagnitude,
      available: Vector[(ClockId, ClockId)]
  )
  case ImpreciseSynchronization(
      trial: K,
      selector: MarkSelector[M],
      source: ClockId,
      target: ClockId,
      maximum: SyncErrorMagnitude,
      best: SyncErrorMagnitude
  )
  case MissingSchema(id: DefinitionId, availableVersions: Vector[Int])

  def message: String = this match
    case MissingRecording(key) => s"Missing recording '${key.value}'."
    case MissingViewing()      => "Missing Viewing geometry for degree-based analysis."
    case MissingAreas(key)     => s"Missing AoiSet '${key.value}'."
    case MissingGrid(key)      => s"Missing grid '${key.value}'."
    case MissingObservedTimeline(trial, selector, expected) =>
      s"Trial '$trial' selector=$selector requires an ObservedTimeline on $expected."
    case Anchor(error) => error.message
    case MissingSynchronization(trial, selector, source, target, maximum, available) =>
      s"Trial '$trial' selector=$selector requires SyncEvidence from $source to $target " +
        s"with uncertainty <= ${maximum.render}; available clock pairs=$available."
    case ImpreciseSynchronization(trial, selector, source, target, maximum, best) =>
      s"Trial '$trial' selector=$selector SyncEvidence from $source to $target has best " +
        s"uncertainty ${best.render}, exceeding requested ${maximum.render}."
    case MissingSchema(id, versions) =>
      s"Missing schema '${id.name}' version ${id.version}; available versions=$versions."

object ProjectFinding:
  given diagnose[K, M]: Diagnose[ProjectFinding[K, M], K] =
    Diagnose.instance(DiagnosticCatalog.projectPrerequisite)(
      ProjectPrerequisiteDiagnostics.project[K, M]
    )

/** Availability is a prerequisite result, never a promise of numerical
  * success. Construction belongs to the query, so callers cannot forge a ready
  * report whose findings say otherwise.
  */
final class ProjectAnalysisAvailability[K, M] private[plan] (
    val analysis: ProjectAnalysis[K, M],
    val findings: Vector[ProjectFinding[K, M]]
):
  def availability: Availability =
    if findings.isEmpty then Availability.Ready else Availability.Unavailable
  def available: Boolean                 = availability == Availability.Ready
  def diagnostics: Vector[Diagnostic[K]] =
    findings.map(f => Diagnostic.of(f).within(Locus.Definition(analysis.id)))

final class ProjectAvailability[K, M] private[plan] (
    val analyses: Vector[ProjectAnalysisAvailability[K, M]]
):
  def available: Vector[ProjectAnalysis[K, M]] =
    analyses.filter(_.available).map(_.analysis)
  def unavailable: Vector[ProjectAnalysisAvailability[K, M]] =
    analyses.filterNot(_.available)

object ProjectPrerequisites:
  /** One query over a project's analysis catalogue. No densities, detections,
    * comparisons or epoch bins are computed. Each declared observed selector is
    * resolved by the same operation used by EpochPlan execution.
    */
  def query[K, M: Eq, U <: Unit2D](
      session: Session[U],
      evidence: ProjectEvidence[K, M],
      catalogue: Vector[ProjectAnalysis[K, M]]
  ): ProjectAvailability[K, M] =
    def check(requirement: ProjectRequirement[K, M]): Vector[ProjectFinding[K, M]] =
      import ProjectFinding.*
      requirement match
        case ProjectRequirement.Recording(key) =>
          Option.when(session.recording(key).isEmpty)(MissingRecording(key)).toVector
        case ProjectRequirement.ViewingGeometry() =>
          Option.when(evidence.viewing.isEmpty)(MissingViewing()).toVector
        case ProjectRequirement.Areas(key) =>
          Option.when(session.aoiSet(key).isEmpty)(MissingAreas(key)).toVector
        case ProjectRequirement.Grid(key) =>
          Option.when(session.grid(key).isEmpty)(MissingGrid(key)).toVector
        case ProjectRequirement.Schema(id) =>
          Option
            .when(!evidence.schemas(id))(
              MissingSchema(
                id,
                evidence.schemas.iterator
                  .filter(_.name == id.name)
                  .map(_.version)
                  .toVector
                  .sorted
              )
            )
            .toVector
        case ProjectRequirement.ObservedAnchor(trial, recording, selector, clock, maximum) =>
          val recordingMissing: Vector[ProjectFinding[K, M]] = Option
            .when(session.recording(recording).isEmpty)(
              MissingRecording(recording)
            )
            .toVector
          val timing: Vector[ProjectFinding[K, M]] = evidence.observed.get(trial) match
            case None           => Vector(MissingObservedTimeline(trial, selector, clock))
            case Some(timeline) =>
              selector.resolve(trial, timeline, clock) match
                case Left(error) => Vector(Anchor(error))
                case Right(at)   =>
                  session.recording(recording).toVector.flatMap { value =>
                    val target = value.clock
                    if Agreement.clocks(clock, target).isRight then Vector.empty
                    else
                      val candidates = evidence.synchronization.filter(s =>
                        Agreement.clocks(clock, s.source).isRight &&
                          Agreement.clocks(target, s.target).isRight
                      )
                      val uncertainties = candidates.map(_.uncertaintyAt(at))
                      if uncertainties.exists(_.toMicros <= maximum.toMicros) then Vector.empty
                      else if uncertainties.isEmpty then
                        Vector(
                          MissingSynchronization(
                            trial,
                            selector,
                            clock,
                            target,
                            maximum,
                            evidence.synchronization.map(s => s.source -> s.target)
                          )
                        )
                      else
                        Vector(
                          ImpreciseSynchronization(
                            trial,
                            selector,
                            clock,
                            target,
                            maximum,
                            uncertainties.minBy(_.toMicros)
                          )
                        )
                  }
          recordingMissing ++ timing
    new ProjectAvailability(
      catalogue.map(analysis =>
        new ProjectAnalysisAvailability(analysis, analysis.requirements.flatMap(check))
      )
    )
