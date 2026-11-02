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

import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px

/** A checked nominal identifier for one location-only trial. */
sealed abstract case class LocationTrialId private (value: String) derives CanEqual

object LocationTrialId:
  private def apply(value: String): LocationTrialId = new LocationTrialId(value) {}

  def of(value: String): Either[LocationInputError, LocationTrialId] =
    Either.cond(value.nonEmpty, LocationTrialId(value), LocationInputError.EmptyTrialId(value))

  private[eyes4s] given DiagnosticOperand[LocationTrialId, Nothing] =
    DiagnosticOperand.of(id => Operand.Name(id.value))

/** A checked nominal identifier for the source of an observed location. */
sealed abstract case class LocationSourceId private (value: String) derives CanEqual

object LocationSourceId:
  private def apply(value: String): LocationSourceId = new LocationSourceId(value) {}

  def of(value: String): Either[LocationInputError, LocationSourceId] =
    Either.cond(
      value.nonEmpty,
      LocationSourceId(value),
      LocationInputError.EmptySourceId(value)
    )

  private[eyes4s] given DiagnosticOperand[LocationSourceId, Nothing] =
    DiagnosticOperand.of(id => Operand.Name(id.value))

/** One imported point. `sourceOrdinal` is a source-provenance identity only;
  * it does not imply chronology or a sampling interval.
  */
final case class LocationObservation(
    source: LocationSourceId,
    sourceOrdinal: Long,
    position: Pt[Px]
) derives CanEqual

/** Refusals while assembling location-only inputs. */
enum LocationInputError derives CanEqual:
  case EmptyTrialId(value: String)
  case EmptySourceId(value: String)
  case EmptyInput
  case EmptyTrial(trial: LocationTrialId)
  case NonPositiveSourceOrdinal(trial: LocationTrialId, source: LocationSourceId, value: Long)
  case DuplicateTrial(trial: LocationTrialId)
  case NonFinitePosition(
      trial: LocationTrialId,
      source: LocationSourceId,
      sourceOrdinal: Long,
      x: Double,
      y: Double
  )
  case PositionOutsideFrame(
      trial: LocationTrialId,
      source: LocationSourceId,
      sourceOrdinal: Long,
      x: Double,
      y: Double,
      frame: FrameId
  )
  case DuplicateProvenance(source: LocationSourceId, sourceOrdinal: Long)

  def message: String = this match
    case EmptyTrialId(value)  => s"A location trial identifier must not be empty, got '$value'."
    case EmptySourceId(value) =>
      s"A location source identifier must not be empty, got '$value'."
    case EmptyInput        => "Location input needs one or more trials."
    case EmptyTrial(trial) => s"Location trial '${trial.value}' has no observations."
    case NonPositiveSourceOrdinal(trial, source, value) =>
      s"Location trial '${trial.value}' source '${source.value}' has source ordinal=$value; source ordinals count raw rows from 1."
    case DuplicateTrial(trial) =>
      s"Location input contains trial '${trial.value}' more than once."
    case NonFinitePosition(trial, source, ordinal, x, y) =>
      s"Location trial '${trial.value}' source '${source.value}' ordinal=$ordinal has non-finite position ($x, $y)."
    case PositionOutsideFrame(trial, source, ordinal, x, y, frame) =>
      s"Location trial '${trial.value}' source '${source.value}' ordinal=$ordinal position ($x, $y) lies outside frame '${frame.name}'."
    case DuplicateProvenance(source, ordinal) =>
      s"Location source '${source.value}' ordinal=$ordinal occurs more than once."

object LocationInputError:
  given diagnose: Diagnose[LocationInputError, Nothing] =
    Diagnose.derived(LocationDiagnostics.input)(_.message)

/** A non-empty, caller-ordered collection of location observations for one
  * trial. Its order is retained as source evidence but has no analytic meaning.
  */
final class LocationTrial private (
    val id: LocationTrialId,
    val observations: Vector[LocationObservation]
)

object LocationTrial:
  def of(
      id: LocationTrialId,
      observations: Vector[LocationObservation]
  ): Either[LocationInputError, LocationTrial] =
    if observations.isEmpty then Left(LocationInputError.EmptyTrial(id))
    else
      observations.iterator
        .find(_.sourceOrdinal <= 0L)
        .map(observation =>
          LocationInputError
            .NonPositiveSourceOrdinal(id, observation.source, observation.sourceOrdinal)
        )
        .toLeft(new LocationTrial(id, observations))

/** A checked location-only input over one pixel frame. */
final class LocationInput private (
    val frame: Frame[Px],
    val trials: Vector[LocationTrial]
):
  lazy val byId: Map[LocationTrialId, LocationTrial] = trials.map(t => t.id -> t).toMap

object LocationInput:
  def of(
      frame: Frame[Px],
      trials: Vector[LocationTrial]
  ): Either[LocationInputError, LocationInput] =
    if trials.isEmpty then return Left(LocationInputError.EmptyInput)
    val trialIds   = scala.collection.mutable.HashSet.empty[LocationTrialId]
    val provenance = scala.collection.mutable.HashSet.empty[(LocationSourceId, Long)]
    var trialIndex = 0
    while trialIndex < trials.size do
      val trial = trials(trialIndex)
      if !trialIds.add(trial.id) then return Left(LocationInputError.DuplicateTrial(trial.id))
      var observationIndex = 0
      while observationIndex < trial.observations.size do
        val observation = trial.observations(observationIndex)
        val point       = observation.position
        if !point.x.isFinite || !point.y.isFinite then
          return Left(
            LocationInputError.NonFinitePosition(
              trial.id,
              observation.source,
              observation.sourceOrdinal,
              point.x,
              point.y
            )
          )
        if !frame.contains(point) then
          return Left(
            LocationInputError.PositionOutsideFrame(
              trial.id,
              observation.source,
              observation.sourceOrdinal,
              point.x,
              point.y,
              frame.id
            )
          )
        if !provenance.add(observation.source -> observation.sourceOrdinal) then
          return Left(
            LocationInputError.DuplicateProvenance(
              observation.source,
              observation.sourceOrdinal
            )
          )
        observationIndex += 1
      trialIndex += 1
    Right(new LocationInput(frame, trials))
