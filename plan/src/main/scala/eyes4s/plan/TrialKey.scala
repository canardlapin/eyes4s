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

import eyes4s.design.*

/** Built-in identities of the trial-keyed layout. */
object TrialKeyDefinitions:
  /** The wire schema of a [[TrialKey]]. */
  val trialKey: DefinitionId = DefinitionId.builtIn("eyes4s.trial-key", 1)

  /** The layout that identifies a trial by participant, phase, trial label
    * and occurrence, and matches on its item attribute.
    */
  val trialLayout: DefinitionId =
    DefinitionId.builtIn("eyes4s.participant-phase-trial-occurrence", 1)

/** A trial identified by participant, phase, trial label and occurrence, with
  * the item it is matched on as a separate attribute.
  *
  * The item is not part of the trial's identity: two trials may share a
  * display and remember different items, and a retrieval trial may name an
  * item no study trial showed. Two keys with the same identity and different
  * items are a conflict the importer and the prepared study refuse
  * (`QuarantineCause.ItemConflict`, `PlanError.MatchItemConflict`).
  */
final case class TrialKey private (
    participant: String,
    phase: String,
    trial: String,
    occurrence: TrialOccurrence,
    item: String
) derives CanEqual:
  /** The identity fields, without the item. */
  def identity: (String, String, String, TrialOccurrence) =
    (participant, phase, trial, occurrence)

object TrialKey:
  def of(
      participant: String,
      phase: String,
      trial: String,
      occurrence: TrialOccurrence,
      item: String
  ): Either[PlanError, TrialKey] =
    Vector("participant" -> participant, "phase" -> phase, "trial" -> trial, "item" -> item)
      .collectFirst {
        case (field, value) if value.trim.isEmpty => PlanError.BlankKeyField(field)
      }
      .toLeft(new TrialKey(participant, phase, trial, occurrence, item))

  given KeyDigest[TrialKey] = KeyDigest.derived[TrialKey]
  given Ordering[TrialKey]  =
    Ordering.by(k => (k.participant, k.phase, k.trial, k.occurrence.value, k.item))

  /** Participant, phase and item as the study projections, with the
    * occurrence and the trial label that identify a trial.
    */
  def layout(id: DefinitionId): StudyLayout[TrialKey] = new StudyLayout(
    id,
    Projection.named("participant")(_.participant),
    Projection.named("item")(_.item),
    Projection.named("phase")(_.phase),
    Some(Projection.named("occurrence")(_.occurrence)),
    Some(Projection.named("trial")(_.trial))
  )
