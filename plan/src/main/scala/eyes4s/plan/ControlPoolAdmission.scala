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

/** Admission facts about declared control-pool references. These explain
  * absent candidates, not a counterfactual number of pairs: First/Last can
  * select another admitted occurrence, and a sampled pool can omit a reference
  * regardless of admission.
  */
object ControlPoolAdmission:
  /** One result for every query, in query order. References are returned in
    * inventory order, restricted to the same participant and other items.
    * SameSelection applies occurrence restrictions; AllOccurrences explicitly
    * includes every occurrence. No pair schedule is traversed.
    */
  def of[K](
      layout: StudyLayout[K],
      pairing: StudyPairing,
      referencePhase: String,
      queries: Vector[K],
      inventory: InventoryLedger
  ): Vector[(K, Vector[DeclaredReference])] =
    val references = inventory.trials
      .filter(_.identity.phase == referencePhase)
      .groupBy(_.identity.participant)
    queries.map { query =>
      val item       = layout.stimulus(query)
      val occurrence = layout.occurrence.map(_(query).value)
      val candidates =
        references.getOrElse(layout.participant(query), Vector.empty).filter { trial =>
          val selected = pairing.controls match
            case ControlReferences.AllOccurrences => true
            case ControlReferences.SameSelection  =>
              pairing.matched match
                case MatchedReferences.SameOccurrence if layout.occurrence.isDefined =>
                  occurrence.contains(trial.identity.occurrence.value)
                case MatchedReferences.Select(OccurrenceChoice.At(n))
                    if layout.occurrence.isDefined =>
                  trial.identity.occurrence.value == n.value
                case _ => true
          trial.keyItem.exists(
            _ != item
          ) && selected && trial.disposition != TrialDisposition.Admitted
        }
      query -> candidates.map(t => DeclaredReference(t.identity, t.disposition))
    }
