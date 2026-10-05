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

/** A reference-phase trial the trial inventory declares for a focal trial's
  * participant and item, with what admission did with it.
  */
final case class DeclaredReference(trial: TrialIdentity, disposition: TrialDisposition)
    derives CanEqual

/** Why a focal trial has no matched reference, judged against the trial
  * inventory: the design as declared, before admission (owner decision on
  * bead S0.7b; review of S7.1). The pairing key is the plan's: participant
  * and item, and the occurrence under [[MatchedReferences.SameOccurrence]]
  * (the focal trial's own) and under `Select(At(n))` (occurrence n).
  */
enum UnmatchedKind derives CanEqual:
  /** No inventory was given, so the reason is not judged. */
  case Undetermined

  /** The inventory declares no reference-phase trial with the focal trial's
    * pairing key: unmatched by design, as a recognition lure or a novel
    * probe is.
    */
  case NoReferenceInDesign

  /** The inventory declares such trials and admission admitted none of them:
    * each with its disposition, in inventory order.
    */
  case ReferenceNotAdmitted(references: Vector[DeclaredReference])

  /** Such a trial was admitted, but the pairing could not use it: a repeated
    * key, or not the occurrence a selection keeps.
    */
  case ReferenceNotPairable(references: Vector[TrialIdentity])

  /** The stable token a consumer maps to wording. */
  def code: String = this match
    case Undetermined            => "undetermined"
    case NoReferenceInDesign     => "no-reference-in-design"
    case ReferenceNotAdmitted(_) => "reference-not-admitted"
    case ReferenceNotPairable(_) => "reference-not-pairable"

/** The reasons of a prepared study's unmatched focal trials, in their
  * schedule order, and how many have each reason.
  */
final class UnmatchedReasons[K] private[plan] (val reasons: Vector[(K, UnmatchedKind)]):
  def reason(key: K): Option[UnmatchedKind] = reasons.collectFirst { case (`key`, r) => r }

  /** Queries without a reference by design: the byDesignQueries count. */
  def byDesign: Int = reasons.count(_._2 == UnmatchedKind.NoReferenceInDesign)

  def notAdmitted: Int = reasons.count {
    case (_, UnmatchedKind.ReferenceNotAdmitted(_)) => true
    case _                                          => false
  }

  def notPairable: Int = reasons.count {
    case (_, UnmatchedKind.ReferenceNotPairable(_)) => true
    case _                                          => false
  }

object UnmatchedReasons:
  /** The reason of each of `unmatched`, focal keys of a plan with `layout`,
    * `pairing` and reference phase `referencePhase`, against `inventory`.
    */
  def of[K](
      layout: StudyLayout[K],
      pairing: StudyPairing,
      referencePhase: String,
      unmatched: Vector[K],
      inventory: InventoryLedger
  ): UnmatchedReasons[K] =
    val sameOccurrence =
      pairing.matched == MatchedReferences.SameOccurrence && layout.occurrence.isDefined
    // Select(At(n)) uses only occurrence n of each item; First and Last
    // choose among the admitted trials, so any declared occurrence counts.
    val selected = pairing.matched match
      case MatchedReferences.Select(OccurrenceChoice.At(n)) if layout.occurrence.isDefined =>
        Some(n.value)
      case _ => None
    // The inventory's reference-phase trials by participant and item.
    val declared = inventory.trials
      .filter(_.identity.phase == referencePhase)
      .flatMap(t => t.keyItem.map(item => (t.identity.participant, item) -> t))
      .groupMap(_._1)(_._2)
    new UnmatchedReasons(unmatched.map { key =>
      val occurrence = layout.occurrence.map(_(key).value)
      val references = declared
        .getOrElse((layout.participant(key), layout.stimulus(key)), Vector.empty)
        .filter(t => !sameOccurrence || occurrence.contains(t.identity.occurrence.value))
        .filter(t => selected.forall(_ == t.identity.occurrence.value))
      val admitted = references.filter(_.disposition == TrialDisposition.Admitted)
      key -> (
        if references.isEmpty then UnmatchedKind.NoReferenceInDesign
        else if admitted.isEmpty then
          UnmatchedKind.ReferenceNotAdmitted(
            references.map(t => DeclaredReference(t.identity, t.disposition))
          )
        else UnmatchedKind.ReferenceNotPairable(admitted.map(_.identity))
      )
    })

  /** Every reason undetermined: no inventory was given. */
  def undetermined[K](unmatched: Vector[K]): UnmatchedReasons[K] =
    new UnmatchedReasons(unmatched.map(_ -> UnmatchedKind.Undetermined))
