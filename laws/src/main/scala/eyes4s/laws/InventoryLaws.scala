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

package eyes4s.laws

import eyes4s.plan.*

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Prop}
import org.typelevel.discipline.Laws

/** How a generated inventory trial's fixation records are constructed. */
enum TrialPlan derives CanEqual:
  /** No record names the trial. */
  case Absent

  /** Valid, ordered, non-overlapping records. */
  case Admitted(records: Int)

  /** Every record has a sample count of 0. */
  case AllRejected(records: Int)

  /** The first record has a sample count of 0; the others are valid. */
  case OneRejected(records: Int)

  /** The second record repeats the first record's ordinal. */
  case DuplicateOrdinals(records: Int)

  /** The second fixation begins before the first ends. */
  case Overlap(records: Int)

  /** Valid records naming an item other than the inventory's. */
  case ItemConflict(records: Int)

  def count: Int = this match
    case Absent               => 0
    case Admitted(n)          => n
    case AllRejected(n)       => n
    case OneRejected(n)       => n
    case DuplicateOrdinals(n) => n
    case Overlap(n)           => n
    case ItemConflict(n)      => n

/** One generated fixation record, before it is written to the table. */
final case class ScenarioRecord(
    identity: TrialIdentity,
    ordinal: Int,
    x: Double,
    y: Double,
    onset: Long,
    duration: Long,
    samples: Int,
    item: String
) derives CanEqual

/** A generated trial inventory and fixation table.
  *
  * The tables use these columns, which an instance declares:
  *
  *   - inventory: `participant, phase, trial, occurrence, item`;
  *   - fixations: `participant, phase, trial, occurrence, ordinal, x, y,
  *     onset, duration, samples`, and `item` when `recordItems` is true.
  *
  * Times are integer milliseconds; `samples` is a positive-count column;
  * positions lie inside a 100 x 100 frame. Records of all trials are
  * interleaved in file order by `order`, so no law can rely on trials being
  * contiguous.
  */
final case class InventoryScenario(
    recordItems: Boolean,
    trials: Vector[(TrialIdentity, String, TrialPlan)],
    unlisted: Vector[(TrialIdentity, Int)],
    order: Vector[Int]
) derives CanEqual:
  /** Every record in file order; record number = index + 2. */
  lazy val records: Vector[ScenarioRecord] =
    val planned = trials.flatMap((id, item, plan) => InventoryScenario.build(id, item, plan)) ++
      unlisted.flatMap((id, n) =>
        InventoryScenario.build(id, "unlisted", TrialPlan.Admitted(n))
      )
    order.map(planned)

  /** Record numbers of one identity, in file order. */
  def recordsOf(identity: TrialIdentity): Vector[Int] =
    records.zipWithIndex.collect { case (r, i) if r.identity == identity => i + 2 }

  def inventoryTable: String =
    val header = "participant,phase,trial,occurrence,item"
    (header +: trials.map((id, item, _) =>
      Vector(id.participant, id.phase, id.trial, id.occurrence.value.toString, item)
        .mkString(",")
    )).mkString("", "\n", "\n")

  def fixationTable: String =
    val header = Vector(
      "participant",
      "phase",
      "trial",
      "occurrence",
      "ordinal",
      "x",
      "y",
      "onset",
      "duration",
      "samples"
    ) ++ Option.when(recordItems)("item")
    (header.mkString(",") +: records.map { r =>
      (Vector(
        r.identity.participant,
        r.identity.phase,
        r.identity.trial,
        r.identity.occurrence.value.toString,
        r.ordinal.toString,
        r.x.toString,
        r.y.toString,
        r.onset.toString,
        r.duration.toString,
        r.samples.toString
      ) ++ Option.when(recordItems)(r.item)).mkString(",")
    }).mkString("", "\n", "\n")

object InventoryScenario:
  private[laws] def build(
      id: TrialIdentity,
      item: String,
      plan: TrialPlan
  ): Vector[ScenarioRecord] =
    Vector.tabulate(plan.count) { i =>
      val onset = plan match
        case TrialPlan.Overlap(_) if i == 1 => 20L
        case _                              => 100L * i
      val ordinal = plan match
        case TrialPlan.DuplicateOrdinals(_) if i == 1 => 1
        case _                                        => i + 1
      val samples = plan match
        case TrialPlan.AllRejected(_)           => 0
        case TrialPlan.OneRejected(_) if i == 0 => 0
        case _                                  => 25
      val named = plan match
        case TrialPlan.ItemConflict(_) => item + "-other"
        case _                         => item
      ScenarioRecord(id, ordinal, 10.0 + 7 * i, 20.0 + 5 * i, onset, 50L, samples, named)
    }

  private def trialIdentity(
      participant: String,
      phase: String,
      trial: String,
      occurrence: Int
  ) =
    (for
      n  <- TrialOccurrence.of(occurrence)
      id <- TrialIdentity.of(participant, phase, trial, n)
    yield id).fold(e => throw new IllegalArgumentException(e.message), identity => identity)

  /** Every plan kind is reachable; item conflicts only when records carry
    * items. Trial labels are distinct, so no two trials share a label.
    */
  def gen: Gen[InventoryScenario] =
    for
      recordItems <- Gen.oneOf(true, false)
      size        <- Gen.choose(1, 8)
      extra       <- Gen.choose(0, 2)
      plans       <- Gen.listOfN(size, planGen(recordItems))
      shape       <- Gen.listOfN(size + extra, Gen.zip(Gen.oneOf("P1", "P2"), Gen.choose(1, 2)))
      items       <- Gen.listOfN(size, Gen.oneOf("beach", "dog", "tower"))
      unlistedN   <- Gen.listOfN(extra, Gen.choose(1, 2))
      keys        <- Gen.listOfN(size * 4 + extra * 2, Gen.choose(0, Int.MaxValue))
    yield
      val ids = shape.zipWithIndex.map { case ((p, occurrence), i) =>
        trialIdentity(p, if i % 2 == 0 then "Encoding" else "Retrieval", s"t$i", occurrence)
      }
      val trials = ids.take(size).zip(items).zip(plans).map { case ((id, item), plan) =>
        (id, item, plan)
      }
      val unlisted = ids.drop(size).zip(unlistedN)
      val total    = plans.map(_.count).sum + unlistedN.sum
      val order    = (0 until total).toVector.sortBy(i => (keys(i % keys.size), i))
      InventoryScenario(recordItems, trials.toVector, unlisted.toVector, order)

  private def planGen(recordItems: Boolean): Gen[TrialPlan] =
    val base = Vector(
      Gen.const(TrialPlan.Absent),
      Gen.choose(1, 3).map(TrialPlan.Admitted.apply),
      Gen.choose(1, 3).map(TrialPlan.AllRejected.apply),
      Gen.choose(2, 3).map(TrialPlan.OneRejected.apply),
      Gen.choose(2, 3).map(TrialPlan.DuplicateOrdinals.apply),
      Gen.choose(2, 3).map(TrialPlan.Overlap.apply)
    ) ++ Option.when(recordItems)(Gen.choose(1, 3).map(TrialPlan.ItemConflict.apply))
    Gen.oneOf(base).flatMap(identity)

/** Laws of a fixation admission joined to a trial inventory. `admit`
  * admits a generated [[InventoryScenario]] under the columns it documents and
  * returns the admission ledger with its inventory, or a message.
  *
  * The laws hold of the ledger alone, so they hold of a ledger saved and read
  * back as well as of the importer's own.
  */
final class InventoryLaws(
    admit: InventoryScenario => Either[String, AdmissionLedger[TrialKey]]
) extends Laws:

  private def withLedger(
      check: (InventoryScenario, AdmissionLedger[TrialKey], InventoryLedger) => Prop
  ): Prop =
    forAll(InventoryScenario.gen) { s =>
      admit(s) match
        case Left(error)   => Prop(false) :| s"admission refused: $error"
        case Right(ledger) =>
          ledger.inventory match
            case None            => Prop(false) :| "the ledger has no inventory"
            case Some(inventory) => check(s, ledger, inventory)
    }

  private def rowLevel(d: Disposition[TrialKey]): Boolean = d match
    case Disposition.Rejected(_, _, AdmissionReason.Quarantined(_, _)) => false
    case Disposition.Rejected(_, _, _)                                 => true
    case Disposition.Admitted(_, _)                                    => false

  def inventory: RuleSet = new SimpleRuleSet(
    "trialInventory",
    "partition: every inventory trial has one entry, in order, and every record one owner" ->
      withLedger { (s, ledger, inventory) =>
        val owners =
          inventory.trials.flatMap(_.records) ++ inventory.unlisted.flatMap(_.records)
        Prop(inventory.trials.map(_.identity) == s.trials.map(_._1)) :| "inventory order" &&
        Prop(ledger.records.map(_.record) == s.records.indices.map(_ + 2).toVector) :|
          "one ledger record per source record" &&
          Prop(owners.sorted == ledger.records.map(_.record)) :| s"owners $owners"
      },
    "a trial every record of which is rejected on its own has no fixations" ->
      withLedger { (_, ledger, inventory) =>
        val byRecord = ledger.records.map(r => r.record -> r.disposition).toMap
        Prop.all(inventory.trials.map { t =>
          val allRowLevel = t.records.nonEmpty && t.records.forall(r => rowLevel(byRecord(r)))
          Prop(allRowLevel == (t.disposition == TrialDisposition.NoFixations)) :|
            s"${t.identity.render} is ${t.disposition.label}"
        }*)
      },
    "a trial is absent exactly when no record names it" ->
      withLedger { (s, _, inventory) =>
        Prop.all(inventory.trials.map { t =>
          val none = s.recordsOf(t.identity).isEmpty
          Prop((t.disposition == TrialDisposition.Absent) == none) &&
          Prop(t.records == s.recordsOf(t.identity)) :| s"${t.identity.render} records"
        }*)
      },
    "each trial's disposition is the one its construction implies" ->
      withLedger { (s, _, inventory) =>
        Prop.all(inventory.trials.zip(s.trials).map { case (t, (_, item, plan)) =>
          val expected: TrialDisposition => Boolean = plan match
            case TrialPlan.Absent         => _ == TrialDisposition.Absent
            case TrialPlan.Admitted(_)    => _ == TrialDisposition.Admitted
            case TrialPlan.AllRejected(_) => _ == TrialDisposition.NoFixations
            case TrialPlan.OneRejected(_) =>
              _ == TrialDisposition.Quarantined(QuarantineCause.RejectedRecords)
            case TrialPlan.DuplicateOrdinals(_) =>
              _ == TrialDisposition.Quarantined(QuarantineCause.DuplicateOrdinals)
            case TrialPlan.Overlap(_) => {
              case TrialDisposition.Quarantined(QuarantineCause.Overlap(_, _, _)) => true
              case _                                                              => false
            }
            case TrialPlan.ItemConflict(_) =>
              _ == TrialDisposition.Quarantined(
                QuarantineCause.InventoryItemConflict(item, Vector(item + "-other"))
              )
          Prop(expected(t.disposition)) :| s"${t.identity.render}: $plan gave ${t.disposition}"
        }*)
      },
    "records of a trial outside the inventory are listed and never admitted" ->
      withLedger { (s, ledger, inventory) =>
        val byRecord = ledger.records.map(r => r.record -> r.disposition).toMap
        Prop(inventory.unlisted.map(_.identity).toSet == s.unlisted.map(_._1).toSet) &&
        Prop.all(inventory.unlisted.map { u =>
          val id = u.identity
          Prop(u.records == s.recordsOf(id)) && Prop(
            u.records.forall(r =>
              byRecord(r) match
                case Disposition.Rejected(_, _, AdmissionReason.Quarantined(_, cause)) =>
                  cause == QuarantineCause.NotInInventory(
                    id.participant,
                    id.phase,
                    id.trial,
                    id.occurrence.value
                  )
                case _ => false
            )
          ) :| s"${id.render} records ${u.records}"
        }*)
      },
    "every keyed record of an inventory trial carries the inventory's item" ->
      withLedger { (s, ledger, inventory) =>
        val byRecord = ledger.records.map(r => r.record -> r.disposition).toMap
        Prop.all(inventory.trials.zip(s.trials).map { case (t, (_, item, _)) =>
          val items = t.records.flatMap(r =>
            byRecord(r) match
              case Disposition.Admitted(key, _)    => Vector(key.item)
              case Disposition.Rejected(_, key, _) => key.map(_.item).toVector
          )
          Prop(items.forall(_ == item)) :| s"${t.identity.render} records name $items"
        }*)
      },
    "an admitted trial admits every record under the inventory's item" ->
      withLedger { (s, ledger, inventory) =>
        val byRecord = ledger.records.map(r => r.record -> r.disposition).toMap
        Prop.all(inventory.trials.zip(s.trials).collect {
          case (t, (_, item, _)) if t.disposition == TrialDisposition.Admitted =>
            Prop(
              t.records.forall(r =>
                byRecord(r) match
                  case Disposition.Admitted(key, _) =>
                    key.item == item && TrialIdentity.of(key) == t.identity
                  case _ => false
              )
            ) :| s"${t.identity.render}"
        }*)
      }
  )
