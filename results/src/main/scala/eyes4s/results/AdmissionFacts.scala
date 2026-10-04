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

package eyes4s.results

import cats.syntax.all.*
import eyes4s.kernel.{GeometryError, Span}
import eyes4s.plan.{
  AdmissionLedger,
  AdmissionReason,
  Disposition,
  Fact,
  FactCode,
  FactError,
  FactSlot,
  FactSource,
  FactValue,
  LedgerCount,
  QuarantineCause,
  ShareBasis,
  TrialDisposition,
  WindowTally
}

/** The admission facts of a methods text (CR6d), read from an admission
  * ledger and the plan's window tallies over the admitted trials. Each fact's
  * source is the ledger count it states.
  */
object AdmissionFacts:
  /** The facts of `ledger` and `tallies` (`StudyPlan.windowTallies`; a
    * refused tally counts for nothing):
    *
    *   - `FixationRecords`: the source records;
    *   - with a trial inventory, `InventoryTrials` and its `Admitted`,
    *     `Quarantined` (with each cause's count), `NoFixations` and `Absent`
    *     trials; without one, the trials the records name by key: quarantined
    *     when a record was quarantined with the trial, admitted when one was
    *     admitted, otherwise with no fixations (absent trials are not known);
    *   - with tallies, `TalliedRecords`, the records and trials outside the
    *     window and the screen, and the outside-window share of fixation
    *     duration when it is defined;
    *   - `OffScreenPolicy`.
    */
  def of[K](
      ledger: AdmissionLedger[K],
      tallies: Vector[(K, Either[GeometryError, WindowTally])]
  ): Either[FactError, Vector[Fact]] =
    def count(slot: FactSlot, ledgerCount: LedgerCount, n: Long) =
      (slot, FactSource.Ledger(ledgerCount), FactValue.Count(n))
    val trials = ledger.inventory match
      case Some(inventory) =>
        val dispositions = inventory.trials.map(_.disposition)
        count(
          FactSlot.InventoryTrials,
          LedgerCount.InventoryTrials,
          dispositions.size.toLong
        ) +:
          dispositionCounts(dispositions, absent = true)
      case None => dispositionCounts(keyedDispositions(ledger), absent = false)
    val tallied = tallies.flatMap(_._2.toOption)
    val window  = Option
      .when(tallied.nonEmpty) {
        def sum(f: WindowTally => Int)    = tallied.map(f(_).toLong).sum
        def trials(f: WindowTally => Int) = tallied.count(f(_) > 0).toLong
        def span(f: WindowTally => Span)  = tallied.map(f).foldLeft(Span.zero)(_ + _)
        val share                         = WindowTally
          .of(
            sum(_.outsideScreen).toInt,
            sum(_.outsideWindow).toInt,
            sum(_.total).toInt,
            span(_.outsideScreenDuration),
            span(_.outsideWindowDuration),
            span(_.totalDuration)
          )
          .toOption
          .flatMap(_.outsideWindowShare)
        Vector(
          count(FactSlot.TalliedRecords, LedgerCount.TalliedRecords, sum(_.total)),
          count(
            FactSlot.RecordsOutsideWindow,
            LedgerCount.RecordsOutsideWindow,
            sum(_.outsideWindow)
          ),
          count(
            FactSlot.TrialsOutsideWindow,
            LedgerCount.TrialsOutsideWindow,
            trials(_.outsideWindow)
          )
        ) ++ share.toVector.map(s =>
          (
            FactSlot.OutsideWindowShare,
            FactSource.Ledger(LedgerCount.OutsideWindowShare),
            FactValue.Share(s, ShareBasis.FixationDuration)
          )
        ) ++ Vector(
          count(
            FactSlot.RecordsOutsideScreen,
            LedgerCount.RecordsOutsideScreen,
            sum(_.outsideScreen)
          ),
          count(
            FactSlot.TrialsOutsideScreen,
            LedgerCount.TrialsOutsideScreen,
            trials(_.outsideScreen)
          )
        )
      }
      .toVector
      .flatten
    val facts =
      Vector(
        count(FactSlot.FixationRecords, LedgerCount.FixationRecords, ledger.records.size.toLong)
      ) ++ trials ++ window :+ (
        FactSlot.OffScreenPolicy,
        FactSource.Ledger(LedgerCount.OffScreenPolicy),
        FactValue.OffScreen(ledger.policy.offScreen)
      )
    facts.traverse((slot, source, value) => Fact.of(slot, source, value))

  /** The trials the records name by key, in first-record order, with what
    * admission did with each.
    */
  private def keyedDispositions[K](ledger: AdmissionLedger[K]): Vector[TrialDisposition] =
    val byKey = ledger.records.flatMap(r =>
      r.disposition match
        case Disposition.Admitted(key, _)          => Some(key -> r.disposition)
        case Disposition.Rejected(_, Some(key), _) => Some(key -> r.disposition)
        case Disposition.Rejected(_, None, _)      => None
    )
    byKey.map(_._1).distinct.map { key =>
      val records = byKey.collect { case (`key`, d) => d }
      records
        .collectFirst {
          case Disposition.Rejected(_, _, AdmissionReason.Quarantined(_, cause)) =>
            TrialDisposition.Quarantined(cause)
        }
        .orElse(
          records.collectFirst { case Disposition.Admitted(_, _) => TrialDisposition.Admitted }
        )
        .getOrElse(TrialDisposition.NoFixations)
    }

  private def dispositionCounts(
      dispositions: Vector[TrialDisposition],
      absent: Boolean
  ): Vector[(FactSlot, FactSource, FactValue)] =
    def count(slot: FactSlot, ledgerCount: LedgerCount, n: Int) =
      (slot, FactSource.Ledger(ledgerCount), FactValue.Count(n.toLong))
    val causes: Vector[QuarantineCause] = dispositions.collect {
      case TrialDisposition.Quarantined(c) => c
    }
    val byCode = causes.map(FactCode.of).groupBy(identity).toVector.sortBy(_._1.value)
    Vector(
      count(
        FactSlot.Admitted,
        LedgerCount.Admitted,
        dispositions.count(_ == TrialDisposition.Admitted)
      ),
      count(FactSlot.Quarantined, LedgerCount.Quarantined, causes.size)
    ) ++ byCode.map((code, all) =>
      count(FactSlot.QuarantineCause(code), LedgerCount.Cause(code), all.size)
    ) ++ Vector(
      count(
        FactSlot.NoFixations,
        LedgerCount.NoFixations,
        dispositions.count(_ == TrialDisposition.NoFixations)
      )
    ) ++ Option
      .when(absent)(
        count(
          FactSlot.Absent,
          LedgerCount.Absent,
          dispositions.count(_ == TrialDisposition.Absent)
        )
      )
      .toVector
