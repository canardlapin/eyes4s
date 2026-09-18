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

import eyes4s.kernel.Unit2D

/** An admission error resolved against the input and ledger it concerns: the
  * trials it names, by key, and links to the ledger records at fault.
  *
  * `AdmissionError` itself carries input positions and record numbers, which
  * are all that `AdmissionLedger.checkAgainst` can name without a key type.
  * Resolution reads the positions back to keys and the records back to their
  * keys; a trial with no record in the ledger is an explicit missing link.
  */
final case class LedgerRefusal[+K](
    error: AdmissionError,
    trials: Vector[K],
    links: Vector[SourceLink[K]]
) derives CanEqual

object LedgerRefusal:
  def of[K, U <: Unit2D](
      input: StudyInput[K, U],
      ledger: AdmissionLedger[K],
      error: AdmissionError
  ): LedgerRefusal[K] =
    import AdmissionError.*
    val rows                          = input.trials.rows
    def atPosition(index: Int)        = rows.lift(index).map(_.key).toVector
    def named(record: Int): Vector[K] =
      ledger.records
        .find(_.record == record)
        .toVector
        .flatMap(_.disposition match
          case Disposition.Admitted(key, _)    => Vector(key)
          case Disposition.Rejected(_, key, _) => key.toVector)
    def recordsOf(key: K): Vector[Int] = ledger.records.collect {
      case SourceRecord(number, Disposition.Admitted(k, _)) if k == key          => number
      case SourceRecord(number, Disposition.Rejected(_, Some(k), _)) if k == key => number
    }
    // The records an error names itself, and the trials it concerns.
    val (records, trials): (Vector[Int], Vector[K]) = error match
      case NonPositiveRecord(record)            => (Vector(record), named(record))
      case RecordOrder(_, _, record)            => (Vector(record), named(record))
      case NegativeOrdinal(record, _)           => (Vector(record), named(record))
      case DuplicateOrdinal(numbers, _)         => (numbers, numbers.flatMap(named))
      case QuarantineScope(record, _)           => (Vector(record), named(record))
      case QuarantineAdmitted(record, admitted) =>
        (Vector(record, admitted), named(record) ++ named(admitted))
      case QuarantinedKeyAdmitted(record, admitted) =>
        (Vector(record, admitted), named(record) ++ named(admitted))
      case OutcomeMismatch(_, _)      => (ledger.rejected.map(_.record), Vector.empty)
      case AmbiguousTrial(indices)    => (Vector.empty, indices.flatMap(atPosition))
      case UnknownTrial(numbers)      => (numbers, numbers.flatMap(named))
      case UnadmittedTrial(index)     => (Vector.empty, atPosition(index))
      case FixationCount(index, _, _) => (Vector.empty, atPosition(index))
    val keys  = trials.distinct
    val links =
      if records.nonEmpty then records.distinct.map(SourceLink.Record(ledger.source, _))
      else
        keys.flatMap { key =>
          recordsOf(key) match
            case Vector() => Vector(SourceLink.Missing(MissingSource.UnknownTrial(key)))
            case numbers  => numbers.map(SourceLink.Record(ledger.source, _))
        }
    LedgerRefusal(error, keys, links)
