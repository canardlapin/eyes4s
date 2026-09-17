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

import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px

/** Ledger invariants that no codec can bypass: a ledger that lies about its
  * own records is refused at construction, and cross-checks against an input
  * report the first offender deterministically.
  */
class AdmissionLedgerSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)
  private val frame                         = get(Frame.screen("display", 2, 2))
  private val header = Vector("participant", "image", "phase", "fixation")
  private val source = SourceRef.of("test.csv", header, Vector.empty)
  private val a      = StudyKey("s1", "a", "encode")
  private val b      = StudyKey("s1", "b", "encode")
  private val raw    = Vector("s1", "a", "encode", "0")

  private def admitted(record: Int, key: StudyKey, ordinal: Int) =
    SourceRecord(record, Disposition.Admitted(key, ordinal))
  private def quarantined(record: Int, key: StudyKey, scope: Vector[Int]) =
    SourceRecord(
      record,
      Disposition.Rejected(
        raw,
        Some(key),
        AdmissionReason.Quarantined(scope, QuarantineCause.RejectedRecords)
      )
    )
  private def rejected(record: Int, key: Option[StudyKey]) =
    SourceRecord(record, Disposition.Rejected(raw, key, AdmissionReason.Width(4, 3)))
  private def ledger(records: SourceRecord[StudyKey]*) =
    AdmissionLedger.decide(source, header, records.toVector, AdmissionDecision.ReviewExclusions)

  private def trial(key: StudyKey, n: Int): Trial[StudyKey, Unit, Scanpath[Px]] =
    val clock = ClockId(s"trial:${KeyDigest[StudyKey].digest(key).render}")
    val fixes = (0 until n).map { i =>
      get(
        Event.Fixation.withoutDispersion(
          get(
            Interval
              .of(clock, Instant.micros(i * 200000L), Instant.micros(i * 200000L + 100000L))
          ),
          Pt[Px](0.5, 0.5),
          10
        )
      )
    }
    Trial(key, (), get(Scanpath.of(frame, clock, IArray.from(fixes))))

  test("a quarantine scope naming an admitted record is refused") {
    assertEquals(
      ledger(rejected(2, Some(a)), quarantined(3, a, Vector(2, 3, 4)), admitted(4, b, 0)),
      Left(AdmissionError.QuarantineAdmitted(3, 4))
    )
  }

  test("a quarantined key that is also admitted is refused, naming both records") {
    assertEquals(
      ledger(rejected(2, Some(a)), quarantined(3, a, Vector(2, 3)), admitted(4, a, 0)),
      Left(AdmissionError.QuarantinedKeyAdmitted(3, 4))
    )
    // The same records with the admitted key changed are a valid ledger.
    assert(
      ledger(rejected(2, Some(a)), quarantined(3, a, Vector(2, 3)), admitted(4, b, 0)).isRight
    )
  }

  test(
    "negative ordinals are refused; duplicate ordinals name the first offender in record order"
  ) {
    assertEquals(
      ledger(admitted(2, a, -1)),
      Left(AdmissionError.NegativeOrdinal(2, -1))
    )
    assertEquals(
      ledger(admitted(2, b, 5), admitted(3, b, 5), admitted(4, a, 1), admitted(5, a, 1)),
      Left(AdmissionError.DuplicateOrdinal(Vector(2, 3), 5))
    )
    assertEquals(
      ledger(admitted(2, a, 1), admitted(3, b, 5), admitted(4, b, 5), admitted(5, a, 1)),
      Left(AdmissionError.DuplicateOrdinal(Vector(2, 5), 1))
    )
  }

  test("checkAgainst accepts the source's own ordinal values and names the first offender") {
    val complete = get(ledger(admitted(2, a, 10), admitted(3, a, 30), admitted(4, b, 7)))
    val input    = StudyInput(Trials(Vector(trial(a, 2), trial(b, 1))))
    assertEquals(complete.checkAgainst(input), Right(()))
    assertEquals(
      complete.checkAgainst(StudyInput(Trials(Vector(trial(a, 3), trial(b, 1))))),
      Left(AdmissionError.FixationCount(0, 3, 2))
    )
    assertEquals(
      complete.checkAgainst(StudyInput(Trials(Vector(trial(b, 1), trial(a, 2), trial(b, 1))))),
      Left(AdmissionError.AmbiguousTrial(Vector(0, 2)))
    )
    assertEquals(
      complete.checkAgainst(StudyInput(Trials(Vector(trial(a, 2))))),
      Left(AdmissionError.UnknownTrial(Vector(4)))
    )
    val partial = get(ledger(admitted(2, a, 10), admitted(3, a, 30)))
    assertEquals(partial.checkAgainst(input), Left(AdmissionError.UnadmittedTrial(1)))
  }

  test("every scanpath error becomes a typed quarantine cause with its operands") {
    assertEquals(QuarantineCause.of(ScanpathError.NoFixations), QuarantineCause.NoFixations)
    assertEquals(
      QuarantineCause.of(ScanpathError.OutOfOrder(2, "[0, 1)", "[0, 2)")),
      QuarantineCause.Overlap(2, "[0, 1)", "[0, 2)")
    )
    assertEquals(
      QuarantineCause.of(ScanpathError.WrongClock(1, "c", "d")),
      QuarantineCause.WrongClock(1, "c", "d")
    )
    assertEquals(
      QuarantineCause.of(ScanpathError.UnmappableFixation(0, frame.id, FrameId("t"), 1.0, 2.0)),
      QuarantineCause.UnmappableFixation(0, frame.id, FrameId("t"), 1.0, 2.0)
    )
  }
