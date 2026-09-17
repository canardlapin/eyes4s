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

package eyes4s.codec

import eyes4s.design.*
import eyes4s.examples.MatchedControlFixtures
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import io.circe.Json

/** The frozen v1 payloads pin decoded meaning independently of the encoder. */
class StudyInputV1Suite extends munit.FunSuite:
  private val OracleTolerance               = 1e-12
  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)
  private val codec                         = StudyInputCodecs.study[Px]

  test("frozen study-input v1 fixes the digest, keys, order, clocks and exact times") {
    val input = get(codec.input.parse(StudyInputFixtures.inputVersionOne))
    assertEquals(input.reference.digest, "cebe7474ab5c2aec")
    assertEquals(input.trials.size, 12)
    val keys = input.trials.rows.map(_.key)
    assertEquals(keys, keys.sorted)
    assertEquals(keys.head, StudyKey("s1", "a", "encode"))
    assertEquals(keys.last, StudyKey("s2", "c", "recall"))
    assertEquals(keys.distinct.size, 12)
    input.trials.rows.foreach { trial =>
      assertEquals(trial.value.frame.id, FrameId("matched-control-display"))
      assertEquals(trial.value.frame.bounds.xMax, 2.0)
      assertEquals(trial.value.frame.yAxis, YAxis.Down)
      assertEquals(
        trial.value.clock,
        ClockId(s"fixation-trial:${KeyDigest[StudyKey].digest(trial.key).render}")
      )
      assertEquals(trial.value.n, 4)
      assert(trial.value.source.isEmpty)
      assert(trial.value.fixations.forall(_.dispersion.isEmpty))
    }
    val first = input.trials.rows.head.value
    assertEquals(first.clock, ClockId("fixation-trial:e5ee90ac5de274ac"))
    assertEquals(
      first.fixations.toVector.map(f => (f.span.onset.toMicros, f.span.offset.toMicros)),
      Vector((0L, 100000L), (110000L, 310000L), (320000L, 520000L), (530000L, 930000L))
    )
    assertEquals(
      first.fixations.toVector.map(f => (f.centre.x, f.centre.y, f.sampleCount)),
      Vector((0.5, 0.5, 100), (1.5, 0.5, 200), (0.5, 1.5, 200), (1.5, 1.5, 400))
    )
    assertEquals(
      get(codec.input.encode(input)),
      get(io.circe.parser.parse(StudyInputFixtures.inputVersionOne))
    )
  }

  test("the pinned study-v1 plan runs on the pinned v1 input and reproduces the oracle") {
    val input = get(codec.input.parse(StudyInputFixtures.inputVersionOne))
    val plan  = get(StudyCodecs.cosine[Px].codec.parse(SavedStudyFixtures.versionOne))
    assertEquals(plan.prerequisites(Some(input)), Vector.empty)
    val result = get(plan.run(input))
    val rows   = get(result.scales.head.contrast).rows
    assertEquals(rows.size, 6)
    rows.zip(MatchedControlFixtures.reductions).foreach { case (row, expected) =>
      assertEquals(s"${row.key.participant}/${row.key.stimulus}/${row.key.phase}", expected.id)
      assertEqualsDouble(get(row.difference).value, expected.difference, OracleTolerance)
    }
  }

  test("frozen admission-ledger v1 fixes the source, dispositions and typed reasons") {
    val ledger = get(codec.ledger.parse(StudyInputFixtures.ledgerVersionOne))
    assertEquals(ledger.outcome, AdmissionOutcome.Refused)
    assertEquals(ledger.source.label, "matched-control.csv")
    assertEquals(ledger.source.records.digest, "75c2ca1c758598b5")
    assertEquals(
      ledger.source,
      SourceRef.of(
        "matched-control.csv",
        StudyInputFixtures.header,
        StudyInputFixtures.refusedRecords
      )
    )
    assertEquals(ledger.header, StudyInputFixtures.header)
    assertEquals(ledger.records.size, 48)
    assertEquals(ledger.records.map(_.record), (2 to 49).toVector)
    assertEquals(ledger.rejected.map(_.record), Vector(2, 3, 4, 5))
    assertEquals(ledger.admitted.size, 44)
    val key = StudyKey("s1", "a", "encode")
    assertEquals(
      ledger.records.head.disposition,
      Disposition.Rejected(
        StudyInputFixtures.refusedRecords.head,
        Some(key),
        AdmissionReason.Time(
          "0",
          "-1",
          "microseconds",
          "duration must be positive and the interval must fit signed microseconds"
        )
      )
    )
    ledger.records.slice(1, 4).zip(StudyInputFixtures.refusedRecords.slice(1, 4)).foreach {
      case (record, raw) =>
        assertEquals(
          record.disposition,
          Disposition.Rejected(
            raw,
            Some(key),
            AdmissionReason.Quarantined(Vector(2, 3, 4, 5), QuarantineCause.RejectedRecords)
          )
        )
    }
    assertEquals(ledger.quarantined, Vector(key))
    assertEquals(
      ledger.records(4).disposition,
      Disposition.Admitted(StudyKey("s1", "b", "encode"), 0)
    )
    assertEquals(
      ledger.admitted
        .map(_.disposition)
        .collect { case Disposition.Admitted(k, _) => k }
        .distinct
        .size,
      11
    )
    val accepted = StudyInput(
      Trials(StudyInputFixtures.matchedControl.trials.rows.filterNot(_.key == key))
    )
    assertEquals(ledger.checkAgainst(accepted), Right(()))
    assertEquals(
      ledger.checkAgainst(StudyInputFixtures.matchedControl),
      Left(AdmissionError.UnadmittedTrial(0))
    )
    assertEquals(
      get(codec.ledger.encode(ledger)),
      get(io.circe.parser.parse(StudyInputFixtures.ledgerVersionOne))
    )
  }

  test("v1 fixtures expose dropped records and collapsed occurrences without roundtrip") {
    val ledgerJson = get(io.circe.parser.parse(StudyInputFixtures.ledgerVersionOne))
    val value      = get(ledgerJson.hcursor.get[Json]("value"))
    val records    = get(value.hcursor.get[Vector[Json]]("records"))
    def withRecords(rows: Vector[Json]): Json = ledgerJson.mapObject(
      _.add("value", value.mapObject(_.add("records", Json.arr(rows*))))
    )
    // Dropping the rejected record its quarantine scope refers to is invalid on its own.
    assertEquals(
      codec.ledger.decode(withRecords(records.drop(1))).left.toOption,
      Some(CodecError.Admission(AdmissionError.QuarantineScope(3, Vector(2, 3, 4, 5))))
    )
    // Dropping an admitted record decodes, but no longer accounts for its trial.
    val partial = get(codec.ledger.decode(withRecords(records.dropRight(1))))
    assertEquals(partial.records.size, 47)
    assertEquals(partial.quarantined, Vector(StudyKey("s1", "a", "encode")))
    assertEquals(
      partial.checkAgainst(
        StudyInput(
          Trials(
            StudyInputFixtures.matchedControl.trials.rows
              .filterNot(_.key == StudyKey("s1", "a", "encode"))
          )
        )
      ),
      Left(AdmissionError.FixationCount(10, 4, 3))
    )
    val inputJson = get(io.circe.parser.parse(StudyInputFixtures.inputVersionOne))
    val trials    = get(inputJson.hcursor.downField("value").get[Json]("trials"))
    val rows      = get(trials.hcursor.get[Vector[Json]]("value"))
    val collapsed = inputJson.mapObject(
      _.add(
        "value",
        get(inputJson.hcursor.get[Json]("value")).mapObject(
          _.add(
            "trials",
            trials.mapObject(_.add("value", Json.arr(rows.updated(1, rows.head)*)))
          )
        )
      )
    )
    codec.input.decode(collapsed) match
      case Left(CodecError.InputIdentity("cebe7474ab5c2aec", other)) =>
        assertNotEquals(other, "cebe7474ab5c2aec")
      case other => fail(s"unexpected $other")
  }
