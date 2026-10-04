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

package eyes4s.io

import eyes4s.codec.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

/** Admission under an explicit policy: off-screen records admitted and
  * reported, or quarantined as in version 1; recorded coordinate corrections
  * applied before containment; conflicting rules named; and the ledger that
  * records all of it on the wire.
  */
class AdmissionPolicySuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)
  private val screen                        = get(Frame.screen("screen", 20, 10))
  private val columns = get(FixationColumns.of("fixation", "x", "y", "onset", "duration", "n"))
  private val keys    = get(FixationKeyReader.study("participant", "image", "phase"))
  private val header  =
    Vector("participant", "image", "phase", "fixation", "x", "y", "onset", "duration", "n")
  private def row(p: String, i: String, ordinal: Int, x: String, y: String, onset: Int) =
    Vector(p, i, "encode", ordinal.toString, x, y, onset.toString, "100", "10")
  private val rows = Vector(
    row("p1", "a", 0, "2", "3", 0),
    row("p1", "a", 1, "25", "3", 200), // off the screen, finite
    row("p1", "b", 0, "4", "8", 0),
    row("p2", "a", 0, "5", "1", 0),
    row("p2", "b", 0, "6", "2", 0)
  )
  private def csv(values: Vector[Vector[String]] = rows) = Rfc4180.encode(header +: values)
  private val a1                                         = StudyKey("p1", "a", "encode")
  private val b1                                         = StudyKey("p1", "b", "encode")
  private val a2                                         = StudyKey("p2", "a", "encode")
  private val b2                                         = StudyKey("p2", "b", "encode")

  private def admit(
      policy: AdmissionPolicy[StudyKey],
      values: Vector[Vector[String]] = rows
  ): FixationImport[StudyKey, Px] =
    get(
      FixationCsv.admit(csv(values), columns, keys, screen, TimestampUnit.Milliseconds, policy)
    )

  private def centres(imported: FixationImport[StudyKey, Px], key: StudyKey): Vector[Pt[Px]] =
    imported.accepted.rows
      .find(_.key == key)
      .toVector
      .flatMap(_.value.fixations.toVector.map(_.centre))

  test("by default an off-screen record is admitted, listed and reported, not quarantined") {
    val imported = admit(AdmissionPolicy.default)
    assertEquals(imported.rejected, Vector.empty)
    assertEquals(imported.accepted.rows.map(_.key), Vector(a1, b1, a2, b2))
    assertEquals(imported.outsideFrame, Vector(OutsideFrame(3, 25.0, 3.0, screen.id)))
    val ledger =
      get(FixationEvidence.ledger("p.csv", imported, AdmissionDecision.RequireComplete))
    assertEquals(ledger.outcome, AdmissionOutcome.Complete)
    assertEquals(ledger.policy.offScreen, OffScreenPolicy.ExcludeRecord)
    assertEquals(ledger.outsideFrame, imported.outsideFrame)
    assertEquals(get(ledger.checkAgainst(get(imported.requireComplete))), ())
  }

  test("the version-1 read and QuarantineTrial reject the record and quarantine its trial") {
    val v1 = get(FixationCsv.read(csv(), columns, keys, screen, TimestampUnit.Milliseconds))
    val q  = admit(AdmissionPolicy(OffScreenPolicy.QuarantineTrial, Vector.empty))
    Vector(v1, q).foreach { imported =>
      assertEquals(imported.accepted.rows.map(_.key), Vector(b1, a2, b2))
      assertEquals(
        imported.rejected.map(r => r.rowNumber -> r.error),
        Vector(
          2 -> FixationRowError.Trial(Vector(2, 3), QuarantineCause.RejectedRecords),
          3 -> FixationRowError.Position(25.0, 3.0, screen.id)
        )
      )
      assertEquals(imported.outsideFrame, Vector.empty)
    }
    val ledger = get(FixationEvidence.ledger("p.csv", v1, AdmissionDecision.ReviewExclusions))
    assert(ledger.isVersion1)
  }

  test("non-finite and unparseable coordinates still quarantine under ExcludeRecord") {
    val bad      = rows.updated(1, row("p1", "a", 1, "Infinity", "3", 200))
    val imported = admit(AdmissionPolicy.default, bad)
    assertEquals(imported.accepted.rows.map(_.key), Vector(b1, a2, b2))
    assertEquals(
      imported.rejected.map(_.error).last,
      FixationRowError.Number("x", "Infinity", "a finite number")
    )
    val text =
      admit(AdmissionPolicy.default, rows.updated(1, row("p1", "a", 1, "far", "3", 200)))
    assertEquals(text.accepted.rows.map(_.key), Vector(b1, a2, b2))
  }

  test("corrections apply to the parsed position before containment; raw fields are kept") {
    val policy = AdmissionPolicy[StudyKey](
      OffScreenPolicy.ExcludeRecord,
      Vector(
        AppliedCorrection(CorrectionScope.Participant("p2"), Correction.FlipY),
        AppliedCorrection(CorrectionScope.Trial(a1), get(Correction.translate(-6, 0)))
      )
    )
    val imported = admit(policy)
    assertEquals(imported.rejected, Vector.empty)
    assertEquals(centres(imported, a1), Vector(Pt[Px](-4, 3), Pt[Px](19, 3)))
    assertEquals(centres(imported, b1), Vector(Pt[Px](4, 8)))
    assertEquals(centres(imported, a2), Vector(Pt[Px](5, 9)))
    assertEquals(centres(imported, b2), Vector(Pt[Px](6, 8)))
    // Containment is decided after the correction: record 2 is now off the
    // screen and record 3, off the screen as written, is now on it.
    assertEquals(imported.outsideFrame, Vector(OutsideFrame(2, -4.0, 3.0, screen.id)))
    assertEquals(imported.sourceRows, rows)
    val ledger =
      get(FixationEvidence.ledger("p.csv", imported, AdmissionDecision.RequireComplete))
    assertEquals(ledger.policy, policy)
    val plain = admit(AdmissionPolicy.default)
    assertEquals(
      FixationEvidence.source("p.csv", imported),
      FixationEvidence.source("p.csv", plain)
    )
    assertNotEquals(
      get(imported.requireComplete).reference,
      get(plain.requireComplete).reference
    )
  }

  test("a trial two rules cover is quarantined, naming both rules") {
    val policy = AdmissionPolicy[StudyKey](
      OffScreenPolicy.ExcludeRecord,
      Vector(
        AppliedCorrection(CorrectionScope.Trial(b2), Correction.FlipX),
        AppliedCorrection(CorrectionScope.Participant("p2"), Correction.FlipY)
      )
    )
    val imported = admit(policy)
    assertEquals(imported.accepted.rows.map(_.key), Vector(a1, b1, a2))
    assertEquals(
      imported.rejected.map(r => r.rowNumber -> r.error),
      Vector(6 -> FixationRowError.Trial(Vector(6), QuarantineCause.CorrectionConflict(0, 1)))
    )
    val ledger =
      get(FixationEvidence.ledger("p.csv", imported, AdmissionDecision.ReviewExclusions))
    assertEquals(
      Diagnostic
        .of(ledger.rejected.last.disposition match
          case Disposition.Rejected(_, _, reason) => reason
          case other                              => fail(s"$other"))
        .causes
        .map(_.code.render),
      Vector("quarantine.correction-conflict")
    )
  }

  test("a participant-scoped rule needs a key reader that names the participant") {
    val anonymous = get(
      FixationKeyReader.of[StudyKey](Vector("participant", "image", "phase"))(
        fields => Right(StudyKey(fields("participant"), fields("image"), fields("phase"))),
        key => ClockId(s"trial:${key.participant}:${key.stimulus}")
      )
    )
    val policy = AdmissionPolicy[StudyKey](
      OffScreenPolicy.ExcludeRecord,
      Vector(AppliedCorrection(CorrectionScope.Participant("p2"), Correction.FlipY))
    )
    assertEquals(
      FixationCsv
        .admit(csv(), columns, anonymous, screen, TimestampUnit.Milliseconds, policy)
        .left
        .toOption,
      Some(FixationImportError.ParticipantScope(Vector(0)))
    )
  }

  test("the ledger records the policy on the wire; a version-1 ledger keeps version 1") {
    val codec  = StudyInputCodecs.study[Px]
    val policy = AdmissionPolicy[StudyKey](
      OffScreenPolicy.ExcludeRecord,
      Vector(AppliedCorrection(CorrectionScope.AllTrials(), get(Correction.translate(0.5, 0))))
    )
    val ledger = get(
      FixationEvidence.ledger("p.csv", admit(policy), AdmissionDecision.RequireComplete)
    )
    val json = get(codec.ledger.encode(ledger))
    assertEquals(json.hcursor.downField("schema").get[Int]("version"), Right(2))
    assertEquals(get(codec.ledger.decode(json)), ledger)
    val v1 = get(
      FixationEvidence.ledger(
        "p.csv",
        get(FixationCsv.read(csv(), columns, keys, screen, TimestampUnit.Milliseconds)),
        AdmissionDecision.ReviewExclusions
      )
    )
    val old = get(codec.ledger.encode(v1))
    assertEquals(old.hcursor.downField("schema").get[Int]("version"), Right(1))
    assertEquals(get(codec.ledger.decode(old)).policy, AdmissionPolicy.version1[StudyKey])
  }

  test("a flip keeps a record on the half-open screen on it, edges included") {
    val edges = Vector(
      row("p2", "a", 0, "0", "0", 0),
      row("p2", "b", 0, "19.999999999999996", "9.999999999999998", 0)
    )
    val policy = AdmissionPolicy[StudyKey](
      OffScreenPolicy.QuarantineTrial,
      Vector(
        AppliedCorrection(CorrectionScope.Trial(a2), Correction.FlipY),
        AppliedCorrection(CorrectionScope.Trial(b2), Correction.FlipX)
      )
    )
    val imported = admit(policy, edges)
    assertEquals(imported.rejected, Vector.empty)
    assertEquals(imported.accepted.rows.map(_.key), Vector(a2, b2))
    imported.accepted.rows.foreach(t =>
      t.value.fixations.foreach(f => assert(screen.contains(f.centre), s"${f.centre}"))
    )
  }

  private val trialKeys = get(FixationKeyReader.trial("participant", "phase", "trial", "item"))
  private val trialHeader =
    Vector(
      "participant",
      "phase",
      "trial",
      "fixation",
      "x",
      "y",
      "onset",
      "duration",
      "n",
      "item"
    )
  private def trialRows(rows: Vector[String]*) =
    get(
      FixationCsv.read(
        Rfc4180.encode(trialHeader +: rows.toVector),
        columns,
        trialKeys,
        screen,
        TimestampUnit.Milliseconds
      )
    )

  test("a v1-policy ledger that names a v2-only cause is written as version 2") {
    val imported = trialRows(
      Vector("p1", "encode", "t1", "0", "2", "3", "0", "100", "10", "a"),
      Vector("p1", "encode", "t1", "1", "4", "3", "200", "100", "10", "b")
    )
    val ledger =
      get(FixationEvidence.ledger("t.csv", imported, AdmissionDecision.ReviewExclusions))
    val json = get(StudyInputCodecs.trial[Px].ledger.encode(ledger))
    assertEquals(json.hcursor.downField("schema").get[Int]("version"), Right(2))
  }

  test("records of one trial naming two items quarantine it once, naming both items") {
    val imported = trialRows(
      Vector("p1", "encode", "t1", "0", "2", "3", "0", "100", "10", "a"),
      Vector("p1", "encode", "t1", "1", "4", "3", "200", "100", "10", "b"),
      Vector("p1", "encode", "t2", "0", "4", "3", "0", "100", "10", "c")
    )
    assertEquals(imported.accepted.rows.map(_.key.trial), Vector("t2"))
    assertEquals(
      imported.rejected.map(r => r.rowNumber -> r.error).distinct,
      Vector(
        2 -> FixationRowError
          .Trial(Vector(2, 3), QuarantineCause.ItemConflict(Vector("a", "b"))),
        3 -> FixationRowError.Trial(
          Vector(2, 3),
          QuarantineCause.ItemConflict(Vector("a", "b"))
        )
      )
    )
    val ledger =
      get(FixationEvidence.ledger("t.csv", imported, AdmissionDecision.ReviewExclusions))
    assertEquals(ledger.quarantined.size, 1)
  }

  test("a parsed key on an otherwise invalid row still joins its occurrence conflict") {
    val keys = get(
      FixationKeyReader.trial("participant", "phase", "trial", "item", Some("occurrence"))
    )
    val header =
      Vector(
        "participant",
        "phase",
        "trial",
        "occurrence",
        "fixation",
        "x",
        "y",
        "onset",
        "duration",
        "n",
        "item"
      )
    val imported = get(
      FixationCsv.read(
        Rfc4180.encode(
          Vector(
            header,
            Vector("p1", "retrieval", "t1", "1", "0", "bad", "3", "0", "100", "10", "d"),
            Vector("p1", "retrieval", "t1", "2", "1", "4", "3", "200", "100", "10", "d")
          )
        ),
        columns,
        keys,
        screen,
        TimestampUnit.Milliseconds
      )
    )
    assertEquals(imported.accepted.rows, Vector.empty)
    assertEquals(
      imported.rejected.map(_.error).distinct,
      Vector(
        FixationRowError.Number("x", "bad", "a finite number"),
        FixationRowError.Trial(Vector(2, 3), QuarantineCause.OccurrenceConflict(Vector(1, 2)))
      )
    )
  }

  test("item conflict takes precedence when a trial label also disagrees on occurrence") {
    val keys = get(
      FixationKeyReader.trial("participant", "phase", "trial", "item", Some("occurrence"))
    )
    val header = Vector(
      "participant",
      "phase",
      "trial",
      "occurrence",
      "fixation",
      "x",
      "y",
      "onset",
      "duration",
      "n",
      "item"
    )
    val imported = get(
      FixationCsv.read(
        Rfc4180.encode(
          Vector(
            header,
            Vector("p1", "retrieval", "t1", "2", "0", "2", "3", "0", "100", "10", "b"),
            Vector("p1", "retrieval", "t1", "1", "1", "4", "3", "200", "100", "10", "a")
          )
        ),
        columns,
        keys,
        screen,
        TimestampUnit.Milliseconds
      )
    )
    assertEquals(
      imported.rejected.map(_.error).distinct,
      Vector(
        FixationRowError.Trial(Vector(2, 3), QuarantineCause.ItemConflict(Vector("a", "b")))
      )
    )
  }
