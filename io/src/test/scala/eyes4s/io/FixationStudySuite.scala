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
import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.examples.{MatchedControlFixtures, StudyGuide}
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

class FixationStudySuite extends munit.FunSuite:
  private val OracleTolerance               = 1e-12
  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)
  private val frame                         = get(Frame.screen("matched-control-display", 2, 2))
  private val grid                          = get(Grid.over(frame, 2, 2))
  private val columns                       = get(
    FixationColumns.of("fixation", "x_px", "y_px", "onset_us", "duration_us", "sample_count")
  )
  private val keys   = get(FixationKeyReader.study("participant", "image", "phase"))
  private val header = Vector(
    "participant",
    "image",
    "phase",
    "fixation",
    "x_px",
    "y_px",
    "onset_us",
    "duration_us",
    "sample_count"
  )
  private val records = MatchedControlFixtures.fixations.map(r =>
    Vector(
      r.participant,
      r.image,
      r.phase,
      r.ordinal.toString,
      r.x.toString,
      r.y.toString,
      r.onsetMicros.toString,
      r.durationMicros.toString,
      r.sampleCount.toString
    )
  )
  private def csv(rows: Vector[Vector[String]] = records): String =
    Rfc4180.encode(header +: rows)
  private def read(rows: Vector[Vector[String]] = records) =
    get(FixationCsv.read(csv(rows), columns, keys, frame, TimestampUnit.Microseconds))

  test("exact compiled guide imports, saves, reloads and exports the pinned study") {
    val imported = read()
    assertEquals(imported.sourceRows.size, 48)
    assertEquals(imported.accepted.size, 12)
    assertEquals(imported.rejected, Vector.empty)
    assert(imported.accepted.rows.forall(_.value.source.isEmpty))
    val output = get(StudyGuide.run(csv()))
    assertEquals(output.contrasts.rows.size, 6)
    val parsed = get(Rfc4180.decode(output.contrasts.encode))
    assertEquals(parsed.head, ContrastCsv.header)
    assert(parsed.tail.forall(_.size == ContrastCsv.header.size))
    val rows = parsed.tail.map(r => ContrastCsv.header.zip(r).toMap)
    MatchedControlFixtures.reductions.foreach { expected =>
      val row = rows
        .find(r => s"${r("participant")}/${r("stimulus")}/${r("phase")}" == expected.id)
        .getOrElse(fail(expected.id))
      assertEqualsDouble(row("difference").toDouble, expected.difference, OracleTolerance)
      assertEquals(row("matched_contributing"), "1")
      assertEquals(row("control_contributing"), "2")
      assertEquals(row("status"), "ok")
      assertEquals(row("spatial_unit"), "px")
      assert(
        get(StudyCodecs.cosine[Px].codec.parse(row("plan_json")))
          .diff(get(StudyCodecs.cosine[Px].codec.parse(output.savedPlan)))
          .isEmpty
      )
    }
  }

  test("one invalid fixation quarantines the entire keyed trial") {
    val altered  = records.updated(0, records.head.updated(7, "-1"))
    val imported = read(altered)
    assertEquals(imported.sourceRows, altered)
    assertEquals(imported.rejected.size, 4)
    assertEquals(imported.accepted.size, 11)
    assertEquals(imported.rejected.map(_.rowNumber), Vector(2, 3, 4, 5))
    assert(imported.requireComplete.isLeft)
    assert(
      imported.rejected.forall(
        _.key.contains(StudyKey(records.head(0), records.head(1), records.head(2)))
      )
    )
  }

  test("duplicate ordinals and temporal overlap invalidate a whole trial") {
    val duplicate = records.updated(1, records(1).updated(3, records.head(3)))
    assertEquals(read(duplicate).rejected.size, 4)
    val overlap = records.updated(1, records(1).updated(6, records.head(6)))
    assertEquals(read(overlap).rejected.size, 4)
  }

  test("unknown key rows and wrong row widths remain observable") {
    val blank  = records.updated(0, records.head.updated(0, ""))
    val report = read(blank)
    assert(report.rejected.exists(r => r.rowNumber == 2 && r.key.isEmpty))
    assert(report.requireComplete.isLeft)
    val short = records.updated(0, records.head.dropRight(1))
    assert(read(short).rejected.exists(_.error == FixationRowError.Width(9, 8)))
    val wide = records.updated(0, records.head :+ "extra")
    assert(read(wide).rejected.exists(_.error == FixationRowError.Width(9, 10)))
  }

  test("malformed CSV and duplicate or missing header names fail explicitly") {
    assert(
      FixationCsv
        .read("a,\"unfinished", columns, keys, frame, TimestampUnit.Microseconds)
        .isLeft
    )
    assert(
      FixationCsv
        .read(
          Rfc4180.encode(header.updated(1, "participant") +: records),
          columns,
          keys,
          frame,
          TimestampUnit.Microseconds
        )
        .isLeft
    )
    assert(FixationColumns.of("i", "x", "x", "t", "d", "n").isLeft)
  }

  test("nonfinite coordinates, off-frame positions and invalid support are rejected") {
    Vector(
      4 -> "NaN",
      4 -> "Infinity",
      4 -> "2.1",
      8 -> "0",
      8 -> "1.5",
      6 -> "999999999999999999999999999"
    ).foreach { case (column, bad) =>
      assert(read(records.updated(0, records.head.updated(column, bad))).requireComplete.isLeft)
    }
  }

  test("decimal seconds and milliseconds preserve exact microsecond timing") {
    Vector(
      TimestampUnit.Seconds      -> BigDecimal(1000000),
      TimestampUnit.Milliseconds -> BigDecimal(1000)
    ).foreach { case (unit, divisor) =>
      val converted = records.map(r =>
        r.updated(6, (BigDecimal(r(6)) / divisor).toString)
          .updated(7, (BigDecimal(r(7)) / divisor).toString)
      )
      val report = get(FixationCsv.read(csv(converted), columns, keys, frame, unit))
      val a      = get(read().requireComplete)
      val b      = get(report.requireComplete)
      assertEquals(a.hash, b.hash)
    }
  }

  test("quoted identifiers survive import and export without column corruption") {
    val quoted = records.map(r => r.updated(0, r(0) + ", \"lab\"\nA"))
    val output = get(StudyGuide.run(csv(quoted)))
    val parsed = get(Rfc4180.decode(output.contrasts.encode))
    assertEquals(parsed.tail.size, 6)
    assert(parsed.tail.forall(row => row(2).contains(", \"lab\"\nA")))
  }

  test("export rejects wrong plans and malformed component projections") {
    val input                = get(read().requireComplete)
    def plan(weight: Weight) = get(
      StudyPlan.cosine(
        input.reference,
        grid,
        "recall",
        "encode",
        weight,
        Vector(StudyEstimate.Binned()),
        FailurePolicy.RequireAll
      )
    )
    val original    = plan(Weight.Duration)
    val result      = get(original.run(input))
    val persistence = StudyCodecs.cosine[Px]
    assert(
      ContrastCsv
        .document(plan(Weight.Uniform), result, persistence, ScoreColumns.similarity)
        .isLeft
    )
    val invalid = get(
      ScoreColumns.of[eyes4s.compare.Similarity, SignedDifference](Vector("value"))(
        _ => Vector.empty,
        d => Vector(d.value)
      )
    )
    assert(ContrastCsv.document(original, result, persistence, invalid).isLeft)
  }

  test("estimation failures and excluded phases are exported, with explicit row scopes") {
    val extra  = records.take(4).map(_.updated(2, "practice"))
    val output = get(
      StudyGuide.run(
        csv(records ++ extra),
        Vector(StudyEstimate.Gaussian(get(Sigma.px(0.01)), eyes4s.surface.EdgePolicy.Truncate))
      )
    )
    val rows = output.contrasts.rows.map(r => ContrastCsv.header.zip(r).toMap)
    assertEquals(rows.count(_("scope") == "excluded_trial"), 1)
    assertEquals(rows.count(r => r("scope") == "contrast" && r("status") == "failed"), 6)
    assert(rows.filter(_("scope") == "contrast").forall(_("control_failed") == "2"))
    assert(rows.forall(_("estimation_failures").nonEmpty))
  }

  test("timestamp admission handles exact long values, ties and extreme decimal exponents") {
    val base  = records.take(1)
    val cases = Vector(
      "9007199254740993" -> 9007199254740993L,
      "-0.5"             -> 0L,
      "0.5"              -> 1L,
      "1e-2147483647"    -> 0L
    )
    cases.foreach { case (text, expected) =>
      val report = read(base.map(_.updated(6, text)))
      assertEquals(
        get(report.requireComplete).trials.rows.head.value.extent.onset.toMicros,
        expected
      )
    }
    assert(read(base.map(_.updated(6, "1e2147483647"))).requireComplete.isLeft)
  }

  test("structured scores export five named signed components per focal key") {
    val input  = get(read().requireComplete)
    val method = new StudyMethod[Unit, Px, MultiMatchScore, MultiMatchDifference](
      get(DefinitionId.of("test.five-component-cosine", 1)),
      "Five components",
      _ => Vector.empty,
      _ =>
        new Compare[Mass[Px], Mass[Px], MultiMatchScore]:
          val info = MeasureInfo(
            "Five components",
            "Analytic export fixture",
            MeasureScale.Probability,
            None
          )
          def compare(a: Mass[Px], b: Mass[Px]): Either[CompareError, MultiMatchScore] =
            Distribution
              .cosine[Px]
              .compare(a, b)
              .flatMap(s =>
                MultiMatchScore
                  .of(s.value, s.value / 2, s.value / 4, s.value / 5, s.value / 10)
                  .left
                  .map(CompareError.InvalidScore("export fixture", _))
              )
    )
    val layout = StudyKey.layout(DefinitionId.studyLayout)
    val plan   = get(
      StudyPlan.of(
        input.reference,
        layout,
        grid,
        "recall",
        "encode",
        Weight.Duration,
        Vector(StudyEstimate.Binned()),
        FailurePolicy.RequireAll,
        method,
        ()
      )
    )
    val persistence = new StudyCodec(
      DefinitionId.study,
      layout,
      StudyCodecs.key(DefinitionId.studyKey),
      method,
      VersionedCodec.unit(DefinitionId.unit)
    )
    val table = get(
      ContrastCsv.document(plan, get(plan.run(input)), persistence, ScoreColumns.multiMatch)
    )
    assertEquals(table.rows.size, 30)
    val rows        = table.rows.map(row => ContrastCsv.header.zip(row).toMap)
    val multipliers = Map(
      "shape"     -> 1.0,
      "direction" -> 0.5,
      "length"    -> 0.25,
      "position"  -> 0.2,
      "duration"  -> 0.1
    )
    MatchedControlFixtures.reductions.foreach { expected =>
      val found = rows.filter(row =>
        s"${row("participant")}/${row("stimulus")}/${row("phase")}" == expected.id
      )
      assertEquals(found.map(_("component")).toSet, multipliers.keySet)
      found.foreach(row =>
        assertEqualsDouble(
          row("difference").toDouble,
          expected.difference * multipliers(row("component")),
          OracleTolerance
        )
      )
    }
  }

  private val inputCodec = StudyInputCodecs.study[Px]

  test(
    "complete import yields a complete ledger and reconstructs the study without the importer"
  ) {
    val imported = read()
    val input    = get(imported.requireComplete)
    val ledger   = get(
      FixationEvidence.ledger(
        "matched-control.csv",
        imported,
        AdmissionDecision.RequireComplete
      )
    )
    assertEquals(ledger.outcome, AdmissionOutcome.Complete)
    assertEquals(ledger.records.size, imported.sourceRows.size)
    assertEquals(ledger.records.map(_.record), (2 to 49).toVector)
    assertEquals(ledger.admitted.size, 48)
    assertEquals(ledger.rejected, Vector.empty)
    assertEquals(ledger.source, FixationEvidence.source("matched-control.csv", imported))
    assertEquals(ledger.checkAgainst(input), Right(()))
    assertEquals(
      imported.admitted.map(r => (r.rowNumber, r.key, r.ordinal)),
      records.zipWithIndex.map { case (r, i) =>
        (i + 2, StudyKey(r(0), r(1), r(2)), r(3).toInt)
      }
    )
    val inputJson  = get(inputCodec.input.encode(input)).noSpaces
    val ledgerJson = get(inputCodec.ledger.encode(ledger)).noSpaces
    val restored   = get(inputCodec.input.parse(inputJson))
    val decisions  = get(inputCodec.ledger.parse(ledgerJson))
    assertEquals(restored.reference, input.reference)
    assertEquals(restored.trials.rows.map(_.key), input.trials.rows.map(_.key))
    assertEquals(decisions, ledger)
    assertEquals(decisions.checkAgainst(restored), Right(()))
    val plan = get(
      StudyPlan.cosine(
        restored.reference,
        grid,
        "recall",
        "encode",
        Weight.Duration,
        Vector(StudyEstimate.Binned()),
        FailurePolicy.RequireAll
      )
    )
    val direct         = get(plan.run(input)).scales.head
    val restoredResult = get(plan.run(restored)).scales.head
    val rows           = get(restoredResult.contrast).rows
    assertEquals(rows.size, 6)
    rows.zip(get(direct.contrast).rows).foreach { case (a, b) =>
      assertEquals(a.key, b.key)
      assertEquals(get(a.difference).value, get(b.difference).value)
    }
    rows.zip(MatchedControlFixtures.reductions).foreach { case (row, expected) =>
      assertEquals(s"${row.key.participant}/${row.key.stimulus}/${row.key.phase}", expected.id)
      assertEqualsDouble(get(row.difference).value, expected.difference, OracleTolerance)
    }
    assertEquals(
      get(plan.prepare(restored)).inputReference,
      get(plan.prepare(input)).inputReference
    )
  }

  test("quarantined trials keep typed reasons, total accounting and the pinned v1 ledger") {
    val altered  = records.updated(0, records.head.updated(7, "-1"))
    val imported = read(altered)
    val refused  = get(
      FixationEvidence.ledger(
        "matched-control.csv",
        imported,
        AdmissionDecision.RequireComplete
      )
    )
    assertEquals(refused.outcome, AdmissionOutcome.Refused)
    assertEquals(refused.records.size, 48)
    assertEquals(refused.admitted.size + refused.rejected.size, imported.sourceRows.size)
    assertEquals(refused.rejected.map(_.record), Vector(2, 3, 4, 5))
    val key = StudyKey("s1", "a", "encode")
    assertEquals(refused.quarantined, Vector(key))
    assertEquals(
      refused.records.head.disposition,
      Disposition.Rejected(
        altered.head,
        Some(key),
        AdmissionReason.Time(
          "0",
          "-1",
          "microseconds",
          "duration must be positive and the interval must fit signed microseconds"
        )
      )
    )
    assertEquals(
      refused.records(1).disposition,
      Disposition.Rejected(
        altered(1),
        Some(key),
        AdmissionReason.Quarantined(Vector(2, 3, 4, 5), QuarantineCause.RejectedRecords)
      )
    )
    assertEquals(
      get(inputCodec.ledger.encode(refused)),
      get(io.circe.parser.parse(StudyInputFixtures.ledgerVersionOne))
    )
    assertEquals(get(inputCodec.ledger.parse(StudyInputFixtures.ledgerVersionOne)), refused)
    val reviewed = get(
      FixationEvidence.ledger(
        "matched-control.csv",
        imported,
        AdmissionDecision.ReviewExclusions
      )
    )
    assertEquals(reviewed.outcome, AdmissionOutcome.ReviewedExclusions)
    assertEquals(reviewed.records, refused.records)
    val accepted = StudyInput(imported.accepted)
    assertEquals(accepted.trials.size, 11)
    assertEquals(reviewed.checkAgainst(accepted), Right(()))
    assertEquals(
      reviewed.checkAgainst(get(read().requireComplete)),
      Left(AdmissionError.UnadmittedTrial(0))
    )
    val restored = get(inputCodec.input.decode(get(inputCodec.input.encode(accepted))))
    assertEquals(restored.reference, accepted.reference)
    assertEquals(
      get(inputCodec.ledger.decode(get(inputCodec.ledger.encode(reviewed)))),
      reviewed
    )
  }

  test("duplicate ordinals, overlaps and unreadable keys are typed ledger reasons") {
    val duplicate = read(records.updated(1, records(1).updated(3, records.head(3))))
    val dupLedger = get(
      FixationEvidence.ledger("dup.csv", duplicate, AdmissionDecision.RequireComplete)
    )
    assertEquals(
      dupLedger.rejected.map(_.disposition).collect {
        case Disposition.Rejected(_, _, AdmissionReason.Quarantined(rows, cause)) =>
          rows -> cause
      },
      Vector.fill(4)(Vector(2, 3, 4, 5) -> QuarantineCause.DuplicateOrdinals)
    )
    val overlap       = read(records.updated(1, records(1).updated(6, records.head(6))))
    val overlapLedger = get(
      FixationEvidence.ledger("overlap.csv", overlap, AdmissionDecision.RequireComplete)
    )
    overlapLedger.rejected.map(_.disposition).foreach {
      case Disposition.Rejected(_, Some(k), AdmissionReason.Quarantined(rows, cause)) =>
        assertEquals(k, StudyKey("s1", "a", "encode"))
        assertEquals(rows, Vector(2, 3, 4, 5))
        cause match
          case QuarantineCause.Overlap(index, _, _) => assertEquals(index, 1)
          case other                                => fail(s"unexpected $other")
      case other => fail(s"unexpected $other")
    }
    val blank       = read(records.updated(0, records.head.updated(0, "")))
    val blankLedger = get(
      FixationEvidence.ledger("blank.csv", blank, AdmissionDecision.RequireComplete)
    )
    blankLedger.records.head.disposition match
      case Disposition.Rejected(raw, None, AdmissionReason.Key(_)) =>
        assertEquals(raw, records.head.updated(0, ""))
      case other => fail(s"unexpected $other")
    assertEquals(blankLedger.records.size, 48)
    val short       = read(records.updated(0, records.head.dropRight(1)))
    val shortLedger = get(
      FixationEvidence.ledger("short.csv", short, AdmissionDecision.RequireComplete)
    )
    assertEquals(
      shortLedger.records.head.disposition,
      Disposition.Rejected(
        records.head.dropRight(1),
        Some(StudyKey("s1", "a", "encode")),
        AdmissionReason.Width(9, 8)
      )
    )
    Vector(dupLedger, overlapLedger, blankLedger, shortLedger).foreach { ledger =>
      assertEquals(get(inputCodec.ledger.decode(get(inputCodec.ledger.encode(ledger)))), ledger)
    }
  }

  test("quoted and newline identifiers survive the input and ledger payloads") {
    val quoted   = records.map(r => r.updated(0, r(0) + ", \"lab\"\nA"))
    val imported = read(quoted)
    val input    = get(imported.requireComplete)
    val ledger   = get(
      FixationEvidence.ledger("quoted.csv", imported, AdmissionDecision.RequireComplete)
    )
    val restored  = get(inputCodec.input.parse(get(inputCodec.input.encode(input)).noSpaces))
    val decisions = get(inputCodec.ledger.parse(get(inputCodec.ledger.encode(ledger)).noSpaces))
    assertEquals(restored.reference, input.reference)
    assert(restored.trials.rows.forall(_.key.participant.contains(", \"lab\"\nA")))
    assertEquals(decisions, ledger)
    assertEquals(decisions.checkAgainst(restored), Right(()))
    assertNotEquals(ledger.source, FixationEvidence.source("quoted.csv", read()))
  }
