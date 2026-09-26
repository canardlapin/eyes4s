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

import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

/** Source-record pages of a fixation table read against its admission
  * ledger: each record's verbatim text and lines, and for an admitted record
  * the position fields as recorded, whose parse corrects to the admitted
  * position bit for bit, then the image position and degrees.
  */
class FixationSourceTextSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)
  private def data(n: Int): DataRecord      = get(DataRecord.of(n))

  private val screen = get(Frame.screen("screen", 1920, 1080))
  private val window =
    get(Subframe.of(screen, FrameId("image"), get(Bounds.of[Px](448, 156, 1472, 924))))
  private val columns = get(
    FixationColumns.of("fixation", "x_px", "y_px", "onset_ms", "duration_ms", "sample_count")
  )
  private val keys = get(FixationKeyReader.study("participant", "image", "phase"))

  private val header =
    Vector(
      "participant",
      "image",
      "phase",
      "fixation",
      "x_px",
      "y_px",
      "onset_ms",
      "duration_ms",
      "sample_count",
      "note"
    )
  private def row(p: String, phase: String, n: Int, x: String, y: String, note: String = "") =
    Vector(p, "beach-042", phase, n.toString, x, y, (n * 500).toString, "412", "3", note)

  // P17's retrieval: the first fixation, then one off the screen (a note with
  // a line break), one outside the image, and the focus fixation 1148, 456.
  // P99's positions are recorded shifted, and a rule translates them back.
  // P50 has a bad duration, so its trial is quarantined.
  private val rows = Vector(
    row("P17", "recall", 0, "960", "540"),
    row("P17", "recall", 1, "2000.0", "500", "off screen,\nsee notes"),
    row("P17", "recall", 2, "100", "100"),
    row("P17", "recall", 3, "1148", "456", "focus"),
    row("P17", "encode", 0, "900", "500"),
    row("P99", "recall", 0, "0.30000000000000004", "10.5"),
    row("P50", "recall", 0, "5", "5").updated(7, "-1"),
    row("P99", "encode", 0, "1e2", "2E2", "exponent\r\nspelling")
  )
  private val text   = Rfc4180.encode(header +: rows)
  private val shift  = get(Correction.translate(-0.1, 0.25))
  private val policy = AdmissionPolicy[StudyKey](
    OffScreenPolicy.ExcludeRecord,
    Vector(AppliedCorrection(CorrectionScope.Participant("P99"), shift))
  )
  private val imported = get(
    FixationCsv.admit(text, columns, keys, screen, TimestampUnit.Milliseconds, policy)
  )
  private val ledger = get(
    FixationEvidence.ledger("fixations.csv", imported, AdmissionDecision.ReviewExclusions)
  )
  private val input = StudyInput(imported.accepted)

  private def plan(source: StudyInput[StudyKey, Px]) = get(
    StudyPlan.configure(
      source.reference,
      StudyKey.layout(DefinitionId.studyLayout),
      get(
        StudyGeometry
          .windowed(window, get(Grid.over(window.frame, 64, 48)), OffWindowPolicy.Exclude)
      ),
      "recall",
      "encode",
      Weight.Duration,
      Vector(StudyScale.Native(StudyEstimate.Binned())),
      Some(get(LinearAngularScale.of(screen, 35.0))),
      FailurePolicy.RequireAll,
      StudyMethod.cosine[Px](DefinitionId.cosine),
      (),
      initialFixations = InitialFixationPolicy.dropFirst[Px]
    )
  )
  private val provenance = get(CoordinateProvenance.of(plan(input), input, Some(ledger)))
  private val listing    = provenance.records
  private val source     = get(FixationSourceText.of(text, ledger.source))
  private val size       = get(PageSize.of(3))

  private def all: Vector[SourceRecordText[StudyKey, Px]] =
    def walk(
        page: SourceTextPage[StudyKey, Px],
        seen: Vector[SourceTextPage[StudyKey, Px]]
    ): Vector[SourceTextPage[StudyKey, Px]] =
      page.next.fold(seen :+ page)(n =>
        walk(get(source.page(listing, "x_px", "y_px", n, size)), seen :+ page)
      )
    walk(get(source.first(listing, "x_px", "y_px", size)), Vector.empty).flatMap(_.entries)

  test("pages give every record with its verbatim text and lines; the total comes first") {
    assertEquals(listing.total, 8)
    val first = get(source.first(listing, "x_px", "y_px", size))
    assertEquals((first.entries.size, first.total, first.next), (3, 8, Some(data(4))))
    val entries = all
    assertEquals(entries.map(_.view.record), (1 to 8).map(data).toVector)
    entries.foreach { e =>
      assertEquals(e.text, get(source.layout.verbatim(e.view.record)))
      assertEquals(e.lines, get(source.layout.lines.span(e.view.record)))
    }
    // Record 2's note spans two lines, so record 4 starts on line 6, not 5.
    assertEquals(entries(1).text, Rfc4180.encode(Vector(rows(1))).stripSuffix("\r\n"))
    assertEquals(entries(1).lines.count, 2L)
    assertEquals(entries(3).lines.first.value, 6L)
    assertEquals(source.header, header)
    assertEquals(get(source.fields(data(4))), rows(3))
    assertEquals(source.fields(data(9)), Left(RecordIdentityError.RecordBeyond(data(9), 8)))
  }

  test("fixations.csv record 4: recorded 1148, 456; image 700, 300; +5.4, +2.4 degrees") {
    val focus = get(all(3).view.fixation.toRight("record 4 has no fixation"))
    assertEquals(focus.record, Right(data(4)))
    assertEquals(focus.source, Some(ledger.source))
    assertEquals(focus.position.number.value, 4)
    val trail = focus.trail
    assertEquals(
      trail.recorded,
      Some(
        RecordedPosition[Px](
          "x_px",
          "y_px",
          "1148",
          "456",
          FramedPosition(screen.id, Pt(1148, 456))
        )
      )
    )
    assertEquals(trail.admitted, FramedPosition(screen.id, Pt[Px](1148, 456)))
    assertEquals(trail.window, Some(FramedPosition(FrameId("image"), Pt[Px](700, 300))))
    assertEquals(trail.placement, MapPlacement.InMap)
    val degrees = get(trail.angular.toRight("no degrees")).position
    assertEquals(f"${degrees.x}%+.1f, ${degrees.y}%+.1f", "+5.4, +2.4")
  }

  test("each record's placement, and the recorded text of every admitted record") {
    val entries = all
    assertEquals(
      entries.map(_.view.fixation.map(_.trail.placement)),
      Vector(
        Some(MapPlacement.DroppedInitial),
        Some(MapPlacement.OutsideScreen),
        Some(MapPlacement.OutsideWindow(OffWindowPolicy.Exclude)),
        Some(MapPlacement.InMap),
        Some(MapPlacement.DroppedInitial),
        Some(MapPlacement.DroppedInitial),
        None, // the quarantined trial's record
        Some(MapPlacement.DroppedInitial)
      )
    )
    entries.zip(rows).foreach { (e, fields) =>
      e.view.fixation.foreach(f =>
        assertEquals(
          f.trail.recorded.map(r => (r.xText, r.yText)),
          Some(fields(4) -> fields(5))
        )
      )
    }
    assertEquals(
      entries(1).view.fixation.flatMap(_.trail.recorded).map(_.parsed.position),
      Some(Pt[Px](2000, 500))
    )
  }

  test("a corrected record's parse corrects to its admitted position, bit for bit") {
    val shifted = get(all(5).view.fixation.toRight("record 6 has no fixation")).trail
    assertEquals(shifted.correction, Some(CorrectionApplied(0, shift)))
    val parsed = get(shifted.recorded.toRight("not recorded")).parsed.position
    assertEquals(parsed, Pt[Px](0.30000000000000004, 10.5))
    assertEquals(shift.correct(screen, parsed), Some(shifted.admitted.position))
    assertEquals(shifted.admitted.position, Pt[Px](0.30000000000000004 - 0.1, 10.75))
    val spelled = get(all(7).view.fixation.toRight("record 8 has no fixation")).trail
    assertEquals(
      spelled.recorded.map(r => (r.xText, r.parsed.position)),
      Some("1e2" -> Pt[Px](100, 200))
    )
  }

  test("a record whose recorded position disagrees is refused alone, naming it") {
    // The same records admitted without the correction: the ledger says the
    // rule applies, but the input holds the uncorrected positions.
    val plain = get(
      FixationCsv.admit(
        text,
        columns,
        keys,
        screen,
        TimestampUnit.Milliseconds,
        AdmissionPolicy.default
      )
    )
    val uncorrected = StudyInput(plain.accepted)
    val mismatched  =
      get(CoordinateProvenance.of(plan(uncorrected), uncorrected, Some(ledger))).records
    val page    = get(source.page(mismatched, "x_px", "y_px", data(5), size))
    val refusal = SourceTextError.RecordedMismatch[StudyKey](
      data(6),
      0.30000000000000004,
      10.5,
      0.30000000000000004 - 0.1,
      10.75,
      0.30000000000000004,
      10.5
    )
    assertEquals(page.entries.map(_.refusal), Vector(None, Some(refusal), None))
    // The refused record keeps its text and lines, but no recorded position.
    val refused = page.entries(1)
    assertEquals(refused.text, get(source.layout.verbatim(data(6))))
    assertEquals(refused.view.fixation.flatMap(_.trail.recorded), None)
    assert(refusal.message.contains("Data record 6 records"), refusal.message)
    // A record the text agrees with is checked as usual.
    assert(page.entries(0).view.fixation.exists(_.trail.recorded.isDefined))
  }

  test("a key reader built from the layout resolves participants through the layout") {
    val layout = StudyKey.layout(DefinitionId.studyLayout)
    val reader = get(
      LayoutKeys.reader(layout, Vector("participant", "image", "phase"))(
        fields => Right(StudyKey(fields("participant"), fields("image"), fields("phase"))),
        key =>
          eyes4s.kernel.ClockId(
            s"fixation-trial:${key.participant}/${key.stimulus}/${key.phase}"
          )
      )
    )
    val viaLayout = get(
      FixationCsv.admit(text, columns, reader, screen, TimestampUnit.Milliseconds, policy)
    )
    assertEquals(viaLayout.accepted.rows.map(_.key), imported.accepted.rows.map(_.key))
    assertEquals(
      viaLayout.accepted.rows.map(_.value.fixations.map(_.centre).toVector),
      imported.accepted.rows.map(_.value.fixations.map(_.centre).toVector)
    )
    assertEquals(
      LayoutKeys
        .reader(layout, Vector.empty)(_ => Left("unused"), _ => eyes4s.kernel.ClockId("c"))
        .map(_ => ()),
      Left(FixationImportError.Columns(Vector.empty))
    )
  }

  test("refusals: another text, no ledger, a missing column, a page beyond the records") {
    val other =
      Rfc4180.encode(header +: rows.updated(3, row("P17", "recall", 3, "1149", "456")))
    val wrong = FixationSourceText.of(other, ledger.source)
    assert(
      wrong match
        case Left(SourceTextError.SourceDigest("fixations.csv", _, _)) => true
        case _                                                         => false
      ,
      wrong.toString
    )
    assert(wrong.left.exists(_.message.contains("the ledger's source 'fixations.csv'")))
    val broken = FixationSourceText.of("a,\"b", ledger.source)
    assertEquals(
      broken.map(_ => ()),
      Left(SourceTextError.Layout(CsvLayoutError.Csv(TidyCsvError.UnterminatedQuotedField(4))))
    )
    val unledgered = get(CoordinateProvenance.of(plan(input), input, None)).records
    val none       = source.first(unledgered, "x_px", "y_px", size)
    assertEquals(
      none.map(_ => ()),
      Left(SourceTextError.SourceMismatch(ledger.source.records.digest, None))
    )
    assert(none.left.exists(_.message.contains("no source")))
    val column = source.first(listing, "x", "y_px", size)
    assertEquals(column.map(_ => ()), Left(SourceTextError.Column("x", header)))
    assert(column.left.exists(_.message.contains("Column 'x'")))
    val beyond = source.page(listing, "x_px", "y_px", data(9), size)
    assertEquals(
      beyond.map(_ => ()),
      Left(SourceTextError.Provenance(ProvenanceError.PageStart(data(9), 8)))
    )
    assert(beyond.left.exists(_.message.contains("data record 9")))
    val field = SourceTextError.Field(data(2), "x_px", "left")
    assert(field.message.contains("'left' in column 'x_px'"))
  }
