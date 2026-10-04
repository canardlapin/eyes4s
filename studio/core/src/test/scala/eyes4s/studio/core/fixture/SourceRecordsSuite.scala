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

package eyes4s.studio.core.fixture

import cats.effect.IO
import eyes4s.plan.{MapPlacement, OffWindowPolicy}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.SourceRole
import eyes4s.studio.core.selection.{FixationIndex, RecordNumber, StudioRef}
import io.circe.syntax.*
import munit.CatsEffectSuite

/** The source records view of protocol 1.7 (S6.4) on the fake backend at t2:
  * fixations.csv's 11,520 records under rev 4. Record 7,214 is P17 enc_03's
  * fixation 6 (FIXTURE.md: screen (1148, 456), image (700, 300), +5.4°,
  * +2.4° from the image centre at the declared 35 px/°). Every record's line
  * and cells are checked against fixations.csv itself in the JVM suite
  * (`SourceRecordsJvmSuite`).
  */
class SourceRecordsSuite extends CatsEffectSuite:

  private val rev4  = AnalysisRevision(4)
  private val r3    = DatasetRevision(3)
  private val enc03 = MockStudy.key("P17", "enc_03")

  /** Degrees are a linear rescale of the pixel position: rounding only. */
  private val DegreeTolerance: Double = 1e-9

  private def fake = FakeStudyBackend.create[IO](StoryMoment.T2)

  private def ok[A](e: Either[BackendError, A]): A = e.fold(x => fail(x.message), identity)

  private def refusal(e: SourceRecordsError) = Left(BackendError.SourceRecordsRefused(rev4, e))

  test("record 7,214 is P17 enc_03's fixation 6, in screen, image and degrees") {
    fake.flatMap(_.sourceRecords(rev4, 7214, 1)).map { result =>
      val page = ok(result)
      assertEquals(
        (page.revision, page.dataset, page.total, page.from),
        (rev4, r3, 11520, 7214)
      )
      assertEquals(page.source.role, SourceRole.Fixations)
      assertEquals(page.source.path.value, "inputs/fixations.csv")
      assertEquals(page.source.bytes.hex, GoldenInventory.fixationsSha256)
      // Rev 4's recipe declares the dataset's 35 px/° as its own angular scale.
      assertEquals((page.pixelsPerDegree, page.scaleSource), (35.0, ScaleSource.Recipe))
      val row = page.rows.head
      assertEquals(
        row.ref,
        StudioRef.SourceRecord(
          enc03,
          FixationIndex.of(6).toOption,
          SourceRole.Fixations,
          RecordNumber.of(7214).toOption.get
        )
      )
      assertEquals(
        row.fixation: Option[StudioRef],
        Some(StudioRef.Fixation(enc03, FixationIndex.of(6).toOption.get))
      )
      assertEquals(
        (row.ordinal, row.onsetMs, row.durationMs, row.samples),
        (Some(6), Some(2160.0), Some(412.0), Some(206))
      )
      assertEquals(row.screen.map(p => (p.x, p.y)), Some((1148.0, 456.0)))
      assertEquals(
        row.image.map(i => (i.at.x, i.at.y, i.insideImage)),
        Some((700.0, 300.0, true))
      )
      val deg = row.degrees.get
      assertEqualsDouble(deg.x, 188.0 / 35.0, DegreeTolerance)
      assertEqualsDouble(deg.y, 84.0 / 35.0, DegreeTolerance)
      assertEquals(row.placement, Some(MapPlacement.InMap))
      assertEquals(row.line, "P17,Encoding,enc_03,1,6,1148.0,456.0,2160,412,206")
    }
  }

  test("a page runs in file order and the last page stops at the last record") {
    for
      b     <- fake
      first <- b.sourceRecords(rev4, 1, 60).map(ok)
      last  <- b.sourceRecords(rev4, 11500, 500).map(ok)
    yield
      assertEquals(first.rows.map(_.record), (1 to 60).toVector)
      assertEquals(last.rows.map(_.record), (11500 to 11520).toVector)
  }

  test("fixation 10 lies left of the image: outside it, and outside rev 4's window") {
    fake.flatMap(_.sourceRecords(rev4, 7218, 1)).map { result =>
      val row = ok(result).rows.head
      assertEquals(row.fixation.map(_.index.value), Some(10))
      assertEquals(row.image.map(i => (i.at.x, i.insideImage)), Some((327.1 - 448.0, false)))
      assert(row.degrees.exists(_.x < 0.0), row.degrees)
      assertEquals(row.placement, Some(MapPlacement.OutsideWindow(OffWindowPolicy.Exclude)))
    }
  }

  test("a record of a quarantined trial names no fixation and has no placement") {
    val ret05 = MockStudy.key("P02", "ret_05")
    for
      b     <- fake
      row   <- b.sourceRecords(rev4, 679, 1).map(ok(_).rows.head)
      fixes <- b.trialFixations(rev4, ret05)
    yield
      assertEquals((row.trial, row.samples), (ret05, Some(0)))
      assertEquals((row.ref.fixation, row.placement), (None, None))
      // Its cells and positions are still the file's.
      assertEquals(row.screen.map(p => (p.x, p.y)), Some((522.2, 555.6)))
      assert(row.image.isDefined && row.degrees.isDefined)
      assert(fixes.isLeft, fixes)
  }

  test("a range outside the file, or a revision it does not serve, is refused") {
    for
      b    <- fake
      zero <- b.sourceRecords(rev4, 0, 10)
      none <- b.sourceRecords(rev4, 1, 0)
      big  <- b.sourceRecords(rev4, 1, 501)
      past <- b.sourceRecords(rev4, 11521, 1)
      rev5 <- b.sourceRecords(AnalysisRevision(5), 1, 1)
      rev9 <- b.sourceRecords(AnalysisRevision(9), 1, 1)
    yield
      assertEquals(zero, refusal(SourceRecordsError.RangeInvalid(0, 10, 500)))
      assertEquals(none, refusal(SourceRecordsError.RangeInvalid(1, 0, 500)))
      assertEquals(big, refusal(SourceRecordsError.RangeInvalid(1, 501, 500)))
      assertEquals(past, refusal(SourceRecordsError.PastEnd(11521, 11520)))
      // Rev 5 is still a draft at t2.
      assert(rev5.left.exists(_.isInstanceOf[BackendError.Unavailable]), rev5)
      assert(rev9.left.exists(_.isInstanceOf[BackendError.UnknownRevision]), rev9)
  }

  test("a page is validated when built and when decoded") {
    fake.flatMap(_.sourceRecords(rev4, 7213, 3)).map { result =>
      val page                                                                      = ok(result)
      val rows                                                                      = page.rows
      def build(from: Int, total: Int, rs: Vector[SourceRecordRow], count: Int = 3) =
        SourceRecordPage.of(
          page.revision,
          page.dataset,
          page.source,
          page.pixelsPerDegree,
          page.scaleSource,
          total,
          from,
          count,
          rs
        )
      assertEquals(build(7213, 11520, rows), Right(page))
      assertEquals(
        build(7213, 11520, rows.reverse),
        Left(SourceRecordsError.OutOfOrder(0, 7215, 7213))
      )
      // A page holds every record asked for that the file has: none short but the last.
      assertEquals(build(7213, 7214, rows), Left(SourceRecordsError.RowCount(7213, 3, 7214, 3)))
      assertEquals(build(7213, 7215, rows).map(_.total), Right(7215))
      assertEquals(
        build(7213, 11520, rows.take(2)),
        Left(SourceRecordsError.RowCount(7213, 3, 11520, 2))
      )
      assertEquals(
        build(7213, 11520, Vector.empty),
        Left(SourceRecordsError.RowCount(7213, 3, 11520, 0))
      )
      assertEquals(
        build(7300, 7214, Vector.empty),
        Left(SourceRecordsError.PastEnd(7300, 7214))
      )
      // An empty file has no page.
      assertEquals(build(1, 0, Vector.empty), Left(SourceRecordsError.PastEnd(1, 0)))
      assertEquals(
        build(7213, 11520, Vector.fill(501)(rows.head), 501),
        Left(SourceRecordsError.RangeInvalid(7213, 501, 500))
      )
      assertEquals(
        SourceRecordPage.of(
          page.revision,
          page.dataset,
          page.source,
          -2.5,
          page.scaleSource,
          11520,
          7213,
          3,
          rows
        ),
        Left(SourceRecordsError.ScaleNotPositive("-2.5"))
      )
      val row = rows.head
      def remake(
          ref: StudioRef.SourceRecord,
          placement: Option[MapPlacement],
          onset: Option[Double]
      ) =
        SourceRecordRow.of(
          ref,
          row.ordinal,
          onset,
          row.durationMs,
          row.samples,
          row.screen,
          row.image,
          row.degrees,
          placement,
          row.line
        )
      assertEquals(
        remake(row.ref, None, row.onsetMs),
        Left(SourceRecordsError.PlacementMismatch(7213))
      )
      assertEquals(
        remake(row.ref.copy(source = SourceRole.Trials), row.placement, row.onsetMs),
        Left(SourceRecordsError.OtherSource(7213, SourceRole.Trials))
      )
      assertEquals(
        remake(row.ref, row.placement, Some(Double.PositiveInfinity)),
        Left(SourceRecordsError.NotFinite(7213, "onset", "Infinity"))
      )
      // Screen, image and degrees: all or none.
      assertEquals(
        SourceRecordRow.of(
          row.ref,
          row.ordinal,
          row.onsetMs,
          row.durationMs,
          row.samples,
          None,
          row.image,
          row.degrees,
          row.placement,
          row.line
        ),
        Left(SourceRecordsError.PositionsPartial(7213, false, true, true))
      )
      assertEquals(
        PlanePoint.of(7213, "screen", 1.0, Double.NaN),
        Left(SourceRecordsError.NotFinite(7213, "screen y", "NaN"))
      )
      // The wire form decodes to the same page, and a tampered one is refused.
      val json = page.asJson
      assertEquals(json.as[SourceRecordPage], Right(page))
      val tampered = json.hcursor.downField("from").withFocus(_ => 7214.asJson).top.get
      assert(tampered.as[SourceRecordPage].isLeft)
    }
  }

  test("a recipe's angular scale is the page's: degrees at the plan's declared units") {
    val unavailable = BackendError.Unavailable(DiagnosticLocus.Revision(rev4))
    val served      = for
      (recipe, geometry) <- FakeTrialViews.study(StoryMoment.T2, rev4)
      doc                <- StorySeed.document(StoryMoment.T2).left.map(_ => unavailable)
      source             <- doc.dataset(r3).flatMap(_.sources.fixations).toRight(unavailable)
      scale              <- eyes4s.studio.core.document.DeclaredPixelsPerDegree
        .of(40.0)
        .left
        .map(e => BackendError.Malformed("scale", e.message))
      page <- FakeSourceRecords.serve(
        rev4,
        r3,
        recipe.copy(angularScale = Some(scale)),
        geometry,
        source,
        7214,
        1
      )
    yield page
    val page = ok(served)
    assertEquals((page.pixelsPerDegree, page.scaleSource), (40.0, ScaleSource.Recipe))
    val deg = page.rows.head.degrees.get
    // Record 7,214 is 188 px right of and 84 px above the image centre.
    assertEqualsDouble(deg.x, 188.0 / 40.0, DegreeTolerance)
    assertEqualsDouble(deg.y, 84.0 / 40.0, DegreeTolerance)
    // The image position is the dataset's, whatever the scale.
    assertEquals(page.rows.head.image.map(i => (i.at.x, i.at.y)), Some((700.0, 300.0)))
  }

  test(
    "the image frame is half-open: its left and top edges inside, right and bottom outside"
  ) {
    val frames = ok(
      FakeTrialViews
        .study(StoryMoment.T2, rev4)
        .flatMap((recipe, geometry) =>
          FakeSourceRecords
            .frames(r3, recipe, geometry)
            .left
            .map(e => BackendError.Malformed("frames", e))
        )
    )._1
    def inside(x: Double, y: Double) =
      FakeSourceRecords
        .position(frames, 1, x, y)
        .fold(e => fail(e.message), identity)
        .get
        ._1
        .insideImage
    // The image is [448, 1472) × [156, 924) on the screen.
    assert(inside(448.0, 156.0))
    assert(inside(1471.9, 923.9))
    assert(!inside(1472.0, 500.0))
    assert(!inside(800.0, 924.0))
    assert(!inside(447.9, 500.0))
    assert(!inside(800.0, 155.9))
  }
