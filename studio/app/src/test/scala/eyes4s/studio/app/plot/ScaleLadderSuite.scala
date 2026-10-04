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

package eyes4s.studio.app.plot

import cats.instances.future.*
import cats.Id
import eyes4s.studio.app.text.{LadderText, LadderTextId}
import eyes4s.studio.core.backend.{
  BackendError,
  Inspection,
  PageRequest,
  PairDesign,
  QueryStatus,
  ResultAddress,
  RunId,
  TrialKey
}
import eyes4s.studio.core.fixture.{MockStudy, StoryMoment}
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.document.ReportingId
import eyes4s.studio.core.navigation.{Page, ReportRef, StudyNavigator, UsedByRole}
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}
import io.circe.Json

import scala.concurrent.{ExecutionContext, Future}

/** The scale ladder's values (ticket S4.5b): read from the fake backend
  * through its inspection and the navigator's pairs, and written as the
  * plot's and table's one value source. The focus query's values are
  * checked against fixture.json itself, read here independently of the
  * backend's decoding.
  */
class ScaleLadderSuite extends munit.FunSuite:

  private given ExecutionContext = ExecutionContext.global

  private val run   = RunId(7)
  private val focus = MockStudy.key("P17", "ret_07")

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private def withSession[A](body: HeadlessSession => Future[A]): Future[A] =
    HeadlessSession
      .open(StoryMoment.T2)
      .flatMap(s => body(s).transformWith(r => s.close.transform(_ => r)))

  private def load(session: HeadlessSession, query: TrialKey, at: RunId = run) =
    session.result(run).flatMap { summary =>
      ScaleLadder.load[Future](session.inspect, session.navigator)(
        at,
        query,
        right(summary).scales
      )
    }

  // fixture.json's record of the focus query.
  private val fixtureFocus: Json =
    val json = right(io.circe.parser.parse(MockStudy.fixtureText))
    right(
      json.hcursor
        .downField("participants")
        .as[Vector[Json]]
        .map(_.filter(_.hcursor.get[String]("id").contains("P17")).flatMap { p =>
          p.hcursor.get[Vector[Json]]("queries").getOrElse(Vector.empty)
        })
        .map(_.filter(_.hcursor.get[String]("trial").contains("ret_07")))
        .flatMap(_.headOption.toRight(io.circe.DecodingFailure("no focus query", Nil)))
    )

  private def numbers(field: String): Vector[Double] =
    right(fixtureFocus.hcursor.get[Vector[Double]](field))

  private val scale   = right(ScaleIndex.of(0))
  private val matched =
    StudioRef.Pair(run, scale, PairDesign.Matched, focus, MockStudy.key("P17", "enc_03"))
  private val controls = Vector.tabulate(300)(i =>
    StudioRef.Pair(
      run,
      scale,
      PairDesign.Control,
      focus,
      MockStudy.key("P17", s"enc_control_$i")
    )
  )

  private def inspectPure(at: RunId, address: ResultAddress): Either[BackendError, Inspection] =
    assertEquals(at, run)
    Right(address match
      case ResultAddress.ContrastRow(_, _)  => Inspection.Contrast(address, 0.7, 0.2, 0.5)
      case ResultAddress.Reduction(_, _, _) => Inspection.Reduction(address, 0.2, controls.size)
      case ResultAddress.PairRow(_, _, _, _) => Inspection.Pair(address, "item", 0.7)
      case _                                 => fail(s"unexpected address: $address"))

  private def navigator(
      matchRef: StudioRef = matched,
      controlRefs: Vector[StudioRef] = controls,
      pageChange: Page[StudioRef] => Page[StudioRef] = identity
  ): StudyNavigator[Id] = new StudyNavigator[Id]:
    def pairs(contrast: StudioRef, design: PairDesign, request: PageRequest) =
      assertEquals(contrast, StudioRef.QueryContrast(run, scale, focus))
      val all = if design == PairDesign.Matched then Vector(matchRef) else controlRefs
      Right(pageChange(Page.of(all, request)))
    def cells(run: RunId, reporting: ReportingId, scale: ScaleIndex, page: PageRequest) = fail(
      "unused"
    )
    def participants(cell: ReportRef.Cell, page: PageRequest)          = fail("unused")
    def queries(participant: ReportRef.Participant, page: PageRequest) = fail("unused")
    def maps(pair: StudioRef)                                          = fail("unused")
    def fixations(map: StudioRef, page: PageRequest)                   = fail("unused")
    def record(fixation: StudioRef)                                    = fail("unused")
    def usedByCounts(map: StudioRef)                                   = fail("unused")
    def usedBy(map: StudioRef, role: UsedByRole, page: PageRequest)    = fail("unused")

  test("the loader preserves all controls across page boundaries") {
    val result = right(ScaleLadder.load[Id](inspectPure, navigator())(run, focus, Vector("2°")))
    assertEquals(result.scales.head.controls.map(_.ref), controls)
  }

  test("pair refs must belong to the requested run, scale, query and design") {
    val reference = MockStudy.key("P17", "enc_03")
    val wrong     = Vector(
      StudioRef.Pair(RunId(99), scale, PairDesign.Matched, focus, reference),
      StudioRef.Pair(run, right(ScaleIndex.of(1)), PairDesign.Matched, focus, reference),
      StudioRef.Pair(run, scale, PairDesign.Matched, MockStudy.key("P17", "ret_08"), reference),
      StudioRef.Pair(run, scale, PairDesign.Control, focus, reference)
    )
    wrong.foreach { ref =>
      val result =
        ScaleLadder.load[Id](inspectPure, navigator(matchRef = ref))(run, focus, Vector("2°"))
      assert(result.isLeft, s"accepted unrelated matched pair: $ref")
    }
    assert(
      ScaleLadder
        .load[Id](inspectPure, navigator(controlRefs = Vector(matched)))(
          run,
          focus,
          Vector("2°")
        )
        .isLeft
    )
  }

  test("inspection addresses must match the requested value's address") {
    val other = ResultAddress.ContrastRow(1, focus)
    Vector("contrast", "matched", "reduction", "control").foreach { target =>
      def changed(at: RunId, address: ResultAddress) = inspectPure(at, address).map {
        case Inspection.Contrast(_, m, b, d) if target == "contrast" =>
          Inspection.Contrast(other, m, b, d)
        case Inspection.Reduction(_, v, n) if target == "reduction" =>
          Inspection.Reduction(other, v, n)
        case Inspection.Pair(ResultAddress.PairRow(_, design, _, _), item, score)
            if (design == PairDesign.Matched && target == "matched") ||
              (design == PairDesign.Control && target == "control") =>
          Inspection.Pair(other, item, score)
        case inspection => inspection
      }
      val result = ScaleLadder.load[Id](changed, navigator())(run, focus, Vector("2°"))
      assert(result.isLeft, s"accepted wrong $target address")
    }
  }

  test("a malformed continuation page fails instead of silently truncating controls") {
    val broken: Page[StudioRef] => Page[StudioRef] =
      p => if p.total > 1 then p.copy(next = Some(p.offset)) else p
    val result = ScaleLadder
      .load[Id](inspectPure, navigator(pageChange = broken))(run, focus, Vector("2°"))
    assert(result.isLeft, "a nonadvancing page must not become a partial successful ladder")
  }

  test("the focus query's ladder is fixture.json's M, B, D and 2° control scores") {
    withSession(s => load(s, focus)).map { loaded =>
      val ladder = right(loaded)
      assertEquals(ladder.scales.map(_.label), Vector("0.5°", "1°", "2°", "4°"))
      assertEquals(ladder.scales.map(_.m), numbers("M"))
      assertEquals(ladder.scales.map(_.b), numbers("B"))
      assertEquals(ladder.scales.map(_.d), numbers("D"))
      assertEquals(ladder.scales.map(_.matchedTrial.trial).distinct, Vector("enc_03"))
      assertEquals(ladder.scales.map(_.matchedItem).distinct, Vector("beach-042"))
      assertEquals(ladder.scales.map(_.members).distinct, Vector(19))
      val scores = right(fixtureFocus.hcursor.get[Vector[Json]]("control_scores_2deg")).map {
        c =>
          val h = c.hcursor
          (
            right(h.get[String]("trial")),
            right(h.get[String]("item")),
            right(h.get[Double]("cos"))
          )
      }
      val at2 = ladder.scales(2)
      assertEquals(
        at2.controls
          .flatMap(c => c.item.zip(c.cosine).map((i, v) => (c.reference.trial, i, v)))
          .sortBy(_._1),
        scores.sortBy(_._1)
      )
      // The fixture scores controls at 2° only: elsewhere each is listed without a cosine.
      ladder.scales.filterNot(_.label == "2°").foreach { s =>
        assertEquals(s.controls.size, 19)
        assert(s.controls.forall(c => c.cosine.isEmpty && c.item.isEmpty), s.label)
      }
      // Every value keeps the ref it was served under.
      assertEquals(at2.contrast, StudioRef.QueryContrast(run, at2.scale, focus))
      assertEquals(
        at2.mean.resultAddress,
        Some(ResultAddress.Reduction(2, PairDesign.Control, focus))
      )
      assert(
        at2.controls.forall(c =>
          c.ref == StudioRef.Pair(run, at2.scale, PairDesign.Control, focus, c.reference)
        )
      )
    }
  }

  test("the focus row of the source is written as the board writes it") {
    withSession(s => load(s, focus)).map { loaded =>
      val columns               = right(LadderColumns.standard)
      val ladder                = right(loaded)
      val source                = right(ScaleLadder.source(ladder, columns))
      val at2                   = ladder.scales(2)
      def cells(ref: StudioRef) = right(source.rowOf(ref).flatMap(source.cells).toRight(ref))
      assertEquals(source.caption, LadderText(LadderTextId.Caption, "P17 · ret_07"))
      assertEquals(
        cells(at2.matched),
        Vector("2°", "M matched", "enc_03", "beach-042", "0.73", "—")
      )
      assertEquals(
        cells(at2.mean),
        Vector("2°", "B control mean", "mean of 19 controls", "—", "0.35", "—")
      )
      assertEquals(cells(at2.contrast), Vector("2°", "D", "M − B", "—", "—", "+0.38"))
      assertEquals(
        cells(at2.controls.head.ref),
        Vector("2°", "Control", "enc_01", "street-112", "0.61", "—")
      )
      // A control the backend does not score at 0.5° is missing, not zero.
      assertEquals(cells(ladder.scales(0).controls.head.ref).drop(3), Vector("—", "—", "—"))
      // Rows go M, B, D, then the controls, scale by scale.
      assertEquals(source.rows.size, 4 * (3 + 19))
      assertEquals(
        source.rows.take(3).map(_.ref),
        Vector(ladder.scales(0).matched, ladder.scales(0).mean, ladder.scales(0).contrast)
      )
    }
  }

  test("an unscored query or an unknown run fails the ladder, naming what failed") {
    val absent = MockStudy.key("P17", "ret_09")
    withSession { s =>
      for
        unscored <- load(s, absent)
        unknown  <- load(s, focus, RunId(99))
      yield (unscored, unknown)
    }.map { (unscored, unknown) =>
      unscored match
        case Left(LadderError.Unscored(q, "0.5°", QueryStatus.NotAdmitted(_))) =>
          assertEquals(q, absent)
        case other => fail(s"expected the absent query unscored, got $other")
      unknown match
        case Left(
              e @ LadderError.Backend(
                q,
                ResultAddress.ContrastRow(0, _),
                BackendError.UnknownRun(r, _)
              )
            ) =>
          assertEquals((q, r), (focus, RunId(99)))
          assert(e.message.contains("P17 · ret_07"), e.message)
        case other => fail(s"expected an unknown run, got $other")
    }
  }
