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

package eyes4s.studio.app.compare

import cats.instances.future.*
import eyes4s.studio.app.StoryModels
import eyes4s.studio.app.plot.{PlotSource, ProfileColumns, ScaleLadder, ScaleProfile}
import eyes4s.studio.core.backend.{PageRequest, PairDesign, PairRowEntry, QueryRow, QueryStatus}
import eyes4s.studio.core.figures.{MethodsReadError, MethodsReads}
import eyes4s.studio.core.fixture.{MockStudy, StoryMoment, StoryMoments}
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}

import scala.concurrent.{ExecutionContext, Future}

/** Compare's pairs table and per-query scale profile headlessly (ticket
  * S8.5; Main.dc.html, the contrast group's tabs), on the fake backend at
  * t2: every pair row of run 7 at the focused scale, read through protocol
  * 1.9's pages; its values equal the scale ladder's (AC); failed and
  * not-served pairs say so and show no number; the query's profile is its
  * served D at every scale.
  */
class PairsTableSuite extends munit.FunSuite:
  import StoryModels.*

  private given ExecutionContext = ExecutionContext.global

  private val run7   = StoryMoments.run7
  private val scales = Vector("0.5°", "1°", "2°", "4°")

  private def right[E, A](e: Either[E, A]): A =
    e.fold(x => fail(s"unexpected Left: $x"), identity)

  private def withSession[A](f: HeadlessSession => Future[A]): Future[A] =
    HeadlessSession
      .open(StoryMoment.T2)
      .flatMap(s => f(s).transformWith(x => s.close.transform(_ => x)))

  private def rows(s: HeadlessSession): Future[Vector[QueryRow]] =
    s.queries(run7, right(PageRequest.first(PageRequest.MaximumSize))).map(q => right(q).rows)

  /** Every pair row of run 7 at `scale`, as the host reads it. */
  private def pairs(s: HeadlessSession, scale: ScaleIndex): Future[Vector[PairRowEntry]] =
    MethodsReads
      .pairRowsAt[Future](s.pairRows, run7, scale.value)
      .map(r => right(r).flatMap(_.rows))

  private def focusAt(scale: ScaleIndex, query: eyes4s.studio.core.backend.TrialKey) =
    PanelFocus(run7, scale, query, None)

  private def tableAt(s: HeadlessSession, scale: ScaleIndex) =
    for
      qs <- rows(s)
      ps <- pairs(s, scale)
    yield
      val focus    = focusAt(scale, p17ret07)
      val (t, eff) = PairsTable.sync(PairsTable.empty, Some(focus))
      assertEquals(eff, Vector(PairsEffect.ReadPairs(run7, scale)))
      val read = PairsTable.read(t, run7, scale, PairsAnswer.Answered(ps))
      (PairsTable.vm(read, Some(focus), TrialPanels.referenceOf(focus, qs), qs, scales), qs, ps)

  private def cosine(src: PlotSource, ref: StudioRef): Option[Double] =
    src.rowOf(ref).flatMap(src.number(_, right(PairsTable.columns).cosine))

  private def scoreText(src: PlotSource, ref: StudioRef): Option[String] =
    src.rowOf(ref).flatMap(r => src.value(r, right(PairsTable.columns).score)).collect {
      case eyes4s.studio.app.plot.PlotValue.Text(t) => t
    }

  // --- reading -------------------------------------------------------------------------------

  test("the table follows the focus: a new run or scale is read once; stale answers drop") {
    val (t, e) = PairsTable.sync(PairsTable.empty, Some(focusAt(sigma2, p17ret07)))
    assertEquals(e, Vector(PairsEffect.ReadPairs(run7, sigma2)))
    // Another query at the same scale reads nothing new.
    assertEquals(
      PairsTable.sync(t, Some(focusAt(sigma2, MockStudy.key("P01", "ret_01"))))._2,
      Vector.empty
    )
    val sigma1   = right(ScaleIndex.of(1))
    val (t1, e1) = PairsTable.sync(t, Some(focusAt(sigma1, p17ret07)))
    assertEquals(e1, Vector(PairsEffect.ReadPairs(run7, sigma1)))
    // An answer for σ 2° arrives after the move to σ 1°: dropped.
    assertEquals(PairsTable.read(t1, run7, sigma2, PairsAnswer.Answered(Vector.empty)), t1)
    val failed = PairsTable.read(
      t1,
      run7,
      sigma1,
      PairsAnswer.Failed(MethodsReadError.PairsShort(run7, 1, 10, 20))
    )
    val vm = PairsTable.vm(failed, Some(focusAt(sigma1, p17ret07)), None, Vector.empty, scales)
    assertEquals(
      vm.status,
      Some(
        "The pair rows could not be read: The pair rows of run 7 at scale 1 hold 10 of the 20 announced."
      )
    )
    assertEquals(vm.retry, Some("Retry"))
    assertEquals(PairsTable.retry(failed), (t1, Vector(PairsEffect.ReadPairs(run7, sigma1))))
  }

  test("every pair row of run 7 at σ 2°, one row per pair, the focused pair the cursor") {
    withSession(s =>
      tableAt(s, sigma2).map { (vm, _, ps) =>
        val src = vm.source.getOrElse(fail(vm.status.toString))
        // FIXTURE.md: 8,969 pair rows per scale.
        assertEquals(src.rows.size, 8969)
        assertEquals(src.rows.size, ps.size)
        assertEquals(vm.focused, Some(pair))
        assertEquals(src.caption, "Pairs of run 7 at σ 2°: 8969 rows")
        assertEquals(
          src.cells(src.rowOf(pair).get),
          Some(
            Vector(
              "P17 · ret_07",
              "beach-042",
              "matched",
              "P17 · enc_03",
              "beach-042",
              "2°",
              "0.73",
              "scored"
            )
          )
        )
      }
    )
  }

  test("the table's values equal the ladder's: M and every control cosine (AC)") {
    withSession(s =>
      for
        (vm, _, _) <- tableAt(s, sigma2)
        ladder     <- ScaleLadder.load[Future](s.inspect, s.navigator)(run7, p17ret07, scales)
      yield
        val src = vm.source.getOrElse(fail("no table"))
        val at  = right(ladder).scales.find(_.scale == sigma2).getOrElse(fail("no 2° rung"))
        assertEquals(cosine(src, at.matched), Some(at.m))
        assertEquals(at.controls.size, 19)
        at.controls.foreach(c =>
          assertEquals(cosine(src, c.ref), c.cosine, c.reference.toString)
        )
    )
  }

  test("failed pairs say why and show no number; pairs the backend does not serve say so") {
    withSession(s =>
      for
        (two, qs, _) <- tableAt(s, sigma2)
        (one, _, _)  <- tableAt(s, right(ScaleIndex.of(1)))
      yield
        val src    = two.source.getOrElse(fail("no table"))
        val failed = qs.collect { case QueryRow(q, _, _, m, _, QueryStatus.Failed(d)) =>
          (q, m, d)
        }
        assertEquals(failed.size, 3)
        failed.foreach { (q, m, d) =>
          val ref = StudioRef.Pair(run7, sigma2, PairDesign.Matched, q, m.get)
          assertEquals(cosine(src, ref), None)
          assertEquals(scoreText(src, ref), Some(s"failed: ${d.message}"))
          assertEquals(src.rowOf(ref).flatMap(src.cells(_)).map(_(6)), Some("—"))
        }
        // Controls are served at 2° only: at 1° a control is "not served", never 0.
        val at1     = one.source.getOrElse(fail("no table"))
        val sigma1  = right(ScaleIndex.of(1))
        val control = StudioRef.Pair(
          run7,
          sigma1,
          PairDesign.Control,
          p17ret07,
          MockStudy.key("P17", "enc_08")
        )
        assertEquals(cosine(at1, control), None)
        assertEquals(scoreText(at1, control), Some("not served"))
        // The matched pair at 1° is served: M 0.58 (FIXTURE.md).
        val matched = StudioRef.Pair(run7, sigma1, PairDesign.Matched, p17ret07, p17enc03)
        assertEquals(cosine(at1, matched), Some(0.58))
    )
  }

  test("rows read for σ 2° are not shown under a σ 1° focus: Reading, never mislabelled") {
    withSession(s =>
      for
        qs <- rows(s)
        ps <- pairs(s, sigma2)
      yield
        val (t, _) = PairsTable.sync(PairsTable.empty, Some(focusAt(sigma2, p17ret07)))
        val read   = PairsTable.read(t, run7, sigma2, PairsAnswer.Answered(ps))
        val sigma1 = right(ScaleIndex.of(1))
        val vm     = PairsTable.vm(read, Some(focusAt(sigma1, p17ret07)), None, qs, scales)
        assertEquals(vm.status, Some("Reading the pair rows of run 7 at σ 1°…"))
        assertEquals(vm.source, None)
    )
  }

  test("a render that changes neither the answer nor the rows reuses the source") {
    withSession(s =>
      for
        qs <- rows(s)
        ps <- pairs(s, sigma2)
      yield
        val focus     = focusAt(sigma2, p17ret07)
        val (t, _)    = PairsTable.sync(PairsTable.empty, Some(focus))
        val read      = PairsTable.read(t, run7, sigma2, PairsAnswer.Answered(ps))
        val (t1, vm1) =
          PairsTable.view(read, Some(focus), TrialPanels.referenceOf(focus, qs), qs, scales)
        // Another query and another reference at the same run and scale: the cursor
        // moves, the source is the very same value.
        val other     = focusAt(sigma2, MockStudy.key("P01", "ret_01"))
        val control   = Some((PairDesign.Control, MockStudy.key("P17", "enc_08")))
        val (t2, vm2) =
          PairsTable.view(t1, Some(other), TrialPanels.referenceOf(other, qs), qs, scales)
        val (_, vm3) = PairsTable.view(t2, Some(focus), control, qs, scales)
        assert(vm1.source.get eq vm2.source.get)
        assert(vm1.source.get eq vm3.source.get)
        assertNotEquals(vm1.focused, vm3.focused)
        // New query rows (another read) rebuild it.
        val (_, vm4) = PairsTable.view(t2, Some(focus), None, qs.map(identity), scales)
        assert(!(vm4.source.get eq vm1.source.get))
    )
  }

  test("Retry reads again only after a failure; a focused pair the table lacks is no cursor") {
    val focus  = focusAt(sigma2, p17ret07)
    val (t, _) = PairsTable.sync(PairsTable.empty, Some(focus))
    val read   = PairsTable.read(t, run7, sigma2, PairsAnswer.Answered(Vector.empty))
    assertEquals(PairsTable.retry(read)._2, Vector.empty)
    assertEquals(PairsTable.retry(t)._2, Vector.empty)
    // The table lists no row of this pair: no cursor is put on it.
    val vm = PairsTable.vm(
      read,
      Some(focus),
      Some((PairDesign.Matched, p17enc03)),
      Vector.empty,
      scales
    )
    assertEquals(vm.focused, None)
    assert(vm.source.isDefined)
  }

  test("no focus asks for a query; a focus waits for its rows") {
    assertEquals(
      PairsTable.vm(PairsTable.empty, None, None, Vector.empty, scales).status,
      Some("Choose a query in the Queries navigator")
    )
    val (t, _) = PairsTable.sync(PairsTable.empty, Some(focusAt(sigma2, p17ret07)))
    assertEquals(
      PairsTable.vm(t, Some(focusAt(sigma2, p17ret07)), None, Vector.empty, scales).status,
      Some("Reading the pair rows of run 7 at σ 2°…")
    )
  }

  // --- the per-query scale profile ---------------------------------------------------------------

  test("the query's scale profile is its served D at every scale; a query without D has none") {
    withSession(s =>
      rows(s).map { qs =>
        val doc      = t2Compare.document
        val revision =
          doc.run(run7).flatMap(r => doc.analysis(r.analysis)).getOrElse(fail("rev 4"))
        val row     = qs.find(_.query == p17ret07).getOrElse(fail("ret_07"))
        val profile =
          right(ScaleProfile.ofQuery(run7, reporting, row, scales, revision.recipe.scales))
        val points = profile.groups.flatMap(_.points)
        // FIXTURE.md: D by scale [0.19, 0.29, 0.38, 0.23].
        assertEquals(
          points.flatMap(_.d).map(d => math.round(d * 100) / 100.0),
          Vector(0.19, 0.29, 0.38, 0.23)
        )
        assertEquals(
          points.head.ref,
          StudioRef.QueryContrast(run7, right(ScaleIndex.of(0)), p17ret07)
        )
        val src = right(ScaleProfile.source(profile, right(ProfileColumns.standard)))
        assertEquals(src.rows.size, 4)
        val notAdmitted = qs.find(_.status.isInstanceOf[QueryStatus.NotAdmitted]).get
        val none        = right(
          ScaleProfile.ofQuery(run7, reporting, notAdmitted, scales, revision.recipe.scales)
        )
        assertEquals(none.groups.flatMap(_.points).map(_.d), Vector.fill(4)(None))
      }
    )
  }
