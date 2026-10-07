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
import eyes4s.studio.app.nav.Place
import eyes4s.studio.app.plot.{LadderColumns, ScaleLadder}
import eyes4s.studio.app.text.Format
import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.core.backend.PairDesign
import eyes4s.studio.core.fixture.{MockStudy, StoryMoment, StoryMoments}
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.selection.StudioRef

import scala.concurrent.{ExecutionContext, Future}

/** Compare's contrast readout headlessly (ticket S8.3; Main.dc.html,
  * contrast): the focus query's readout is FIXTURE.md's at every scale, B is
  * the mean of the control pairs' cosines (never a cosine to an averaged
  * map), the inspected pair follows the reference panel, and Prev and Next
  * walk the matched pair and then the controls by cosine.
  */
class ContrastPaneSuite extends munit.FunSuite:
  import StoryModels.*

  private given ExecutionContext = ExecutionContext.global

  private val run7    = StoryMoments.run7
  private val columns = LadderColumns.standard.fold(e => fail(e.message), identity)

  private def right[E, A](e: Either[E, A]): A =
    e.fold(x => fail(s"unexpected Left: $x"), identity)

  private def ascii(s: String): String = s.replace(Format.Minus, "-")

  /** The pane at `m`, its ladder read from the fake backend. */
  private def loaded(m: AppModel): Future[ContrastPane] =
    HeadlessSession.open(StoryMoment.T2).flatMap { s =>
      val focus                                 = TrialPanels.focusOf(m)
      val (pane, fx)                            = ContrastPane.sync(ContrastPane.empty, focus)
      val ContrastEffect.LoadLadder(run, query) = fx.head: @unchecked
      (for
        summary <- s.result(run)
        ladder  <- ScaleLadder.load[Future](s.inspect, s.navigator)(
          run,
          query,
          right(summary).scaleLabels
        )
      yield ContrastPane.read(pane, run, query, LadderAnswer.Answered(right(ladder))))
        .transformWith(x => s.close.transform(_ => x))
    }

  private def vmAt(p: ContrastPane, m: AppModel): ContrastVM =
    val focus = TrialPanels.focusOf(m)
    ContrastPane.vm(p, focus, focus.flatMap(_.reference), columns)

  private val fixtureM = Vector(0.41, 0.58, 0.73, 0.86)
  private val fixtureB = Vector(0.22, 0.29, 0.35, 0.63)
  private val fixtureD = Vector(0.19, 0.29, 0.38, 0.23)

  test("the focus readout at 2° is M 0.73, B 0.35 of 19 controls, D +0.38, with refs") {
    loaded(t2Compare).map { p =>
      val vm = vmAt(p, t2Compare)
      assertEquals(vm.status, None)
      assertEquals(vm.caption, "ret_07 contrast · σ 2°")
      assertEquals(vm.hero.map(h => (h.label, h.value, h.ref)), Some(("D", "+0.38", query)))
      assertEquals(vm.m.map(v => (v.label, v.value, v.ref)), Some(("M matched", "0.73", pair)))
      assertEquals(vm.b.map(v => (v.label, v.value)), Some(("B mean of 19 controls", "0.35")))
      assertEquals(
        vm.confound,
        "Spatial correspondence, not replay. D does not separate participant-specific " +
          "reinstatement from item-driven salience common to all viewers of that image, and " +
          "may retain residual centre bias."
      )
      assert(vm.ladder.isDefined)
      assertEquals(vm.focusScale, Some("2°"))
    }
  }

  test("all four scales match FIXTURE.md") {
    loaded(t2Compare).map { p =>
      val ladder = p.ladder match
        case Some(LadderAnswer.Answered(l)) => l
        case other                          => fail(s"no ladder: $other")
      assertEquals(ladder.scales.map(_.label), Vector("0.5°", "1°", "2°", "4°"))
      ladder.scales.zipWithIndex.foreach { (s, i) =>
        assertEquals(Format.decimal(s.m, 2), Format.decimal(fixtureM(i), 2), s.label)
        assertEquals(Format.decimal(s.b, 2), Format.decimal(fixtureB(i), 2), s.label)
        assertEquals(
          ascii(Format.signed(s.d, 2)),
          "+" + Format.decimal(fixtureD(i), 2),
          s.label
        )
        // Every scale's readout reports that scale's values.
        val at  = right(eyes4s.studio.core.selection.ScaleIndex.of(i))
        val ref = StudioRef.QueryContrast(run7, at, p17ret07)
        val m   = AppModel.run(t2Compare, Vector(Intent.Explain(Place.At(ref))))._1
        val vm  = vmAt(p, m)
        assertEquals(
          vm.hero.map(_.value).map(ascii),
          Some("+" + Format.decimal(fixtureD(i), 2))
        )
        assertEquals(vm.caption, s"ret_07 contrast · σ ${s.label}")
      }
    }
  }

  test("B is the mean of the control pairs' cosines, never a cosine to an averaged map") {
    loaded(t2Compare).map { p =>
      val ladder = p.ladder.collect { case LadderAnswer.Answered(l) => l }.get
      // The fixture serves control cosines at 2° only; elsewhere they are
      // missing, never zero.
      ladder.scales.foreach { s =>
        assertEquals(s.controls.size, s.members, s.label)
        assert(s.mean.toString.contains("Reduction"), s.mean)
        val cosines = s.controls.flatMap(_.cosine)
        if s.label == "2°" then
          assertEquals(cosines.size, s.members)
          // The backend's B is the pairwise mean, to the fixture's two decimals.
          assertEqualsDouble(s.b, cosines.sum / cosines.size, 0.005)
        else assertEquals(cosines, Vector.empty, s.label)
      }
      val at2 = ladder.scales(2)
      assertEquals(at2.members, 19)
      assertEquals(Format.decimal(at2.controls.flatMap(_.cosine).max, 2), "0.61")
    }
  }

  test("Prev and Next walk the matched pair, then the controls by descending cosine") {
    loaded(t2Compare).map { p =>
      val vm = vmAt(p, t2Compare)
      val i  = vm.inspected.getOrElse(fail("no inspected pair"))
      assertEquals(i.heading, "Matched · beach-042")
      assertEquals((i.score.value, i.score.ref), ("0.73", pair))
      assertEquals(i.note, "The matched pair. Its cosine is M.")
      assertEquals(i.rank, "matched · then 19 controls")
      assertEquals(i.prev, None)
      // Only a step that can be taken is a stop.
      import eyes4s.studio.app.vm.{A11yRole, FocusStop}
      assertEquals(
        ContrastPane.focusStops(vm),
        Vector(FocusStop(A11yRole.Button, "Next →, next reference"))
      )
      // Next: the highest control, street-112 (enc_01) at 0.61.
      val next    = i.next.getOrElse(fail("no next"))
      val first   = AppModel.run(t2Compare, Vector(next))._1
      val onFirst = vmAt(p, first).inspected.get
      assertEquals(onFirst.heading, "Control · street-112")
      assertEquals(onFirst.score.value, "0.61")
      assertEquals(onFirst.note, "One of 19 controls. It enters B; it is not M.")
      assertEquals(onFirst.rank, "control 1 of 19 by cosine")
      assertEquals(onFirst.prev, Some(Intent.Explain(Place.At(pair))))
      // Walking Next visits every control once, by descending cosine, then stops.
      val walked = Iterator
        .iterate((first, Option(onFirst))) { (m, vm) =>
          vm.flatMap(_.next).fold((m, None)) { n =>
            val m2 = AppModel.run(m, Vector(n))._1
            (m2, vmAt(p, m2).inspected)
          }
        }
        .takeWhile(_._2.isDefined)
        .map(_._2.get)
        .toVector
      assertEquals(walked.size, 19)
      val scores = walked.map(_.score.value.toDouble)
      assertEquals(scores, scores.sortBy(-_))
      assertEquals(walked.last.next, None)
      assertEquals(walked.map(_.score.ref).distinct.size, 19)
      assert(walked.forall(_.score.ref match
        case StudioRef.Pair(_, _, PairDesign.Control, _, _) => true
        case _                                              => false))
    }
  }

  test("the pane reads a query once, ignores a stale ladder and says what it is doing") {
    val focus      = TrialPanels.focusOf(t2Compare)
    val (p, first) = ContrastPane.sync(ContrastPane.empty, focus)
    assertEquals(first, Vector(ContrastEffect.LoadLadder(run7, p17ret07)))
    assertEquals(ContrastPane.sync(p, focus)._2, Vector.empty)
    assertEquals(vmAt(p, t2Compare).status, Some("Reading the contrast of ret_07…"))
    val stale =
      ContrastPane.read(p, run7, MockStudy.key("P02", "ret_01"), LadderAnswer.Failed("x"))
    assertEquals(stale, p)
    val failed = ContrastPane.read(p, run7, p17ret07, LadderAnswer.Failed("no pairs"))
    assertEquals(
      vmAt(failed, t2Compare).status,
      Some("The contrast of ret_07 could not be read: no pairs")
    )
    assertEquals(
      ContrastPane.vm(ContrastPane.empty, None, None, columns).status,
      Some("Choose a query in the Queries navigator")
    )
  }

  /** `p` with the 2° control `trial` served without a cosine, as the
    * backend's `Unavailable` leaves it, and B over the other 18.
    */
  private def unscored(p: ContrastPane, trial: String): ContrastPane =
    val ladder = p.ladder.collect { case LadderAnswer.Answered(l) => l }.get
    val scales = ladder.scales.map { s =>
      if s.label != "2°" then s
      else
        s.copy(
          controls = s.controls.map(c =>
            if c.reference.trial == trial then c.copy(item = None, cosine = None) else c
          ),
          members = s.members - 1
        )
    }
    p.copy(ladder = Some(LadderAnswer.Answered(ladder.copy(scales = scales))))

  test("a control served without a cosine is said so, ranked last, with one count of B") {
    loaded(t2Compare).map { p0 =>
      // enc_01 (street-112) has the highest cosine; served without one, it goes last.
      val p  = unscored(p0, "enc_01")
      val vm = vmAt(p, t2Compare)
      assertEquals(vm.b.map(_.label), Some("B mean of 18 controls"))
      val i = vm.inspected.get
      assertEquals(i.rank, "matched · then 18 controls, 1 without a served cosine")
      val walked = Iterator
        .iterate((t2Compare, Option(i))) { (m, vm) =>
          vm.flatMap(_.next).fold((m, None)) { n =>
            val m2 = AppModel.run(m, Vector(n))._1
            (m2, vmAt(p, m2).inspected)
          }
        }
        .drop(1)
        .takeWhile(_._2.isDefined)
        .map(_._2.get)
        .toVector
      assertEquals(walked.size, 19)
      // The scored controls first, by descending cosine; the unscored one last.
      val scored = walked.init.map(_.score.value.toDouble)
      assertEquals(scored, scored.sortBy(-_))
      assertEquals(walked.head.note, "One of 18 controls. It enters B; it is not M.")
      assertEquals(walked.head.rank, "control 1 of 18 by cosine")
      val last = walked.last
      assertEquals(last.heading, "Control · enc_01")
      assertEquals(last.score.value, "not served")
      assertEquals(
        last.note,
        "No cosine is served for this control here. B is the backend's mean of 18 controls."
      )
      assertEquals(last.rank, "no cosine · after the 18 controls ranked by cosine")
      assertEquals(last.next, None)
    }
  }

  test("a scale the ladder lacks has a status; a failed read offers Retry") {
    loaded(t2Compare).map { p =>
      val ladder = p.ladder.collect { case LadderAnswer.Answered(l) => l }.get
      val short  = p.copy(ladder =
        Some(LadderAnswer.Answered(ladder.copy(scales = ladder.scales.take(2))))
      )
      val vm = vmAt(short, t2Compare)
      assertEquals(vm.status, Some("The ladder of ret_07 has no scale 2."))
      assertEquals((vm.hero, vm.inspected), (None, None))
      assertEquals(vm.retry, None)
      val failed = ContrastPane.read(p, run7, p17ret07, LadderAnswer.Failed("no pairs"))
      assertEquals(vmAt(failed, t2Compare).retry, Some("Retry"))
      val (again, effects) = ContrastPane.retry(failed)
      assertEquals(effects, Vector(ContrastEffect.LoadLadder(run7, p17ret07)))
      assertEquals(again.ladder, None)
      assertEquals(vmAt(again, t2Compare).status, Some("Reading the contrast of ret_07…"))
      // Nothing failed: nothing to retry.
      assertEquals(ContrastPane.retry(p), (p, Vector.empty))
    }
  }
