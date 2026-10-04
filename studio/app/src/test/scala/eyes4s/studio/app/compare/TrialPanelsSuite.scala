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

import eyes4s.studio.app.nav.Place
import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.plan.MapPlacement
import eyes4s.studio.core.backend.{
  AnalysisRevision,
  PageRequest,
  PairDesign,
  QueryRow,
  ResultAddress,
  TrialKey
}
import eyes4s.studio.core.fixture.{GoldenAssets, MockStudy, StoryMoment, StoryMoments}
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.selection.{FixationIndex, StudioRef}

import scala.concurrent.{ExecutionContext, Future}

/** Compare's query and reference trial panels headlessly (ticket S8.2;
  * Main.dc.html, panels): the query panel follows the trail's query and
  * reports its contrast; the reference panel shows the trail's pair, or the
  * matched reference, and reports the inspected pair's score, a separate
  * readout with its own ref; a control switches the reference panel to
  * 'Control · item' and the matched reference stays named and reachable.
  */
class TrialPanelsSuite extends munit.FunSuite:
  import StoryModels.*

  private given ExecutionContext = ExecutionContext.global

  private val run7               = StoryMoments.run7
  private val scales             = Vector("0.5°", "1°", "2°", "4°")
  private val p17enc08           = MockStudy.key("P17", "enc_08")
  private val control: StudioRef =
    StudioRef.Pair(run7, sigma2, PairDesign.Control, p17ret07, p17enc08)

  private def right[E, A](e: Either[E, A]): A =
    e.fold(x => fail(s"unexpected Left: $x"), identity)

  // The golden registry's displays (embedded at build time) and three made-up
  // fixations per trial: what a content source answers, behind the port.
  private lazy val registry =
    right(GoldenAssets.registry(right(StoryMoments.t2).datasets.last))
  private val rev4 = AnalysisRevision(4)
  private def contentOf(
      rev: AnalysisRevision,
      trial: TrialKey
  ): Either[ContentError, TrialContent] =
    if rev != rev4 then Left(ContentError.Unreadable(trial, s"no revision ${rev.number}"))
    else
      registry.display(trial).toRight(ContentError.NotServed(trial)).map { d =>
        TrialContent(
          d,
          registry.screen,
          Vector.tabulate(3)(i =>
            ContentFixation(
              trial,
              right(FixationIndex.of(i + 1)),
              900.0 + 10 * i,
              500.0,
              100 * i,
              200,
              MapPlacement.InMap
            )
          )
        )
      }

  private def withSession[A](f: HeadlessSession => Future[A]): Future[A] =
    HeadlessSession
      .open(StoryMoment.T2)
      .flatMap(s => f(s).transformWith(x => s.close.transform(_ => x)))

  private def rowsOf(s: HeadlessSession): Future[Vector[QueryRow]] =
    s.queries(run7, right(PageRequest.first(PageRequest.MaximumSize))).map(q => right(q).rows)

  /** Sync on `m`, perform the one inspection asked, and record its answer. */
  private def settle(
      s: HeadlessSession,
      panels: TrialPanels,
      m: AppModel,
      rows: Vector[QueryRow],
      revision: Option[AnalysisRevision] = None
  ): Future[(TrialPanels, Vector[PanelsEffect])] =
    val (synced, effects) = TrialPanels.sync(panels, m, rows, revision)
    effects
      .foldLeft(Future.successful(synced)) {
        case (acc, PanelsEffect.InspectPair(run, address, pair)) =>
          acc.flatMap(p =>
            s.inspect(run, address).map { a =>
              TrialPanels.update(
                p,
                PanelsIntent
                  .PairRead(pair, a.fold(PairAnswer.Refused(_), PairAnswer.Answered(_)))
              )
            }
          )
        case (acc, PanelsEffect.ReadContent(rev, trial)) =>
          acc.map(p =>
            TrialPanels.update(p, PanelsIntent.ContentRead(trial, contentOf(rev, trial)))
          )
      }
      .map(p => (p, effects))

  test("at t2 the query panel is P17 ret_07 and the reference its matched enc_03") {
    withSession { s =>
      rowsOf(s).flatMap { rows =>
        settle(s, TrialPanels.empty, t2Compare, rows).map { (panels, effects) =>
          assertEquals(
            effects,
            Vector(
              PanelsEffect.InspectPair(
                run7,
                ResultAddress.PairRow(2, PairDesign.Matched, p17ret07, p17enc03),
                pair
              )
            )
          )
          val vm = TrialPanels.vm(panels, rows, scales)
          val q  = vm.query.getOrElse(fail("no query panel"))
          assertEquals((q.role, q.title), (PanelRole.Query, "P17 · ret_07 · beach-042"))
          assertEquals(q.readout, PanelReadout("Query contrast D · σ 2°: +0.38", query))
          val r = vm.reference.getOrElse(fail("no reference panel"))
          assertEquals(
            (r.role, r.title, r.trial),
            (PanelRole.Matched, "P17 · enc_03 · beach-042", p17enc03)
          )
          assertEquals(r.readout.ref, pair)
          assert(r.readout.text.startsWith("Inspected pair score · σ 2°: 0."), r.readout.text)
          assertEquals(
            vm.extras,
            Some(ReferenceExtras(None, "Matched reference: P17 · enc_03 · beach-042"))
          )
          // Synced again on the same model, nothing more is asked.
          assertEquals(TrialPanels.sync(panels, t2Compare, rows, None)._2, Vector.empty)
        }
      }
    }
  }

  test("a control switches the reference panel to 'Control · item'; Back returns to matched") {
    withSession { s =>
      rowsOf(s).flatMap { rows =>
        val (onControl, _) = AppModel.run(t2Compare, Vector(Intent.Explain(Place.At(control))))
        for
          (matched, _)  <- settle(s, TrialPanels.empty, t2Compare, rows)
          (switched, e) <- settle(s, matched, onControl, rows)
        yield
          assertEquals(
            e.collect { case PanelsEffect.InspectPair(_, address, _) => address },
            Vector(ResultAddress.PairRow(2, PairDesign.Control, p17ret07, p17enc08))
          )
          val vm = TrialPanels.vm(switched, rows, scales)
          val r  = vm.reference.get
          assertEquals((r.role.label, r.title), ("Control", "P17 · enc_08 · dog-077"))
          assertEquals(r.readout.ref, control)
          // The matched identity stays named and reachable.
          val extras = vm.extras.get
          assertEquals(extras.matched, "Matched reference: P17 · enc_03 · beach-042")
          val (label, back) = extras.back.getOrElse(fail("no way back"))
          assertEquals(label, "Back to matched reference")
          assertEquals(back, Intent.Explain(Place.At(pair)))
          val (returned, _) = AppModel.run(onControl, Vector(back))
          val again         = TrialPanels.sync(switched, returned, rows, None)._1
          assertEquals(
            TrialPanels.focusOf(returned).flatMap(_.reference),
            Some((PairDesign.Matched, p17enc03))
          )
          assertEquals(
            TrialPanels.vm(again, rows, scales).reference.map(_.role),
            Some(PanelRole.Matched)
          )
          // Inside the panels' stops: the underlay toggle, and Back while a control shows.
          import eyes4s.studio.app.vm.{A11yRole, FocusStop}
          assertEquals(
            TrialPanels.queryStops(vm),
            Vector(FocusStop(A11yRole.ToggleButton, "Underlay remembered image"))
          )
          assertEquals(
            TrialPanels.referenceStops(vm),
            Vector(FocusStop(A11yRole.Button, "Back to matched reference"))
          )
          assertEquals(
            TrialPanels.referenceStops(TrialPanels.vm(matched, rows, scales)),
            Vector.empty
          )
          // The query panel did not move.
          assertEquals(vm.query, TrialPanels.vm(matched, rows, scales).query)
      }
    }
  }

  test("the pair score and the query contrast are separate readouts with separate refs") {
    withSession { s =>
      rowsOf(s).flatMap { rows =>
        val (onControl, _) = AppModel.run(t2Compare, Vector(Intent.Explain(Place.At(control))))
        settle(s, TrialPanels.empty, onControl, rows).map { (panels, _) =>
          val vm = TrialPanels.vm(panels, rows, scales)
          val q  = vm.query.get.readout
          val r  = vm.reference.get.readout
          assertNotEquals(q.ref, r.ref)
          assert(
            q.ref match { case StudioRef.QueryContrast(_, _, _) => true; case _ => false },
            q.ref
          )
          assert(
            r.ref match { case StudioRef.Pair(_, _, _, _, _) => true; case _ => false },
            r.ref
          )
          assert(q.text.startsWith("Query contrast D"), q.text)
          assert(r.text.startsWith("Inspected pair score"), r.text)
          // The two numbers differ: D is the query's, the score the pair's.
          assertNotEquals(q.text.split(": ").last, r.text.split(": ").last)
        }
      }
    }
  }

  test("a stale answer is ignored; a trail at the query shows the matched reference") {
    withSession { s =>
      rowsOf(s).map { rows =>
        val atQuery = AppModel.run(t2Compare, Vector(Intent.Explain(Place.At(query))))._1
        val (p, e)  = TrialPanels.sync(TrialPanels.empty, atQuery, rows, None)
        assertEquals(TrialPanels.focusOf(atQuery).map(_.reference), Some(None))
        assertEquals(e.size, 1)
        assertEquals(
          TrialPanels.vm(p, rows, scales).reference.map(_.title),
          Some("P17 · enc_03 · beach-042")
        )
        assertEquals(
          TrialPanels.vm(p, rows, scales).reference.map(_.readout.text),
          Some("Inspected pair score · σ 2°: reading…")
        )
        val other = StudioRef.Pair(
          run7,
          sigma2,
          PairDesign.Matched,
          MockStudy.key("P02", "ret_01"),
          p17enc03
        )
        val stale =
          TrialPanels.update(p, PanelsIntent.PairRead(other, PairAnswer.Failed("late")))
        assertEquals(stale, p)
        // No query on the trail: the panels say where to choose one.
        val none = TrialPanels.vm(TrialPanels.empty, rows, scales)
        assertEquals(none.empty, Some("Choose a query in the Queries navigator"))
        assertEquals(TrialPanels.update(p, PanelsIntent.Underlay(true)).underlay, true)
      }
    }
  }

  test("the panels read the query's, the reference's and the matched trial's content once") {
    withSession { s =>
      rowsOf(s).flatMap { rows =>
        val (onControl, _) = AppModel.run(t2Compare, Vector(Intent.Explain(Place.At(control))))
        for
          (matched, e1)  <- settle(s, TrialPanels.empty, t2Compare, rows, Some(rev4))
          (switched, e2) <- settle(s, matched, onControl, rows, Some(rev4))
        yield
          // ret_07, and enc_03 once although it is both reference and matched.
          assertEquals(
            e1.collect { case PanelsEffect.ReadContent(_, k) => k },
            Vector(p17ret07, p17enc03)
          )
          val vm = TrialPanels.vm(matched, rows, scales)
          vm.query.get.content match
            case PanelContent.Shown(c) => assertEquals(c.display.trial, p17ret07)
            case other                 => fail(s"query content: $other")
          assertEquals(vm.query.get.count, "3 fix")
          // The remembered image is the matched reference's stored image.
          val enc03 = registry.display(p17enc03).get.display
          val asset = enc03 match
            case eyes4s.studio.core.assets.Display.Image(
                  eyes4s.studio.core.assets.AssetLink.Present(a)
                ) =>
              a
            case other => fail(s"enc_03 shows $other")
          assertEquals(vm.remembered, Some(Remembered(asset, false)))
          val on = TrialPanels.update(matched, PanelsIntent.Underlay(true))
          assertEquals(
            TrialPanels.vm(on, rows, scales).remembered,
            Some(Remembered(asset, true))
          )
          // A control: only its trial is new; the query and matched trial stay.
          assertEquals(
            e2.collect { case PanelsEffect.ReadContent(_, k) => k },
            Vector(p17enc08)
          )
          TrialPanels.vm(switched, rows, scales).reference.get.content match
            case PanelContent.Shown(c) => assertEquals(c.display.trial, p17enc08)
            case other                 => fail(s"reference content: $other")
          assertEquals(switched.contents.keySet, Set(p17ret07, p17enc03, p17enc08))
          // Back on the matched reference, the control's content is let go.
          val (back, e3) = TrialPanels.sync(switched, t2Compare, rows, Some(rev4))
          assertEquals(e3.collect { case PanelsEffect.ReadContent(_, k) => k }, Vector.empty)
          assertEquals(back.contents.keySet, Set(p17ret07, p17enc03))
      }
    }
  }

  test(
    "content is read only with a revision; stale content is dropped; a refusal names the trial"
  ) {
    withSession { s =>
      rowsOf(s).map { rows =>
        assertEquals(
          TrialPanels
            .sync(TrialPanels.empty, t2Compare, rows, None)
            ._2
            .collect { case e @ PanelsEffect.ReadContent(_, _) => e },
          Vector.empty
        )
        val (p, _) = TrialPanels.sync(TrialPanels.empty, t2Compare, rows, Some(rev4))
        assertEquals(TrialPanels.vm(p, rows, scales).query.get.content, PanelContent.Reading)
        val other = MockStudy.key("P02", "ret_01")
        assertEquals(
          TrialPanels.update(p, PanelsIntent.ContentRead(other, contentOf(rev4, other))),
          p
        )
        val refused = TrialPanels.update(
          p,
          PanelsIntent.ContentRead(p17ret07, Left(ContentError.NotServed(p17ret07)))
        )
        assertEquals(
          TrialPanels.vm(refused, rows, scales).query.get.content,
          PanelContent.Unavailable("The content of P17 · ret_07 is not served.")
        )
        assertEquals(TrialPanels.vm(refused, rows, scales).query.get.count, "")
      }
    }
  }

  test("a trial's Table tab lists its fixations with their refs, as served") {
    val c      = right(contentOf(rev4, p17ret07))
    val source = right(TrialPanels.fixationTable(c))
    assertEquals(source.caption, "Fixations of P17 · ret_07")
    assertEquals(
      source.columns.map(_.header),
      Vector("Fixation", "x (px)", "y (px)", "Onset (ms)", "Duration (ms)", "Map placement")
    )
    assertEquals(source.rows.map(_.ref), c.fixations.map(_.ref))
    assertEquals(
      source.cells(1).getOrElse(fail("no row")),
      Vector("2", "910.0", "500.0", "100", "200", "in map")
    )
  }
