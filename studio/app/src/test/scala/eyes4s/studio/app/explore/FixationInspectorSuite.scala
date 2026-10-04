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

package eyes4s.studio.app.explore

import cats.instances.future.*
import eyes4s.plan.MapPlacement
import eyes4s.studio.app.nav.{Location, Place}
import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.core.backend.{AdmittedFixation, AnalysisRevision, RunId, TrialFixations}
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.core.fixture.{GoldenAssets, MockStudy, StoryMoment, StoryMoments}
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.selection.{FixationIndex, ScaleIndex, StudioRef}

import scala.concurrent.{ExecutionContext, Future}

/** Explore's fixation inspector headlessly (ticket S6.5): at t2 the story's
  * selected record is fixation 6 of P17 enc_03 — record 7,214, onset 2,160
  * ms, 412 ms, screen (1148, 456), image (700, 300), (+5.4°, +2.4°) — with
  * its source, trial and the run's used-by links at the shown run's scale
  * (ret_07 matched, 18 pairs as a control); a late answer is ignored.
  */
class FixationInspectorSuite extends munit.FunSuite:

  private given ExecutionContext = ExecutionContext.global

  private def right[E, A](e: Either[E, A]): A =
    e.fold(x => fail(s"unexpected Left: $x"), identity)

  private val rev4                      = AnalysisRevision(4)
  private val enc03                     = MockStudy.key("P17", "enc_03")
  private val six                       = right(FixationIndex.of(6))
  private val sixth: StudioRef.Fixation = StudioRef.Fixation(enc03, six)

  // fixations.csv's record 7,214, verbatim.
  private val line7214 = "P17,Encoding,enc_03,1,6,1148.0,456.0,2160,412,206"

  /** The inspector at t2, every read answered: fixations, used-by and
    * displays and the record (protocol 1.7, mapped as the window maps it)
    * from the fake backend.
    */
  private def settled: Future[FixationInspector] =
    val m        = StoryModels.t2Explore
    val (s0, fx) = FixationInspector.sync(FixationInspector.empty, m)
    HeadlessSession.open(StoryMoment.T2).flatMap { h =>
      def perform(
          s: FixationInspector,
          effects: Vector[InspectorEffect]
      ): Future[FixationInspector] =
        effects.foldLeft(Future.successful(s)) { (acc, e) =>
          acc.flatMap { st =>
            e match
              case InspectorEffect.ReadFixations(r, t, ask) =>
                h.trialFixations(r, t).flatMap { a =>
                  val answer = Right(
                    a.fold(err => BackendAnswer.Refused(err.message), BackendAnswer.Answered(_))
                  )
                  val (n, more) = FixationInspector.update(
                    st,
                    InspectorIntent.FixationsRead(r, t, ask, answer)
                  )
                  perform(n, more)
                }
              case InspectorEffect.ReadRecord(r, record, ask) =>
                assertEquals(record, 7214)
                // The window's adapter: protocol 1.7's page, mapped.
                h.sourceRecords(r, record, 1).map { a =>
                  val answer = Right(
                    a.fold(
                      err => BackendAnswer.Refused(err.message),
                      p => BackendAnswer.Answered(SourceRecords.served(p))
                    )
                  )
                  FixationInspector
                    .update(st, InspectorIntent.RecordRead(r, record, ask, answer))
                    ._1
                }
              case InspectorEffect.ReadUsedBy(map, ask) =>
                FixationInspector.readUsedBy[Future](h.navigator)(map).map { a =>
                  val answer = Right(
                    a.fold(err => BackendAnswer.Refused(err.message), BackendAnswer.Answered(_))
                  )
                  FixationInspector.update(st, InspectorIntent.UsedByRead(map, ask, answer))._1
                }
              case InspectorEffect.ReadDisplays(dataset, ask) =>
                val shown = GoldenAssets.registry(dataset).map(DisplaySource.Served(_))
                Future.successful(
                  FixationInspector.update(st, InspectorIntent.DisplaysRead(ask, shown))._1
                )
          }
        }
      perform(s0, fx).transformWith(x => h.close.transform(_ => x))
    }

  test("t2's selected record is fixation 6 of enc_03; the reads are asked once") {
    val m       = StoryModels.t2Explore
    val (s, fx) = FixationInspector.sync(FixationInspector.empty, m)
    assertEquals(s.focus, Some(sixth))
    assertEquals(
      fx.take(2),
      Vector(
        InspectorEffect.ReadFixations(rev4, enc03, 1),
        InspectorEffect.ReadUsedBy(
          StudioRef.TrialMap(StoryMoments.run7, StoryModels.sigma2, enc03),
          1
        )
      )
    )
    assert(fx.exists { case InspectorEffect.ReadDisplays(_, 1) => true; case _ => false }, fx)
    assertEquals(FixationInspector.sync(s, m)._2, Vector.empty)
  }

  test(
    "the fixture fixation: record 7,214, 2,160 ms, 412 ms, (1148, 456), (700, 300), (+5.4°, +2.4°)"
  ) {
    settled.map { s =>
      val vm = FixationInspector.vm(s)
      assertEquals(vm.status, None)
      assertEquals(vm.title, "Fixation 6 of 13")
      assertEquals(
        vm.fixation.map(l => (l.label, l.value)),
        Vector(
          ("Onset · duration", "2,160 ms · 412 ms"),
          ("Image frame px", "700, 300"),
          ("Screen px (raw)", "1148.0, 456.0"),
          ("Degrees from centre", "+5.4°, +2.4°"),
          ("Analysis window", "Inside")
        )
      )
      val source = vm.source.map(l => (l.label, l.value)).toMap
      assertEquals(source("Record"), "7,214 of 11,520")
      assertEquals(source("Ledger"), "Admitted · dataset r3")
      assert(source("Digest").startsWith("sha256:"), source)
      assertEquals(
        source("File"),
        right(StoryMoments.t2).datasets.last.sources.fixations.get.path.value
      )
      assertEquals(
        vm.trial.map(l => (l.label, l.value)),
        Vector(
          ("Key", "P17 · Encoding · enc_03 · occ 1"),
          ("Display", "Image · beach-042.png"),
          ("Match item", "beach-042"),
          ("Fixations", "13 · 1 outside")
        )
      )
      // The scale the served degrees are at, as the page states it.
      assertEquals(
        vm.frameNote,
        Some("Degrees from image centre, x right, y up; 35 px/°, the recipe's scale.")
      )
      assertEquals(FixationInspector.vm(s).raw, None)
    }
  }

  test("used by run 7 at σ 2°: ret_07 as the matched reference, 18 pairs as a control") {
    settled.map { s =>
      val vm = FixationInspector.vm(s)
      assertEquals(vm.usedByTitle, "Used by (run 7, σ 2°)")
      assertEquals(
        vm.usedBy.map(_.label),
        Vector("ret_07 contrast (matched reference)", "18 pairs (as a control)")
      )
      assertEquals(vm.usedBy.head.go, Intent.Explain(Place.At(StoryModels.pair)))
      // The control link opens Compare's query view, with its control list.
      vm.usedBy(1).go match
        case Intent.Explain(Place.At(StudioRef.QueryContrast(run, scale, focal))) =>
          assertEquals(
            (run, scale, focal.participant),
            (StoryMoments.run7, StoryModels.sigma2, "P17")
          )
        case other => fail(s"the control link goes to $other")
    }
  }

  private def navigate(m: AppModel, intents: Intent*): AppModel =
    AppModel.run(m, intents.toVector)._1

  test("the used-by scale is the shown run's: from its trail, else its first scale, named") {
    val m      = StoryModels.t2Explore
    val run7   = StoryMoments.run7
    val sigma1 = right(ScaleIndex.of(1))
    assertEquals(FixationInspector.scaleOf(m, run7).map(_.index), Some(StoryModels.sigma2))
    // A trail ref of another run is not this run's scale.
    val other = navigate(
      m,
      Intent.Navigate(
        Location(
          Perspective.Compare,
          Vector(Place.At(StudioRef.QueryContrast(StoryMoments.run5, sigma1, enc03)))
        )
      )
    )
    assertEquals(FixationInspector.scaleOf(other, run7).map(_.index), Some(ScaleIndex.first))
    assertEquals(FixationInspector.scaleOf(other, StoryMoments.run5).map(_.index), Some(sigma1))
    assertEquals(FixationInspector.scaleOf(m, RunId(99)), None)
  }

  test("a scale change in Compare, and a run change, re-read the used-by links") {
    val m      = StoryModels.t2Explore
    val (s, _) = FixationInspector.sync(FixationInspector.empty, m)
    val sigma1 = right(ScaleIndex.of(1))
    val atOne  = navigate(
      m,
      Intent.Navigate(
        Location(
          Perspective.Compare,
          Vector(Place.At(StudioRef.QueryContrast(StoryMoments.run7, sigma1, enc03)))
        )
      )
    )
    val (t, fx) = FixationInspector.sync(s, atOne)
    assertEquals(t.scale.map(_.index), Some(sigma1))
    assert(
      fx.contains(
        InspectorEffect.ReadUsedBy(StudioRef.TrialMap(StoryMoments.run7, sigma1, enc03), t.ask)
      ),
      fx
    )
    val run6    = navigate(m, Intent.Dispatch(Command.ShowRun(Some(StoryMoments.run6))))
    val (u, gx) = FixationInspector.sync(s, run6)
    assertEquals(u.run, Some(StoryMoments.run6))
    assert(
      gx.exists {
        case InspectorEffect.ReadUsedBy(StudioRef.TrialMap(StoryMoments.run6, _, _), a) =>
          a == u.ask
        case _ => false
      },
      gx
    )
    assertEquals(FixationInspector.vm(u).usedByTitle.startsWith("Used by (run 6, "), true)
    // A run change at the same scale is still a change.
    val sameScale = navigate(
      run6,
      Intent.Navigate(
        Location(
          Perspective.Compare,
          Vector(
            Place.At(StudioRef.QueryContrast(StoryMoments.run6, StoryModels.sigma2, enc03))
          )
        )
      )
    )
    val (v, hx) = FixationInspector.sync(s, sameScale)
    assertEquals(v.scale, s.scale)
    assertEquals(v.run, Some(StoryMoments.run6))
    assert(
      hx.contains(
        InspectorEffect.ReadUsedBy(
          StudioRef.TrialMap(StoryMoments.run6, StoryModels.sigma2, enc03),
          v.ask
        )
      ),
      hx
    )
  }

  test("'outside' counts fixations outside the window only, not dropped or off-screen ones") {
    settled.map { s =>
      val t = s.fixations.toOption.collect { case BackendAnswer.Answered(t) => t }.get
      def placed(i: Int, p: MapPlacement) =
        val a = t.fixations(i)
        right(
          AdmittedFixation.of(a.ref, a.record, a.screenX, a.screenY, a.onsetMs, a.durationMs, p)
        )
      val inside =
        t.fixations.indices.filter(i => t.fixations(i).placement == MapPlacement.InMap)
      val mixed = t.fixations
        .updated(inside(0), placed(inside(0), MapPlacement.DroppedInitial))
        .updated(inside(1), placed(inside(1), MapPlacement.OutsideScreen))
      val u  = right(TrialFixations.of(t.revision, t.dataset, t.trial, mixed))
      val vm = FixationInspector.vm(
        s.copy(fixations = eyes4s.studio.app.geometry.Loading.Ready(BackendAnswer.Answered(u)))
      )
      val outside = t.fixations.count(_.placement match
        case MapPlacement.OutsideWindow(_) => true
        case _                             => false)
      assertEquals(outside, 1)
      assertEquals(vm.trial.last.value, "13 · 1 outside")
    }
  }

  test("'Show raw record' gives the verbatim line; a late answer is ignored") {
    settled.map { s =>
      assertEquals(FixationInspector.vm(s).raw, None)
      val raw = FixationInspector.update(s, InspectorIntent.ShowRaw(true))._1
      assertEquals(FixationInspector.vm(raw).raw, Some(line7214))
      val late = FixationInspector
        .update(
          s,
          InspectorIntent.UsedByRead(
            StudioRef.TrialMap(StoryMoments.run7, StoryModels.sigma2, enc03),
            s.ask - 1,
            Left("late")
          )
        )
        ._1
      assertEquals(late, s)
      // A record answer for another record is not installed.
      val other = FixationInspector
        .update(
          s.copy(record = eyes4s.studio.app.geometry.Loading.Waiting),
          InspectorIntent.RecordRead(rev4, 7215, s.ask, Left("other"))
        )
        ._1
      assertEquals(other.record, eyes4s.studio.app.geometry.Loading.Waiting)
    }
  }

  test("with nothing selected the inspector asks to select a fixation") {
    val vm = FixationInspector.vm(FixationInspector.empty)
    assertEquals(vm.status, Some("Select a fixation to inspect it"))
    assertEquals(ScaleIndex.of(0).isRight, true)
  }
