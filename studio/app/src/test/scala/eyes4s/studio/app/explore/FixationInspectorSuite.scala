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
import eyes4s.studio.app.nav.Place
import eyes4s.studio.app.{Intent, StoryModels}
import eyes4s.studio.core.backend.{AnalysisRevision, PairDesign}
import eyes4s.studio.core.document.SourceRole
import eyes4s.studio.core.fixture.{GoldenAssets, MockStudy, StoryMoment, StoryMoments}
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.selection.{FixationIndex, RecordNumber, ScaleIndex, StudioRef}

import scala.concurrent.{ExecutionContext, Future}

/** Explore's fixation inspector headlessly (ticket S6.5): at t2 the story's
  * selected record is fixation 6 of P17 enc_03 — record 7,214, onset 2,160
  * ms, 412 ms, screen (1148, 456), image (700, 300), (+5.4°, +2.4°) — with
  * its source, trial and the run's used-by links (ret_07 matched, 18 other
  * P17 queries as a control); a late answer is ignored.
  */
class FixationInspectorSuite extends munit.FunSuite:

  private given ExecutionContext = ExecutionContext.global

  private def right[E, A](e: Either[E, A]): A =
    e.fold(x => fail(s"unexpected Left: $x"), identity)

  private val rev4                      = AnalysisRevision(4)
  private val enc03                     = MockStudy.key("P17", "enc_03")
  private val six                       = right(FixationIndex.of(6))
  private val sixth: StudioRef.Fixation = StudioRef.Fixation(enc03, six)

  // The record a source would serve for record 7,214: fixations.csv's line,
  // its image-frame position and its degrees at the board's 35 px/°.
  private val record7214 = SourceRecordRow(
    right(RecordNumber.of(7214)),
    StudioRef.SourceRecord(
      enc03,
      Some(six),
      SourceRole.Fixations,
      right(RecordNumber.of(7214))
    ),
    Some(six),
    enc03,
    6,
    2160.0,
    412.0,
    "1148.0",
    "456.0",
    Some(FramePosition(700.0, 300.0)),
    Some(FramePosition((700.0 - 512.0) / 35.0, (384.0 - 300.0) / 35.0)),
    Some(206),
    RecordPlace.Inside,
    "P17,Encoding,enc_03,1,6,1148.0,456.0,2160,412,206"
  )

  /** The inspector at t2, every read answered: fixations, used-by and
    * displays from the fake backend, the record from `record7214`.
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
                val page = SourceRecordPage(record - 1, 11520, Vector(record7214))
                Future.successful(
                  FixationInspector
                    .update(
                      st,
                      InspectorIntent
                        .RecordRead(r, record, ask, Right(BackendAnswer.Answered(page)))
                    )
                    ._1
                )
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
          ("Image frame px", "700.0, 300.0"),
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
      // The board's FrameNote names the declared px/°.
      assert(vm.frameNote.exists(_.contains("px/°")), vm.frameNote)
    }
  }

  test("used by run 7: ret_07 as the matched reference, 18 other P17 queries as a control") {
    settled.map { s =>
      val vm = FixationInspector.vm(s)
      assertEquals(vm.usedByTitle, "Used by (run 7)")
      assertEquals(
        vm.usedBy.map(_.label),
        Vector(
          "ret_07 contrast (matched reference)",
          "18 other admitted P17 queries (as a control)"
        )
      )
      assertEquals(vm.usedBy.head.go, Intent.Explain(Place.At(StoryModels.pair)))
      vm.usedBy(1).go match
        case Intent.Explain(Place.At(StudioRef.Pair(_, _, PairDesign.Control, focal, ref))) =>
          assertEquals((focal.participant, ref), ("P17", enc03))
        case other => fail(s"the control link goes to $other")
    }
  }

  test("'Show raw record' gives the verbatim line; a late answer is ignored") {
    settled.map { s =>
      assertEquals(FixationInspector.vm(s).raw, None)
      val raw = FixationInspector.update(s, InspectorIntent.ShowRaw(true))._1
      assertEquals(FixationInspector.vm(raw).raw, Some(record7214.raw))
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
