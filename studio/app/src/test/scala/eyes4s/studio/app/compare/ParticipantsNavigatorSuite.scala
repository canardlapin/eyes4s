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

import eyes4s.studio.app.{AppModel, StoryModels}
import eyes4s.studio.app.nav.Place
import eyes4s.studio.core.backend.ReportRole
import eyes4s.studio.core.document.{Perspective, ReportingSpec}
import eyes4s.studio.core.fixture.{StoryMoment, StoryMoments}
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.selection.{ReportGroup, ScaleIndex, StudioRef}
import scala.concurrent.{ExecutionContext, Future}

class ParticipantsNavigatorSuite extends munit.FunSuite:
  private given ExecutionContext              = ExecutionContext.global
  private val model                           = StoryModels.t3Summary
  private val scale                           = StoryModels.sigma2
  private def right[E, A](a: Either[E, A]): A = a.fold(e => fail(e.toString), identity)
  private def loaded: Future[CompareSummary]  =
    HeadlessSession.open(StoryMoment.T3).flatMap { session =>
      val start = CompareSummary.sync(CompareSummary.empty, model)._1
      (for
        result <- session.result(StoryMoments.run7)
        first = CompareSummary.update(
          start,
          SummaryIntent.SummaryRead(StoryMoments.run7, SummaryAnswer.Answered(right(result)))
        )
        answers <- Future.sequence(first._2.collect {
          case SummaryEffect.RequestReport(run, spec, k, whole) =>
            session
              .report(run, spec, k.value)
              .map(a =>
                (run, spec, k, whole, a.fold(ReportAnswer.Refused(_), ReportAnswer.Answered(_)))
              )
        })
      yield answers.foldLeft(first._1) { case (s, (run, spec, k, whole, a)) =>
        CompareSummary.update(s, SummaryIntent.ReportRead(run, spec, k, whole, a))._1
      }).transformWith(answer => session.close.transform(_ => answer))
    }
  private def vm(
      s: CompareSummary,
      m: AppModel = model,
      nav: ParticipantsNavigator = ParticipantsNavigator.empty
  ) =
    ParticipantsNavigator.vm(ParticipantsNavigator.follow(nav, s), s, m)

  test(
    "24 overall cells keep exact refs, signed means and separately labelled run accounting"
  ) {
    loaded.map { s =>
      val view = vm(s)
      assertEquals(view.status, None)
      assertEquals(view.heading, "Overall D · σ 2°")
      assertEquals(view.rows.size, 24)
      val report = s.reports((scale, true)).asInstanceOf[ReportAnswer.Answered].report
      assertEquals(
        view.rows.map(_.ref),
        report.participants
          .filter(p => p.role == ReportRole.Difference && p.group.isEmpty)
          .map(_.ref)
      )
      val p5 = view.rows.find(_.participant == "P05").get
      assertEquals(p5.value, "+0.35")
      assertEquals(p5.accounting, "17 / 20 · 3 failed")
      val p17 = view.rows.find(_.participant == "P17").get
      assertEquals(p17.value, "+0.38")
      assertEquals(p17.queries, "19")
      assert(p17.spoken.contains("overall D +0.38, n 19"))
      assert(view.rows.head.cursor)
    }
  }
  test(
    "activation keeps the saved reporting root and exact overall cell, distinct from a grouped selection"
  ) {
    loaded.map { s =>
      val view    = vm(s); val row = view.rows.find(_.participant == "P17").get
      val current = ParticipantsNavigator
        .follow(ParticipantsNavigator.empty, s)
        .copy(cursor = Some(row.ref))
      val shown  = vm(s, nav = current)
      val intent = ParticipantsNavigator.key(current, shown, NavigatorKey.Activate)._2.get
      val moved  = AppModel.update(model, intent)._1
      assertEquals(
        moved.navigation.trail(Perspective.Compare),
        Vector(Place.Summary(s.spec.get.id), Place.At(row.ref))
      )
      assert(vm(s, moved).rows.find(_.participant == "P17").get.selected)
      assertEquals(vm(s).rows.filter(_.selected).map(_.participant), Vector("P17"))
      assert(!model.selection.selected.contains(row.ref))
      row.ref match
        case StudioRef.ReportParticipant(run, rep, k, group, role, p) =>
          assertEquals(
            (run, k, group, role, p),
            (StoryMoments.run7, scale, ReportGroup.Whole, ReportRole.Difference, "P17")
          )
          assertEquals(rep, CompareSummary.overall(s.spec.get).get.id)
        case other => fail(s"not a served report ref: $other")
    }
  }
  test(
    "one cursor moves to both ends without activating; changed scale and edited spec reset it"
  ) {
    loaded.map { s =>
      val nav  = ParticipantsNavigator.follow(ParticipantsNavigator.empty, s)
      val last = ParticipantsNavigator.key(nav, vm(s), NavigatorKey.Last)
      assertEquals(last._2, None)
      assert(vm(s, nav = last._1).rows.last.cursor)
      val down = ParticipantsNavigator.key(last._1, vm(s, nav = last._1), NavigatorKey.Down)._1
      assertEquals(down.cursor, last._1.cursor)
      val first = ParticipantsNavigator.key(down, vm(s, nav = down), NavigatorKey.First)._1
      assert(vm(s, nav = first).rows.head.cursor)
      val changed = s.copy(scale = Some(right(ScaleIndex.of(0))))
      assertEquals(ParticipantsNavigator.follow(last._1, changed).cursor, None)
      val spec   = s.spec.get
      val edited = right(
        ReportingSpec.of(
          spec.id,
          spec.name + " edited",
          spec.groupBy,
          spec.filters,
          spec.minimumPerGroup,
          spec.weighting
        )
      )
      assertEquals(
        ParticipantsNavigator.follow(last._1, s.copy(spec = Some(edited))).cursor,
        None
      )
    }
  }
  test("no run, loading, refusal, missing values and absent accounting stay explicit") {
    assertEquals(vm(CompareSummary.empty).status, Some("No run shown."))
    loaded.map { s =>
      assert(vm(s.copy(reports = Map.empty)).status.exists(_.startsWith("Reading")))
      assert(vm(s.copy(spec = None)).status.exists(_.contains("No reporting specification")))
      val noScale = s.copy(
        scale = None,
        reports = Map((scale, false) -> ReportAnswer.Failed("no served scale"))
      )
      assertEquals(vm(noScale).status, Some("no served scale"))
      assertEquals(
        vm(
          s.copy(reports =
            s.reports.updated((scale, true), ReportAnswer.Failed("server refused overall"))
          )
        ).status,
        Some("server refused overall")
      )
      val report  = s.reports((scale, true)).asInstanceOf[ReportAnswer.Answered].report
      val changed = report.copy(participants =
        report.participants.map(p => if p.participant == "P01" then p.copy(value = None) else p)
      )
      val missing =
        vm(s.copy(reports = s.reports.updated((scale, true), ReportAnswer.Answered(changed))))
      assertEquals(missing.rows.head.value, "no value")
      val noCounts = s.copy(summary =
        s.answered.map(r =>
          SummaryAnswer.Answered(
            r.copy(participants = r.participants.filterNot(_.participant == "P01"))
          )
        )
      )
      assert(vm(noCounts).rows.isEmpty)
      assert(vm(noCounts).status.exists(_.contains("P01")))
    }
  }
  test(
    "wrong run, reporting, scale, duplicate participant or row ref refuses the entire context"
  ) {
    loaded.map { s =>
      val report = s.reports((scale, true)).asInstanceOf[ReportAnswer.Answered].report
      val cases  = Vector(
        report.copy(run = StoryMoments.run8),
        report.copy(scale = 0),
        report.copy(reporting = s.spec.get.id),
        report.copy(participants =
          report.participants :+ report.participants.find(_.role == ReportRole.Difference).get
        ),
        report.copy(participants =
          report.participants.map(p =>
            p.copy(ref =
              StudioRef.QueryTally(
                StoryMoments.run7,
                eyes4s.studio.core.selection.QueryCount.Requested
              )
            )
          )
        )
      )
      cases.foreach { report =>
        val refused =
          vm(s.copy(reports = s.reports.updated((scale, true), ReportAnswer.Answered(report))))
        assert(refused.rows.isEmpty)
        assert(refused.status.exists(_.contains("inconsistent")))
      }
    }
  }
  test("an old report reply cannot repopulate a newly shown run") {
    loaded.map { s =>
      val next  = s.copy(run = Some(StoryMoments.run8), summary = None, reports = Map.empty)
      val old   = s.reports((scale, true))
      val reply = CompareSummary
        .update(
          next,
          SummaryIntent.ReportRead(
            StoryMoments.run7,
            CompareSummary.overall(s.spec.get).get,
            scale,
            true,
            old
          )
        )
        ._1
      assertEquals(reply, next)
      assert(vm(reply).rows.isEmpty)
    }
  }
