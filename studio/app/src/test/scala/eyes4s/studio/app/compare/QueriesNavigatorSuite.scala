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
import eyes4s.studio.app.text.Format
import eyes4s.studio.app.{AppModel, StoryModels}
import eyes4s.studio.core.backend.PageRequest
import eyes4s.studio.core.fixture.{MockStudy, StoryMoment, StoryMoments}
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.selection.{QueryCount, ScaleIndex, StudioRef}

import scala.concurrent.{ExecutionContext, Future}

/** The Queries and Items navigators headlessly (ticket S8.1; Main.dc.html,
  * left): run 7's count strip is FIXTURE.md's 480 = 454 + 3 + 9 + 14 with
  * by design n/a, participants carry their contributing count and mean D,
  * each query its D at 2° with a zero-centred bar or its status, the
  * filter narrows by participant or item, and opening a query walks the
  * trail to it.
  */
class QueriesNavigatorSuite extends munit.FunSuite:
  import StoryModels.*

  private given ExecutionContext = ExecutionContext.global

  private val run7 = StoryMoments.run7

  private def right[E, A](e: Either[E, A]): A =
    e.fold(x => fail(s"unexpected Left: $x"), identity)

  private def ascii(s: String): String = s.replace(Format.Minus, "-")

  private def loaded(m: AppModel): Future[CompareSummary] =
    HeadlessSession.open(StoryMoment.T2).flatMap { session =>
      val page = right(PageRequest.first(PageRequest.MaximumSize))
      (for
        r0 <- session.result(run7)
        q0 <- session.queries(run7, page)
        r                 = right(r0)
        q                 = right(q0).rows
        (s0, _)           = CompareSummary.sync(CompareSummary.empty, m)
        (s1, reportReads) = CompareSummary.update(
          s0,
          SummaryIntent.SummaryRead(run7, SummaryAnswer.Answered(r))
        )
        reports <- Future.sequence(reportReads.collect {
          case SummaryEffect.RequestReport(_, spec, scale, whole) =>
            session
              .report(run7, spec, scale.value)
              .map(answer =>
                (
                  spec,
                  scale,
                  whole,
                  answer.fold(ReportAnswer.Refused(_), ReportAnswer.Answered(_))
                )
              )
        })
      yield
        val withReports = reports.foldLeft(s1) { case (state, (spec, scale, whole, answer)) =>
          CompareSummary
            .update(state, SummaryIntent.ReportRead(run7, spec, scale, whole, answer))
            ._1
        }
        val atTwoDegrees = CompareSummary
          .update(withReports, SummaryIntent.ChooseScale(right(ScaleIndex.of(2))))
          ._1
        CompareSummary
          .update(atTwoDegrees, SummaryIntent.QueriesRead(run7, QueriesAnswer.Answered(q)))
          ._1
      ).transformWith(result => session.close.transform(_ => result))
    }

  test("the count strip is FIXTURE.md's: 480 = 454 + 3 + 9 + 14, by design n/a") {
    loaded(t2Compare).map { s =>
      val vm = QueriesNavigator.vm(QueriesNavigator.initial, s, Vector.empty)
      assertEquals(
        vm.strip.map(l => (l.label, l.count)),
        Vector(
          ("Query contrasts requested", "480"),
          ("Contributing", "454"),
          ("Failed (empty map)", "3"),
          ("No match · query not admitted", "9 · 14"),
          ("By design (n/a for this preset)", "n/a")
        )
      )
      assertEquals(vm.strip.map(_.failure), Vector(false, false, true, false, false))
      val c = s.answered.get.contrasts
      assertEquals(c.requested, c.contributing + c.failed + c.noMatch + c.queryNotAdmitted)
    }
  }

  test(
    "participants carry their contributing count and mean D; queries their D at 2° or status"
  ) {
    loaded(t2Compare).map { s =>
      val vm = QueriesNavigator.vm(QueriesNavigator.initial, s, Vector.empty)
      assertEquals(vm.dColumn, "D · 2°")
      assertEquals(vm.groups.size, 24)
      assertEquals(vm.groups.map(_.entries.size).sum, 480)
      val p17 = vm.groups.find(_.key == "participant:P17").getOrElse(fail("no P17"))
      assertEquals(p17.summary, "19 of 20 · +0.38")
      val p05 = vm.groups.find(_.key == "participant:P05").get
      assertEquals(ascii(p05.summary), "17 of 20 · -0.08")
      def entry(trial: String) = p17.entries.find(_.trial == trial).getOrElse(fail(trial))
      val ret07                = entry("ret_07")
      assertEquals((ret07.item, ret07.said, ret07.scored), ("beach-042", "+0.38", true))
      // Zero-centred: the bar runs from zero to D over the run's largest |D|.
      val ends   = vm.groups.flatMap(_.entries).flatMap(_.bar).map(_.fromZeroTo)
      val extent = ends.map(math.abs).max
      assertEqualsDouble(extent, 1.0, 1e-12)
      assert(ends.exists(_ < 0.0), "some D is negative")
      assert(ret07.bar.exists(b => b.fromZeroTo > 0.0 && b.fromZeroTo < 1.0), ret07.bar)
      val ret06 = entry("ret_06")
      assertEqualsDouble(ret06.bar.get.fromZeroTo / ret07.bar.get.fromZeroTo, 0.50 / 0.38, 0.03)
      // A query with no score says why, and has no bar.
      val ret09 = entry("ret_09")
      assertEquals((ret09.said, ret09.scored, ret09.bar), ("not admitted", false, None))
      // P05's failed queries.
      assertEquals(p05.entries.count(_.said == "failed"), 3)
      // Refs are the queries' contrasts at 2°.
      assertEquals(ret07.ref, query)
    }
  }

  test("the selection opens its group; the filter narrows by participant or item") {
    loaded(t2Compare).map { s =>
      val shut = QueriesNavigator.vm(QueriesNavigator.initial, s, Vector.empty)
      assert(shut.groups.forall(!_.open))
      val sel = QueriesNavigator.vm(QueriesNavigator.initial, s, Vector(query))
      assertEquals(sel.groups.filter(_.open).map(_.key), Vector("participant:P17"))
      assert(
        sel.groups
          .find(_.key == "participant:P17")
          .get
          .entries
          .find(_.trial == "ret_07")
          .get
          .selected
      )
      val opened = QueriesNavigator.toggle(QueriesNavigator.initial, "participant:P02", false)
      assertEquals(
        QueriesNavigator.vm(opened, s, Vector.empty).groups.filter(_.open).map(_.key),
        Vector("participant:P02")
      )
      val reclosed = QueriesNavigator.toggle(opened, "participant:P02", true)
      assert(QueriesNavigator.vm(reclosed, s, Vector.empty).groups.forall(!_.open))
      // The selection's group closes when the user closes it.
      val p17shut = QueriesNavigator.toggle(QueriesNavigator.initial, "participant:P17", true)
      assert(QueriesNavigator.vm(p17shut, s, Vector(query)).groups.forall(!_.open))
      // Filter by item.
      val beach = QueriesNavigator.vm(
        QueriesNavigator.filter(QueriesNavigator.initial, "BEACH-042"),
        s,
        Vector.empty
      )
      val es = beach.groups.flatMap(_.entries)
      assert(es.nonEmpty && es.forall(_.item == "beach-042"), es.map(_.item))
      assertEquals(beach.items.map(_.label), Vector("beach-042"))
      // Filter by participant.
      val p17 = QueriesNavigator.vm(
        QueriesNavigator.filter(QueriesNavigator.initial, "p17"),
        s,
        Vector.empty
      )
      assertEquals(p17.groups.map(_.key), Vector("participant:P17"))
      assertEquals(p17.groups.head.entries.size, 20)
      // Items, sorted, each with its queries.
      val all = QueriesNavigator.vm(QueriesNavigator.initial, s, Vector.empty)
      assertEquals(all.items.map(_.label), all.items.map(_.label).sorted)
      assertEquals(all.items.map(_.entries.size).sum, 480)
      // The count strip and the bars' scale are the run's, whatever the filter.
      assertEquals(beach.strip, all.strip)
      val bars =
        (vm: QueriesNavigatorVM) => vm.groups.flatMap(_.entries).map(e => e.ref -> e.bar).toMap
      val allBars = bars(all)
      bars(beach).foreach((ref, bar) => assertEquals(bar, allBars(ref)))
    }
  }

  test("opening a query walks the trail to it, keeping the spec and the query's group") {
    loaded(t2Compare).map { s =>
      val vm    = QueriesNavigator.vm(QueriesNavigator.initial, s, Vector.empty)
      val ret07 =
        vm.groups.find(_.key == "participant:P17").get.entries.find(_.trial == "ret_07").get
      val m     = AppModel.run(t2Compare, Vector(ret07.open))._1
      val trail = m.location.trail
      assertEquals(trail.last, Place.At(query))
      assert(trail.contains(Place.Summary(reporting)), trail)
      assert(trail.contains(Place.Group(reporting, remembered)), trail)
    }
  }

  test("with no run, the navigator says so; while reading, it says that") {
    val none = QueriesNavigator.vm(QueriesNavigator.initial, CompareSummary.empty, Vector.empty)
    assertEquals(none.empty, Some("No run yet — Open Analysis ⌘3"))
    assertEquals(none.strip, Vector.empty)
    val (reading, _) = CompareSummary.sync(CompareSummary.empty, t2Compare)
    assertEquals(
      QueriesNavigator.vm(QueriesNavigator.initial, reading, Vector.empty).empty,
      Some("Reading run 7…")
    )
    // The study's queries are FIXTURE.md's 480.
    assertEquals(right(MockStudy.load).queries.size, 480)
  }

  test("the row cursor walks headers and open queries; Enter opens a query or a group") {
    loaded(t2Compare).map { s =>
      import NavigatorKey.*
      val Q                           = NavigatorKind.Queries
      def vmOf(nav: QueriesNavigator) = QueriesNavigator.vm(nav, s, Vector(query))
      def press(nav: QueriesNavigator, k: NavigatorKey) =
        QueriesNavigator.key(nav, vmOf(nav), Q, k)
      def cursorOf(nav: QueriesNavigator) = nav.cursors.get(Q)
      val start                           = QueriesNavigator.initial
      // No cursor: Down starts at the first header; Up stops there.
      val (first, none) = press(start, Down)
      assertEquals(
        (cursorOf(first), none),
        (Some(NavigatorRow.Header("participant:P01")), None)
      )
      assertEquals(cursorOf(press(first, Up)._1), Some(NavigatorRow.Header("participant:P01")))
      // Rows: 24 headers and P17's 20 open queries; End goes to the last.
      assertEquals(vmOf(first).rows(Q).size, 44)
      assertEquals(
        cursorOf(press(first, Last)._1),
        Some(NavigatorRow.Header("participant:P24"))
      )
      assertEquals(
        cursorOf(press(press(first, Last)._1, Down)._1),
        Some(NavigatorRow.Header("participant:P24"))
      )
      // Right opens P01; Down then enters its first query; Enter opens it.
      val (p01open, _) = press(first, Expand)
      assert(vmOf(p01open).groups.head.open)
      assert(vmOf(p01open).groups.head.cursor)
      val (onQuery, _) = press(p01open, Down)
      val ref01        = vmOf(p01open).groups.head.entries.head.ref
      assertEquals(cursorOf(onQuery), Some(NavigatorRow.Query(ref01)))
      assert(vmOf(onQuery).groups.head.entries.head.cursor)
      assertEquals(
        press(onQuery, Activate)._2,
        Some(eyes4s.studio.app.Intent.Explain(Place.At(ref01)))
      )
      // Left on a query goes to its header; Left there closes it; Enter reopens.
      val (back, _) = press(onQuery, Collapse)
      assertEquals(cursorOf(back), Some(NavigatorRow.Header("participant:P01")))
      val (shut, _) = press(back, Collapse)
      assert(!vmOf(shut).groups.head.open)
      val (reopened, sent) = press(shut, Activate)
      assert(vmOf(reopened).groups.head.open && sent.isEmpty)
      // The Items navigator has its own cursor.
      assertEquals(
        QueriesNavigator
          .key(onQuery, vmOf(onQuery), NavigatorKind.Items, Down)
          ._1
          .cursors
          .get(NavigatorKind.Items),
        Some(NavigatorRow.Header(vmOf(onQuery).items.head.key))
      )
      assertEquals(cursorOf(onQuery), Some(NavigatorRow.Query(ref01)))
    }
  }

  test("the filter is the one control inside the navigator's stop, unless it is empty") {
    loaded(t2Compare).map { s =>
      val vm = QueriesNavigator.vm(QueriesNavigator.initial, s, Vector.empty)
      assertEquals(
        QueriesNavigator.focusStops(vm),
        Vector(
          eyes4s.studio.app.vm
            .FocusStop(eyes4s.studio.app.vm.A11yRole.TextField, "Filter participant or item")
        )
      )
      assertEquals(
        QueriesNavigator.focusStops(
          QueriesNavigator.vm(QueriesNavigator.initial, CompareSummary.empty, Vector.empty)
        ),
        Vector.empty
      )
    }
  }

  test("every number of the strip and the participant headers names its ref") {
    loaded(t2Compare).map { s =>
      val vm = QueriesNavigator.vm(QueriesNavigator.initial, s, Vector.empty)
      def tally(c: QueryCount): StudioRef = StudioRef.QueryTally(run7, c)
      assertEquals(
        vm.strip.map(_.refs),
        Vector(
          Vector(tally(QueryCount.Requested)),
          Vector(tally(QueryCount.Contributing)),
          Vector(tally(QueryCount.Failed)),
          Vector(tally(QueryCount.NoMatch), tally(QueryCount.QueryNotAdmitted)),
          // "n/a" is no number.
          Vector.empty
        )
      )
      // One ref per number shown: "9 · 14" is two.
      assertEquals(vm.strip.map(_.refs.size), Vector(1, 1, 1, 2, 0))
      val p17 = vm.groups.find(_.key == "participant:P17").get
      assertEquals(
        p17.summaryRef,
        Some(StudioRef.ParticipantSummary(run7, reporting, sigma2, None, "P17"))
      )
      assert(vm.groups.forall(_.summaryRef.isDefined))
      // A tally reads as a path, and lies under the requested queries.
      assertEquals(
        eyes4s.studio.app.text.SummaryText.tally(run7, QueryCount.Contributing),
        "Contributing · run 7"
      )
      assertEquals(tally(QueryCount.Failed).parent, Some(tally(QueryCount.Requested)))
      assertEquals(tally(QueryCount.Requested).parent, None)
      assert(tally(QueryCount.Failed).isAggregate)
    }
  }

  test("one filter: the view-model carries it for both navigators") {
    loaded(t2Compare).map { s =>
      val nav = QueriesNavigator.filter(QueriesNavigator.initial, "P17")
      val vm  = QueriesNavigator.vm(nav, s, Vector.empty)
      assertEquals(vm.filter, "P17")
      // The Items navigator shows the same filtered queries and the same text.
      assertEquals(
        vm.items.flatMap(_.entries).map(_.ref).toSet,
        vm.groups.flatMap(_.entries).map(_.ref).toSet
      )
      assertEquals(QueriesNavigator.vm(QueriesNavigator.initial, s, Vector.empty).filter, "")
    }
  }

  test("a query picked at another σ is still selected, and opens its group") {
    loaded(t2Compare).map { s =>
      val at4 = StudioRef.QueryContrast(run7, right(ScaleIndex.of(3)), p17ret07)
      val vm  = QueriesNavigator.vm(QueriesNavigator.initial, s, Vector(at4))
      val p17 = vm.groups.find(_.key == "participant:P17").get
      assert(p17.open)
      assertEquals(p17.entries.filter(_.selected).map(_.trial), Vector("ret_07"))
      // Another run's contrast of the same query is not this query.
      val other = StudioRef.QueryContrast(eyes4s.studio.core.backend.RunId(5), sigma2, p17ret07)
      assert(
        !QueriesNavigator.vm(QueriesNavigator.initial, s, Vector(other)).groups.exists(_.open)
      )
    }
  }

  test("the row cursor is announced: a header with its summary, or a query with its D") {
    loaded(t2Compare).map { s =>
      import NavigatorKey.*
      val vm0 = QueriesNavigator.vm(QueriesNavigator.initial, s, Vector.empty)
      assertEquals(vm0.cursorText(NavigatorKind.Queries), None)
      val (onP01, _) =
        QueriesNavigator.key(QueriesNavigator.initial, vm0, NavigatorKind.Queries, Down)
      val vm1 = QueriesNavigator.vm(onP01, s, Vector.empty)
      val p01 = vm1.groups.head
      assertEquals(
        vm1.cursorText(NavigatorKind.Queries),
        Some(s"P01: ${p01.summary}, closed")
      )
      val (opened, _)  = QueriesNavigator.key(onP01, vm1, NavigatorKind.Queries, Activate)
      val vm2          = QueriesNavigator.vm(opened, s, Vector.empty)
      val (onQuery, _) = QueriesNavigator.key(opened, vm2, NavigatorKind.Queries, Down)
      val vm3          = QueriesNavigator.vm(onQuery, s, Vector.empty)
      val first        = vm3.groups.head.entries.head
      assertEquals(
        vm3.cursorText(NavigatorKind.Queries),
        Some(s"P01: ${first.trial}, ${first.item}, D ${first.said}")
      )
      // The other navigator's cursor is its own.
      assertEquals(vm3.cursorText(NavigatorKind.Items), None)
    }
  }

  test("answered without a σ of participant means: the navigator says why, not 'Reading'") {
    loaded(t2Compare).map { s =>
      val noSpec =
        QueriesNavigator.vm(
          QueriesNavigator.initial,
          s.copy(reporting = None, scale = None, spec = None, reports = Map.empty),
          Vector.empty
        )
      assertEquals(
        noSpec.empty,
        Some(
          "Run 7's queries are read, but the document has no reporting spec to show them under."
        )
      )
      // The strip is still the run's.
      assertEquals(noSpec.strip.map(_.count).head, "480")
      // A participant listed twice: eyes4s's means check refuses every σ.
      val r      = s.answered.get
      val twice  = r.copy(participants = r.participants :+ r.participants.head)
      val broken = s.copy(
        summary = Some(SummaryAnswer.Answered(twice)),
        scale = None,
        reports = Map.empty
      )
      assertEquals(broken.shown, None)
      val refused = QueriesNavigator.vm(QueriesNavigator.initial, broken, Vector.empty)
      assert(
        refused.empty.exists(
          _.startsWith("Run 7's queries are read, but no σ has its participant means: ")
        ),
        refused.empty
      )
      assert(!refused.empty.exists(_.contains("Reading")))
    }
  }

  test("a participant header's mean D is at the shown σ") {
    loaded(t2Compare).map { s =>
      val at1 = s.copy(scale = Some(right(ScaleIndex.of(1))))
      val vm  = QueriesNavigator.vm(QueriesNavigator.initial, at1, Vector.empty)
      val p17 = vm.groups.find(_.key == "participant:P17").get
      val ps  = s.answered.get.participants.find(_.participant == "P17").get
      val d1  = Format.signed(ps.all.dByScale(1), 2)
      assert(p17.summary.endsWith(d1), (p17.summary, d1))
      assertNotEquals(d1, Format.signed(ps.all.d, 2))
    }
  }

  test("the selection includes the Compare trail; a new run resets open, closed and cursors") {
    val sel = QueriesNavigator.selection(t2Compare)
    assert(sel.contains(query), sel)
    assert(t2Compare.selection.selected.forall(sel.contains))
    val used = QueriesNavigator
      .toggle(QueriesNavigator.filter(QueriesNavigator.initial, "P17"), "participant:P17", true)
      .copy(cursors = Map(NavigatorKind.Queries -> NavigatorRow.Header("participant:P17")))
    val on7 = QueriesNavigator.follow(used, Some(run7))
    assertEquals(QueriesNavigator.follow(on7, Some(run7)), on7)
    val on8 = QueriesNavigator.follow(on7, Some(eyes4s.studio.core.backend.RunId(8)))
    assertEquals(
      on8,
      QueriesNavigator.initial
        .copy(run = Some(eyes4s.studio.core.backend.RunId(8)), filter = "P17")
    )
  }
