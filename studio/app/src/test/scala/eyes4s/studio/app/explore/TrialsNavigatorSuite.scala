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

import eyes4s.studio.app.geometry.Loading
import eyes4s.studio.app.nav.Place
import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.core.assets.AssetRegistry
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.core.fixture.{GoldenAssets, MockStudy, StoryMoment, StoryMoments}
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.selection.StudioRef

import scala.concurrent.{ExecutionContext, Future}

/** The trials navigator, headless (ticket S6.1; Explore.dc.html, left), on
  * the fake backend's ledger of r3 at t2 and the golden asset registry:
  * P16 has one trial quarantined, P17 ret_09 and one P18 trial are absent,
  * and P17's enc_03 (fixation 6 is on Explore's trail) is selected.
  */
class TrialsNavigatorSuite extends munit.FunSuite:
  import StoryMoments.r3

  private given ExecutionContext = ExecutionContext.global

  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private def model: AppModel = StoryModels.t2Explore

  private val p17ret09 = MockStudy.key("P17", "ret_09")

  /** The fake's ledger of r3 at t2. */
  private def ledger: Future[Vector[LedgerEntry]] =
    for
      session <- HeadlessSession.open(StoryMoment.T2)
      entries <- session.wholeLedger(r3)
      _       <- session.close
    yield ok(entries)

  private def displays(m: AppModel): AssetRegistry =
    ok(GoldenAssets.registry(m.document.dataset(r3).get))

  /** The navigator synced to `m` with the ledger and displays read. */
  private def loaded(m: AppModel, entries: Vector[LedgerEntry]): TrialsNavigator =
    val (synced, effects) = TrialsNavigator.sync(TrialsNavigator.empty, m)
    assertEquals(
      effects,
      Vector(
        NavigatorEffect.RequestEntries(r3, 1),
        NavigatorEffect.RequestDisplays(m.document.dataset(r3).get, 1)
      )
    )
    Vector(
      NavigatorIntent.EntriesRead(r3, 1, Right(entries)),
      NavigatorIntent.DisplaysRead(r3, 1, Right(displays(m)))
    ).foldLeft(synced)((n, i) => TrialsNavigator.update(n, m, i)._1)

  private def row(vm: NavigatorPaneVM, label: String): NavigatorRowVM =
    vm.rows.find(_.label == label).getOrElse(fail(s"no row $label in ${vm.rows.map(_.label)}"))

  private def header(vm: NavigatorPaneVM, p: String) =
    vm.rows.find(r => r.kind == NavigatorRowKind.Participant && r.label == p).get

  // ---------------------------------------------------------------------------

  test("fixture statuses: P16 1 quarantined, P17 ret_09 absent, P18 1 absent") {
    ledger.map { entries =>
      val vm = TrialsNavigatorVM.trials(loaded(model, entries), model)
      assertEquals(vm.title, "Trials")
      assertEquals(vm.list, "Trials of r3")
      assertEquals(header(vm, "P16").detail, "40 · 1 quar.")
      assertEquals(header(vm, "P17").detail, "40 · 1 absent")
      assertEquals(header(vm, "P18").detail, "40 · 1 absent")
      // P17 holds the selected trial: it is open, the others closed.
      assertEquals(
        vm.rows.filter(_.kind == NavigatorRowKind.Participant).map(r => r.label -> r.open),
        entries.map(_.trial.participant).distinct.map(p => p -> Some(p == "P17"))
      )
      val p17 = vm.rows.dropWhile(_.label != "P17").drop(1).takeWhile(_.depth > 0)
      assertEquals(
        p17.filter(_.kind == NavigatorRowKind.Phase).map(r => (r.label, r.open)),
        Vector(
          ("Encoding · 20 · image", Some(true)),
          ("Retrieval · 20 · blank + cross", Some(false))
        )
      )
      // Encoding is open on the selected enc_03; Retrieval is the run of its
      // trials and the one not admitted, ret_09.
      val enc03 = p17.find(_.label == "enc_03").get
      assertEquals(
        (enc03.item, enc03.selected, enc03.glyph),
        ("beach-042", true, Some(TrialGlyph.Image))
      )
      assertEquals(enc03.ref, Some(StudioRef.Trial(StoryModels.p17enc03)))
      val retrieval = p17.dropWhile(_.label != "Retrieval · 20 · blank + cross").drop(1)
      assertEquals(
        retrieval.map(r => (r.kind, r.label, r.item, r.detail, r.glyph, r.warn)),
        Vector(
          (
            NavigatorRowKind.Range,
            "ret_01–20",
            "19 admitted",
            "",
            Some(TrialGlyph.BlankWithCross),
            false
          ),
          (
            NavigatorRowKind.Trial,
            "ret_09",
            "market-066",
            "absent · no records",
            Some(TrialGlyph.NotAdmitted),
            true
          )
        )
      )
      assertEquals(
        retrieval(1).accessible,
        "ret_09 market-066, absent: in trials.csv, no fixation records"
      )
      assertEquals(retrieval(1).ref, Some(StudioRef.Trial(p17ret09)))
      assertEquals(
        vm.legend.map(_.label),
        Vector("Image", "Blank + cross", "Image missing", "Not admitted")
      )
      assertEquals(vm.footer, "Explore is view-only. Nothing here changes an analysis.")
      assertEquals((vm.note, vm.retry), (None, None))
    }
  }

  test("every trial of the ledger is listed exactly once, with its own status") {
    ledger.map { entries =>
      val nav = loaded(model, entries)
      // Open everything: the tree then lists each ledger entry once.
      val groups =
        entries.map(_.trial.participant).distinct.map(NavigatorGroup.Participant(_)) ++
          entries.map(e => NavigatorGroup.PhaseOf(e.trial.participant, e.trial.phase)).distinct
      val all    = nav.copy(toggled = groups.map(_ -> true).toMap)
      val vm     = TrialsNavigatorVM.trials(all, model)
      val trials = vm.rows.filter(_.kind == NavigatorRowKind.Trial)
      assertEquals(trials.map(_.ref), entries.map(e => Some(StudioRef.Trial(e.trial))))
      val reg = displays(model)
      trials.zip(entries).foreach { (r, e) =>
        assertEquals(r.detail, TrialsNavigatorVM.status(e, Some(reg)).getOrElse(""), e)
        assertEquals(r.warn, r.detail.nonEmpty, e)
      }
      // The quarantined and missing images are among them.
      assertEquals(
        trials.count(_.detail.startsWith("quarantined")) +
          trials.count(_.detail == "no-fixations"),
        entries.count(e =>
          e.disposition match
            case TrialDisposition.Quarantined(_) | TrialDisposition.NoFixations => true
            case _                                                              => false
        )
      )
      assertEquals(
        trials.filter(_.detail.contains("image missing")).map(_.ref),
        entries
          .filter(e => reg.display(e.trial).exists(_.isMissing))
          .map(e => Some(StudioRef.Trial(e.trial)))
      )
      assert(trials.exists(_.detail.contains("image missing")))
    }
  }

  test("a group opens and closes by hand; activating a trial explores it") {
    ledger.map { entries =>
      val nav    = loaded(model, entries)
      val p16    = NavigatorGroup.Participant("P16")
      val opened = TrialsNavigator.update(nav, model, NavigatorIntent.Toggle(p16))._1
      val vm     = TrialsNavigatorVM.trials(opened, model)
      assertEquals(header(vm, "P16").open, Some(true))
      val p17    = NavigatorGroup.Participant("P17")
      val closed = TrialsNavigator.update(opened, model, NavigatorIntent.Toggle(p17))._1
      assertEquals(header(TrialsNavigatorVM.trials(closed, model), "P17").open, Some(false))
      assertEquals(
        header(TrialsNavigatorVM.trials(closed, model), "P17").accessible,
        "P17, 40 · 1 absent, collapsed"
      )
      // The collapsed retrieval run opens to every trial.
      val ret   = NavigatorGroup.PhaseOf("P17", Phase.Retrieval)
      val range =
        TrialsNavigatorVM.trials(nav, model).rows.find(_.kind == NavigatorRowKind.Range)
      assertEquals(range.flatMap(_.activate), Some(NavigatorIntent.Toggle(ret)))
      val all = TrialsNavigator.update(nav, model, NavigatorIntent.Toggle(ret))._1
      assertEquals(
        TrialsNavigatorVM
          .trials(all, model)
          .rows
          .count(r => r.kind == NavigatorRowKind.Trial && r.label.startsWith("ret_")),
        20
      )
      val (_, effects) = TrialsNavigator.update(nav, model, NavigatorIntent.OpenTrial(p17ret09))
      assertEquals(
        effects,
        Vector(NavigatorEffect.App(Intent.Explain(Place.At(StudioRef.Trial(p17ret09)))))
      )
      val explored =
        AppModel.update(model, Intent.Explain(Place.At(StudioRef.Trial(p17ret09))))._1
      assertEquals(explored.perspective, Perspective.Explore)
      assertEquals(TrialsNavigator.selected(explored), Some(p17ret09))
      // The selected trial's phase opens.
      val now = TrialsNavigatorVM.trials(nav, explored)
      assert(now.rows.exists(r => r.label == "ret_09" && r.selected), now.rows)
      assertEquals(now.rows.count(_.kind == NavigatorRowKind.Range), 1)
    }
  }

  test("the filter lists matching trials under open groups") {
    ledger.map { entries =>
      val nav = TrialsNavigator
        .update(loaded(model, entries), model, NavigatorIntent.Filter("beach-042"))
        ._1
      val vm     = TrialsNavigatorVM.trials(nav, model)
      val trials = vm.rows.filter(_.kind == NavigatorRowKind.Trial)
      assert(trials.nonEmpty)
      assert(trials.forall(_.item == "beach-042"), trials)
      assertEquals(trials.size, entries.count(_.item == "beach-042"))
      assert(vm.rows.forall(_.open.forall(identity)), vm.rows)
      assertEquals(vm.rows.count(_.kind == NavigatorRowKind.Range), 0)
      val p17 = header(vm, "P17")
      assertEquals(
        p17.detail,
        s"${entries.count(e => e.item == "beach-042" && e.trial.participant == "P17")} match"
      )
      val byTrial = TrialsNavigator.update(nav, model, NavigatorIntent.Filter("RET_09"))._1
      val rows    =
        TrialsNavigatorVM.trials(byTrial, model).rows.filter(_.kind == NavigatorRowKind.Trial)
      assertEquals(rows.map(_.label).distinct, Vector("ret_09"))
      val nothing = TrialsNavigator.update(nav, model, NavigatorIntent.Filter("zzz"))._1
      val none    = TrialsNavigatorVM.trials(nothing, model)
      assertEquals((none.rows, none.note), (Vector.empty, Some("No trials match “zzz”.")))
    }
  }

  test("the Items pane lists each match item and opens to its trials") {
    ledger.map { entries =>
      val nav = TrialsNavigator
        .update(loaded(model, entries), model, NavigatorIntent.FilterItems("beach"))
        ._1
      val vm = TrialsNavigatorVM.items(nav, model)
      assertEquals(vm.title, "Items")
      assert(
        vm.rows.forall(r => r.kind == NavigatorRowKind.Item && r.label.contains("beach")),
        vm.rows
      )
      val beach = row(vm, "beach-042")
      val count = entries.count(_.item == "beach-042")
      assertEquals(beach.detail, s"$count trials")
      val opened = TrialsNavigator
        .update(nav, model, NavigatorIntent.Toggle(NavigatorGroup.Item("beach-042")))
        ._1
      val trials =
        TrialsNavigatorVM.items(opened, model).rows.filter(_.kind == NavigatorRowKind.Trial)
      assertEquals(trials.size, count)
      assert(trials.exists(r => r.label == "P17 · enc_03" && r.selected), trials)
      // A missing image is told on its item.
      val all     = TrialsNavigatorVM.items(loaded(model, entries), model)
      val missing = displays(model).missing.map(_.file.value.stripSuffix(".png"))
      assertEquals(missing, Vector("forest-044", "kitchen-081"))
      missing.foreach(item =>
        assert(
          row(all, item).detail.contains("image missing") && row(all, item).warn,
          row(all, item)
        )
      )
    }
  }

  test("reading: waiting, a failed read with Retry, and answers to an earlier ask ignored") {
    ledger.map { entries =>
      val (synced, _) = TrialsNavigator.sync(TrialsNavigator.empty, model)
      val waiting     = TrialsNavigatorVM.trials(synced, model)
      assertEquals(
        (waiting.rows, waiting.note),
        (Vector.empty, Some("Reading the trials of r3…"))
      )
      val failed = TrialsNavigator
        .update(
          synced,
          model,
          NavigatorIntent.EntriesRead(r3, 1, Left("the backend timed out"))
        )
        ._1
      val vm = TrialsNavigatorVM.trials(failed, model)
      assertEquals(vm.note, Some("The trials of r3 are not available: the backend timed out"))
      assertEquals(vm.retry, Some("Retry"))
      assertEquals(
        TrialsNavigatorVM.focusStops(vm).map(_.render),
        Vector(
          "text-field: Filter participant, trial, item",
          "list: Trials of r3",
          "button: Retry"
        )
      )
      val (retried, effects) = TrialsNavigator.update(failed, model, NavigatorIntent.Retry)
      assertEquals(
        effects,
        Vector(
          NavigatorEffect.RequestEntries(r3, 2),
          NavigatorEffect.RequestDisplays(model.document.dataset(r3).get, 2)
        )
      )
      // The first ask's late answer is not the second's.
      val late = TrialsNavigator
        .update(retried, model, NavigatorIntent.EntriesRead(r3, 1, Right(entries)))
        ._1
      assertEquals(late, retried)
      val read = TrialsNavigator
        .update(retried, model, NavigatorIntent.EntriesRead(r3, 2, Right(entries)))
        ._1
      assertEquals(read.entries, Loading.Ready(entries))
      // Without displays the trials are still listed, without glyphs but the
      // not-admitted one, and the failure is told.
      val noDisplays = TrialsNavigator
        .update(read, model, NavigatorIntent.DisplaysRead(r3, 2, Left("no registry")))
        ._1
      val bare = TrialsNavigatorVM.trials(noDisplays, model)
      assertEquals(bare.note, Some("The displays of r3 are not available: no registry"))
      assertEquals(header(bare, "P17").detail, "40 · 1 absent")
      assertEquals(bare.legend.map(_.glyph), Vector(TrialGlyph.NotAdmitted))
      // Syncing the same revision asks nothing.
      assertEquals(TrialsNavigator.sync(read, model), (read, Vector.empty))
    }
  }

  test("the shown revision is the latest admitted one; none before an admission") {
    assertEquals(TrialsNavigator.shown(model).map(_.id), Some(r3))
    assertEquals(TrialsNavigator.shown(StoryModels.t1Data).map(_.id), Some(StoryMoments.r2))
    val first = StoryModels.firstRun
    assertEquals(TrialsNavigator.sync(TrialsNavigator.empty, first)._2, Vector.empty)
    assertEquals(
      TrialsNavigatorVM.trials(TrialsNavigator.empty, first).note,
      Some("No admitted dataset revision yet: admit one in Data.")
    )
  }

  test("a run of trials names its last by what differs") {
    def k(trial: String) = MockStudy.key("P17", trial)
    assertEquals(TrialsNavigatorVM.range(k("ret_01"), k("ret_20")), "ret_01–20")
    assertEquals(TrialsNavigatorVM.range(k("a1"), k("b2")), "a1–b2")
  }
