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

package eyes4s.studio.app.admission

import cats.data.NonEmptyVector
import eyes4s.studio.app.geometry.Loading
import eyes4s.studio.app.vm.Shell
import eyes4s.studio.app.{AppEffect, AppModel, Intent, StoryModels}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.{InventoryScenario, StoryMoment, StoryMoments}
import eyes4s.studio.core.freshness.{RunStanding, StaleReason}
import eyes4s.studio.core.headless.HeadlessSession
import eyes4s.studio.core.selection.{InventoryKind, StudioRef, TallyRegion}

import scala.concurrent.{ExecutionContext, Future}

/** The admission ledger's behaviour, headless (ticket S5.6; Data.dc.html,
  * admission), on the fake backend's admission and ledger of r3: the
  * fixture's 960 = 937 + 17 (by cause) + 6; every count opens exactly the
  * trials the ledger lists with its disposition; Require complete refuses
  * while trials are quarantined; admitting r3 marks run 5 (on r2) stale.
  */
class AdmissionLedgerSuite extends munit.FunSuite:
  import StoryMoments.{r2, r3, run5}

  private given ExecutionContext = ExecutionContext.global

  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  /** t1: r3 is a pending re-import of r2, the ledger open on it. */
  private def t1: AppModel = StoryModels.t1Data

  /** The fake's answers for r3 at `moment`, after `prepare`. */
  private def served(
      moment: StoryMoment,
      prepare: HeadlessSession => Future[Unit] = _ => Future.unit
  ): Future[(AdmissionAnswer, Either[String, Vector[LedgerEntry]])] =
    for
      session <- HeadlessSession.open(moment)
      _       <- prepare(session)
      counts  <- session.admission(r3)
      ledger  <- session.wholeLedger(r3)
      _       <- session.close
    yield (
      counts.fold(AdmissionAnswer.Refused(_), AdmissionAnswer.Answered(_)),
      ledger.left.map(_.message)
    )

  /** The ledger synced to `model` with the backend's answers read. */
  private def loaded(
      model: AppModel,
      answers: (AdmissionAnswer, Either[String, Vector[LedgerEntry]])
  ): AdmissionLedger =
    val (synced, effects) = AdmissionLedger.sync(AdmissionLedger.empty, model)
    assertEquals(
      effects,
      Vector(LedgerEffect.RequestCounts(r3), LedgerEffect.RequestLedger(r3))
    )
    val (counts, entries) = answers
    Vector(LedgerIntent.CountsRead(r3, counts), LedgerIntent.LedgerRead(r3, entries))
      .foldLeft(synced)((l, i) => AdmissionLedger.update(l, model, i)._1)

  /** Apply the ledger's app intents, as the host does; the app's effects. */
  private def perform(
      model: AppModel,
      effects: Vector[LedgerEffect]
  ): (AppModel, Vector[AppEffect]) =
    effects.foldLeft((model, Vector.empty[AppEffect])) {
      case ((m, out), LedgerEffect.App(i)) =>
        val (next, more) = AppModel.update(m, i)
        (next, out ++ more)
      case (acc, _) => acc
    }

  private def rows(vm: AdmissionLedgerVM): Vector[(String, String)] =
    vm.rows.map(r => r.label -> r.value)

  private def entriesOf(ledger: AdmissionLedger): Vector[LedgerEntry] =
    ledger.entries.toOption.getOrElse(fail("the ledger was not read"))

  // ---------------------------------------------------------------------------

  test("fixture: 960 = 937 + 17 (by cause) + 6, each count traced to its ref") {
    served(StoryMoment.T1).map { answers =>
      val vm = AdmissionLedgerVM.of(loaded(t1, answers), t1)
      assertEquals(vm.title, "Admission")
      assertEquals(vm.header, "Counted from the trials.csv inventory")
      assertEquals(
        rows(vm),
        Vector(
          "Inventory"                                                         -> "960",
          "Admitted"                                                          -> "937",
          "Quarantined — whole trial held back when any record is invalid"    -> "17",
          "duplicate-ordinals"                                                -> "4",
          "no-fixations"                                                      -> "5",
          "overlap"                                                           -> "6",
          "rejected-records"                                                  -> "2",
          "Absent — in trials.csv, no fixation records at all"                -> "6",
          "Outside screen — excluded from maps and reported, not quarantined" -> "0"
        )
      )
      assertEquals(vm.equation, Some("937 + 17 + 6 = 960."))
      def count(kind: InventoryKind) = StudioRef.InventoryCount(r3, kind)
      assertEquals(
        vm.rows.map(_.ref),
        Vector(
          count(InventoryKind.Inventory),
          count(InventoryKind.Admitted),
          count(InventoryKind.Quarantined),
          count(InventoryKind.Cause("quarantine.duplicate-ordinals")),
          count(InventoryKind.NoFixations),
          count(InventoryKind.Cause("quarantine.overlap")),
          count(InventoryKind.Cause("quarantine.rejected-records")),
          count(InventoryKind.Absent),
          StudioRef.WindowTally(r3, TallyRegion.OutsideScreen)
        )
      )
      assertEquals(
        vm.rows.map(_.style),
        Vector(CountStyle.Total, CountStyle.Plain, CountStyle.Group) ++
          Vector.fill(4)(CountStyle.Cause) ++ Vector(CountStyle.Plain, CountStyle.Reported)
      )
      assert(vm.rows.forall(_.counted), vm.rows)
      assertEquals(vm.rows(3).accessible, "4 trials quarantined for duplicate-ordinals")
      // Outside screen is reported, with its records, never as a quarantine cause.
      assertEquals(vm.rows.last.detail, Some("0 records"))
      assertEquals(
        vm.definitions.map(d => d.term + d.text),
        Vector(
          "no-fixations: the trial has records but none is admissible.",
          "Absent: listed in the inventory, no records in fixations.csv."
        )
      )
      assertEquals(vm.countsSource, "Counts: eyes4s admission of r3.")
      assertEquals(vm.opened, None)
    }
  }

  test("every count opens exactly its trials, through the Data trail") {
    served(StoryMoment.T1).map { answers =>
      val ledger  = loaded(t1, answers)
      val entries = entriesOf(ledger)
      assertEquals(entries.size, 960)
      assertEquals(entries.map(_.trial).distinct.size, 960)
      val counted = AdmissionLedgerVM.of(ledger, t1).rows
      val opened  = counted.map { row =>
        val (_, effects) = AdmissionLedger.update(ledger, t1, LedgerIntent.Open(row.ref))
        val (model, _)   = perform(t1, effects)
        val vm           = AdmissionLedgerVM.of(ledger, model)
        val open         = vm.opened.getOrElse(fail(s"${row.label} opened nothing"))
        assertEquals(open.ref, row.ref)
        assert(vm.rows.find(_.ref == row.ref).exists(_.opened), row.label)
        // Exactly as many trials as the count, each once.
        assertEquals(open.rows.size.toString, row.value, row.label)
        assertEquals(open.rows.map(_.trial).distinct.size, open.rows.size, row.label)
        // Activating the open count again closes it.
        val (_, again) = AdmissionLedger.update(ledger, model, LedgerIntent.Open(row.ref))
        assertEquals(AdmissionLedgerVM.of(ledger, perform(model, again)._1).opened, None)
        row.ref -> open.rows.map(_.trial).toSet
      }.toMap
      def trials(kind: InventoryKind)           = opened(StudioRef.InventoryCount(r3, kind))
      def where(p: TrialDisposition => Boolean) =
        entries.filter(e => p(e.disposition)).map(_.trial).toSet
      // Each count's trials are the ledger's with its disposition, and no others.
      assertEquals(trials(InventoryKind.Inventory), entries.map(_.trial).toSet)
      assertEquals(trials(InventoryKind.Admitted), where(_ == TrialDisposition.Admitted))
      assertEquals(trials(InventoryKind.Absent), where(_ == TrialDisposition.Absent))
      assertEquals(trials(InventoryKind.NoFixations), where(_ == TrialDisposition.NoFixations))
      val causes = Vector("duplicate-ordinals", "overlap", "rejected-records").map { c =>
        val these = trials(InventoryKind.Cause(s"quarantine.$c"))
        assertEquals(
          these,
          where {
            case TrialDisposition.Quarantined(cause) => cause.code == s"quarantine.$c"
            case _                                   => false
          },
          c
        )
        these
      }
      // The causes and no-fixations partition the quarantined trials, and
      // admitted, quarantined and absent partition the inventory.
      val quarantined = trials(InventoryKind.Quarantined)
      assertEquals(causes.reduce(_ ++ _) ++ trials(InventoryKind.NoFixations), quarantined)
      assertEquals(causes.map(_.size).sum + trials(InventoryKind.NoFixations).size, 17)
      assertEquals(
        trials(InventoryKind.Admitted) ++ quarantined ++ trials(InventoryKind.Absent),
        trials(InventoryKind.Inventory)
      )
      assertEquals(opened(StudioRef.WindowTally(r3, TallyRegion.OutsideScreen)), Set.empty)
    }
  }

  test("an open count shows its trials and why, and each trial opens in Explore") {
    served(StoryMoment.T1).map { answers =>
      val ledger  = loaded(t1, answers)
      val overlap = StudioRef.InventoryCount(r3, InventoryKind.Cause("quarantine.overlap"))
      val model   =
        perform(t1, AdmissionLedger.update(ledger, t1, LedgerIntent.Open(overlap))._2)._1
      assertEquals(
        Shell.context(model).trail.map(_.label),
        Vector("Dataset r3 (draft)", "Admission", "Quarantined", "overlap")
      )
      val open = AdmissionLedgerVM.of(ledger, model).opened.get
      assertEquals(open.title, "Quarantined · overlap · 6 trials")
      assert(open.rows.forall(_.detail.startsWith("overlap: fixation")), open.rows)
      val first = open.rows.head
      assertEquals(first.ref, StudioRef.Trial(first.trial))
      val (_, explain) =
        AdmissionLedger.update(ledger, model, LedgerIntent.OpenTrial(first.trial))
      val explored = perform(model, explain)._1
      assertEquals(explored.perspective, Perspective.Explore)
      assertEquals(explored.location.trail.last, eyes4s.studio.app.nav.Place.At(first.ref))
      // Back returns to the open count; Close returns to the ledger.
      val back = AppModel.update(explored, Intent.Back)._1
      assertEquals(AdmissionLedger.opened(back, r3), Some(overlap))
      val closed = perform(back, AdmissionLedger.update(ledger, back, LedgerIntent.Close)._2)._1
      assertEquals(AdmissionLedger.opened(closed, r3), None)
      // Absent trials are the inventory's, with no records.
      val absent   = StudioRef.InventoryCount(r3, InventoryKind.Absent)
      val onAbsent =
        perform(t1, AdmissionLedger.update(ledger, t1, LedgerIntent.Open(absent))._2)._1
      val absentRows = AdmissionLedgerVM.of(ledger, onAbsent).opened.get.rows
      assertEquals(absentRows.size, 6)
      assert(absentRows.forall(_.detail == "in the inventory, no fixation records"), absentRows)
      assert(absentRows.exists(_.trial == MockStudyKeys.p17ret09), absentRows)
    }
  }

  test("Require complete refuses r3 while 17 trials are quarantined") {
    served(StoryMoment.T1).map { answers =>
      val ledger = loaded(t1, answers)
      val vm     = AdmissionLedgerVM.of(ledger, t1)
      assertEquals(ledger.decision, LedgerDecision.RequireComplete)
      assertEquals(
        vm.decisions.map(d => (d.label, d.note, d.selected)),
        Vector(
          (
            "Require complete",
            "Refuses r3 while 17 trials are quarantined (eyes4s admission). The 6 absent " +
              "trials are counted by the inventory join.",
            true
          ),
          (
            "Review exclusions",
            "Admits 937 trials; 17 quarantined and 6 absent are recorded with their causes in r3.",
            false
          )
        )
      )
      assertEquals(vm.admit, "Admit as r3")
      assertEquals(vm.canAdmit, false)
      val refusal = "Require complete refuses r3: 17 trials are quarantined. Review " +
        "exclusions admits 937 trials."
      assertEquals(vm.admitNote, Some(refusal))
      val (after, effects) = AdmissionLedger.update(ledger, t1, LedgerIntent.Admit)
      assertEquals(effects, Vector.empty)
      assertEquals(after.problem, Some(refusal))
      val review = AdmissionLedger
        .update(ledger, t1, LedgerIntent.ChooseDecision(LedgerDecision.ReviewExclusions))
        ._1
      assert(AdmissionLedgerVM.of(review, t1).canAdmit)
    }
  }

  test("admitting r3 creates the admitted revision and marks run 5 (r2) stale") {
    served(StoryMoment.T1).map { answers =>
      val review = AdmissionLedger
        .update(
          loaded(t1, answers),
          t1,
          LedgerIntent.ChooseDecision(LedgerDecision.ReviewExclusions)
        )
        ._1
      val before = AdmissionLedgerVM.of(review, t1)
      assertEquals(before.decisionTitle, "Admit dataset r3")
      assertEquals(
        before.consequence,
        Some("Admitting creates dataset r3. Run 5 stays on r2 and is marked stale.")
      )
      assertEquals(
        before.changes,
        Some(
          "Changes from r2: onset declared ms; Block → occurrence; 4 trials change status vs " +
            "r2 (overlap → admitted 3, admitted → no-fixations 1)."
        )
      )
      assertEquals(t1.freshness.standing(run5), Some(RunStanding.Current))
      // A pending r3 is first verified.
      val (asked, verify) = AdmissionLedger.update(review, t1, LedgerIntent.Admit)
      assertEquals(verify, Vector(LedgerEffect.App(Intent.Dispatch(Command.VerifyDataset(r3)))))
      assertEquals(asked.admitting, Some(r3))
      val (verifying, appEffects) = perform(t1, verify)
      val content                 = appEffects
        .collectFirst { case AppEffect.RequestAdmission(`r3`, c) => c }
        .getOrElse(fail(s"no admission request in $appEffects"))
      assertEquals(
        verifying.document.dataset(r3).map(_.decision),
        Some(AdmissionDecision.Verifying(content))
      )
      val synced = AdmissionLedger.sync(asked, verifying)._1
      assertEquals(synced.admitting, Some(r3))
      assertEquals(AdmissionLedgerVM.of(synced, verifying).canAdmit, false)
      assertEquals(
        AdmissionLedgerVM.of(synced, verifying).admitNote,
        Some("Verifying r3 with eyes4s…")
      )
      // eyes4s's answer for exactly the verified content admits it.
      val (answered, admit) =
        AdmissionLedger.update(
          synced,
          verifying,
          LedgerIntent.Verified(r3, content, answers._1)
        )
      assertEquals(
        admit,
        Vector(
          LedgerEffect.App(
            Intent.Dispatch(
              Command.Admit(r3, content, CoreBinding.unbound, CoreBinding.unbound)
            )
          )
        )
      )
      val admitted = perform(verifying, admit)._1
      assert(admitted.document.dataset(r3).exists(_.decision.isAdmitted))
      assertEquals(
        admitted.freshness.standing(run5),
        Some(RunStanding.Stale(NonEmptyVector.one(StaleReason.DatasetMoved(r2, r3))))
      )
      val after = AdmissionLedgerVM.of(AdmissionLedger.sync(answered, admitted)._1, admitted)
      assertEquals(
        after.status,
        Some(
          "r3 is admitted. Run 5 (rev 3) used r2 and is now stale. A change to its mapping " +
            "or geometry creates a new dataset revision."
        )
      )
      assertEquals((after.canAdmit, after.canDecide, after.consequence), (false, false, None))
      assertEquals(Shell.context(admitted).trail.map(_.label).head, "Dataset r3")
    }
  }

  test("an answer that no longer applies admits nothing") {
    served(StoryMoment.T1).map { answers =>
      val review = AdmissionLedger
        .update(
          loaded(t1, answers),
          t1,
          LedgerIntent.ChooseDecision(LedgerDecision.ReviewExclusions)
        )
        ._1
      val (asked, verify)         = AdmissionLedger.update(review, t1, LedgerIntent.Admit)
      val (verifying, appEffects) = perform(t1, verify)
      val content = appEffects.collectFirst { case AppEffect.RequestAdmission(_, c) => c }.get
      // Require complete chosen while eyes4s verifies: refused on the answer.
      val strict = AdmissionLedger
        .update(asked, verifying, LedgerIntent.ChooseDecision(LedgerDecision.RequireComplete))
        ._1
      val (refused, none) =
        AdmissionLedger.update(
          strict,
          verifying,
          LedgerIntent.Verified(r3, content, answers._1)
        )
      assertEquals(none, Vector.empty)
      assert(
        refused.problem.exists(_.startsWith("Require complete refuses r3")),
        refused.problem
      )
      assertEquals(refused.admitting, None)
      // An answer for other content (the revision was edited since) admits nothing.
      val other = ok(DatasetRevisionSpec.contentDigest(StoryModels.t1.dataset(r2).get))
      assertEquals(
        AdmissionLedger
          .update(asked, verifying, LedgerIntent.Verified(r3, other, answers._1))
          ._2,
        Vector.empty
      )
      // A backend refusal is told, and nothing is admitted.
      val gone            = BackendError.UnknownDataset(r3, Vector(r2))
      val (told, nothing) = AdmissionLedger.update(
        asked,
        verifying,
        LedgerIntent.Verified(r3, content, AdmissionAnswer.Refused(gone))
      )
      assertEquals(
        (nothing, told.problem, told.admitting),
        (Vector.empty, Some(gone.message), None)
      )
      // Withdrawing the verification stops waiting for it.
      val withdrawn =
        AppModel.update(verifying, Intent.Dispatch(Command.WithdrawVerification(r3)))._1
      assertEquals(AdmissionLedger.sync(asked, withdrawn)._1.admitting, None)
    }
  }

  test("without an inventory, absent trials cannot be counted, never 0") {
    served(StoryMoment.T1, _.serveInventory(r3, InventoryScenario.Undeclared)).map { answers =>
      val ledger = loaded(t1, answers)
      val vm     = AdmissionLedgerVM.of(ledger, t1)
      assertEquals(entriesOf(ledger).size, 954)
      assertEquals(vm.rows.head.value, "—")
      assertEquals(vm.rows.head.counted, false)
      assertEquals(vm.rows.head.note, Some("not joined"))
      val absent = vm.rows.find(_.ref == StudioRef.InventoryCount(r3, InventoryKind.Absent)).get
      assertEquals((absent.value, absent.counted), ("—", false))
      assertEquals(
        absent.note,
        Some("Absent trials cannot be counted without a trial inventory (trials.csv).")
      )
      assertEquals(vm.equation, None)
      assertEquals(
        vm.decisions.map(_.note).last,
        "Admits 937 trials; 17 quarantined are recorded with their causes in r3."
      )
    }
  }

  test("a refused inventory shows eyes4s's issues and counts nothing") {
    val issue = InventoryIssue.Width(12, 6, 5)
    served(StoryMoment.T1, _.serveInventory(r3, InventoryScenario.Refused(Vector(issue)))).map {
      answers =>
        val ledger = loaded(t1, answers)
        val vm     = AdmissionLedgerVM.of(ledger, t1)
        assertEquals(vm.rows, Vector.empty)
        assertEquals(vm.issues, Vector("trials.csv record 12 has 5 fields; the header has 6."))
        assert(vm.countsSource.startsWith("Counts of r3 are not available: "), vm.countsSource)
        assertEquals(vm.canAdmit, false)
        assert(ledger.entries match { case Loading.Failed(_) => true; case _ => false })
    }
  }

  test("outside screen is a reported count under ExcludeRecord, never a quarantine cause") {
    served(StoryMoment.T1).map { case (answer, entries) =>
      val summary = answer match
        case AdmissionAnswer.Answered(s) => s
        case other                       => fail(other.toString)
      val all = entries.fold(fail(_), identity)
      // A backend that reports two records of one admitted trial off screen.
      val at   = all.indexWhere(_.disposition == TrialDisposition.Admitted)
      val offs =
        Vector(OutsideFrame(7, -3.0, 5.0, "screen"), OutsideFrame(9, 2000.0, 5.0, "screen"))
      val ledgerE = all.updated(at, all(at).copy(outsideFrame = offs))
      val counted =
        summary.copy(window = summary.window.copy(outsideScreen = 2, trialsOutsideScreen = 1))
      val ledger = loaded(t1, (AdmissionAnswer.Answered(counted), Right(ledgerE)))
      val vm     = AdmissionLedgerVM.of(ledger, t1)
      val tally  = StudioRef.WindowTally(r3, TallyRegion.OutsideScreen)
      val row    = vm.rows.find(_.ref == tally).get
      assertEquals(
        (row.value, row.detail, row.style),
        ("1", Some("2 records"), CountStyle.Reported)
      )
      // The quarantined count and its causes are unchanged.
      assertEquals(vm.rows(2).value, "17")
      val model =
        perform(t1, AdmissionLedger.update(ledger, t1, LedgerIntent.Open(tally))._2)._1
      val open = AdmissionLedgerVM.of(ledger, model).opened.get
      assertEquals(
        open.rows.map(r => (r.trial, r.detail)),
        Vector(all(at).trial -> "2 outside screen: records 7, 9")
      )
      // Under QuarantineTrial there is no reported count: the trials are quarantined.
      val strict = t1.document.dataset(r3).get
      val policy = AppModel
        .update(
          t1,
          Intent.Dispatch(Command.SetOffScreenPolicy(r3, OffScreenChoice.QuarantineTrial))
        )
        ._1
      assert(strict.admission.offScreen == OffScreenChoice.ExcludeRecord)
      assertEquals(
        AdmissionLedgerVM.of(ledger, policy).rows.exists(_.ref == tally),
        false
      )
    }
  }

  test("counts and entries for another revision are ignored; a new revision resets") {
    served(StoryMoment.T1).map { answers =>
      val ledger = loaded(t1, answers)
      val stale = AdmissionLedger.update(ledger, t1, LedgerIntent.CountsRead(r2, answers._1))._1
      assertEquals(stale, ledger)
      val onR2 = AppModel
        .update(
          t1,
          Intent.Navigate(
            eyes4s.studio.app.nav.Location(
              Perspective.Data,
              Vector(eyes4s.studio.app.nav.Place.Dataset(r2))
            )
          )
        )
        ._1
      val (moved, effects) = AdmissionLedger.sync(ledger, onR2)
      assertEquals(moved.shown.map(_.id), Some(r2))
      assertEquals(
        effects,
        Vector(LedgerEffect.RequestCounts(r2), LedgerEffect.RequestLedger(r2))
      )
      assertEquals(moved.counts, Loading.Waiting)
    }
  }

  test("a count names itself in the trail and the status bar") {
    val cause = StudioRef.InventoryCount(r3, InventoryKind.Cause("quarantine.overlap"))
    assertEquals(AdmissionLedgerVM.countTitle(cause), "Quarantined · overlap")
    val labels = eyes4s.studio.app.vm.Labels(t1, eyes4s.studio.app.text.Messages.english)
    assertEquals(labels.selected(cause), "r3 · Quarantined · overlap")
    assertEquals(
      labels.selected(StudioRef.InventoryCount(r3, InventoryKind.Absent)),
      "r3 · Absent"
    )
  }

/** Trial keys the ledger tests name. */
private object MockStudyKeys:
  val p17ret09: TrialKey = eyes4s.studio.core.fixture.MockStudy.key("P17", "ret_09")
