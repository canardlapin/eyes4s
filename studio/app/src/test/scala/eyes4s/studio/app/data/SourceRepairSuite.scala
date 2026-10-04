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

package eyes4s.studio.app.data

import eyes4s.codec.ByteDigest
import eyes4s.studio.app.analysis.{DesignEffect, DesignPreview, Preflight, ResolvedDesign}
import eyes4s.studio.app.explore.DisplaySource
import eyes4s.studio.app.{AppEffect, AppModel, Intent, Notice, StoryModels}
import eyes4s.studio.app.text.SourcesText
import eyes4s.studio.core.backend.DatasetRevision
import eyes4s.studio.core.assets.{SourceBlock, SourceFinding, SourceState}
import eyes4s.studio.core.bundle.{InputEntry, InputKind, InputStatus}
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.{AdmissionDecision, Source, SourceRole, StudioDocument}
import eyes4s.studio.core.fixture.{GoldenAssets, StoryMoments}

import java.nio.charset.StandardCharsets.UTF_8

/** Source repair, headless (ticket S2.5; Data.dc.html, sources): a stored
  * source whose bytes changed at its address, or went missing, is shown on
  * its source card with Repair…, and blocks Save & run (the run card says
  * why, and a dispatched run is refused) until it is re-copied with its
  * exact bytes, or replaced by a new pending revision that must be admitted.
  */
class SourceRepairSuite extends munit.FunSuite:

  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private val r3 = StoryMoments.r3

  private def bytes(text: String): IArray[Byte] = IArray.from(text.getBytes(UTF_8))

  private def sourceOf(d: StudioDocument, role: SourceRole): Source =
    d.dataset(r3).flatMap(_.sources.entries.find(_.role == role)).getOrElse(fail(s"no $role"))

  private def entry(s: Source): InputEntry =
    ok(InputEntry.of(InputKind.Source(s.role), s.path.value.split('/').last, s.bytes, 1L))

  /** Every r3 source present, but the fixations' bytes changed to `edited`. */
  private def changed(d: StudioDocument, edited: IArray[Byte]): Vector[InputStatus] =
    d.dataset(r3)
      .get
      .sources
      .entries
      .map(s =>
        if s.role == SourceRole.Fixations then
          InputStatus.Changed(entry(s), ByteDigest.sha256(edited))
        else InputStatus.Present(entry(s))
      )

  /** `m`'s latest asked check answered with `statuses`. */
  private def answer(m: AppModel, statuses: Vector[InputStatus]): AppModel =
    AppModel.update(m, Intent.InputsChecked(m.checks.asked, statuses))._1

  private val edited = bytes("participant,phase,trial\nedited\n")

  /** The fixture model as a project window that asked for its check. */
  private def project(m: AppModel): AppModel =
    val (asked, effects) = AppModel.update(m, Intent.CheckInputs)
    assertEquals(effects, Vector(AppEffect.CheckInputs(asked.checks.asked)))
    asked

  private def damaged(findings: SourceFinding*): Option[(DatasetRevision, SourceBlock)] =
    Some((r3, SourceBlock.Damaged(findings.toVector)))

  test(
    "fail closed: before its first check, or when the check fails, a project runs and previews nothing"
  ) {
    val m     = StoryModels.t2Analysis
    val asked = project(m)
    assertEquals(asked.runBlock, Some((r3, SourceBlock.Unchecked)))
    val unchecked =
      "r3 is blocked until the project's stored files are checked against their digests."
    def card(at: AppModel) =
      Preflight.vm(ResolvedDesign.sync(ResolvedDesign.empty, at)._1, at).card
    assertEquals((card(asked).enabled, card(asked).reason), (false, Some(unchecked)))
    // The design is not previewed: no backend preview is asked for.
    val (design, effects) = ResolvedDesign.sync(ResolvedDesign.empty, asked)
    assertEquals(effects.collect { case e: DesignEffect.StartPreview => e }, Vector.empty)
    assertEquals(design.preview, DesignPreview.Refused(unchecked))
    // A failed check blocks, naming why.
    val failed =
      AppModel.update(asked, Intent.InputsCheckFailed(asked.checks.asked, "store offline"))._1
    assertEquals(failed.runBlock, Some((r3, SourceBlock.CheckFailed("store offline"))))
    assertEquals(
      card(failed).reason,
      Some("r3 is blocked: the project's stored files could not be checked (store offline).")
    )
    // A window with no project store has nothing stored to check, and runs.
    assertEquals(m.runBlock, None)
    val ran = AppModel.update(m, Intent.Dispatch(Command.SaveAndRun(None)))._1
    assertEquals(ran.document.runs.size, m.document.runs.size + 1)
  }

  test(
    "a changed source blocks: the run card says why, and every Save & run checks again first"
  ) {
    val m       = StoryModels.t2Analysis
    val blocked = answer(project(m), changed(m.document, edited))
    val fix     = sourceOf(m.document, SourceRole.Fixations)
    val found   = ByteDigest.sha256(edited)
    val finding = SourceFinding(r3, fix, SourceState.Changed(found))
    assertEquals(blocked.runBlock, damaged(finding))
    val card = Preflight.vm(ResolvedDesign.sync(ResolvedDesign.empty, blocked)._1, blocked).card
    assertEquals((card.enabled, card.run), (false, None))
    assertEquals(
      card.reason,
      Some(
        s"r3 is blocked: fixations.csv is changed since it was stored " +
          s"(sha256:${fix.bytes.hex} recorded, ${found.hex} found). " +
          "Repair it in Data · Sources, or admit a revision that replaces it."
      )
    )
    // A run dispatched anyway asks for a check first, and is refused when the
    // check still finds the source changed; nothing changes.
    val (waiting, ask) = AppModel.update(blocked, Intent.Dispatch(Command.SaveAndRun(None)))
    assertEquals(ask, Vector(AppEffect.CheckInputs(waiting.checks.asked)))
    assertEquals(waiting.document, blocked.document)
    val (refused, none) = AppModel.update(
      waiting,
      Intent.InputsChecked(waiting.checks.asked, changed(m.document, edited))
    )
    assertEquals(none, Vector.empty)
    assertEquals(refused.document, blocked.document)
    assertEquals(
      refused.notice,
      Some(Notice.SourcesBlocked(r3, SourceBlock.Damaged(Vector(finding))))
    )
    // Repaired since: the run's own check finds every source present, and it runs.
    val present =
      m.document.dataset(r3).get.sources.entries.map(s => InputStatus.Present(entry(s)))
    val (asking, again) = AppModel.update(refused, Intent.Dispatch(Command.SaveAndRun(None)))
    assertEquals(again, Vector(AppEffect.CheckInputs(asking.checks.asked)))
    val ran = answer(asking, present)
    assertEquals(ran.runBlock, None)
    assertEquals(ran.document.runs.size, m.document.runs.size + 1)
    // A source damaged after the last check is found by the run's own check.
    val clean        = answer(project(m), present)
    val (pending, _) = AppModel.update(clean, Intent.Dispatch(Command.SaveAndRun(None)))
    val (stopped, _) = AppModel.update(
      pending,
      Intent.InputsChecked(pending.checks.asked, changed(m.document, edited))
    )
    assertEquals(stopped.document.runs.size, m.document.runs.size)
    assertEquals(
      stopped.notice,
      Some(Notice.SourcesBlocked(r3, SourceBlock.Damaged(Vector(finding))))
    )
  }

  test(
    "a source missing from the project, or not listed at all, blocks too, naming its digest"
  ) {
    val m       = project(StoryModels.t2Analysis)
    val sources = m.document.dataset(r3).get.sources.entries
    val fix     = sourceOf(m.document, SourceRole.Fixations)
    val missing =
      sources.map(s =>
        if s.role == SourceRole.Fixations then InputStatus.Missing(entry(s))
        else InputStatus.Present(entry(s))
      )
    val blocked = answer(m, missing)
    assertEquals(blocked.runBlock, damaged(SourceFinding(r3, fix, SourceState.Missing)))
    assertEquals(
      SourcesText.state(SourceFinding(r3, fix, SourceState.Missing)),
      s"missing from the project (sha256:${fix.bytes.hex} recorded)"
    )
    assertEquals(
      answer(m, Vector.empty).runBlock.map {
        case (_, SourceBlock.Damaged(fs)) => fs.map(_.state).distinct
        case other                        => fail(s"$other")
      },
      Some(Vector(SourceState.Missing))
    )
  }

  /** The Sources pane on r3, read. */
  private def loaded(m: AppModel): SourcesPane =
    val (pane, _) = SourcesPane.sync(SourcesPane.empty, m)
    val spec      = m.document.dataset(r3).get
    val served    = GoldenAssets.registry(spec).map(DisplaySource.Served(_))
    SourcesPane.update(pane, m, SourcesIntent.RegistryRead(r3, 1, served))._1

  test(
    "the source card says what changed and offers Repair…; other bytes make a pending revision"
  ) {
    val m =
      answer(project(StoryModels.t2Explore), changed(StoryModels.t2Explore.document, edited))
    val pane = loaded(m)
    val vm   = SourcesVM.of(pane, m)
    // Checked: the pane says nothing of the check itself.
    assertEquals(vm.check, None)
    assertEquals(
      SourcesVM.of(pane, project(StoryModels.t2Explore)).check,
      Some("r3 is blocked until the project's stored files are checked against their digests.")
    )
    val fix = sourceOf(m.document, SourceRole.Fixations)
    val row = vm.sources.head
    assertEquals(row.name, "fixations.csv")
    assertEquals(
      row.problem,
      Some(
        s"changed since it was stored (sha256:${fix.bytes.hex} recorded, " +
          s"${ByteDigest.sha256(edited).hex} found)"
      )
    )
    assertEquals(
      row.repair,
      Some(("Repair fixations.csv…", SourcesIntent.RepairSource(SourceRole.Fixations)))
    )
    // The other cards are present: no problem, no Repair….
    assertEquals(vm.sources.tail.map(r => (r.problem, r.repair)).distinct, Vector((None, None)))
    // Repair… locates the source.
    assertEquals(
      SourcesPane.update(pane, m, SourcesIntent.RepairSource(SourceRole.Fixations))._2,
      Vector(SourcesEffect.LocateSource(r3, fix))
    )
    // Other bytes are stored as a replacement; once stored, a new pending
    // revision replaces the source, and the inputs are checked again.
    val other       = bytes("participant,phase,trial\nanother\n")
    val replacement = fix.copy(bytes = ByteDigest.sha256(other), semantic = None)
    val (_, store)  =
      SourcesPane.update(
        pane,
        m,
        SourcesIntent.SourceChosen(r3, SourceRole.Fixations, other, ByteDigest.sha256(other))
      )
    assertEquals(store, Vector(SourcesEffect.Store(r3, replacement, other)))
    val (stored, dispatched) =
      SourcesPane.update(pane, m, SourcesIntent.SourceStored(r3, replacement, Right(())))
    val next = eyes4s.studio.app.geometry.GeometryPanel.nextRevision(m.document)
    assertEquals(
      stored.note,
      Some(
        s"fixations.csv has other bytes than r3 recorded; ${next.label} replaces it and must be admitted before it is run."
      )
    )
    val command = dispatched
      .collectFirst { case SourcesEffect.App(Intent.Dispatch(c)) => c }
      .getOrElse(fail("no command"))
    assertEquals(dispatched.last, SourcesEffect.App(Intent.CheckInputs))
    val after = AppModel.update(m, Intent.Dispatch(command))._1
    val spec  = after.document.dataset(next).getOrElse(fail("no new revision"))
    val old   = m.document.dataset(r3).get
    assertEquals(spec.parent, Some(r3))
    assertEquals(spec.decision, AdmissionDecision.Pending)
    assertEquals(spec.sources.fixations, Some(replacement))
    assertEquals(spec.sources.trials, old.sources.trials)
    assertEquals(
      (spec.mapping, spec.geometry, spec.admission),
      (old.mapping, old.geometry, old.admission)
    )
    // A failed store is said, naming the file.
    assertEquals(
      SourcesPane
        .update(pane, m, SourcesIntent.SourceStored(r3, replacement, Left("disk full")))
        ._1
        .note,
      Some("fixations.csv could not be repaired: disk full")
    )
  }

  test(
    "the recorded bytes are re-copied, then checked again; a choice for an old revision stores nothing"
  ) {
    val m        = StoryModels.t2Explore
    val original = bytes("participant,phase,trial\noriginal\n")
    val pane0    = loaded(m)
    // r3 as if its fixations were these bytes.
    val spec = pane0.dataset.get
    val fix  = spec.sources.fixations.get.copy(bytes = ByteDigest.sha256(original))
    val pane = pane0.copy(dataset =
      Some(
        spec.copy(sources =
          ok(
            eyes4s.studio.core.document.Sources.of(
              spec.sources.entries.map(s => if s.role == SourceRole.Fixations then fix else s)
            )
          )
        )
      )
    )
    assertEquals(
      SourcesPane
        .update(
          pane,
          m,
          SourcesIntent
            .SourceChosen(r3, SourceRole.Fixations, original, ByteDigest.sha256(original))
        )
        ._2,
      Vector(SourcesEffect.Restore(r3, fix, original))
    )
    val (restored, check) =
      SourcesPane.update(
        pane,
        m,
        SourcesIntent.SourceRestored(r3, SourceRole.Fixations, Right(()))
      )
    assertEquals(check, Vector(SourcesEffect.App(Intent.CheckInputs)))
    assertEquals(
      restored.note,
      Some(
        s"fixations.csv re-copied: its bytes are the ones r3 recorded (sha256:${fix.bytes.hex.take(12)})."
      )
    )
    // A file chosen for a revision no longer shown stores nothing, and says so.
    val (orphaned, none) =
      SourcesPane.update(
        pane,
        m,
        SourcesIntent.SourceChosen(
          StoryMoments.r3.copy(number = 9),
          SourceRole.Fixations,
          original,
          ByteDigest.sha256(original)
        )
      )
    assertEquals(none, Vector.empty)
    assert(orphaned.note.exists(_.startsWith("The revision changed before")), orphaned.note)
  }

  test(
    "an import after the last check checks again; until then its files are unchecked, not missing"
  ) {
    val m      = StoryModels.t2Explore
    val all    = m.document.datasets.flatMap(_.sources.entries).distinct
    val clean  = answer(project(m), all.map(s => InputStatus.Present(entry(s))))
    val r3spec = m.document.dataset(r3).get
    val other  = bytes("participant,phase,trial\nanother\n")
    val fix    =
      r3spec.sources.fixations.get.copy(bytes = ByteDigest.sha256(other), semantic = None)
    val sources = ok(
      eyes4s.studio.core.document.Sources.of(
        r3spec.sources.entries.map(s => if s.role == SourceRole.Fixations then fix else s)
      )
    )
    val reimport = Command.ImportSources(
      Some(r3),
      sources,
      r3spec.mapping,
      r3spec.units,
      r3spec.geometry,
      r3spec.attributes,
      None,
      r3spec.inventory
    )
    val (imported, effects) = AppModel.update(clean, Intent.Dispatch(reimport))
    // The import asks for a new check: its file is not in the last one.
    assertEquals(
      effects.collect { case c: AppEffect.CheckInputs => c },
      Vector(AppEffect.CheckInputs(imported.checks.asked))
    )
    assert(imported.checks.outstanding)
    val r4 = imported.document.datasets.last.id
    assertEquals(
      imported.sources.of(r4).map(f => (f.name, f.state)),
      Vector(("fixations.csv", SourceState.Unchecked), ("trials.csv", SourceState.Present))
    )
    // Unchecked is no reason to offer a repair; it blocks until checked.
    assertEquals(imported.sources.block(r4).isDefined, true)
    // The check lists the new file: present.
    val listed = answer(imported, (all :+ fix).map(s => InputStatus.Present(entry(s))))
    assertEquals(listed.sources.block(r4), None)
    // A change that adds no source asks for nothing.
    val (_, none) = AppModel.update(
      listed,
      Intent.Dispatch(Command.SetUnits(r4, r3spec.units.copy(time = None)))
    )
    assertEquals(none.collect { case c: AppEffect.CheckInputs => c }, Vector.empty)
  }

  test("a pane opened while unchecked previews once the check finds the sources present") {
    val m               = StoryModels.t2Analysis
    val asked           = project(m)
    val (pane, refused) = ResolvedDesign.sync(ResolvedDesign.empty, asked)
    assertEquals(refused.collect { case e: DesignEffect.StartPreview => e }, Vector.empty)
    val present = answer(
      asked,
      m.document.datasets.flatMap(_.sources.entries).map(s => InputStatus.Present(entry(s)))
    )
    val (ready, start) = ResolvedDesign.sync(pane, present)
    assertEquals(start.collect { case e: DesignEffect.StartPreview => e.revision }.size, 1)
    assertEquals(ready.blocked, None)
    // And it is not asked again while the sources stay present.
    assertEquals(ResolvedDesign.sync(ready, present)._2, Vector.empty)
  }

  test(
    "a run waits for its own check: an earlier answer is not enough; a second run is told so"
  ) {
    val m       = StoryModels.t2Analysis
    val asked   = project(m) // round 1, asked when the window opened
    val present =
      m.document.datasets.flatMap(_.sources.entries).map(s => InputStatus.Present(entry(s)))
    val (waiting, ask) = AppModel.update(asked, Intent.Dispatch(Command.SaveAndRun(None)))
    assertEquals(ask, Vector(AppEffect.CheckInputs(2L)))
    // The open-time check (round 1) answers: the run still waits for round 2.
    val early = AppModel.update(waiting, Intent.InputsChecked(1L, present))._1
    assertEquals(early.document.runs.size, m.document.runs.size)
    assert(early.checks.runAfter.isDefined)
    // A second Save & run meanwhile is told it waits, not dropped silently.
    val (again, none) = AppModel.update(early, Intent.Dispatch(Command.SaveAndRun(None)))
    assertEquals((none, again.notice), (Vector.empty, Some(Notice.RunWaiting)))
    // Its own round answers: it runs.
    val ran = AppModel.update(again, Intent.InputsChecked(2L, present))._1
    assertEquals(ran.document.runs.size, m.document.runs.size + 1)
    // A stale answer after a newer one is ignored.
    val stale = AppModel.update(ran, Intent.InputsCheckFailed(1L, "late"))._1
    assertEquals(stale.inputs, ran.inputs)
  }

  test("an answer for a round never asked is ignored; the asked round still decides") {
    val m       = StoryModels.t2Analysis
    val present =
      m.document.datasets.flatMap(_.sources.entries).map(s => InputStatus.Present(entry(s)))
    val (waiting, _) = AppModel.update(project(m), Intent.Dispatch(Command.SaveAndRun(None)))
    val forged       = AppModel.update(waiting, Intent.InputsChecked(99L, present))._1
    assertEquals(forged, waiting)
    // The real answer to the asked round is still taken.
    val ran = AppModel.update(forged, Intent.InputsChecked(waiting.checks.asked, present))._1
    assertEquals(ran.document.runs.size, m.document.runs.size + 1)
  }

  test("the same bytes imported under another name are checked again, never shown missing") {
    val m      = StoryModels.t2Explore
    val all    = m.document.datasets.flatMap(_.sources.entries).distinct
    val clean  = answer(project(m), all.map(s => InputStatus.Present(entry(s))))
    val r3spec = m.document.dataset(r3).get
    val fix    = r3spec.sources.fixations.get
      .copy(path = ok(eyes4s.studio.core.document.SourcePath.of("inputs/renamed.csv")))
    val sources = ok(
      eyes4s.studio.core.document.Sources.of(
        r3spec.sources.entries.map(s => if s.role == SourceRole.Fixations then fix else s)
      )
    )
    val (imported, effects) = AppModel.update(
      clean,
      Intent.Dispatch(
        Command.ImportSources(
          Some(r3),
          sources,
          r3spec.mapping,
          r3spec.units,
          r3spec.geometry,
          r3spec.attributes,
          None,
          r3spec.inventory
        )
      )
    )
    assertEquals(effects.collect { case c: AppEffect.CheckInputs => c }.size, 1)
    val r4 = imported.document.datasets.last.id
    assertEquals(
      imported.sources.of(r4).find(_.name == "renamed.csv").map(_.state),
      Some(SourceState.Unchecked)
    )
  }

  test("a waiting Save & run can be cancelled from the run card") {
    val m            = StoryModels.t2Analysis
    val (waiting, _) = AppModel.update(project(m), Intent.Dispatch(Command.SaveAndRun(None)))
    val card = Preflight.vm(ResolvedDesign.sync(ResolvedDesign.empty, waiting)._1, waiting).card
    assertEquals(
      (card.button, card.enabled, card.run),
      ("Cancel the waiting Save & run", true, Some(Intent.CancelWaitingRun))
    )
    assertEquals(
      card.reason,
      Some("Save & run is already waiting for the project's stored files to be checked.")
    )
    val cancelled = AppModel.update(waiting, Intent.CancelWaitingRun)._1
    assertEquals(cancelled.checks.runAfter, None)
    // Its check's answer then runs nothing.
    val present =
      m.document.datasets.flatMap(_.sources.entries).map(s => InputStatus.Present(entry(s)))
    val after =
      AppModel.update(cancelled, Intent.InputsChecked(waiting.checks.asked, present))._1
    assertEquals(after.document.runs.size, m.document.runs.size)
  }
