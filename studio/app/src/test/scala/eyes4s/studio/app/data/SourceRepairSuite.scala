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
import eyes4s.studio.app.analysis.{Preflight, ResolvedDesign}
import eyes4s.studio.app.explore.DisplaySource
import eyes4s.studio.app.{AppModel, Intent, Notice, StoryModels}
import eyes4s.studio.core.assets.{SourceFinding, SourceState}
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
  private def changed(d: StudioDocument, edited: IArray[Byte]): Intent =
    Intent.InputsChecked(
      d.dataset(r3)
        .get
        .sources
        .entries
        .map(s =>
          if s.role == SourceRole.Fixations then
            InputStatus.Changed(entry(s), ByteDigest.sha256(edited))
          else InputStatus.Present(entry(s))
        )
    )

  private val edited = bytes("participant,phase,trial\nedited\n")

  test("a changed source blocks Save & run: the run card says why, and a run is refused") {
    val m       = StoryModels.t2Analysis
    val blocked = AppModel.update(m, changed(m.document, edited))._1
    val fix     = sourceOf(m.document, SourceRole.Fixations)
    val found   = ByteDigest.sha256(edited)
    assertEquals(
      blocked.runBlockers,
      Some((r3, Vector(SourceFinding(r3, fix, SourceState.Changed(found)))))
    )
    val card = Preflight.vm(ResolvedDesign.sync(ResolvedDesign.empty, blocked)._1, blocked).card
    assertEquals(card.enabled, false)
    assertEquals(card.run, None)
    assertEquals(
      card.reason,
      Some(
        s"r3 cannot be run: fixations.csv is changed since it was stored · " +
          s"sha256:${fix.bytes.hex.take(12)} recorded, ${found.hex.take(12)} found. " +
          "Repair it in Data · Sources, or admit a revision that replaces it."
      )
    )
    // A run dispatched anyway is refused, and nothing changes.
    val (refused, effects) = AppModel.update(blocked, Intent.Dispatch(Command.SaveAndRun(None)))
    assertEquals(effects, Vector.empty)
    assertEquals(refused.document, blocked.document)
    assertEquals(
      refused.notice,
      Some(
        Notice.SourcesBlocked(r3, Vector(SourceFinding(r3, fix, SourceState.Changed(found))))
      )
    )
    // Unchecked, or with every source present, the run goes ahead.
    val present = Intent.InputsChecked(
      m.document.dataset(r3).get.sources.entries.map(s => InputStatus.Present(entry(s)))
    )
    for unblocked <- Vector(m, AppModel.update(m, present)._1) do
      assertEquals(unblocked.runBlockers, None)
      val ran = AppModel.update(unblocked, Intent.Dispatch(Command.SaveAndRun(None)))._1
      assertEquals(ran.document.runs.size, m.document.runs.size + 1)
  }

  test("a source missing from the project, or not listed at all, blocks too") {
    val m       = StoryModels.t2Analysis
    val sources = m.document.dataset(r3).get.sources.entries
    val missing = Intent.InputsChecked(
      sources.map(s =>
        if s.role == SourceRole.Fixations then InputStatus.Missing(entry(s))
        else InputStatus.Present(entry(s))
      )
    )
    assertEquals(
      AppModel.update(m, missing)._1.runBlockers.map(_._2.map(f => (f.name, f.state))),
      Some(Vector(("fixations.csv", SourceState.Missing)))
    )
    val unlisted = Intent.InputsChecked(Vector.empty)
    assertEquals(
      AppModel.update(m, unlisted)._1.runBlockers.map(_._2.map(_.state).distinct),
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
      AppModel.update(StoryModels.t2Explore, changed(StoryModels.t2Explore.document, edited))._1
    val pane = loaded(m)
    val vm   = SourcesVM.of(pane, m)
    val fix  = sourceOf(m.document, SourceRole.Fixations)
    val row  = vm.sources.head
    assertEquals(row.name, "fixations.csv")
    assertEquals(
      row.problem,
      Some(
        s"changed since it was stored · sha256:${fix.bytes.hex.take(12)} recorded, " +
          s"${ByteDigest.sha256(edited).hex.take(12)} found"
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
      SourcesPane.update(pane, m, SourcesIntent.SourceChosen(r3, SourceRole.Fixations, other))
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
    assertEquals(dispatched.last, SourcesEffect.CheckInputs)
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
        .update(pane, m, SourcesIntent.SourceChosen(r3, SourceRole.Fixations, original))
        ._2,
      Vector(SourcesEffect.Restore(r3, fix, original))
    )
    val (restored, check) =
      SourcesPane.update(
        pane,
        m,
        SourcesIntent.SourceRestored(r3, SourceRole.Fixations, Right(()))
      )
    assertEquals(check, Vector(SourcesEffect.CheckInputs))
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
          original
        )
      )
    assertEquals(none, Vector.empty)
    assert(orphaned.note.exists(_.startsWith("The revision changed before")), orphaned.note)
  }
