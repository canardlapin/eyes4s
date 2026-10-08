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

package eyes4s.studio.core.command

import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.*

class FamilyReducerSuite extends munit.FunSuite:
  import FamilySamples.*
  import Command.*

  test("unrelated edits, undo/redo and draft discard/restore preserve exact ownership") {
    val base   = document(draft = Some(draftA))
    val edited = get(History.start(base).apply(RemoveReporting(base.reporting.head.id))).history
    assertEquals(edited.document.analysisFamilies, Some(registry))
    val discarded = get(edited.apply(DiscardDraft)).history
    assertEquals(discarded.document.analysisFamilies, Some(registry))
    val restored = get(discarded.undo).history
    assertEquals(restored.document, edited.document)
    assertEquals(get(restored.redo).history.document, discarded.document)
    val themed = get(restored.apply(SetTheme(Theme.Dark))).history
    assertEquals(themed.document.science, restored.document.science)
    assertEquals(get(themed.undoView).history.document, restored.document)
  }

  test("StartDraft explicitly edits an older family while allocating revision ids globally") {
    val history =
      get(History.start(document()).apply(StartDraft(a1.id, None, Vector(change)))).history
    assertEquals(history.document.draft.map(_.id), Some(AnalysisRevision(5)))
    assertEquals(history.document.familyOf(AnalysisRevision(5)), Some(a))
    assertEquals(history.document.analysisFamilies, Some(registry))
  }

  test(
    "SaveAndRun extends the base owner's registry atomically; outcomes retain both families"
  ) {
    val base  = document(draft = Some(draftA))
    val saved = get(History.start(base).apply(SaveAndRun(None))).history
    assertEquals(saved.document.analyses.map(_.id), analyses.map(_.id) :+ draftA.id)
    assertEquals(saved.document.familyOf(draftA.id), Some(a))
    assertEquals(saved.document.familyOf(b4.id), Some(b))
    val nextRegistry = saved.document.analysisFamilies.get
    assertEquals(nextRegistry.owners.size, registry.owners.size + 1)
    assertEquals(nextRegistry.families, registry.families)
    assertEquals(nextRegistry.revisions, saved.document.analyses.map(_.id))
    val completed = get(
      saved.apply(RecordRunOutcome(RunId(1), RunLifecycle.Completed, CoreBinding.unbound))
    ).history
    assertEquals(completed.document.analysisFamilies, saved.document.analysisFamilies)
  }

  test("checkpointed journal replay preserves draft ownership and atomic saved ownership") {
    val base     = document()
    val commands = Vector[Command](
      StartDraft(a1.id, None, Vector(change)),
      DiscardDraft,
      RestoreDraft(draftA),
      SaveAndRun(None)
    )
    val next = commands.foldLeft(History.start(base))((history, command) =>
      get(history.apply(command)).history
    )
    val lines = get(CommandJournal.start(base)) +:
      commands.zipWithIndex.map((command, i) =>
        get(CommandJournal.entry(i + 1, JournalEntry.Apply(command)))
      )
    val journal =
      (lines :+ get(CommandJournal.checkpoint(commands.size, next.document))).mkString("\n")
    val replay = get(CommandJournal.replay(base, journal))
    assertEquals(replay.history.document, next.document)
    assertEquals(replay.checkpoints, Vector(commands.size))
    assertEquals(replay.torn, None)
    assertEquals(replay.history.document.familyOf(draftA.id), Some(a))
  }

  test("journal undo and redo entries replay draft ownership from an explicit base") {
    val base   = document()
    val edited = get(History.start(base).apply(StartDraft(a1.id, None, Vector(change)))).history
    val undone = get(edited.undo).history
    val redone = get(undone.redo).history
    val entries = Vector(
      JournalEntry.Apply(StartDraft(a1.id, None, Vector(change))),
      JournalEntry.Undo,
      JournalEntry.Redo
    )
    val lines = get(CommandJournal.start(base)) +:
      entries.zipWithIndex.map((entry, i) => get(CommandJournal.entry(i + 1, entry)))
    val replay = get(
      CommandJournal.replay(
        base,
        (lines :+ get(CommandJournal.checkpoint(entries.size, redone.document))).mkString("\n")
      )
    )
    assertEquals(replay.history.document, redone.document)
    assertEquals(replay.history.document.familyOf(draftA.id), Some(a))
    assertEquals(replay.history.document.analysisFamilies, Some(registry))
    assertEquals(replay.checkpoints, Vector(entries.size))
  }

  test("an untargeted edit keeps the global latest base even when an older family is shown") {
    val shown  = run(1, a1, RunLifecycle.Completed)
    val base   = document(Vector(shown), shown = Some(shown.id))
    val edited = get(History.start(base).apply(ChangeRecipe(change))).history.document
    assertEquals(edited.draft.map(_.origin), Some(DraftOrigin.Existing(b4.id)))
    assertEquals(edited.familyOf(draftA.id), Some(b))
    assertEquals(edited.analysisFamilies, Some(registry))
  }
