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

package eyes4s.studio.app.runs

import eyes4s.codec.{ArtifactName, ByteDigest}
import eyes4s.studio.core.document.DocumentSamples.{t1, t3}
import eyes4s.studio.core.document.{RunLifecycle, RunRef, StudioDocument}
import eyes4s.studio.core.fixture.StoryMoments.{run5, run6, run7, run8}
import eyes4s.studio.core.runs.*

/** The run storage panel (S2.6): sizes, kept reasons and pruning only after
  * confirmation, over the store's classification.
  */
class RunStoragePanelSuite extends munit.FunSuite:

  private def right[E, A](e: Either[E, A]): A =
    e.fold(err => throw new AssertionError(err.toString), identity)

  private val result = right(ArtifactName.of("study-result.json"))

  /** t3 with run 8 completed: Figure 1 binds run 7 (shown), Figure 2 run 5. */
  private val document: StudioDocument = right(
    StudioDocument.of(
      t3.datasets,
      t3.analyses,
      t3.draft,
      t3.runs.map(r => if r.id == run8 then r.copy(state = RunLifecycle.Completed) else r),
      t3.reporting,
      t3.figures,
      t3.presentation,
      Vector.empty
    )
  )

  /** A stored record of `run` whose content has `length` bytes. */
  private def stored(run: RunRef, length: Int): ArchiveRecord =
    val archive = right(RunArchive.of(run, Vector(result -> IArray.fill[Byte](length)(1))))
    val digest  = archive.index.entries.head.sha256
    ArchiveRecord.Stored(
      archive.index,
      100L,
      ByteDigest.sha256(IArray.fill[Byte](1)(0)),
      Vector(right(ArchivePaths.content(run.id, digest)), right(ArchivePaths.index(run.id)))
    )

  private val storage: RunStorage = RunStorage.classify(
    Vector(
      stored(document.run(run5).get, 4_000_000),
      stored(document.run(run7).get, 12_300_000),
      stored(document.run(run8).get, 2_400_000),
      ArchiveRecord.Incomplete(
        run6,
        Vector(right(ArchivePaths.content(run6, ByteDigest.sha256(IArray.fill[Byte](1)(2)))))
      )
    ),
    RetentionBasis.of(document)
  )

  private def loaded: RunStoragePanel =
    RunStoragePanel.start._1.update(StorageIntent.Loaded(storage))._1

  test("the panel starts by listing the store") {
    val (panel, effects) = RunStoragePanel.start
    assertEquals(effects, Vector(StorageEffect.Refresh))
    assertEquals(panel.view(document).summary, RunStorageText.Loading)
  }

  test("every stored archive shows its size and why it is kept") {
    val view = loaded.view(document)
    assertEquals(
      view.rows.map(r => (r.run, r.size, r.status, r.choosable)),
      Vector(
        (run5, "4.0 MB", "Kept · Figure 2", false),
        (run6, "size unknown", "Incomplete · Prunable", true),
        (run7, "12.3 MB", "Kept · Figure 1, shown", false),
        (run8, "2.4 MB", "Prunable", true)
      )
    )
    assertEquals(view.rows.head.label, "run 5 · rev 3 · data r2")
    assertEquals(view.summary, "4 stored runs · 18.7 MB · 2.4 MB prunable")
    assertEquals(view.pruneEnabled, false)
  }

  test("a kept run cannot be chosen; pruning needs confirmation and names the plan") {
    val (afterToggle, _) = loaded
      .update(StorageIntent.Toggle(run5))
      ._1
      .update(StorageIntent.Toggle(run8))
    assertEquals(afterToggle.view(document).rows.filter(_.chosen).map(_.run), Vector(run8))
    val (asking, none) = afterToggle.update(StorageIntent.RequestPrune)
    assertEquals(none, Vector.empty)
    val dialog = asking.view(document).dialog.get
    assertEquals(dialog.title, "Prune run results?")
    assert(dialog.body.startsWith("Delete the stored results of run 8 (2.4 MB)."), dialog.body)
    // Cancel prunes nothing.
    assertEquals(asking.update(StorageIntent.Cancel)._2, Vector.empty)
    val (busy, effects) = asking.update(StorageIntent.Confirm)
    assertEquals(
      effects.collect { case StorageEffect.Prune(c) => c.plan.runs },
      Vector(Vector(run8))
    )
    assertEquals(busy.view(document).rows.exists(_.choosable), false)
    val (done, refresh) = busy.update(
      StorageIntent.Pruned(PruneReport(Vector(run8), 2_400_100L, Vector.empty))
    )
    assertEquals(refresh, Vector(StorageEffect.Refresh))
    assertEquals(done.view(document).notice, Some("Pruned run 8; 2.4 MB freed."))
  }

  test("choosing all prunable never chooses a figure-bound or shown run") {
    val (all, _)    = loaded.update(StorageIntent.ChooseAllPrunable)
    val (asking, _) = all.update(StorageIntent.RequestPrune)
    assertEquals(all.view(document).rows.filter(_.chosen).map(_.run), Vector(run6, run8))
    assert(
      asking.view(document).dialog.exists(_.body.contains("run 6 and run 8 (at least 2.4 MB)")),
      asking.view(document).dialog
    )
  }

  test("a refresh that makes a chosen run kept unchooses it") {
    val chosen = loaded.update(StorageIntent.Toggle(run8))._1
    val bound  = RunStorage.classify(
      storage.rows.map(_.record),
      RetentionBasis.of(document).withReady(run8)
    )
    val after = chosen.update(StorageIntent.Loaded(bound))._1
    assertEquals(after.view(document).rows.filter(_.chosen), Vector.empty)
    assertEquals(after.update(StorageIntent.RequestPrune)._1.view(document).dialog, None)
  }

  test("byte counts use decimal units with one place") {
    assertEquals(RunStorageText.bytes(0L), "0 B")
    assertEquals(RunStorageText.bytes(999L), "999 B")
    assertEquals(RunStorageText.bytes(1000L), "1.0 kB")
    assertEquals(RunStorageText.bytes(999_960L), "1.0 MB")
    assertEquals(RunStorageText.bytes(12_345_678L), "12.3 MB")
    assertEquals(RunStorageText.bytes(Long.MaxValue), "9.2 EB")
  }

  test("a run not in the document is labelled as such") {
    // t1 has run 5 only.
    val rows = loaded.view(t1).rows
    assertEquals(rows.head.label, "run 5 · rev 3 · data r2")
    assertEquals(rows.last.label, "run 8 · not in this document")
  }
