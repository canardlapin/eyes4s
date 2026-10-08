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

package eyes4s.studio.core.document

import eyes4s.studio.core.backend.*
import eyes4s.studio.core.command.*
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.freshness.*
import io.circe.Json
import io.circe.syntax.*
import scala.compiletime.testing.typeCheckErrors

class SeededFamilyDraftSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val legacy   = get(Reducer.run(DocumentSamples.t2, Command.DiscardDraft)).document
  private val explicit = FamilySamples.document()
  private val saved    = legacy.analyses.last
  private val studio   = saved.studio.copy(preset = Preset.Custom)
  private val start    =
    new Command.StartFamily("Independent", saved.dataset, saved.recipe, studio)
  private def apply(base: StudioDocument, command: Command): StudioDocument = get(
    Reducer.run(base, command)
  ).document
  private def empty = get(
    StudioDocument.of(
      legacy.datasets,
      Vector.empty,
      None,
      Vector.empty,
      legacy.reporting,
      Vector.empty,
      PresentationState.default,
      Vector.empty
    )
  )

  private def explicitEmpty = get(
    StudioDocument.of(
      empty.datasets,
      empty.analyses,
      None,
      empty.runs,
      empty.reporting,
      empty.figures,
      empty.presentation,
      Vector.empty,
      Some(get(AnalysisFamilyRegistry.of(Vector.empty, Vector.empty, Vector.empty)))
    )
  )

  test("start/discard preserves exact legacy or explicit saved facts and bytes") {
    Vector(legacy, explicit, empty, explicitEmpty).foreach { base =>
      val before  = get(StudioDocument.encode(base))
      val started = apply(base, start)
      val draft   = started.draft.get
      assert(draft.isNewFamily)
      assert(!draft.isInitial)
      assertEquals(draft.savedBase, None)
      assertEquals(draft.changes, Vector.empty)
      assertEquals(started.analyses, base.analyses)
      assertEquals(started.runs, base.runs)
      assertEquals(started.analysisFamilies, base.analysisFamilies)
      assertEquals(started.familyOf(draft.id), Some(get(base.nextFamilyId)))
      assertEquals(started.draftContext.map(_.recipe), Some(saved.recipe))
      assertEquals(started.draftContext.map(_.studio), Some(studio))
      val discarded = apply(started, Command.DiscardDraft)
      assertEquals(discarded, base)
      assertEquals(get(StudioDocument.encode(discarded)), before)
      assertEquals(apply(discarded, Command.RestoreDraft(draft)), started)
      assert(Reducer.run(started, start).isLeft)
    }
  }

  test("seeded family edits/reverts and undo/redo keep the independent draft alive") {
    val initial  = get(History.start(legacy).apply(start)).history
    val change   = RecipeChange.Grid(saved.recipe.grid, get(GridSize.of(32, 24)))
    val edited   = get(initial.apply(Command.ChangeRecipe(change))).history
    val reverted = get(edited.apply(Command.ChangeRecipe(change.inverse))).history
    assertEquals(reverted.document.draft, initial.document.draft)
    assertEquals(get(edited.undo).history.document, initial.document)
    assertEquals(get(get(initial.undo).history.redo).history.document, initial.document)
    val data    = legacy.datasets.head.id
    val rebased = get(initial.apply(Command.RebaseDraft(data))).history
    assertEquals(
      get(rebased.apply(Command.RebaseDraft(saved.dataset))).history.document.draft,
      initial.document.draft
    )
    assertEquals(get(rebased.undo).history.document, initial.document)
    assertEquals(
      Draft.against(AnalysisRevision(5), saved, None, Vector.empty),
      Left(DocumentError.NoChanges(AnalysisRevision(5), saved.id))
    )
  }

  test("save atomically materializes the new family while retaining every previous owner") {
    Vector(legacy, explicit, empty, explicitEmpty).foreach { base =>
      val started  = apply(base, start)
      val draft    = started.draft.get
      val familyId = get(base.nextFamilyId)
      val renamed  = studio.copy(name = get(RevisionName.of("Revision override")))
      val result   = get(Reducer.run(started, Command.SaveAndRun(Some(renamed))))
      val doc      = result.document
      assertEquals(doc.analyses.dropRight(1), base.analyses)
      assertEquals(doc.analyses.last.id, draft.id)
      assertEquals(doc.analyses.last.studio, renamed)
      assertEquals(doc.familyOf(draft.id), Some(familyId))
      assertEquals(
        doc.analysisFamilies.flatMap(_.family(familyId)).map(_.name),
        Some("Independent")
      )
      base.analyses.foreach(a => assertEquals(doc.familyOf(a.id), base.familyOf(a.id)))
      assertEquals(doc.analysisFamilies.get.owners.size, doc.analyses.size)
      assertEquals(doc.draft, None)
      assertEquals(doc.runs.last.analysis, draft.id)
      assert(
        result.effects.contains(Effect.RequestRun(doc.runs.last.id, draft.id, saved.dataset))
      )
      val wire = get(StudioDocument.encode(doc))
      assertEquals(wire.hcursor.downField("schema").get[Int]("version"), Right(10))
      assertEquals(get(StudioDocument.decode(wire)), doc)
    }
    val materialized =
      apply(apply(legacy, start), Command.SaveAndRun(None)).analysisFamilies.get
    assertEquals(
      materialized.family(AnalysisFamilyId.Legacy).map(_.name),
      Some(legacy.analyses.head.studio.name.value)
    )
  }

  test("provisional families never stale or compare against an unrelated shown run") {
    val started = apply(legacy, start)
    val shown   = started.run(started.presentation.shownRun.get).get
    assert(!started.sameFamily(shown.analysis, started.draft.get.id))
    val before = Freshness.of(legacy, SessionFacts.empty)
    val after  = Freshness.of(started, SessionFacts.empty)
    assertEquals(after.runs, before.runs)
    assertEquals(after.banner, before.banner)
  }

  test("new origin and both command carriers require doc11/science7/journal9") {
    Vector(legacy, explicit).foreach { base =>
      val doc      = apply(base, start)
      val draft    = doc.draft.get
      val document = get(StudioDocument.ladder)
      val encoded  = get(document.codec.encode(doc))
      assertEquals(encoded.hcursor.downField("schema").get[Int]("version"), Right(11))
      assertEquals(get(document.codec.decode(encoded)), doc)
      document.versions.filter(_.version < 11).foreach { v =>
        assert(document.writeAt(v, doc).isLeft)
        assert(document.readAt(v, doc.asJson).isLeft)
        assert(
          document.codec
            .decode(
              encoded.hcursor
                .downField("schema")
                .downField("version")
                .withFocus(_ => Json.fromInt(v.version))
                .top
                .get
            )
            .isLeft
        )
      }
      val science = get(ScienceContent.ladder)
      assertEquals(
        get(science.codec.encode(doc.science)).hcursor.downField("schema").get[Int]("version"),
        Right(7)
      )
      science.versions.filter(_.version < 7).foreach { v =>
        assert(science.writeAt(v, doc.science).isLeft)
        assert(science.readAt(v, doc.science.asJson).isLeft)
      }
      val journal = get(CommandJournal.ladder)
      Vector[Command](start, Command.RestoreDraft(draft)).foreach { command =>
        val line   = JournalLine.Entry(1, JournalEntry.Apply(command))
        val stored = get(journal.codec.encode(line))
        assertEquals(stored.hcursor.downField("schema").get[Int]("version"), Right(9))
        assertEquals(get(journal.codec.decode(stored)), line)
        journal.versions.filter(_.version < 9).foreach { v =>
          assert(journal.writeAt(v, line).isLeft)
          assert(journal.readAt(v, line.asJson).isLeft)
        }
      }
    }
  }

  test(
    "bundle reopen and checkpointed journal recovery preserve provisional and saved identity"
  ) {
    Vector(legacy, explicit).foreach { base =>
      val started = apply(base, start)
      val encoded = get(
        ProjectBundle.encode(started, SharingOptions.complete, BundleSamples.inputsFor(started))
      )
      val files = encoded.parts.toMap
      assertEquals(
        get(ProjectBundle.assemble(encoded.manifest, path => Right(files(path)))).document,
        started
      )
      val entries = Vector(
        JournalEntry.Apply(start),
        JournalEntry.Undo,
        JournalEntry.Redo,
        JournalEntry.Apply(Command.DiscardDraft),
        JournalEntry.Undo,
        JournalEntry.Apply(Command.SaveAndRun(None))
      )
      val text   = get(CommandJournal.write(base, entries, checkpointEvery = 1))
      val replay = get(CommandJournal.replay(base, text))
      val direct = apply(started, Command.SaveAndRun(None))
      assertEquals(replay.history.document, direct)
      assertEquals(replay.checkpoints, (1 to entries.size).toVector)
      assertEquals(replay.torn, None)
    }
  }

  test("display names do not allocate identity and exhausted counters fail as values") {
    val sameName  = apply(explicit, start.copy(familyName = "Same name"))
    val persisted = apply(sameName, Command.SaveAndRun(None))
    assertEquals(
      persisted.analysisFamilies.get.families.map(_.name),
      Vector.fill(3)("Same name")
    )
    assertEquals(
      persisted.familyOf(persisted.analyses.last.id),
      Some(get(AnalysisFamilyId.of(3)))
    )

    def counterDocument(
        revision: AnalysisRevisionSpec,
        registry: Option[AnalysisFamilyRegistry]
    ) =
      get(
        StudioDocument.of(
          legacy.datasets,
          Vector(revision),
          None,
          Vector.empty,
          legacy.reporting,
          Vector.empty,
          PresentationState.default,
          Vector.empty,
          registry
        )
      )
    val last              = saved.copy(id = AnalysisRevision(Int.MaxValue))
    val exhaustedRevision = counterDocument(last, None)
    assertEquals(
      exhaustedRevision.nextAnalysisId,
      Left(DocumentError.ExhaustedAnalysisRevision(last.id))
    )
    assert(Reducer.run(exhaustedRevision, start).isLeft)
    assert(
      Reducer
        .run(
          exhaustedRevision,
          Command.StartDraft(
            last.id,
            None,
            Vector(RecipeChange.Grid(last.recipe.grid, get(GridSize.of(32, 24))))
          )
        )
        .isLeft
    )
    val lastFamily = get(AnalysisFamily.of(get(AnalysisFamilyId.of(Int.MaxValue)), "Last"))
    val registry   = get(
      AnalysisFamilyRegistry.of(
        Vector(lastFamily),
        Vector(get(AnalysisFamilyOwner.of(saved.id, lastFamily.id))),
        Vector(saved.id)
      )
    )
    val exhaustedFamily = counterDocument(saved, Some(registry))
    assertEquals(
      exhaustedFamily.nextFamilyId,
      Left(DocumentError.FamilyOwnership(AnalysisFamilyError.Exhausted(lastFamily.id)))
    )
    assert(Reducer.run(exhaustedFamily, start).isLeft)
  }

  test("occupied family IDs, stale revisions, blank names and unadmitted seeds are refused") {
    assert(Reducer.run(legacy, start.copy(familyName = " ")).isLeft)
    val started = apply(legacy, start)
    val before  = get(StudioDocument.encode(started))
    assert(
      Reducer
        .run(started, Command.SaveAndRun(Some(studio.copy(preset = Preset.PerceptionImagery))))
        .isLeft
    )
    assertEquals(get(StudioDocument.encode(started)), before)
    val staleDocument = started.asJson.hcursor
      .downField("draft")
      .downField("id")
      .withFocus(_ => saved.id.asJson)
      .top
      .get
    assert(staleDocument.as[StudioDocument].isLeft)
    val occupiedDocument = started.asJson.hcursor
      .downField("draft")
      .downField("origin")
      .downField("NewFamily")
      .downField("family")
      .downField("id")
      .withFocus(_ => AnalysisFamilyId.Legacy.asJson)
      .top
      .get
    assert(occupiedDocument.as[StudioDocument].isLeft)
    assert(Reducer.run(DocumentSamples.t1, start).isLeft)
    val family   = get(AnalysisFamily.of(AnalysisFamilyId.Legacy, "Duplicate"))
    val occupied =
      get(Draft.newFamily(AnalysisRevision(5), family, saved.dataset, saved.recipe, studio))
    assert(Reducer.run(legacy, Command.RestoreDraft(occupied)).isLeft)
    val next  = get(AnalysisFamily.of(get(legacy.nextFamilyId), "New"))
    val stale = get(Draft.newFamily(saved.id, next, saved.dataset, saved.recipe, studio))
    assert(Reducer.run(legacy, Command.RestoreDraft(stale)).isLeft)
    assert(
      Draft.newFamily(AnalysisRevision(0), next, saved.dataset, saved.recipe, studio).isLeft
    )
    val raw = apply(legacy, start).draft.get.asJson
    assert(raw.deepMerge(Json.obj("base" -> saved.id.asJson)).as[Draft].isLeft)
    assert(raw.mapObject(_.add("origin", Json.Null)).as[Draft].isLeft)
    assert(
      typeCheckErrors(
        "summon[scala.deriving.Mirror.ProductOf[eyes4s.studio.core.document.Draft]]"
      ).nonEmpty
    )
    assert(
      typeCheckErrors(
        "eyes4s.studio.core.document.DocumentSamples.t2.draft.get.copy(id = eyes4s.studio.core.backend.AnalysisRevision(0))"
      ).nonEmpty
    )
  }
