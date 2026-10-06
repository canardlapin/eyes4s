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

import eyes4s.studio.core.backend.{AnalysisRevision, DatasetRevision, RunId}
import eyes4s.studio.core.command.{
  Command,
  CommandJournal,
  Effect,
  History,
  JournalEntry,
  JournalLine,
  Reducer
}
import eyes4s.studio.core.fixture.StoryMoments
import io.circe.syntax.*

class InitialDraftSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val sample                            = get(StoryMoments.t2)
  private val data  = sample.datasets.last.copy(id = DatasetRevision(1), parent = None)
  private val saved = sample.analyses.last
  private val empty = get(
    StudioDocument.of(
      Vector(data),
      Vector.empty,
      None,
      Vector.empty,
      Vector.empty,
      Vector.empty,
      PresentationState.default,
      Vector.empty
    )
  )
  private val start   = Command.StartAnalysis(data.id, saved.recipe, saved.studio)
  private def initial = get(Reducer.run(empty, start)).document

  test(
    "first working recipe creates no saved base and SaveAndRun creates the first revision and run"
  ) {
    val before = initial
    assertEquals(before.analyses, Vector.empty)
    assertEquals(before.runs, Vector.empty)
    assertEquals(before.draft.flatMap(_.savedBase), None)
    assertEquals(
      before.draftContext.map(c => (c.id, c.dataset, c.recipe)),
      Some((AnalysisRevision(1), data.id, saved.recipe))
    )
    val decoded   = get(before.asJson.as[StudioDocument])
    val completed = get(Reducer.run(decoded, Command.SaveAndRun(None)))
    assertEquals(completed.document.analyses.map(_.id), Vector(AnalysisRevision(1)))
    assertEquals(completed.document.runs.map(_.id), Vector(RunId(1)))
    assertEquals(completed.document.draft, None)
    assertEquals(completed.document.analyses.head.recipe, saved.recipe)
    assertEquals(completed.document.analyses.head.studio, saved.studio)
    assertEquals(
      completed.effects,
      Vector(Effect.RequestRun(RunId(1), AnalysisRevision(1), data.id), Effect.Persist)
    )
    assert(Reducer.run(completed.document, start).isLeft)
  }

  test("initial recipe edits revert and undo without discarding the working recipe") {
    val started  = get(History.start(empty).apply(start)).history
    val change   = RecipeChange.Grid(saved.recipe.grid, get(GridSize.of(32, 24)))
    val changed  = get(started.apply(Command.ChangeRecipe(change))).history
    val reverted = get(changed.apply(Command.ChangeRecipe(change.inverse))).history
    assertEquals(reverted.document.draft, started.document.draft)
    assertEquals(get(changed.undo).history.document.draft, started.document.draft)
    assertEquals(
      get(get(started.undo).history.redo).history.document.draft,
      started.document.draft
    )
  }

  test(
    "initial origins use conditional new schema versions; every older writer and reader refuses"
  ) {
    val document = initial
    val ladder   = get(StudioDocument.ladder)
    val encoded  = get(ladder.codec.encode(document))
    assertEquals(encoded.hcursor.downField("schema").get[Int]("version"), Right(7))
    assertEquals(ladder.codec.decode(encoded), Right(document))
    ladder.versions.dropRight(1).foreach { version =>
      assert(ladder.writeAt(version, document).isLeft)
      assert(ladder.readAt(version, document.asJson).isLeft)
    }
    val science = get(ScienceContent.ladder)
    assertEquals(
      get(science.codec.encode(document.science)).hcursor
        .downField("schema")
        .get[Int]("version"),
      Right(3)
    )
    science.versions.dropRight(1).foreach { version =>
      assert(science.writeAt(version, document.science).isLeft)
      assert(science.readAt(version, document.science.asJson).isLeft)
    }
    val journal = get(CommandJournal.ladder)
    Vector(start, Command.RestoreDraft(document.draft.get)).foreach { command =>
      val line = JournalLine.Entry(1, JournalEntry.Apply(command))
      assertEquals(
        get(journal.codec.encode(line)).hcursor.downField("schema").get[Int]("version"),
        Right(4)
      )
      assertEquals(journal.codec.decode(get(journal.codec.encode(line))), Right(line))
      journal.versions.dropRight(1).foreach { version =>
        assert(journal.writeAt(version, line).isLeft)
        assert(journal.readAt(version, line.asJson).isLeft)
      }
    }
    val legacy = sample.draft.get.asJson
    assert(legacy.hcursor.downField("base").succeeded)
    assert(!legacy.hcursor.downField("origin").succeeded)
    assertEquals(legacy.as[Draft], Right(sample.draft.get))
  }

  test("an initial origin refuses wrong first identity and coexistence with saved revisions") {
    val wrong = get(Draft.initial(AnalysisRevision(2), data.id, saved.recipe, saved.studio))
    assert(
      StudioDocument
        .of(
          empty.datasets,
          Vector.empty,
          Some(wrong),
          Vector.empty,
          Vector.empty,
          Vector.empty,
          empty.presentation,
          Vector.empty
        )
        .isLeft
    )
    assert(
      StudioDocument
        .of(
          sample.datasets,
          sample.analyses,
          initial.draft,
          Vector.empty,
          Vector.empty,
          Vector.empty,
          empty.presentation,
          Vector.empty
        )
        .isLeft
    )
  }

  test("an initial seed must hold its declared preset; an explicit custom seed remains valid") {
    val invalid = saved.recipe.copy(phases =
      PhasePair(
        eyes4s.studio.core.backend.Phase("Imagery"),
        eyes4s.studio.core.backend.Phase("Perception")
      )
    )
    val id = AnalysisRevision(1)
    assertEquals(
      Draft.initial(id, data.id, invalid, saved.studio),
      Left(DocumentError.InitialPresetNotHeld(id, saved.studio.preset))
    )
    val custom   = saved.studio.copy(preset = Preset.Custom)
    val working  = get(Draft.initial(id, data.id, invalid, custom))
    val document = get(
      StudioDocument.of(
        empty.datasets,
        Vector.empty,
        Some(working),
        Vector.empty,
        Vector.empty,
        Vector.empty,
        empty.presentation,
        Vector.empty
      )
    )
    val decoded   = get(StudioDocument.decode(get(StudioDocument.encode(document))))
    val completed = get(Reducer.run(decoded, Command.SaveAndRun(None))).document
    assertEquals(completed.analyses.head.recipe, invalid)
    assertEquals(completed.analyses.head.studio, custom)
  }

  test(
    "invalid initial presets are refused through raw and relabelled document/science/journal payloads"
  ) {
    val invalid = saved.recipe.copy(phases =
      PhasePair(
        eyes4s.studio.core.backend.Phase("Imagery"),
        eyes4s.studio.core.backend.Phase("Perception")
      )
    )
    def corrupt(json: io.circe.Json) = json.hcursor
      .downField("draft")
      .downField("origin")
      .downField("Initial")
      .downField("seedRecipe")
      .withFocus(_ => invalid.asJson)
      .top
      .get
    val raw = corrupt(initial.asJson)
    assert(raw.as[StudioDocument].isLeft)
    get(StudioDocument.ladder).versions.foreach(version =>
      assert(get(StudioDocument.ladder).readAt(version, raw).isLeft)
    )
    val science = corrupt(initial.science.asJson)
    get(ScienceContent.ladder).versions.foreach(version =>
      assert(get(ScienceContent.ladder).readAt(version, science).isLeft)
    )
    val command = get(io.circe.parser.parse("""{"RestoreDraft":{"draft":null}}""")).hcursor
      .downField("RestoreDraft")
      .downField("draft")
      .withFocus(_ => raw.hcursor.downField("draft").focus.get)
      .top
      .get
    assert(command.as[Command].isLeft)
    val validLine = (JournalLine.Entry(
      1,
      JournalEntry.Apply(Command.RestoreDraft(initial.draft.get))
    ): JournalLine).asJson
    val badLine = validLine.hcursor
      .downField("Entry")
      .downField("entry")
      .downField("Apply")
      .downField("command")
      .withFocus(_ => command)
      .top
      .get
    get(CommandJournal.ladder).versions.foreach(version =>
      assert(get(CommandJournal.ladder).readAt(version, badLine).isLeft)
    )
  }

  test(
    "existing drafts accept only the legacy base representation, including older envelopes"
  ) {
    val tagged = sample.draft.get.asJson.mapObject(
      _.add("origin", (DraftOrigin.Existing(AnalysisRevision(3)): DraftOrigin).asJson)
    )
    assert(tagged.as[Draft].left.toOption.exists(_.message.contains("tagged Existing")))
    assert(tagged.mapObject(_.remove("base")).as[Draft].isLeft)
    val raw = sample.asJson.mapObject(_.add("draft", tagged))
    get(StudioDocument.ladder).versions.foreach(version =>
      assert(get(StudioDocument.ladder).readAt(version, raw).isLeft)
    )
    val science = sample.science.asJson.mapObject(_.add("draft", tagged))
    get(ScienceContent.ladder).versions.foreach(version =>
      assert(get(ScienceContent.ladder).readAt(version, science).isLeft)
    )
  }
