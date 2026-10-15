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

import eyes4s.plan.ExpectedUnmatchedRule
import eyes4s.studio.core.command.{Command, History, Reducer}
import eyes4s.studio.core.fixture.StoryMoments
import io.circe.syntax.*

class ExpectedUnmatchedDocumentSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val sample                            = get(StoryMoments.t2)
  private val rule    = get(ExpectedUnmatchedRule.of("old_new", Vector("new", "lure")))
  private val change  = RecipeChange.ExpectedUnmatched(None, Some(rule))
  private def changed = get(Reducer.run(sample, Command.ChangeRecipe(change))).document

  test("selection changes are undoable and saved recipes retain the declared rule") {
    val before = History.start(sample)
    val edited = get(before.apply(Command.ChangeRecipe(change))).history
    assertEquals(edited.document.draftContext.flatMap(_.recipe.expectedUnmatched), Some(rule))
    assertEquals(get(edited.undo).history.document, sample)
    val saved = get(Reducer.run(edited.document, Command.SaveAndRun(None))).document
    assertEquals(saved.analyses.last.recipe.expectedUnmatched, Some(rule))
    assertEquals(get(StudioDocument.decode(get(StudioDocument.encode(saved)))), saved)
  }

  test("document v12 and science v8 preserve the rule and old schema versions refuse it") {
    val document = changed
    val ladder   = get(StudioDocument.ladder)
    val json     = get(ladder.codec.encode(document))
    assertEquals(json.hcursor.downField("schema").get[Int]("version"), Right(12))
    assertEquals(get(ladder.codec.decode(json)), document)
    ladder.versions.filter(_.version < 12).foreach { v =>
      assert(ladder.writeAt(v, document).isLeft, v.toString)
      assert(ladder.readAt(v, document.asJson).isLeft, v.toString)
    }
    val science = get(ScienceContent.ladder)
    assertEquals(
      get(science.codec.encode(document.science)).hcursor
        .downField("schema")
        .get[Int]("version"),
      Right(8)
    )
    science.versions.filter(_.version < 8).foreach { v =>
      assert(science.writeAt(v, document.science).isLeft, v.toString)
      assert(science.readAt(v, document.science.asJson).isLeft, v.toString)
    }
  }

  test(
    "recipes without a selection keep their legacy shape; invalid declarations refuse decoding"
  ) {
    val recipe = sample.analyses.last.recipe
    assert(!recipe.asJson.hcursor.downField("expectedUnmatched").succeeded)
    assertEquals(get(recipe.asJson.as[Recipe]), recipe)
    val malformed = recipe.asJson.mapObject(
      _.add(
        "expectedUnmatched",
        io.circe.Json.obj(
          "column" -> io.circe.Json.fromString(""),
          "values" -> io.circe.Json.arr(io.circe.Json.fromString("new"))
        )
      )
    )
    assert(malformed.as[Recipe].isLeft)
  }

  test("explicit fixation coordinates survive saving and are refused by older schemas") {
    val before = sample.draftContext.get.recipe.initialFixations
    val after  = InitialFixationChoice.DropLeadingAtScreenCross(
      get(ScreenCross.of(960, 540)),
      get(CrossRadius.of(12))
    )
    val document = get(
      Reducer.run(sample, Command.ChangeRecipe(RecipeChange.InitialFixations(before, after)))
    ).document
    assertEquals(get(StudioDocument.decode(get(StudioDocument.encode(document)))), document)
    val ladder = get(StudioDocument.ladder)
    ladder.versions
      .filter(_.version < 12)
      .foreach(v => assert(ladder.writeAt(v, document).isLeft))
    val science = get(ScienceContent.ladder)
    science.versions
      .filter(_.version < 8)
      .foreach(v => assert(science.writeAt(v, document.science).isLeft))
    assert(ScreenCross.of(Double.NaN, 540).isLeft)
  }
