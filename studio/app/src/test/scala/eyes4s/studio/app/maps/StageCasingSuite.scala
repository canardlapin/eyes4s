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

package eyes4s.studio.app.maps

import eyes4s.studio.app.tokens.{
  StageToken,
  StageVariant,
  Theme,
  ThemedToken,
  TokenRef,
  TokenUsage,
  Tokens,
  Wcag
}

/** Where a stage mark wears its inner casing (review ticket 17; DESIGN_SPEC
  * section 15, "Stage casing"; owner decision 2026-10-13: only where
  * needed). Runs on the JVM and on Scala.js.
  */
class StageCasingSuite extends munit.FunSuite:

  private def combos =
    for
      theme <- Theme.values.toList
      stage <- StageVariant.values.toList
      mark  <- TokenUsage.innerCased
    yield (theme, stage, mark)

  private def label(theme: Theme, stage: StageVariant, mark: TokenRef) =
    s"${theme.toString.toLowerCase} theme, ${stage.cssName} stage, --${mark.cssName}"

  test(
    "the frame grounds are the blank screen and the stage, bare and under every map colour"
  ) {
    StageVariant.values.foreach { stage =>
      val grounds = StageCasing.frameGrounds(stage)
      assertEquals(grounds.size, 2 * (StageCasing.RampSteps + 2))
      assertEquals(grounds.map(_._1).distinct.size, grounds.size)
    }
  }

  test("with the rule applied, every stage mark reaches 3:1 on every frame ground") {
    val failures = for
      (theme, stage, mark) <- combos
      halo  = Tokens.staged(stage, StageToken.Halo)
      inner = Tokens.resolve(TokenUsage.innerCasing(stage), theme, stage)
      fill  = Tokens.resolve(mark, theme, stage)
      cased = StageCasing.needed(mark, theme, stage)
      (where, ground) <- StageCasing.frameGrounds(stage).toList
      bands = fill :: halo :: (if cased then List(inner) else Nil)
      best  = bands.map(Wcag.contrastOn(_, ground)).max
      if best < Wcag.NonTextMinimum
    yield f"${label(theme, stage, mark)} over $where: $best%.2f"
    assert(failures.isEmpty, failures.mkString("\n", "\n", ""))
  }

  test("a combination that passes without the inner casing does not get it") {
    combos.foreach { (theme, stage, mark) =>
      val halo   = Tokens.staged(stage, StageToken.Halo)
      val fill   = Tokens.resolve(mark, theme, stage)
      val passes = StageCasing.frameGrounds(stage).forall { (_, ground) =>
        math.max(Wcag.contrastOn(fill, ground), Wcag.contrastOn(halo, ground)) >=
          Wcag.NonTextMinimum
      }
      assertEquals(StageCasing.needed(mark, theme, stage), !passes, label(theme, stage, mark))
    }
    // The combinations that keep the boards' lighter marks, pinned: the query
    // mark in the light theme on the dark and mid stages, and in the dark
    // theme on the light stage. Every other mark is cased everywhere.
    val query = TokenRef.Themed(ThemedToken.Query)
    val light = for
      (theme, stage, mark) <- combos
      if !StageCasing.needed(mark, theme, stage)
    yield (theme, stage, mark)
    assertEquals(
      light.toSet,
      Set(
        (Theme.Light, StageVariant.Dark, query),
        (Theme.Light, StageVariant.Mid, query),
        (Theme.Dark, StageVariant.Light, query)
      )
    )
  }

  test("a cased combination names the ground it fails on") {
    assertEquals(
      StageCasing
        .failingGround(TokenRef.Themed(ThemedToken.Query), Theme.Dark, StageVariant.Dark),
      Some("the blank screen")
    )
    assertEquals(
      StageCasing
        .failingGround(TokenRef.Themed(ThemedToken.Query), Theme.Light, StageVariant.Dark),
      None
    )
  }
