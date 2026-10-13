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

package eyes4s.studio.app.tokens

/** WCAG contrast of every token pair the design draws, in both themes and on
  * every stage (ticket S1.1). Runs on the JVM and on Scala.js.
  */
class TokenContrastSuite extends munit.FunSuite:

  /** Reference values are quoted to two decimals; this is half a unit of the
    * last quoted place.
    */
  private val QuotedPlaces = 0.005

  private def ratio(fg: Colour, bg: Colour): Double =
    Wcag.contrast(fg, bg).fold(e => fail(e.message), identity)

  private def lstar(c: Colour): Double =
    Wcag.lightness(c).fold(e => fail(e.message), identity)

  private val white = Colour.srgb(0xffffff)
  private val black = Colour.srgb(0x000000)

  // One (theme, stage) context per pair: the stage varies only when a pair
  // involves it, so non-stage pairs are not reported three times.
  private def contexts(onStage: Boolean): List[(Theme, StageVariant)] =
    for
      theme <- Theme.values.toList
      stage <- if onStage then StageVariant.values.toList else List(Tokens.defaultStage)
    yield (theme, stage)

  private def label(theme: Theme, stage: StageVariant, onStage: Boolean): String =
    val th = theme.toString.toLowerCase
    if onStage then s"$th theme, ${stage.cssName} stage" else s"$th theme"

  private def textFailures(uses: List[TextUse]): List[String] =
    for
      use <- uses
      onStage = TokenUsage.onStage(use.text, use.background)
      (theme, stage) <- contexts(onStage)
      fg  = Tokens.resolve(use.text, theme, stage)
      bg  = Tokens.resolve(use.background, theme, stage)
      r   = ratio(fg, bg)
      min = Wcag.textMinimum(use.sizePx)
      if r < min
    yield f"${label(theme, stage, onStage)}: --${use.text.cssName} (${fg.webCss}) on " +
      f"--${use.background.cssName} (${bg.webCss}) at ${use.sizePx}px, ${use.where}: " +
      f"$r%.2f < $min%.1f"

  private def markFailures(uses: List[MarkUse]): List[String] =
    for
      use <- uses
      onStage = TokenUsage.onStage((use.mark :: use.background :: use.casing.toList)*)
      (theme, stage) <- contexts(onStage)
      bg     = Tokens.resolve(use.background, theme, stage)
      mark   = ratio(Tokens.resolve(use.mark, theme, stage), bg)
      casing = use.casing.map(c => ratio(Tokens.resolve(c, theme, stage), bg))
      if mark < Wcag.NonTextMinimum && casing.forall(_ < Wcag.NonTextMinimum)
    yield f"${label(theme, stage, onStage)}: ${use.where}: mark $mark%.2f" +
      casing.fold("")(c => f", casing $c%.2f") + f" < ${Wcag.NonTextMinimum}%.1f"

  // ---------------------------------------------------------------------------
  // The arithmetic, against independent reference values
  // ---------------------------------------------------------------------------

  test("contrast ratio matches WCAG reference values") {
    assertEqualsDouble(ratio(black, white), 21.0, 1e-9)
    assertEqualsDouble(ratio(white, white), 1.0, 1e-12)
    // #767676 on white is the canonical smallest 4.5:1 grey (WebAIM: 4.54).
    assertEqualsDouble(ratio(Colour.srgb(0x767676), white), 4.54, QuotedPlaces)
    assertEqualsDouble(ratio(Colour.srgb(0x777777), white), 4.48, QuotedPlaces)
    // The ratio is symmetric in its two opaque operands.
    val (a, b) = (Colour.srgb(0x0e6e68), Colour.srgb(0xdcefec))
    assertEqualsDouble(ratio(a, b), ratio(b, a), 1e-12)
  }

  test("a translucent foreground is composited over its background") {
    // 50% black over white is sRGB 0.5 grey: 3.98:1 (#808080, at 128/255, is 3.95).
    assertEqualsDouble(ratio(Colour.srgba(0x000000, 50), white), 3.98, QuotedPlaces)
    assertEqualsDouble(ratio(Colour.srgba(0x000000, 0), white), 1.0, 1e-12)
  }

  test("a translucent background is refused and named") {
    val veil = Colour.srgba(0x1c1e21, 35)
    assertEquals(Wcag.contrast(white, veil), Left(WcagError.TranslucentBackground(veil)))
    assert(clue(WcagError.TranslucentBackground(veil).message).contains("rgba(28,30,33,.35)"))
    assertEquals(Wcag.lightness(veil), Left(WcagError.TranslucentColour(veil)))
  }

  test("CIE L* matches reference values") {
    assertEqualsDouble(lstar(white), 100.0, 1e-9)
    assertEqualsDouble(lstar(black), 0.0, 1e-12)
    // #777777 has L* 50.03 (sRGB, D65).
    assertEqualsDouble(lstar(Colour.srgb(0x777777)), 50.03, QuotedPlaces)
  }

  test("the large-text threshold starts at 24px") {
    assertEquals(Wcag.textMinimum(23), Wcag.TextMinimum)
    assertEquals(Wcag.textMinimum(24), Wcag.LargeTextMinimum)
  }

  // ---------------------------------------------------------------------------
  // The design's pairs
  // ---------------------------------------------------------------------------

  test("every text pair meets 4.5:1 (3:1 from 24px) in both themes, on every stage") {
    val failures = textFailures(TokenUsage.text)
    assert(failures.isEmpty, failures.mkString("\n", "\n", ""))
  }

  test("every graphical mark meets 3:1 in both themes, on every stage") {
    val failures = markFailures(TokenUsage.marks)
    assert(failures.isEmpty, failures.mkString("\n", "\n", ""))
  }

  test("role pills sit on their own soft backgrounds") {
    import ThemedToken.*
    val pills = List(Query -> QuerySoft, MatchText -> MatchSoft, ControlText -> ControlSoft)
    pills.foreach { case (fg, bg) =>
      assert(
        TokenUsage.text.exists(u =>
          u.text == TokenRef.Themed(fg) && u.background == TokenRef.Themed(bg)
        ),
        s"no text pair --${fg.cssName} on --${bg.cssName}"
      )
    }
  }

  test("text on a selected row is measured on --sel-fill") {
    import ThemedToken.*
    assert(
      TokenUsage.selectedRowText.contains(Ink) && TokenUsage.selectedRowText.contains(Ink2)
    )
    TokenUsage.selectedRowText.foreach { fg =>
      assert(
        TokenUsage.text.exists(u =>
          u.text == TokenRef.Themed(fg) && u.background == TokenRef.Themed(SelFill)
        ),
        s"no text pair --${fg.cssName} on --sel-fill"
      )
    }
  }

  test("--neutral-mark is the theme's ink at alpha .35, never an opaque grey") {
    // DESIGN_SPEC section 5; review ticket 11 settled the token over the
    // board's former opaque --ink-3.
    Theme.values.foreach { theme =>
      val mark = Tokens.themed(theme, ThemedToken.NeutralMark)
      val ink  = Tokens.themed(theme, ThemedToken.Ink)
      assertEquals(mark.alphaPercent, 35, s"$theme")
      assertEquals(mark.hexRgb, ink.hexRgb, s"$theme")
    }
  }

  test("hover (--surface-2) never reads as selection (--sel-fill)") {
    // No contrast threshold applies to two fills (WCAG 1.4.11 measures a
    // control against its surroundings); the rule is only that they differ,
    // and selection also takes weight 600 (DESIGN_SPEC section 15).
    Theme.values.foreach { theme =>
      val sel   = Tokens.themed(theme, ThemedToken.SelFill)
      val hover = Tokens.themed(theme, ThemedToken.Surface2)
      assertNotEquals(sel, hover, s"$theme: --sel-fill equals --surface-2")
      assert(
        ratio(sel, hover) > 1.0,
        s"$theme: --sel-fill and --surface-2 have equal luminance"
      )
    }
  }

  test("every token is measured, or is listed as unmeasured with a reason") {
    val foregrounds = TokenUsage.text.map(_.text) ++ TokenUsage.marks.map(_.mark) ++
      TokenUsage.marks.flatMap(_.casing)
    val measured = (foregrounds ++ TokenUsage.text.map(_.background) ++
      TokenUsage.marks.map(_.background)).toSet
    val missing = Tokens.all.filterNot(r => measured(r) || TokenUsage.unmeasured.contains(r))
    assertEquals(missing.map(_.cssName), Nil)
    // An unmeasured token is never drawn as a foreground.
    assertEquals(
      TokenUsage.unmeasured.keys.toList.filter(foregrounds.toSet).map(_.cssName),
      Nil
    )
  }

  test("every casing rule is applied, needed and sufficient") {
    TokenUsage.casingRules.foreach { rule =>
      val uses =
        TokenUsage.marks.filter(u => u.mark == rule.mark && u.casing.contains(rule.casing))
      assert(uses.nonEmpty, s"rule for --${rule.mark.cssName} is never applied")
      assertEquals(markFailures(uses), Nil)
      assert(
        markFailures(uses.map(_.copy(casing = None))).nonEmpty,
        s"--${rule.mark.cssName} passes without its --${rule.casing.cssName} casing: " +
          "the rule is not needed"
      )
    }
  }

  // A stage mark over the image frame (DESIGN_SPEC section 15, "Stage
  // casing"): the markFailures rule with two bands, the stage halo and the
  // inner casing inside it. `inner = false` measures the halo alone.
  private def frameFailures(inner: Boolean): List[String] =
    for
      theme  <- Theme.values.toList
      stage  <- StageVariant.values.toList
      ground <- TokenUsage.mapGrounds ++ TokenUsage.imageGrounds
      mark   <- TokenUsage.innerCased
      bg   = Tokens.resolve(ground, theme, stage)
      m    = ratio(Tokens.resolve(mark, theme, stage), bg)
      halo = ratio(Tokens.staged(stage, StageToken.Halo), bg)
      in   = ratio(Tokens.resolve(TokenUsage.innerCasing(stage), theme, stage), bg)
      if m < Wcag.NonTextMinimum && halo < Wcag.NonTextMinimum &&
        (!inner || in < Wcag.NonTextMinimum)
    yield f"${label(theme, stage, true)}: --${mark.cssName} on --${ground.cssName}: " +
      f"mark $m%.2f, halo $halo%.2f, inner casing $in%.2f < ${Wcag.NonTextMinimum}%.1f"

  test(
    "over map and image grounds, a stage mark or focus ring reaches 3:1 with its inner casing"
  ) {
    // Both themes x three stages x (map cells, the blank screen).
    val failures = frameFailures(inner = true)
    assert(failures.isEmpty, failures.mkString("\n", "\n", ""))
  }

  test("the inner casing is needed: the stage halo alone leaves marks below 3:1") {
    val bare = frameFailures(inner = false)
    // White --halo on the blank screen, ink --halo on the darkest map cell.
    assert(bare.exists(_.startsWith("dark theme, dark stage: --query on --screen")), bare)
    assert(bare.exists(_.startsWith("light theme, light stage: --query on --ramp-3")), bare)
    assert(bare.exists(_.contains("--accent on")), bare)
  }

  test("the inner casing stands off its halo, and the cross reads on every frame ground") {
    StageVariant.values.foreach { stage =>
      val halo  = Tokens.staged(stage, StageToken.Halo)
      val inner = Tokens.resolve(TokenUsage.innerCasing(stage), Theme.Light, stage)
      assert(
        ratio(inner, halo) >= Wcag.NonTextMinimum,
        s"${stage.cssName}: ${ratio(inner, halo)}"
      )
      // The fixation cross: --halo ink over the inner-casing tone.
      (TokenUsage.mapGrounds ++ TokenUsage.imageGrounds).foreach { ground =>
        val bg = Tokens.resolve(ground, Theme.Light, stage)
        assert(
          math.max(ratio(halo, bg), ratio(inner, bg)) >= Wcag.NonTextMinimum,
          s"${stage.cssName} stage: cross on --${ground.cssName}"
        )
      }
    }
  }

  test("mid stage: white captions reach 4.5:1 and L* stays near 50") {
    val mid = Tokens.staged(StageVariant.Mid, StageToken.Stage)
    assert(ratio(Tokens.staged(StageVariant.Mid, StageToken.OnStage), mid) >= Wcag.TextMinimum)
    assertEqualsDouble(lstar(mid), 50.0, 1.0)
  }

  test("the dark theme separates query and matched by at least 20 L*") {
    val spreads = Theme.values.toList.map { theme =>
      val q = lstar(Tokens.themed(theme, ThemedToken.Query))
      val m = lstar(Tokens.themed(theme, ThemedToken.Match))
      theme -> math.abs(q - m)
    }.toMap
    assert(spreads(Theme.Dark) >= 20.0, s"dark query/matched spread ${spreads(Theme.Dark)} L*")
    assert(
      spreads(Theme.Light) >= 20.0,
      s"light query/matched spread ${spreads(Theme.Light)} L*"
    )
  }

  // ---------------------------------------------------------------------------
  // The checks bite: planted pairs the design must never use
  // ---------------------------------------------------------------------------

  test("the text check rejects a pair below its threshold") {
    import ThemedToken.*
    val planted = TextUse(TokenRef.Themed(Hairline), TokenRef.Themed(Surface), 11, "planted")
    assertEquals(textFailures(List(planted)).size, Theme.values.size)
    val large = TextUse(TokenRef.Themed(Control), TokenRef.Themed(Ground), 11, "planted")
    // Light --control on --ground is 3.28: body text fails, large text passes.
    assertEquals(textFailures(List(large)).size, 1)
    assertEquals(textFailures(List(large.copy(sizePx = 24))), Nil)
  }

  test("the mark check rejects an uncased mark below 3:1 and honours a casing") {
    import ThemedToken.*
    // Light --control-fill is 2.28 to 2.65 on the chrome surfaces: only its
    // --control casing makes it perceivable.
    val bare = MarkUse(TokenRef.Themed(ControlFill), TokenRef.Themed(Surface), None, "planted")
    assertEquals(markFailures(List(bare)).size, 1)
    assertEquals(markFailures(List(bare.copy(casing = Some(TokenRef.Themed(Control))))), Nil)
    // Without its halo, the dark-theme query mark disappears on the light stage.
    val uncased = MarkUse(TokenRef.Themed(Query), TokenRef.Staged(StageToken.Stage), None, "x")
    assert(markFailures(List(uncased)).exists(_.startsWith("dark theme, light stage")))
  }
