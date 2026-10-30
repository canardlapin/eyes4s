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

import eyes4s.studio.app.geometry.DensityPicture
import eyes4s.studio.app.maps.{DensityBand, DensityCasing, DensityGround, StageCasing}

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
  // inner casing inside it where StageCasing applies it. `inner = false`
  // measures the halo alone.
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
        (!inner || !StageCasing.needed(mark, theme, stage) || in < Wcag.NonTextMinimum)
    yield f"${label(theme, stage, true)}: --${mark.cssName} on --${ground.cssName}: " +
      f"mark $m%.2f, halo $halo%.2f, inner casing $in%.2f < ${Wcag.NonTextMinimum}%.1f"

  test(
    "over map and image grounds, a stage mark or focus ring reaches 3:1 as StageCasing cases it"
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

  // ---------------------------------------------------------------------------
  // The placement density (DESIGN_SPEC section 15.1, "Contrast"; option B)
  // ---------------------------------------------------------------------------

  /** The least CIE L* step between two adjacent density levels, opaque: they
    * stay distinct steps of one ramp. Measured in L*, not as a contrast
    * ratio: the v4 ramp is evenly spaced in lightness (12.76, 11.02, 11.68),
    * and a ratio compresses its dark end (1.59, 1.50, 1.46) although the
    * steps look equal (owner decision 2026-10-30: the golden's ramp stands).
    */
  private val DensityLevelSeparation = 10.0

  private val densityLevels = (1 to DensityPicture.Levels).toList

  private def densityFill(level: Int): Colour = Tokens.palette(DensityCasing.fill(level))

  private def densityCases =
    for
      stage  <- StageVariant.values.toList
      ground <- DensityGround.values.toList
      level  <- densityLevels
    yield (stage, ground, level)

  // A density level against one ground of its stage: its opaque fill, and
  // the one band DensityCasing gives its edges there. The fill is opaque, so
  // a theme does not change it. `cased = false` measures the fill alone.
  private def densityFailures(cased: Boolean): List[String] =
    for
      (stage, ground, level) <- densityCases
      fill = DensityCasing.fillContrast(level, stage, ground)
      band =
        if cased then DensityCasing.band(level, stage, ground)
        else None
      best = (fill :: band.map(DensityCasing.bandContrast(_, stage, ground)).toList).max
      if best < Wcag.NonTextMinimum
    yield f"${stage.cssName} stage: level $level (--${DensityCasing.fill(level).cssName}) " +
      f"on ${ground.where}: fill $fill%.2f, best $best%.2f < ${Wcag.NonTextMinimum}%.1f"

  test("every density level reaches 3:1 on every stage and on the screen, as cased") {
    val failures = densityFailures(cased = true)
    assert(failures.isEmpty, failures.mkString("\n", "\n", ""))
    // The band itself carries the contrast against its ground.
    for
      (stage, ground, level) <- densityCases
      band                   <- DensityCasing.band(level, stage, ground)
    do
      assert(
        DensityCasing.bandContrast(band, stage, ground) >= Wcag.NonTextMinimum,
        s"${stage.cssName} stage, level $level on ${ground.where}: $band"
      )
  }

  test("a density edge is cased only where its level's fill is below 3:1 on its ground") {
    // Derived, not tabulated: cased iff the opaque fill fails on that ground,
    // with the band of higher contrast against it.
    densityCases.foreach { (stage, ground, level) =>
      val fails = DensityCasing.fillContrast(level, stage, ground) < Wcag.NonTextMinimum
      val best  = DensityBand.values.maxBy(DensityCasing.bandContrast(_, stage, ground))
      assertEquals(
        DensityCasing.band(level, stage, ground),
        Option.when(fails)(best),
        s"${stage.cssName} stage, level $level on ${ground.where}"
      )
    }
    // What today's tokens give, lowest level first. The v4 ramp is darker
    // than the screen and the mid stage: the halo cases the dark levels on
    // the Dark stage and the light ones on Mid; on the screen, the light
    // levels take the inner casing on Dark and Mid (white --halo is 2.82:1
    // there) and the halo on Light. Nothing is cased on the Light stage.
    import DensityBand.{Halo, InnerCasing}
    val none2 = Vector.fill[Option[DensityBand]](2)(None)
    val table = densityCases
      .groupMap((stage, ground, _) => (stage, ground))((stage, ground, level) =>
        DensityCasing.band(level, stage, ground)
      )
      .view
      .mapValues(_.toVector)
      .toMap
    assertEquals(
      table,
      Map(
        (StageVariant.Dark, DensityGround.Stage)  -> (None +: Vector.fill(3)(Some(Halo))),
        (StageVariant.Dark, DensityGround.Screen) -> (Vector.fill(2)(
          Some(InnerCasing)
        ) ++ none2),
        (StageVariant.Mid, DensityGround.Stage)  -> Vector.fill(3)(Some(Halo)).:+(None),
        (StageVariant.Mid, DensityGround.Screen) -> (Vector.fill(2)(
          Some(InnerCasing)
        ) ++ none2),
        (StageVariant.Light, DensityGround.Stage)  -> Vector.fill(4)(None),
        (StageVariant.Light, DensityGround.Screen) -> (Vector.fill(2)(Some(Halo)) ++ none2)
      )
    )
    // Without its band, a level falls below 3:1: a Dark-stage singleton over
    // the blank screen (1.49), >=64 on the Dark stage (1.19), a Mid-stage
    // singleton on the stage (1.08).
    val bare = densityFailures(cased = false)
    assert(
      bare.exists(_.startsWith("dark stage: level 1 (--ramp-0) on the blank screen")),
      bare
    )
    assert(bare.exists(_.startsWith("dark stage: level 4 (--ramp-3) on the stage")), bare)
    assert(bare.exists(_.startsWith("mid stage: level 1 (--ramp-0) on the stage")), bare)
    val pinned = List(
      (StageVariant.Dark, DensityGround.Screen, 1, 1.49),
      (StageVariant.Dark, DensityGround.Stage, 4, 1.19),
      (StageVariant.Mid, DensityGround.Stage, 1, 1.08)
    )
    pinned.foreach { (stage, ground, level, expected) =>
      assertEqualsDouble(
        DensityCasing.fillContrast(level, stage, ground),
        expected,
        QuotedPlaces
      )
    }
  }

  test("a density level whose fill reads on its ground is not cased there") {
    // Dark 1-3 on the stage (4.14), Mid >=64 on the stage (3.22), Light 1-3
    // and >=64 on the stage (3.75, 13.07), 16-63 on the screen (3.55).
    List(
      (StageVariant.Dark, DensityGround.Stage, 1, 4.14),
      (StageVariant.Mid, DensityGround.Stage, 4, 3.22),
      (StageVariant.Light, DensityGround.Stage, 1, 3.75),
      (StageVariant.Light, DensityGround.Stage, 4, 13.07),
      (StageVariant.Light, DensityGround.Screen, 3, 3.55)
    ).foreach { (stage, ground, level, expected) =>
      assertEquals(DensityCasing.band(level, stage, ground), None, s"${stage.cssName} $level")
      assertEqualsDouble(
        DensityCasing.fillContrast(level, stage, ground),
        expected,
        QuotedPlaces
      )
    }
  }

  test("a density band's contrast against its ground is pinned") {
    // Halo on the stages: 17.40 (Dark), 4.54 (Mid), 14.92 (Light); on the
    // screen, the inner casing 5.07 (Dark, Mid) beats white --halo 2.82, and
    // ink --halo 5.92 (Light) beats the white inner casing 1.95.
    List(
      (StageVariant.Dark, DensityGround.Stage, DensityBand.Halo, 17.40),
      (StageVariant.Mid, DensityGround.Stage, DensityBand.Halo, 4.54),
      (StageVariant.Light, DensityGround.Stage, DensityBand.Halo, 14.92),
      (StageVariant.Dark, DensityGround.Screen, DensityBand.InnerCasing, 5.07),
      (StageVariant.Dark, DensityGround.Screen, DensityBand.Halo, 2.82),
      (StageVariant.Mid, DensityGround.Screen, DensityBand.InnerCasing, 5.07),
      (StageVariant.Light, DensityGround.Screen, DensityBand.Halo, 5.92),
      (StageVariant.Light, DensityGround.Screen, DensityBand.InnerCasing, 1.95)
    ).foreach { (stage, ground, band, expected) =>
      assertEqualsDouble(
        DensityCasing.bandContrast(band, stage, ground),
        expected,
        QuotedPlaces,
        s"${stage.cssName} $band on ${ground.where}"
      )
    }
  }

  test("adjacent opaque density levels stay apart and ordered by luminance") {
    val luminance = densityLevels.map(l =>
      Wcag.relativeLuminance(densityFill(l)).fold(e => fail(e.message), identity)
    )
    // Lighter to darker, level 1 first: the order survives greyscale.
    assertEquals(luminance, luminance.sorted.reverse)
    assertEquals(luminance.distinct.size, luminance.size)
    val steps = densityLevels.zip(densityLevels.drop(1)).map { (a, b) =>
      lstar(densityFill(a)) - lstar(densityFill(b))
    }
    steps.zipWithIndex.foreach { (d, i) =>
      assert(d >= DensityLevelSeparation, f"levels ${i + 1} and ${i + 2}: $d%.2f L*")
    }
    List(12.76, 11.02, 11.68).zip(steps).foreach { (expected, measured) =>
      assertEqualsDouble(measured, expected, QuotedPlaces)
    }
  }

  test("mid stage: white captions reach 4.5:1 and L* stays near 50") {
    val mid = Tokens.staged(StageVariant.Mid, StageToken.Stage)
    assert(ratio(Tokens.staged(StageVariant.Mid, StageToken.OnStage), mid) >= Wcag.TextMinimum)
    assertEqualsDouble(lstar(mid), 50.0, 1.0)
  }

  /** The least CIE L* between query and matched in either theme. The marks
    * differ in shape (circle, diamond) and the pills carry their names, so
    * lightness is a second channel, not the only one: 12 L* keeps the pair
    * apart in greyscale (owner decision 2026-10-30: the golden's v4 roles,
    * 15.0 light and 13.3 dark, stand; the former law asked 20).
    */
  private val RoleLightnessSpread = 12.0

  test("query and matched stay at least 12 L* apart in both themes") {
    Theme.values.foreach { theme =>
      val q = lstar(Tokens.themed(theme, ThemedToken.Query))
      val m = lstar(Tokens.themed(theme, ThemedToken.Match))
      assert(
        math.abs(q - m) >= RoleLightnessSpread,
        s"${theme.toString.toLowerCase} query/matched spread ${math.abs(q - m)} L*"
      )
    }
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
