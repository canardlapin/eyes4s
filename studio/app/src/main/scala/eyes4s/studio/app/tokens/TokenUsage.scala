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

/** Text drawn in token `text` on token `background` at `sizePx` CSS pixels. */
final case class TextUse(text: TokenRef, background: TokenRef, sizePx: Int, where: String)
    derives CanEqual

/** A graphical mark in token `mark` on token `background`.
  *
  * `casing` is a band drawn around the mark: the stage halo, or a stroke
  * around a fill. The mark is perceivable when either the mark itself or its
  * casing reaches 3:1 against the background.
  */
final case class MarkUse(
    mark: TokenRef,
    background: TokenRef,
    casing: Option[TokenRef],
    where: String
) derives CanEqual

/** A drawing rule: `mark` is never drawn without `casing` around it.
  *
  * Renderers (S1.x shell, S4 scenes) import these rather than restating them;
  * `TokenContrastSuite` proves each rule is needed and sufficient.
  */
final case class CasingRule(mark: TokenRef, casing: TokenRef, reason: String) derives CanEqual

/** The foreground/background pairs the design draws (ticket S1.1).
  *
  * Collected from the boards in `docs/studio/design/` and DESIGN_SPEC sections
  * 4, 5, 6 and 12. `TokenContrastSuite` checks every pair in both themes and,
  * for a pair that involves the stage, on every stage variant. A new place
  * that puts a token on a surface adds its pair here.
  */
object TokenUsage:

  private def t(token: ThemedToken): TokenRef  = TokenRef.Themed(token)
  private def s(token: StageToken): TokenRef   = TokenRef.Staged(token)
  private def p(token: PaletteToken): TokenRef = TokenRef.Palette(token)

  import ThemedToken.*
  import StageToken.{Halo, OnStage, Stage}
  import PaletteToken.*

  // ---------------------------------------------------------------------------
  // Drawing rules
  // ---------------------------------------------------------------------------

  /** A filled control mark is drawn inside a `--control` outline: light
    * `--control-fill` alone is 2.28 to 2.65:1 on the chrome surfaces.
    */
  val ControlFillOutline: CasingRule = CasingRule(
    t(ControlFill),
    t(Control),
    "--control-fill alone is below 3:1 on light chrome surfaces"
  )

  /** A filled control mark on paper is drawn inside a `--fig-control` outline:
    * `--fig-control-fill` alone is 2.76:1 on white paper.
    */
  val FigureControlFillOutline: CasingRule = CasingRule(
    p(FigControlFill),
    p(FigControl),
    "--fig-control-fill alone is 2.76:1 on paper"
  )

  /** A role mark on the stage is cased in `--halo` (DESIGN_SPEC section 12). */
  val StageMarkHalo: List[CasingRule] =
    List(Query, Match, Control, NeutralMark).map { m =>
      CasingRule(t(m), s(Halo), s"--${m.cssName} alone falls below 3:1 on some stage")
    }

  /** The keyboard focus ring on the stage is cased in `--halo`: light
    * `--accent` alone is 2.86:1 on the dark stage.
    */
  val StageFocusRingHalo: CasingRule = CasingRule(
    t(Accent),
    s(Halo),
    "--accent alone is below 3:1 on the dark and mid stages"
  )

  /** An isoline is `--isoline-ink` inside an `--isoline-case` band, so it
    * reads over light and dark map cells and every stage.
    */
  val IsolineCasing: CasingRule = CasingRule(
    p(IsolineInk),
    p(IsolineCase),
    "--isoline-ink alone vanishes over dark map cells and the dark stage"
  )

  /** Every drawing rule. */
  val casingRules: List[CasingRule] =
    List(ControlFillOutline, FigureControlFillOutline, StageFocusRingHalo, IsolineCasing) ++
      StageMarkHalo

  private def cased(rule: CasingRule, background: TokenRef, where: String): MarkUse =
    MarkUse(rule.mark, background, Some(rule.casing), where)

  // ---------------------------------------------------------------------------
  // Pairs
  // ---------------------------------------------------------------------------

  private val Body   = 12
  private val Label  = 11
  private val Hero   = 28
  private val Figure = 9

  private val chromeSurfaces = List(Ground, Surface, Surface2, Surface3)

  /** Every text pair. */
  val text: List[TextUse] =
    (for
      ink     <- List(Ink, Ink2, Ink3)
      surface <- chromeSurfaces
    yield TextUse(t(ink), t(surface), Label, "labels, body and hints on chrome surfaces")) ++
      List(
        TextUse(t(Ink), t(Surface), Hero, "hero number"),
        TextUse(t(Ink), t(FailSoft), Label, "Studio check block"),
        TextUse(t(Surface), t(Ink), Label, "inverted chip and tooltip"),
        TextUse(t(Accent), t(Surface), Label, "accent text links"),
        TextUse(t(Accent), t(AccentSoft), Label, "status tag"),
        TextUse(t(OnAccent), t(Accent), Body, "primary action"),
        TextUse(t(Query), t(QuerySoft), Label, "query role pill"),
        TextUse(t(Query), t(Surface), Label, "query role label"),
        TextUse(t(MatchText), t(MatchSoft), Label, "matched role pill"),
        TextUse(t(MatchText), t(Surface), Label, "matched label and M column"),
        TextUse(t(ControlText), t(ControlSoft), Label, "control role pill"),
        TextUse(t(ControlText), t(Surface), Label, "control label and B column"),
        TextUse(t(Fail), t(FailSoft), Label, "failed chip and blocker tag"),
        TextUse(t(Fail), t(Surface), Label, "Studio check title"),
        TextUse(t(WarnText), t(WarnSoft), Label, "stale chip, pending tag"),
        TextUse(t(WarnText), t(Surface), Label, "stale chip stripes"),
        TextUse(t(TitleBarText), t(TitleBar), Body, "mock title bar"),
        TextUse(s(OnStage), s(Stage), Label, "stage captions"),
        TextUse(p(PaperInk), p(Paper), Figure, "figure axes and panel letters"),
        TextUse(p(PaperInk2), p(Paper), Figure, "figure captions")
      )

  /** Every graphical-mark pair. */
  val marks: List[MarkUse] =
    val onChrome =
      for
        (mark, casing, where) <- List(
          (Query, None, "query mark (filled circle)"),
          (Match, None, "matched mark (diamond)"),
          (Control, None, "control mark (hollow circle), B tick"),
          (ControlFill, Some(ControlFillOutline.casing), "filled control mark, outlined"),
          (Accent, None, "focus rule, current dot, progress bar"),
          (Ink, None, "D bar, zero rule, axes, selection ring"),
          (Ink3, None, "Forgotten group (dashed), missing-value marker"),
          (Fail, None, "failure outline"),
          (WarnText, None, "missing-asset cross")
        )
        surface <- List(Ground, Surface, Surface2)
      yield MarkUse(t(mark), t(surface), casing, s"$where on ${surface.cssName}")
    val onStage =
      StageMarkHalo.map(r => cased(r, s(Stage), s"--${r.mark.cssName} mark on the stage")) ++
        List(
          cased(StageFocusRingHalo, s(Stage), "focused mark ring on the stage"),
          MarkUse(t(SelRingInner), s(Stage), Some(t(Ink)), "selection ring on the stage"),
          MarkUse(s(OnStage), s(Stage), None, "analysis-window outline"),
          cased(IsolineCasing, s(Stage), "isoline on the stage")
        )
    val onMaps =
      List(Ramp0, Ramp1, Ramp2, Ramp3).map { stop =>
        cased(IsolineCasing, p(stop), s"isoline over a --${stop.cssName} map cell")
      }
    val onPaper =
      List(
        MarkUse(p(FigQuery), p(Paper), None, "figure query mark"),
        MarkUse(p(FigMatch), p(Paper), None, "figure matched mark"),
        MarkUse(p(FigControl), p(Paper), None, "figure control mark (hollow circle)"),
        cased(FigureControlFillOutline, p(Paper), "figure filled control mark"),
        MarkUse(p(PaperInk), p(Paper), None, "figure zero rule and D bars")
      )
    onChrome ++ onStage ++ onMaps ++ onPaper

  /** Tokens that no check measures, and why: fills behind text, decorative
    * rules and stimulus depiction. WCAG 1.4.11 exempts rules that carry no
    * information.
    */
  val unmeasured: Map[TokenRef, String] = Map(
    t(Hairline)  -> "decorative 1px separators and card borders",
    p(PaperRule) -> "axis spines, paired-participant lines, caption rule: decorative",
    p(Screen)    -> "stimulus depiction: the blank screen a participant saw",
    p(DivNeg)    -> "map fill; values are read from the legend and table twin",
    p(DivMid)    -> "map fill; values are read from the legend and table twin",
    p(DivPos)    -> "map fill; values are read from the legend and table twin"
  )

  /** True when a pair involves the stage, so it varies with [[StageVariant]]. */
  def onStage(refs: TokenRef*): Boolean = refs.exists {
    case TokenRef.Staged(_) => true
    case _                  => false
  }
