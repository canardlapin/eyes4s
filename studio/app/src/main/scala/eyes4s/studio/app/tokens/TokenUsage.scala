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

/** The foreground/background pairs the design draws (ticket S1.1).
  *
  * Collected from the boards in `docs/studio/design/` and DESIGN_SPEC sections
  * 4, 5, 6 and 12. `TokenContrastSuite` checks every pair in both themes and,
  * for a pair that involves the stage, on every stage variant. A new place
  * that puts a token on a surface adds its pair here.
  */
object TokenUsage:

  private def t(token: ThemedToken): TokenRef = TokenRef.Themed(token)
  private def s(token: StageToken): TokenRef  = TokenRef.Staged(token)

  import ThemedToken.*
  import StageToken.{Halo, OnStage, Stage}

  private val Body  = 12
  private val Label = 11
  private val Hero  = 28

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
        TextUse(s(OnStage), s(Stage), Label, "stage captions")
      )

  /** Every graphical-mark pair. */
  val marks: List[MarkUse] =
    val onChrome =
      for
        (mark, casing, where) <- List(
          (Query, None, "query mark (filled circle)"),
          (Match, None, "matched mark (diamond)"),
          (Control, None, "control mark (hollow circle), B tick"),
          (ControlFill, Some(t(Control)), "filled control mark, cased in control"),
          (Accent, None, "focus rule, current dot, progress bar"),
          (Ink, None, "D bar, zero rule, axes, selection ring"),
          (Ink3, None, "Forgotten group (dashed), missing-value marker"),
          (Fail, None, "failure outline"),
          (WarnText, None, "missing-asset cross")
        )
        surface <- List(Ground, Surface, Surface2)
      yield MarkUse(t(mark), t(surface), casing, s"$where on ${surface.cssName}")
    val onStage = List(
      MarkUse(t(Query), s(Stage), Some(s(Halo)), "query mark on the stage"),
      MarkUse(t(Match), s(Stage), Some(s(Halo)), "matched mark on the stage"),
      MarkUse(t(Control), s(Stage), Some(s(Halo)), "control mark on the stage"),
      MarkUse(t(NeutralMark), s(Stage), Some(s(Halo)), "Explore fixation on the stage"),
      MarkUse(t(Accent), s(Stage), Some(s(Halo)), "focused mark ring on the stage"),
      MarkUse(t(SelRingInner), s(Stage), Some(t(Ink)), "selection ring on the stage"),
      MarkUse(s(OnStage), s(Stage), None, "isolines and window outline")
    )
    onChrome ++ onStage

  /** True when a pair involves the stage, so it varies with [[StageVariant]]. */
  def onStage(refs: TokenRef*): Boolean = refs.exists {
    case TokenRef.Staged(_) => true
    case _                  => false
  }
