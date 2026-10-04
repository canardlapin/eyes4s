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

/** The chrome theme. Light is the default; dark is maintained alongside it. */
enum Theme derives CanEqual:
  case Light, Dark

/** The image-stage surround (DESIGN_SPEC section 4, "Appearance"). It is
  * independent of the chrome theme: either theme can show any stage.
  */
enum StageVariant(val cssName: String) derives CanEqual:

  /** The default surround, `--stage` as the boards define it. */
  case Dark  extends StageVariant("dark")
  case Mid   extends StageVariant("mid")
  case Light extends StageVariant("light")

/** A colour token whose value depends on the chrome [[Theme]]. `cssName` is
  * the CSS custom property without its leading `--`.
  */
enum ThemedToken(val cssName: String) derives CanEqual:
  case Ground      extends ThemedToken("ground")
  case Surface     extends ThemedToken("surface")
  case Surface2    extends ThemedToken("surface-2")
  case Surface3    extends ThemedToken("surface-3")
  case Hairline    extends ThemedToken("hairline")
  case Ink         extends ThemedToken("ink")
  case Ink2        extends ThemedToken("ink-2")
  case Ink3        extends ThemedToken("ink-3")
  case Accent      extends ThemedToken("accent")
  case AccentSoft  extends ThemedToken("accent-soft")
  case OnAccent    extends ThemedToken("on-accent")
  case Query       extends ThemedToken("query")
  case QuerySoft   extends ThemedToken("query-soft")
  case Match       extends ThemedToken("match")
  case MatchSoft   extends ThemedToken("match-soft")
  case MatchText   extends ThemedToken("match-text")
  case Control     extends ThemedToken("control")
  case ControlFill extends ThemedToken("control-fill")
  case ControlText extends ThemedToken("control-text")
  case ControlSoft extends ThemedToken("control-soft")
  case Fail        extends ThemedToken("fail")
  case FailSoft    extends ThemedToken("fail-soft")
  case WarnText    extends ThemedToken("warn-text")
  case WarnSoft    extends ThemedToken("warn-soft")

  /** Explore's role-free fixation fill; translucent. */
  case NeutralMark extends ThemedToken("neutral-mark")

  /** The inner band of the selection ring; the outer band is [[Ink]]. */
  case SelRingInner extends ThemedToken("sel-ring-inner")

  /** The mock macOS title bar of the boards. The native bar cannot be tinted;
    * these exist for a web shell that draws its own.
    */
  case TitleBar     extends ThemedToken("tl-bg")
  case TitleBarText extends ThemedToken("tl-text")

/** A colour token whose value depends on the [[StageVariant]] only. */
enum StageToken(val cssName: String) derives CanEqual:

  /** The surround drawn around the image frame. */
  case Stage extends StageToken("stage")

  /** Captions, isolines and outlines drawn directly on the stage. */
  case OnStage extends StageToken("on-stage")

  /** The casing drawn around a mark on the stage (DESIGN_SPEC section 12). */
  case Halo extends StageToken("halo")

/** A data-palette colour: the same in every theme and on every stage. */
enum PaletteToken(val cssName: String) derives CanEqual:

  /** Mass-map ramp stops, light to dark (DESIGN_SPEC section 6). */
  case Ramp0 extends PaletteToken("ramp-0")
  case Ramp1 extends PaletteToken("ramp-1")
  case Ramp2 extends PaletteToken("ramp-2")
  case Ramp3 extends PaletteToken("ramp-3")

  /** The diverging BlueRust ramp for difference maps; zero is pinned to
    * [[DivMid]].
    */
  case DivNeg extends PaletteToken("div-neg")
  case DivMid extends PaletteToken("div-mid")
  case DivPos extends PaletteToken("div-pos")

  /** The grey that depicts a blank experiment screen in display-kind art. */
  case Screen extends PaletteToken("screen")

  /** Cased isolines (DESIGN_SPEC section 12): a translucent ink line inside a
    * translucent white casing, legible over any map cell and any stage.
    */
  case IsolineInk  extends PaletteToken("isoline-ink")
  case IsolineCase extends PaletteToken("isoline-case")

  /** The journal-figure page (Figures board): white paper whatever the theme. */
  case Paper     extends PaletteToken("paper")
  case PaperInk  extends PaletteToken("paper-ink")
  case PaperInk2 extends PaletteToken("paper-ink-2")

  /** Axis spines, paired-participant lines and the caption rule. */
  case PaperRule extends PaletteToken("paper-rule")

  /** Role marks on paper. */
  case FigQuery   extends PaletteToken("fig-query")
  case FigMatch   extends PaletteToken("fig-match")
  case FigControl extends PaletteToken("fig-control")

  /** Filled control marks on paper: only inside a [[FigControl]] outline. */
  case FigControlFill extends PaletteToken("fig-control-fill")

/** Any token, for code that treats the three families alike. */
enum TokenRef derives CanEqual:
  case Themed(token: ThemedToken)
  case Staged(token: StageToken)
  case Palette(token: PaletteToken)

  /** The CSS custom property name without its leading `--`. */
  def cssName: String = this match
    case Themed(t)  => t.cssName
    case Staged(t)  => t.cssName
    case Palette(t) => t.cssName

/** The mass-map ramp, lightest stop first. */
final case class SequentialRamp(stops: List[Colour]) derives CanEqual

/** A diverging ramp with its zero pinned to `zero`. */
final case class DivergingRamp(negative: Colour, zero: Colour, positive: Colour)
    derives CanEqual

/** The one source of every Eyes Studio colour (DESIGN_SPEC sections 4, 5, 6, 12;
  * ticket S1.1).
  *
  * Values are the token block of `docs/studio/design/Main.dc.html` (`.es`,
  * `.es.dark`, `[data-stage]`), the data palettes of `System.dc.html` and the
  * paper palette of `Figures.dc.html`, with changes that `TokenContrastSuite`
  * requires and the ticket records:
  *
  *   - the mid stage is `#767676`, not `#808080`, so that its white captions
  *     reach 4.54:1 (3.95:1 on `#808080`); its L* stays near 50 (coordinator
  *     decision, S1.1 review);
  *   - `halo` is a stage token: white on the dark and mid stages as the boards
  *     draw it, ink on the light stage, where a white halo is 1.12:1 and a
  *     dark-theme query mark would otherwise vanish (1.35:1);
  *   - `fig-control` is the figure-safe `#B86E0E` (3.98:1 on paper); the
  *     board's `#D98A1C` (2.76:1) becomes `fig-control-fill`, drawn only inside
  *     a `fig-control` outline;
  *   - `isoline-ink` and `isoline-case` are new (DESIGN_SPEC section 12).
  *
  * Everything else is generated from here: the JavaFX looked-up colours
  * (`studio.css`, `studio-dark.css`), the web custom properties
  * (`docs/studio/tokens/studio-tokens.css`) and, directly, the constants that
  * Intaglio scenes read. `NoLiteralColourLint` rejects a colour literal
  * anywhere else in the studio sources.
  */
object Tokens:

  /** The stage every theme opens with. */
  val defaultStage: StageVariant = StageVariant.Dark

  /** A themed token's value. */
  def themed(theme: Theme, token: ThemedToken): Colour =
    theme match
      case Theme.Light => light(token)
      case Theme.Dark  => dark(token)

  /** A stage token's value. */
  def staged(stage: StageVariant, token: StageToken): Colour =
    (stage, token) match
      case (StageVariant.Dark, StageToken.Stage)    => Colour.srgb(0x1a1a1a)
      case (StageVariant.Dark, StageToken.OnStage)  => Colour.srgb(0xd9d6cf)
      case (StageVariant.Dark, StageToken.Halo)     => Colour.srgb(0xffffff)
      case (StageVariant.Mid, StageToken.Stage)     => Colour.srgb(0x767676)
      case (StageVariant.Mid, StageToken.OnStage)   => Colour.srgb(0xffffff)
      case (StageVariant.Mid, StageToken.Halo)      => Colour.srgb(0xffffff)
      case (StageVariant.Light, StageToken.Stage)   => Colour.srgb(0xf2f2f2)
      case (StageVariant.Light, StageToken.OnStage) => Colour.srgb(0x1c1e21)
      case (StageVariant.Light, StageToken.Halo)    => Colour.srgb(0x1c1e21)

  /** A data-palette colour. */
  def palette(token: PaletteToken): Colour =
    token match
      case PaletteToken.Ramp0          => Colour.srgb(0xf9d9ea)
      case PaletteToken.Ramp1          => Colour.srgb(0xe58fb9)
      case PaletteToken.Ramp2          => Colour.srgb(0xb83f86)
      case PaletteToken.Ramp3          => Colour.srgb(0x6b0f4f)
      case PaletteToken.DivNeg         => Colour.srgb(0x2e5a9c)
      case PaletteToken.DivMid         => Colour.srgb(0xf3f1ec)
      case PaletteToken.DivPos         => Colour.srgb(0xb4442f)
      case PaletteToken.Screen         => Colour.srgb(0x9a9a96)
      case PaletteToken.IsolineInk     => Colour.srgba(0x1c1e21, 90)
      case PaletteToken.IsolineCase    => Colour.srgba(0xffffff, 90)
      case PaletteToken.Paper          => Colour.srgb(0xffffff)
      case PaletteToken.PaperInk       => Colour.srgb(0x1c1e21)
      case PaletteToken.PaperInk2      => Colour.srgb(0x464b52)
      case PaletteToken.PaperRule      => Colour.srgb(0xc9c5bc)
      case PaletteToken.FigQuery       => Colour.srgb(0x3a2e6e)
      case PaletteToken.FigMatch       => Colour.srgb(0x2f6db5)
      case PaletteToken.FigControl     => Colour.srgb(0xb86e0e)
      case PaletteToken.FigControlFill => Colour.srgb(0xd98a1c)

  /** Any token's value in a theme, on a stage. */
  def resolve(ref: TokenRef, theme: Theme, stage: StageVariant): Colour =
    ref match
      case TokenRef.Themed(t)  => themed(theme, t)
      case TokenRef.Staged(t)  => staged(stage, t)
      case TokenRef.Palette(t) => palette(t)

  /** The mass-map ramp (DESIGN_SPEC section 6). */
  val massRamp: SequentialRamp = SequentialRamp(
    List(PaletteToken.Ramp0, PaletteToken.Ramp1, PaletteToken.Ramp2, PaletteToken.Ramp3)
      .map(palette)
  )

  /** The BlueRust difference-map ramp, zero pinned (DESIGN_SPEC section 6). */
  val differenceRamp: DivergingRamp = DivergingRamp(
    palette(PaletteToken.DivNeg),
    palette(PaletteToken.DivMid),
    palette(PaletteToken.DivPos)
  )

  /** Every token of the three families, in declaration order. */
  val all: List[TokenRef] =
    ThemedToken.values.toList.map(TokenRef.Themed(_)) ++
      StageToken.values.toList.map(TokenRef.Staged(_)) ++
      PaletteToken.values.toList.map(TokenRef.Palette(_))

  private def light(token: ThemedToken): Colour =
    token match
      case ThemedToken.Ground       => Colour.srgb(0xece9e2)
      case ThemedToken.Surface      => Colour.srgb(0xfbfaf7)
      case ThemedToken.Surface2     => Colour.srgb(0xf3f1ec)
      case ThemedToken.Surface3     => Colour.srgb(0xe9e6df)
      case ThemedToken.Hairline     => Colour.srgb(0xdad6cc)
      case ThemedToken.Ink          => Colour.srgb(0x1c1e21)
      case ThemedToken.Ink2         => Colour.srgb(0x464b52)
      case ThemedToken.Ink3         => Colour.srgb(0x5e636a)
      case ThemedToken.Accent       => Colour.srgb(0x0e6e68)
      case ThemedToken.AccentSoft   => Colour.srgb(0xdcefec)
      case ThemedToken.OnAccent     => Colour.srgb(0xffffff)
      case ThemedToken.Query        => Colour.srgb(0x3a2e6e)
      case ThemedToken.QuerySoft    => Colour.srgb(0xe6e2f2)
      case ThemedToken.Match        => Colour.srgb(0x2f6db5)
      case ThemedToken.MatchSoft    => Colour.srgb(0xe0eaf6)
      case ThemedToken.MatchText    => Colour.srgb(0x255a98)
      case ThemedToken.Control      => Colour.srgb(0xb86e0e)
      case ThemedToken.ControlFill  => Colour.srgb(0xd98a1c)
      case ThemedToken.ControlText  => Colour.srgb(0x9a4a07)
      case ThemedToken.ControlSoft  => Colour.srgb(0xf7e7d0)
      case ThemedToken.Fail         => Colour.srgb(0xa3261b)
      case ThemedToken.FailSoft     => Colour.srgb(0xf6deda)
      case ThemedToken.WarnText     => Colour.srgb(0x7a4f00)
      case ThemedToken.WarnSoft     => Colour.srgb(0xfbf1d9)
      case ThemedToken.NeutralMark  => Colour.srgba(0x1c1e21, 35)
      case ThemedToken.SelRingInner => Colour.srgb(0xffffff)
      case ThemedToken.TitleBar     => Colour.srgb(0xececec)
      case ThemedToken.TitleBarText => Colour.srgb(0x4d4d4d)

  private def dark(token: ThemedToken): Colour =
    token match
      case ThemedToken.Ground       => Colour.srgb(0x121315)
      case ThemedToken.Surface      => Colour.srgb(0x1b1c1f)
      case ThemedToken.Surface2     => Colour.srgb(0x222327)
      case ThemedToken.Surface3     => Colour.srgb(0x2a2c31)
      case ThemedToken.Hairline     => Colour.srgb(0x34363c)
      case ThemedToken.Ink          => Colour.srgb(0xeceae5)
      case ThemedToken.Ink2         => Colour.srgb(0xbdbab3)
      case ThemedToken.Ink3         => Colour.srgb(0x9c9990)
      case ThemedToken.Accent       => Colour.srgb(0x43b5aa)
      case ThemedToken.AccentSoft   => Colour.srgb(0x183a37)
      case ThemedToken.OnAccent     => Colour.srgb(0x0b1413)
      case ThemedToken.Query        => Colour.srgb(0xd6cbff)
      case ThemedToken.QuerySoft    => Colour.srgb(0x2b2544)
      case ThemedToken.Match        => Colour.srgb(0x5c95e0)
      case ThemedToken.MatchSoft    => Colour.srgb(0x1d2a3d)
      case ThemedToken.MatchText    => Colour.srgb(0x7fb0ee)
      case ThemedToken.Control      => Colour.srgb(0xe8a23c)
      case ThemedToken.ControlFill  => Colour.srgb(0xe8a23c)
      case ThemedToken.ControlText  => Colour.srgb(0xebb066)
      case ThemedToken.ControlSoft  => Colour.srgb(0x3a2a14)
      case ThemedToken.Fail         => Colour.srgb(0xf08a7e)
      case ThemedToken.FailSoft     => Colour.srgb(0x3a1e1b)
      case ThemedToken.WarnText     => Colour.srgb(0xe7c274)
      case ThemedToken.WarnSoft     => Colour.srgb(0x33290f)
      case ThemedToken.NeutralMark  => Colour.srgba(0xeceae5, 35)
      case ThemedToken.SelRingInner => Colour.srgb(0x000000)
      case ThemedToken.TitleBar     => Colour.srgb(0x2b2b2b)
      case ThemedToken.TitleBarText => Colour.srgb(0xb0b0b0)
