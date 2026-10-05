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

/** Stylesheets generated from [[Tokens]] (ticket S1.1).
  *
  * Pure text: studio-desktop writes and checks the files (`TokenFiles`), so
  * this layer stays portable. Names follow the web custom properties: `--ink`
  * on the web is the JavaFX looked-up colour `-es-ink`, and `ThemedToken.Ink`
  * in Scala.
  */
object TokenCss:

  /** The JavaFX looked-up colour for a token, e.g. `-es-surface-2`. */
  def javaFxName(ref: TokenRef): String = s"-es-${ref.cssName}"

  /** The web custom property for a token, e.g. `--surface-2`. */
  def webName(ref: TokenRef): String = s"--${ref.cssName}"

  /** The JavaFX style class that selects a stage variant, e.g. `stage-mid`. */
  def stageStyleClass(stage: StageVariant): String = s"stage-${stage.cssName}"

  private[studio] val licenseHeader: String =
    """|/*
       | * Copyright 2026 canardlapin
       | *
       | * Licensed under the Apache License, Version 2.0 (the "License");
       | * you may not use this file except in compliance with the License.
       | * You may obtain a copy of the License at
       | *
       | *     http://www.apache.org/licenses/LICENSE-2.0
       | *
       | * Unless required by applicable law or agreed to in writing, software
       | * distributed under the License is distributed on an "AS IS" BASIS,
       | * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
       | * See the License for the specific language governing permissions and
       | * limitations under the License.
       | */
       |""".stripMargin

  private val generatedNote: String =
    "/* Generated from eyes4s.studio.app.tokens.Tokens (ticket S1.1). Do not edit:\n" +
      " * change Tokens.scala and run `sbt studioTokens`. */\n"

  private def block(selector: String, declarations: List[(String, String)]): String =
    declarations
      .map { case (name, value) => s"  $name: $value;\n" }
      .mkString(s"$selector {\n", "", "}\n")

  private def stageDeclarations(
      stage: StageVariant,
      name: TokenRef => String,
      value: Colour => String
  ): List[(String, String)] =
    StageToken.values.toList.map { t =>
      name(TokenRef.Staged(t)) -> value(Tokens.staged(stage, t))
    }

  // ---------------------------------------------------------------------------
  // JavaFX
  // ---------------------------------------------------------------------------

  /** The JavaFX stylesheet of one theme: every token as a looked-up colour on
    * `.root`, the default stage included, and one style class per stage
    * variant that redefines the stage tokens for the subtree it is set on.
    * Each theme's sheet is complete; a scene loads exactly one.
    */
  def javaFx(theme: Theme): String =
    val themed = ThemedToken.values.toList.map { t =>
      javaFxName(TokenRef.Themed(t)) -> Tokens.themed(theme, t).javaFxCss
    }
    val palette = PaletteToken.values.toList.map { t =>
      javaFxName(TokenRef.Palette(t)) -> Tokens.palette(t).javaFxCss
    }
    val root = themed ++
      stageDeclarations(Tokens.defaultStage, javaFxName, _.javaFxCss) ++
      palette
    val stages = StageVariant.values.toList.map { stage =>
      block(s".${stageStyleClass(stage)}", stageDeclarations(stage, javaFxName, _.javaFxCss))
    }
    val themeName = theme match
      case Theme.Light => "light"
      case Theme.Dark  => "dark"
    licenseHeader + generatedNote +
      s"\n/* Eyes Studio tokens, $themeName theme: JavaFX looked-up colours. */\n" +
      block(".root", root) +
      "\n/* Stage variants: set one of these style classes on the stage node. */\n" +
      stages.mkString("\n")

  /** The file name JavaFX loads for a theme. */
  def javaFxFileName(theme: Theme): String = theme match
    case Theme.Light => "studio.css"
    case Theme.Dark  => "studio-dark.css"

  // ---------------------------------------------------------------------------
  // Web
  // ---------------------------------------------------------------------------

  /** The selection ring as the boards write it (a CSS box-shadow). */
  def webSelectionRing(theme: Theme): String =
    val inner = Tokens.themed(theme, ThemedToken.SelRingInner).webCss
    val outer = Tokens.themed(theme, ThemedToken.Ink).webCss
    s"0 0 0 2px $inner,0 0 0 4px $outer"

  private def webThemed(theme: Theme): List[(String, String)] =
    ThemedToken.values.toList.map { t =>
      webName(TokenRef.Themed(t)) -> Tokens.themed(theme, t).webCss
    } :+ ("--sel-ring" -> webSelectionRing(theme))

  /** The web custom properties: the same selectors as the boards' token block
    * (`.es`, `.es.dark`, `[data-stage=…]`). Checked in CI, not shipped.
    */
  def web: String =
    val palette = PaletteToken.values.toList.map { t =>
      webName(TokenRef.Palette(t)) -> Tokens.palette(t).webCss
    }
    val stages = StageVariant.values.toList.map { stage =>
      val v = stage.cssName
      block(
        s"[data-stage=$v],\n.es[data-stage=$v]",
        stageDeclarations(stage, webName, _.webCss)
      )
    }
    licenseHeader + generatedNote +
      "\n/* Eyes Studio tokens as web custom properties, for a future web shell. */\n" +
      block(
        ".es",
        webThemed(Theme.Light) ++
          stageDeclarations(Tokens.defaultStage, webName, _.webCss) ++
          palette
      ) +
      "\n" + block(".es.dark", webThemed(Theme.Dark)) +
      "\n" + stages.mkString("\n")
