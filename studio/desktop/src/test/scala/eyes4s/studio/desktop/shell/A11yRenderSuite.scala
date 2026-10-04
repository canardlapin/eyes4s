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

package eyes4s.studio.desktop.shell

import eyes4s.studio.app.appearance.Appearance
import eyes4s.studio.app.tokens.{Theme, ThemedToken, Tokens, Wcag}
import eyes4s.studio.app.{AppModel, StoryModels}
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.FxStage
import javafx.scene.control.{Menu, RadioMenuItem}
import javafx.scene.layout.Region
import javafx.scene.paint.Color
import javafx.scene.text.{FontWeight, Text}
import javafx.scene.{Node, Parent}
import scala.jdk.CollectionConverters.*

/** The rendered window against WCAG (ticket S10.5; DESIGN_SPEC §10): every
  * shown, enabled text in every perspective, light and dark, reaches its
  * contrast minimum over what is drawn behind it, and every Tab stop changes
  * how it is drawn when focused. TokenContrastSuite checks the token pairs
  * the spec names; this checks the pairs the window actually draws.
  */
class A11yRenderSuite extends ShellFxSuite:

  private val perspectives: Vector[(String, () => AppModel, StoryMoment)] = Vector(
    ("data", () => StoryModels.t1Data, StoryMoment.T1),
    ("data-empty", () => StoryModels.firstRun, StoryMoment.T2),
    ("explore", () => StoryModels.t2Explore, StoryMoment.T2),
    ("analysis", () => StoryModels.t2Analysis, StoryMoment.T2),
    ("compare", () => StoryModels.t2Compare, StoryMoment.T2),
    ("compare-t3", () => StoryModels.t3Summary, StoryMoment.T3),
    ("figures", () => StoryModels.t2Figures, StoryMoment.T2)
  )

  private def all(n: Node): Vector[Node] = n +: (n match
    case p: Parent => p.getChildrenUnmodifiable.asScala.toVector.flatMap(all)
    case _         => Vector.empty)

  private def ancestors(n: Node): Iterator[Node] =
    Iterator.iterate(n)(_.getParent).takeWhile(_ != null)

  private def shown(n: Node): Boolean =
    ancestors(n).forall(a => a.isVisible && a.getOpacity > 0) && {
      val b = n.localToScene(n.getBoundsInLocal)
      b.getWidth > 0 && b.getHeight > 0 && b.getMaxX > 0 && b.getMaxY > 0
    }

  private def dark(fx: FxStage, w: StudioWindow): Unit =
    runOnFx {
      val view = w.shell.menus.find(_.getText == "View").getOrElse(fail("no View menu"))
      view.getItems.asScala
        .collectFirst { case m: Menu if m.getText == "Appearance" => m }
        .flatMap(
          _.getItems.asScala.collectFirst {
            case r: RadioMenuItem
                if r.getId == s"view.appearance-${Appearance.Dark.toString.toLowerCase}" =>
              r
          }
        )
        .getOrElse(fail("no View › Appearance › Dark"))
        .fire()
    }
    fx.awaitLayout()

  // --- WCAG 2.x relative luminance, on JavaFX colours -------------------------

  private type Rgb = (Double, Double, Double)

  private def over(fg: Color, alpha: Double, bg: Rgb): Rgb =
    (
      fg.getRed * alpha + bg._1 * (1 - alpha),
      fg.getGreen * alpha + bg._2 * (1 - alpha),
      fg.getBlue * alpha + bg._3 * (1 - alpha)
    )

  private def luminance(c: Rgb): Double =
    def lin(v: Double) = if v <= 0.04045 then v / 12.92 else math.pow((v + 0.055) / 1.055, 2.4)
    0.2126 * lin(c._1) + 0.7152 * lin(c._2) + 0.0722 * lin(c._3)

  private def ratio(a: Rgb, b: Rgb): Double =
    val (x, y) = (luminance(a), luminance(b))
    (math.max(x, y) + 0.05) / (math.min(x, y) + 0.05)

  private def fx(c: eyes4s.studio.app.tokens.Colour): Color =
    Color.rgb(c.red, c.green, c.blue, c.alphaPercent / 100.0)

  /** The colours a fill can draw: a colour, or each stop of a gradient. */
  private def colours(p: javafx.scene.paint.Paint): Option[Vector[Color]] = p match
    case c: Color                             => Some(Vector(c))
    case g: javafx.scene.paint.LinearGradient =>
      Some(g.getStops.asScala.toVector.map(_.getColor))
    case g: javafx.scene.paint.RadialGradient =>
      Some(g.getStops.asScala.toVector.map(_.getColor))
    case _ => None

  /** What can be drawn behind `n`: its ancestors' fills, nearest first,
    * composited down to the first opaque one (the scene's fill below all).
    * A gradient contributes each of its stops, so every colour it reaches is
    * checked; a fill that is neither (an image) is not resolved.
    */
  private def behind(n: Node, scene: javafx.scene.Scene): Either[String, Vector[Rgb]] =
    val layers = Vector.newBuilder[Vector[Color]]
    var opaque = false
    val it     = ancestors(n.getParent)
    var bad    = Option.empty[String]
    while !opaque && bad.isEmpty && it.hasNext do
      it.next() match
        case r: Region if r.getBackground != null =>
          r.getBackground.getFills.asScala.reverseIterator.takeWhile(_ => !opaque).foreach {
            f =>
              colours(f.getFill) match
                case Some(cs) if cs.exists(_.getOpacity > 0) =>
                  layers += cs
                  if cs.forall(_.getOpacity >= 1) then opaque = true
                case Some(_) => ()
                case None    => bad = Some(s"fill ${f.getFill} in ${r.getStyleClass}")
          }
        case _ => ()
    bad.toLeft {
      val base: Rgb = scene.getFill match
        case c: Color => (c.getRed, c.getGreen, c.getBlue)
        case _        => (1.0, 1.0, 1.0)
      layers
        .result()
        .reverse
        .foldLeft(Vector(base))((acc, cs) =>
          for
            below <- acc
            c     <- cs
          yield over(c, c.getOpacity, below)
        )
    }

  private def path(n: Node): String =
    ancestors(n).toVector.reverse
      .map(a =>
        a.getStyleClass.asScala.headOption
          .orElse(Option(a.getId))
          .getOrElse(a.getClass.getSimpleName)
      )
      .takeRight(4)
      .mkString(" > ")

  private def large(t: Text): Boolean =
    val f    = t.getFont
    val bold =
      Option(FontWeight.findByName(f.getStyle.split(' ').last)).exists(_.getWeight >= 700) ||
        f.getStyle.toLowerCase.contains("bold")
    f.getSize >= Wcag.LargeTextPx || (bold && f.getSize >= 18.66)

  /** Every shown, enabled text below the minimum: what it says, where, its
    * ratio and the minimum.
    */
  private def lowContrast(w: StudioWindow, scene: javafx.scene.Scene): Vector[String] =
    runOnFx {
      all(w.root)
        .collect {
          case t: Text
              if t.getText != null && t.getText.trim.nonEmpty && shown(t) && !t.isDisabled =>
            t
        }
        .flatMap { t =>
          val alpha = ancestors(t).map(_.getOpacity).product
          t.getFill match
            case fg: Color =>
              behind(t, scene) match
                case Left(why) =>
                  Vector(s"unresolved: '${t.getText.take(40)}' at ${path(t)}: $why")
                case Right(bgs) =>
                  // The worst colour the text can be drawn over.
                  val (r, bg) = bgs
                    .map(bg => (ratio(over(fg, fg.getOpacity * alpha, bg), bg), bg))
                    .minBy(_._1)
                  val min = if large(t) then Wcag.LargeTextMinimum else Wcag.TextMinimum
                  Option
                    .when(r < min - 1e-9)(
                      f"${r}%.2f < $min: '${t.getText.take(40)}' at ${path(t)} (${fg} on ${bg})"
                    )
                    .toVector
            case other =>
              Vector(s"unresolved: '${t.getText.take(40)}' at ${path(t)}: fill $other")
        }
        .distinct
    }

  test("the local luminance agrees with Wcag on a token pair") {
    val (ink3, surface) = (
      Tokens.themed(Theme.Light, ThemedToken.Ink3),
      Tokens.themed(Theme.Light, ThemedToken.Surface)
    )
    val ours = ratio(
      (fx(ink3).getRed, fx(ink3).getGreen, fx(ink3).getBlue),
      (fx(surface).getRed, fx(surface).getGreen, fx(surface).getBlue)
    )
    // JavaFX keeps a colour's channels as floats.
    val FloatChannels = 1e-6
    assertEqualsDouble(
      ours,
      Wcag.contrast(ink3, surface).fold(e => fail(e.message), identity),
      FloatChannels
    )
  }

  for
    (name, model, moment) <- perspectives
    theme                 <- Vector("light", "dark")
  do
    fxStage.test(s"$name, $theme: every shown text reaches its contrast minimum") { fx =>
      assumeFullStage(fx)
      val w = boot(fx, model(), moment)
      if theme == "dark" then dark(fx, w)
      val found = lowContrast(w, fx.scene)
      val texts = runOnFx(all(w.root).count {
        case t: Text =>
          t.getText != null && t.getText.trim.nonEmpty && shown(t) && !t.isDisabled
        case _ => false
      })
      // The walk reads the window's texts, not an empty scene.
      assert(texts >= 20, s"only $texts texts shown")
      assertEquals(found, Vector.empty[String])
    }

  /** Every node Tab can reach: focus-traversable, enabled and shown. */
  private def focusable(w: StudioWindow): Vector[Node] = runOnFx(
    all(w.root).filter(n => n.isFocusTraversable && !n.isDisabled && shown(n))
  )

  private val Focused     = javafx.css.PseudoClass.getPseudoClass("focused")
  private val FocusWithin = javafx.css.PseudoClass.getPseudoClass("focus-within")

  /** How `n`, what it draws and what encloses it are drawn: background fills
    * and border strokes, under CSS as it stands.
    */
  private def drawn(n: Node): Vector[String] =
    n.getScene.getRoot.applyCss()
    (ancestors(n).toVector ++ all(n).drop(1).take(8)).collect { case r: Region =>
      Option(r.getBackground).toVector
        .flatMap(_.getFills.asScala.map(f => s"${f.getFill}@${f.getInsets}")) ++
        Option(r.getBorder).toVector
          .flatMap(_.getStrokes.asScala.map(s => s"${s.getTopStroke}/${s.getWidths}"))
    }.flatten

  /** `n` drawn focused or not: `:focused` on it and `:focus-within` on it
    * and what encloses it, as JavaFX sets them for the focus owner. A
    * headless window cannot be sure to hold the OS focus that JavaFX shows
    * them under, so they are set directly (as A11yTreeSuite does).
    */
  private def as(n: Node, focused: Boolean): Vector[String] =
    n.pseudoClassStateChanged(Focused, focused)
    ancestors(n).foreach(_.pseudoClassStateChanged(FocusWithin, focused))
    drawn(n)

  /** The Tab stops drawn the same focused and not. */
  private def unmarked(w: StudioWindow): Vector[String] = runOnFx {
    focusable(w).flatMap { n =>
      val (before, after) = (as(n, false), as(n, true))
      // Back to the state JavaFX holds.
      n.pseudoClassStateChanged(Focused, n.isFocused)
      ancestors(n).foreach(a => a.pseudoClassStateChanged(FocusWithin, a.isFocusWithin))
      Option.when(before == after)(
        s"${n.getAccessibleRole}: ${n.getAccessibleText.take(60)} at ${path(n)}"
      )
    }
  }

  for
    (name, model, moment) <- perspectives
    theme                 <- Vector("light", "dark")
  do
    fxStage.test(s"$name, $theme: every Tab stop is drawn differently when focused") { fx =>
      assumeFullStage(fx)
      val w = boot(fx, model(), moment)
      if theme == "dark" then dark(fx, w)
      val stops = focusable(w)
      assert(stops.size >= 3, s"only ${stops.size} stops")
      assertEquals(unmarked(w), Vector.empty[String])
    }
