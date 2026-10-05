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

import eyes4s.studio.app.tokens.Wcag
import javafx.scene.control.Labeled
import javafx.scene.layout.Region
import javafx.scene.paint.Color
import javafx.scene.text.{FontWeight, Text}
import javafx.scene.{AccessibleRole, Node, Parent, Scene}
import scala.jdk.CollectionConverters.*

/** The accessibility checks of a drawn scene graph (ticket S10.5), for any
  * root: the window, the import wizard, a popped-out dock window. Call them
  * on the JavaFX thread. Each returns what fails, named and placed, so a
  * suite asserts it is empty.
  */
object A11yChecks:

  def all(n: Node): Vector[Node] = n +: (n match
    case p: Parent => p.getChildrenUnmodifiable.asScala.toVector.flatMap(all)
    case _         => Vector.empty)

  def ancestors(n: Node): Iterator[Node] =
    Iterator.iterate(n)(_.getParent).takeWhile(_ != null)

  /** Shown: visible and not transparent all the way up, with an area. */
  def shown(n: Node): Boolean =
    ancestors(n).forall(a => a.isVisible && a.getOpacity > 0) && {
      val b = n.localToScene(n.getBoundsInLocal)
      b.getWidth > 0 && b.getHeight > 0 && b.getMaxX > 0 && b.getMaxY > 0
    }

  /** Where `n` is: the last four of its ancestors' first style classes. */
  def path(n: Node): String =
    ancestors(n).toVector.reverse
      .map(a =>
        a.getStyleClass.asScala.headOption
          .orElse(Option(a.getId))
          .getOrElse(a.getClass.getSimpleName)
      )
      .takeRight(4)
      .mkString(" > ")

  /** Every node Tab can reach: focus-traversable, enabled and shown. */
  def focusable(root: Parent): Vector[Node] =
    all(root).filter(n => n.isFocusTraversable && !n.isDisabled && shown(n))

  /** Shown, enabled texts with something to read. */
  def texts(root: Parent): Vector[Text] =
    all(root).collect {
      case t: Text
          if t.getText != null && t.getText.trim.nonEmpty && shown(t) && !t.isDisabled =>
        t
    }

  // --- names ------------------------------------------------------------------

  /** Tab stops without an accessible role and name, and labelled controls
    * whose name does not hold their drawn words (A11yTreeSuite's audit).
    */
  def unlabelled(root: Parent): Vector[String] =
    val stops    = focusable(root)
    val nameless = stops.collect {
      case n
          if Option(n.getAccessibleText).forall(_.trim.isEmpty) ||
            n.getAccessibleRole == AccessibleRole.NODE =>
        s"no name or role: $n at ${path(n)}"
    }
    val unread = stops.collect {
      case l: Labeled
          if l.getText != null && l.getText.nonEmpty &&
            !Option(l.getAccessibleText).exists(_.contains(l.getText)) =>
        s"name '${l.getAccessibleText}' does not say '${l.getText}' at ${path(l)}"
    }
    nameless ++ unread

  /** One action shown twice by design, the same name for the same function
    * (WCAG 3.2.4): the jobs chip's action, mirrored in the status bar.
    */
  private def mirrored(ns: Vector[Node]): Boolean =
    ns.size == 2 && ns.exists(n => ancestors(n).exists(_.getStyleClass.contains("jobs"))) &&
      ns.exists(n => ancestors(n).exists(_.getStyleClass.contains("status-job")))

  /** Tab stops that share a role and an accessible name: a screen reader
    * user cannot tell them apart. Each repeated name, with where it is.
    */
  def ambiguous(root: Parent): Vector[String] =
    focusable(root)
      .groupBy(n => (n.getAccessibleRole, Option(n.getAccessibleText).getOrElse("")))
      .collect {
        case ((role, name), ns) if ns.size > 1 && !mirrored(ns) =>
          s"$role '$name' ×${ns.size}: ${ns.map(path).mkString("; ")}"
      }
      .toVector
      .sorted

  // --- contrast ---------------------------------------------------------------

  type Rgb = (Double, Double, Double)

  def rgb(c: Color): Rgb = (c.getRed, c.getGreen, c.getBlue)

  def over(fg: Color, alpha: Double, bg: Rgb): Rgb =
    (
      fg.getRed * alpha + bg._1 * (1 - alpha),
      fg.getGreen * alpha + bg._2 * (1 - alpha),
      fg.getBlue * alpha + bg._3 * (1 - alpha)
    )

  /** WCAG 2.x relative luminance. */
  def luminance(c: Rgb): Double =
    def lin(v: Double) = if v <= 0.04045 then v / 12.92 else math.pow((v + 0.055) / 1.055, 2.4)
    0.2126 * lin(c._1) + 0.7152 * lin(c._2) + 0.0722 * lin(c._3)

  def ratio(a: Rgb, b: Rgb): Double =
    val (x, y) = (luminance(a), luminance(b))
    (math.max(x, y) + 0.05) / (math.min(x, y) + 0.05)

  /** The colours a fill can draw: a colour, or each stop of a gradient. */
  private def colours(p: javafx.scene.paint.Paint): Option[Vector[Color]] = p match
    case c: Color                             => Some(Vector(c))
    case g: javafx.scene.paint.LinearGradient =>
      Some(g.getStops.asScala.toVector.map(_.getColor))
    case g: javafx.scene.paint.RadialGradient =>
      Some(g.getStops.asScala.toVector.map(_.getColor))
    case _ => None

  private def mix(a: Rgb, b: Rgb, alpha: Double): Rgb =
    (
      a._1 * alpha + b._1 * (1 - alpha),
      a._2 * alpha + b._2 * (1 - alpha),
      a._3 * alpha + b._3 * (1 - alpha)
    )

  /** The pixels of `t` as drawn: a pixel of its glyph and the pixel beside
    * it, for each colour its backdrop can take. From the scene's fill down
    * the chain from the root to `t`, each region paints its fills, and each
    * node's opacity mixes everything it draws (fills, children, the glyph)
    * over what was below it, as JavaFX composites a node. A gradient
    * contributes each of its stops; a fill that is neither a colour nor a
    * gradient (an image) is not resolved.
    */
  def pixels(t: Text, fg: Color, scene: Scene): Either[String, Vector[(Rgb, Rgb)]] =
    val chain = ancestors(t).toVector.reverse
    def at(i: Int, below: Rgb): Either[String, Vector[(Rgb, Rgb)]] =
      val n                                    = chain(i)
      val painted: Either[String, Vector[Rgb]] = n match
        case r: Region if r.getBackground != null =>
          r.getBackground.getFills.asScala
            .foldLeft(Right(Vector(below)): Either[String, Vector[Rgb]]) { (acc, f) =>
              acc.flatMap(cur =>
                colours(f.getFill)
                  .toRight(s"fill ${f.getFill} in ${r.getStyleClass}")
                  .map(cs => cur.flatMap(c0 => cs.map(c => over(c, c.getOpacity, c0))))
              )
            }
        case _ => Right(Vector(below))
      val inner: Either[String, Vector[(Rgb, Rgb)]] =
        if i == chain.size - 1 then painted.map(_.map(p => (over(fg, fg.getOpacity, p), p)))
        else
          painted.flatMap(ps =>
            ps.foldLeft(Right(Vector.empty): Either[String, Vector[(Rgb, Rgb)]])((acc, p) =>
              acc.flatMap(v => at(i + 1, p).map(v ++ _))
            )
          )
      val o = n.getOpacity
      inner.map(_.map((glyph, beside) => (mix(glyph, below, o), mix(beside, below, o))))
    val base: Rgb = scene.getFill match
      case c: Color => rgb(c)
      case _        => (1.0, 1.0, 1.0)
    at(0, base).map(_.distinct)

  /** Large text (WCAG): 24 px, or 18.66 px bold. */
  private def large(t: Text): Boolean =
    val f    = t.getFont
    val bold =
      Option(FontWeight.findByName(f.getStyle.split(' ').last)).exists(_.getWeight >= 700) ||
        f.getStyle.toLowerCase.contains("bold")
    f.getSize >= Wcag.LargeTextPx || (bold && f.getSize >= 18.66)

  /** Every shown, enabled text below its minimum (4.5:1, 3:1 when large) over
    * the worst colour it can be drawn on: what it says, where, its ratio.
    */
  def lowContrast(root: Parent): Vector[String] =
    texts(root).flatMap { t =>
      t.getFill match
        case fg: Color =>
          pixels(t, fg, root.getScene) match
            case Left(why) => Vector(s"unresolved: '${t.getText.take(40)}' at ${path(t)}: $why")
            case Right(ps) =>
              val (r, (glyph, beside)) = ps.map(p => (ratio(p._1, p._2), p)).minBy(_._1)
              val min = if large(t) then Wcag.LargeTextMinimum else Wcag.TextMinimum
              Option
                .when(r < min - 1e-9)(
                  f"${r}%.2f < $min: '${t.getText.take(40)}' at ${path(t)} ($glyph on $beside)"
                )
                .toVector
        case other => Vector(s"unresolved: '${t.getText.take(40)}' at ${path(t)}: fill $other")
    }.distinct

  // --- focus ------------------------------------------------------------------

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
  def unmarked(root: Parent): Vector[String] =
    focusable(root).flatMap { n =>
      val (before, after) = (as(n, false), as(n, true))
      // Back to the state JavaFX holds.
      n.pseudoClassStateChanged(Focused, n.isFocused)
      ancestors(n).foreach(a => a.pseudoClassStateChanged(FocusWithin, a.isFocusWithin))
      Option.when(before == after)(
        s"${n.getAccessibleRole}: ${Option(n.getAccessibleText).getOrElse("").take(60)} at ${path(n)}"
      )
    }
