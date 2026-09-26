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

import eyes4s.studio.app.Intent
import eyes4s.studio.app.icons.Icon
import eyes4s.studio.app.vm.{ContextStripVM, DraftBannerVM, FreshnessTone, FreshnessVM}
import javafx.css.PseudoClass
import javafx.scene.control.{Button, Label}
import javafx.scene.layout.{HBox, Region}

/** The context strip (32 px): back and forward, the trail, and freshness.
  * S1.6 refines it; this renders the S1.0 [[ContextStripVM]] as it is.
  */
final class ContextStrip(dispatch: Intent => Unit):

  val node: HBox = HBox()
  node.getStyleClass.add("context-strip")
  Fx.fixHeight(node, ContextStrip.HeightPx)

  /** The crumbs as drawn, in order. */
  def crumbs: Vector[Button] =
    import scala.jdk.CollectionConverters.*
    node.getChildren.asScala.toVector.flatMap {
      case h: HBox if h.getStyleClass.contains("trail") =>
        h.getChildren.asScala.toVector.collect { case b: Button => b }
      case _ => Vector.empty
    }

  def render(vm: ContextStripVM): Unit =
    val back    = Fx.iconButton(Icon.Back, vm.back, dispatch)
    val forward = Fx.iconButton(Icon.Forward, vm.forward, dispatch)
    val trail   = HBox()
    trail.getStyleClass.add("trail")
    vm.trail.zipWithIndex.foreach { (c, i) =>
      if i > 0 then trail.getChildren.add(Fx.label("›", "sep")): Unit
      val b = Button(c.label)
      b.setMnemonicParsing(false) // "ret_07" is a name, not a mnemonic
      b.getStyleClass.add("crumb")
      b.pseudoClassStateChanged(Fx.Current, c.current)
      b.setOnAction(_ => dispatch(c.intent))
      trail.getChildren.add(b)
    }
    val badges = Vector(ContextStrip.badge(vm.freshness)) ++ vm.newer.map(ContextStrip.badge) ++
      vm.notes.map(n => Fx.label(n, "context-note")) ++ vm.draft.map { d =>
        val b = Button(d.text)
        b.setMnemonicParsing(false)
        b.getStyleClass.addAll("chip", "draft-chip")
        b.pseudoClassStateChanged(ContextStrip.Blocked, d.blocked)
        b.setOnAction(_ => dispatch(d.intent))
        b
      }
    node.getChildren.setAll((Vector(back, forward, trail, Fx.spacer()) ++ badges)*): Unit

object ContextStrip:
  val HeightPx: Double = 32

  private val Blocked = PseudoClass.getPseudoClass("blocked")

  /** A freshness chip: a dot in the tone's colour, then the words. */
  def badge(vm: FreshnessVM): Label =
    val dot = Region()
    dot.getStyleClass.addAll("dot", s"tone-${tone(vm.tone)}")
    val l = Fx.label(vm.text, "chip", "freshness")
    l.setGraphic(dot)
    l

  private def tone(t: FreshnessTone): String = t match
    case FreshnessTone.Current     => "current"
    case FreshnessTone.Stale       => "stale"
    case FreshnessTone.Running     => "running"
    case FreshnessTone.Failed      => "failed"
    case FreshnessTone.Cancelled   => "cancelled"
    case FreshnessTone.NoRun       => "no-run"
    case FreshnessTone.PendingData => "pending-data"

/** The draft banner (30 px, Compare and Figures only). S1.7 refines it. */
final class DraftBanner(dispatch: Intent => Unit):

  val node: HBox = HBox()
  node.getStyleClass.add("draft-banner")
  Fx.fixHeight(node, DraftBanner.HeightPx)

  def render(vm: Option[DraftBannerVM]): Unit =
    node.setVisible(vm.isDefined)
    node.setManaged(vm.isDefined)
    vm.foreach { b =>
      val texts = Vector(Fx.label(b.lead, "banner-lead")) ++
        Option.when(b.detail.nonEmpty)(Fx.label(b.detail, "banner-detail"))
      val actions = b.actions.map(a => Fx.button(a, dispatch, "bar-button", "banner-action"))
      node.getChildren.setAll((texts ++ (Fx.spacer() +: actions))*)
    }

object DraftBanner:
  val HeightPx: Double = 30
