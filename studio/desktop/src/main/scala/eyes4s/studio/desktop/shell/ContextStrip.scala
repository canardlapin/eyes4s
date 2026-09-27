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
import eyes4s.studio.app.text.{MessageId, Messages}
import eyes4s.studio.app.vm.{
  ActionVM,
  ConfirmationVM,
  ContextStripVM,
  CrumbVM,
  DraftBannerVM,
  DraftChipVM,
  FreshnessTone,
  FreshnessVM,
  NoticeVM
}
import javafx.css.PseudoClass
import javafx.scene.control.{Button, Label}
import javafx.scene.layout.{HBox, Region}

/** The context strip (ticket S1.6), 32 px: back and forward (⌘[ ⌘]), the
  * trail as live crumbs (the current one in ink 600), the freshness badge
  * and, beside it, the newer run's chip or the dashed draft chip that opens
  * Analysis. It renders a [[ContextStripVM]]; every crumb dispatches its
  * intent, which may cross perspectives.
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

  // The strip's first four children are fixed: back, forward, the trail and
  // the spacer. Each part is rebuilt only when its view-model changes, so a
  // focused crumb keeps its focus while a running job's chip ticks.
  private val trail = HBox()
  trail.getStyleClass.add("trail")

  private type Chips = (FreshnessVM, Option[FreshnessVM], Vector[String], Option[DraftChipVM])

  private var back: Option[Button]                   = None
  private var forward: Option[Button]                = None
  private var shownNav: Option[(ActionVM, ActionVM)] = None
  private var shownTrail: Option[Vector[CrumbVM]]    = None
  private var shownChips: Option[Chips]              = None

  /** The icon button for `action`, reused while its words stay the same. */
  private def nav(icon: Icon, action: ActionVM, current: Option[Button]): Button =
    val b = current
      .filter(_.getAccessibleText == action.label)
      .getOrElse(Fx.iconButton(icon, action, dispatch))
    b.setDisable(!action.enabled)
    b.setOnAction(_ => dispatch(action.intent))
    b

  def render(vm: ContextStripVM): Unit =
    if !shownNav.contains((vm.back, vm.forward)) || back.isEmpty then
      shownNav = Some((vm.back, vm.forward))
      back = Some(nav(Icon.Back, vm.back, back))
      forward = Some(nav(Icon.Forward, vm.forward, forward))
    if !shownTrail.contains(vm.trail) then
      shownTrail = Some(vm.trail)
      val crumbs = vm.trail.zipWithIndex.flatMap { (c, i) =>
        val b = Button(c.label)
        b.setMnemonicParsing(false) // "ret_07" is a name, not a mnemonic
        b.getStyleClass.add("crumb")
        b.setAccessibleText(c.accessible)
        b.pseudoClassStateChanged(Fx.Current, c.current)
        b.setOnAction(_ => dispatch(c.intent))
        (if i > 0 then Vector(Fx.label("›", "sep")) else Vector.empty) :+ b
      }
      trail.getChildren.setAll(crumbs*): Unit
    val fixed = Vector(back, forward).flatten ++ Vector(trail)
    val kids  = node.getChildren
    if kids.size < 4 || (0 until 3).exists(i => !(kids.get(i) eq fixed(i))) then
      kids.setAll((fixed :+ Fx.spacer())*)
      shownChips = None
    val chips = (vm.freshness, vm.newer, vm.notes, vm.draft)
    if !shownChips.contains(chips) then
      shownChips = Some(chips)
      val badges =
        Vector(ContextStrip.badge(vm.freshness)) ++ vm.newer.map(ContextStrip.badge) ++
          vm.notes.map(n => Fx.label(n, "context-note")) ++ vm.draft.map { d =>
            val b = Button(d.text)
            b.setMnemonicParsing(false)
            b.getStyleClass.addAll("chip", "draft-chip")
            b.setAccessibleText(d.accessible)
            b.pseudoClassStateChanged(ContextStrip.Blocked, d.blocked)
            b.setOnAction(_ => dispatch(d.intent))
            b
          }
      kids.remove(4, kids.size)
      kids.addAll(badges*): Unit

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
    case FreshnessTone.Showing     => "showing"

/** The draft banner (ticket S1.7), 30 px and dashed, only in Compare and
  * Figures: the semibold lead ("Showing run 7 (analysis rev 4).") and the
  * detail generated from the plan diff and job state, then its buttons
  * (Review in Analysis, Discard draft; Show run 8 while a run is in
  * progress). Hidden when the view-model has none.
  */
final class DraftBanner(dispatch: Intent => Unit):

  val node: HBox = HBox()
  node.getStyleClass.add("draft-banner")
  Fx.fixHeight(node, DraftBanner.HeightPx)

  private var shown: Option[DraftBannerVM] = None

  def render(vm: Option[DraftBannerVM]): Unit =
    node.setVisible(vm.isDefined)
    node.setManaged(vm.isDefined)
    // Rebuilt only when the words or buttons change, so a focused button
    // keeps its focus across unrelated updates.
    if shown != vm then
      shown = vm
      vm.foreach { b =>
        val texts = Vector(Fx.label(b.lead, "banner-lead")) ++
          Option.when(b.detail.nonEmpty)(Fx.label(b.detail, "banner-detail"))
        val actions = b.actions.map(a => Fx.button(a, dispatch, "bar-button", "banner-action"))
        node.getChildren.setAll((texts ++ (Fx.spacer() +: actions))*)
      }

object DraftBanner:
  val HeightPx: Double = 30

/** The model's notice (a refused command, a layout that could not be read),
  * with Dismiss. S1.9 may move it; the words are the view-model's.
  */
final class NoticeBar(dispatch: Intent => Unit, messages: Messages):

  val text: Label = Fx.label("", "notice-text")

  val node: HBox = HBox()
  node.getStyleClass.add("notice-bar")
  Fx.fixHeight(node, DraftBanner.HeightPx)
  node.setVisible(false)
  node.setManaged(false)

  def render(vm: Option[NoticeVM]): Unit =
    node.setVisible(vm.isDefined)
    node.setManaged(vm.isDefined)
    vm.foreach { n =>
      text.setText(n.text)
      val dismiss = ActionVM(messages(MessageId.Dismiss), true, n.dismiss)
      node.getChildren.setAll(
        text,
        Fx.spacer(),
        Fx.button(dismiss, dispatch, "bar-button")
      ): Unit
    }

/** Discard draft's confirmation (S1.7): the question, then Discard draft
  * and Keep draft. Shown only while the model has a pending confirmation.
  */
final class ConfirmBar(dispatch: Intent => Unit):

  val text: Label = Fx.label("", "confirm-text")

  val node: HBox = HBox()
  node.getStyleClass.add("confirm-bar")
  Fx.fixHeight(node, DraftBanner.HeightPx)
  node.setVisible(false)
  node.setManaged(false)

  private var shown: Option[ConfirmationVM] = None

  def render(vm: Option[ConfirmationVM]): Unit =
    node.setVisible(vm.isDefined)
    node.setManaged(vm.isDefined)
    // Rebuilt only when it changes, so a focused button keeps its focus.
    if shown != vm then
      shown = vm
      vm.foreach { c =>
        text.setText(c.text)
        node.getChildren.setAll(
          text,
          Fx.spacer(),
          Fx.button(c.cancel, dispatch, "bar-button"),
          Fx.button(c.confirm, dispatch, "bar-button", "confirm-action")
        ): Unit
      }
