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

package eyes4s.studio.app.layout

import cats.data.NonEmptyVector
import eyes4s.studio.app.text.MessageId
import eyes4s.studio.core.document.Perspective

/** Why a layout declaration was refused; each case names its operands. */
enum LayoutError derives CanEqual:
  case BadId(value: String)
  case BadWeight(layout: String, weight: Double)
  case SelectedOutOfRange(pane: String, selected: Int, size: Int)
  case DuplicatePane(layout: String, pane: String)

  /** One pane id declared with different titles or kinds in different
    * layouts: a retained pane is one view, so it has one declaration.
    */
  case ConflictingPane(pane: String, layouts: Vector[String])
  case NoLayouts(perspective: Perspective)

  def message: String = this match
    case BadId(v) =>
      s"'$v' is not a pane or layout id (lower-case words joined by '.' or '-')."
    case BadWeight(l, w)             => s"Layout $l has a split weight $w that is not positive."
    case SelectedOutOfRange(p, s, n) => s"Group of $p selects tab $s of $n."
    case DuplicatePane(l, p)         => s"Layout $l declares pane $p more than once."
    case ConflictingPane(p, ls)      =>
      s"Pane $p is declared differently in layouts ${ls.mkString(", ")}."
    case NoLayouts(p) => s"Perspective ${p.label} declares no layout."

private[layout] object Ids:
  def valid(value: String): Boolean =
    value.nonEmpty && value
      .split("[.-]", -1)
      .forall(w =>
        w.nonEmpty && w.forall(c => (c >= 'a' && c <= 'z') || (c >= '0' && c <= '9'))
      )

/** A pane's identity, stable across layouts: a pane that appears in two
  * layouts of one perspective (Compare's scale profile) is one live view.
  */
final case class PaneId private[layout] (value: String) derives CanEqual

object PaneId:
  def of(value: String): Either[LayoutError, PaneId] =
    Either.cond(Ids.valid(value), new PaneId(value), LayoutError.BadId(value))

final case class LayoutId private[layout] (value: String) derives CanEqual

object LayoutId:
  def of(value: String): Either[LayoutError, LayoutId] =
    Either.cond(Ids.valid(value), new LayoutId(value), LayoutError.BadId(value))

/** What a pane is, which fixes its keyboard contract (DESIGN_SPEC section
  * 10): a plot is one focus stop with a roving cursor and always has a
  * sibling [[PaneKind.Table]].
  */
enum PaneKind derives CanEqual:
  case Navigator, Plot, Table, Form, Inspector, Text, Start

/** A pane's tab title. `Fixed` titles are the boards' words; a `Dynamic`
  * title is computed by the pane's view-model ("Query P17 · ret_07 ·
  * beach-042") and `generic` names it until then.
  */
enum PaneTitle derives CanEqual:
  case Fixed(words: String)
  case Dynamic(generic: String)

  def text: String = this match
    case Fixed(t)   => t
    case Dynamic(g) => g

final case class PaneDecl(id: PaneId, title: PaneTitle, kind: PaneKind) derives CanEqual

enum Axis derives CanEqual:
  /** Children side by side. */
  case Horizontal

  /** Children stacked. */
  case Vertical

/** A dock layout: splits of tab groups. */
enum LayoutNode derives CanEqual:
  case Split(axis: Axis, children: NonEmptyVector[(LayoutNode, Double)])

  /** A tab group; `selected` indexes `panes`. `navigator` groups may be
    * minimized to a strip (DESIGN_SPEC section 3).
    */
  case Group(panes: NonEmptyVector[PaneDecl], selected: Int, navigator: Boolean)

  /** Every group, in reading order (the F6 order). */
  def groups: Vector[Group] = this match
    case Split(_, children) => children.toVector.flatMap(_._1.groups)
    case g: Group           => Vector(g)

  def allPanes: Vector[PaneDecl] = groups.flatMap(_.panes.toVector)

/** One perspective layout, with the status-bar hint it shows. */
final case class PerspectiveLayout(
    id: LayoutId,
    perspective: Perspective,
    root: LayoutNode,
    hint: MessageId
) derives CanEqual:
  def groups: Vector[LayoutNode.Group]   = root.groups
  def panes: Vector[PaneDecl]            = root.allPanes
  def pane(id: PaneId): Option[PaneDecl] = panes.find(_.id == id)

  /** The group holding `pane`. */
  def groupOf(pane: PaneId): Option[LayoutNode.Group] =
    groups.find(_.panes.exists(_.id == pane))

  /** The pane a group shows first. */
  def selectedPane(group: LayoutNode.Group): PaneDecl =
    group.panes.toVector.lift(group.selected).getOrElse(group.panes.head)

/** Which Compare layout the trail asks for (DESIGN_SPEC section 2). */
enum CompareLayout derives CanEqual:
  case Summary, Query

/** The perspectives and their default layouts, declared UI-neutrally
  * (DESIGN_SPEC section 13). A shell maps them to its docking library.
  * studio-desktop (S1.5a) maps them onto scaladock 628c46f `Perspectives(dock)`:
  * one Dock with retained panes, and one named perspective per
  * [[PerspectiveLayout]], named by its [[LayoutId]]. A [[PaneId]] that
  * appears in several layouts is one retained, live view.
  */
final case class LayoutSpec(
    perspectives: Vector[(Perspective, NonEmptyVector[PerspectiveLayout])]
) derives CanEqual:

  def layouts(perspective: Perspective): Vector[PerspectiveLayout] =
    perspectives.find(_._1 == perspective).toVector.flatMap(_._2.toVector)

  def all: Vector[PerspectiveLayout] = perspectives.flatMap(_._2.toVector)

  def layout(id: LayoutId): Option[PerspectiveLayout] = all.find(_.id == id)
