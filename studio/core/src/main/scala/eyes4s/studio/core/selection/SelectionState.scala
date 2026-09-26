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

package eyes4s.studio.core.selection

/** How an input combines with the selection: Intaglio's
  * `SelectionOperation`, case for case, so S4 maps one onto the other.
  */
enum SelectionMode derives CanEqual:
  case Replace, Add, Subtract, Toggle, Clear

/** Intaglio's `InputCause`. */
enum InputCause derives CanEqual:
  case Pointer, Keyboard, Programmatic, Projected

/** The view an input came from, such as "compare.scale-ladder". */
final case class ViewId private (value: String) derives CanEqual

object ViewId:
  def of(value: String): Either[SelectionError, ViewId] =
    Either.cond(value.trim.nonEmpty, new ViewId(value), SelectionError.BlankView)

/** The selection context's revision. It moves when what the refs can mean
  * changes (another document, another shown run); an input produced against
  * an earlier context is stale.
  */
final case class ContextRevision(value: Long) derives CanEqual:
  def next: ContextRevision = ContextRevision(value + 1)

/** Intaglio's `InputStamp`: the context an input was produced against, its
  * origin view, and a sequence number that strictly increases per view.
  */
final case class InputStamp(
    context: ContextRevision,
    origin: ViewId,
    sequence: Long,
    cause: InputCause
) derives CanEqual

/** One selection input. `refs` is ignored by Clear. */
final case class SelectionInput(stamp: InputStamp, mode: SelectionMode, refs: Vector[StudioRef])
    derives CanEqual

/** Why an input was refused; every case names its operands. */
enum SelectionError derives CanEqual:
  case BlankView
  case StaleContext(origin: ViewId, expected: ContextRevision, received: ContextRevision)
  case Duplicate(origin: ViewId, sequence: Long)
  case OutOfOrder(origin: ViewId, sequence: Long, last: Long)
  case NegativeSequence(origin: ViewId, sequence: Long)

  def message: String = this match
    case BlankView                           => "A view id is blank."
    case StaleContext(origin, expected, got) =>
      s"Input from ${origin.value} was made against context ${got.value}; the selection is " +
        s"at context ${expected.value}."
    case Duplicate(origin, sequence) =>
      s"Input $sequence from ${origin.value} was already applied."
    case OutOfOrder(origin, sequence, last) =>
      s"Input $sequence from ${origin.value} is older than its last applied input $last."
    case NegativeSequence(origin, sequence) =>
      s"Input sequence $sequence from ${origin.value} is negative."

/** How one ref stands relative to the selection. */
enum SelectionRelation derives CanEqual:
  /** The ref itself is selected. */
  case Selected

  /** The ref contains a selected ref (a trial whose fixation is selected). */
  case ContainsSelection

  /** The ref lies inside a selected ref. It is context, not selection:
    * selecting an aggregate never selects its observations.
    */
  case WithinSelection
  case Unrelated

/** The global selection (ticket S3.3): an ordered set of refs, the context
  * revision and the last applied sequence per view. Hover is not here: it is
  * local to a view ([[Hover]]).
  */
final case class SelectionState private (
    context: ContextRevision,
    selected: Vector[StudioRef],
    delivered: Map[ViewId, Long],
    revision: Long
) derives CanEqual:

  def isSelected(ref: StudioRef): Boolean = selected.contains(ref)

  /** Apply an input, or refuse it without changing anything. An accepted
    * input that leaves the refs unchanged still records its sequence.
    */
  def submit(input: SelectionInput): Either[SelectionError, SelectionState] =
    val stamp = input.stamp
    for
      _ <- Either.cond(
        stamp.context == context,
        (),
        SelectionError.StaleContext(stamp.origin, context, stamp.context)
      )
      _ <- Either.cond(
        stamp.sequence >= 0,
        (),
        SelectionError.NegativeSequence(stamp.origin, stamp.sequence)
      )
      _ <- delivered.get(stamp.origin) match
        case Some(last) if stamp.sequence == last =>
          Left(SelectionError.Duplicate(stamp.origin, stamp.sequence))
        case Some(last) if stamp.sequence < last =>
          Left(SelectionError.OutOfOrder(stamp.origin, stamp.sequence, last))
        case _ => Right(())
    yield
      val refs = input.refs.distinct
      val next = input.mode match
        case SelectionMode.Replace  => refs
        case SelectionMode.Add      => selected ++ refs.filterNot(selected.contains)
        case SelectionMode.Subtract => selected.filterNot(refs.contains)
        case SelectionMode.Toggle   =>
          selected.filterNot(refs.contains) ++ refs.filterNot(selected.contains)
        case SelectionMode.Clear => Vector.empty
      copy(
        selected = next,
        delivered = delivered.updated(stamp.origin, stamp.sequence),
        revision = if next == selected then revision else revision + 1
      )

  /** A new context: sequences restart, and only the refs `keep` accepts
    * survive (for instance, refs of the run still shown).
    */
  def rebase(keep: StudioRef => Boolean): SelectionState =
    val next = selected.filter(keep)
    SelectionState(
      context.next,
      next,
      Map.empty,
      if next == selected then revision else revision + 1
    )

  /** How `ref` stands relative to the selection under `lineage`. */
  def relation(ref: StudioRef, lineage: Lineage = Lineage.structural): SelectionRelation =
    if isSelected(ref) then SelectionRelation.Selected
    else
      val ancestors = lineage.ancestors(ref)
      if ancestors.exists(isSelected) then SelectionRelation.WithinSelection
      else if selected.exists(s => lineage.ancestors(s).contains(ref)) then
        SelectionRelation.ContainsSelection
      else SelectionRelation.Unrelated

object SelectionState:
  val empty: SelectionState = SelectionState(ContextRevision(0), Vector.empty, Map.empty, 0)

  /** An empty selection in the given context. */
  def at(context: ContextRevision): SelectionState =
    SelectionState(context, Vector.empty, Map.empty, 0)

/** A view's own hover: never shared through the bus. */
final case class Hover(target: Option[StudioRef]) derives CanEqual:
  /** The next hover, and whether it changed. */
  def set(next: Option[StudioRef]): (Hover, Boolean) = (Hover(next), next != target)

object Hover:
  val none: Hover = Hover(None)
