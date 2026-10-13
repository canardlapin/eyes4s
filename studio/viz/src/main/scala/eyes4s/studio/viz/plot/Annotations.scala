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

package eyes4s.studio.viz.plot

import eyes4s.studio.app.text.{ParticipantText, ParticipantTextId}
import eyes4s.studio.app.tokens.{FontFace, ThemedToken, TypeSize}
import eyes4s.studio.core.selection.StudioRef

/** The participant plot's annotation column (review ticket 26, slice a; the
  * Results board's notes beside its participant D plot): per annotated
  * participant, a block of short lines, one fact per line, such as "P17 ·
  * selected", "19 of 20 contributing", "Rem +0.38 (n 17)" or "3 failed
  * queries:" and "no value, not zero".
  *
  * Each block is anchored to one of the plot's participant rows, its
  * `anchor`: a participant's mean in one group. [[ParticipantPlot]] lays the
  * blocks out in a column right of its frame and joins each block to its
  * anchor's mark by a straight leader that stops at the frame's edge.
  *
  * The column's rule, stated once here and nowhere else:
  *
  *   - Width: [[ColumnChars]] monospace cells of the 11 px mono face
  *     ([[CellPx]] each), after a [[GapPx]] gap from the frame's right edge;
  *     the frame narrows by [[ColumnPx]] to make room. With no blocks the
  *     column takes no room and the plot is drawn exactly as without it.
  *   - Lines: one per fact, never wrapped. A line longer than
  *     [[ColumnChars]] code points is cut to one fewer and ends in
  *     [[Elision]] ([[Annotations.fit]]).
  *   - Place: every line takes one [[SlotPx]] slot. Blocks are stacked from
  *     the frame's top, ordered by their anchor's D from highest to lowest
  *     (so leaders do not cross), with [[BlockGapSlots]] empty slot between
  *     two blocks. A block's leader meets the frame's edge at the middle of
  *     its first line.
  *
  * A block's anchor must be a participant row the plot draws; the plot
  * refuses one that is not ([[PlotBuildError.Unanchored]]).
  */
final case class Annotations private (blocks: Vector[Annotations.Block]):

  /** True when there is nothing to write: the plot draws no column. */
  def isEmpty: Boolean = blocks.isEmpty

object Annotations:

  /** No annotations. */
  val empty: Annotations = Annotations(Vector.empty)

  /** `blocks`, each anchored to its own row. */
  def of(blocks: Vector[Block]): Either[AnnotationError, Annotations] =
    val anchors = blocks.map(_.anchor)
    anchors
      .diff(anchors.distinct)
      .headOption
      .fold(Right(Annotations(blocks)))(ref => Left(AnnotationError.SharedAnchor(ref)))

  /** A line's text, by its id in the participant plot's catalogue. */
  final case class Label(id: ParticipantTextId, args: Vector[String]):

    /** The line as the catalogue writes it, before it is fitted. */
    def text: String = ParticipantText(id, args*)

  object Label:
    def apply(id: ParticipantTextId, args: String*): Label = Label(id, args.toVector)

  /** What a line is to its block: the heading names the participant, a fact
    * follows it. A heading is set in the medium mono face, a fact in the
    * regular one.
    */
  enum Role derives CanEqual:
    case Heading, Fact

  /** A line's colour, by what it says rather than by a colour: `Primary`
    * (ink) for what the block is about, `Secondary` (ink-2) for the numbers
    * that back it, `Failure` (the fail colour) for a participant or a value
    * that failed.
    */
  enum Tone derives CanEqual:
    case Primary, Secondary, Failure

  /** One line of a block. Without a `tone`, a heading is `Primary` and a
    * fact `Secondary`.
    */
  final case class Line(label: Label, role: Role = Role.Fact, tone: Option[Tone] = None):

    /** The tone the line is drawn in. */
    def drawnTone: Tone = tone.getOrElse(role match
      case Role.Heading => Tone.Primary
      case Role.Fact    => Tone.Secondary)

  /** One participant's lines, top to bottom, and the row they annotate. */
  final case class Block private (anchor: StudioRef, lines: Vector[Line])

  object Block:
    def of(anchor: StudioRef, lines: Vector[Line]): Either[AnnotationError, Block] =
      Either.cond(lines.nonEmpty, Block(anchor, lines), AnnotationError.NoLines(anchor))

  /** The column's width, in monospace cells. */
  val ColumnChars: Int = 28

  /** The advance of every glyph of the mono face, as a fraction of its size
    * (IBM Plex Mono: 600 units to the 1000-unit em).
    */
  val MonoAdvanceEm: Double = 0.6

  /** The column's type size, in logical pixels (the 11 of the type scale). */
  val TextPx: Double = TypeSize.T11.px.toDouble

  /** One monospace cell's width, in logical pixels. */
  val CellPx: Double = MonoAdvanceEm * TextPx

  /** The gap between the frame's right edge and the column. */
  val GapPx: Double = 8.0

  /** The room the column takes right of the frame: the gap and the cells. */
  val ColumnPx: Double = GapPx + ColumnChars * CellPx

  /** The height of one line's slot. */
  val SlotPx: Double = 14.0

  /** The empty slots between two blocks. */
  val BlockGapSlots: Int = 1

  /** What ends a line that was cut to fit. */
  val Elision: String = "…"

  /** `text` as the column writes it: unchanged when it has at most
    * [[ColumnChars]] code points, otherwise its first `ColumnChars − 1`
    * code points and [[Elision]]. Code points, so a "·", "−" or "Δ" counts
    * one cell, as it takes one in the mono face.
    */
  def fit(text: String): String =
    if text.codePointCount(0, text.length) <= ColumnChars then text
    else text.substring(0, text.offsetByCodePoints(0, ColumnChars - 1)) + Elision

  /** The colour token of `tone`. */
  def token(tone: Tone): ThemedToken = tone match
    case Tone.Primary   => ThemedToken.Ink
    case Tone.Secondary => ThemedToken.Ink2
    case Tone.Failure   => ThemedToken.Fail

  /** The face of a line of `role`. */
  def face(role: Role): FontFace = role match
    case Role.Heading => FontFace.MonoMedium
    case Role.Fact    => FontFace.MonoRegular

  /** The slot of each line of `blocks`, counted from the frame's top, block
    * by block in the order given: a block's lines take consecutive slots and
    * [[BlockGapSlots]] are left empty before the next block.
    */
  def slots(blocks: Vector[Block]): Vector[Vector[Int]] =
    blocks
      .foldLeft((0, Vector.empty[Vector[Int]])) { case ((next, acc), b) =>
        (next + b.lines.size + BlockGapSlots, acc :+ b.lines.indices.toVector.map(next + _))
      }
      ._2

/** Why annotations were refused. */
enum AnnotationError derives CanEqual:

  /** The block of row `anchor` has no lines. */
  case NoLines(anchor: StudioRef)

  /** Two blocks annotate row `anchor`. */
  case SharedAnchor(anchor: StudioRef)

  def message: String = this match
    case NoLines(ref)      => s"the annotation of row $ref has no lines"
    case SharedAnchor(ref) => s"row $ref is annotated by more than one block"
