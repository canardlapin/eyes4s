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

import eyes4s.studio.app.plot.{NoteBlock, NoteLine, NoteRole, NoteTone, ParticipantNotes}
import eyes4s.studio.app.text.{ParticipantText, ParticipantTextId}
import eyes4s.studio.app.tokens.{FontFace, Theme, ThemedToken}
import eyes4s.studio.core.selection.StudioRef
import intaglio.{ExtentExpr, Grob, Length, LengthExpr, Point, Size}
import intaglio.interaction.NamedPicking
import munit.FunSuite

/** The participant plot's annotation column (review ticket 26, slice a):
  * blocks stacked top down by their anchor's D, each joined to its anchor's
  * mark by a leader that stops at the frame's edge, lines in fixed slots,
  * the stated width and elision rule, and no annotations leaving the plot
  * exactly as it was.
  */
class AnnotationsSuite extends FunSuite:
  import Annotations.{Block, Label, Line, Role, Tone}
  import ParticipantSamples.*

  private val PointsPerPixel = 72.0 / 96.0

  private def right[E, A](either: Either[E, A]): A =
    either.fold(e => fail(s"unexpected Left: $e"), identity)

  private def cellRef(p: String, g: String): StudioRef =
    board.cells
      .find(c => c.participant == p && c.group.label == g)
      .getOrElse(fail(s"$p $g"))
      .ref

  private def built(annotations: Annotations): BuiltPlot =
    right(ParticipantPlot(columns, annotations = annotations).build(source(board), Theme.Light))

  private def grobs(plot: BuiltPlot): Vector[Grob] =
    def all(g: Grob): Vector[Grob] = g +: g.children.flatMap(all)
    plot.plot.scene.grobs.flatMap(all)

  private def texts(plot: BuiltPlot): Vector[Grob.Text] =
    grobs(plot).collect { case t: Grob.Text => t }

  private val edge = right(LengthExpr.npc(1.0))

  // The leaders: one segment each, ending at the frame's edge, from a mark.
  private def leaders(plot: BuiltPlot): Vector[Grob.Segments] =
    val left = right(LengthExpr.npc(0.0))
    grobs(plot).collect {
      case s: Grob.Segments
          if s.name.isEmpty && s.segments.size == 1 &&
            s.segments.head._2.x == edge && s.segments.head._1.x != left =>
        s
    }

  // The middle of slot `k`, down from the frame's top.
  private def slotY(k: Int): LengthExpr =
    edge - right(ExtentExpr.points((k + 0.5) * Annotations.SlotPx * PointsPerPixel))

  private val lineX: LengthExpr =
    edge + right(ExtentExpr.points(Annotations.GapPx * PointsPerPixel))

  // The board's two notes: P17 (selected) and P05 (failed queries).
  private val p17 = right(
    Block.of(
      cellRef("P17", "Forgotten"),
      Vector(
        Line(Label(ParticipantTextId.Title, Vector.empty), Role.Heading),
        Line(Label(ParticipantTextId.Participants, "24")),
        Line(Label(ParticipantTextId.Queries, "17"))
      )
    )
  )
  private val p05 = right(
    Block.of(
      cellRef("P05", "Forgotten"),
      Vector(
        Line(Label(ParticipantTextId.Queries, "3"), Role.Heading, Some(Tone.Failure)),
        Line(Label(ParticipantTextId.OneQuery), tone = Some(Tone.Primary)),
        Line(Label(ParticipantTextId.NoValue), tone = Some(Tone.Failure))
      )
    )
  )

  test("no annotations draw nothing and leave the plot exactly as it was") {
    val plain = right(ParticipantPlot(columns).build(source(board), Theme.Light))
    val empty = built(Annotations.empty)
    assertEquals(empty.plot, plain.plot)
    assertEquals(empty.marks, plain.marks)
    assertEquals(leaders(empty), Vector.empty)
    // No group is added, not even an empty one: the background, the grid,
    // the marks, their labels and the title, as before the column existed.
    assertEquals(empty.plot.scene.grobs.size, 5)
    assert(grobs(empty).forall {
      case g: Grob.Group => g.children.nonEmpty
      case _             => true
    })
    assertEquals(
      empty.plot.panel.viewport.size,
      right(Size.npc(0.86, 0.74)),
      "the frame keeps its whole width"
    )
    // And annotations change nothing but the frame's width and the column.
    val annotated = built(right(Annotations.of(Vector(p17))))
    assertEquals(annotated.marks, plain.marks)
    assertEquals(annotated.encoding, plain.encoding)
    assertEquals(
      texts(annotated).map(_.label).diff(texts(plain).map(_.label)),
      p17.lines.map(_.label.text)
    )
  }

  test("the column is ColumnChars mono cells wide, after a gap, and the frame narrows by it") {
    // 28 cells of the 11 px mono face (0.6 em each) and an 8 px gap: the
    // board's 188 px column right of its frame holds its longest line.
    assertEquals(Annotations.CellPx, 0.6 * 11.0)
    assertEquals(Annotations.ColumnPx, 8.0 + 28 * 6.6)
    assert(Annotations.ColumnChars * Annotations.CellPx <= 188.0)
    assert("P05 · 17 of 20 contributing".length <= Annotations.ColumnChars)
    val plot  = built(right(Annotations.of(Vector(p17))))
    val width = right(
      ExtentExpr.fromExpr(
        right(LengthExpr.npc(0.86)) -
          LengthExpr(right(Length.points(Annotations.ColumnPx * PointsPerPixel)))
      )
    )
    assertEquals(
      plot.plot.panel.viewport.size,
      Size.fromExtents(width, right(ExtentExpr.npc(0.74)))
    )
    // Every line starts the gap right of the frame's edge, left-aligned.
    val written = texts(plot).filter(t => p17.lines.map(_.label.text).contains(t.label))
    assertEquals(written.size, 3)
    written.foreach(t =>
      assertEquals(t.at.x, lineX, t.label)
      assertEquals(t.anchor.horizontal, intaglio.HJust.Left, t.label)
    )
  }

  test("blocks stack top down by their anchor's D; leaders join each mark to its block") {
    // Given P05 first: it is drawn second, as its anchor (−0.23) is lower
    // than P17's (+0.32).
    val plot = built(right(Annotations.of(Vector(p05, p17))))
    val ls   = leaders(plot)
    assertEquals(ls.size, 2)
    // P17's three lines take slots 0–2; one empty slot; P05's take 4–6.
    val slots = Vector(p17 -> Vector(0, 1, 2), p05 -> Vector(4, 5, 6))
    assertEquals(Annotations.slots(Vector(p17, p05)), slots.map(_._2))
    slots.zip(ls).foreach { case ((block, ks), leader) =>
      val mark       = right(plot.markOf(block.anchor).toRight(block.anchor))
      val (from, to) = leader.segments.head
      val nudge      = right(Length.points(mark.nudgePx.dxPx * PointsPerPixel))
      assertEquals(
        from,
        Point(
          right(LengthExpr.native(mark.at.x)) + LengthExpr(nudge),
          right(LengthExpr.native(mark.at.y))
        )
      )
      assertEquals(to, Point(edge, slotY(ks.head)), "the leader meets the first line")
      block.lines.zip(ks).foreach { (line, k) =>
        val t = texts(plot).find(_.label == line.label.text).getOrElse(fail(line.label.text))
        assertEquals(t.at, Point(lineX, slotY(k)), line.label.text)
      }
    }
  }

  test("the column holds the frame's drawn height in whole slots, never a line below it") {
    // The Results board's 216 px frame: 15 lines.
    assertEquals(Annotations.capacity(216.0), 15)
    assertEquals(Annotations.capacity(Annotations.SlotPx), 1)
    assertEquals(Annotations.capacity(Annotations.SlotPx - 0.5), 0)
    Vector(0.0, -14.0, Double.NaN).foreach(h => assertEquals(Annotations.capacity(h), 0, h))
    // The plot's frame is FrameHeightNpc of its height, as it is drawn.
    assertEquals(
      built(Annotations.empty).plot.panel.viewport.size,
      right(Size.npc(0.86, ParticipantPlot.FrameHeightNpc))
    )
    assertEquals(ParticipantPlot.frameHeightPx(200.0), ParticipantPlot.FrameHeightNpc * 200.0)
    Vector(60.0, 120.0, 151.0, 300.0, 900.0).foreach { h =>
      val slots = ParticipantPlot.noteSlots(h)
      val frame = ParticipantPlot.frameHeightPx(h)
      // The last slot ends inside the frame, and one more would not.
      assert(slots * Annotations.SlotPx <= frame, s"$h px: $slots slots in $frame px")
      assert((slots + 1) * Annotations.SlotPx > frame, s"$h px: $slots slots in $frame px")
    }
  }

  test("a line's role sets its face and its tone its colour; a leader takes its heading's") {
    val plot                       = built(right(Annotations.of(Vector(p05, p17))))
    def colour(token: ThemedToken) = Some(IntaglioColours.themed(Theme.Light, token))
    def of(label: Label)           =
      texts(plot).find(_.label == label.text).getOrElse(fail(label.text))
    val heading = of(p17.lines(0).label)
    assertEquals(heading.gp.fontFamily, Some(FontFace.MonoMedium.javaFxFamily))
    assertEquals(heading.gp.fill, colour(ThemedToken.Ink))
    val fact = of(p17.lines(1).label)
    assertEquals(fact.gp.fontFamily, Some(FontFace.MonoRegular.javaFxFamily))
    assertEquals(fact.gp.fill, colour(ThemedToken.Ink2))
    assertEquals(of(p05.lines(0).label).gp.fill, colour(ThemedToken.Fail))
    assertEquals(of(p05.lines(1).label).gp.fill, colour(ThemedToken.Ink))
    assertEquals(of(p05.lines(2).label).gp.fill, colour(ThemedToken.Fail))
    assertEquals(
      leaders(plot).map(_.gp.stroke),
      Vector(colour(ThemedToken.Ink), colour(ThemedToken.Fail))
    )
  }

  test("a long line is cut to ColumnChars − 1 code points and an ellipsis; none wraps") {
    val exact = "x" * Annotations.ColumnChars
    assertEquals(Annotations.fit(exact), exact)
    assertEquals(Annotations.fit(exact + "y"), "x" * (Annotations.ColumnChars - 1) + "…")
    // Code points, not chars: a supplementary letter counts once.
    val wide = "𝒟" * Annotations.ColumnChars
    assertEquals(Annotations.fit(wide), wide)
    assertEquals(Annotations.fit(wide + "·"), "𝒟" * (Annotations.ColumnChars - 1) + "…")
    // In the plot: the caption's long argument is cut, on one line.
    val long  = "a reporting group with a long name"
    val label = Label(ParticipantTextId.Caption, long)
    val block = right(Block.of(cellRef("P01", "Remembered"), Vector(Line(label))))
    val drawn = texts(built(right(Annotations.of(Vector(block))))).map(_.label)
    val cut   = drawn.filter(_.startsWith("Participant D at"))
    assertEquals(cut, Vector(label.text.take(Annotations.ColumnChars - 1) + "…"))
    assert(!cut.head.contains('\n'))
    assertEquals(cut.head.codePointCount(0, cut.head.length), Annotations.ColumnChars)
  }

  test("marks are still picked where they are drawn in the narrowed frame") {
    val plot      = built(right(Annotations.of(Vector(p05, p17))))
    val surface   = right(PlotSurface(536, 272, 1.0))
    val transform = right(PlotTransform.resolve(plot.plot, surface))
    val picking   = right(NamedPicking.compile(plot.plot.scene, transform.renderContext))
    val targets   = right(PlotTargets.resolve(plot, transform, picking))
    Vector(p17.anchor, p05.anchor, cellRef("P10", "Remembered")).foreach { ref =>
      val at = right(targets.target(ref).toRight(ref)).anchor
      assertEquals(right(targets.pick(at, 0.0)).map(_.ref), Some(ref))
      // Every mark lies left of the column.
      assert(at.x < 536 - Annotations.ColumnPx, s"$ref at $at")
    }
  }

  test("a block needs lines and its own anchor; the plot refuses an anchor it does not draw") {
    val ref = cellRef("P01", "Remembered")
    assertEquals(Block.of(ref, Vector.empty), Left(AnnotationError.NoLines(ref)))
    val one = right(Block.of(ref, Vector(Line(Label(ParticipantTextId.NoValue)))))
    assertEquals(Annotations.of(Vector(one, p17, one)), Left(AnnotationError.SharedAnchor(ref)))
    assert(AnnotationError.SharedAnchor(ref).message.contains(ref.toString))
    // A grand mean is not a participant row, nor is a row of another source.
    val grandRef = board.groups.head.ref
    val foreign  = StudioRef.Participant("P99")
    Vector(grandRef, foreign).foreach { anchor =>
      val block   = right(Block.of(anchor, Vector(Line(Label(ParticipantTextId.NoValue)))))
      val refused =
        ParticipantPlot(columns, annotations = right(Annotations.of(Vector(block))))
          .build(source(board), Theme.Light)
      assertEquals(
        refused.map(_.plot),
        Left(PlotBuildError.Unanchored("participant-plot", anchor))
      )
    }
  }

  test("a missing mean's ring can be annotated: its leader starts on the band") {
    val means   = zeroAndMissing
    val missing = means.cells
      .find(c => c.participant == "P02" && c.group.label == "Remembered")
      .getOrElse(fail("P02"))
      .ref
    val block = right(
      Block.of(
        missing,
        Vector(Line(Label(ParticipantTextId.NoValue), tone = Some(Tone.Failure)))
      )
    )
    val plot = right(
      ParticipantPlot(columns, annotations = right(Annotations.of(Vector(block))))
        .build(source(means), Theme.Light)
    )
    val mark = right(plot.markOf(missing).toRight(missing))
    assertEquals(
      leaders(plot).map(_.segments.head._1.y),
      Vector(right(LengthExpr.native(mark.at.y)))
    )
    // The band's own label and the annotation's line.
    assertEquals(texts(plot).count(_.label == ParticipantText(ParticipantTextId.NoValue)), 2)
  }

  test("the app's notes draw as written and are spoken in the plot's accessible summary") {
    def note(
        id: ParticipantTextId,
        role: NoteRole,
        tone: NoteTone,
        spoken: String,
        args: String*
    ) =
      NoteLine(id, args.toVector, role, tone, spoken)
    val notes = ParticipantNotes(
      Vector(
        NoteBlock(
          "P17",
          cellRef("P17", "Forgotten"),
          None,
          Vector(
            note(
              ParticipantTextId.NoteSelected,
              NoteRole.Heading,
              NoteTone.Primary,
              "P17 · selected",
              "P17"
            ),
            note(
              ParticipantTextId.NoteGroupMean,
              NoteRole.Fact,
              NoteTone.Secondary,
              "Forgotten +0.32, n 2",
              "Forg",
              "+0.32",
              "2"
            )
          )
        ),
        NoteBlock(
          "P05",
          cellRef("P05", "Forgotten"),
          None,
          Vector(
            note(
              ParticipantTextId.NoteNoValue,
              NoteRole.Fact,
              NoteTone.Failure,
              "no value, not zero"
            )
          )
        )
      ),
      0
    )
    val annotations = right(Annotations.from(notes))
    assertEquals(
      annotations.blocks.map(b => b.lines.map(l => (l.label.text, l.role, l.drawnTone))),
      Vector(
        Vector(
          ("P17 · selected", Role.Heading, Tone.Primary),
          ("Forg +0.32 (n 2)", Role.Fact, Tone.Secondary)
        ),
        Vector(("no value, not zero", Role.Fact, Tone.Failure))
      )
    )
    val plot = built(annotations)
    assert(texts(plot).exists(_.label == "Forg +0.32 (n 2)"))
    val plain = right(ParticipantPlot(columns).build(source(board), Theme.Light))
    // The summary as before, then the blocks in the order given, as spoken.
    assertEquals(
      plot.description,
      plain.description.replace(
        ParticipantText(ParticipantTextId.Summary, source(board).caption, "2"),
        ParticipantText(ParticipantTextId.Summary, source(board).caption, "2") +
          ". Annotations: P17 · selected, Forgotten +0.32, n 2; no value, not zero"
      )
    )
    assert(plot.description != plain.description)
    assert(!plain.description.contains("Annotations"), plain.description)
  }
