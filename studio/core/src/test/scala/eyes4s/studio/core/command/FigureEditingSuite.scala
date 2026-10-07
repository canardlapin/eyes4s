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

package eyes4s.studio.core.command

import eyes4s.studio.core.document.*
import eyes4s.studio.core.backend.RunId
import eyes4s.codec.SchemaLadder
import io.circe.{Encoder, Json}
import io.circe.syntax.*

/** Stored author drafts and stable panel moves share the ordinary science history. */
class FigureEditingSuite extends munit.FunSuite:
  import Command.*

  private def ok[E, A](value: Either[E, A]): A =
    value.fold(e => fail(e.toString), identity)

  private val base   = DocumentSamples.t2
  private val figure = base.figures.head
  private val id     = figure.id
  private val first  = figure.panels.head.letter
  private val draft  = ok(
    FigureMethodsDraft.of("  Generated β\n\n", "\tAuthored 🦆\r\n", Some(" Pending δ\n"))
  )

  private def changed(document: StudioDocument, command: Command): StudioDocument =
    ok(Reducer.run(document, command)).document
  private def withDraft = changed(base, SetFigureMethods(id, Some(draft)))
  private def stored(document: StudioDocument, figureId: FigureId = id) =
    document.figures.find(_.id == figureId).getOrElse(fail("missing figure"))

  private def relabel(envelope: Json, version: Int): Json =
    envelope.hcursor
      .downField("schema")
      .downField("version")
      .withFocus(_ => Json.fromInt(version))
      .top
      .getOrElse(fail("missing envelope"))

  private def refuseEarlier[A: Encoder](
      ladder: SchemaLadder[A],
      value: A
  ): Unit =
    val envelope = ok(ladder.codec.encode(value))
    ladder.versions.dropRight(1).foreach { version =>
      assert(ladder.writeAt(version, value).isLeft, s"write $version")
      assert(ladder.readAt(version, value.asJson).isLeft, s"read $version")
      assert(
        ladder.codec.decode(relabel(envelope, version.version)).isLeft,
        s"relabel $version"
      )
    }

  test(
    "draft codecs preserve exact unicode, whitespace, empty edits and optional pending text"
  ) {
    Vector(
      draft,
      ok(FigureMethodsDraft.of("", "", None)),
      ok(FigureMethodsDraft.of("base\n", "", Some("")))
    ).foreach { d =>
      assertEquals(io.circe.parser.decode[FigureMethodsDraft](d.asJson.noSpaces), Right(d))
      val document = changed(base, SetFigureMethods(id, Some(d)))
      assertEquals(StudioDocument.decode(ok(StudioDocument.encode(document))), Right(document))
      val reopened = ok(StudioDocument.decode(ok(StudioDocument.encode(document))))
      assertEquals(stored(reopened).methods, Some(d))
    }
  }

  test(
    "legacy figures omit methods and still decode; explicit null and malformed drafts refuse"
  ) {
    assert(!figure.asJson.hcursor.keys.getOrElse(Vector.empty).toVector.contains("methods"))
    assertEquals(figure.asJson.as[FigureSpec], Right(figure))
    Vector(Json.Null, Json.obj("base" -> Json.fromString("base")), Json.fromString("draft"))
      .foreach { bad =>
        assert(figure.asJson.mapObject(_.add("methods", bad)).as[FigureSpec].isLeft)
      }
  }

  test("stored methods alone select document 8 and science 4; every older rung refuses") {
    val document      = withDraft
    val docLadder     = ok(StudioDocument.ladder)
    val scienceLadder = ok(ScienceContent.ladder)
    assertEquals(docLadder.earliest(document).version, 8)
    assertEquals(scienceLadder.earliest(document.science).version, 4)
    refuseEarlier(docLadder, document)
    val envelope = ok(scienceLadder.codec.encode(document.science))
    scienceLadder.versions.dropRight(1).foreach { version =>
      assert(scienceLadder.writeAt(version, document.science).isLeft, s"write $version")
      assert(scienceLadder.readAt(version, document.science.asJson).isLeft, s"read $version")
      assert(
        scienceLadder.codec.decode(relabel(envelope, version.version)).isLeft,
        s"relabel $version"
      )
    }
  }

  test(
    "new commands and a draft-bearing restore select journal 7 and refuse every earlier rung"
  ) {
    val ladder = ok(CommandJournal.ladder)
    Vector[Command](
      MovePanel(id, first, 4),
      SetFigureMethods(id, Some(draft)),
      SetFigureMethods(id, None),
      RestoreFigure(stored(withDraft))
    ).foreach { command =>
      val line = JournalLine.Entry(1, JournalEntry.Apply(command))
      assertEquals(ladder.earliest(line).version, 7)
      refuseEarlier(ladder, line)
      assertEquals(ladder.codec.decode(ok(ladder.codec.encode(line))), Right(line))
    }
    val legacy = JournalLine.Entry(1, JournalEntry.Apply(RestoreFigure(figure)))
    assertEquals(ladder.earliest(legacy).version, 1)
  }

  test("removing methods restores the legacy document encoding and scientific digest") {
    val before   = ok(StudioDocument.encode(base))
    val restored = changed(withDraft, SetFigureMethods(id, None))
    assertEquals(restored, base)
    assertEquals(StudioDocument.encode(restored), Right(before))
    assertEquals(StudioDocument.scienceDigest(restored), StudioDocument.scienceDigest(base))
    assertNotEquals(StudioDocument.scienceDigest(withDraft), StudioDocument.scienceDigest(base))
    assertEquals(withDraft.analyses, base.analyses)
    assertEquals(withDraft.runs, base.runs)
    assertEquals(withDraft.presentation, base.presentation)
    assertEquals(SetFigureMethods(id, Some(draft)).kind, ChangeKind.ReportingNoRerun)
  }

  test("methods apply, replacement and clear all have exact science undo and redo") {
    val replacement = ok(FigureMethodsDraft.of("other base", "", Some("")))
    var history     = History.start(base)
    Vector(Some(draft), Some(replacement), None).foreach { value =>
      val before = history.document
      val step   = ok(history.apply(SetFigureMethods(id, value)))
      assertEquals(step.effects, Vector(Effect.Persist))
      assertEquals(stored(step.history.document).methods, value)
      assertEquals(step.history.presentation, history.presentation)
      val undone = ok(step.history.undo).history
      assertEquals(undone.document, before)
      val redone = ok(undone.redo).history
      assertEquals(redone.document, step.history.document)
      history = redone
    }
  }

  test("methods errors name an unknown figure or unchanged figure") {
    val unknown = ok(FigureId.of(99))
    assertEquals(
      Reducer.run(base, SetFigureMethods(unknown, Some(draft))),
      Left(CommandError.UnknownFigure(unknown))
    )
    assertEquals(
      Reducer.run(base, SetFigureMethods(id, None)),
      Left(CommandError.NoChange("SetFigureMethods", Target.OnFigure(id)))
    )
    assertEquals(
      Reducer.run(withDraft, SetFigureMethods(id, Some(draft))),
      Left(CommandError.NoChange("SetFigureMethods", Target.OnFigure(id)))
    )
  }

  test("every panel move keeps identities and draft; inverse and redo restore exact ordering") {
    val document = withDraft
    figure.panels.zipWithIndex.foreach { (panel, from) =>
      figure.panels.indices.filter(_ != from).foreach { to =>
        val done = ok(History.start(document).apply(MovePanel(id, panel.letter, to))).history
        val expected = figure.panels.patch(from, Vector.empty, 1).patch(to, Vector(panel), 0)
        assertEquals(stored(done.document).panels, expected)
        assertEquals(stored(done.document).methods, Some(draft))
        assertEquals(done.science.done.head.inverse, MovePanel(id, panel.letter, from))
        assertEquals(ok(done.undo).history.document, document)
        assertEquals(ok(ok(done.undo).history.redo).history.document, done.document)
      }
    }
  }

  test("panel move refuses unknown identity, bad final index and unchanged position") {
    val unknown = ok(FigureId.of(99))
    val missing = ok(PanelLetter.of("Z"))
    assertEquals(
      Reducer.run(base, MovePanel(unknown, first, 1)),
      Left(CommandError.UnknownFigure(unknown))
    )
    assertEquals(
      Reducer.run(base, MovePanel(id, missing, 1)),
      Left(CommandError.UnknownPanel(id, missing))
    )
    Vector(-1, figure.panels.size).foreach { index =>
      assertEquals(
        Reducer.run(base, MovePanel(id, first, index)),
        Left(CommandError.PanelIndex(id, index, figure.panels.size))
      )
    }
    assertEquals(
      Reducer.run(base, MovePanel(id, first, 0)),
      Left(CommandError.NoChange("MovePanel", Target.OnPanel(id, first)))
    )
  }

  test("all panel rebuilds and successful rebind preserve exact stored methods") {
    val extra    = figure.panels.head.copy(letter = ok(PanelLetter.of("F")))
    val commands = Vector[Command](
      AddPanel(id, 1, extra),
      RemovePanel(id, first),
      MovePanel(id, first, 4),
      RetitlePanel(id, first, "Changed title"),
      SetPanelScale(id, first, PanelScale.AllScales),
      SetPanelSelection(id, first, PanelSelection.AllQueries)
    )
    commands.foreach { command =>
      val done = ok(History.start(withDraft).apply(command)).history
      assertEquals(stored(done.document).methods, Some(draft), command)
      assertEquals(ok(done.undo).history.document, withDraft, command)
    }
    val other   = base.figures(1)
    val both    = changed(withDraft, SetFigureMethods(other.id, Some(draft)))
    val rebound =
      ok(History.start(both).apply(BindFigure(other.id, RunId(7), other.reporting))).history
    assertEquals(stored(rebound.document, other.id).methods, Some(draft))
    assertEquals(stored(rebound.document).methods, Some(draft))
    assertEquals(ok(rebound.undo).history.document, both)
  }

  test("delete undo restores the full draft and removing the last panel still refuses") {
    val deleted = ok(History.start(withDraft).apply(DeleteFigure(id))).history
    assertEquals(deleted.science.done.head.inverse, RestoreFigure(stored(withDraft)))
    assertEquals(ok(deleted.undo).history.document, withDraft)
    assertEquals(ok(ok(deleted.undo).history.redo).history.document, deleted.document)
    val one =
      figure.panels.tail.foldLeft(withDraft)((d, p) => changed(d, RemovePanel(id, p.letter)))
    assertEquals(stored(one).methods, Some(draft))
    assertEquals(
      Reducer.run(one, RemovePanel(id, first)),
      Left(
        CommandError.Refused(
          "RemovePanel",
          Target.OnPanel(id, first),
          DocumentError.NoPanels(id)
        )
      )
    )
  }

  test(
    "journal replay restores methods, pending choice, panel order and history with checkpoints"
  ) {
    val entries = Vector(
      JournalEntry.Apply(SetFigureMethods(id, Some(draft))),
      JournalEntry.Apply(MovePanel(id, first, 4)),
      JournalEntry.Undo,
      JournalEntry.Redo,
      JournalEntry.Apply(DeleteFigure(id)),
      JournalEntry.Undo,
      JournalEntry.Apply(SetFigureMethods(id, None)),
      JournalEntry.Undo
    )
    val live   = entries.foldLeft(History.start(base))((h, e) => ok(h.perform(e)).history)
    val text   = ok(CommandJournal.write(base, entries, checkpointEvery = 1))
    val replay = ok(CommandJournal.replay(base, text))
    assertEquals(replay.history, live)
    assertEquals(replay.entries, entries)
    assertEquals(stored(replay.history.document).methods, Some(draft))
    assertEquals(stored(replay.history.document).panels.last.letter, first)
    assertEquals(replay.torn, None)
  }

  test("two figures have independent drafts and view edits do not undo authored text") {
    val other  = base.figures(1).id
    val second = ok(FigureMethodsDraft.of("Second", "Second edited", None))
    val a      = ok(History.start(base).apply(SetFigureMethods(id, Some(draft)))).history
    val b      = ok(a.apply(SetFigureMethods(other, Some(second)))).history
    val viewed = ok(b.apply(SetTheme(Theme.Dark))).history
    val undone = ok(viewed.undo).history
    assertEquals(stored(undone.document).methods, Some(draft))
    assertEquals(stored(undone.document, other).methods, None)
    assertEquals(undone.document.presentation, viewed.document.presentation)
    assertEquals(ok(undone.redo).history.document, viewed.document)
  }
