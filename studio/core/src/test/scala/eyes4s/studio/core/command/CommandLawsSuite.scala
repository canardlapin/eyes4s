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

import eyes4s.studio.core.document.StudioDocument
import io.circe.syntax.*
import org.scalacheck.Prop.forAll
import org.scalacheck.rng.Seed
import org.scalacheck.{Gen, Test}

/** The command laws (ticket S2.2) over generated sessions, on the JVM and
  * Scala.js: undo inverts every reversible command, redo restores it, view
  * edits never touch the science and science edits never touch the view,
  * undo walks back exactly to the last barrier, and commands round-trip
  * through JSON.
  */
class CommandLawsSuite extends munit.ScalaCheckSuite:
  import CommandGen.*

  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(60)

  private def applied(t: Trace): Option[(Command, Step)] = (t.entry, t.result) match
    case (JournalEntry.Apply(c), Right(s)) => Some(c -> s)
    case _                                 => None

  private def recording(t: Trace): Option[Recording] = t.entry match
    case JournalEntry.Apply(c) => Reducer.run(t.before.document, c).toOption.map(_.recording)
    case _                     => None

  property("undo(do(c)) restores the document and redo(undo(do(c))) restores do(c)") {
    forAll(session(12)) { (_, traces) =>
      traces.foreach { t =>
        (applied(t), recording(t)) match
          case (Some(c, s), Some(Recording.Reversible(_))) =>
            val which  = History.stackOf(c)
            val undone = s.history.undoOn(which)
            assertEquals(undone.map(_.history.document), Right(t.before.document), c)
            assertEquals(undone.map(_.effects.contains(Effect.Persist)), Right(true), c)
            val redone = undone.flatMap(_.history.redoOn(which))
            assertEquals(redone.map(_.history.document), Right(s.history.document), c)
            assertEquals(redone.map(_.history.stack(which)), Right(s.history.stack(which)), c)
          case _ => ()
      }
    }
  }

  property("undo still inverts an edit after backend facts and view edits come between") {
    forAll(interleaving) { (h, c, between) =>
      Reducer.run(h.document, c).map(_.recording) match
        case Right(Recording.Reversible(_)) =>
          val done = h.apply(c).toOption.get.history
          // Apply each interleaved command on both sides while both accept it.
          val (after, without) = between.foldLeft((done, h)) { case ((l, r), x) =>
            (l.apply(x), r.apply(x)) match
              case (Right(ls), Right(rs)) => (ls.history, rs.history)
              case _                      => (l, r)
          }
          val which  = History.stackOf(c)
          val undone = after.undoOn(which)
          assertEquals(undone.map(_.history.document), Right(without.document), (c, between))
          assertEquals(
            undone.flatMap(_.history.redoOn(which)).map(_.history.document),
            without.apply(c).map(_.history.document),
            (c, between)
          )
        case _ => ()
    }
  }

  property("a view-only command never changes the science, its hash or its history") {
    forAll(session(12)) { (_, traces) =>
      traces.foreach { t =>
        applied(t).filter(_._1.kind == ChangeKind.ViewOnly).foreach { (c, s) =>
          val (before, after) = (t.before.document, s.history.document)
          assertEquals(after.science, before.science, c)
          assertEquals(
            StudioDocument.scienceDigest(after),
            StudioDocument.scienceDigest(before),
            c
          )
          assertEquals(s.history.science, t.before.science, c)
        }
      }
    }
  }

  property("a science command never changes the presentation or the view history") {
    forAll(session(12)) { (_, traces) =>
      traces.foreach { t =>
        applied(t).filter(_._1.kind != ChangeKind.ViewOnly).foreach { (c, s) =>
          assertEquals(s.history.document.presentation, t.before.document.presentation, c)
          assertEquals(s.history.presentation, t.before.presentation, c)
        }
      }
    }
  }

  property("science undo never changes the presentation; view undo never changes the science") {
    forAll(session(16)) { (_, traces) =>
      traces.foreach { t =>
        (t.entry, t.result) match
          case (JournalEntry.Undo | JournalEntry.Redo, Right(s)) =>
            assertEquals(s.history.document.presentation, t.before.document.presentation)
          case (JournalEntry.UndoView | JournalEntry.RedoView, Right(s)) =>
            assertEquals(s.history.document.science, t.before.document.science)
          case _ => ()
      }
    }
  }

  property("undoing every edit returns to the last barrier, and redoing all restores the end") {
    val edits = (h: History) => edit(h.document).map(JournalEntry.Apply(_))
    forAll(session(10, edits)) { (start, traces) =>
      val lastBarrier = traces.lastIndexWhere(t =>
        t.result.isRight && recording(t).exists(_.isInstanceOf[Recording.Barrier])
      )
      val floor = if lastBarrier < 0 then start
      else traces(lastBarrier).result.toOption.get.history.document
      val end = traces.lastOption.fold(History.start(start))(t =>
        t.result.fold(_ => t.before, _.history)
      )
      def undoAll(h: History, n: Int): (History, CommandError, Int) =
        h.undo.fold(e => (h, e, n), s => undoAll(s.history, n + 1))
      val (bottom, stop, undone) = undoAll(end, 0)
      assertEquals(bottom.document, floor)
      if lastBarrier < 0 then
        assertEquals(stop, CommandError.NothingToUndo(HistoryStack.Science))
      else assert(stop.isInstanceOf[CommandError.UndoBlocked], stop)
      def redoAll(h: History): History = h.redo.fold(_ => h, s => redoAll(s.history))
      val top                          = redoAll(bottom)
      assertEquals(top.document, end.document)
      assertEquals(top.science.done.size, undone)
    }
  }

  property("a barrier empties the science stacks and blocks undo") {
    forAll(session(12)) { (_, traces) =>
      traces.foreach { t =>
        (applied(t), recording(t)) match
          case (Some(c, s), Some(Recording.Barrier(b))) =>
            assertEquals(s.history.science.done, Nil, c)
            assertEquals(s.history.science.undone, Nil, c)
            assertEquals(s.history.undo.map(_ => ()), Left(CommandError.UndoBlocked(b)), c)
            assertEquals(s.history.presentation, t.before.presentation, c)
          case (Some(c, s), Some(Recording.Unrecorded)) =>
            assertEquals(s.history.science, t.before.science, c)
            assertEquals(s.history.presentation, t.before.presentation, c)
          case _ => ()
      }
    }
  }

  property("a refused entry is a typed error with a message") {
    forAll(session(12)) { (_, traces) =>
      traces.flatMap(_.result.left.toOption).foreach(e => assert(e.message.nonEmpty, e))
    }
  }

  property("commands and journal entries round-trip through JSON") {
    forAll(session(12)) { (_, traces) =>
      traces.foreach { t =>
        assertEquals(
          io.circe.parser.decode[JournalEntry](t.entry.asJson.noSpaces),
          Right(t.entry)
        )
        t.entry match
          case JournalEntry.Apply(c) =>
            assertEquals(io.circe.parser.decode[Command](c.asJson.noSpaces), Right(c))
          case _ => ()
      }
    }
  }

  test("every command case applies in some session, and undo covers every reversible one") {
    val traces = (0L until 1200L).flatMap(seed =>
      session(16).pureApply(Gen.Parameters.default, Seed(seed))._2
    )
    val applied = traces.collect { case Trace(_, JournalEntry.Apply(c), Right(_)) =>
      c.name
    }.toSet
    val undone = traces.collect {
      case Trace(h, JournalEntry.Undo, Right(_))     => h.science.done.head.command.name
      case Trace(h, JournalEntry.UndoView, Right(_)) => h.presentation.done.head.command.name
    }.toSet
    assertEquals(allCommands.filterNot(applied), Vector.empty)
    assertEquals(allCommands.filterNot(irreversible).filterNot(undone), Vector.empty)
  }
