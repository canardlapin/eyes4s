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

package eyes4s.studio.app

import eyes4s.studio.app.keys.CommandRegistry
import eyes4s.studio.app.nav.Location
import eyes4s.studio.app.vm.Shell
import eyes4s.studio.core.command.{Command, HistoryStack, JournalEntry}
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.core.execution.ExecutionEffect
import org.scalacheck.Prop.forAll
import org.scalacheck.Test

/** The laws of the app update (ticket S1.0), over generated intent sequences
  * on the JVM and Scala.js: update is deterministic and total, every model
  * it reaches projects to view-models, and the document intents mirror the
  * S2.2 history exactly.
  */
class AppUpdateLawsSuite extends munit.ScalaCheckSuite:
  import AppGen.*

  override def scalaCheckTestParameters: Test.Parameters =
    super.scalaCheckTestParameters.withMinSuccessfulTests(60)

  property("update is deterministic: the same model and intent give the same result") {
    forAll(session(16)) { traces =>
      traces.foreach { t =>
        assertEquals(AppModel.update(t.before, t.intent), (t.after, t.effects), t.intent)
      }
    }
  }

  property("update is total: every model reached keeps its invariants and projects") {
    forAll(session(20)) { traces =>
      traces.foreach { t =>
        val m = t.after
        assertEquals(m.location.perspective, m.document.presentation.perspective, t.intent)
        assertEquals(m.layout.perspective, m.perspective, t.intent)
        assert(m.layout.pane(m.focusedPane).isDefined, t.intent)
        val vm = Shell.project(m)
        assertEquals(vm.appBar.perspectives.count(_.selected), 1, t.intent)
        assertEquals(vm.context.trail.count(_.current), m.location.trail.size.min(1), t.intent)
        assert(vm.status.hint.nonEmpty && vm.status.job.text.nonEmpty, t.intent)
        assert(vm.status.saved.nonEmpty, t.intent)
        // No template placeholder survives rendering.
        val texts = Vector(vm.context.freshness.text, vm.status.hint, vm.status.job.text) ++
          vm.context.trail.map(_.label) ++ vm.status.selected ++ vm.banner.toVector.flatMap(b =>
            b.lead +: b.detail +: b.actions.map(_.label)
          ) ++ vm.context.draft.map(_.text) :+ vm.appBar.jobs.text
        texts.foreach(s => assert(!s.contains("{"), s"$s after ${t.intent}"))
        // S1.11: every stop of every reachable model has a role and a name.
        eyes4s.studio.app.vm.A11y
          .tabOrder(m)
          .foreach(s => assert(s.name.trim.nonEmpty, s"$s after ${t.intent}"))
        // S1.8: the status bar always has its four slots.
        assert(
          vm.status.selected.forall(_.nonEmpty) && vm.status.noSelection.nonEmpty,
          t.intent
        )
      }
    }
  }

  property("Dispatch, Undo and Redo mirror the S2.2 history, journal entry first") {
    forAll(session(20)) { traces =>
      traces.foreach { t =>
        val mirrored = t.intent match
          case Intent.Dispatch(c) => Some((JournalEntry.Apply(c), t.before.history.apply(c)))
          case Intent.Undo(HistoryStack.Science) =>
            Some((JournalEntry.Undo, t.before.history.undoOn(HistoryStack.Science)))
          case Intent.Undo(HistoryStack.Presentation) =>
            Some((JournalEntry.UndoView, t.before.history.undoOn(HistoryStack.Presentation)))
          case Intent.Redo(HistoryStack.Science) =>
            Some((JournalEntry.Redo, t.before.history.redoOn(HistoryStack.Science)))
          case Intent.Redo(HistoryStack.Presentation) =>
            Some((JournalEntry.RedoView, t.before.history.redoOn(HistoryStack.Presentation)))
          case _ => None
        mirrored.foreach {
          case (entry, Right(step)) =>
            assertEquals(t.after.history, step.history, t.intent)
            val doc = step.history.document
            // A persisting step is the model's next edit, and names it.
            val mapped =
              step.effects.map(AppEffect.of(_, doc, t.after.save.edits, t.before.prepared))
            if step.effects.contains(eyes4s.studio.core.command.Effect.Persist) then
              assertEquals(t.after.save.edits, t.before.save.edits.next, t.intent)
            else assertEquals(t.after.save.edits, t.before.save.edits, t.intent)
            val submits = mapped
              .collect { case AppEffect.Execution(e) => e }
              .flatMap(ExecutionEffect.submitted)
            // Compatible declarations preserve the exact native request;
            // incompatible identities without a submission are announced once.
            val require = AppModel
              .requestedStamp(doc)
              .filter(s =>
                submits.isEmpty && !t.before.jobs.shelf.required
                  .exists(_.agreesWithDeclarations(s))
              )
              .map(s => AppEffect.Execution(ExecutionEffect.Require(s)))
            assertEquals(t.effects, (AppEffect.Journal(entry) +: mapped) ++ require, t.intent)
            (submits ++ require.flatMap(_ => AppModel.requestedStamp(doc))).lastOption
              .foreach(s => assertEquals(t.after.jobs.shelf.required, Some(s), t.intent))
            assertEquals(t.after.notice, None, t.intent)
          case (entry, Left(error)) =>
            assertEquals(t.after.history, t.before.history, t.intent)
            assertEquals(t.effects, Vector.empty, t.intent)
            assertEquals(t.after.notice, Some(Notice.Refused(entry, error)), t.intent)
        }
      }
    }
  }

  property("undo after a reversible Dispatch restores the document") {
    forAll(session(12)) { traces =>
      traces.foreach { t =>
        t.intent match
          case Intent.Dispatch(c) if t.after.history != t.before.history =>
            val stack = eyes4s.studio.core.command.History.stackOf(c)
            if t.after.history.stack(stack).done.headOption.exists(_.command == c) then
              val (undone, _) = AppModel.update(t.after, Intent.Undo(stack))
              assertEquals(undone.document, t.before.document, c)
          case _ => ()
      }
    }
  }

  property("only document intents change the document; hover never selects") {
    forAll(session(20)) { traces =>
      traces.foreach { t =>
        t.intent match
          case Intent.HoverOver(_, _) =>
            assertEquals(t.after.selection, t.before.selection)
            assertEquals(t.after.document, t.before.document)
          case Intent.Select(_) | Intent.JobsChanged(_) | Intent.SessionChanged(_) |
              Intent.ItemsLoaded(_) | Intent.Saved(_, _) | Intent.SaveFailed(_) |
              Intent.FocusPane(_) | Intent.FocusNextPane | Intent.ToggleMaximize |
              Intent.PaneSubject(_, _) | Intent.Dismiss | Intent.RequestDiscardDraft |
              Intent.RequestImport =>
            assertEquals(t.after.document, t.before.document, t.intent)
          case _ => ()
      }
    }
  }

  property("when the shown run changes, the selection moves context and keeps no other run") {
    forAll(session(20)) { traces =>
      traces.foreach { t =>
        val shown = t.after.document.presentation.shownRun
        if t.before.document.presentation.shownRun != shown then
          assertEquals(t.after.selection.context, t.before.selection.context.next, t.intent)
          t.after.selection.selected.foreach { ref =>
            assert(AppModel.runOf(ref).forall(r => shown.contains(r)), (t.intent, ref))
          }
      }
    }
  }

  test("the generated sessions reach every branch the laws speak about") {
    val params = org.scalacheck.Gen.Parameters.default
    val seed   = org.scalacheck.rng.Seed(20260926L)
    val traces =
      (0 until 150).flatMap(i => session(20).pureApply(params, seed.reseed(i.toLong)))
    def count(p: Trace => Boolean) = traces.count(p)
    def changed(t: Trace)          = t.after.history != t.before.history
    val reached                    = Map(
      "dispatch applied"   -> count(t => t.intent.isInstanceOf[Intent.Dispatch] && changed(t)),
      "dispatch refused"   -> count(t => t.intent.isInstanceOf[Intent.Dispatch] && !changed(t)),
      "undo applied"       -> count(t => t.intent.isInstanceOf[Intent.Undo] && changed(t)),
      "redo applied"       -> count(t => t.intent.isInstanceOf[Intent.Redo] && changed(t)),
      "selection accepted" -> count(t =>
        t.intent.isInstanceOf[Intent.Select] && t.after.selection != t.before.selection
      ),
      "selection refused" -> count(t =>
        t.intent.isInstanceOf[Intent.Select] && t.after.notice
          .exists(_.isInstanceOf[Notice.SelectionRefused])
      ),
      "perspective moved" -> count(t => t.after.perspective != t.before.perspective),
      "back taken"        -> count(t =>
        t.intent == Intent.Back && t.after.location != t.before.location
      ),
      "shown run changed" -> count(t =>
        t.after.document.presentation.shownRun != t.before.document.presentation.shownRun
      ),
      "job requested" -> count(_.effects.exists {
        case AppEffect.Execution(ExecutionEffect.Submit(_)) => true
        case _                                              => false
      }),
      "run shown from the shelf" -> count(t =>
        t.intent.isInstanceOf[Intent.ShowRun] &&
          t.after.jobs.shelf.shown != t.before.jobs.shelf.shown
      ),
      "key invoked" -> count(t =>
        t.intent.isInstanceOf[Intent.KeyPressed] && t.effects.nonEmpty
      )
    )
    reached.foreach((branch, n) => assert(n > 0, s"no generated step reached: $branch"))
  }

  property("Show goes through the shelf: refused with its NotReady exactly when it says so") {
    forAll(session(20)) { traces =>
      traces.foreach { t =>
        t.intent match
          case Intent.ShowRun(run) =>
            t.before.jobs.show(run) match
              case Left(error) =>
                assertEquals(t.effects, Vector.empty, run)
                assertEquals(t.after.history, t.before.history, run)
                assertEquals(t.after.notice, Some(Notice.ExecutionRefused(error)), run)
              case Right(_) =>
                assertEquals(t.after.jobs.ready, None, run)
          case _ => ()
      }
    }
  }

  property("a refused selection changes nothing but the notice") {
    forAll(session(20)) { traces =>
      traces.foreach { t =>
        t.intent match
          case Intent.Select(input) if t.before.selection.submit(input).isLeft =>
            assertEquals(t.after.selection, t.before.selection, input)
            assertEquals(t.after.history, t.before.history, input)
            assertEquals(t.after.navigation, t.before.navigation, input)
            assertEquals(t.effects, Vector.empty, input)
            assert(t.after.notice.exists {
              case Notice.SelectionRefused(_) => true
              case _                          => false
            })
          case _ => ()
      }
    }
  }

  property("a key is its registered command, and a disabled command changes only the notice") {
    forAll(session(12)) { traces =>
      traces.foreach { t =>
        t.intent match
          case Intent.KeyPressed(chord) =>
            val expected = CommandRegistry.keymap.get(chord) match
              case Some(id) => AppModel.update(t.before, Intent.Invoke(id))
              case None     => (t.before, Vector.empty)
            assertEquals((t.after, t.effects), expected, chord)
          case Intent.Invoke(id) =>
            CommandRegistry.find(id).flatMap(_.intent(t.before)) match
              case Some(resolved) =>
                assertEquals((t.after, t.effects), AppModel.update(t.before, resolved))
              case None =>
                val why = CommandRegistry.find(id).flatMap(_.reason(t.before))
                assertEquals(t.after.notice, Some(Notice.Unavailable(id, why)))
                assertEquals(t.effects, Vector.empty)
          case _ => ()
      }
    }
  }

  property("Back returns to where a navigation started, and Forward undoes Back") {
    forAll(session(16)) { traces =>
      traces.foreach { t =>
        t.intent match
          case Intent.Navigate(to) if t.after.location == to && t.before.location != to =>
            val (back, _) = AppModel.update(t.after, Intent.Back)
            assertEquals(back.location, t.before.location, to)
            val (forward, _) = AppModel.update(back, Intent.Forward)
            assertEquals(forward.location, to)
          case _ => ()
      }
    }
  }

  // -------------------------------------------------------------------------
  // Story-moment examples
  // -------------------------------------------------------------------------

  test("t2: Save & run submits run 8's stamp, journals it, persists and requires it") {
    import eyes4s.studio.core.fixture.StoryMoments.*
    val (m, effects) =
      AppModel.update(StoryModels.t2Compare, Intent.Dispatch(Command.SaveAndRun(None)))
    val stamp = AppModel.stampOf(m.document, rev5, r3)
    assertEquals(
      effects,
      Vector(
        AppEffect.Journal(JournalEntry.Apply(Command.SaveAndRun(None))),
        AppEffect.Execution(ExecutionEffect.Submit(stamp)),
        AppEffect.Persist(m.save.edits)
      )
    )
    assertEquals(m.jobs.shelf.required, Some(stamp))
    assert(m.save.edited)
  }

  test("undo at the Save & run barrier says why, naming the command by its label") {
    val (m, _) =
      AppModel.update(StoryModels.t2Compare, Intent.Dispatch(Command.SaveAndRun(None)))
    assertEquals(CommandRegistry.undo.enabled(m), false)
    val (blocked, effects) =
      AppModel.update(m, Intent.KeyPressed(CommandRegistry.undo.shortcut.get))
    assertEquals(effects, Vector.empty)
    assertEquals(
      blocked.notice.map(_.message),
      Some("Undo: Saving rev 5 started run 8; earlier edits can no longer be undone.")
    )
    assertEquals(vm.Shell.project(blocked).notice.map(_.text), blocked.notice.map(_.message))
  }

  test("t3: Cancel on the chip cancels run 8's job") {
    import eyes4s.studio.core.fixture.StoryMoments.*
    val cancel       = vm.Shell.appBar(StoryModels.t3Summary).jobs.action.get
    val (_, effects) = AppModel.update(StoryModels.t3Summary, cancel.intent)
    assert(effects.contains(AppEffect.Execution(ExecutionEffect.Cancel(run8Job))), effects)
  }

  test("Showing another run rebases the selection and drops refs of the old run") {
    import eyes4s.studio.core.fixture.StoryMoments.*
    val m0 = StoryModels.t2Compare
    assertEquals(m0.selection.selected, Vector(StoryModels.pair))
    val (m1, _) = AppModel.update(m0, Intent.Dispatch(Command.ShowRun(Some(run5))))
    assertEquals(m1.document.presentation.shownRun, Some(run5))
    assertEquals(m1.selection.selected, Vector.empty)
    assertEquals(m1.selection.context, m0.selection.context.next)
  }

  test("Discard draft asks first; Confirm discards; the stale confirmation is refused") {
    val m0       = StoryModels.t2Compare
    val (m1, e1) = AppModel.update(m0, Intent.RequestDiscardDraft)
    assertEquals(e1, Vector.empty)
    assert(m1.pending.isDefined)
    val (m2, e2) = AppModel.update(m1, Intent.Confirm)
    assertEquals(m2.document.draft, None)
    assertEquals(m2.pending, None)
    assert(e2.contains(AppEffect.Journal(JournalEntry.Apply(Command.DiscardDraft))))
    // The draft goes some other way while the confirmation is open.
    val (m3, _)  = AppModel.update(m1, Intent.Dispatch(Command.DiscardDraft))
    val (m4, e4) = AppModel.update(m3, Intent.Confirm)
    assertEquals(e4, Vector.empty)
    assertEquals(
      m4.notice,
      Some(
        Notice.Outdated(Confirmation.DiscardDraft(eyes4s.studio.core.fixture.StoryMoments.rev5))
      )
    )
  }

  test("A crumb crosses perspectives: the pair crumb in Explore returns to Compare · query") {
    val m0      = StoryModels.t2Explore
    val (m1, _) = AppModel.update(m0, Intent.OpenCrumb(4))
    assertEquals(m1.perspective, Perspective.Compare)
    assertEquals(m1.location, Location(Perspective.Compare, StoryModels.queryTrail))
    assertEquals(m1.layout.id.value, "compare.query")
    val (m2, _) = AppModel.update(m1, Intent.OpenCrumb(0))
    assertEquals(m2.layout.id.value, "compare.summary")
  }

  test("F6 cycles the groups of the current layout; ⌘⇧↩ toggles maximize") {
    val m0     = StoryModels.t2Compare
    val groups = m0.layout.groups.size
    val cycled =
      (1 to groups).foldLeft(m0)((m, _) => AppModel.update(m, Intent.FocusNextPane)._1)
    assertEquals(cycled.focusedPane, m0.focusedPane)
    val max = AppModel.update(m0, Intent.KeyPressed(CommandRegistry.maximize.shortcut.get))._1
    assert(max.isMaximized)
  }
