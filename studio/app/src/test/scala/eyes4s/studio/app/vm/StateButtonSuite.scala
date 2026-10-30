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

package eyes4s.studio.app.vm

import eyes4s.studio.app.{AppModel, Confirmation, Intent, StoryModels}
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.core.fixture.StoryMoments

/** The study-wide state button and its menu (overhaul decisions 2 and 3),
  * in the golden's words (docs/studio/overhaul/mockups/state-menu.html).
  */
class StateButtonSuite extends munit.FunSuite:

  test("idle (t2): the run, the shown data muted, and the draft not run") {
    val s = Shell.state(StoryModels.t2Compare)
    assertEquals(s.text, "Run 7 · current | Data r3 | Analysis rev 5 · not run")
    assertEquals(s.run.dot, Some(FreshnessTone.Current))
    assertEquals((s.data.muted, s.data.draft), (true, false))
    assertEquals((s.analysis.muted, s.analysis.draft), (false, true))
    assertEquals(s.cancel, None)
    assertEquals(s.confirm, None)
    assertEquals(
      s.menu.map(_.head),
      Vector("Run 7 · current", "Dataset r3 · admitted", "Analysis rev 5 · not run", "Jobs")
    )
    assertEquals(s.menu.head.facts, Vector("Analysis" -> "rev 4", "Data" -> "r3"))
    val draft = s.menu(2)
    assertEquals(draft.actions.map(_.label), Vector("Review in Analysis", "Discard draft"))
    assertEquals(
      draft.actions.map(_.intent),
      Vector(Intent.ReviewDraft, Intent.RequestDiscardDraft)
    )
    assertEquals(draft.destructive, Some(1))
    assertEquals(s.menu(3).note, Some("None running"))
    assertEquals(
      s.accessible,
      "Study state: Run 7 · current | Data r3 | Analysis rev 5 · not run. Opens the state menu"
    )
  }

  test("a dataset not yet admitted (t1): the Data slot carries the draft mark") {
    val s = Shell.state(StoryModels.t1Data)
    assertEquals(s.text, "Run 5 · current | Data r3 · not admitted | Analysis rev 3")
    assertEquals((s.data.draft, s.data.muted), (true, false))
    assertEquals(s.analysis.muted, true)
    val dataset = s.menu(1)
    assertEquals(dataset.head, "Dataset r3 · not admitted")
    assertEquals(
      dataset.note,
      Some("Run 5 (rev 3) used r2 · becomes stale when r3 is admitted")
    )
    assertEquals(
      dataset.actions.map(_.intent),
      Vector(Intent.SwitchPerspective(Perspective.Data))
    )
  }

  test("running (t3): showing run 7, and the Analysis slot is run 8 with its count") {
    val s = Shell.state(StoryModels.t3Summary)
    assertEquals(s.text, "Showing run 7 | Data r3 | Run 8 · comparing 21,400 / 44,845")
    assertEquals(s.run.dot, Some(FreshnessTone.Showing))
    assertEquals(s.analysis.count, Some("21,400 / 44,845"))
    assert(s.analysis.progress.exists(p => p > 0.47 && p < 0.48), s.analysis)
    val cancel = s.cancel.getOrElse(fail("no cancel"))
    assertEquals(cancel.accessible, "Cancel run 8")
    assertEquals(cancel.intent, Intent.RequestCancelJob(StoryMoments.run8Job))
    assertEquals(s.menu(2).head, "Analysis rev 5 · running as run 8")
    val jobs = s.menu(3)
    assertEquals(jobs.note, Some("Run 8 · Comparing · 21,400 / 44,845 pairs"))
    assertEquals(
      jobs.actions.map(_.label),
      Vector("Show run 8 · when finished", "Cancel run 8…")
    )
    assertEquals(jobs.actions.map(_.enabled), Vector(false, true))
  }

  test("cancel asks inline: the menu shows the confirmation, Keep running the default") {
    val m          = StoryModels.t3Summary
    val (asked, _) = AppModel.update(m, Shell.state(m).cancel.get.intent)
    assertEquals(
      asked.pending,
      Some(Confirmation.CancelRun(StoryMoments.run8Job, StoryMoments.run8))
    )
    val confirm = Shell.state(asked).confirm.getOrElse(fail("no confirmation"))
    assertEquals(confirm.head, "Cancel run 8?")
    assertEquals(confirm.note, Some("21,400 of 44,845 pairs are done. Run 7 stays shown."))
    assertEquals(confirm.actions.map(_.label), Vector("Cancel run", "Keep running"))
    assertEquals(confirm.actions.map(_.intent), Vector(Intent.Confirm, Intent.Dismiss))
    assertEquals((confirm.destructive, confirm.default), (Some(0), Some(1)))
    // The discard-draft bar is not this confirmation's.
    assertEquals(Shell.confirmation(asked), None)
  }

  test("⌘. asks too, and is inert when no job can be cancelled") {
    import eyes4s.studio.app.keys.{CommandRegistry, Key, KeyChord}
    val chord = KeyChord.command(Key.Period)
    assertEquals(CommandRegistry.cancelRun.shortcut, Some(chord))
    val (asked, effects) = AppModel.update(StoryModels.t3Summary, Intent.KeyPressed(chord))
    assertEquals(effects, Vector.empty)
    assert(asked.pending.exists(_.isInstanceOf[Confirmation.CancelRun]), asked.pending)
    assertEquals(CommandRegistry.cancelRun.intent(StoryModels.t2Compare), None)
  }
