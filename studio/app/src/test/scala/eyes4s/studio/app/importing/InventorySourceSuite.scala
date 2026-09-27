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

package eyes4s.studio.app.importing

import cats.effect.IO
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.{AdmissionDecision, DatasetRevisionSpec, Sources}
import eyes4s.studio.core.fixture.{FakeStudyBackend, StoryMoment, StoryMoments}
import munit.CatsEffectSuite

/** The inventory's line in the Data sources and its absent count (ticket
  * S5.4; Data.dc.html): the fixture's 960 trials and 6 absent as the fake
  * backend serves them, and, without a joined inventory, the reason absent
  * trials cannot be counted instead of a count.
  */
class InventorySourceSuite extends CatsEffectSuite:

  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private val r3   = DatasetRevision(3)
  private val spec = ok(StoryMoments.t2).dataset(r3).get

  test("fixture: trials.csv · 960 trials · inventory; Absent · 6") {
    for
      fake    <- FakeStudyBackend.create[IO](StoryMoment.T2)
      summary <- fake.admission(r3).map(ok)
    yield
      val vm = InventorySource.vm(spec, InventoryAnswer.Summary(summary))
      assertEquals(vm.source, "trials.csv · 960 trials · inventory")
      assertEquals(
        (vm.absentLabel, vm.absentDefinition, vm.absentCount),
        ("Absent", "in trials.csv, no fixation records at all", Some("6"))
      )
      assertEquals((vm.absentNote, vm.issues), (None, Vector.empty))
  }

  test("without trials.csv, it says absent trials cannot be counted, never 0") {
    val fixationsOnly = spec.copy(
      sources = ok(Sources.of(spec.sources.fixations.toVector)),
      inventory = None
    )
    val vm = InventorySource.vm(fixationsOnly, InventoryAnswer.NotAsked)
    assertEquals(vm.source, "No trials.csv")
    assertEquals(vm.absentCount, None)
    assertEquals(
      vm.absentNote,
      Some("Absent trials cannot be counted without a trial inventory (trials.csv).")
    )
    // The backend joined none: the same, whatever the summary's other counts.
    val undeclared = InventorySource.vm(
      spec,
      InventoryAnswer.Summary(
        AdmissionSummary(
          r3,
          DatasetState.Admitted,
          InventoryJoin.Undeclared,
          937,
          Vector.empty,
          5,
          11520,
          WindowTotals(0, 0, 0, 0, 0, 0, 0, None, 0L, 0L, 0L),
          259,
          257,
          Vector.empty,
          ""
        )
      )
    )
    assertEquals((undeclared.source, undeclared.absentCount), ("trials.csv · not joined", None))
    assertEquals(undeclared.absentNote, vm.absentNote)
  }

  test("an unmapped trials.csv, or one not yet admitted, is not counted either") {
    val unmapped = InventorySource.vm(spec.copy(inventory = None), InventoryAnswer.NotAsked)
    assertEquals(unmapped.source, "trials.csv · columns not mapped")
    assertEquals(
      unmapped.absentNote,
      Some("Absent trials cannot be counted until the columns of trials.csv are mapped.")
    )
    val pending = InventorySource.vm(
      spec.copy(decision = AdmissionDecision.Pending),
      InventoryAnswer.NotAsked
    )
    assertEquals(pending.source, "trials.csv · counted on admission")
    assertEquals(pending.absentCount, None)
  }

  test("conflicting metadata: eyes4s's refusal is shown naming the trial and the column") {
    val refusal = BackendError.InventoryRefused(
      r3,
      Vector(
        InventoryIssue.Conflict(
          TrialLabel("P01", "Encoding", "enc_01"),
          Vector(2, 962),
          Vector("response")
        )
      )
    )
    val vm = InventorySource.vm(spec, InventoryAnswer.Refused(refusal))
    assertEquals(vm.source, "trials.csv · refused by eyes4s")
    assertEquals(vm.absentCount, None)
    assertEquals(
      vm.issues,
      Vector(
        IssueVM(
          "Trial P01/Encoding/enc_01 has conflicting values in response: trials.csv records " +
            "2, 962 declare it differently.",
          Some("response"),
          true
        )
      )
    )
  }
