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

package eyes4s.studio.core.importing

import cats.effect.IO
import eyes4s.io.*
import eyes4s.kernel.*
import eyes4s.plan.{
  AttributeColumn,
  AttributeKind,
  InventoryError,
  InventoryTrial,
  SampleCountRule,
  TrialDisposition
}
import eyes4s.studio.core.backend.{
  BackendError,
  DatasetRevision,
  InventoryIssue,
  InventoryJoin,
  PageRequest,
  TrialLabel,
  TrialDisposition as StudioDisposition
}
import eyes4s.studio.core.document.{ColumnRole, InventoryMapping}
import eyes4s.studio.core.fixture.{FakeStudyBackend, GoldenCsv, StoryMoment, StoryMoments}
import munit.CatsEffectSuite

/** The trial inventory join (ticket S5.4) is eyes4s's: these tests hand the
  * golden tables to eyes4s-io under the mapping the story's r3 records, and
  * hold what eyes4s answers against what Studio shows.
  *
  *  - The recorded mapping joins 960 inventory trials, 6 of them absent, the
  *    same trials the fake backend serves as absent.
  *  - Repeated inventory rows with equal values are one trial: they never
  *    multiply a trial's fixations.
  *  - A trial-level value that differs between rows is eyes4s's refusal, and
  *    Studio's typed error names the trial and the column.
  *
  * The mapping's eyes4s columns are built here as the real backend (S3.7)
  * will build them: the identity, the item, and the response and declared
  * attributes as attribute columns.
  */
class InventoryJoinJvmSuite extends CatsEffectSuite:

  private def get[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private val screen = get(Frame.screen("screen", 1920, 1080))

  private val mapping: InventoryMapping = get(StoryMoments.inventory(occurrence = true))

  private def name(role: ColumnRole): Option[String] = mapping.column(role).map(_.value)

  private val trialColumns = get(
    TrialColumns.of(
      name(ColumnRole.Participant).get,
      name(ColumnRole.Phase).get,
      name(ColumnRole.Trial).get,
      name(ColumnRole.Occurrence)
    )
  )

  private val inventoryColumns = get(
    TrialInventoryColumns.of(
      trialColumns,
      name(ColumnRole.Item),
      name(ColumnRole.Response).map(AttributeColumn(_, AttributeKind.Text)).toVector ++
        mapping.coreAttributes
    )
  )

  private val table = get(
    FixationTable.of(
      trialColumns,
      "ordinal",
      "x",
      "y",
      TimeColumns("onset_ms", "duration_ms", TimestampUnit.Milliseconds),
      SampleCountRule.PositiveColumn("sample_count")
    )
  )

  private def join(trials: String): InventoryImport[Unit2D.Px] =
    get(
      FixationCsv.admitInventory(
        GoldenCsv.fixations,
        table,
        get(TrialInventory.read(trials, inventoryColumns)),
        screen
      )
    )

  private lazy val golden = join(GoldenCsv.trials)

  private def label(t: InventoryTrial): (String, String) =
    (t.identity.participant, t.identity.trial)

  test("fixture: eyes4s joins 960 inventory trials, 6 absent, the fake's absent trials") {
    val absent = golden.trials.filter(_.disposition == TrialDisposition.Absent)
    assertEquals((golden.trials.size, absent.size), (960, 6))
    for
      fake    <- FakeStudyBackend.create[IO](StoryMoment.T2)
      summary <- fake.admission(DatasetRevision(3))
      page    <- fake.ledger(DatasetRevision(3), get(PageRequest.of(0, 1000)))
    yield
      assertEquals(summary.map(_.inventory), Right(InventoryJoin.Joined(960, 6)))
      val served = get(page).entries.collect {
        case e if e.disposition == StudioDisposition.Absent =>
          (e.trial.participant, e.trial.trial)
      }
      assertEquals(served.toSet, absent.map(label).toSet)
  }

  test("repeated inventory rows with equal values are one trial: no fixation is multiplied") {
    val lines = GoldenCsv.trials.linesIterator.toVector
    // Every tenth trial declared twice more, the copies at the end.
    val copies  = lines.tail.zipWithIndex.collect { case (l, i) if i % 10 == 0 => l }
    val doubled = (lines ++ copies ++ copies).mkString("", "\n", "\n")
    val joined  = join(doubled)
    assertEquals(joined.trials.size, golden.trials.size)
    assertEquals(joined.fixations.admitted.size, golden.fixations.admitted.size)
    assertEquals(joined.fixations.admitted, golden.fixations.admitted)
    assertEquals(
      joined.trials.map(t => label(t) -> (t.records, t.disposition)),
      golden.trials.map(t => label(t) -> (t.records, t.disposition))
    )
    // Each declared trial lists every inventory record that declares it.
    val first = joined.trials.head
    assertEquals(first.rows.size, 3)
  }

  test("a conflicting trial-level value is a typed error naming the trial and the column") {
    val lines = GoldenCsv.trials.linesIterator.toVector
    // P01/Encoding/enc_01, declared again with another response.
    val conflicting = lines.lift(1).get.stripSuffix(",") + ",remembered"
    val text        = (lines :+ conflicting).mkString("", "\n", "\n")
    val issues      = TrialInventory.read(text, inventoryColumns) match
      case Left(FixationImportError.Inventory(errors)) => errors.toVector.map(InventoryIssue.of)
      case other => fail(s"expected eyes4s to refuse the inventory, got $other")
    assertEquals(
      issues,
      Vector(
        InventoryIssue.Conflict(
          TrialLabel("P01", "Encoding", "enc_01"),
          Vector(2, lines.size + 1),
          Vector("response")
        )
      )
    )
    val refused = BackendError.InventoryRefused(DatasetRevision(4), issues)
    assert(refused.message.contains("P01/Encoding/enc_01"), refused.message)
    assert(refused.message.contains("response"), refused.message)
    assertEquals(refused.code, "studio-backend.inventory-refused")
  }

  test("an unmapped eyes4s refusal keeps its name and message") {
    val e = InventoryError.DuplicateAttribute(Vector("response"))
    assertEquals(InventoryIssue.of(e), InventoryIssue.Other("DuplicateAttribute", e.message))
  }
