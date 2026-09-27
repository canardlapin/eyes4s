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
import eyes4s.plan.InventoryError
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.command.{Command, CommandError, Reducer}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.{FakeStudyBackend, StoryMoment, StoryMoments}
import io.circe.parser.decode
import io.circe.syntax.*
import munit.CatsEffectSuite

/** The trial inventory join (ticket S5.4): the inventory a dataset declares,
  * the counts admission reports against it, and the typed errors when
  * eyes4s refuses it. `InventoryJoinJvmSuite` holds the same answers against
  * eyes4s-io on the golden tables.
  */
class InventoryJoinSuite extends CatsEffectSuite:

  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private def column(s: String): ColumnName = ok(ColumnName.of(s))

  private val r3        = DatasetRevision(3)
  private val t1        = ok(StoryMoments.t1)
  private val inventory = ok(StoryMoments.inventory(occurrence = true))

  // --- What admission reports ------------------------------------------------

  test("fixture: 960 inventory trials, 6 absent; the ledger lists exactly those 6") {
    for
      fake    <- FakeStudyBackend.create[IO](StoryMoment.T2)
      summary <- fake.admission(r3).map(ok)
      page    <- fake.ledger(r3, ok(PageRequest.of(0, 1000))).map(ok)
    yield
      assertEquals(summary.inventory, InventoryJoin.Joined(960, 6))
      assertEquals((summary.inventoryTrials, summary.absent), (Some(960), Some(6)))
      val absent = page.entries.filter(_.disposition == TrialDisposition.Absent)
      assertEquals(absent.size, 6)
      assertEquals(page.page.total, 960)
      // Every inventory trial has one disposition: 937 + 17 + 6 = 960.
      assertEquals(
        summary.admitted + summary.quarantinedTrials + summary.noFixations + absent.size,
        960
      )
  }

  test("without an inventory, absent trials are not counted, never reported as 0") {
    val undeclared = InventoryJoin.Undeclared
    assertEquals((undeclared.trialCount, undeclared.absentCount), (None, None))
    assertEquals(
      (InventoryJoin.Joined(960, 0).trialCount, InventoryJoin.Joined(960, 0).absentCount),
      (Some(960), Some(0))
    )
    val json = (undeclared: InventoryJoin).asJson
    assertEquals(json.noSpaces, """{"Undeclared":{}}""")
    assertEquals(json.as[InventoryJoin], Right(undeclared))
  }

  // --- Conflicting metadata ------------------------------------------------

  test("conflicting metadata is a typed error naming the trial and the column") {
    val core =
      InventoryError.Conflict("P01", "Encoding", "enc_01", Vector(2, 962), Vector("response"))
    val issue = InventoryIssue.of(core)
    assertEquals(
      issue,
      InventoryIssue.Conflict(
        TrialLabel("P01", "Encoding", "enc_01"),
        Vector(2, 962),
        Vector("response")
      )
    )
    assertEquals(
      issue.message,
      "Trial P01/Encoding/enc_01 has conflicting values in response: trials.csv records " +
        "2, 962 declare it differently."
    )
    val refused = BackendError.InventoryRefused(r3, Vector(issue))
    assertEquals(
      refused.diagnostic.subject,
      Vector(
        DiagnosticLocus.Dataset(r3),
        DiagnosticLocus.Records(Vector(2, 962)),
        DiagnosticLocus.Field("response")
      )
    )
    assertEquals(refused.diagnostic.code, "studio-backend.inventory-refused")
    assertEquals(refused.asJson.as[BackendError], Right(refused))
  }

  test("eyes4s's other inventory refusals keep their record and column") {
    assertEquals(
      InventoryIssue.of(InventoryError.Width(7, 8, 6)),
      InventoryIssue.Width(7, 8, 6)
    )
    val field =
      InventoryIssue.of(InventoryError.Field(9, "occurrence", "x", "a positive integer"))
    assertEquals(field, InventoryIssue.Field(9, "occurrence", "x", "a positive integer"))
    assertEquals(
      field.loci,
      Vector(DiagnosticLocus.Record(9), DiagnosticLocus.Field("occurrence"))
    )
  }

  // --- The inventory mapping the document records ----------------------------

  test("the story's r3 records trials.csv's mapping; it round-trips") {
    val spec = t1.dataset(r3).get
    assertEquals(spec.inventory, Some(inventory))
    assertEquals(inventory.column(ColumnRole.Occurrence), Some(column("occurrence")))
    assertEquals(
      inventory.attributes.columns,
      Vector(column("display_kind"), column("image_file"))
    )
    assertEquals(decode[DatasetRevisionSpec](spec.asJson.noSpaces), Right(spec))
    // Without one, the version-1 wire form has no inventory field.
    assert(!spec.copy(inventory = None).asJson.noSpaces.contains("\"inventory\""))
  }

  test(
    "an inventory mapping refuses, naming the column: missing identity, foreign role, clash"
  ) {
    def binding(role: ColumnRole, name: String) = ColumnBinding(role, column(name))
    val identity                                = Vector(
      binding(ColumnRole.Participant, "participant"),
      binding(ColumnRole.Phase, "phase"),
      binding(ColumnRole.Trial, "trial")
    )
    assertEquals(
      InventoryMapping
        .of(identity.filterNot(_.role == ColumnRole.Phase), DeclaredAttributes.empty),
      Left(DocumentError.MissingInventoryRoles(Vector(ColumnRole.Phase)))
    )
    assertEquals(
      InventoryMapping.of(identity :+ binding(ColumnRole.X, "x"), DeclaredAttributes.empty),
      Left(DocumentError.InventoryRoleNotRead("x", ColumnRole.X))
    )
    val clash = ok(
      DeclaredAttributes.of(Vector(AttributeBinding(column("trial"), AttributeKindChoice.Text)))
    )
    assertEquals(
      InventoryMapping.of(identity, clash),
      Left(DocumentError.InventoryAttributeIsMapped("trial", ColumnRole.Trial))
    )
  }

  test("a trials source is mapped to commit; a revision saved before S5.4 loads unmapped") {
    val spec                                     = t1.dataset(r3).get
    val next                                     = DatasetRevision(4)
    def importing(inv: Option[InventoryMapping]) =
      Command.ImportSources(
        Some(r3),
        spec.sources,
        spec.mapping,
        spec.units,
        spec.geometry,
        spec.attributes,
        inv
      )
    Reducer.step(t1, importing(None)) match
      case Left(CommandError.Refused(_, _, error)) =>
        assertEquals(error, DocumentError.InventoryUnmapped(next, "inputs/trials.csv"))
        assert(error.message.contains("inputs/trials.csv"), error.message)
      case other => fail(s"expected an inventory refusal, got $other")
    val (imported, _) = ok(Reducer.step(t1, importing(Some(inventory))))
    assertEquals(imported.dataset(next).flatMap(_.inventory), Some(inventory))
    // Without a trials source there is nothing to map.
    val fixationsOnly = ok(Sources.of(spec.sources.fixations.toVector))
    assert(
      Reducer
        .step(
          t1,
          Command.ImportSources(
            Some(r3),
            fixationsOnly,
            spec.mapping,
            spec.units,
            spec.geometry,
            spec.attributes
          )
        )
        .isRight
    )
    // A revision saved before S5.4 has none: it loads (LegacyPhaseSuite reads
    // such a document), and verification is refused until it is mapped.
    val stored   = spec.copy(inventory = None, decision = AdmissionDecision.Pending)
    val unmapped = ok(
      StudioDocument.of(
        t1.datasets.map(d => if d.id == r3 then stored else d),
        t1.analyses,
        t1.draft,
        t1.runs,
        t1.reporting,
        t1.figures,
        t1.presentation,
        t1.jobs
      )
    )
    Reducer.step(unmapped, Command.VerifyDataset(r3)) match
      case Left(CommandError.Refused(_, _, error)) =>
        assertEquals(error, DocumentError.InventoryUnmapped(r3, "inputs/trials.csv"))
      case other => fail(s"expected an inventory refusal, got $other")
    // A re-map of it that still leaves trials.csv unmapped is refused, even
    // when it changes the fixation mapping: a legacy revision is only
    // revised into one with an inventory mapping.
    val withoutOccurrence =
      ok(ColumnMapping.of(stored.mapping.bindings.filterNot(_.role == ColumnRole.Occurrence)))
    assertNotEquals(withoutOccurrence, stored.mapping)
    Reducer.step(
      unmapped,
      Command.ReviseDataset(
        r3,
        withoutOccurrence,
        stored.units,
        stored.geometry,
        stored.attributes,
        None
      )
    ) match
      case Left(CommandError.Refused(_, _, error)) =>
        assertEquals(error, DocumentError.InventoryUnmapped(r3, "inputs/trials.csv"))
      case other => fail(s"expected an inventory refusal, got $other")
    // A re-map adds the mapping; undoing it restores the stored revision.
    val revise = Command.ReviseDataset(
      r3,
      stored.mapping,
      stored.units,
      stored.geometry,
      stored.attributes,
      Some(inventory)
    )
    val (remapped, _) = ok(Reducer.step(unmapped, revise))
    assertEquals(remapped.dataset(r3).flatMap(_.inventory), Some(inventory))
    val inverse = Command.ReviseDataset(
      r3,
      stored.mapping,
      stored.units,
      stored.geometry,
      stored.attributes,
      None
    )
    assertEquals(ok(Reducer.step(remapped, inverse))._1.dataset(r3), Some(stored))
  }

  test("a document refuses an inventory mapping without a trials source") {
    val spec          = t1.dataset(r3).get
    val fixationsOnly = ok(Sources.of(spec.sources.fixations.toVector))
    val orphan        = spec.copy(sources = fixationsOnly, decision = AdmissionDecision.Pending)
    assertEquals(
      StudioDocument.of(
        t1.datasets.map(d => if d.id == r3 then orphan else d),
        t1.analyses,
        t1.draft,
        t1.runs,
        t1.reporting,
        t1.figures,
        t1.presentation,
        t1.jobs
      ),
      Left(DocumentError.InventoryWithoutTrials(r3))
    )
  }

  // --- The wizard's draft of it --------------------------------------------

  private val trialsCsv =
    "participant,phase,trial,occurrence,item,display_kind,image_file,response\n" +
      "P01,Encoding,enc_01,1,beach-007,image,beach-007.png,\n"

  private val preview = ok(CsvSniffer.sniff("trials.csv", trialsCsv))

  test("a trials draft resolves to the mapping; a re-map restores a revision's own") {
    val restored = ok(TrialMetadataDraft.ofDataset(preview, r3, inventory))
    assertEquals(restored.resolve, Right(inventory))
    val proposed = TrialMetadataDraft.proposed(preview)
    assertEquals(
      proposed.resolve.map(_.bindings.map(b => b.role -> b.column.value)),
      Right(
        Vector(
          ColumnRole.Participant -> "participant",
          ColumnRole.Phase       -> "phase",
          ColumnRole.Trial       -> "trial",
          ColumnRole.Occurrence  -> "occurrence",
          ColumnRole.Item        -> "item",
          ColumnRole.Response    -> "response"
        )
      )
    )
    val renamed = ok(CsvSniffer.sniff("trials.csv", trialsCsv.replace("occurrence,", "block,")))
    assertEquals(
      TrialMetadataDraft.ofDataset(renamed, r3, inventory).left.map(_.toVector),
      Left(
        Vector(
          MappingError.ColumnAbsent(
            "trials.csv",
            MappingOrigin.Dataset(r3),
            column("occurrence"),
            ColumnRole.Occurrence
          )
        )
      )
    )
  }

  test("a trials draft without a phase does not resolve: the issue names the file and role") {
    val noPhase = ok(CsvSniffer.sniff("trials.csv", trialsCsv.replace(",phase,", ",stage_x,")))
    val draft   = TrialMetadataDraft.proposed(noPhase)
    assertEquals(
      draft.resolve.left.map(_.toVector),
      Left(
        Vector(
          MappingError.MissingRole(
            "trials.csv",
            ColumnRole.Phase,
            Vector(column("stage_x"), column("display_kind"), column("image_file"))
          )
        )
      )
    )
  }
