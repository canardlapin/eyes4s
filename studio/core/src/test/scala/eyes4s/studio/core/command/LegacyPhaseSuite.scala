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

import eyes4s.studio.core.backend.DatasetRevision
import eyes4s.studio.core.document.*
import eyes4s.studio.core.fixture.StoryMoments
import io.circe.parser.decode
import io.circe.syntax.*

/** Projects saved before the phase was a required role (S5.2 admitted
  * mappings without one) still load (S5.3 review): a stored mapping and a
  * journaled `ImportSources` without a phase decode, and a loaded document
  * keeps them. Committing them is refused with the typed error naming the
  * phase: `ImportSources`, `ReviseDataset` of such a revision, and
  * `VerifyDataset`. Undoing a re-map that added the phase is allowed.
  */
class LegacyPhaseSuite extends munit.FunSuite:

  private def ok[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private val missingPhase = DocumentError.MissingColumnRoles(Vector(ColumnRole.Phase))

  private def refusedForPhase(r: Either[CommandError, ?])(using munit.Location): Unit =
    r match
      case Left(CommandError.Refused(_, _, error)) => assertEquals(error, missingPhase)
      case other => fail(s"expected a phase refusal, got $other")

  private val legacyMapping: ColumnMapping = ok(
    decode[ColumnMapping](LegacyMappings.mappingJson)
  )

  /** t1 as S5.2 saved it, its r3 pending. */
  private val loaded: StudioDocument =
    val doc = ok(LegacyMappings.legacy(DocumentSamples.t1))
    doc.dataset(DatasetRevision(3)).map(_.decision) match
      case Some(AdmissionDecision.Verifying(_)) =>
        ok(Reducer.step(doc, Command.WithdrawVerification(DatasetRevision(3))))._1
      case _ => doc

  private val r3 = loaded.dataset(DatasetRevision(3)).getOrElse(fail("no r3"))

  test("a stored S5.2 mapping without a phase decodes, and needs a re-map to commit") {
    assertEquals(legacyMapping.column(ColumnRole.Phase), None)
    assertEquals(legacyMapping.missingForImport, Vector(ColumnRole.Phase))
    assertEquals(ColumnMapping.admissible(legacyMapping), Left(missingPhase))
    assertEquals(decode[ColumnMapping](legacyMapping.asJson.noSpaces), Right(legacyMapping))
    // Structural checks still hold on decode: a stored role missing is refused.
    val noTrial =
      LegacyMappings.mappingJson.replace("""{"role":{"Trial":{}},"column":"trial"},""", "")
    assert(decode[ColumnMapping](noTrial).isLeft)
  }

  test("a document saved without phases loads with its mappings as stored") {
    assert(loaded.datasets.nonEmpty)
    loaded.datasets.foreach(d =>
      assertEquals(d.mapping.missingForImport, Vector(ColumnRole.Phase), d.id.label)
    )
    // It round-trips as it is.
    assertEquals(ok(StudioDocument.decode(ok(StudioDocument.encode(loaded)))), loaded)
  }

  test("a journaled ImportSources without a phase replays; VerifyDataset then refuses") {
    val command = Command.ImportSources(
      Some(r3.id),
      r3.sources,
      legacyMapping,
      r3.units,
      r3.geometry,
      r3.attributes
    )
    val line: JournalLine = JournalLine.Entry(4, JournalEntry.Apply(command))
    assertEquals(decode[JournalLine](line.asJson.noSpaces), Right(line))
    // A new commit of it is refused...
    refusedForPhase(Reducer.step(loaded, command))
    refusedForPhase(Reducer.step(DocumentSamples.t1, command))
    // ...but an S5.2 recovery journal holding it replays: replay rebuilds
    // history under the stored-role rule, so no unsaved work is lost.
    val journal = Vector(
      ok(CommandJournal.start(loaded)),
      ok(CommandJournal.entry(1, JournalEntry.Apply(command)))
    ).mkString("", "\n", "\n")
    val replay   = ok(CommandJournal.replay(loaded, journal))
    val restored = replay.history.document
    val r4       = restored.datasets.last
    assertEquals(r4.id, DatasetRevision(4))
    assertEquals(r4.mapping, legacyMapping)
    // The rebuilt history commits under the import rule again: the revision
    // is not sent for admission, and the undo it recorded still works.
    assertEquals(replay.history.rule, MappingRule.Commit)
    refusedForPhase(replay.history.apply(Command.VerifyDataset(r4.id)))
    assert(replay.history.undo.isRight)
  }

  test("Admit refuses a verifying revision without a phase (a backstop to VerifyDataset)") {
    // S5.2 could send one for admission; replay rebuilds that state.
    val (verifying, _) = ok(
      Reducer
        .run(loaded, Command.VerifyDataset(r3.id), MappingRule.Replay)
        .map(o => (o.document, o.effects))
    )
    val content = verifying.dataset(r3.id).map(_.decision) match
      case Some(AdmissionDecision.Verifying(c)) => c
      case other                                => fail(s"expected Verifying, got $other")
    refusedForPhase(
      Reducer.step(
        verifying,
        Command.Admit(r3.id, content, CoreBinding.unbound, CoreBinding.unbound)
      )
    )
  }

  test("SetMapping follows ReviseDataset's rule: an old revision only into an admissible one") {
    val noPhase =
      ok(ColumnMapping.of(r3.mapping.bindings.filterNot(_.column.value == "occurrence")))
    refusedForPhase(Reducer.step(loaded, Command.SetMapping(r3.id, noPhase)))
    val withPhase = ok(
      ColumnMapping.of(
        r3.mapping.bindings :+ ColumnBinding(ColumnRole.Phase, ok(ColumnName.of("phase")))
      )
    )
    val history = History.start(loaded)
    val step    = ok(history.apply(Command.SetMapping(r3.id, withPhase)))
    assertEquals(step.history.document.dataset(r3.id).map(_.mapping), Some(withPhase))
    // Undo restores the stored mapping over the admissible one.
    val undone = ok(step.history.undo)
    assertEquals(undone.history.document.dataset(r3.id), Some(r3))
  }

  test("a loaded revision without a phase is not sent for admission") {
    refusedForPhase(Reducer.step(loaded, Command.VerifyDataset(r3.id)))
  }

  test("revising it without a phase is refused; adding one is not, and undo restores it") {
    val otherUnits = DeclaredUnits(Some(TimeUnit.Microseconds))
    refusedForPhase(
      Reducer.step(
        loaded,
        Command.ReviseDataset(r3.id, r3.mapping, otherUnits, r3.geometry, r3.attributes)
      )
    )
    val withPhase = ok(
      ColumnMapping.of(
        r3.mapping.bindings :+ ColumnBinding(ColumnRole.Phase, ok(ColumnName.of("phase")))
      )
    )
    // An S5.2 project recorded no inventory mapping either; the re-map adds both.
    val inventory = ok(StoryMoments.inventory(occurrence = true))
    val revise    = Command.ReviseDataset(
      r3.id,
      withPhase,
      r3.units,
      r3.geometry,
      r3.attributes,
      Some(inventory)
    )
    val (remapped, _) = ok(Reducer.step(loaded, revise))
    assertEquals(remapped.dataset(r3.id).map(_.mapping), Some(withPhase))
    // The inverse the reducer records restores the stored mapping.
    val inverse = Command.ReviseDataset(r3.id, r3.mapping, r3.units, r3.geometry, r3.attributes)
    val (undone, _) = ok(Reducer.step(remapped, inverse))
    assertEquals(undone.dataset(r3.id), Some(r3))
  }
