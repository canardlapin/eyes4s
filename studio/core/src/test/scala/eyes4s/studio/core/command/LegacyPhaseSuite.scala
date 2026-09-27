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

  test("a journaled ImportSources without a phase decodes; applying it is refused") {
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
    refusedForPhase(Reducer.step(loaded, command))
    refusedForPhase(Reducer.step(DocumentSamples.t1, command))
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
    val revise = Command.ReviseDataset(r3.id, withPhase, r3.units, r3.geometry, r3.attributes)
    val (remapped, _) = ok(Reducer.step(loaded, revise))
    assertEquals(remapped.dataset(r3.id).map(_.mapping), Some(withPhase))
    // The inverse the reducer records restores the stored mapping.
    val inverse = Command.ReviseDataset(r3.id, r3.mapping, r3.units, r3.geometry, r3.attributes)
    val (undone, _) = ok(Reducer.step(remapped, inverse))
    assertEquals(undone.dataset(r3.id), Some(r3))
  }
