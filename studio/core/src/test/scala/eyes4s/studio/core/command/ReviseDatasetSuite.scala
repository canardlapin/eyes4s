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

import eyes4s.plan.{AttributeColumn, AttributeKind}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.document.DocumentGen.right
import eyes4s.studio.core.fixture.StoryMoments
import io.circe.parser.decode
import io.circe.syntax.*

/** The import wizard's document commands (ticket S5.2): a re-admit revises
  * mapping, units, geometry and attributes in one undoable step, and the
  * attributes a revision declares travel with it.
  */
class ReviseDatasetSuite extends munit.FunSuite:
  import Command.*
  import StoryMoments.{r2, r3}

  private val t1      = DocumentSamples.t1
  private val pending = t1.dataset(r3).get

  private def ok(e: Either[CommandError, Step]): Step =
    e.fold(err => fail(err.message), identity)

  private def column(s: String) = right(ColumnName.of(s))

  private val pupil = CommandSamples.pupil

  /** r3 with occurrence as an attribute, seconds, and Pupil declared. */
  private val remapped = right(
    ColumnMapping.of(pending.mapping.bindings.filterNot(_.role == ColumnRole.Occurrence))
  )
  private val attributes = right(
    DeclaredAttributes.of(
      Vector(
        AttributeBinding(column("occurrence"), AttributeKindChoice.Text),
        AttributeBinding(column("Pupil"), AttributeKindChoice.Number)
      )
    )
  )
  private val seconds = DeclaredUnits(Some(TimeUnit.Seconds))

  test("one ReviseDataset changes all four; one undo restores the prior spec exactly") {
    val step = ok(
      History
        .start(t1)
        .apply(ReviseDataset(r3, remapped, seconds, pending.geometry, attributes))
    )
    val revised = step.history.document.dataset(r3).get
    assertEquals(revised.mapping, remapped)
    assertEquals(revised.units, seconds)
    assertEquals(revised.attributes, attributes)
    assertEquals(step.effects, Vector(Effect.Persist))
    val undone = step.history.undo.fold(e => fail(e.message), identity)
    assertEquals(undone.history.document.dataset(r3), Some(pending))
    assertEquals(undone.history.document, t1)
    val redone = undone.history.redo.fold(e => fail(e.message), identity)
    assertEquals(redone.history.document.dataset(r3), Some(revised))
    assertEquals(
      ReviseDataset(r3, remapped, seconds, pending.geometry, attributes).kind,
      ChangeKind.DatasetReadmit
    )
  }

  test("a revision that changes nothing is refused, naming the command") {
    val same =
      ReviseDataset(r3, pending.mapping, pending.units, pending.geometry, pending.attributes)
    assert(History.start(t1).apply(same).left.exists(_.message.contains("ReviseDataset")))
  }

  test("an admitted revision cannot be revised; it is re-imported") {
    val t2     = DocumentSamples.t2
    val r3spec = t2.dataset(r3).get
    assert(
      History
        .start(t2)
        .apply(ReviseDataset(r3, remapped, seconds, r3spec.geometry, attributes))
        .isLeft
    )
    val reimport = ok(
      History
        .start(t2)
        .apply(
          ImportSources(
            Some(r3),
            r3spec.sources,
            remapped,
            seconds,
            r3spec.geometry,
            attributes,
            None
          )
        )
    )
    val r4 = reimport.history.document.datasets.last
    assertEquals(
      r4.attributes.core,
      Vector(
        AttributeColumn("occurrence", AttributeKind.Text),
        AttributeColumn("Pupil", AttributeKind.Number)
      )
    )
  }

  test("a column cannot be both an attribute and a role's column") {
    val clash = right(
      DeclaredAttributes.of(Vector(AttributeBinding(column("x"), AttributeKindChoice.Number)))
    )
    val refused = History
      .start(t1)
      .apply(ReviseDataset(r3, pending.mapping, pending.units, pending.geometry, clash))
    assert(
      refused.left.exists(_.message.contains("column x is both an attribute and the x column")),
      refused.toString
    )
    assertEquals(
      DeclaredAttributes.of(
        Vector(
          AttributeBinding(column("a"), AttributeKindChoice.Text),
          AttributeBinding(column("a"), AttributeKindChoice.Number)
        )
      ),
      Left(DocumentError.RepeatedAttribute("a"))
    )
  }

  test("an ImportSources journal line written before S5.2 still reads, with no attributes") {
    val legacy = decode[Command](CommandPins.importSourcesV1)
    legacy match
      case Right(ImportSources(parent, _, _, _, _, attrs, admission)) =>
        assertEquals(parent, Some(r2))
        assertEquals(attrs, DeclaredAttributes.empty)
        // Nor, before S5.5, an admission choice: the parent's is inherited.
        assertEquals(admission, None)
      case other => fail(s"expected ImportSources, got $other")
  }

  test(
    "a revision without attributes keeps the version-1 wire form; with some, they round-trip"
  ) {
    assert(!pending.asJson.noSpaces.contains("attributes"))
    val withAttrs = pending.copy(attributes = pupil)
    assertEquals(decode[DatasetRevisionSpec](withAttrs.asJson.noSpaces), Right(withAttrs))
    // The attributes are part of what admission verifies.
    assertNotEquals(
      DatasetRevisionSpec.contentDigest(withAttrs),
      DatasetRevisionSpec.contentDigest(pending)
    )
  }

  test("a re-import with an admission choice takes it; without one it inherits the parent's") {
    val t2     = DocumentSamples.t2
    val r3spec = t2.dataset(r3).get
    val rule   = CorrectionRule(CorrectionTarget.AllTrials, CoordinateCorrection.FlipX)
    val choice = AdmissionChoice(OffScreenChoice.QuarantineTrial, Vector(rule))
    def reimport(admission: Option[AdmissionChoice]) = ImportSources(
      Some(r3),
      r3spec.sources,
      r3spec.mapping,
      r3spec.units,
      r3spec.geometry,
      r3spec.attributes,
      admission
    )
    val chosen = ok(History.start(t2).apply(reimport(Some(choice))))
    val r4     = chosen.history.document.datasets.last
    assertEquals((r4.parent, r4.admission), (Some(r3), choice))
    // One undo removes the whole re-admit.
    assertEquals(ok(chosen.history.undo).history.document, t2)
    val inherited = ok(History.start(t2).apply(reimport(None))).history.document.datasets.last
    assertEquals(inherited.admission, r3spec.admission)
  }
