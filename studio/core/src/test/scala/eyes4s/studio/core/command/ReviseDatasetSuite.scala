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

  /** r3's inventory without the occurrence, as its fixations now map none:
    * both files name the trial by the same key (S5.4 follow-up).
    */
  private def withoutOccurrence(inv: Option[InventoryMapping]): Option[InventoryMapping] =
    inv.map(i =>
      right(
        InventoryMapping.of(i.bindings.filterNot(_.role == ColumnRole.Occurrence), i.attributes)
      )
    )

  test("one ReviseDataset changes all four; one undo restores the prior spec exactly") {
    val step = ok(
      History
        .start(t1)
        .apply(
          ReviseDataset(r3, remapped, seconds, pending.geometry, attributes, pending.inventory)
        )
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
      ReviseDataset(
        r3,
        remapped,
        seconds,
        pending.geometry,
        attributes,
        pending.inventory
      ).kind,
      ChangeKind.DatasetReadmit
    )
  }

  test("a revision that changes nothing is refused, naming the command") {
    val same =
      ReviseDataset(
        r3,
        pending.mapping,
        pending.units,
        pending.geometry,
        pending.attributes,
        pending.inventory
      )
    assert(History.start(t1).apply(same).left.exists(_.message.contains("ReviseDataset")))
  }

  test("an admitted revision cannot be revised; it is re-imported") {
    val t2     = DocumentSamples.t2
    val r3spec = t2.dataset(r3).get
    assert(
      History
        .start(t2)
        .apply(
          ReviseDataset(
            r3,
            remapped,
            seconds,
            r3spec.geometry,
            attributes,
            r3spec.inventory
          )
        )
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
            None,
            r3spec.inventory
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
      .apply(
        ReviseDataset(
          r3,
          pending.mapping,
          pending.units,
          pending.geometry,
          clash,
          pending.inventory
        )
      )
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
      case Right(ImportSources(parent, _, _, _, _, attrs, admission, _)) =>
        assertEquals(parent, Some(r2))
        assertEquals(attrs, DeclaredAttributes.empty)
        // Nor, before S5.5, an admission choice: the parent's is inherited.
        assertEquals(admission, None)
      case other => fail(s"expected ImportSources, got $other")
  }

  test(
    "a revision without attributes keeps the version-1 wire form; with some, they round-trip"
  ) {
    assert(!pending.copy(inventory = None).asJson.noSpaces.contains("attributes"))
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
      admission,
      r3spec.inventory
    )
    val chosen = ok(History.start(t2).apply(reimport(Some(choice))))
    val r4     = chosen.history.document.datasets.last
    assertEquals((r4.parent, r4.admission), (Some(r3), choice))
    // One undo removes the whole re-admit.
    assertEquals(ok(chosen.history.undo).history.document, t2)
    val inherited = ok(History.start(t2).apply(reimport(None))).history.document.datasets.last
    assertEquals(inherited.admission, r3spec.admission)
  }

  test("the fixations and the inventory must name a trial by the same key (S5.4 follow-up)") {
    // As with the phase (S5.3), a pending revision may be re-mapped into one
    // whose keys disagree, so an undo always restores it; it is kept from
    // admission: verifying it is refused, naming the role and both files.
    val revised = History
      .start(t1)
      .apply(
        ReviseDataset(r3, remapped, seconds, pending.geometry, attributes, pending.inventory)
      )
      .fold(e => fail(e.message), _.history.document)
    assertEquals(
      History.start(revised).apply(VerifyDataset(r3)).left.map(_.message),
      Left(
        "VerifyDataset on dataset r3 is refused: Dataset r3: occurrence is mapped in " +
          "trials.csv but not in fixations.csv; both files must name the trial by the same key."
      )
    )
    // A new revision may be imported with keys that disagree (one file is
    // mapped before the other, as in the golden journey); it cannot be verified.
    val t2        = DocumentSamples.t2
    val r3spec    = t2.dataset(r3).get
    val importOne = ImportSources(
      Some(r3),
      r3spec.sources,
      r3spec.mapping,
      r3spec.units,
      r3spec.geometry,
      r3spec.attributes,
      None,
      withoutOccurrence(r3spec.inventory)
    )
    val imported = ok(History.start(t2).apply(importOne)).history.document
    val r4       = imported.datasets.last.id
    assertEquals(
      History.start(imported).apply(VerifyDataset(r4)).left.map(_.message),
      Left(
        "VerifyDataset on dataset r4 is refused: Dataset r4: occurrence is mapped in " +
          "fixations.csv but not in trials.csv; both files must name the trial by the same key."
      )
    )
  }

  test("Admit and ResumeVerification backstop the trial key and inventory checks (S5.4)") {
    // r3 re-mapped so its keys disagree, then sent for verification as a
    // replayed journal does (Replay does not check).
    val disagreeing = History
      .start(t1)
      .apply(
        ReviseDataset(r3, remapped, seconds, pending.geometry, attributes, pending.inventory)
      )
      .fold(e => fail(e.message), _.history.document)
    val verifying = Reducer
      .run(disagreeing, VerifyDataset(r3), MappingRule.Replay)
      .fold(e => fail(e.message), _.document)
    val content = verifying.dataset(r3).map(_.decision) match
      case Some(AdmissionDecision.Verifying(c)) => c
      case other                                => fail(s"expected Verifying, got $other")
    def admitOf(c: eyes4s.codec.CanonicalDigest[DatasetRevisionSpec]) = Admit(
      r3,
      c,
      Some(eyes4s.plan.AdmissionDecision.RequireComplete),
      CoreBinding.unbound,
      CoreBinding.unbound
    )
    val admit = admitOf(content)
    val keys  = "both files must name the trial by the same key"
    assert(
      Reducer.step(verifying, admit).left.exists(_.message.contains(keys)),
      Reducer.step(verifying, admit)
    )
    // A direct ResumeVerification is VerifyDataset's checks too.
    val digest = DatasetRevisionSpec
      .contentDigest(disagreeing.dataset(r3).get)
      .fold(e => fail(e.message), identity)
    assert(
      Reducer
        .step(disagreeing, ResumeVerification(r3, digest))
        .left
        .exists(_.message.contains(keys))
    )
    // An unmapped trials file is refused the same way.
    val unmapped = StudioDocument
      .of(
        t1.datasets.map(d => if d.id == r3 then d.copy(inventory = None) else d),
        t1.analyses,
        t1.draft,
        t1.runs,
        t1.reporting,
        t1.figures,
        t1.presentation,
        t1.jobs
      )
      .fold(e => fail(e.toString), identity)
    val unmappedVerifying = Reducer
      .run(unmapped, VerifyDataset(r3), MappingRule.Replay)
      .fold(e => fail(e.message), _.document)
    val unmappedContent = unmappedVerifying.dataset(r3).map(_.decision) match
      case Some(AdmissionDecision.Verifying(c)) => c
      case other                                => fail(s"expected Verifying, got $other")
    assert(
      Reducer
        .step(unmappedVerifying, admitOf(unmappedContent))
        .left
        .exists(_.message.contains("are not mapped"))
    )
    assert(
      Reducer
        .step(unmapped, ResumeVerification(r3, unmappedContent))
        .left
        .exists(_.message.contains("are not mapped"))
    )
    // The documented limit of WithdrawVerification: a document stored
    // Verifying with disagreeing keys withdraws, but its undo (ResumeVerification)
    // is refused, typed; the revision is never admitted.
    val withdrawn = History
      .start(verifying)
      .apply(WithdrawVerification(r3))
      .fold(e => fail(e.message), _.history)
    assert(
      withdrawn.undo.left.exists(_.message.contains(keys)),
      withdrawn.undo.map(_.history.document.dataset(r3).map(_.decision))
    )
    // Agreeing keys verify, resume and admit as before.
    val t1content =
      DatasetRevisionSpec.contentDigest(pending).fold(e => fail(e.message), identity)
    assert(Reducer.step(t1, ResumeVerification(r3, t1content)).isRight)
  }
