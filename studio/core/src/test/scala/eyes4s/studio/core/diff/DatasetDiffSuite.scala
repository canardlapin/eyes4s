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

package eyes4s.studio.core.diff

import eyes4s.codec.ByteDigest
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.*
import eyes4s.studio.core.document.DocumentSamples.t2
import eyes4s.studio.core.fixture.StoryMoments.{r2, r3}
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

/** The dataset revision diff (ticket S5.8). */
class DatasetDiffSuite extends munit.ScalaCheckSuite:

  private def right[E, A](e: Either[E, A]): A =
    e.fold(err => throw new AssertionError(err.toString), identity)

  private val spec2 = t2.dataset(r2).get
  private val spec3 = t2.dataset(r3).get

  private def column(name: String) = right(ColumnName.of(name))

  /** `spec` with its fixation occurrence column renamed `name` and no
    * inventory: the board's "Block" column.
    */
  private def withOccurrenceColumn(spec: DatasetRevisionSpec, name: String) =
    spec.copy(
      mapping = right(
        ColumnMapping.of(spec.mapping.bindings.map {
          case ColumnBinding(ColumnRole.Occurrence, _) =>
            ColumnBinding(ColumnRole.Occurrence, column(name))
          case b => b
        })
      ),
      inventory = None
    )

  private def key(trial: String) = TrialKey("P01", Phase.Retrieval, trial, 1)
  private def entry(trial: String, d: TrialDisposition) =
    LedgerEntry(key(trial), "item", None, d, Vector.empty)
  private def overlap(i: Int) =
    TrialDisposition.Quarantined(QuarantineCause.Overlap(i, "1200 ms", "1180 ms"))

  /** r2's and r3's ledgers as FIXTURE.md describes them: 3 trials go from
    * overlap to admitted, 1 from admitted to no-fixations.
    */
  private val ledger2 = Vector(
    entry("ret_01", overlap(2)),
    entry("ret_02", TrialDisposition.Admitted),
    entry("ret_03", overlap(4)),
    entry("ret_04", TrialDisposition.Admitted),
    entry("ret_05", overlap(1))
  )
  private val ledger3 = Vector(
    entry("ret_01", TrialDisposition.Admitted),
    entry("ret_02", TrialDisposition.Admitted),
    entry("ret_03", TrialDisposition.Admitted),
    entry("ret_04", TrialDisposition.NoFixations),
    entry("ret_05", TrialDisposition.Admitted)
  )
  private val compared =
    StatusDiff.Compared(right(StatusChanges.between(r2, ledger2, r3, ledger3)))

  // --- Acceptance -----------------------------------------------------------

  test("r2 → r3 reads 'onset declared ms; Block → occurrence; 4 trials change status'") {
    val diff = right(
      DatasetDiff.of(
        withOccurrenceColumn(spec2, "Block"),
        withOccurrenceColumn(spec3, "Block"),
        compared
      )
    )
    assertEquals(
      DatasetDiffText.summary(diff),
      "onset declared ms; Block → occurrence; 4 trials change status"
    )
    // It feeds Figure 2's stale text (Figures.dc.html).
    assertEquals(
      DatasetDiffText.staleCause(diff),
      "dataset r3 changed the admission status of 4 trials"
    )
    assertEquals(
      diff.status match
        case StatusDiff.Compared(c) => DatasetDiffText.transitions(c)
        case other                  => other.toString
      ,
      "overlap → admitted 3, admitted → no-fixations 1"
    )
  }

  test("the fixture's r2 → r3 document changes: units, then the occurrence mapping") {
    val diff       = right(DatasetDiff.fromParent(t2, r3, StatusDiff.NotRead))
    val occurrence = column("occurrence")
    assertEquals(
      diff.changes,
      Vector(
        DatasetChange.Units(DeclaredUnits(None), DeclaredUnits(Some(TimeUnit.Milliseconds))),
        DatasetChange
          .Mapped(MappedSource.Fixations, ColumnRole.Occurrence, None, Some(occurrence)),
        DatasetChange
          .Mapped(MappedSource.Inventory, ColumnRole.Occurrence, None, Some(occurrence)),
        DatasetChange.Attribute(
          MappedSource.Inventory,
          occurrence,
          Some(AttributeKindChoice.Text),
          None
        )
      )
    )
    // The golden column is named "occurrence" (FIXTURE.md's "Block"); the
    // inventory's repeat and the attribute that became the role are said once.
    assertEquals(DatasetDiffText.summary(diff), "onset declared ms; occurrence → occurrence")
    assertEquals(
      DatasetDiffText.staleCause(diff),
      "dataset r3 changed the units and the mapping"
    )
  }

  test("a ledger that cannot be read is said, not hidden") {
    val diff = right(
      DatasetDiff.fromParent(t2, r3, StatusDiff.Unavailable(r2, "data r2 is not served"))
    )
    assertEquals(
      DatasetDiffText.summary(diff),
      "onset declared ms; occurrence → occurrence; trial status vs r2 unavailable"
    )
    assertEquals(diff.statusChanges, None)
  }

  // --- Status changes ---------------------------------------------------------

  test("statuses are compared without their operands; unlisted trials are changes") {
    val before  = Vector(entry("a", overlap(1)), entry("b", TrialDisposition.Admitted))
    val after   = Vector(entry("a", overlap(7)), entry("c", TrialDisposition.Absent))
    val changes = right(StatusChanges.between(r2, before, r3, after))
    assertEquals(
      changes.changes,
      Vector(
        StatusChange(key("c"), TrialStatus.Unlisted, TrialStatus.Absent),
        StatusChange(key("b"), TrialStatus.Admitted, TrialStatus.Unlisted)
      )
    )
    assertEquals(
      DatasetDiffText.transitions(changes),
      "not listed → absent 1, admitted → not listed 1"
    )
    assertEquals(DatasetDiffText.statusCount(1), "1 trial changes status")
  }

  test("a ledger listing a trial twice is refused, naming it") {
    val twice =
      Vector(entry("a", TrialDisposition.Admitted), entry("a", TrialDisposition.Absent))
    assertEquals(
      StatusChanges.between(r2, twice, r3, ledger3),
      Left(DiffError.RepeatedTrials(r2, Vector(key("a"))))
    )
  }

  test("a status comparison of other revisions is refused") {
    val other = StatusDiff.Compared(right(StatusChanges.between(r3, ledger2, r2, ledger3)))
    assertEquals(
      DatasetDiff.of(spec2, spec3, other),
      Left(DiffError.StatusOfOther((r2, r3), (r3, r2)))
    )
    val diff = right(DatasetDiff.of(spec2, spec3, StatusDiff.NotRead))
    assertEquals(diff.withStatus(other), Left(DiffError.StatusOfOther((r2, r3), (r3, r2))))
    assertEquals(diff.withStatus(compared).map(_.statusChanges), Right(Some(4)))
  }

  test("fromParent needs a known revision with a parent") {
    assertEquals(
      DatasetDiff.fromParent(t2, r2, StatusDiff.NotRead),
      Left(DiffError.NoParent(r2))
    )
    assertEquals(
      DatasetDiff.fromParent(t2, DatasetRevision(9), StatusDiff.NotRead),
      Left(DiffError.UnknownDataset(DatasetRevision(9), Vector(r2, r3)))
    )
  }

  // --- Every kind of change -----------------------------------------------------

  test("geometry, admission choices and sources each read as one phrase") {
    val screen = right(ScreenSize.of(2560, 1440))
    val image  = right(ImagePlacement.of(0, 0, 1280, 960))
    val ppd    = right(DeclaredPixelsPerDegree.of(40.0))
    val flip   = CorrectionRule(CorrectionTarget.AllTrials, CoordinateCorrection.FlipX)
    val shift  = CorrectionRule(
      CorrectionTarget.Participant(right(ParticipantId.of("P07"))),
      CoordinateCorrection.Translate(right(Offset.of(2.0, -3.5)))
    )
    val base = spec3.copy(admission =
      AdmissionChoice(OffScreenChoice.ExcludeRecord, Vector(flip, shift))
    )
    val sources = right(
      Sources.of(base.sources.entries.map {
        case s if s.role == SourceRole.Trials =>
          s.copy(bytes = ByteDigest.sha256(IArray.fill[Byte](1)(7)))
        case s => s
      })
    )
    val moved = base.copy(
      sources = sources,
      geometry = right(Geometry.of(screen, image, ppd)),
      admission = AdmissionChoice(OffScreenChoice.QuarantineTrial, Vector(shift))
    )
    val diff = right(DatasetDiff.of(base, moved, StatusDiff.NotRead))
    assertEquals(
      DatasetDiffText.summary(diff),
      "trial inventory source replaced; screen 1920×1080 px → 2560×1440 px; " +
        "image 1024×768 px at (448, 156) → 1280×960 px at (0, 0); 35 → 40 px/°; " +
        "off-screen exclude record → quarantine trial; correction removed: flip horizontally for all trials"
    )
    assertEquals(
      DatasetDiffText.staleCause(diff),
      "dataset r3 changed the sources, the geometry, the off-screen policy and the corrections"
    )
    val reordered = right(
      DatasetDiff.of(
        base,
        base.copy(admission = base.admission.copy(corrections = Vector(shift, flip))),
        StatusDiff.NotRead
      )
    )
    assertEquals(DatasetDiffText.summary(reordered), "corrections reordered")
    val added =
      right(DatasetDiff.of(moved, moved.copy(admission = base.admission), StatusDiff.NotRead))
    assert(
      DatasetDiffText
        .summary(added)
        .contains("correction added: flip horizontally for all trials"),
      DatasetDiffText.summary(added)
    )
    assertEquals(DatasetDiffText.rule(shift), "shift (2, -3.5) px for P07")
  }

  test("identical content is no change, and its stale cause is the admission alone") {
    val diff =
      right(DatasetDiff.of(spec3, spec3.copy(id = DatasetRevision(4)), StatusDiff.NotRead))
    assertEquals(diff.changes, Vector.empty)
    assertEquals(DatasetDiffText.summary(diff), "")
    assertEquals(DatasetDiffText.staleCause(diff), "dataset r4 is admitted")
  }

  property("a diff is empty exactly when the revisions' content is equal") {
    val pair = for
      a    <- DocumentGen.dataset(1, Vector.empty)
      b    <- DocumentGen.dataset(2, Vector(1))
      same <- Gen.oneOf(true, false)
    yield (a, if same then a.copy(id = b.id, parent = b.parent, decision = b.decision) else b)
    forAll(pair) { (a, b) =>
      def content(d: DatasetRevisionSpec) =
        (d.sources, d.mapping, d.units, d.geometry, d.admission, d.attributes, d.inventory)
      val diff = right(DatasetDiff.of(a, b, StatusDiff.NotRead))
      assertEquals(diff.changes.isEmpty, content(a) == content(b))
      assertEquals(right(DatasetDiff.of(a, a, StatusDiff.NotRead)).changes, Vector.empty)
    }
  }
