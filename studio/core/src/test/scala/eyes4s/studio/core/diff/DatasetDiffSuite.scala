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
import eyes4s.studio.core.selection.StudioRef
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
      DatasetDiff.fromParent(
        t2,
        r3,
        StatusDiff.Unavailable(
          r2,
          LedgerUnavailable.Refused(
            LedgerReadError.Refused(BackendError.Unavailable(DiagnosticLocus.Dataset(r2)))
          )
        )
      )
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
    // Every changed trial is a ref the count leads to.
    assertEquals(
      changes.transitions.flatMap(_.refs),
      Vector(StudioRef.Trial(key("c")), StudioRef.Trial(key("b")))
    )
    assertEquals(changes.changes.map(_.ref), changes.transitions.flatMap(_.refs))
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

  test("fixation attributes declared, dropped and re-kinded each read as one change") {
    def attrs(bs: (String, AttributeKindChoice)*) =
      right(DeclaredAttributes.of(bs.toVector.map((c, k) => AttributeBinding(column(c), k))))
    val from = spec3.copy(
      attributes =
        attrs("block" -> AttributeKindChoice.Text, "rt" -> AttributeKindChoice.Integer)
    )
    val to = spec3.copy(
      attributes = attrs("rt" -> AttributeKindChoice.Number, "cue" -> AttributeKindChoice.Text)
    )
    val diff = right(DatasetDiff.of(from, to, StatusDiff.NotRead))
    assertEquals(
      diff.changes,
      Vector(
        DatasetChange.Attribute(
          MappedSource.Fixations,
          column("block"),
          Some(AttributeKindChoice.Text),
          None
        ),
        DatasetChange.Attribute(
          MappedSource.Fixations,
          column("rt"),
          Some(AttributeKindChoice.Integer),
          Some(AttributeKindChoice.Number)
        ),
        DatasetChange.Attribute(
          MappedSource.Fixations,
          column("cue"),
          None,
          Some(AttributeKindChoice.Text)
        )
      )
    )
    assertEquals(
      DatasetDiffText.summary(diff),
      "block no longer declared; rt integer → number; cue declared text"
    )
    assertEquals(DatasetDiffText.staleCause(diff), "dataset r3 changed the mapping")
  }

  test("a source's import name and semantic identity are content") {
    val fixations                = spec3.sources.fixations.get
    def withFixations(f: Source) =
      spec3.copy(sources = right(Sources.of(spec3.sources.entries.map {
        case s if s.role == SourceRole.Fixations => f
        case s                                   => s
      })))
    val renamed =
      withFixations(fixations.copy(path = right(SourcePath.of("inputs/fixations-v2.csv"))))
    val identity = right(SemanticIdentity.of("0123456789abcdef"))
    val bound    = withFixations(fixations.copy(semantic = Some(identity)))
    assertEquals(
      DatasetDiffText.summary(right(DatasetDiff.of(spec3, renamed, StatusDiff.NotRead))),
      "fixations source renamed inputs/fixations.csv → inputs/fixations-v2.csv"
    )
    assertEquals(
      right(DatasetDiff.of(spec3, bound, StatusDiff.NotRead)).changes,
      Vector(DatasetChange.SourceIdentified(SourceRole.Fixations, None, Some(identity)))
    )
    assertEquals(
      DatasetDiffText.summary(right(DatasetDiff.of(bound, spec3, StatusDiff.NotRead))),
      "fixations source identity unbound"
    )
  }

  /** `a` with exactly one field of its content changed, and whether a
    * change belongs to that field.
    */
  private def perturb(
      a: DatasetRevisionSpec
  ): Gen[(String, DatasetRevisionSpec, DatasetChange => Boolean)] =
    import DatasetChange.*
    val fixations                = a.sources.fixations.get
    def withFixations(f: Source) =
      a.copy(sources = right(Sources.of(a.sources.entries.map {
        case s if s.role == SourceRole.Fixations => f
        case s                                   => s
      })))
    val g     = a.geometry
    val other = ByteDigest.sha256(IArray.unsafeFromArray(fixations.bytes.hex.getBytes("UTF-8")))
    val flipY = CorrectionRule(CorrectionTarget.AllTrials, CoordinateCorrection.FlipY)
    val identity = right(SemanticIdentity.of("0123456789abcdef"))
    val remaps   = a.mapping.bindings.map { b =>
      (
        s"remap ${b.role.label}",
        a.copy(mapping = right(ColumnMapping.of(a.mapping.bindings.map {
          case `b` => ColumnBinding(b.role, column(s"${b.column.value}_re"))
          case x   => x
        }))),
        (c: DatasetChange) =>
          c match
            case Mapped(MappedSource.Fixations, r, _, _) => r == b.role
            case _                                       => false
      )
    }
    val fixed = Vector[(String, DatasetRevisionSpec, DatasetChange => Boolean)](
      ("bytes", withFixations(fixations.copy(bytes = other)), _.isInstanceOf[SourceBytes]),
      (
        "rename",
        withFixations(fixations.copy(path = right(SourcePath.of(fixations.path.value + "-2")))),
        _.isInstanceOf[SourceRenamed]
      ),
      (
        "identity",
        withFixations(fixations.copy(semantic = if fixations.semantic.isDefined then None
        else Some(identity))),
        _.isInstanceOf[SourceIdentified]
      ),
      (
        "units",
        a.copy(units = DeclaredUnits(a.units.time match
          case Some(TimeUnit.Milliseconds) => Some(TimeUnit.Seconds)
          case _                           => Some(TimeUnit.Milliseconds))),
        _.isInstanceOf[Units]
      ),
      (
        "screen",
        a.copy(geometry =
          right(
            Geometry.of(
              right(ScreenSize.of(g.screen.width + 10, g.screen.height + 10)),
              g.image,
              g.pixelsPerDegree
            )
          )
        ),
        _.isInstanceOf[Screen]
      ),
      (
        "image",
        // Another valid placement: narrower, else wider, else moved, else
        // shorter or taller (a 1-pixel image filling its screen's width).
        a.copy(geometry =
          Vector(
            (g.image.left, g.image.top, g.image.width - 1, g.image.height),
            (g.image.left, g.image.top, g.image.width + 1, g.image.height),
            (g.image.left - 1, g.image.top, g.image.width, g.image.height),
            (g.image.left, g.image.top, g.image.width, g.image.height - 1),
            (g.image.left, g.image.top, g.image.width, g.image.height + 1),
            (g.image.left, g.image.top - 1, g.image.width, g.image.height)
          ).iterator
            .map((l, t, w, h) =>
              ImagePlacement
                .of(l, t, w, h)
                .toOption
                .flatMap(i => Geometry.of(g.screen, i, g.pixelsPerDegree).toOption)
            )
            .collectFirst { case Some(changed) => changed }
            .getOrElse(fail(s"no other placement of ${g.image} fits ${g.screen}"))
        ),
        _.isInstanceOf[Image]
      ),
      (
        "pixels per degree",
        a.copy(geometry =
          right(
            Geometry.of(
              g.screen,
              g.image,
              right(DeclaredPixelsPerDegree.of(g.pixelsPerDegree.value + 1))
            )
          )
        ),
        _.isInstanceOf[PixelsPerDegree]
      ),
      (
        "off-screen",
        a.copy(admission = a.admission.copy(offScreen = a.admission.offScreen match
          case OffScreenChoice.ExcludeRecord   => OffScreenChoice.QuarantineTrial
          case OffScreenChoice.QuarantineTrial => OffScreenChoice.ExcludeRecord)),
        _.isInstanceOf[OffScreen]
      ),
      (
        "corrections",
        a.copy(admission =
          a.admission.copy(corrections =
            if a.admission.corrections.contains(flipY) then
              a.admission.corrections.filterNot(_ == flipY)
            else a.admission.corrections :+ flipY
          )
        ),
        c => c.isInstanceOf[CorrectionAdded] || c.isInstanceOf[CorrectionRemoved]
      ),
      (
        "fixation attribute",
        a.copy(attributes =
          right(
            DeclaredAttributes.of(
              a.attributes.bindings :+ AttributeBinding(
                column("zz_attr"),
                AttributeKindChoice.Text
              )
            )
          )
        ),
        {
          case Attribute(MappedSource.Fixations, c, None, Some(_)) => c == column("zz_attr")
          case _                                                   => false
        }
      ),
      (
        "inventory",
        a.copy(inventory = if a.inventory.isDefined then None
        else Some(DocumentGen.identityInventory)),
        _.isInstanceOf[InventoryMapped]
      )
    )
    Gen.oneOf(fixed ++ remaps)

  property("a one-field change is exactly that field's change; no change is no change") {
    val cases = for
      a <- DocumentGen.dataset(1, Vector.empty)
      p <- perturb(a)
    yield (a, p)
    forAll(cases) { case (a, (field, b, belongs)) =>
      val diff = right(DatasetDiff.of(a, b, StatusDiff.NotRead))
      assert(diff.changes.nonEmpty, s"$field: no change")
      assert(diff.changes.forall(belongs), s"$field: ${diff.changes}")
      val same = a.copy(
        id = DatasetRevision(2),
        parent = Some(a.id),
        decision = AdmissionDecision.Pending
      )
      assertEquals(right(DatasetDiff.of(a, same, StatusDiff.NotRead)).changes, Vector.empty)
    }
  }
