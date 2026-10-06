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

package eyes4s.io

import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

class FixationPositionsSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val frame                             = get(Frame.screen("position-screen", 10, 8))
  private val trialColumns = get(TrialColumns.of("p", "phase", "trial", Some("occ")))
  private val columns = get(FixationColumns.of("ordinal", "x", "y", "onset", "duration", "n"))
  private val keys    = get(FixationKeyReader.trial("p", "phase", "trial", "item", Some("occ")))
  private val header  =
    Vector("p", "phase", "trial", "occ", "item", "ordinal", "x", "y", "onset", "duration", "n")
  private val rows = Vector(
    Vector("P1", "R", "t1", "1", "a", "1", "3", "2", "0", "1", "1"),
    Vector("P1", "R", "t1", "1", "a", "2", "9", "2", "2", "1", "1"),
    Vector("P1", "R", "t1", "1", "a", "3", "-1", "2", "4", "1", "1")
  )
  private def csv(records: Vector[Vector[String]] = rows) = Rfc4180.encode(header +: records)
  private val window                                      = get(
    Subframe.of(frame, FrameId("position-window"), get(Bounds.of[Px](2, 1, 8, 7)))
  )

  test("position preview uses the importer's corrections and preserves raw source records") {
    val choices = Vector(Correction.FlipX, Correction.FlipY, get(Correction.translate(-2, 1)))
    choices.foreach { correction =>
      val previewPolicy = AdmissionPolicy(
        OffScreenPolicy.ExcludeRecord,
        Vector(AppliedCorrection(CorrectionScope.AllTrials[TrialIdentity](), correction))
      )
      val importPolicy = AdmissionPolicy(
        OffScreenPolicy.ExcludeRecord,
        Vector(AppliedCorrection(CorrectionScope.AllTrials[TrialKey](), correction))
      )
      val preview =
        get(FixationPositions.read(csv(), trialColumns, "x", "y", frame, previewPolicy))
      val imported = get(
        FixationCsv.admit(csv(), columns, keys, frame, TimestampUnit.Microseconds, importPolicy)
      )
      val path = imported.accepted.rows.head.value
      assertEquals(
        preview.positions.map(_.corrected),
        path.fixations.iterator.map(_.centre).toVector
      )
      val expected = correction match
        case Correction.FlipX => Vector(Pt[Px](7, 2), Pt[Px](1, 2), Pt[Px](11, 2))
        case Correction.FlipY => Vector(Pt[Px](3, 6), Pt[Px](9, 6), Pt[Px](-1, 6))
        case _                => Vector(Pt[Px](1, 3), Pt[Px](7, 3), Pt[Px](-3, 3))
      assertEquals(preview.positions.map(_.corrected), expected)
      assertEquals(
        preview.positions.map(_.raw),
        rows.map(r => Pt[Px](r(6).toDouble, r(7).toDouble))
      )
      assertEquals(preview.positions.map(_.fields), rows)
      assertEquals(preview.positions.map(_.rule), Vector.fill(3)(Some(0)))
      assertEquals(preview.records, rows.size)
    }
  }

  test(
    "analytical translation, half-open window boundaries and angular projection are explicit"
  ) {
    val preview = get(FixationPositions.read(csv(), trialColumns, "x", "y", frame))
    val placed  = get(
      preview.place(
        window,
        Some(get(LinearAngularScale.of(frame, 2))),
        FrameId("position-degrees")
      )
    )
    assertEquals(
      placed.positions.map(_.image),
      Vector(Pt[Px](1, 1), Pt[Px](7, 1), Pt[Px](-3, 1))
    )
    assertEquals(
      placed.positions.map(_.degrees),
      Vector(
        Some(Pt[Unit2D.Deg](-1, 1)),
        Some(Pt[Unit2D.Deg](2, 1)),
        Some(Pt[Unit2D.Deg](-3, 1))
      )
    )
    assertEquals(placed.positions.map(_.outsideWindow), Vector(false, true, false))
    assertEquals(placed.positions.map(_.outsideFrame), Vector(false, false, true))
    assertEquals(
      placed.tallies.map(t => (t.records, t.outsideWindow, t.outsideFrame)),
      Vector((3, 1, 1))
    )
    val counts = get(get(placed.measure).binned(get(Grid.over(frame, 10, 8))))
    assertEquals(counts.sum, 2.0)
    val boundary = get(
      FixationPositions.read(
        csv(Vector(rows.head.updated(6, "8"))),
        trialColumns,
        "x",
        "y",
        frame
      )
    )
    assert(get(boundary.place(window, None, FrameId("unused"))).positions.head.outsideWindow)
  }

  test("malformed positions, widths and identities return every record as placed or unplaced") {
    val bad =
      Vector(rows.head.updated(6, "NaN"), rows.head.dropRight(1), rows.head.updated(0, ""))
    val preview =
      get(FixationPositions.read(csv(rows.take(1) ++ bad), trialColumns, "x", "y", frame))
    assertEquals(preview.records, 4)
    assertEquals(preview.positions.map(_.record.value), Vector(1))
    assertEquals(preview.unplaced.map(_.record.value), Vector(2, 3, 4))
    assertEquals(preview.unplaced.map(_.fields), bad)
    assertEquals(preview.unplaced.map(_.trial.isDefined), Vector(true, true, false))
  }

  test("off-screen policy is retained without hiding positions of quarantined trials") {
    val exclude    = get(FixationPositions.read(csv(), trialColumns, "x", "y", frame))
    val quarantine = get(
      FixationPositions.read(
        csv(),
        trialColumns,
        "x",
        "y",
        frame,
        AdmissionPolicy.version1[TrialIdentity]
      )
    )
    assertEquals(exclude.positions.map(_.corrected), quarantine.positions.map(_.corrected))
    assertEquals(exclude.positions.map(_.admissionFailure), Vector.fill(3)(None))
    assertEquals(
      quarantine.positions.map(_.admissionFailure.isDefined),
      Vector(false, false, true)
    )
    val imported = get(
      FixationCsv.admit(
        csv(),
        columns,
        keys,
        frame,
        TimestampUnit.Microseconds,
        AdmissionPolicy.version1[TrialKey]
      )
    )
    assert(imported.accepted.rows.isEmpty)
    assertEquals(imported.rejected.size, 3)
  }

  test("overlapping correction scopes remain unplaced and quarantine the same native trial") {
    val conflicting   = Vector(Correction.FlipX, Correction.FlipY)
    val previewPolicy = AdmissionPolicy(
      OffScreenPolicy.ExcludeRecord,
      conflicting.map(c => AppliedCorrection(CorrectionScope.AllTrials[TrialIdentity](), c))
    )
    val importPolicy = AdmissionPolicy(
      OffScreenPolicy.ExcludeRecord,
      conflicting.map(c => AppliedCorrection(CorrectionScope.AllTrials[TrialKey](), c))
    )
    val preview =
      get(FixationPositions.read(csv(), trialColumns, "x", "y", frame, previewPolicy))
    assertEquals(preview.positions.size, 0)
    assertEquals(preview.unplaced.size, rows.size)
    val placed = get(preview.place(window, None, FrameId("unused")))
    assertEquals(placed.tallies.map(_.records), Vector(0))
    assertEquals(placed.records, rows.size)
    preview.unplaced.foreach { row =>
      assertEquals(
        row.error,
        FixationRowError.Trial(
          Vector(row.record.value + 1),
          QuarantineCause.CorrectionConflict(0, 1)
        )
      )
    }
    val imported = get(
      FixationCsv.admit(csv(), columns, keys, frame, TimestampUnit.Microseconds, importPolicy)
    )
    assert(imported.accepted.rows.isEmpty)
    assert(imported.rejected.forall(_.error match
      case FixationRowError.Trial(_, QuarantineCause.CorrectionConflict(0, 1)) => true
      case _                                                                   => false))
  }

  test("unplaceable-only trials retain source order and zero positioned tallies") {
    val bad = Vector(rows.head.updated(0, "P2").updated(2, "bad").updated(6, "NaN"), rows.head)
    val preview = get(FixationPositions.read(csv(bad), trialColumns, "x", "y", frame))
    val placed  = get(preview.place(window, None, FrameId("unused")))
    assertEquals(placed.records, 2)
    assertEquals(
      placed.tallies.map(t => (t.trial.participant, t.records)),
      Vector(("P2", 0), ("P1", 1))
    )
    assertEquals(placed.unplaced.map(_.record.value), Vector(1))
    val allInvalid =
      get(FixationPositions.read(csv(bad.take(1)), trialColumns, "x", "y", frame))
    val allPlaced = get(allInvalid.place(window, None, FrameId("unused")))
    assertEquals(
      allPlaced.tallies.map(t => (t.records, t.outsideWindow, t.outsideFrame)),
      Vector((0, 0, 0))
    )
    assertEquals(
      get(get(allPlaced.measure).binned(get(Grid.over(frame, 2, 2)))).toVector,
      Vector.fill(4)(0.0)
    )
  }

  test("geometry identity and empty position previews remain explicit") {
    val empty  = get(FixationPositions.read(csv(Vector.empty), trialColumns, "x", "y", frame))
    val placed = get(empty.place(window, None, FrameId("unused")))
    assertEquals(placed.records, 0)
    assertEquals(
      get(get(placed.measure).binned(get(Grid.over(frame, 2, 2)))).toVector,
      Vector.fill(4)(0.0)
    )
    val another     = get(Frame.screen("other-position-screen", 10, 8))
    val otherWindow = get(
      Subframe.of(another, FrameId("other-position-window"), get(Bounds.of[Px](2, 1, 8, 7)))
    )
    assert(empty.place(otherWindow, None, FrameId("unused")).isLeft)
  }
