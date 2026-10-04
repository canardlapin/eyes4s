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

package eyes4s.studio.core.geometry

import eyes4s.codec.ByteDigest
import eyes4s.kernel.{Pt, Unit2D}
import eyes4s.studio.core.backend.{Phase, TrialKey}
import eyes4s.studio.core.command.{Command, History}
import eyes4s.studio.core.document.*
import eyes4s.studio.core.document.DocumentGen.right
import eyes4s.studio.core.fixture.StoryMoments

import java.nio.charset.StandardCharsets.UTF_8

/** The geometry panel's correction ledger (ticket S5.5): corrections are
  * recorded as the revision's admission rules and applied by eyes4s to a
  * derived view; the source positions, the source bytes and their digest
  * never change. The worked example follows eyes4s's frames from screen
  * pixels to the image frame and to degrees from its centre, x right and
  * y up (the golden record 7,214: screen (1148, 456) → image (700, 300) →
  * (+5.4°, +2.4°)).
  */
class CorrectionLedgerSuite extends munit.FunSuite:
  import StoryMoments.r3

  private val t1      = eyes4s.studio.core.document.DocumentSamples.t1
  private val pending = t1.dataset(r3).get

  private def get[A](e: Either[GeometryProblem, A]): A = e.fold(p => fail(p.message), identity)

  /** Five records in the golden file's columns: two of P05 ret_04 (one on
    * the screen outside the image, one off the screen), the golden focus
    * record, one of another P05 trial and one with an unreadable x.
    */
  private val csv: String =
    """participant,phase,trial,occurrence,ordinal,x,y,onset_ms,duration_ms,sample_count
      |P05,Retrieval,ret_04,1,1,260,120,0,200,100
      |P05,Retrieval,ret_04,1,2,-40.5,500,210,200,100
      |P17,Encoding,enc_03,1,6,1148,456,2160,412,206
      |P05,Encoding,enc_01,1,1,700,420,0,100,50
      |P09,Encoding,enc_01,1,1,n/a,420,0,100,50
      |""".stripMargin
  private val bytes: IArray[Byte] = IArray.from(csv.getBytes(UTF_8))

  /** r3 of the story, reading the five records above. */
  private val spec: DatasetRevisionSpec =
    val src = Source(
      SourceRole.Fixations,
      right(SourcePath.of("inputs/fixations.csv")),
      ByteDigest.sha256(bytes),
      None
    )
    pending.copy(sources = right(Sources.of(Vector(src))))

  private val p05ret04 = TrialKey("P05", Phase.Retrieval, "ret_04", 1)
  private val p05enc01 = TrialKey("P05", Phase.Encoding, "enc_01", 1)
  private val p17enc03 = TrialKey("P17", Phase.Encoding, "enc_03", 1)

  private def rule(target: CorrectionTarget, c: CoordinateCorrection) =
    CorrectionRule(target, c)
  private val flipTrial = rule(CorrectionTarget.Trial(p05ret04), CoordinateCorrection.FlipX)

  private def withRules(rules: CorrectionRule*): DatasetRevisionSpec =
    spec.copy(admission = spec.admission.copy(corrections = rules.toVector))

  private lazy val positions = get(SourcePositions.read(spec, bytes))

  test("source positions are read as recorded, by record, with unreadable ones counted") {
    assertEquals(positions.records, 5)
    assertEquals(positions.positions.map(_.record), Vector(1, 2, 3, 4))
    assertEquals(positions.position(3), Some(SourcePosition(3, p17enc03, 1148.0, 456.0)))
    assertEquals(positions.unplaced.map(_.record), Vector(5))
    assert(positions.unplaced.head.reason.contains("x 'n/a'"), positions.unplaced)
    assertEquals(positions.trials.take(2), Vector(p05ret04, p17enc03))
  }

  test("bytes that are not the revision's fixation source are refused, naming it") {
    val other = IArray.from((csv + "P01,Encoding,enc_01,1,1,1,1,0,2,1\n").getBytes(UTF_8))
    SourcePositions.read(spec, other) match
      case Left(GeometryProblem.NotTheSource(d, path, expected, _)) =>
        assertEquals(
          (d, path, expected),
          (r3, "inputs/fixations.csv", spec.sources.fixations.get.bytes.hex)
        )
      case other => fail(s"expected NotTheSource, got $other")
  }

  test("the worked example: screen (1148, 456) → image (700, 300) → (+5.4°, +2.4°)") {
    val ledger  = get(CorrectionLedger.of(spec))
    val example = get(WorkedExample.of(ledger, positions.position(3).get))
    assertEquals(example.placed.rule, None)
    assertEquals(example.placed.corrected, Pt[Unit2D.Px](1148.0, 456.0))
    assertEquals(example.image, Pt[Unit2D.Px](700.0, 300.0))
    val deg = example.degrees.get
    // Declared 35 px/°: (700 − 512) / 35 right, (384 − 300) / 35 up.
    assertEqualsDouble(deg.x, 188.0 / 35.0, 1e-9)
    assertEqualsDouble(deg.y, 84.0 / 35.0, 1e-9)
    assert(example.placed.placement.isInside)
  }

  test("placement: outside the image frame and outside the screen are told apart") {
    val ledger = get(CorrectionLedger.of(spec))
    val placed = get(ledger.placeAll(positions.positions))
    assertEquals(
      placed.map(_.placement.productPrefix),
      Vector("OutsideWindow", "OutsideScreen", "Inside", "Inside")
    )
  }

  test("a flip is recorded as a rule and applied to a copy; the source position never moves") {
    val ledger = get(CorrectionLedger.of(withRules(flipTrial)))
    val raw    = positions.position(1).get
    val placed = get(ledger.place(raw))
    assertEquals(placed.source, raw)
    assertEquals(placed.source.point, Pt[Unit2D.Px](260.0, 120.0))
    assertEquals(placed.rule, Some(0))
    // eyes4s's half-open reflection about the screen's vertical centre line.
    assertEquals(placed.corrected, Pt[Unit2D.Px](1920.0 - 260.0, 120.0))
    assertEquals(placed.placement, Placement.OutsideWindow)
    // Another trial of the participant is not covered by a trial rule.
    val other = get(ledger.place(positions.position(4).get))
    assertEquals((other.rule, other.corrected), (None, Pt[Unit2D.Px](700.0, 420.0)))
    // Re-reading the source under the corrected revision gives the same positions.
    assertEquals(
      get(SourcePositions.read(withRules(flipTrial), bytes)).positions,
      positions.positions
    )
  }

  test("a participant rule covers every trial of the participant; a translation adds") {
    val p05    = right(ParticipantId.of("P05"))
    val offset = right(Offset.of(12.5, -20.0))
    val ledger = get(
      CorrectionLedger.of(
        withRules(
          rule(CorrectionTarget.Participant(p05), CoordinateCorrection.Translate(offset))
        )
      )
    )
    val placed = get(ledger.placeAll(positions.positions))
    assertEquals(placed.map(_.rule), Vector(Some(0), Some(0), None, Some(0)))
    assertEquals(placed(3).corrected, Pt[Unit2D.Px](712.5, 400.0))
    assertEquals(placed(3).source, positions.positions(3))
    assertEquals(placed(3).source.trial, p05enc01)
  }

  test("placement is of the corrected position: a correction moves a record across frames") {
    val intoWindow = right(Offset.of(400.0, 100.0))
    val offScreen  = right(Offset.of(1300.0, 0.0))
    val ledger     = get(
      CorrectionLedger.of(
        withRules(
          rule(CorrectionTarget.Trial(p05ret04), CoordinateCorrection.Translate(intoWindow)),
          rule(CorrectionTarget.Trial(p05enc01), CoordinateCorrection.Translate(offScreen))
        )
      )
    )
    // Record 1 (260, 120) is on the screen, outside the image frame; corrected to
    // (660, 220) it is inside, at (212, 64) in the frame.
    val in = get(ledger.place(positions.position(1).get))
    assertEquals(in.corrected, Pt[Unit2D.Px](660.0, 220.0))
    assertEquals(in.placement, Placement.Inside(Pt[Unit2D.Px](212.0, 64.0)))
    // Record 4 (700, 420) is inside; corrected to (2000, 420) it is off the screen.
    val out = get(ledger.place(positions.position(4).get))
    assertEquals(out.corrected, Pt[Unit2D.Px](2000.0, 420.0))
    assertEquals(out.placement, Placement.OutsideScreen)
    // The worked example follows the corrected position too.
    val example = get(WorkedExample.of(ledger, positions.position(1).get))
    assertEquals(example.image, Pt[Unit2D.Px](212.0, 64.0))
    // Uncorrected, both records sit where the source puts them.
    val plain = get(CorrectionLedger.of(spec))
    assertEquals(
      Vector(1, 4).map(r =>
        get(plain.place(positions.position(r).get)).placement.productPrefix
      ),
      Vector("OutsideWindow", "Inside")
    )
  }

  test("two rules covering one trial: eyes4s's conflict, naming the trial and both rules") {
    val all    = rule(CorrectionTarget.AllTrials, CoordinateCorrection.FlipY)
    val ledger = get(CorrectionLedger.of(withRules(flipTrial, all)))
    val raw    = positions.position(1).get
    assertEquals(ledger.place(raw), Left(GeometryProblem.CorrectionConflict(p05ret04, 0, 1)))
    assert(
      ledger
        .place(raw)
        .left
        .toOption
        .get
        .message
        .contains("rules 1 and 2 both cover P05 · ret_04")
    )
    // A trial only the second rule covers is placed.
    assertEquals(get(ledger.place(positions.position(3).get)).rule, Some(1))
  }

  test("recording a correction changes the revision's rules, never its sources or bytes") {
    val history = History.start(t1)
    val step    = history
      .apply(Command.AddCorrection(r3, 0, flipTrial))
      .fold(e => fail(e.message), identity)
    val after = step.history.document.dataset(r3).get
    assertEquals(after.admission.corrections, Vector(flipTrial))
    assertEquals(after.sources, pending.sources)
    assertEquals(after.geometry, pending.geometry)
    assertEquals(after.mapping, pending.mapping)
    // eyes4s records the rule as the policy's only correction.
    val policy = get(CorrectionLedger.policy(r3, after.admission))
    assertEquals(policy.corrections.size, 1)
    assertEquals(policy.offScreen, eyes4s.plan.OffScreenPolicy.ExcludeRecord)
  }

  test("the density bins every on-screen record once; an off-screen record falls in no cell") {
    val ledger  = get(CorrectionLedger.of(spec))
    val placed  = get(ledger.placeAll(positions.positions))
    val density = get(PlacementDensity.of(ledger.frames, placed))
    assertEquals((density.columns, density.rows), (64, 36))
    assertEquals(density.counts.length, 64 * 36)
    assertEquals((density.placed, density.binned), (4, 3))
    // Record 3 at (1148, 456): 30 px cells, column 38, row 15.
    assertEquals(density.counts(15 * 64 + 38), 1.0)
  }

  test("the reducer refuses a rule that overlaps one recorded: eyes4s's correctionFor (S5.5)") {
    val p05 = CorrectionTarget.Participant(right(ParticipantId.of("P05")))
    val p17 = CorrectionTarget.Participant(right(ParticipantId.of("P17")))
    val all = rule(CorrectionTarget.AllTrials, CoordinateCorrection.FlipY)
    // Overlapping pairs, each refused by the reducer, naming both rules.
    val overlapping = Vector(
      Vector(flipTrial)                             -> all,
      Vector(rule(p05, CoordinateCorrection.FlipX)) -> flipTrial,
      Vector(rule(p05, CoordinateCorrection.FlipX)) -> rule(p05, CoordinateCorrection.FlipY),
      Vector(all)                                   -> rule(p17, CoordinateCorrection.FlipX)
    )
    overlapping.foreach { (recorded, added) =>
      val start = History.start(t1).apply(Command.AddCorrection(r3, 0, recorded.head)) match
        case Right(step) => step.history
        case Left(e)     => fail(e.message)
      assertEquals(
        start.apply(Command.AddCorrection(r3, 1, added)).left.map(_.message),
        Left(
          "AddCorrection on dataset r3 is refused: Dataset r3: correction rules 1 and 2 both " +
            "cover a trial; at most one rule may cover a trial."
        ),
        (recorded, added)
      )
    }
    // Rules that cover no common trial are recorded.
    val apart = History
      .start(t1)
      .apply(Command.AddCorrection(r3, 0, rule(p05, CoordinateCorrection.FlipX)))
      .flatMap(
        _.history.apply(Command.AddCorrection(r3, 1, rule(p17, CoordinateCorrection.FlipY)))
      )
      .flatMap(
        _.history.apply(
          Command.AddCorrection(
            r3,
            2,
            rule(
              CorrectionTarget.Trial(TrialKey("P09", Phase.Encoding, "enc_01", 1)),
              CoordinateCorrection.FlipX
            )
          )
        )
      )
    assert(apart.isRight, apart)
    // A re-import that carries overlapping rules is refused too.
    val t2       = eyes4s.studio.core.document.DocumentSamples.t2
    val r3spec   = t2.dataset(r3).get
    val reimport = Command.ImportSources(
      Some(r3),
      r3spec.sources,
      r3spec.mapping,
      r3spec.units,
      r3spec.geometry,
      r3spec.attributes,
      Some(r3spec.admission.copy(corrections = Vector(all, flipTrial))),
      r3spec.inventory
    )
    assert(History.start(t2).apply(reimport).left.exists(_.message.contains("rules 1 and 2")))
  }
