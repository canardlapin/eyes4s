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

import eyes4s.io.*
import eyes4s.kernel.ClockId
import eyes4s.plan.{AdmissionPolicy, AppliedCorrection, CorrectionScope, WindowTally}
import eyes4s.studio.core.backend.{Phase, TrialKey}
import eyes4s.studio.core.document.{
  CoordinateCorrection,
  CorrectionRule,
  CorrectionTarget,
  DatasetRevisionSpec,
  OffScreenChoice,
  Offset,
  ParticipantId,
  Source,
  SourcePath,
  SourceRole,
  Sources
}
import eyes4s.studio.core.fixture.{GoldenCsv, MockStudy, StoryMoments}

import java.nio.charset.StandardCharsets.UTF_8

/** The geometry panel's placement of the golden fixation source agrees with
  * the fixture and with eyes4s's own admission (ticket S5.5). r3 reads
  * fixtures/studio-golden/fixations.csv; placed under r3's declared
  * geometry, its records outside the image frame are the fixture's 543 of
  * 11,520 in 409 trials, and in every trial eyes4s admits
  * (`FixationCsv.admit`, default policy), eyes4s's `WindowTally.window`
  * counts the same records outside the window and outside the screen.
  */
class GoldenPlacementJvmSuite extends munit.FunSuite:

  private def get[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  // The file's exact text, generated into test scope (studio-core reads no files).
  private val bytes: Array[Byte] = GoldenCsv.fixations.getBytes(UTF_8)

  private val r3 = get(
    StoryMoments.t1.flatMap(_.dataset(StoryMoments.r3).toRight("the story has no r3"))
  )

  private val ledger    = get(CorrectionLedger.of(r3))
  private val positions = get(SourcePositions.read(r3, IArray.from(bytes)))
  private val placed    = get(ledger.placeAll(positions.positions))

  private def trialsWhere(p: PlacedPosition => Boolean): Set[TrialKey] =
    placed.filter(p).map(_.source.trial).toSet

  private val outsideWindow = (p: PlacedPosition) => p.placement == Placement.OutsideWindow
  private val outsideScreen = (p: PlacedPosition) => p.placement == Placement.OutsideScreen

  test("r3's golden records place as the fixture counts them: 543 of 11,520 in 409 trials") {
    val summary = get(MockStudy.load).summary
    assertEquals(positions.unplaced, Vector.empty)
    assertEquals(placed.size, summary.fixationRecords)
    assertEquals(placed.count(outsideWindow), summary.outsideWindowRecords)
    assertEquals(trialsWhere(outsideWindow).size, summary.outsideWindowTrials)
    assertEquals((placed.size, placed.count(outsideWindow)), (11520, 543))
    assertEquals(placed.count(outsideScreen), 0)
  }

  private val keyColumns = Vector("participant", "phase", "trial", "occurrence")

  private def keyOf(t: TrialKey): String =
    Vector(t.participant, t.phase.label, t.trial, t.occurrence.toString).mkString("\t")

  /** `spec`'s records as the panel places them, and eyes4s's window tally of
    * each trial `FixationCsv.admit` accepts under the same admission policy;
    * the per-trial counts must agree. Returns eyes4s's tallies by trial.
    */
  private def agreement(spec: DatasetRevisionSpec): Map[TrialKey, WindowTally] =
    val ours    = get(CorrectionLedger.of(spec))
    val byTrial = get(ours.placeAll(positions.positions)).groupBy(_.source.trial)
    val policy  = AdmissionPolicy[String](
      ours.policy.offScreen,
      ours.policy.corrections.map { c =>
        val scope: CorrectionScope[String] = c.scope match
          case CorrectionScope.AllTrials()    => CorrectionScope.AllTrials()
          case CorrectionScope.Participant(p) => CorrectionScope.Participant(p)
          case CorrectionScope.Trial(k)       => CorrectionScope.Trial(keyOf(k))
        AppliedCorrection(scope, c.correction)
      }
    )
    val columns = get(
      FixationColumns.of("ordinal", "x", "y", "onset_ms", "duration_ms", "sample_count")
    )
    val keys = get(
      FixationKeyReader.withParticipant[String](keyColumns)(
        fields => Right(keyColumns.map(fields).mkString("\t")),
        key => ClockId(s"fixation-trial:$key"),
        _.takeWhile(_ != '\t')
      )
    )
    val imported = get(
      FixationCsv.admit(
        GoldenCsv.fixations,
        columns,
        keys,
        ours.frames.screen,
        TimestampUnit.Milliseconds,
        policy
      )
    )
    val rows = imported.accepted.rows
    assert(rows.size > 900, s"eyes4s accepted only ${rows.size} trials")
    rows.map { row =>
      val trial = row.key.split("\t").toList match
        case List(p, phase, t, o) => TrialKey(p, Phase(phase), t, o.toInt)
        case _                    => fail(s"bad key ${row.key}")
      val tally = get(WindowTally.window(ours.frames.image, row.value))
      val mine  = byTrial.getOrElse(trial, Vector.empty)
      assertEquals(
        (mine.size, mine.count(outsideWindow), mine.count(outsideScreen)),
        (tally.total, tally.outsideWindow, tally.outsideScreen),
        trial.label
      )
      trial -> tally
    }.toMap

  test("in every trial eyes4s admits, its window tally counts the records the panel places") {
    agreement(r3)
  }

  test("under a recorded correction, the panel places each record where eyes4s does") {
    // P05 shifted 300 px right; one P17 trial flipped vertically.
    val p05   = get(ParticipantId.of("P05"))
    val shift = get(Offset.of(300.0, 0.0))
    val flip  = TrialKey("P17", Phase.Encoding, "enc_03", 1)
    val rules = Vector(
      CorrectionRule(CorrectionTarget.Participant(p05), CoordinateCorrection.Translate(shift)),
      CorrectionRule(CorrectionTarget.Trial(flip), CoordinateCorrection.FlipY)
    )
    val corrected = agreement(r3.copy(admission = r3.admission.copy(corrections = rules)))
    val plain     = agreement(r3)
    // The correction changes eyes4s's tallies for P05, so the check bites.
    val moved = corrected.keySet.filter(_.participant == "P05").count { t =>
      plain.get(t).map(p => (p.outsideWindow, p.outsideScreen)) !=
        corrected.get(t).map(c => (c.outsideWindow, c.outsideScreen))
    }
    assert(moved > 0, "the P05 shift changed no trial's tally")
  }

  // --- off-screen records (S5.5 follow-up): the golden has none -------------------

  /** Three trials; P02 enc_01 has a record left of the screen and P03 enc_01
    * one below it; P01 enc_01 stays on the screen (one record outside the
    * image frame, inside the screen).
    */
  private val offScreenCsv: String =
    """participant,phase,trial,occurrence,ordinal,x,y,onset_ms,duration_ms,sample_count
      |P01,Encoding,enc_01,1,1,700,420,0,200,100
      |P01,Encoding,enc_01,1,2,100,420,210,200,100
      |P02,Encoding,enc_01,1,1,700,420,0,200,100
      |P02,Encoding,enc_01,1,2,-40.5,500,210,200,100
      |P03,Encoding,enc_01,1,1,1900,1200,0,200,100
      |P03,Encoding,enc_01,1,2,900,500,210,200,100
      |""".stripMargin

  private def offScreenSpec(choice: OffScreenChoice): DatasetRevisionSpec =
    val raw    = offScreenCsv.getBytes(UTF_8)
    val source = Source(
      SourceRole.Fixations,
      get(SourcePath.of("inputs/fixations.csv")),
      eyes4s.codec.ByteDigest.sha256(IArray.from(raw)),
      None
    )
    r3.copy(
      sources = get(Sources.of(Vector(source))),
      inventory = None,
      admission = r3.admission.copy(offScreen = choice)
    )

  /** The panel's off-screen records, and eyes4s's: under ExcludeRecord the
    * records it admits outside the frame, under QuarantineTrial the records
    * it rejects for their position, with the trials it leaves out.
    */
  private def offScreen(
      choice: OffScreenChoice
  ): (Vector[Int], Vector[Int], Set[TrialKey], Set[TrialKey]) =
    val spec    = offScreenSpec(choice)
    val ours    = get(CorrectionLedger.of(spec))
    val read    = get(SourcePositions.read(spec, IArray.from(offScreenCsv.getBytes(UTF_8))))
    val placed  = get(ours.placeAll(read.positions))
    val columns = get(
      FixationColumns.of("ordinal", "x", "y", "onset_ms", "duration_ms", "sample_count")
    )
    val keys = get(
      FixationKeyReader.withParticipant[String](keyColumns)(
        fields => Right(keyColumns.map(fields).mkString("\t")),
        key => ClockId(s"fixation-trial:$key"),
        _.takeWhile(_ != '\t')
      )
    )
    val imported = get(
      FixationCsv.admit(
        offScreenCsv,
        columns,
        keys,
        ours.frames.screen,
        TimestampUnit.Milliseconds,
        AdmissionPolicy[String](ours.policy.offScreen, Vector.empty)
      )
    )
    // eyes4s numbers a row by its line in the file, the header being line 1;
    // the panel numbers data records from 1. Record n is line n + 1.
    val theirs = (choice match
      case OffScreenChoice.ExcludeRecord   => imported.outsideFrame.map(_.record)
      case OffScreenChoice.QuarantineTrial =>
        imported.rejected.collect {
          case r if r.error.isInstanceOf[FixationRowError.Position] => r.rowNumber
        }
    ).map(_ - 1)
    def trial(key: String) = key.split("\t").toList match
      case List(p, phase, t, o) => TrialKey(p, Phase(phase), t, o.toInt)
      case _                    => fail(s"bad key $key")
    (
      placed.filter(outsideScreen).map(_.source.record),
      theirs,
      read.trials.toSet -- imported.accepted.rows.map(r => trial(r.key)).toSet,
      placed.filter(outsideScreen).map(_.source.trial).toSet
    )

  test(
    "off-screen records: the panel and eyes4s agree under ExcludeRecord and QuarantineTrial"
  ) {
    // Records 4 (P02, x < 0) and 5 (P03, y > 1080) are off the screen; 2 is off the image only.
    val (ours, theirs, left, offTrials) = offScreen(OffScreenChoice.ExcludeRecord)
    assertEquals(ours, Vector(4, 5))
    assertEquals(theirs, ours)
    // Excluding a record leaves its trial in.
    assertEquals(left, Set.empty[TrialKey])
    val (qOurs, qTheirs, qLeft, qTrials) = offScreen(OffScreenChoice.QuarantineTrial)
    assertEquals(qOurs, Vector(4, 5))
    assertEquals(qTheirs, qOurs)
    // Quarantining leaves out exactly the trials with an off-screen record.
    assertEquals(qLeft, qTrials)
    assertEquals(
      qTrials,
      Set(
        TrialKey("P02", Phase.Encoding, "enc_01", 1),
        TrialKey("P03", Phase.Encoding, "enc_01", 1)
      )
    )
    assertEquals(offTrials, qTrials)
  }
