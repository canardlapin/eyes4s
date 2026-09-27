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
import eyes4s.plan.{AdmissionPolicy, WindowTally}
import eyes4s.studio.core.backend.{Phase, TrialKey}
import eyes4s.studio.core.fixture.{GoldenFixations, MockStudy, StoryMoments}

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
  private val bytes: Array[Byte] = GoldenFixations.csv.getBytes(UTF_8)

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

  test("in every trial eyes4s admits, its window tally counts the records the panel places") {
    val keyColumns = Vector("participant", "phase", "trial", "occurrence")
    val columns    = get(
      FixationColumns.of("ordinal", "x", "y", "onset_ms", "duration_ms", "sample_count")
    )
    val keys = get(
      FixationKeyReader.of[String](keyColumns)(
        fields => Right(keyColumns.map(fields).mkString("\t")),
        key => ClockId(s"fixation-trial:$key")
      )
    )
    val imported = get(
      FixationCsv.admit(
        GoldenFixations.csv,
        columns,
        keys,
        ledger.frames.screen,
        TimestampUnit.Milliseconds,
        AdmissionPolicy.default[String]
      )
    )
    val byTrial = placed.groupBy(_.source.trial)
    val rows    = imported.accepted.rows
    assert(rows.size > 900, s"eyes4s accepted only ${rows.size} trials")
    rows.foreach { row =>
      val trial = row.key.split("\t").toList match
        case List(p, phase, t, o) => TrialKey(p, Phase(phase), t, o.toInt)
        case _                    => fail(s"bad key ${row.key}")
      val tally: WindowTally = get(WindowTally.window(ledger.frames.image, row.value))
      val ours               = byTrial.getOrElse(trial, Vector.empty)
      assertEquals(
        (ours.size, ours.count(outsideWindow), ours.count(outsideScreen)),
        (tally.total, tally.outsideWindow, tally.outsideScreen),
        trial.label
      )
    }
  }
