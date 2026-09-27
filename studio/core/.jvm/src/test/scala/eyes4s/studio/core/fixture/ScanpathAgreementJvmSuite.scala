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

package eyes4s.studio.core.fixture

import eyes4s.io.*
import eyes4s.kernel.*
import eyes4s.plan.AdmissionPolicy
import eyes4s.studio.core.backend.{Phase, TrialKey}

/** The fixture generator's scanpaths agree with eyes4s's own admission: the
  * generator's planted records ([[PlantedScanpaths]]), read by
  * `FixationCsv.admit` under the default `ExcludeRecord` policy against the
  * 1920×1080 screen, give every trial the same scanpath, record for record
  * and position for position, and the records eyes4s finds off the screen
  * (`!frame.contains(centre)`, the test eyes4s's `MapPlacement.OutsideScreen`
  * applies) are the ones the fixture marks `OutsideScreen`.
  */
class ScanpathAgreementJvmSuite extends munit.FunSuite:

  private def get[E, A](e: Either[E, A]): A = e.fold(x => fail(x.toString), identity)

  private val screen = get(Frame.screen("screen", 1920, 1080))

  private val keyColumns = Vector("participant", "phase", "trial", "occurrence")

  private val imported =
    val columns = get(
      FixationColumns
        .of("ordinal", "x", "y", "onset_ms", "duration_ms", "sample_count")
    )
    val keys = get(
      FixationKeyReader.of[String](keyColumns)(
        fields => Right(keyColumns.map(fields).mkString("\t")),
        key => ClockId(s"fixation-trial:$key")
      )
    )
    get(
      FixationCsv.admit(
        PlantedScanpaths.fixationsCsv,
        columns,
        keys,
        screen,
        TimestampUnit.Milliseconds,
        AdmissionPolicy.default[String]
      )
    )

  private def trialKey(key: String): TrialKey =
    key.split("\t").toList match
      case List(p, phase, trial, occurrence) =>
        TrialKey(p, Phase(phase), trial, occurrence.toInt)
      case _ => fail(s"bad key $key")

  private val fixture = get(FakeNavigator.parseScanpaths(PlantedScanpaths.scanpaths))

  test("eyes4s admits every planted record, off-screen ones included") {
    assertEquals(imported.rejected, Vector.empty)
    assertEquals(imported.admitted.size, PlantedScanpaths.fixationsCsv.linesIterator.size - 1)
    assert(imported.outsideFrame.nonEmpty, "the planted records have no off-screen record")
  }

  test("each trial's scanpath has eyes4s's length, records and positions") {
    val trials = imported.accepted.rows
    assertEquals(trials.map(t => trialKey(t.key)).toSet, fixture.keySet)
    trials.foreach { trial =>
      val key = trialKey(trial.key)
      // eyes4s's scanpath order: the trial's admitted rows by ordinal. A CSV
      // row number counts the header as row 1, so data record = row - 1.
      val records = imported.admitted
        .filter(_.key == trial.key)
        .sortBy(_.ordinal)
        .map(_.rowNumber - 1)
      assertEquals(trial.value.n, records.size, key.label)
      assertEquals(fixture(key).map(_.record), records, key.label)
    }
  }

  test("the fixture marks OutsideScreen exactly where eyes4s's centre is off the screen") {
    imported.accepted.rows.foreach { trial =>
      val key    = trialKey(trial.key)
      val eyes4s = trial.value.fixations.toVector.map { f =>
        if screen.contains(f.centre) then ScreenPlacement.OnScreen
        else ScreenPlacement.OutsideScreen
      }
      assertEquals(fixture(key).map(_.placement), eyes4s, key.label)
    }
    val marked = fixture.values.flatten.collect {
      case r if r.placement == ScreenPlacement.OutsideScreen => r.record
    }
    assertEquals(marked.toSet, imported.outsideFrame.map(_.record - 1).toSet)
  }

  test("an off-screen record mid-scanpath shifts no later position") {
    val enc01 = fixture(TrialKey("P90", Phase.Encoding, "enc_01", 1))
    val first = enc01.indexWhere(_.placement == ScreenPlacement.OutsideScreen)
    assert(first > 0 && first < enc01.size - 1, s"off-screen record at $first is not mid-path")
    // Record 5, ordinal 5, is ScanpathPosition 4 in eyes4s and here.
    assertEquals(enc01.indexWhere(_.record == 5), 4)
  }
