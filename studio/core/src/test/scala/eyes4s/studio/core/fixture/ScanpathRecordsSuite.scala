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

import eyes4s.studio.core.backend.*

/** The fixture's scanpaths keep every admitted record, as eyes4s does: an
  * off-screen record stays in its trial's scanpath, marked `OutsideScreen`,
  * so every later fixation keeps eyes4s's `ScanpathPosition`. Checked on the
  * golden fixture, which has no off-screen record, and on the generator's
  * planted records ([[PlantedScanpaths]]), which do.
  */
class ScanpathRecordsSuite extends munit.FunSuite:

  private def ok[A](e: Either[String, A]): A = e.fold(m => fail(m), identity)

  private val study: MockStudy =
    MockStudy.load.fold(e => throw new AssertionError(e), identity)

  private val golden = ok(FakeNavigator.parseScanpaths(GoldenInventory.scanpaths))

  private val admitted: Vector[LedgerEntry] =
    study.inventory.filter(_.disposition == TrialDisposition.Admitted)

  import ScreenPlacement.*

  test("the golden scanpaths parse, one per admitted trial") {
    assertEquals(golden.keySet, admitted.map(_.trial).toSet)
    assertEquals(study.scanpaths, golden)
  }

  test("an unreadable scanpath line fails the load, naming the line") {
    val bad   = "P17\tEncoding\tenc_03\t1\t7209,seven"
    val lines = GoldenInventory.scanpaths.linesIterator.toVector
    val text  = (lines.head +: bad +: lines.tail).mkString("\n")
    assertEquals(
      MockStudy.assemble(MockStudy.fixtureText, GoldenInventory.trials, text),
      Left(s"scanpath line '$bad': bad record 'seven'")
    )
    // The same inputs with the golden scanpaths load.
    assert(
      MockStudy
        .assemble(MockStudy.fixtureText, GoldenInventory.trials, GoldenInventory.scanpaths)
        .isRight
    )
  }

  test("every tallied record is a fixation of its trial's scanpath") {
    assertEquals(golden.values.map(_.size).sum, GoldenInventory.talliedRecords)
    val records = golden.values.flatten.map(_.record).toVector
    assertEquals(records.distinct.size, records.size)
  }

  test("a scanpath marks exactly its trial's outside-frame records OutsideScreen") {
    admitted.foreach { e =>
      val marked = golden(e.trial).filter(_.placement == OutsideScreen).map(_.record)
      assertEquals(marked, e.outsideFrame.map(_.record), e.trial.label)
    }
    val outside = golden.values.flatten.count(_.placement == OutsideScreen)
    assertEquals(outside, GoldenInventory.outsideScreen)
  }

  test("P17 enc_03: 13 fixations, fixation 6 is record 7,214") {
    val enc03 = golden(TrialKey("P17", Phase.Encoding, "enc_03", 1))
    assertEquals(enc03.size, 13)
    assertEquals(enc03(5), ScanpathRecord(7214, OnScreen))
    assertEquals(golden(TrialKey("P17", Phase.Retrieval, "ret_07", 1)).size, 12)
  }

  test("planted: off-screen records keep their positions, in ordinal order") {
    val planted = ok(FakeNavigator.parseScanpaths(PlantedScanpaths.scanpaths))
    // Records 3 (ordinal 2) and 4 (ordinal 4) are off the screen; record 2 has
    // ordinal 3. Dropping the off-screen records would shift record 2 from
    // position 2 to position 1 and record 5 from position 4 to position 2.
    assertEquals(
      planted(TrialKey("P90", Phase.Encoding, "enc_01", 1)),
      Vector(
        ScanpathRecord(1, OnScreen),
        ScanpathRecord(3, OutsideScreen),
        ScanpathRecord(2, OnScreen),
        ScanpathRecord(4, OutsideScreen),
        ScanpathRecord(5, OnScreen)
      )
    )
    // x = 1920 is off the half-open 1920-px screen.
    assertEquals(
      planted(TrialKey("P90", Phase.Encoding, "enc_02", 1)),
      Vector(
        ScanpathRecord(6, OnScreen),
        ScanpathRecord(7, OutsideScreen),
        ScanpathRecord(8, OnScreen)
      )
    )
  }

  test("a scanpath line that cannot be read is refused, naming it") {
    val bad = "P01\tEncoding\tenc_01\t1\t1,x:outside-screen"
    assertEquals(
      FakeNavigator.parseScanpaths(bad),
      Left(s"scanpath line '$bad': bad record 'x:outside-screen'")
    )
    val repeated = "P01\tEncoding\tenc_01\t1\t1\nP01\tEncoding\tenc_01\t1\t2"
    assert(FakeNavigator.parseScanpaths(repeated).isLeft)
    assert(FakeNavigator.parseScanpaths("P01\tEncoding\tenc_01\t1\t").isLeft)
  }
