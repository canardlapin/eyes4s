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

class LocationBundleImportSuite extends munit.FunSuite:
  private val target    = "sun_bnjxztsvnercygxn.jpg"
  private val duplicate = "sun_bdwttbytrbnqyqsk.jpg"
  private val headers   =
    "image_index\timage\tcategory\tobserver_slot\trecording_label\tinitials\tsource_trial\tphase\tslot_state\tgaze_state\tsdt\tresponse_state\tsource_locations\tadmitted_locations\timage_family\n"
  private val ledgerHeader =
    "image_index\tobserver_slot\tphase\tordinal\traw_x\traw_y\trounded_x\trounded_y\tadmitted\tx_px\ty_px\traw_zero_x\traw_zero_y\treason\n"
  private def accepted(source: LocationBundleText) = LocationBundleImport.read(source) match
    case Right(value) => value
    case Left(error)  => fail(error.message)
  private def refused(source: LocationBundleText): Unit =
    LocationBundleImport.read(source) match
      case Left(error) => assert(error.message.nonEmpty)
      case Right(_)    => fail("expected a named import refusal")

  private def bundle(
      pairControl: String = "a1",
      ledgerExtra: String = "",
      phaseExtra: String = ""
  ): LocationBundleText =
    val trials =
      s"""trial_id,observer_slot,image,category,phase,sdt,image_index,image_family
         |e1,1,$target,c,enc,1,1,$duplicate
         |r1,1,$target,c,rec,2,1,$duplicate
         |d1,1,$duplicate,c,enc,1,2,$duplicate
         |a1,1,apple.jpg,c,enc,1,3,apple.jpg
         |z1,1,zoo.jpg,c,enc,1,4,zoo.jpg
         |""".stripMargin
    val points = "trial_id\tx_px\ty_px\n" + "e1\t0\t2\nr1\t2\t4\nd1\t4\t5\na1\t6\t7\nz1\t8\t9\n"
    val phases = headers +
      s"1\t$target\tc\t1\tlabel-1\tAB\t1\tenc\tmetadata_present\tlocations\t1\trecorded\t1\t1\t$duplicate\n" +
      s"1\t$target\tc\t1\tlabel-1\tAB\t1\trec\tmetadata_present\tlocations\t2\trecorded\t1\t1\t$duplicate\n" +
      s"2\t$duplicate\tc\t1\tlabel-1\tAB\t2\tenc\tmetadata_present\tlocations\t1\trecorded\t1\t1\t$duplicate\n" +
      "3\tapple.jpg\tc\t1\tlabel-1\tAB\t3\tenc\tmetadata_present\tlocations\t1\trecorded\t1\t1\tapple.jpg\n" +
      "4\tzoo.jpg\tc\t1\tlabel-1\tAB\t4\tenc\tmetadata_present\tlocations\t1\trecorded\t1\t1\tzoo.jpg\n" +
      s"1\t$target\tc\t2\tlabel-2\tAB\t\trec2\tmetadata_present\tmissing_phase\t\tabsent\t0\t0\t$duplicate\n" + phaseExtra
    val ledger = ledgerHeader +
      "1\t1\tenc\t1\t1.49\t2.5\t1\t3\t1\t0\t2\t0.49\t1.5\tadmitted\n" +
      "1\t1\trec\t1\t3.49\t4.5\t3\t5\t1\t2\t4\t2.49\t3.5\tadmitted\n" +
      "2\t1\tenc\t1\t5\t6\t5\t6\t1\t4\t5\t4\t5\tadmitted\n" +
      "3\t1\tenc\t1\t7\t8\t7\t8\t1\t6\t7\t6\t7\tadmitted\n" +
      "4\t1\tenc\t1\t9\t10\t9\t10\t1\t8\t9\t8\t9\tadmitted\n" + ledgerExtra
    LocationBundleText(
      trials,
      points,
      s"pair_id\tfocal_id\tencoding_id\tcontrol_id\np1\tr1\te1\t$pairControl\n",
      phases,
      ledger
    )

  test(
    "preserves asymmetric MATLAB raster coordinates, source gaps, missing phases and shared initials"
  ) {
    val result = accepted(bundle())
    assertEquals(
      result.input.trials.find(_.id.value == "e1").get.observations.head.position.x,
      0.0
    )
    assertEquals(
      result.input.trials.find(_.id.value == "e1").get.observations.head.position.y,
      2.0
    )
    assertEquals(result.phaseLedger.count(_.initials == "AB"), 6)
    assert(result.phaseLedger.exists(_.gazeState == "missing_phase"))
    assertEquals(result.queries.map(_.id), Vector("p1"))
  }

  test("retains rejected nonfinite and out-of-bounds rows while rejecting a false admission") {
    val extras = "1\t2\trec2\t1\tnan\t2\tnan\t2\t0\t\t\tnan\t1\tnonfinite\n" +
      "1\t2\trec2\t2\t1000.5\t1\t1001\t1\t0\t\t\t999.5\t0\toutside_pixel_bounds\n"
    val base   = bundle(ledgerExtra = extras)
    val result = accepted(
      base.copy(phaseLedgerTsv =
        base.phaseLedgerTsv.replace(
          s"1\t$target\tc\t2\tlabel-2\tAB\t\trec2\tmetadata_present\tmissing_phase\t\tabsent\t0\t0\t$duplicate\n",
          s"1\t$target\tc\t2\tlabel-2\tAB\t\trec2\tmetadata_present\tlocations\t1\trecorded\t2\t0\t$duplicate\n"
        )
      )
    )
    assertEquals(result.locationLedger.count(!_.admitted), 2)
    assertEquals(result.locationLedger.filter(!_.admitted).map(_.ordinal), Vector(1L, 2L))
  }

  test("refuses phase source/admission accounting disagreement") {
    val source = bundle()
    val bad    = source.copy(phaseLedgerTsv =
      source.phaseLedgerTsv.replace(
        s"1\t$target\tc\t1\tlabel-1\tAB\t1\trec\tmetadata_present\tlocations\t2\trecorded\t1\t1\t$duplicate\n",
        s"1\t$target\tc\t1\tlabel-1\tAB\t1\trec\tmetadata_present\tlocations\t2\trecorded\t2\t1\t$duplicate\n"
      )
    )
    refused(bad)
  }

  test("excludes duplicate-image family controls and requires lexical first control") {
    refused(bundle(pairControl = "d1"))
    refused(bundle(pairControl = "z1"))
  }

  test("requires exactly one supplied pair for every eligible retrieval focal") {
    val source = bundle()
    refused(source.copy(pairsTsv = source.pairsTsv + "p2\tr1\te1\ta1\n"))
    refused(source.copy(pairsTsv = "pair_id\tfocal_id\tencoding_id\tcontrol_id\n"))
  }

  test("retains an admitted source ordinal after a rejected predecessor") {
    val source      = bundle()
    val replacement =
      "1\t1\trec\t1\tnan\t4.5\tnan\t5\t0\t\t\tnan\t3.5\tnonfinite\n" +
        "1\t1\trec\t2\t3.49\t4.5\t3\t5\t1\t2\t4\t2.49\t3.5\tadmitted\n"
    val changed = source.copy(
      locationLedgerTsv = source.locationLedgerTsv
        .replace("1\t1\trec\t1\t3.49\t4.5\t3\t5\t1\t2\t4\t2.49\t3.5\tadmitted\n", replacement),
      phaseLedgerTsv = source.phaseLedgerTsv.replace(
        s"1\t$target\tc\t1\tlabel-1\tAB\t1\trec\tmetadata_present\tlocations\t2\trecorded\t1\t1\t$duplicate\n",
        s"1\t$target\tc\t1\tlabel-1\tAB\t1\trec\tmetadata_present\tlocations\t2\trecorded\t2\t1\t$duplicate\n"
      )
    )
    assertEquals(
      accepted(changed).input.trials
        .find(_.id.value == "r1")
        .get
        .observations
        .head
        .sourceOrdinal,
      2L
    )
  }

  test("refuses independent mutations of every derived ledger field") {
    val source = bundle()
    val row    = "1\t1\tenc\t1\t1.49\t2.5\t1\t3\t1\t0\t2\t0.49\t1.5\tadmitted\n"
    Vector(
      row.replace("1.49", "1000.5"),
      row.replace("\t1\t3\t1\t0", "\t2\t3\t1\t0"),
      row.replace("\t0.49\t1.5", "\t0.5\t1.5"),
      row.replace("admitted", "outside_pixel_bounds"),
      row.replace("\t1\t0\t2\t", "\t0\t\t\t")
    ).foreach(replacement =>
      refused(
        source.copy(locationLedgerTsv = source.locationLedgerTsv.replace(row, replacement))
      )
    )
  }

  test(
    "refuses family-only, unknown-phase, duplicate rejected ordinal and false-admission mutations"
  ) {
    val source = bundle()
    refused(
      source.copy(trialsCsv =
        source.trialsCsv
          .replace(s"e1,1,$target,c,enc,1,1,$duplicate", s"e1,1,$target,c,enc,1,1,$target")
      )
    )
    refused(
      source.copy(phaseLedgerTsv =
        source.phaseLedgerTsv.replace("\tenc\tmetadata_present", "\tunknown\tmetadata_present")
      )
    )
    val rejected =
      bundle(ledgerExtra = "1\t2\trec2\t1\tnan\t2\tnan\t2\t0\t\t\tnan\t1\tnonfinite\n")
    val phase =
      rejected.phaseLedgerTsv.replace(s"\t0\t0\t$duplicate\n", s"\t1\t0\t$duplicate\n")
    refused(
      rejected.copy(
        phaseLedgerTsv = phase,
        locationLedgerTsv =
          rejected.locationLedgerTsv + "1\t2\trec2\t1\t1001\t2\t1001\t2\t0\t\t\t1000\t1\toutside_pixel_bounds\n"
      )
    )
    refused(
      source.copy(locationLedgerTsv =
        source.locationLedgerTsv.replace(
          "\t1\t0\t2\t0.49\t1.5\tadmitted",
          "\t1\t0\t2\t0.49\t1.5\toutside_pixel_bounds"
        )
      )
    )
  }

  test("accepts shuffled ledger and lightweight point rows") {
    val source         = bundle()
    val lines          = source.locationLedgerTsv.linesIterator.toVector
    val shuffledLedger = (lines.head +: lines.tail.reverse).mkString("\n") + "\n"
    val points         = source.locationsTsv.linesIterator.toVector
    val shuffledPoints = (points.head +: points.tail.reverse).mkString("\n") + "\n"
    assertEquals(
      accepted(
        source.copy(locationLedgerTsv = shuffledLedger, locationsTsv = shuffledPoints)
      ).queries.map(_.id),
      Vector("p1")
    )
  }

  test("one image cannot acquire another index and a second matched encoding") {
    val good = bundle()
    accepted(good)
    val duplicateTrial = good.trialsCsv.linesIterator
      .find(_.startsWith("e1,"))
      .get
      .replace("e1,", "e2,")
      .replace(",1," + duplicate, ",5," + duplicate)
    val encodingPhase = good.phaseLedgerTsv.linesIterator
      .find(line => line.startsWith("1\t" + target) && line.contains("\tenc\t"))
      .get
    val bad = good.copy(
      trialsCsv = good.trialsCsv + duplicateTrial + "\n",
      locationsTsv = good.locationsTsv + "e2\t0\t2\n",
      phaseLedgerTsv = good.phaseLedgerTsv + "5" + encodingPhase.drop(1) + "\n",
      locationLedgerTsv =
        good.locationLedgerTsv + "5\t1\tenc\t1\t1.49\t2.5\t1\t3\t1\t0\t2\t0.49\t1.5\tadmitted\n",
      pairsTsv = good.pairsTsv.replace("\te1\t", "\te2\t")
    )
    assert(
      LocationBundleImport
        .read(bad)
        .left
        .toOption
        .exists(_.isInstanceOf[LocationImportError.ImageIdentity])
    )
  }

  test("a duplicate partner outside the supplied release does not change an image family") {
    val good                                       = bundle()
    def omit(text: String, prefix: String): String =
      text.linesIterator.filterNot(_.startsWith(prefix)).mkString("\n") + "\n"
    val single = good.copy(
      trialsCsv = omit(good.trialsCsv, "d1,").replace(duplicate, target),
      locationsTsv = omit(good.locationsTsv, "d1\t"),
      phaseLedgerTsv = omit(good.phaseLedgerTsv, "2\t").replace(duplicate, target),
      locationLedgerTsv = omit(good.locationLedgerTsv, "2\t")
    )
    val result = accepted(single)
    assertEquals(result.trialMetadata.find(_.id.value == "e1").map(_.imageFamily), Some(target))
  }
