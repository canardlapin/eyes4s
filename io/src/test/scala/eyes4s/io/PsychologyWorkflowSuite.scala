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

class PsychologyWorkflowSuite extends munit.FunSuite:

  /** Exact 90-row excerpt from AdSERP's public Zenodo record 15236546,
    * 'pupil-data.zip/pupil-data/p049-b4-t6.csv'.
    *
    * The source file is MIT-licensed and identified by SHA-256
    * 'bd5f7e02fb261e7b04415d015cfd880ab475826c630f0d98cfdac5cc27150c11'.
    * This excerpt covers source rows 231--320 and includes the native invalid
    * run at timestamps 1678716023889--1678716024003. Its pinned SHA-256 is
    * asserted below so a changed fixture cannot silently certify a different
    * workflow.
    */
  private val publicTrial =
    """|timestamp,BPOGX,BPOGY,LPD,LPV,RPD,RPV
       |1678716023627,286,37,16.1748,1,15.72388,1
       |1678716023634,283,27,16.37855,1,16.38566,1
       |1678716023641,275,38,16.75625,1,15.76367,1
       |1678716023648,283,31,16.47646,1,15.8989,1
       |1678716023654,284,36,16.38384,1,16.37151,1
       |1678716023661,309,42,16.18787,1,16.19212,1
       |1678716023667,306,56,16.26494,1,16.41568,1
       |1678716023674,308,61,16.45727,1,15.93374,1
       |1678716023681,302,48,16.4782,1,16.28888,1
       |1678716023688,303,43,16.53103,1,16.00881,1
       |1678716023695,304,47,16.76045,1,15.99618,1
       |1678716023701,294,62,16.18228,1,16.07875,1
       |1678716023708,298,90,16.4353,1,15.91919,1
       |1678716023715,301,100,16.27377,1,15.89931,1
       |1678716023721,284,82,16.45027,1,16.00389,1
       |1678716023729,286,66,16.15063,1,15.89931,1
       |1678716023735,285,61,16.1262,1,16.47783,1
       |1678716023742,286,65,16.00095,1,16.54063,1
       |1678716023748,281,57,15.92675,1,16.37913,1
       |1678716023755,279,52,15.86271,1,16.57829,1
       |1678716023761,268,68,17.38,1,16.65694,1
       |1678716023768,268,75,17.03852,1,17.05421,1
       |1678716023775,269,76,17.06575,1,15.76329,1
       |1678716023782,266,82,17.33442,1,15.80801,1
       |1678716023788,259,78,17.31759,1,16.96101,1
       |1678716023795,259,78,17.3913,1,16.01009,1
       |1678716023802,282,60,17.03699,1,15.99461,1
       |1678716023808,299,61,16.80276,1,16.32905,1
       |1678716023815,299,61,16.67921,1,16.35268,1
       |1678716023822,286,41,16.92378,1,16.41162,1
       |1678716023829,292,43,16.89485,1,16.38194,1
       |1678716023835,292,43,16.40362,1,15.88995,1
       |1678716023843,302,49,16.57062,1,16.76645,1
       |1678716023849,331,64,16.57053,1,15.95534,1
       |1678716023855,331,64,16.89856,1,17.19242,1
       |1678716023862,290,70,17.20355,1,16.85268,1
       |1678716023868,302,127,16.94791,1,15.45347,1
       |1678716023875,302,127,16.63858,1,9.22122,1
       |1678716023882,440,669,12.2255,1,9.22122,0
       |1678716023889,440,669,12.2255,0,9.22122,0
       |1678716023895,440,669,12.2255,0,9.22122,0
       |1678716023902,440,669,12.2255,0,9.22122,0
       |1678716023909,440,669,12.2255,0,9.22122,0
       |1678716023916,440,669,12.2255,0,9.22122,0
       |1678716023922,440,669,12.2255,0,9.22122,0
       |1678716023929,440,669,12.2255,0,9.22122,0
       |1678716023936,440,669,12.2255,0,9.22122,0
       |1678716023942,440,669,12.2255,0,9.22122,0
       |1678716023950,440,669,12.2255,0,9.22122,0
       |1678716023956,440,669,12.2255,0,9.22122,0
       |1678716023963,440,669,12.2255,0,9.22122,0
       |1678716023969,440,669,12.2255,0,9.22122,0
       |1678716023976,440,669,12.2255,0,9.22122,0
       |1678716023983,440,669,12.2255,0,9.22122,0
       |1678716023989,440,669,12.2255,0,9.22122,0
       |1678716023996,440,669,12.2255,0,9.22122,0
       |1678716024003,440,669,12.2255,0,9.22122,0
       |1678716024009,220,98,17.64664,1,17.19048,1
       |1678716024018,222,102,17.41543,1,17.4227,1
       |1678716024023,236,100,16.70372,1,17.23262,1
       |1678716024029,242,111,16.37418,1,17.16932,1
       |1678716024036,244,115,17.32192,1,16.68538,1
       |1678716024043,240,122,17.15079,1,17.15284,1
       |1678716024050,224,137,17.09329,1,16.77675,1
       |1678716024056,219,124,17.55083,1,16.87175,1
       |1678716024063,218,99,17.52372,1,17.30272,1
       |1678716024069,215,94,17.02748,1,17.07471,1
       |1678716024076,216,98,17.47252,1,16.74614,1
       |1678716024083,223,105,17.2193,1,16.78398,1
       |1678716024090,210,124,17.01851,1,16.74872,1
       |1678716024097,215,138,17.24068,1,17.26526,1
       |1678716024103,230,132,17.16367,1,16.55654,1
       |1678716024110,217,132,17.12316,1,16.76808,1
       |1678716024117,216,128,17.25674,1,17.40866,1
       |1678716024123,225,118,17.50633,1,17.25128,1
       |1678716024130,237,104,17.52279,1,16.58838,1
       |1678716024136,234,96,17.06662,1,17.27677,1
       |1678716024144,227,93,17.08217,1,17.41287,1
       |1678716024150,239,102,17.19094,1,17.10146,1
       |1678716024157,237,97,17.35522,1,17.06995,1
       |1678716024163,220,100,17.56914,1,16.5268,1
       |1678716024170,213,86,16.70205,1,16.89274,1
       |1678716024177,211,81,17.76707,1,16.58088,1
       |1678716024184,217,85,17.25039,1,17.37838,1
       |1678716024190,231,98,17.28481,1,16.97795,1
       |1678716024197,235,112,17.01647,1,17.18098,1
       |1678716024203,230,138,17.50633,1,17.25875,1
       |1678716024211,218,119,17.64972,1,17.02408,1
       |1678716024217,220,123,17.0501,1,17.18363,1
       |1678716024223,216,138,17.29359,1,17.25415,1
       |""".stripMargin

  private val marks = Vector(
    WorkflowSyncMark.of("trial-start", 1678716023627L, 0L).toOption.get,
    WorkflowSyncMark.of("trial-end", 1678716024223L, 596L).toOption.get
  )
  private val areas = Vector(
    WorkflowAoi.of("left", "Left result band", 200, 0, 320, 180).toOption.get,
    WorkflowAoi.of("right", "Right result band", 320, 0, 500, 180).toOption.get
  )
  private val plan = AdserpWorkflowPlan
    .of(
      sourceName = "AdSERP/p049-b4-t6/rows-231-320.csv",
      participant = "p049",
      trial = "b4-t6-excerpt",
      conditions = Vector("dataset" -> "AdSERP", "task" -> "SERP"),
      displayWidthPixels = 1280,
      displayHeightPixels = 1024,
      viewingDistanceMillimetres = 600.0,
      displayWidthMillimetres = 530.0,
      displayHeightMillimetres = 300.0,
      trackerClock = "gazepoint-unix",
      analysisClock = "trial-relative",
      synchronizationModel = SynchronizationModel.OffsetOnly,
      synchronizationMarks = marks,
      interpolationMaxGapMilliseconds = 150L,
      velocityThresholdDegreesPerSecond = 30.0,
      minimumEventDurationMilliseconds = 20L,
      areas = areas,
      sourceMetadata = Vector(
        "dataset"            -> "AdSERP",
        "zenodoRecord"       -> "15236546",
        "originalFile"       -> "pupil-data/p049-b4-t6.csv",
        "originalFileSha256" ->
          "bd5f7e02fb261e7b04415d015cfd880ab475826c630f0d98cfdac5cc27150c11",
        "license" -> "MIT",
        "doi"     -> "10.1145/3726302.3730325"
      )
    )
    .toOption
    .get

  test("a real tracker excerpt reaches auditable tidy AOI results through the simple path") {
    val result = PsychologyWorkflow
      .run(plan, publicTrial)
      .fold(error => fail(error.message), identity)
    val report   = result.report
    val document = TidyCsv.decode(result.csv).toOption.get
    def cell(row: Vector[String], name: String): String = row(TidyCsv.header.indexOf(name))
    val exportedRows                                    =
      document.rows.map(row =>
        (
          cell(row, "aoi_id"),
          cell(row, "transition_to_aoi_id"),
          cell(row, "estimand"),
          cell(row, "value")
        )
      )

    assertEquals(
      result.imported.sourceDigest.hex,
      "57dc3230c1e2a701017011a9652e215fcb09b7f1b63a16fe00ca0ed1ca9639d0"
    )
    assertEquals(report.sourceRows, 90)
    assertEquals(report.acceptedRows, 90)
    assertEquals(report.rejectedRows, 0)
    assertEquals(report.synchronizationMarks, 2)
    assertEquals(report.synchronizationRejectedMarks, 0)
    assertEquals(report.synchronizationRmsMilliseconds, 0.0)
    assertEquals(report.projectedSamples, 72)
    assertEquals(report.interpolatedSamples, 18)
    assertEquals(report.detectedEvents, 9)
    assertEquals(result.angularRecording.size, 90)
    assertEquals(result.preparedRecording.size, 90)
    assertEquals(report.result.participant, "p049")
    assertEquals(report.result.trial, "b4-t6-excerpt")
    assertEquals(report.result.areaRows, 8)
    assertEquals(report.result.transitionRows, 2)
    assertEquals(report.result.resultRows, 10)
    assertEquals(report.result.excludedMilliseconds, 0.0)
    assertEquals(report.result.unclassifiedMilliseconds, 128.0)
    assertEquals(report.result.measuredSamples, 72)
    assertEquals(report.result.derivedSamples, 90)
    assertEquals(report.result.warnings.length, 6)
    assertEquals(
      exportedRows,
      Vector(
        ("left", "", "dwell", "476500"),
        ("left", "", "dwell_proportion", "0.790215588723"),
        ("left", "", "first_entry_latency", "0"),
        ("left", "", "run_count", "3"),
        ("right", "", "dwell", "12500"),
        ("right", "", "dwell_proportion", "0.020729684909"),
        ("right", "", "first_entry_latency", "222000"),
        ("right", "", "run_count", "1"),
        ("left", "right", "transition_count", "1"),
        ("right", "left", "transition_count", "1")
      )
    )
    assertEquals(
      result.tidy.evidence.provenance.steps.map(_.operation),
      Vector(
        "import-delimited",
        "synchronize",
        "warp-visual-angle",
        "interpolate-gaps",
        "detect",
        "aoi-measure"
      )
    )
    assertEquals(result.tidy.evidence.inputIdentity.hex, result.imported.sourceDigest.hex)
    assertEquals(result.warnings, result.tidy.evidence.warnings)
    assertEquals(result.provenance, result.tidy.evidence.provenance)
    assert(
      result.tidy.evidence.provenance.render.contains("offsetMicros=-1678716023627000")
    )
    assert(
      result.tidy.evidence.detectorConfiguration.exists(
        _._1 == "velocityThresholdDegPerSecond"
      )
    )
    assert(result.tidy.evidence.frame.unitName.contains("visual angle"))
    assertEquals(result.tidy.evidence.clock.source.clock.name, "gazepoint-unix")
    assertEquals(result.tidy.evidence.clock.analysis.name, "trial-relative")
    assertEquals(
      Sha256.ofUtf8(result.csv).hex,
      "c9a2ec351a09d94533305ef2bdb7743ee4773f41a6c1d2d21c068a13927b9f76"
    )
    assertEquals(document.encode, result.csv)
    assert(result.explain.contains("Accepted 90 of 90 source rows"))
    assert(result.explain.contains("Input SHA-256"))
  }

  test("invalid plans and input failures remain named values") {
    assert(
      WorkflowSyncMark
        .of("overflow", Long.MaxValue, 0L)
        .left
        .exists(_.isInstanceOf[PsychologyWorkflowError.MillisecondsOutsideRange])
    )
    assert(
      WorkflowAoi
        .of("bad", "Bad", 10, 10, 5, 20)
        .left
        .exists(_.isInstanceOf[PsychologyWorkflowError.DegenerateAoiBounds])
    )
    assert(
      PsychologyWorkflow
        .run(plan, "timestamp,BPOGX,BPOGY,LPD,LPV,RPD,RPV\nbad,row")
        .left
        .exists(_.isInstanceOf[PsychologyWorkflowError.ImportFailed])
    )
  }
