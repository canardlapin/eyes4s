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

package eyes4s.codec

import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

/** Typed sources of the pinned v1 recording and temporal input payloads.
  *
  * The compact strings mirror src/test/resources/eyes4s/recording-input-v1.json,
  * binocular-recording-input-v1.json, study-input-source-supported-v1.json and
  * temporal-study-input-v1.json for portable JVM/JS tests; the JVM suite checks
  * the pretty-printed resource bytes.
  */
object InputPayloadFixtures:
  private def get[E, A](value: Either[E, A]): A =
    value.fold(e => throw new IllegalStateException(s"$e"), identity)

  val display: Frame[Px] = get(Frame.screen("display", 1000, 1000))
  val tracker: ClockId   = ClockId("tracker")

  private def tracked(us: Long, x: Double, y: Double, pupil: Double) =
    Sample(Instant.micros(us), Gaze.Tracked(Pt[Px](x, y), Some(pupil)))

  /** A 500 Hz recording, one timestamp quantised off the nominal period, with
    * every support category and derived lineage represented.
    */
  val monocular: Recording[Px] = get(
    Recording.of(
      display,
      tracker,
      Rate.Fixed(get(Hz(500.0))),
      Eye.Left,
      Some(PupilUnit.Arbitrary),
      IArray(
        tracked(0, 500.0, 500.0, 900.0),
        tracked(2000, 510.5, 500.0, 901.0).withSmoothedGaze(
          Gaze.Tracked(Pt[Px](510.25, 500.0), Some(901.0))
        ),
        Sample(Instant.micros(4001), Gaze.Blink[Px]()),
        Sample(Instant.micros(6000), Gaze.Lost[Px]()),
        Sample(
          Instant.micros(8000),
          Gaze.Tracked(Pt[Px](515.0, 502.0), None),
          SampleLineage.interpolated
        ),
        Sample(Instant.micros(10000), Gaze.OffScreen[Px](Pt[Px](1200.0, -5.0))),
        Sample(
          Instant.micros(12000),
          Gaze.Tracked(Pt[Px](520.0, 500.0), Some(903.0)),
          SampleLineage.interpolated.smoothed
        ),
        tracked(14000, 525.0, 505.0, 905.0)
      ),
      get(SamplingTolerance.of(Span.micros(2)))
    )
  )

  val viewing: Viewing = get(Viewing.millimetres(600, 500, 500))

  /** Three consistent marks and one outlier that the residual limit rejects;
    * the refit over the retained marks gives an offset of 1000010 us.
    */
  val synchronization: ObservedSynchronization = ObservedSynchronization(
    ClockId("display"),
    SyncFitMode.OffsetOnly,
    Vector(
      get(SyncMark.of("t1", Instant.micros(0), Instant.micros(1000000))),
      get(SyncMark.of("t2", Instant.micros(10000000), Instant.micros(11000010))),
      get(SyncMark.of("t3", Instant.micros(20000000), Instant.micros(21000020))),
      get(SyncMark.of("t4", Instant.micros(30000000), Instant.micros(31005000)))
    ),
    Some(get(SyncResidualLimit.of(Span.micros(2000))))
  )

  val input: RecordingInput[Px] = get(
    RecordingInput.of(
      RecordingRef("synthetic-left-headfixed-500"),
      RecordingChannels.Monocular(monocular),
      Some(viewing),
      Some(synchronization)
    )
  )

  /** Seeded from io/src/test/resources/eyes4s/io/eyelink/corpus/synthetic-binocular-remote-2000.asc:
    * two 2000 Hz samples with the right eye missing on the second.
    */
  val binocular: BinocularRecording[Px] = get(
    BinocularRecording.of(
      display,
      tracker,
      Rate.Fixed(get(Hz(2000.0))),
      Some(PupilUnit.Arbitrary),
      IArray(Instant.micros(300000), Instant.micros(300500)),
      IArray(
        Gaze.Tracked(Pt[Px](1.0, 2.0), Some(3.0)),
        Gaze.Tracked(Pt[Px](4.5, 5.5), Some(6.5))
      ),
      IArray(Gaze.Tracked(Pt[Px](4.0, 5.0), Some(6.0)), Gaze.Lost[Px]())
    )
  )

  val binocularInput: RecordingInput[Px] = get(
    RecordingInput.of(
      RecordingRef("synthetic-binocular-remote-2000"),
      RecordingChannels.Binocular(binocular),
      None,
      None
    )
  )

  val trialClock: ClockId = ClockId("trial")

  /** A 1 kHz recording whose two fixations are backed by exact sample ranges;
    * the blink inside the first range is excluded from its summary.
    */
  val sourceRecording: Recording[Px] = get(
    Recording.of(
      display,
      trialClock,
      Rate.Fixed(get(Hz(1000.0))),
      Eye.Right,
      None,
      IArray.tabulate(10) { i =>
        val gaze =
          if i == 2 then Gaze.Blink[Px]()
          else if i < 5 then Gaze.Tracked(Pt[Px](100.0 + i, 100.0), None)
          else Gaze.Tracked(Pt[Px](300.0, 100.0 + (i - 5)), None)
        Sample(Instant.millis(i.toLong), gaze)
      }
    )
  )

  private def interval(clock: ClockId, from: Long, until: Long): Interval =
    get(Interval.of(clock, Instant.micros(from), Instant.micros(until)))

  val sourceSupportedScanpath: Scanpath[Px] =
    val first = get(
      Event.Fixation.of(
        interval(trialClock, 0, 5000),
        Pt[Px](0.0, 0.0),
        0.0,
        DispersionMethod.RmsRadius,
        1
      )
    )
    val second = get(
      Event.Fixation.withoutDispersion(interval(trialClock, 5000, 10000), Pt[Px](0.0, 0.0), 1)
    )
    val series = get(
      EventSeries.of(
        sourceRecording,
        RecordingRef("synthetic-right-headfixed-1000"),
        Vector(first, second),
        Vector(get(SampleRange.of(0, 5)), get(SampleRange.of(5, 10)))
      )
    )
    get(Scanpath.fromEvents(series))

  val sourceSupportedStudy: StudyInput[StudyKey, Px] =
    StudyInput(
      Trials(Vector(Trial(StudyKey("s1", "a", "encode"), (), sourceSupportedScanpath)))
    )

  // Temporal payload derived from TemporalFixtures: the ordinary trials, one
  // trial shifted by an anchor beyond JavaScript's exact integer range, and
  // one trial deliberately left without an epoch.
  val temporalFrame: Frame[Px]  = get(Frame.screen("temporal", 2, 2))
  val shiftedKey: StudyKey      = StudyKey("s1", "a", "encode")
  val shift: Long               = 9007199254740993L
  val missingEpochKey: StudyKey = StudyKey("s1", "a", "retest")

  def temporalClock(k: StudyKey): ClockId =
    ClockId(s"fixation-trial:${KeyDigest[StudyKey].digest(k).render}")

  private def offset(k: StudyKey): Long = if k == shiftedKey then shift else 0L

  val temporalStudy: StudyInput[StudyKey, Px] =
    val trials = TemporalFixtures.csv.linesIterator
      .drop(1)
      .map(_.split(",").toVector)
      .toVector
      .groupBy(row => StudyKey(row(0), row(1), row(2)))
      .toVector
      .sortBy(_._1)
      .map { case (k, rows) =>
        val fixes = rows
          .sortBy(_(3).toInt)
          .map(row =>
            get(
              Event.Fixation.withoutDispersion(
                interval(
                  temporalClock(k),
                  row(6).toLong + offset(k),
                  row(6).toLong + row(7).toLong + offset(k)
                ),
                Pt[Px](row(4).toDouble, row(5).toDouble),
                row(8).toInt
              )
            )
          )
        Trial(k, (), get(Scanpath.of(temporalFrame, temporalClock(k), IArray.from(fixes))))
      }
    StudyInput(Trials(trials))

  def temporalEpochs(complete: Boolean): Vector[(StudyKey, TrialEpoch)] =
    TemporalFixtures.coverage.toVector
      .map { case (name, spans) =>
        val pieces = name.split("/")
        val k      = StudyKey(pieces(0), pieces(1), pieces(2))
        k -> TrialEpoch(
          Instant.micros(offset(k)),
          get(
            ObservedCoverage.of(
              temporalClock(k),
              spans.map { case (a, b) =>
                interval(temporalClock(k), a + offset(k), b + offset(k))
              }
            )
          )
        )
      }
      .filter { case (k, _) => complete || k != missingEpochKey }
      .sortBy(_._1)

  val temporal: TemporalStudyInput[StudyKey, Px] =
    get(TemporalStudyInput.of(temporalStudy, temporalEpochs(complete = false)))

  val temporalComplete: TemporalStudyInput[StudyKey, Px] =
    get(TemporalStudyInput.of(temporalStudy, temporalEpochs(complete = true)))

  val recordingInputVersionOne: String =
    """{"schema":{"name":"eyes4s.recording-input","version":1},"value":{"unit":"px","source":"synthetic-left-headfixed-500","input":"d4c0d76ef6fd5169","identities":{"frames":[{"id":"display","unit":"px","xMin":0.0,"yMin":0.0,"xMax":1000.0,"yMax":1000.0,"yAxis":"Down"}],"grids":[],"clocks":["tracker","display"]},"channels":{"kind":"monocular","recording":{"frame":"display","clock":"tracker","eye":"left","pupilUnit":"arbitrary","rate":{"kind":"fixed","hz":500.0},"samplingToleranceMicros":"2","recording":"2c826dc41ae25e67","samples":{"length":8,"tMicros":["0","2000","4001","6000","8000","10000","12000","14000"],"state":["tracked","tracked","blink","lost","tracked","offScreen","tracked","tracked"],"x":[500.0,510.25,null,null,515.0,1200.0,520.0,525.0],"y":[500.0,500.0,null,null,502.0,-5.0,500.0,505.0],"pupil":[900.0,901.0,null,null,null,null,903.0,905.0],"lineage":["measured","measured>smoothed","measured","measured","interpolated","measured","interpolated>smoothed","measured"]}}},"viewing":{"distanceMm":600.0,"widthMm":500.0,"heightMm":500.0},"synchronization":{"target":"display","mode":"offset-only","marks":[{"id":"t1","sourceMicros":"0","targetMicros":"1000000"},{"id":"t2","sourceMicros":"10000000","targetMicros":"11000010"},{"id":"t3","sourceMicros":"20000000","targetMicros":"21000020"},{"id":"t4","sourceMicros":"30000000","targetMicros":"31005000"}],"residualLimitMicros":"2000","fitted":{"offsetMicros":"1000010","drift":0.0}}}}"""

  val binocularInputVersionOne: String =
    """{"schema":{"name":"eyes4s.recording-input","version":1},"value":{"unit":"px","source":"synthetic-binocular-remote-2000","input":"c5e4009b9cb153b0","identities":{"frames":[{"id":"display","unit":"px","xMin":0.0,"yMin":0.0,"xMax":1000.0,"yMax":1000.0,"yAxis":"Down"}],"grids":[],"clocks":["tracker"]},"channels":{"kind":"binocular","recording":{"frame":"display","clock":"tracker","pupilUnit":"arbitrary","rate":{"kind":"fixed","hz":2000.0},"samplingToleranceMicros":"1","recording":"39e3dafb2de70ca2","samples":{"length":2,"tMicros":["300000","300500"],"left":{"state":["tracked","tracked"],"x":[1.0,4.5],"y":[2.0,5.5],"pupil":[3.0,6.5]},"right":{"state":["tracked","lost"],"x":[4.0,null],"y":[5.0,null],"pupil":[6.0,null]}}}},"viewing":null,"synchronization":null}}"""

  val sourceSupportedVersionOne: String =
    """{"schema":{"name":"eyes4s.study-input","version":1},"value":{"layout":{"name":"eyes4s.participant-stimulus-phase","version":1},"keySchema":{"name":"eyes4s.study-key","version":1},"unit":"px","input":"f4f250e30583ceaa","identities":{"frames":[{"id":"display","unit":"px","xMin":0.0,"yMin":0.0,"xMax":1000.0,"yMax":1000.0,"yAxis":"Down"}],"grids":[],"clocks":["trial"]},"trials":{"schema":{"name":"eyes4s.trials","version":1},"value":[{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"a","phase":"encode"}},"meta":{"schema":{"name":"eyes4s.unit","version":1},"value":{}},"value":{"schema":{"name":"eyes4s.scanpath","version":1},"value":{"frame":"display","clock":"trial","fixations":[{"onsetMicros":"0","offsetMicros":"5000","x":102.0,"y":100.0,"sampleCount":4,"dispersion":{"value":1.5811388300841898,"method":"rmsRadius"}},{"onsetMicros":"5000","offsetMicros":"10000","x":300.0,"y":102.0,"sampleCount":5}],"source":{"ref":"synthetic-right-headfixed-1000","recording":{"frame":"display","clock":"trial","eye":"right","pupilUnit":null,"rate":{"kind":"fixed","hz":1000.0},"samplingToleranceMicros":"1","recording":"e466fbcfe9b3547a","samples":{"length":10,"tMicros":["0","1000","2000","3000","4000","5000","6000","7000","8000","9000"],"state":["tracked","tracked","blink","tracked","tracked","tracked","tracked","tracked","tracked","tracked"],"x":[100.0,101.0,null,103.0,104.0,300.0,300.0,300.0,300.0,300.0],"y":[100.0,100.0,null,100.0,100.0,100.0,101.0,102.0,103.0,104.0],"pupil":[null,null,null,null,null,null,null,null,null,null],"lineage":["measured","measured","measured","measured","measured","measured","measured","measured","measured","measured"]}},"support":[{"from":0,"until":5},{"from":5,"until":10}]}}}}]}}}"""

  val temporalInputVersionOne: String =
    """{"schema":{"name":"eyes4s.temporal-study-input","version":1},"value":{"layout":{"name":"eyes4s.participant-stimulus-phase","version":1},"keySchema":{"name":"eyes4s.study-key","version":1},"unit":"px","input":"def40de68e529acf","study":{"kind":"inline","payload":{"schema":{"name":"eyes4s.study-input","version":1},"value":{"layout":{"name":"eyes4s.participant-stimulus-phase","version":1},"keySchema":{"name":"eyes4s.study-key","version":1},"unit":"px","input":"4fd98f50693c79ad","identities":{"frames":[{"id":"temporal","unit":"px","xMin":0.0,"yMin":0.0,"xMax":2.0,"yMax":2.0,"yAxis":"Down"}],"grids":[],"clocks":["fixation-trial:e5ee90ac5de274ac","fixation-trial:1966b114a051e430","fixation-trial:92f11d0c26531591","fixation-trial:71cf0b836f87958f","fixation-trial:511af6d534d2d177","fixation-trial:42861ebde1bc774f","fixation-trial:e2db26ec48c12617","fixation-trial:1d19da57590cdd9b","fixation-trial:d9df8e0cff0e467f","fixation-trial:b2efc1a803f87179","fixation-trial:fc8771ee29333db9","fixation-trial:fe37c91e9c531fa6","fixation-trial:728faa6c51a28c62","fixation-trial:0871979179efa5a0","fixation-trial:264e5a9fc941f997","fixation-trial:2e1a9a5dfe8d8931","fixation-trial:14a0d777804fe58f","fixation-trial:f0ba02fb12e9cebd"]},"trials":{"schema":{"name":"eyes4s.trials","version":1},"value":[{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"a","phase":"encode"}},"meta":{"schema":{"name":"eyes4s.unit","version":1},"value":{}},"value":{"schema":{"name":"eyes4s.scanpath","version":1},"value":{"frame":"temporal","clock":"fixation-trial:e5ee90ac5de274ac","fixations":[{"onsetMicros":"9007199254740993","offsetMicros":"9007199254840993","x":0.5,"y":0.5,"sampleCount":100},{"onsetMicros":"9007199254850993","offsetMicros":"9007199255050993","x":1.5,"y":0.5,"sampleCount":200},{"onsetMicros":"9007199255060993","offsetMicros":"9007199255260993","x":0.5,"y":1.5,"sampleCount":200},{"onsetMicros":"9007199255270993","offsetMicros":"9007199255670993","x":1.5,"y":1.5,"sampleCount":400}]}}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"a","phase":"recall"}},"meta":{"schema":{"name":"eyes4s.unit","version":1},"value":{}},"value":{"schema":{"name":"eyes4s.scanpath","version":1},"value":{"frame":"temporal","clock":"fixation-trial:1966b114a051e430","fixations":[{"onsetMicros":"0","offsetMicros":"100000","x":0.5,"y":0.5,"sampleCount":100},{"onsetMicros":"110000","offsetMicros":"310000","x":1.5,"y":0.5,"sampleCount":200},{"onsetMicros":"320000","offsetMicros":"520000","x":0.5,"y":1.5,"sampleCount":200},{"onsetMicros":"530000","offsetMicros":"930000","x":1.5,"y":1.5,"sampleCount":400}]}}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"a","phase":"retest"}},"meta":{"schema":{"name":"eyes4s.unit","version":1},"value":{}},"value":{"schema":{"name":"eyes4s.scanpath","version":1},"value":{"frame":"temporal","clock":"fixation-trial:92f11d0c26531591","fixations":[{"onsetMicros":"0","offsetMicros":"100000","x":1.5,"y":0.5,"sampleCount":100},{"onsetMicros":"110000","offsetMicros":"310000","x":1.5,"y":1.5,"sampleCount":200},{"onsetMicros":"320000","offsetMicros":"520000","x":0.5,"y":0.5,"sampleCount":200},{"onsetMicros":"530000","offsetMicros":"930000","x":0.5,"y":1.5,"sampleCount":400}]}}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"b","phase":"encode"}},"meta":{"schema":{"name":"eyes4s.unit","version":1},"value":{}},"value":{"schema":{"name":"eyes4s.scanpath","version":1},"value":{"frame":"temporal","clock":"fixation-trial:71cf0b836f87958f","fixations":[{"onsetMicros":"0","offsetMicros":"400000","x":0.5,"y":0.5,"sampleCount":400},{"onsetMicros":"410000","offsetMicros":"610000","x":1.5,"y":0.5,"sampleCount":200},{"onsetMicros":"620000","offsetMicros":"820000","x":0.5,"y":1.5,"sampleCount":200},{"onsetMicros":"830000","offsetMicros":"930000","x":1.5,"y":1.5,"sampleCount":100}]}}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"b","phase":"recall"}},"meta":{"schema":{"name":"eyes4s.unit","version":1},"value":{}},"value":{"schema":{"name":"eyes4s.scanpath","version":1},"value":{"frame":"temporal","clock":"fixation-trial:511af6d534d2d177","fixations":[{"onsetMicros":"0","offsetMicros":"200000","x":0.5,"y":0.5,"sampleCount":200},{"onsetMicros":"210000","offsetMicros":"610000","x":1.5,"y":0.5,"sampleCount":400},{"onsetMicros":"620000","offsetMicros":"720000","x":0.5,"y":1.5,"sampleCount":100},{"onsetMicros":"730000","offsetMicros":"930000","x":1.5,"y":1.5,"sampleCount":200}]}}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"b","phase":"retest"}},"meta":{"schema":{"name":"eyes4s.unit","version":1},"value":{}},"value":{"schema":{"name":"eyes4s.scanpath","version":1},"value":{"frame":"temporal","clock":"fixation-trial:42861ebde1bc774f","fixations":[{"onsetMicros":"0","offsetMicros":"200000","x":1.5,"y":0.5,"sampleCount":200},{"onsetMicros":"210000","offsetMicros":"610000","x":1.5,"y":1.5,"sampleCount":400},{"onsetMicros":"620000","offsetMicros":"720000","x":0.5,"y":0.5,"sampleCount":100},{"onsetMicros":"730000","offsetMicros":"930000","x":0.5,"y":1.5,"sampleCount":200}]}}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"c","phase":"encode"}},"meta":{"schema":{"name":"eyes4s.unit","version":1},"value":{}},"value":{"schema":{"name":"eyes4s.scanpath","version":1},"value":{"frame":"temporal","clock":"fixation-trial:e2db26ec48c12617","fixations":[{"onsetMicros":"0","offsetMicros":"200000","x":0.5,"y":0.5,"sampleCount":200},{"onsetMicros":"210000","offsetMicros":"610000","x":1.5,"y":0.5,"sampleCount":400},{"onsetMicros":"620000","offsetMicros":"720000","x":0.5,"y":1.5,"sampleCount":100},{"onsetMicros":"730000","offsetMicros":"930000","x":1.5,"y":1.5,"sampleCount":200}]}}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"c","phase":"recall"}},"meta":{"schema":{"name":"eyes4s.unit","version":1},"value":{}},"value":{"schema":{"name":"eyes4s.scanpath","version":1},"value":{"frame":"temporal","clock":"fixation-trial:1d19da57590cdd9b","fixations":[{"onsetMicros":"0","offsetMicros":"400000","x":0.5,"y":0.5,"sampleCount":400},{"onsetMicros":"410000","offsetMicros":"610000","x":1.5,"y":0.5,"sampleCount":200},{"onsetMicros":"620000","offsetMicros":"820000","x":0.5,"y":1.5,"sampleCount":200},{"onsetMicros":"830000","offsetMicros":"930000","x":1.5,"y":1.5,"sampleCount":100}]}}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"c","phase":"retest"}},"meta":{"schema":{"name":"eyes4s.unit","version":1},"value":{}},"value":{"schema":{"name":"eyes4s.scanpath","version":1},"value":{"frame":"temporal","clock":"fixation-trial:d9df8e0cff0e467f","fixations":[{"onsetMicros":"0","offsetMicros":"400000","x":1.5,"y":0.5,"sampleCount":400},{"onsetMicros":"410000","offsetMicros":"610000","x":1.5,"y":1.5,"sampleCount":200},{"onsetMicros":"620000","offsetMicros":"820000","x":0.5,"y":0.5,"sampleCount":200},{"onsetMicros":"830000","offsetMicros":"930000","x":0.5,"y":1.5,"sampleCount":100}]}}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"a","phase":"encode"}},"meta":{"schema":{"name":"eyes4s.unit","version":1},"value":{}},"value":{"schema":{"name":"eyes4s.scanpath","version":1},"value":{"frame":"temporal","clock":"fixation-trial:b2efc1a803f87179","fixations":[{"onsetMicros":"0","offsetMicros":"200000","x":0.5,"y":0.5,"sampleCount":200},{"onsetMicros":"210000","offsetMicros":"310000","x":1.5,"y":0.5,"sampleCount":100},{"onsetMicros":"320000","offsetMicros":"720000","x":0.5,"y":1.5,"sampleCount":400},{"onsetMicros":"730000","offsetMicros":"930000","x":1.5,"y":1.5,"sampleCount":200}]}}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"a","phase":"recall"}},"meta":{"schema":{"name":"eyes4s.unit","version":1},"value":{}},"value":{"schema":{"name":"eyes4s.scanpath","version":1},"value":{"frame":"temporal","clock":"fixation-trial:fc8771ee29333db9","fixations":[{"onsetMicros":"0","offsetMicros":"400000","x":0.5,"y":0.5,"sampleCount":400},{"onsetMicros":"410000","offsetMicros":"510000","x":1.5,"y":0.5,"sampleCount":100},{"onsetMicros":"520000","offsetMicros":"720000","x":0.5,"y":1.5,"sampleCount":200},{"onsetMicros":"730000","offsetMicros":"930000","x":1.5,"y":1.5,"sampleCount":200}]}}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"a","phase":"retest"}},"meta":{"schema":{"name":"eyes4s.unit","version":1},"value":{}},"value":{"schema":{"name":"eyes4s.scanpath","version":1},"value":{"frame":"temporal","clock":"fixation-trial:fe37c91e9c531fa6","fixations":[{"onsetMicros":"0","offsetMicros":"400000","x":1.5,"y":0.5,"sampleCount":400},{"onsetMicros":"410000","offsetMicros":"510000","x":1.5,"y":1.5,"sampleCount":100},{"onsetMicros":"520000","offsetMicros":"720000","x":0.5,"y":0.5,"sampleCount":200},{"onsetMicros":"730000","offsetMicros":"930000","x":0.5,"y":1.5,"sampleCount":200}]}}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"b","phase":"encode"}},"meta":{"schema":{"name":"eyes4s.unit","version":1},"value":{}},"value":{"schema":{"name":"eyes4s.scanpath","version":1},"value":{"frame":"temporal","clock":"fixation-trial:728faa6c51a28c62","fixations":[{"onsetMicros":"0","offsetMicros":"100000","x":0.5,"y":0.5,"sampleCount":100},{"onsetMicros":"110000","offsetMicros":"510000","x":1.5,"y":0.5,"sampleCount":400},{"onsetMicros":"520000","offsetMicros":"720000","x":0.5,"y":1.5,"sampleCount":200},{"onsetMicros":"730000","offsetMicros":"930000","x":1.5,"y":1.5,"sampleCount":200}]}}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"b","phase":"recall"}},"meta":{"schema":{"name":"eyes4s.unit","version":1},"value":{}},"value":{"schema":{"name":"eyes4s.scanpath","version":1},"value":{"frame":"temporal","clock":"fixation-trial:0871979179efa5a0","fixations":[{"onsetMicros":"0","offsetMicros":"100000","x":0.5,"y":0.5,"sampleCount":100},{"onsetMicros":"110000","offsetMicros":"510000","x":1.5,"y":0.5,"sampleCount":400},{"onsetMicros":"520000","offsetMicros":"720000","x":0.5,"y":1.5,"sampleCount":200},{"onsetMicros":"730000","offsetMicros":"930000","x":1.5,"y":1.5,"sampleCount":200}]}}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"b","phase":"retest"}},"meta":{"schema":{"name":"eyes4s.unit","version":1},"value":{}},"value":{"schema":{"name":"eyes4s.scanpath","version":1},"value":{"frame":"temporal","clock":"fixation-trial:264e5a9fc941f997","fixations":[{"onsetMicros":"0","offsetMicros":"100000","x":1.5,"y":0.5,"sampleCount":100},{"onsetMicros":"110000","offsetMicros":"510000","x":1.5,"y":1.5,"sampleCount":400},{"onsetMicros":"520000","offsetMicros":"720000","x":0.5,"y":0.5,"sampleCount":200},{"onsetMicros":"730000","offsetMicros":"930000","x":0.5,"y":1.5,"sampleCount":200}]}}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"c","phase":"encode"}},"meta":{"schema":{"name":"eyes4s.unit","version":1},"value":{}},"value":{"schema":{"name":"eyes4s.scanpath","version":1},"value":{"frame":"temporal","clock":"fixation-trial:2e1a9a5dfe8d8931","fixations":[{"onsetMicros":"0","offsetMicros":"400000","x":0.5,"y":0.5,"sampleCount":400},{"onsetMicros":"410000","offsetMicros":"510000","x":1.5,"y":0.5,"sampleCount":100},{"onsetMicros":"520000","offsetMicros":"720000","x":0.5,"y":1.5,"sampleCount":200},{"onsetMicros":"730000","offsetMicros":"930000","x":1.5,"y":1.5,"sampleCount":200}]}}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"c","phase":"recall"}},"meta":{"schema":{"name":"eyes4s.unit","version":1},"value":{}},"value":{"schema":{"name":"eyes4s.scanpath","version":1},"value":{"frame":"temporal","clock":"fixation-trial:14a0d777804fe58f","fixations":[{"onsetMicros":"0","offsetMicros":"200000","x":0.5,"y":0.5,"sampleCount":200},{"onsetMicros":"210000","offsetMicros":"310000","x":1.5,"y":0.5,"sampleCount":100},{"onsetMicros":"320000","offsetMicros":"720000","x":0.5,"y":1.5,"sampleCount":400},{"onsetMicros":"730000","offsetMicros":"930000","x":1.5,"y":1.5,"sampleCount":200}]}}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"c","phase":"retest"}},"meta":{"schema":{"name":"eyes4s.unit","version":1},"value":{}},"value":{"schema":{"name":"eyes4s.scanpath","version":1},"value":{"frame":"temporal","clock":"fixation-trial:f0ba02fb12e9cebd","fixations":[{"onsetMicros":"0","offsetMicros":"200000","x":1.5,"y":0.5,"sampleCount":200},{"onsetMicros":"210000","offsetMicros":"310000","x":1.5,"y":1.5,"sampleCount":100},{"onsetMicros":"320000","offsetMicros":"720000","x":0.5,"y":0.5,"sampleCount":400},{"onsetMicros":"730000","offsetMicros":"930000","x":0.5,"y":1.5,"sampleCount":200}]}}}]}}}},"identities":{"frames":[],"grids":[],"clocks":["fixation-trial:e5ee90ac5de274ac","fixation-trial:1966b114a051e430","fixation-trial:71cf0b836f87958f","fixation-trial:511af6d534d2d177","fixation-trial:42861ebde1bc774f","fixation-trial:e2db26ec48c12617","fixation-trial:1d19da57590cdd9b","fixation-trial:d9df8e0cff0e467f","fixation-trial:b2efc1a803f87179","fixation-trial:fc8771ee29333db9","fixation-trial:fe37c91e9c531fa6","fixation-trial:728faa6c51a28c62","fixation-trial:0871979179efa5a0","fixation-trial:264e5a9fc941f997","fixation-trial:2e1a9a5dfe8d8931","fixation-trial:14a0d777804fe58f","fixation-trial:f0ba02fb12e9cebd"]},"epochs":[{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"a","phase":"encode"}},"anchorMicros":"9007199254740993","coverage":{"clock":"fixation-trial:e5ee90ac5de274ac","intervals":[{"clock":"fixation-trial:e5ee90ac5de274ac","onsetMicros":"9007199254740993","offsetMicros":"9007199255670993"}]}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"a","phase":"recall"}},"anchorMicros":"0","coverage":{"clock":"fixation-trial:1966b114a051e430","intervals":[{"clock":"fixation-trial:1966b114a051e430","onsetMicros":"0","offsetMicros":"930000"}]}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"b","phase":"encode"}},"anchorMicros":"0","coverage":{"clock":"fixation-trial:71cf0b836f87958f","intervals":[{"clock":"fixation-trial:71cf0b836f87958f","onsetMicros":"0","offsetMicros":"930000"}]}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"b","phase":"recall"}},"anchorMicros":"0","coverage":{"clock":"fixation-trial:511af6d534d2d177","intervals":[{"clock":"fixation-trial:511af6d534d2d177","onsetMicros":"0","offsetMicros":"930000"}]}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"b","phase":"retest"}},"anchorMicros":"0","coverage":{"clock":"fixation-trial:42861ebde1bc774f","intervals":[{"clock":"fixation-trial:42861ebde1bc774f","onsetMicros":"0","offsetMicros":"930000"}]}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"c","phase":"encode"}},"anchorMicros":"0","coverage":{"clock":"fixation-trial:e2db26ec48c12617","intervals":[{"clock":"fixation-trial:e2db26ec48c12617","onsetMicros":"0","offsetMicros":"930000"}]}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"c","phase":"recall"}},"anchorMicros":"0","coverage":{"clock":"fixation-trial:1d19da57590cdd9b","intervals":[{"clock":"fixation-trial:1d19da57590cdd9b","onsetMicros":"0","offsetMicros":"930000"}]}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s1","stimulus":"c","phase":"retest"}},"anchorMicros":"0","coverage":{"clock":"fixation-trial:d9df8e0cff0e467f","intervals":[{"clock":"fixation-trial:d9df8e0cff0e467f","onsetMicros":"0","offsetMicros":"930000"}]}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"a","phase":"encode"}},"anchorMicros":"0","coverage":{"clock":"fixation-trial:b2efc1a803f87179","intervals":[{"clock":"fixation-trial:b2efc1a803f87179","onsetMicros":"0","offsetMicros":"930000"}]}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"a","phase":"recall"}},"anchorMicros":"0","coverage":{"clock":"fixation-trial:fc8771ee29333db9","intervals":[{"clock":"fixation-trial:fc8771ee29333db9","onsetMicros":"0","offsetMicros":"930000"}]}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"a","phase":"retest"}},"anchorMicros":"0","coverage":{"clock":"fixation-trial:fe37c91e9c531fa6","intervals":[{"clock":"fixation-trial:fe37c91e9c531fa6","onsetMicros":"0","offsetMicros":"930000"}]}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"b","phase":"encode"}},"anchorMicros":"0","coverage":{"clock":"fixation-trial:728faa6c51a28c62","intervals":[{"clock":"fixation-trial:728faa6c51a28c62","onsetMicros":"0","offsetMicros":"930000"}]}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"b","phase":"recall"}},"anchorMicros":"0","coverage":{"clock":"fixation-trial:0871979179efa5a0","intervals":[{"clock":"fixation-trial:0871979179efa5a0","onsetMicros":"0","offsetMicros":"930000"}]}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"b","phase":"retest"}},"anchorMicros":"0","coverage":{"clock":"fixation-trial:264e5a9fc941f997","intervals":[{"clock":"fixation-trial:264e5a9fc941f997","onsetMicros":"0","offsetMicros":"350000"},{"clock":"fixation-trial:264e5a9fc941f997","onsetMicros":"500000","offsetMicros":"930000"}]}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"c","phase":"encode"}},"anchorMicros":"0","coverage":{"clock":"fixation-trial:2e1a9a5dfe8d8931","intervals":[{"clock":"fixation-trial:2e1a9a5dfe8d8931","onsetMicros":"0","offsetMicros":"930000"}]}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"c","phase":"recall"}},"anchorMicros":"0","coverage":{"clock":"fixation-trial:14a0d777804fe58f","intervals":[{"clock":"fixation-trial:14a0d777804fe58f","onsetMicros":"0","offsetMicros":"930000"}]}},{"key":{"schema":{"name":"eyes4s.study-key","version":1},"value":{"participant":"s2","stimulus":"c","phase":"retest"}},"anchorMicros":"0","coverage":{"clock":"fixation-trial:f0ba02fb12e9cebd","intervals":[{"clock":"fixation-trial:f0ba02fb12e9cebd","onsetMicros":"0","offsetMicros":"930000"}]}}]}}"""
