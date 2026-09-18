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

package example

import eyes4s.compare.ComparisonQuantum
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

/** The recording journey's input and its independent oracle.
  *
  * The samples are the pinned I-VT conformance fixture
  * `ivt-symmetric-central-boundaries` of
  * `tools/detector-conformance/reference.json`: ten samples at 1 kHz whose
  * angular positions are 0, 0, 0, 0, 5, 10, 10, 10, 10 and 10 degrees, with a
  * 2000 deg/s threshold and a 2 ms minimum. Its two fixations are the
  * pymovements 0.26.2 oracle's, mapped to half-open support; the saccade
  * between them is the fixture's stated eyes4s expectation. Here they
  * are display pixels on a 1000 x 1000 display, 1 mm per pixel, viewed from
  * 600 mm: `x = 500 + 600 tan(theta)`. The pixel values are decimal literals,
  * so the recording and its digest are identical on the JVM and Scala.js;
  * only the angular warp is trigonometric.
  */
object RecordingFixtures:
  import JourneySetup.get

  val display: Frame[Px]    = get(Frame.screen("recording-display", 1000, 1000))
  val viewing: Viewing      = get(Viewing.millimetres(600.0, 1000.0, 1000.0))
  val trackerClock: ClockId = ClockId("my.lab.tracker")
  val analysisClock         = ClockId("my.lab.experiment")
  val source: RecordingRef  = RecordingRef("my.lab.session-01")
  val angular: FrameId      = FrameId("recording-angular")

  /** Degrees of the oracle, and the pixel literals `500 + 600 tan(theta)`. */
  val degrees: Vector[Double] = Vector(0.0, 0.0, 0.0, 0.0, 5.0, 10.0, 10.0, 10.0, 10.0, 10.0)
  private val pixels = Map(0.0 -> 500.0, 5.0 -> 552.4931981155544, 10.0 -> 605.796188425079)

  val recording: Recording[Px] = get(
    Recording.of(
      display,
      trackerClock,
      Rate.Fixed(get(Hz(1000.0))),
      Eye.Left,
      None,
      IArray.from(degrees.zipWithIndex.map { (theta, i) =>
        Sample(Instant.millis(i.toLong), Gaze.Tracked(Pt[Px](pixels(theta), 500.0), None))
      })
    )
  )

  val marks: Vector[SyncMark] = Vector(
    get(SyncMark.of("start", Instant.millis(0), Instant.millis(0))),
    get(SyncMark.of("end", Instant.millis(9), Instant.millis(9)))
  )

  val synchronization: ObservedSynchronization =
    ObservedSynchronization(analysisClock, SyncFitMode.OffsetOnly, marks, None)

  /** The normalized input an importer would produce: channels, bench geometry and marks. */
  val input: RecordingInput[Px] = get(
    RecordingInput.of(
      source,
      RecordingChannels.Monocular(recording),
      Some(viewing),
      Some(synchronization)
    )
  )

  /** Two areas split at x = 550 px: the first fixation is left, the rest right. */
  val areas: Vector[RecordingArea] = Vector(
    get(
      RecipeParameters.area.parse(
        ("left", "Left half", get(RecipeParameters.bounds[Px].parse((0.0, 0.0, 550.0, 1000.0))))
      )
    ),
    get(
      RecipeParameters.area.parse(
        (
          "right",
          "Right half",
          get(RecipeParameters.bounds[Px].parse((550.0, 0.0, 1000.0, 1000.0)))
        )
      )
    )
  )

  val gap: InterpolationGap = get(RecipeParameters.interpolationGap.parse(Span.micros(0)))

  val thresholdDegPerSecond = 2000.0
  val minimumMicros         = 2000L

  /** The shipped I-VT, parameters through its typed recipe descriptors. */
  val ivt: RecordingRoute[IvtParameters] = get(
    RecordingRoute.ivt(
      get(
        RecipeParameters.ivtThreshold.parse(
          get(Velocity.perSecond[Unit2D.Deg](thresholdDegPerSecond))
        )
      ),
      get(RecipeParameters.minimumDuration.parse(Span.micros(minimumMicros)))
    )
  )

  /** The laboratory detector, parameters through its own smart constructors. */
  val lab: RecordingRoute[LabIvtParameters] = get(
    RecordingRoute.lab(get(LabIvtParameters.of(thresholdDegPerSecond, minimumMicros)))
  )

  /** Two samples per chunk: five interpolation and five detection steps. */
  val quanta: WorkQuanta =
    WorkQuanta(PairQuantum.default, ComparisonQuantum.default, get(SampleQuantum.of(2)))

  /** The oracle's events in eyes4s' half-open convention (its inclusive
    * millisecond offsets plus one period), in microseconds: kind, onset,
    * exclusive offset, and the fixation centre or the saccade's start and end
    * in degrees.
    */
  enum Expected derives CanEqual:
    case Fixation(onsetMicros: Long, offsetMicros: Long, x: Double, y: Double)
    case Saccade(onsetMicros: Long, offsetMicros: Long, fromX: Double, toX: Double)

  val expected: Vector[Expected] = Vector(
    Expected.Fixation(0L, 3000L, 0.0, 0.0),
    Expected.Saccade(3000L, 6000L, 0.0, 10.0),
    Expected.Fixation(6000L, 10000L, 10.0, 0.0)
  )

  /** The sample support of each expected event, from its milliseconds at 1 kHz. */
  val support: Vector[(Int, Int)] = Vector((0, 3), (3, 6), (6, 10))

  /** Each sample's area after the exclusive assignment: the split is at 550 px. */
  val areasBySample: Vector[String] =
    degrees.map(theta => if theta == 0.0 then "left" else "right")
