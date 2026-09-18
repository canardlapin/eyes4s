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

import eyes4s.compare.Similarity
import eyes4s.core.{FixationBoundary, Weight}
import eyes4s.design.{FailurePolicy, SignedDifference}
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*
import eyes4s.surface.EdgePolicy

/** The shipped plan codecs whose schema identity the caller supplies, under
  * the conventional identities their pinned fixtures use:
  * `eyes4s.recording-plan@1` (recording-v1.json, an I-VT plan) and
  * `eyes4s.temporal-study@1` (temporal-study-v1.json, the temporal fixture's
  * plan over both repetitions, every window and the binned and Gaussian
  * scales). The compact strings mirror the resource files for portable
  * JVM/JS tests; the JVM suites check the pretty-printed resource bytes.
  */
object ConventionalPlanFixtures:
  private def get[E, A](value: Either[E, A]): A =
    value.fold(e => throw new IllegalStateException(s"$e"), identity)
  private def id(name: String): DefinitionId = get(DefinitionId.of(name, 1))

  val recordingCodec: RecordingPlanCodec[IvtParameters] = RecordingCodecs.ivt(
    id("eyes4s.recording-plan"),
    id("eyes4s.recording.ivt"),
    id("eyes4s.ivt-parameters")
  )

  val temporalCodec: TemporalStudyCodec[StudyKey, Px, Unit, Similarity, SignedDifference] =
    new TemporalStudyCodec(id("eyes4s.temporal-study"), StudyCodecs.cosine[Px])

  /** The temporal plan over the pinned temporal input, with its missing epoch
    * and its anchor beyond JavaScript's exact integer range.
    */
  lazy val temporalPlan: TemporalStudyPlan[StudyKey, Px, Unit, Similarity, SignedDifference] =
    val input = InputPayloadFixtures.temporal
    val base  = get(
      StudyPlan.cosine(
        input.study.reference,
        get(Grid.over(InputPayloadFixtures.temporalFrame, 2, 2)),
        "recall",
        "encode",
        Weight.Duration,
        Vector(
          StudyEstimate.Binned(),
          StudyEstimate.Gaussian(get(Sigma.px(1)), EdgePolicy.Truncate)
        ),
        FailurePolicy.RequireAll
      )
    )
    val windows = TemporalFixtures.windows.map { (name, from, until) =>
      get(StudyWindow.of(name, get(Window.of(Span.micros(from), Span.micros(until)))))
    }
    val repeats = TemporalFixtures.repetitions.map { (name, focal, reference) =>
      get(RepetitionContrast.withinParticipant(name, focal, reference))
    }
    get(
      TemporalStudyPlan.of(
        base,
        input.reference,
        windows,
        repeats,
        FixationBoundary.ClipDuration
      )
    )

  // Mirrors resources/eyes4s/recording-v1.json.
  val recordingPlanVersionOne: String =
    """{"schema":{"name":"eyes4s.recording-plan","version":1},"value":{"method":{"name":"eyes4s.recording.ivt","version":1},"parameters":{"schema":{"name":"eyes4s.ivt-parameters","version":1},"value":{"threshold":30,"minimumMicros":"60000"}},"input":"0123456789abcdef","source":"recording-v1-oracle","display":{"id":"screen-v1","unit":"px","xMin":0,"yMin":0,"xMax":800,"yMax":600,"yAxis":"Down"},"trackerClock":"tracker-v1","analysisClock":"analysis-v1","angularFrame":"angular-v1","viewing":{"distanceMm":600,"widthMm":400,"heightMm":300},"syncModel":"OffsetOnly","marks":[{"id":"trigger-v1","sourceMicros":"9007199254740993","targetMicros":"9007199254741003"}],"residualLimitMicros":"500","interpolationGapMicros":"75000","areas":[{"id":"image","label":"Image","xMin":100,"yMin":100,"xMax":700,"yMax":500}]}}"""

  // Mirrors resources/eyes4s/temporal-study-v1.json.
  val temporalStudyVersionOne: String =
    """{"schema":{"name":"eyes4s.temporal-study","version":1},"value":{"study":{"schema":{"name":"eyes4s.study","version":1},"value":{"layout":{"name":"eyes4s.participant-stimulus-phase","version":1},"keySchema":{"name":"eyes4s.study-key","version":1},"method":{"name":"eyes4s.cosine","version":1},"input":"4fd98f50693c79ad","frame":{"id":"temporal","unit":"px","xMin":0.0,"yMin":0.0,"xMax":2.0,"yMax":2.0,"yAxis":"Down"},"gridId":"temporal@2x2","nx":2,"ny":2,"focalPhase":"recall","referencePhase":"encode","weight":"Duration","policy":{"kind":"requireAll"},"estimates":[{"kind":"binned"},{"kind":"gaussian","sigma":1.0,"edges":"Truncate"}],"parameters":{"schema":{"name":"eyes4s.unit","version":1},"value":{}}}},"input":"def40de68e529acf","scope":"withinParticipant","boundary":"ClipDuration","windows":[{"name":"early","fromMicros":"0","untilMicros":"300000"},{"name":"middle","fromMicros":"300000","untilMicros":"600000"},{"name":"late","fromMicros":"600000","untilMicros":"950000"},{"name":"outside","fromMicros":"950000","untilMicros":"1100000"}],"repetitions":[{"name":"recall-encode","focal":"recall","reference":"encode"},{"name":"retest-recall","focal":"retest","reference":"recall"}]}}"""
