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

package eyes4s.fs2

import cats.effect.kernel.{Concurrent, Resource}
import eyes4s.core.Recording
import eyes4s.design.WorkQuanta
import eyes4s.kernel.Provenance
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

import _root_.fs2.Stream

/** The identity of one recording submission: the plan's input digest, its
  * detector identity and full description, plus the sample quantum that cuts
  * its chunked steps. Pair and comparison quanta play no part in a recording
  * step and are not part of the id.
  */
final case class RecordingRunId(
    input: String,
    method: DefinitionId,
    parameters: Vector[(String, Vector[Provenance.Param])],
    sampleQuantum: Int
) derives CanEqual

object RecordingRunId:
  def of(plan: RecordingPlan[?], quanta: WorkQuanta): RecordingRunId =
    RecordingRunId(plan.input.digest, plan.method.id, plan.description, quanta.samples.value)

/** The counted span of a recording run is [[eyes4s.plan.RecordingSegment]],
  * stated in the pure plan module with its totals; these aliases keep the
  * name and its cases here.
  */
type RecordingSegment = eyes4s.plan.RecordingSegment
val RecordingSegment: eyes4s.plan.RecordingSegment.type = eyes4s.plan.RecordingSegment

type RecordingProgress   = RunProgress[RecordingRunId, RecordingStage, RecordingSegment]
type RecordingOutcome[P] = RunOutcome[
  RecordingRunId,
  RecordingStage,
  RecordingSegment,
  RecordingPlanError,
  RecordingAnalysis[P]
]
type RecordingEvent[P] =
  RunEvent[
    RecordingRunId,
    RecordingStage,
    RecordingSegment,
    RecordingPlanError,
    RecordingAnalysis[P]
  ]
type RecordingRun[F[_], P] =
  Run[
    F,
    RecordingRunId,
    RecordingStage,
    RecordingSegment,
    RecordingPlanError,
    RecordingAnalysis[P]
  ]

/** Interpret a recording plan's bounded steps under Cats Effect through the
  * shared [[Execution]] runner. The cursor is the one `RecordingPlan.run`
  * drives, so a completed analysis is the pure result, chunk cuts included;
  * the detector's machine is flushed once, inside the last detection step,
  * and a run cancelled before it has flushed nothing.
  */
object RecordingExecution:

  def apply[F[_]](using F: Concurrent[F]): Runner[F] = new Runner[F]

  final class Runner[F[_]](using F: Concurrent[F]):
    private val execution = Execution[F]

    /** The deterministic step sequence as a stream; see [[Execution.Runner.events]]. */
    def events[P](
        plan: RecordingPlan[P],
        recording: Recording[Px],
        quanta: WorkQuanta = WorkQuanta.default
    ): Stream[F, RecordingEvent[P]] =
      execution.events(submission(plan, recording, quanta))

    /** Run a recording plan on its own fiber; see [[Execution.Runner.start]]. */
    def start[P](
        plan: RecordingPlan[P],
        recording: Recording[Px],
        quanta: WorkQuanta = WorkQuanta.default,
        between: F[Unit] = F.cede
    ): Resource[F, RecordingRun[F, P]] =
      execution.start(submission(plan, recording, quanta), between)

  /** The recording family's submission: id, cursor, segments and totals. */
  def submission[P](
      plan: RecordingPlan[P],
      recording: Recording[Px],
      quanta: WorkQuanta = WorkQuanta.default
  ): Submission[
    RecordingRunId,
    RecordingCursor[P],
    RecordingStage,
    RecordingSegment,
    RecordingPlanError,
    RecordingAnalysis[P]
  ] =
    new Submission(
      RecordingRunId.of(plan, quanta),
      quanta,
      () => plan.work(recording),
      RecordingSegment.of,
      (segment, _) => total(recording.size, segment)
    )

  /** Totals a recording plan can state before a segment runs; see
    * [[eyes4s.plan.RecordingSegment.total]]. All are exact.
    */
  def total(samples: Int, segment: RecordingSegment): SegmentTotal =
    RecordingSegment.total(samples, segment)
