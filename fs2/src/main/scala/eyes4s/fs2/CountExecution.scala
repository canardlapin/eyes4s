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

import eyes4s.design.WorkQuanta
import eyes4s.kernel.Unit2D
import eyes4s.plan.*

/** Exact preview counting uses the ordinary cancellable runner. The two
  * segments visit matched and control schedules; totals are Counting until
  * completion supplies StudyCounts, not estimates based on Cartesian size.
  */
object CountExecution:
  def submission[K, U <: Unit2D, P, S, D](
      work: PreparedStudy[K, U, P, S, D],
      quanta: WorkQuanta = WorkQuanta.default
  ): Submission[StudyRunId, CountCursor[K], StudyDesign, StudyDesign, PlanError, StudyCounts[
    K
  ]] =
    new Submission(
      StudyRunId.of(work, quanta),
      quanta,
      () => work.countWork,
      identity,
      (_, _) => SegmentTotal.Counting
    )
