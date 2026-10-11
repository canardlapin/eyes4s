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

package eyes4s.studio.desktop.runtime

import eyes4s.studio.app.StoryModels
import eyes4s.studio.core.backend.{
  DiagnosticLevel,
  DiagnosticOrigin,
  JobId,
  JobProgress,
  StudioDiagnostic
}
import eyes4s.studio.core.execution.{
  ExecutionEvent,
  ExecutionJob,
  ExecutionProgress,
  JobPhase,
  RunReady
}
import scala.collection.mutable.ArrayBuffer

class ExecutionEventDeliverySuite extends munit.FunSuite:
  private def job(step: Long, id: JobId = StoryModels.run8Running.id): ExecutionJob =
    val report   = StoryModels.run8Progress(step).report
    val progress = ExecutionProgress(
      JobProgress
        .of(id, report.run, step, report.segment, report.meter, report.totals)
        .toOption
        .get
    )
    StoryModels.run8Job(JobPhase.Running(progress)).copy(id = id)

  private final class Harness:
    val scheduled = ArrayBuffer.empty[Runnable]
    val seen      = ArrayBuffer.empty[ExecutionEvent]
    val queue     = ExecutionEventDelivery(r => { scheduled += r; () }, e => { seen += e; () })
    def flush(): Unit = scheduled.remove(0).run()

  test(
    "a burst schedules one FX task, preserving latest progress, completion, readiness and artifact failure"
  ) {
    val h        = Harness()
    val last     = job(1000)
    val queued   = ExecutionEvent.Changed(last.copy(phase = JobPhase.Queued))
    val complete =
      ExecutionEvent.Changed(last.copy(phase = JobPhase.Succeeded(last.phase.lastReport.get)))
    val ready      = ExecutionEvent.Ready(RunReady(last.id, last.run, last.stamp))
    val diagnostic = StudioDiagnostic(
      "test-artifact-failure",
      DiagnosticLevel.Error,
      DiagnosticOrigin.EyesCore,
      Vector.empty,
      ""
    )
    val refused = ExecutionEvent.ArtifactsRefused(last.id, last.run, diagnostic)
    h.queue.submit(queued)
    (1L to 1000L).foreach(n => h.queue.submit(ExecutionEvent.Changed(job(n))))
    Vector(complete, ready, refused).foreach(h.queue.submit)
    assertEquals(h.scheduled.size, 1)
    assertEquals(h.queue.supersededProgressSnapshots, 999L)
    h.flush()
    assertEquals(
      h.seen.toVector,
      Vector(queued, ExecutionEvent.Changed(last), complete, ready, refused)
    )
  }

  test("other jobs, cancellation and non-advancing reports are never superseded") {
    val h      = Harness()
    val first  = job(1)
    val events = Vector(
      ExecutionEvent.Changed(first),
      ExecutionEvent.Changed(job(2, JobId(999))),
      ExecutionEvent.Changed(job(3)),
      ExecutionEvent.Changed(first.copy(phase = JobPhase.Cancelling(first.phase.lastReport))),
      ExecutionEvent.Changed(job(4)),
      ExecutionEvent.Changed(job(2)),
      ExecutionEvent.Changed(first.copy(phase = JobPhase.Cancelled(first.phase.lastReport)))
    )
    events.foreach(h.queue.submit)
    h.flush()
    assertEquals(h.seen.toVector, events)
    assertEquals(h.queue.supersededProgressSnapshots, 0L)
  }

  test(
    "events arriving during delivery schedule a later task without overtaking the drained batch"
  ) {
    val tasks   = ArrayBuffer.empty[Runnable]
    val seen    = ArrayBuffer.empty[ExecutionEvent]
    val first   = ExecutionEvent.Changed(job(1))
    val barrier = ExecutionEvent.Changed(job(2).copy(phase = JobPhase.Cancelling(None)))
    val later   = ExecutionEvent.Changed(job(3, JobId(999)))
    lazy val queue: ExecutionEventDelivery = ExecutionEventDelivery(
      r => { tasks += r; () },
      e =>
        seen += e
        if e == first then queue.submit(later)
    )
    queue.submit(first)
    queue.submit(barrier)
    tasks.remove(0).run()
    assertEquals(seen.toVector, Vector(first, barrier))
    assertEquals(tasks.size, 1)
    tasks.remove(0).run()
    assertEquals(seen.toVector, Vector(first, barrier, later))
  }
