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

import eyes4s.studio.core.execution.{ExecutionEvent, JobPhase}
import scala.collection.mutable.ArrayBuffer

/** The desktop displays the latest pending progress snapshot. Only adjacent,
  * advancing Running reports for the same job/run/stamp are superseded; phase
  * transitions, readiness and artifact events retain their publication order.
  * The service's event stream and every scientific result remain unchanged.
  */
private[desktop] final class ExecutionEventDelivery(
    post: Runnable => Unit,
    deliver: ExecutionEvent => Unit
):
  private val pending    = ArrayBuffer.empty[ExecutionEvent]
  private var scheduled  = false
  private var superseded = 0L

  def supersededProgressSnapshots: Long = synchronized(superseded)

  def submit(event: ExecutionEvent): Unit =
    val schedule = synchronized {
      val replaces = (pending.lastOption, event) match
        case (Some(ExecutionEvent.Changed(before)), ExecutionEvent.Changed(after))
            if before.id == after.id && before.run == after.run && before.stamp == after.stamp =>
          (before.phase, after.phase) match
            case (JobPhase.Running(a), JobPhase.Running(b)) => b.follows(a)
            case _                                          => false
        case _ => false
      if replaces then
        pending(pending.size - 1) = event
        superseded += 1
      else pending += event
      if scheduled then false
      else
        scheduled = true
        true
    }
    if schedule then post(() => flush())

  private def flush(): Unit =
    val events = synchronized {
      val batch = pending.toVector
      pending.clear()
      scheduled = false
      batch
    }
    events.foreach(deliver)
