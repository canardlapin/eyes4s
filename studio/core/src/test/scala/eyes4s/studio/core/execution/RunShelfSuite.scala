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

package eyes4s.studio.core.execution

import eyes4s.studio.core.backend.{AnalysisRevision, DatasetRevision, JobId, RunId}
import eyes4s.studio.core.document.CoreBinding

class RunShelfSuite extends munit.FunSuite:
  private val stamp =
    RunStamp(AnalysisRevision(1), DatasetRevision(1), CoreBinding.unbound, CoreBinding.unbound)
  private def ready(n: Int): ExecutionEvent =
    ExecutionEvent.Ready(RunReady(JobId(n), RunId(n), stamp))

  test("a shown or older run cannot become a ready notice, even at the required stamp") {
    val shelf = RunShelf.of(Some(RunId(8))).require(stamp)
    List(7, 8).foreach(n => assertEquals(shelf.receive(ready(n)), shelf))
    assertEquals(shelf.receive(ready(9)).pending.map(_.run), Some(RunId(9)))
    assertEquals(RunShelf.of(None).receive(ready(9)).pending, None)
  }

  test(
    "document undo reconciles shown while retaining the request and any newer pending notice"
  ) {
    val shelf  = RunShelf.of(Some(RunId(8))).require(stamp)
    val undone = shelf.withShown(Some(RunId(7)))
    assertEquals(undone.required, Some(stamp))
    assertEquals(undone.receive(ready(8)).pending.map(_.run), Some(RunId(8)))
    val newer = undone.receive(ready(9))
    assertEquals(newer.withShown(Some(RunId(8))).pending, newer.pending)
    assertEquals(newer.withShown(Some(RunId(9))).pending, None)
  }
