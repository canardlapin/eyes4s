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

import cats.effect.unsafe.implicits.global
import eyes4s.studio.app.{AppEffect, Intent}
import eyes4s.studio.core.backend.{AnalysisRevision, BackendError}
import eyes4s.studio.core.execution.{ExecutionEffect, ExecutionError}
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.core.preview.{PreviewBudget, PreviewEvent, PreviewId, PreviewReady}

import java.util.concurrent.{LinkedBlockingQueue, TimeUnit}

/** The desktop path of a refused prepared design (S7.5, E2E-05): the
  * execution service refuses a receipt the backend no longer retains, the
  * refusal is recorded with its exact requested run, and the app settles it
  * without bypassing that refusal. No JavaFX: the UI thread is the
  * caller's.
  */
class PreparedRefusalSuite extends munit.FunSuite:

  test("a refused SubmitPreview reaches the app as PreparedRefused") {
    val session = StudioSession.start(StoryMoment.T2, _ => ())
    try
      val budget  = PreviewBudget.of(24).fold(e => fail(e.message), identity)
      val counted = session.await(
        session.backend.previewCounting(AnalysisRevision(5), budget).compile.toVector
      )
      val receipt = counted
        .collectFirst { case Right(PreviewEvent.Ready(r)) => r }
        .getOrElse(fail("no receipt"))
      // A receipt for a preview the backend does not retain (evicted).
      val evicted = PreviewReady
        .of(
          PreviewId(99L),
          receipt.stamp,
          receipt.candidates,
          receipt.counts,
          receipt.diagnostics
        )
        .fold(e => fail(e.message), identity)
      val told    = LinkedBlockingQueue[Intent]()
      val effects =
        DesktopEffects(session, (_, _) => (), _ => (), _ => (), ui = f => f(), project = None)
      effects.perform(AppEffect.Execution(ExecutionEffect.SubmitPreview(evicted)), told.put)
      val intent = Option(told.poll(20, TimeUnit.SECONDS)).getOrElse(fail("nothing dispatched"))
      val error  =
        ExecutionError.Backend(BackendError.UnknownPreview(PreviewId(99L), Vector.empty))
      intent match
        case Intent.PreparedRefused(
              r,
              ExecutionError.Backend(BackendError.UnknownPreview(id, _)),
              None
            ) =>
          assertEquals((r, id), (evicted, PreviewId(99L)))
        case other => fail(s"expected PreparedRefused, got $other ($error)")
      assert(effects.problems.exists {
        case EffectProblem.Refused(ExecutionEffect.SubmitPreview(r, _), _) => r == evicted
        case _                                                             => false
      })
      // A plain submission's refusal is only recorded, as before.
      effects.perform(AppEffect.Execution(ExecutionEffect.Submit(receipt.stamp)), told.put)
      assertEquals(Option(told.poll(2, TimeUnit.SECONDS)), None)
    finally session.close()
  }
