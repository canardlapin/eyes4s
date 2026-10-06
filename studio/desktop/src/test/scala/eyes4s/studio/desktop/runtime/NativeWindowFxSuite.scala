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
import eyes4s.studio.app.compare.ReportAnswer
import eyes4s.studio.core.backend.{PageRequest, QueryStatus, ReportRole}
import eyes4s.studio.core.document.ReportingContrast
import eyes4s.studio.core.fixture.StoryMoments
import eyes4s.studio.core.selection.ScaleIndex
import eyes4s.studio.desktop.journey.FixtureDoc
import eyes4s.studio.desktop.shell.ShellFxSuite
import scala.concurrent.duration.*

/** The native window must display the exact report returned for its current
  * specification. Editing reporting reuses scores; it must not start a run.
  */
class NativeWindowFxSuite extends ShellFxSuite:
  override val munitTimeout: Duration = 300.seconds

  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)

  fxStage.test(
    "native startup and explicit contrast edits display native reports without rerunning"
  ) { fx =>
    val w = boot(
      fx,
      StoryModels.t2Compare,
      nativeSources =
        Some(DatasetSourceHosts.golden(FixtureDoc.root.resolve("fixtures/studio-golden")))
    )
    assertEquals(w.session.fixture, None)
    val scale = get(ScaleIndex.of(2))
    val spec  = runOnFx(w.runtime.model.document.reporting.head)
    // This wait runs on the test thread. JavaFX remains free to receive the
    // same completion and to render the asynchronously requested reports.
    val native = get(w.session.await(w.session.reads.report(StoryMoments.run7, spec, 2)))
    eventually(fx, "the exact native grouped report") {
      w.summary.summary.reports.get((scale, false)).contains(ReportAnswer.Answered(native))
    }
    assertEquals(native.contrasts, Vector.empty)
    val page = get(
      w.session.await(
        w.session.reads
          .queries(StoryMoments.run7, get(PageRequest.of(0, PageRequest.MaximumSize)))
      )
    )
    assertEquals(page.rows.size, 480)
    val unadmitted = page.rows.collect {
      case q if q.status.isInstanceOf[QueryStatus.NotAdmitted] => q
    }
    assertEquals(unadmitted.size, 14)
    assert(unadmitted.forall(_.matched.isEmpty))
    val before = w.session.await(w.session.backend.jobs)
    assertEquals(before.size, 1)

    runOnFx(w.reporting.applyContrast("Remembered", "Forgotten"))
    eventually(fx, "the ordered operands in the document") {
      w.runtime.model.document.reporting.head.contrast == ReportingContrast
        .of("Remembered", "Forgotten")
        .toOption
    }
    val edited  = runOnFx(w.runtime.model.document.reporting.head)
    val changed = get(w.session.await(w.session.reads.report(StoryMoments.run7, edited, 2)))
    eventually(fx, "the edited native report") {
      w.summary.summary.reports.get((scale, false)).contains(ReportAnswer.Answered(changed))
    }
    val difference =
      changed.contrast(ReportRole.Difference).getOrElse(fail("missing native contrast"))
    assert(difference.estimate.exists(_ != 0.0))
    assertEquals(w.session.await(w.session.backend.jobs), before)
    assertEquals(
      runOnFx(w.runtime.model.document.presentation.shownRun),
      Some(StoryMoments.run7)
    )
  }
