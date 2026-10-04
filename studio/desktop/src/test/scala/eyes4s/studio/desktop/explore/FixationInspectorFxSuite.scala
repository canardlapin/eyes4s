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

package eyes4s.studio.desktop.explore

import eyes4s.studio.app.StoryModels
import eyes4s.studio.app.nav.Place
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.core.fixture.StoryMoment
import eyes4s.studio.desktop.StudioWindow
import eyes4s.studio.desktop.harness.{FxStage, StudioTheme}
import eyes4s.studio.desktop.shell.ShellFxSuite

import scala.concurrent.duration.Duration

/** Explore's fixation inspector in the studio window (ticket S6.5;
  * Explore.dc.html, right) at t2, with fixtures/studio-golden's records
  * behind the port: the fixture fixation 6 = record 7,214, onset 2,160 ms,
  * 412 ms, screen (1148, 456), image (700, 300), (+5.4°, +2.4°); its source;
  * 'Used by' links that open Compare at the pair; and the trial.
  */
class FixationInspectorFxSuite extends ShellFxSuite:

  override val munitTimeout: Duration = Duration(180, "s")

  private def ready(fx: FxStage, w: StudioWindow): Unit =
    eventually(fx, "the inspector is filled") {
      w.inspector.lines.exists(_ == ("Record", "7,214 of 11,520")) &&
      w.inspector.linkLabels.size == 2 &&
      w.inspector.lines.exists((k, v) => k == "Display" && v != "—")
    }

  fxStage.test(
    "the fixture fixation: record 7,214, 2,160 ms, 412 ms, (1148, 456), (700, 300), (+5.4°, +2.4°)"
  ) { fx =>
    val w = boot(fx, StoryModels.t2Explore, StoryMoment.T2)
    ready(fx, w)
    assertEquals(runOnFx(w.inspector.titleText), "Fixation 6 of 13")
    val lines = runOnFx(w.inspector.lines).toMap
    assertEquals(lines("Onset · duration"), "2,160 ms · 412 ms")
    assertEquals(lines("Screen px (raw)"), "1148.0, 456.0")
    assertEquals(lines("Image frame px"), "700, 300")
    assertEquals(lines("Degrees from centre"), "+5.4°, +2.4°")
    assertEquals(lines("Analysis window"), "Inside")
    assertEquals(lines("Record"), "7,214 of 11,520")
    assertEquals(lines("Ledger"), "Admitted · dataset r3")
    assertEquals(lines("Key"), "P17 · Encoding · enc_03 · occ 1")
    assertEquals(lines("Display"), "Image · beach-042.png")
    assertEquals(lines("Match item"), "beach-042")
    assertEquals(lines("Fixations"), "13 · 1 outside")
    // The numbers are the fixture's.
    val (sx, sy) = lines("Screen px (raw)").split(", ") match
      case Array(a, b) => (a.toDouble, b.toDouble)
      case other       => fail(other.mkString)
    assertEquals((sx, sy), (1148.0, 456.0))
    fx.snapshot(StudioTheme.Light)
  }

  fxStage.test(
    "'Used by' links: ret_07 (matched) opens its pair in Compare; 18 pairs as a control"
  ) { fx =>
    val w = boot(fx, StoryModels.t2Explore, StoryMoment.T2)
    ready(fx, w)
    assertEquals(
      runOnFx(w.inspector.linkLabels),
      Vector(
        "ret_07 contrast (matched reference)",
        "18 pairs (as a control)"
      )
    )
    runOnFx(w.inspector.follow("ret_07 contrast (matched reference)"))
    fx.awaitLayout()
    val m = runOnFx(w.runtime.model)
    assertEquals(m.perspective, Perspective.Compare)
    assertEquals(m.location.trail.last, Place.At(StoryModels.pair))
  }

  fxStage.test("'Show raw record' shows the fixation's verbatim record") { fx =>
    val w = boot(fx, StoryModels.t2Explore, StoryMoment.T2)
    ready(fx, w)
    assertEquals(runOnFx(w.inspector.rawText), None)
    runOnFx(w.inspector.toggleRaw())
    fx.awaitLayout()
    assertEquals(
      runOnFx(w.inspector.rawText),
      Some("P17,Encoding,enc_03,1,6,1148.0,456.0,2160,412,206")
    )
  }
