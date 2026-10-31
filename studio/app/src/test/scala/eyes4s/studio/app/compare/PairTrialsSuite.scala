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

package eyes4s.studio.app.compare

import eyes4s.studio.app.{Intent, StoryModels}
import eyes4s.studio.app.explore.ExploreRole
import eyes4s.studio.app.nav.{DataSection, Location, Place}
import eyes4s.studio.core.backend.{PairDesign, PairRowEntry, PairScoreState}
import eyes4s.studio.core.document.Perspective
import eyes4s.studio.core.fixture.MockStudy
import eyes4s.studio.core.selection.StudioRef
import munit.FunSuite

/** The Results drill's pair navigator (overhaul v4): the query, its matched
  * reference and its controls by cosine, the shown trial marked, and each
  * row opening its trial below its own pair.
  */
class PairTrialsSuite extends FunSuite:
  import StoryModels.*

  private val m = t2Explore

  private def entry(ref: String, design: PairDesign, cosine: Double) =
    PairRowEntry(
      p17ret07,
      design,
      MockStudy.key("P17", ref),
      s"$ref-item",
      PairScoreState.Scored(cosine),
      None,
      None
    )

  private val entries = Vector(
    entry("enc_03", PairDesign.Matched, 0.94),
    entry("enc_09", PairDesign.Control, 0.60),
    entry("enc_11", PairDesign.Control, 0.72)
  )

  private val focus = TrialPanels.focusOf(m)

  private def vm(nav: PairTrials = PairTrials.empty) =
    PairTrials.vm(nav, m, focus, Some(entries), Vector.empty, Some("σ 2°"))

  test("the query, the matched reference, then controls by cosine, highest first") {
    val v = vm()
    assertEquals(v.count, "4")
    assertEquals(
      v.sections.map(s => (s.heading, s.rows.map(r => (r.trial.trial, r.role, r.value)))),
      Vector(
        ("Query", Vector(("ret_07", ExploreRole.Query, ""))),
        ("Matched reference", Vector(("enc_03", ExploreRole.Matched, "0.94"))),
        (
          "Controls · cosine at σ 2°",
          Vector(
            ("enc_11", ExploreRole.Control, "0.72"),
            ("enc_09", ExploreRole.Control, "0.60")
          )
        )
      )
    )
    // t2Explore drills into enc_03: it is shown, and the cursor rests on it.
    assertEquals(v.rows.filter(_.shown).map(_.trial.trial), Vector("enc_03"))
    assertEquals(v.rows.filter(_.cursor).map(_.trial.trial), Vector("enc_03"))
  }

  test("a control opens its trial below its own pair, the trail above kept") {
    val control = vm().rows.find(_.trial.trial == "enc_11").get
    val above   = m.location.trail.takeWhile {
      case Place.At(StudioRef.Pair(_, _, _, _, _)) => false
      case _                                       => true
    }
    assertEquals(
      control.open,
      Intent.Navigate(
        Location(
          Perspective.Results,
          above ++ Vector(
            Place.At(
              StudioRef.Pair(run7Id, sigma2, PairDesign.Control, p17ret07, control.trial)
            ),
            Place.At(StudioRef.Trial(control.trial))
          )
        )
      )
    )
  }

  test("arrows move the cursor; Enter opens the trial under it") {
    val (down, none) = PairTrials.key(PairTrials.empty, vm(), NavigatorKey.Down)
    assertEquals(none, None)
    assertEquals(down.cursor.map(_.trial), Some("enc_11"))
    val (_, open) = PairTrials.key(down, vm(down), NavigatorKey.Activate)
    assertEquals(open, vm(down).rows.find(_.trial.trial == "enc_11").map(_.open))
  }

  test("All P17 trials opens Data › Trials at the participant") {
    val (words, go) = vm().all.get
    assertEquals(words, "All P17 trials")
    go match
      case Intent.Navigate(Location(Perspective.Data, trail)) =>
        assertEquals(
          trail.drop(1),
          Vector(Place.DataView(DataSection.Trials), Place.At(StudioRef.Participant("P17")))
        )
      case other => fail(s"opens $other")
  }

  test("without a pair it says so; before the rows it says it is reading") {
    assertEquals(
      PairTrials.vm(PairTrials.empty, m, None, None, Vector.empty, None).status.isDefined,
      true
    )
    assertEquals(
      PairTrials.vm(PairTrials.empty, m, focus, None, Vector.empty, None).status,
      Some("Reading the pair's trials…")
    )
  }

  private def run7Id = eyes4s.studio.core.fixture.StoryMoments.run7
