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

package eyes4s.studio.app.nav

import eyes4s.studio.app.{AppModel, Intent, StoryModels}
import eyes4s.studio.app.vm.{FreshnessTone, Shell}
import eyes4s.studio.core.backend.{
  DiagnosticLevel,
  DiagnosticOrigin,
  PairDesign,
  Response,
  StudioDiagnostic
}
import eyes4s.studio.core.command.Command
import eyes4s.studio.core.document.{Perspective, SourceRole}
import eyes4s.studio.core.execution.{ExecutionEvent, JobPhase}
import eyes4s.studio.core.fixture.StoryMoments
import eyes4s.studio.core.navigation.ChainLevel
import eyes4s.studio.core.selection.{FixationIndex, RecordNumber, StudioRef}

/** The provenance trail model (S3.4): explain, parents, the trail grammar of
  * DESIGN_SPEC section 3, and history across perspectives (E2E-17).
  */
class TrailModelSuite extends munit.FunSuite:
  import StoryModels.*

  private val run7      = StoryMoments.run7
  private val forgotten = Response("Forgotten")

  private def p17In(group: Option[Response]): StudioRef =
    StudioRef.ParticipantSummary(run7, reporting, sigma2, group, "P17")

  private val map03: StudioRef    = StudioRef.TrialMap(run7, sigma2, p17enc03)
  private val fixation: StudioRef = StudioRef.Fixation(p17enc03, fixation6)

  /** The full chain of the design spec's trail grammar, map included. */
  private val chain: Vector[Place] = Vector(
    Place.Summary(reporting),
    Place.Group(reporting, remembered),
    Place.At(p17In(Some(remembered))),
    Place.At(query),
    Place.At(pair),
    Place.At(map03),
    Place.At(fixation),
    Place.At(record)
  )

  private def crumbs(m: AppModel): Vector[String] = Shell.context(m).trail.map(_.label)

  private def compareSummary: AppModel =
    play(
      t2Compare,
      _ => Intent.SwitchPerspective(Perspective.Compare),
      _ => Intent.OpenCrumb(0)
    )

  test("Explain P17 from the summary lands on the P17 group with the spec and group kept") {
    val start = compareSummary
    assertEquals(start.location.trail, Vector(Place.Summary(reporting)))
    val m = AppModel.update(start, Intent.Explain(Place.At(p17In(Some(remembered)))))._1
    assertEquals(m.perspective, Perspective.Compare)
    assertEquals(crumbs(m), Vector("Summary · by retrieval response", "Remembered", "P17"))
    assertEquals(
      crumbs(m).mkString(" › "),
      "Summary · by retrieval response › Remembered › P17"
    )
    assertEquals(Provenance.reporting(m.location.trail), Some(reporting))
    assertEquals(Provenance.group(m.location.trail), Some(remembered))
  }

  test("each level of the chain names the level the grammar gives it") {
    assertEquals(
      chain.map(Provenance.level),
      ChainLevel.values.toVector.map(Some(_))
    )
    assertEquals(Provenance.refs(chain).size, 6)
  }

  test("every step of the chain has its predecessor as parent, in the chain's context") {
    chain.indices.drop(1).foreach { i =>
      assertEquals(Provenance.parent(chain(i), chain.take(i)), Some(chain(i - 1)), s"step $i")
    }
    assertEquals(Provenance.parent(chain.head, Vector.empty), None)
  }

  test("explaining the record from the summary builds the whole chain") {
    // Context-free, a map sits under its trial: the chain needs its pair on the trail.
    val down = chain.indices.drop(1).foldLeft(chain.take(1)) { (trail, i) =>
      Provenance.explain(trail, chain(i))
    }
    assertEquals(down, chain)
  }

  test("explain keeps the prefix a target descends from, and only that prefix") {
    // Main's trail: the board goes pair › fixation › record (no map crumb).
    val explore = Provenance.explain(queryTrail, Place.At(record))
    assertEquals(
      explore,
      queryTrail ++ Vector(Place.At(fixation), Place.At(record))
    )
    // A target on the trail truncates it there.
    assertEquals(Provenance.explain(chain, Place.At(query)), chain.take(4))
    // A sibling group replaces the group and everything below it.
    assertEquals(
      Provenance.explain(chain, Place.At(p17In(Some(forgotten)))),
      Vector(
        Place.Summary(reporting),
        Place.Group(reporting, forgotten),
        Place.At(p17In(Some(forgotten)))
      )
    )
    // An unrelated target starts at its own root.
    val other = StudioRef.Trial(StoryModels.p17ret07.copy(participant = "P03"))
    assertEquals(
      Provenance.explain(chain, Place.At(other)),
      Vector(Place.At(StudioRef.Participant("P03")), Place.At(other))
    )
  }

  test("a query reached without a summary sits under its participant") {
    val explore =
      Vector(Place.At(StudioRef.Participant("P17")), Place.At(StudioRef.Trial(p17enc03)))
    assertEquals(
      Provenance.explain(explore, Place.At(query)),
      Vector(Place.At(StudioRef.Participant("P17")), Place.At(query))
    )
  }

  test("explain from an empty trail climbs to a root whose parent is none") {
    val targets = chain ++ Vector(
      Place.At(
        StudioRef.SourceRecord(p17enc03, None, SourceRole.Trials, ok(RecordNumber.of(3)))
      ),
      Place.At(StudioRef.Pair(run7, sigma2, PairDesign.Control, p17ret07, p17enc03))
    )
    targets.foreach { target =>
      val t = Provenance.explain(Vector.empty, target)
      assertEquals(t.last, target)
      assertEquals(Provenance.parent(t.head, Vector.empty), None, s"root of $target")
      t.indices
        .drop(1)
        .foreach(i =>
          assertEquals(Provenance.parent(t(i), t.take(i)), Some(t(i - 1)), s"$target step $i")
        )
    }
  }

  test("the perspective follows the target: fixation and record cross into Explore") {
    val start         = compareSummary
    val (atRecord, _) = AppModel.run(
      start,
      chain.drop(1).map(Intent.Explain(_))
    )
    assertEquals(atRecord.perspective, Perspective.Explore)
    assertEquals(atRecord.location.trail, chain)
    assertEquals(
      crumbs(atRecord),
      Vector(
        "Summary · by retrieval response",
        "Remembered",
        "P17",
        "ret_07 · beach-042",
        "pair ret_07 × enc_03",
        "map enc_03 · σ 2°",
        "enc_03 · fixation 6",
        "fixations.csv record 7,214"
      )
    )
    // The trail survives the switch: Compare still shows where it was left.
    assertEquals(atRecord.navigation.trail(Perspective.Compare), chain.take(6))
  }

  test("E2E-17: a crumb round-trip into Explore and back is lossless") {
    val explore = t2Explore
    assertEquals(explore.perspective, Perspective.Explore)
    val before = explore.location
    // Open the pair crumb: it crosses into Compare with the prefix.
    val atPair = AppModel.update(explore, Intent.OpenCrumb(4))._1
    assertEquals(atPair.perspective, Perspective.Compare)
    assertEquals(atPair.location.trail, queryTrail)
    assertEquals(atPair.navigation.trail(Perspective.Explore), before.trail)
    // Back returns to the record exactly; forward to the pair.
    val back = AppModel.update(atPair, Intent.Back)._1
    assertEquals(back.location, before)
    assertEquals(back.selection, explore.selection)
    val forward = AppModel.update(back, Intent.Forward)._1
    assertEquals(forward.location, atPair.location)
    // ⌘2 from Compare returns to Explore at the record, the trail intact.
    val switched = AppModel.update(forward, Intent.SwitchPerspective(Perspective.Explore))._1
    assertEquals(switched.location, before)
    assertEquals(crumbs(switched), crumbs(explore))
  }

  test("back and forward cross perspectives in order") {
    val m0      = compareSummary
    val (m1, _) = AppModel.run(
      m0,
      Vector(
        Intent.Explain(Place.At(p17In(Some(remembered)))),
        Intent.Explain(Place.At(record)),
        Intent.SwitchPerspective(Perspective.Analysis)
      )
    )
    val visited = Vector(
      Perspective.Analysis,
      Perspective.Explore,
      Perspective.Compare,
      Perspective.Compare
    )
    val (back, seen) = (1 to 3).foldLeft((m1, Vector(m1.perspective))) { case ((m, ps), _) =>
      val n = AppModel.update(m, Intent.Back)._1
      (n, ps :+ n.perspective)
    }
    assertEquals(seen, visited)
    assertEquals(back.location, m0.location)
    assert(!back.navigation.canGoBack || back.navigation.canGoForward)
    val (again, _) = AppModel.run(back, Vector.fill(3)(Intent.Forward))
    assertEquals(again.location, m1.location)
  }

  test("an explain to where one is changes nothing") {
    val m    = t2Explore
    val same = AppModel.update(m, Intent.Explain(m.location.trail.last))._1
    assertEquals(same.location, m.location)
    assertEquals(same.navigation, m.navigation)
  }

  test("the fixation index and record in the chain are the fixture's focus fixation") {
    assertEquals(fixation6, ok(FixationIndex.of(6)))
    assertEquals(
      record,
      StudioRef.SourceRecord(
        p17enc03,
        Some(fixation6),
        SourceRole.Fixations,
        ok(RecordNumber.of(7214))
      )
    )
  }

  // -------------------------------------------------------------------------
  // S1.6: the context strip's crumbs, back/forward and freshness
  // -------------------------------------------------------------------------

  /** The board models, each at its story moment. */
  private def boards: Vector[(String, AppModel)] = Vector(
    "t1 Data"         -> StoryModels.t1Data,
    "t2 Compare"      -> t2Compare,
    "t2 Explore"      -> t2Explore,
    "t2 Analysis"     -> StoryModels.t2Analysis,
    "t2 Figures"      -> StoryModels.t2Figures,
    "t3 Summary"      -> StoryModels.t3Summary,
    "t2 Compare root" -> compareSummary
  )

  test("S1.6: every crumb of every board is live and lands on its prefix") {
    for (board, m) <- boards; (crumb, i) <- Shell.context(m).trail.zipWithIndex do
      val target = Shell.crumbTarget(m, i).getOrElse(fail(s"$board crumb $i has no target"))
      assertEquals(crumb.intent, Intent.OpenCrumb(i), s"$board crumb $i")
      val landed = AppModel.update(m, crumb.intent)._1
      assertEquals(landed.location, target, s"$board crumb $i")
      assertEquals(landed.location.trail, m.location.trail.take(i + 1), s"$board crumb $i")
      // `opens` names the crossing, and only a crossing.
      assertEquals(
        crumb.opens,
        Option.when(target.perspective != m.perspective)(target.perspective),
        s"$board crumb $i"
      )
      // A crumb that moves can be undone by Back.
      if !crumb.current then
        assertEquals(AppModel.update(landed, Intent.Back)._1.location, m.location, board)
  }

  test("S1.6: Explore's crumbs name the perspective they open; the current crumb says so") {
    val trail = Shell.context(t2Explore).trail
    assertEquals(
      trail.map(_.accessible),
      Vector(
        "Summary · by retrieval response, opens in Compare",
        "Remembered, opens in Compare",
        "P17, opens in Compare",
        "ret_07 · beach-042, opens in Compare",
        "pair ret_07 × enc_03, opens in Compare",
        "enc_03 · fixation 6",
        "fixations.csv record 7,214, current location"
      )
    )
    assertEquals(
      trail.map(_.opens.isDefined),
      Vector(true, true, true, true, true, false, false)
    )
  }

  test("S1.6 · E2E-17: the fixation and record crumbs cross into Explore and back") {
    // Compare at the pair: open the record, which crosses into Explore.
    val compare  = t2Compare
    val atRecord = AppModel.update(compare, Intent.Explain(Place.At(record)))._1
    assertEquals(atRecord.perspective, Perspective.Explore)
    val crumbsThere = Shell.context(atRecord).trail
    assertEquals(crumbsThere.last.label, "fixations.csv record 7,214")
    assertEquals(crumbsThere(crumbsThere.size - 2).label, "enc_03 · fixation 6")
    // The fixation crumb stays in Explore and truncates the trail there.
    val fixationCrumb = crumbsThere.size - 2
    val atFixation    = AppModel.update(atRecord, crumbsThere(fixationCrumb).intent)._1
    assertEquals(atFixation.perspective, Perspective.Explore)
    assertEquals(atFixation.location.trail.last, Place.At(fixation))
    // The pair crumb goes back to Compare, exactly where it was left.
    val pairCrumb = Shell.context(atFixation).trail.indexWhere(_.label.startsWith("pair"))
    val back      = AppModel.update(atFixation, Intent.OpenCrumb(pairCrumb))._1
    assertEquals(back.perspective, Perspective.Compare)
    assertEquals(back.location, compare.location)
    // ⌘[ twice retraces the round trip: fixation, then record.
    val (retraced, _) = AppModel.run(back, Vector(Intent.Back, Intent.Back))
    assertEquals(retraced.location, atRecord.location)
    // ⌘] twice returns to Compare.
    val (again, _) = AppModel.run(retraced, Vector(Intent.Forward, Intent.Forward))
    assertEquals(again.location, back.location)
    assertEquals(again.selection, compare.selection)
  }

  test("S1.6: back and forward are enabled exactly when the history has a step") {
    val m0 = compareSummary
    val c0 = Shell.context(m0)
    assertEquals((c0.back.enabled, c0.forward.enabled), (true, false))
    assertEquals(c0.back.label, "Back (⌘[)")
    assertEquals(c0.forward.label, "Forward (⌘])")
    val m1 = AppModel.update(m0, c0.back.intent)._1
    val c1 = Shell.context(m1)
    assert(c1.forward.enabled)
    assertEquals(AppModel.update(m1, c1.forward.intent)._1.location, m0.location)
    // A fresh project has nowhere to go.
    val fresh = Shell.context(StoryModels.firstRun)
    assertEquals((fresh.back.enabled, fresh.forward.enabled), (false, false))
  }

  private val lost = StudioDiagnostic(
    "studio-execution.lost-job",
    DiagnosticLevel.Error,
    DiagnosticOrigin.Host,
    Vector.empty,
    "lost"
  )

  test("S1.6: freshness shows current, draft, stale, running and failed as S2.7 derives them") {
    // Current, with the dashed draft chip (t2).
    val t2 = Shell.context(t2Compare)
    assertEquals(t2.freshness.text, "Analysis rev 4 · run 7 · data r3 · current")
    assertEquals(t2.freshness.tone, FreshnessTone.Current)
    assertEquals(t2.draft.map(_.text), Some("Draft rev 5 · 1 change · ready"))
    assertEquals(t2.draft.map(_.intent), Some(Intent.ReviewDraft))
    assertEquals(
      t2.draft.map(_.accessible),
      Some("Draft rev 5 · 1 change · ready; review in Analysis")
    )
    // The draft chip opens Analysis at the draft.
    val reviewed = AppModel.update(t2Compare, Intent.ReviewDraft)._1
    assertEquals(reviewed.perspective, Perspective.Analysis)
    assertEquals(Shell.context(reviewed).trail.last.label, "Draft rev 5")
    // Stale: showing run 5, which used r2 (r3 is admitted).
    val stale = AppModel
      .update(t2Compare, Intent.Dispatch(Command.ShowRun(Some(StoryMoments.run5))))
      ._1
    val s = Shell.context(stale)
    assertEquals(s.freshness.tone, FreshnessTone.Stale)
    assert(s.freshness.text.endsWith("· stale"), s.freshness.text)
    // Running (t3): the two-chip form, the badge's dot hollow.
    val t3 = Shell.context(StoryModels.t3Summary)
    assertEquals(t3.freshness.text, "Showing analysis rev 4 · run 7 · data r3")
    assertEquals(t3.freshness.tone, FreshnessTone.Showing)
    assertEquals(
      t3.newer.map(n => (n.text, n.tone)),
      Some(("Rev 5 · run 8 running · 48%", FreshnessTone.Running))
    )
    // Failed: run 8's job fails; the badge stays current and a failed chip says so.
    val failed = AppModel
      .update(
        StoryModels.t3Summary,
        Intent.Execution(
          ExecutionEvent.Changed(StoryModels.run8Job(JobPhase.Failed(Vector(lost, lost), None)))
        )
      )
      ._1
    val f = Shell.context(failed)
    assertEquals(f.freshness.text, "Analysis rev 4 · run 7 · data r3 · current")
    assertEquals(f.freshness.tone, FreshnessTone.Current)
    assertEquals(
      f.newer.map(n => (n.text, n.tone)),
      Some(("Run 8 failed", FreshnessTone.Failed))
    )
  }
