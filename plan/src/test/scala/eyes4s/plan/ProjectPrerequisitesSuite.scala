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

package eyes4s.plan

import cats.instances.string.*
import eyes4s.aoi.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import scala.compiletime.testing.typeCheckErrors

class ProjectPrerequisitesSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val frame                             = get(Frame.screen("project-display", 100, 100))
  private val tracker                           = ClockId("tracker")
  private val observed                          = ClockId("observed")
  private val key                               = get(SessionKey.of("recording-A"))
  private val empty                             = Session.empty(frame)
  private val recording                         = get(
    Recording.of(
      frame,
      tracker,
      Rate.Irregular,
      Eye.Left,
      None,
      IArray(Sample(Instant.micros(0), Gaze.Tracked(Pt[Px](5, 5), None)))
    )
  )
  private val session = get(empty.addRecording(key, recording))
  private val absent = ProjectEvidence[String, String](None, Map.empty, Vector.empty, Set.empty)
  private val selector = MarkSelector("start", Occurrence.RequireUnique)
  private def id(name: String, version: Int = 1) = get(DefinitionId.of(name, version))
  private def entry(name: String, needs: ProjectRequirement[String, String]*) =
    ProjectAnalysis(id(name), needs.toVector)
  private def timeline(clock: ClockId, times: Long*) =
    get(ObservedTimeline.of(clock, times.toVector.map(t => Mark(Instant.micros(t), "start"))))
  private def anchor(
      s: MarkSelector[String] = selector,
      clock: ClockId = observed,
      maximum: Long = 0
  ): ProjectRequirement[String, String] =
    ProjectRequirement.ObservedAnchor(
      "trial-7",
      key,
      s,
      clock,
      get(SyncErrorMagnitude.of(Span.micros(maximum)))
    )
  private def findings(
      need: ProjectRequirement[String, String],
      evidence: ProjectEvidence[String, String] = absent
  ) =
    ProjectPrerequisites
      .query(session, evidence, Vector(entry("analysis", need)))
      .analyses
      .head
      .findings
  private def sync(source: ClockId = observed, target: ClockId = tracker, noise: Long = 0) =
    get(
      SyncEvidence.fromCommonMarks(
        source,
        target,
        SyncFitMode.OffsetOnly,
        Vector(
          get(SyncMark.of("a", Instant.micros(0), Instant.micros(10))),
          get(SyncMark.of("b", Instant.micros(100), Instant.micros(110 + noise)))
        )
      )
    )

  test("one query retains the catalogue and names every absent project resource") {
    val catalogue = Vector(
      entry("angular", ProjectRequirement.ViewingGeometry()),
      entry("reading", ProjectRequirement.Areas(key)),
      entry("map", ProjectRequirement.Grid(key)),
      entry("recording", ProjectRequirement.Recording(key))
    )
    val report = ProjectPrerequisites.query(empty, absent, catalogue)
    assertEquals(report.analyses.map(_.analysis), catalogue)
    assertEquals(report.available, Vector.empty)
    assertEquals(
      report.unavailable.flatMap(_.findings),
      Vector(
        ProjectFinding.MissingViewing(),
        ProjectFinding.MissingAreas(key),
        ProjectFinding.MissingGrid(key),
        ProjectFinding.MissingRecording(key)
      )
    )
    report.unavailable.foreach(r => assert(r.findings.forall(_.message.nonEmpty)))
  }

  test("only checked Session members and actual Viewing satisfy spatial prerequisites") {
    val area  = get(Aoi.of("a", "area", frame, get(Region.rect(Pt[Px](0, 0), Pt[Px](10, 10)))))
    val areas = get(AoiSet.of(Vector(area)))
    val populated = get(
      get(session.addAoiSet(key, areas)).addGrid(key, get(Grid.of(GridId("map"), frame, 2, 2)))
    )
    val evidence = absent.copy(viewing = Some(get(Viewing.millimetres(600, 500, 500))))
    val analysis = entry(
      "spatial",
      ProjectRequirement.ViewingGeometry(),
      ProjectRequirement.Areas(key),
      ProjectRequirement.Grid(key),
      ProjectRequirement.Recording(key)
    )
    val report = ProjectPrerequisites.query(populated, evidence, Vector(analysis, analysis))
    assertEquals(report.available, Vector(analysis, analysis))
    assertEquals(report.unavailable, Vector.empty)
  }

  test("schema versions are exact and installed alternatives are named") {
    val required = id("consumer.marks", 3)
    val evidence = absent.copy(schemas = Set(id("consumer.marks", 1), id("consumer.marks", 2)))
    assertEquals(
      findings(ProjectRequirement.Schema(required), evidence),
      Vector(ProjectFinding.MissingSchema(required, Vector(1, 2)))
    )
    assert(
      findings(
        ProjectRequirement.Schema(required),
        evidence.copy(schemas = evidence.schemas + required)
      ).isEmpty
    )
  }

  test("missing and ambiguous observed anchors retain the trial selector and count") {
    assertEquals(
      findings(anchor()),
      Vector(ProjectFinding.MissingObservedTimeline("trial-7", selector, observed))
    )
    Vector(Vector.empty[Long], Vector(1L, 2L)).foreach { times =>
      val evidence = absent.copy(observed = Map("trial-7" -> timeline(observed, times*)))
      assertEquals(
        findings(anchor(), evidence),
        Vector(ProjectFinding.Anchor(EpochError.AnchorMatches("trial-7", selector, times.size)))
      )
    }
  }

  test("occurrence policies agree with epoch execution including equal-time order") {
    val marks = timeline(tracker, 20, 5, 5)
    Vector(
      Occurrence.First                           -> 5L,
      Occurrence.Last                            -> 20L,
      Occurrence.Nth(get(NonNegativeLong.of(1))) -> 5L
    ).foreach { (occurrence, expected) =>
      val select   = MarkSelector("start", occurrence)
      val evidence = absent.copy(observed = Map("trial-7" -> marks))
      assert(findings(anchor(select, tracker), evidence).isEmpty)
      val plan = get(
        EpochPlan.of(
          select,
          get(Window.of(Span.zero, Span.micros(10))),
          get(PositiveSpan.of(Span.micros(10))),
          FinalBin.RequireExactDivision
        )
      )
      assertEquals(get(select.resolve("trial-7", marks, tracker)).toMicros, expected)
      assertEquals(
        get(
          plan.resolve("trial-7", marks, tracker, get(NonNegativeLong.of(1)))
        ).anchor.toMicros,
        expected
      )
    }
  }

  test("a foreign observed clock is refused before any synchronization claim") {
    val foreign  = ClockId("foreign")
    val evidence = absent.copy(
      observed = Map("trial-7" -> timeline(foreign, 0)),
      synchronization = Vector(sync())
    )
    val error = findings(anchor(), evidence).head
    assert(error match
      case ProjectFinding.Anchor(EpochError.Clock("trial-7", `selector`, underlying)) =>
        underlying.message.contains("foreign") && underlying.message.contains("observed")
      case _ => false)
  }

  test(
    "direction and nominal clocks constrain synchronization; reverse evidence is insufficient"
  ) {
    Vector(
      sync(tracker, observed),
      sync(ClockId("foreign-source"), tracker),
      sync(observed, ClockId("foreign-target"))
    ).foreach { candidate =>
      val evidence = absent.copy(
        observed = Map("trial-7" -> timeline(observed, 0)),
        synchronization = Vector(candidate)
      )
      assertEquals(
        findings(anchor(), evidence),
        Vector(
          ProjectFinding.MissingSynchronization(
            "trial-7",
            selector,
            observed,
            tracker,
            SyncErrorMagnitude.zero,
            Vector(candidate.source -> candidate.target)
          )
        )
      )
      assert(findings(anchor(), evidence.copy(synchronization = Vector(sync()))).isEmpty)
    }
  }

  test("uncertainty acceptance is inclusive and chooses any adequate clock-matched evidence") {
    val noisy = sync(noise = 4)
    assertEquals(noisy.uncertainty.toMicros, 2L)
    val evidence = absent.copy(
      observed = Map("trial-7" -> timeline(observed, 0)),
      synchronization = Vector(noisy)
    )
    assertEquals(
      findings(anchor(maximum = 1), evidence),
      Vector(
        ProjectFinding.ImpreciseSynchronization(
          "trial-7",
          selector,
          observed,
          tracker,
          get(SyncErrorMagnitude.of(Span.micros(1))),
          noisy.uncertainty
        )
      )
    )
    assert(findings(anchor(maximum = 2), evidence).isEmpty)
    assert(findings(anchor(), evidence.copy(synchronization = Vector(noisy, sync()))).isEmpty)
  }

  test("planned timing cannot be supplied as observed evidence") {
    assert(typeCheckErrors("""
      def forbidden(planned: eyes4s.kernel.PlannedTimeline[String]): eyes4s.kernel.ObservedTimeline[String] = planned
    """).nonEmpty)
  }

  test("common diagnostics retain analysis context typed trial keys and nested anchor causes") {
    val analysis = entry("aligned", anchor())
    val evidence = absent.copy(observed = Map("trial-7" -> timeline(observed, 1, 2)))
    val report   = ProjectPrerequisites.query(session, evidence, Vector(analysis)).analyses.head
    assertEquals(report.availability, Availability.Unavailable)
    val diagnostic = report.diagnostics.head
    assertEquals(diagnostic.subject.head, Locus.Definition(analysis.id))
    assertEquals(diagnostic.keys, Vector("trial-7"))
    assertEquals(diagnostic.code.render, "project-prerequisite.anchor")
    assertEquals(diagnostic.causes.head.code.render, "epoch.anchor-matches")
    assertEquals(diagnostic.causes.head.operand("count"), Some(Operand.Integer(BigInt(2))))
    val ready = ProjectPrerequisites
      .query(session, absent, Vector(entry("recording", ProjectRequirement.Recording(key))))
      .analyses
      .head
    assertEquals(ready.availability, Availability.Ready)
    assertEquals(ready.diagnostics, Vector.empty)
  }
