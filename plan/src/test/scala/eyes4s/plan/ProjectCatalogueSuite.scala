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

import cats.data.NonEmptyVector
import cats.instances.string.*
import eyes4s.aoi.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px

class ProjectCatalogueSuite extends munit.FunSuite:
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(e.toString), identity)
  private val frame                             = get(Frame.screen("catalogue", 100, 100))
  private val key                               = get(SessionKey.of("recording"))
  private val grid                              = get(SessionKey.of("grid"))
  private val areas                             = get(SessionKey.of("areas"))
  private val clock                             = ClockId("recording clock")
  private val observedClock                     = ClockId("observed clock")
  private val records = ProjectInputOrigin.Recordings(NonEmptyVector.one(key))
  private val anchor  = ProjectAnchor(
    "trial",
    key,
    MarkSelector("start", Occurrence.RequireUnique),
    observedClock,
    get(SyncErrorMagnitude.of(Span.micros(0)))
  )
  private val anchors = NonEmptyVector.one(anchor)
  private val absent = ProjectEvidence[String, String](None, Map.empty, Vector.empty, Set.empty)
  private val empty  = Session.empty(frame)
  private val recording = get(
    Recording.of(
      frame,
      clock,
      Rate.Irregular,
      Eye.Left,
      None,
      IArray(Sample(Instant.micros(0), Gaze.Tracked(Pt[Px](5, 5), None)))
    )
  )
  private val area = get(
    Aoi.of("a", "area", frame, get(Region.rect(Pt[Px](0, 0), Pt[Px](10, 10))))
  )
  private val withRecording = get(empty.addRecording(key, recording))
  private val withGrid      = get(
    withRecording.addGrid(grid, get(Grid.of(GridId("grid"), frame, 2, 2)))
  )
  private val populated = get(withGrid.addAoiSet(areas, get(AoiSet.of(Vector(area)))))
  private val id        = get(DefinitionId.of("consumer.catalogue", 1))
  private val workflows: Vector[ProjectWorkflow[String, String]] = Vector(
    ProjectWorkflow.FixationStudy(records, grid, ProjectCoordinates.DegreesFromPixels),
    ProjectWorkflow.EventRecording(key, areas, Vector(anchor)),
    ProjectWorkflow.TemporalStudy(
      records,
      grid,
      ProjectCoordinates.Native,
      ProjectTemporalWindows.Observed(anchors)
    ),
    ProjectWorkflow.Repetition(records, grid, ProjectCoordinates.Native),
    ProjectWorkflow.PointSampling(records, grid, ProjectCoordinates.Native),
    ProjectWorkflow.TemplateFit(ProjectTemplateInput.FixedFeatures),
    ProjectWorkflow.Decomposition(grid),
    ProjectWorkflow.ScanpathComparison(records, ProjectCoordinates.Native),
    ProjectWorkflow.Epochs(anchors)
  )
  private def query(
      workflow: ProjectWorkflow[String, String],
      session: Session[Px] = populated,
      evidence: ProjectEvidence[String, String] = absent,
      schemas: Vector[DefinitionId] = Vector.empty
  ) = ProjectCatalogue.query(session, evidence, Vector((id, workflow, schemas))).analyses.head

  test("the built-in workflow catalogue covers every analysis and reusable component") {
    assertEquals(workflows.flatMap(_.kind.toOption).toSet, AnalysisKind.values.toSet)
    assertEquals(workflows.flatMap(_.kind.swap.toOption).toSet, AnalysisComponent.values.toSet)
    val report =
      ProjectCatalogue.query(empty, absent, workflows.map(w => (id, w, Vector.empty)))
    assertEquals(report.analyses.map(_.analysis), workflows.map(_.analysis(id)))
    assertEquals(report.analyses.size, workflows.size)
  }

  test("degree conversion names missing Viewing while native coordinates stay available") {
    val angular = query(workflows.head)
    assertEquals(angular.findings, Vector(ProjectFinding.MissingViewing()))
    val ready = absent.copy(viewing = Some(get(Viewing.millimetres(600, 500, 500))))
    assert(query(workflows.head, evidence = ready).available)
    assert(
      query(ProjectWorkflow.ScanpathComparison(records, ProjectCoordinates.Native)).available
    )
    assertEquals(
      query(
        ProjectWorkflow.ScanpathComparison(records, ProjectCoordinates.DegreesFromPixels)
      ).findings,
      Vector(ProjectFinding.MissingViewing())
    )
  }

  test("reading and spatial workflows name their exact absent resources") {
    val reading = query(ProjectWorkflow.EventRecording(key, areas, Vector.empty), empty)
    assertEquals(
      reading.findings,
      Vector(
        ProjectFinding.MissingRecording(key),
        ProjectFinding.MissingViewing(),
        ProjectFinding.MissingAreas(areas)
      )
    )
    Vector(workflows(0), workflows(2), workflows(3), workflows(4)).foreach { workflow =>
      val findings = query(workflow, empty).findings
      assert(findings.contains(ProjectFinding.MissingRecording(key)))
      assert(findings.contains(ProjectFinding.MissingGrid(grid)))
    }
    assertEquals(query(workflows(6), empty).findings, Vector(ProjectFinding.MissingGrid(grid)))
  }

  test("fixed features and measured maps need no new viewing geometry or recordings") {
    assert(query(workflows(5), empty).available)
    val maps = ProjectWorkflow.TemplateFit[String, String](ProjectTemplateInput.MeanMap(grid))
    assertEquals(query(maps, empty).findings, Vector(ProjectFinding.MissingGrid(grid)))
    assert(query(maps).available)
    assert(query(workflows(6)).available)
  }

  test(
    "epoch and temporal bindings preserve trial, selector, clock and observed-only evidence"
  ) {
    Vector(workflows(1), workflows(2), workflows(8)).foreach { workflow =>
      assert(
        query(workflow).findings.contains(
          ProjectFinding.MissingObservedTimeline("trial", anchor.selector, observedClock)
        )
      )
      val duplicates = absent.copy(observed =
        Map(
          "trial" -> get(
            ObservedTimeline.of(
              observedClock,
              Vector(Mark(Instant.micros(0), "start"), Mark(Instant.micros(1), "start"))
            )
          )
        )
      )
      assert(
        query(workflow, evidence = duplicates).findings.contains(
          ProjectFinding.Anchor(EpochError.AnchorMatches("trial", anchor.selector, 2))
        )
      )
    }
    val observed = absent.copy(observed =
      Map(
        "trial" -> get(
          ObservedTimeline.of(
            observedClock,
            Vector(Mark(Instant.micros(0), "start"))
          )
        )
      )
    )
    assert(query(workflows(8), evidence = observed).findings.exists {
      case ProjectFinding.MissingSynchronization("trial", selector, source, target, _, _) =>
        selector == anchor.selector && source == observedClock && target == clock
      case _ => false
    })
    val sameClockAnchor   = anchor.copy(observedClock = clock)
    val sameClockEvidence = absent.copy(observed =
      Map(
        "trial" -> get(
          ObservedTimeline.of(
            clock,
            Vector(Mark(Instant.micros(0), "start"))
          )
        )
      )
    )
    assert(
      query(
        ProjectWorkflow.Epochs(NonEmptyVector.one(sameClockAnchor)),
        evidence = sameClockEvidence
      ).available
    )
  }

  test("supplied maps, scanpaths and windows retain only their actual project resources") {
    val supplied = ProjectInputOrigin.Supplied
    val gridOnly = get(empty.addGrid(grid, get(Grid.of(GridId("grid"), frame, 2, 2))))
    Vector[ProjectWorkflow[String, String]](
      ProjectWorkflow.FixationStudy(supplied, grid, ProjectCoordinates.Native),
      ProjectWorkflow.Repetition(supplied, grid, ProjectCoordinates.Native),
      ProjectWorkflow.PointSampling(supplied, grid, ProjectCoordinates.Native),
      ProjectWorkflow.TemporalStudy(
        supplied,
        grid,
        ProjectCoordinates.Native,
        ProjectTemporalWindows.Supplied()
      )
    ).foreach(workflow => assert(query(workflow, gridOnly).available))
    assert(
      query(
        ProjectWorkflow.ScanpathComparison(supplied, ProjectCoordinates.Native),
        empty
      ).available
    )
    assertEquals(
      query(
        ProjectWorkflow.PointSampling(supplied, grid, ProjectCoordinates.DegreesFromPixels),
        gridOnly
      ).findings,
      Vector(ProjectFinding.MissingViewing())
    )
  }

  test("installed schema versions and repeated configured entries retain their identities") {
    val wanted   = get(DefinitionId.of("consumer.mark", 2))
    val old      = get(DefinitionId.of("consumer.mark", 1))
    val workflow = workflows(5)
    val evidence = absent.copy(schemas = Set(old))
    val report   = ProjectCatalogue.query(
      empty,
      evidence,
      Vector((id, workflow, Vector(wanted)), (wanted, workflow, Vector.empty))
    )
    assertEquals(
      report.analyses.head.findings,
      Vector(ProjectFinding.MissingSchema(wanted, Vector(1)))
    )
    assertEquals(report.available.map(_.id), Vector(wanted))
    assert(
      query(workflow, empty, evidence.copy(schemas = Set(wanted)), Vector(wanted)).available
    )
  }
