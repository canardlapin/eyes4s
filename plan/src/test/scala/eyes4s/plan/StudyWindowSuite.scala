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

import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Deg, Px}
import eyes4s.surface.EdgePolicy

/** A study over an analysis window: what is mapped, what is counted and
  * reported, what fails, and that a scale in degrees is exactly its declared
  * pixel equivalent.
  */
class StudyWindowSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)
  private val screen                        = get(Frame.screen("screen", 20, 10))
  private val window                        =
    get(Subframe.of(screen, FrameId("image"), get(Bounds.of[Px](5, 2, 15, 8))))
  private val grid = get(Grid.over(window.frame, 5, 3))

  private def path(key: StudyKey, points: (Double, Double)*): Scanpath[Px] =
    val clock = ClockId(s"${key.participant}/${key.stimulus}/${key.phase}")
    val fixes = points.zipWithIndex.map { case ((x, y), i) =>
      get(
        Event.Fixation.withoutDispersion(
          get(
            Interval
              .of(clock, Instant.micros(i * 1000L), Instant.micros(i * 1000L + 100L * (i + 1)))
          ),
          Pt[Px](x, y),
          1
        )
      )
    }
    get(Scanpath.of(screen, clock, IArray.from(fixes)))

  private val recallA = StudyKey("p1", "a", "recall")
  private val recallB = StudyKey("p1", "b", "recall")
  private val encodeA = StudyKey("p1", "a", "encode")
  private val encodeB = StudyKey("p1", "b", "encode")

  // recallA: two inside, one on the screen outside the window, one off the screen.
  private val trials = Vector(
    Trial(recallA, (), path(recallA, (6, 3), (14.5, 7.5), (2, 2), (25, 3))),
    Trial(recallB, (), path(recallB, (1, 1), (19, 9))),
    Trial(encodeA, (), path(encodeA, (7, 4), (12, 6))),
    Trial(encodeB, (), path(encodeB, (8, 3), (13, 5)))
  )
  private val input = StudyInput(Trials(trials))

  private def plan(
      geometry: StudyGeometry[Px],
      source: StudyInput[StudyKey, Px] = input,
      scales: Vector[StudyScale[Px]] = Vector(StudyScale.Native(StudyEstimate.Binned())),
      angular: Option[LinearAngularScale[Px]] = None
  ) = StudyPlan.configure(
    source.reference,
    StudyKey.layout(DefinitionId.studyLayout),
    geometry,
    "recall",
    "encode",
    Weight.Duration,
    scales,
    angular,
    FailurePolicy.RequireAll,
    StudyMethod.cosine[Px](DefinitionId.cosine),
    ()
  )

  private def windowed(policy: OffWindowPolicy) =
    get(StudyGeometry.windowed(window, grid, policy))

  private def tally(key: StudyKey) =
    get(
      get(plan(windowed(OffWindowPolicy.Exclude)))
        .windowTallies(input)
        .toMap
        .get(key)
        .toRight("none")
    )

  test("tallies separate the window from the screen, by count and by duration") {
    val a = tally(recallA)
    assertEquals((a.outsideWindow, a.outsideScreen, a.total, a.inside), (1, 1, 4, 2))
    assertEquals(a.outsideWindowDuration, Span.micros(300))
    assertEquals(a.outsideScreenDuration, Span.micros(400))
    assertEquals(a.totalDuration, Span.micros(1000))
    assertEquals(a.outsideWindowShare, Some(0.3))
    val b = tally(recallB)
    assert(b.allOutside)
    val work    = get(get(plan(windowed(OffWindowPolicy.Exclude))).prepare(input))
    val preview = get(work.preview)
    assertEquals(preview.windowTallies, work.windowTallies)
    assertEquals(work.windowSummary, preview.windowSummary)
    assertEquals(preview.windowSummary, WindowSummary(3, 1, 10, 2, 1, 4))
  }

  test(
    "under Exclude the map is built from the inside fixations alone, in window coordinates"
  ) {
    val inside = StudyInput(
      Trials(
        trials.updated(0, Trial(recallA, (), path(recallA, (6, 3), (14.5, 7.5))))
      )
    )
    val full    = get(get(plan(windowed(OffWindowPolicy.Exclude))).run(input))
    val trimmed = get(get(plan(windowed(OffWindowPolicy.Exclude), inside)).run(inside))
    def mass(result: StudyResult[StudyKey, Px, Similarity, SignedDifference], key: StudyKey) =
      result.scales.head.estimation.collectFirst { case (`key`, Right(m)) => m.values.toVector }
    val cells = mass(full, recallA).getOrElse(fail("no mass"))
    assertEquals(mass(trimmed, recallA), Some(cells))
    val occupied = Vector(Pt[Px](1, 1), Pt[Px](9.5, 5.5)).flatMap(grid.indexOf)
    assertEquals(cells.zipWithIndex.collect { case (v, i) if v > 0 => i }, occupied.sorted)
    assertEquals(
      full.scales.head.estimation.collect { case (k, Left(f)) => k -> f },
      Vector(recallB -> StudyFailure.OffWindow(recallB, tally(recallB)))
    )
  }

  test(
    "FailTrial fails a trial with any fixation outside the window; all outside always fails"
  ) {
    val result   = get(get(plan(windowed(OffWindowPolicy.FailTrial))).run(input))
    val failures = result.scales.head.estimation.collect { case (k, Left(f)) => k -> f }
    val a        = get(WindowTally.window(window, trials(0).value))
    assertEquals(
      failures,
      Vector(
        recallA -> StudyFailure.OffWindow(recallA, a),
        recallB -> StudyFailure.OffWindow(recallB, tally(recallB))
      )
    )
    assertEquals(Diagnostic.of(failures.head._2).code.render, "study-failure.off-window")
  }

  test("preflight warns for each trial with fixations outside, and runs anyway") {
    val p      = get(plan(windowed(OffWindowPolicy.FailTrial)))
    val report = p.preflight(Some(input))
    assertEquals(
      report.findings,
      Vector(
        StudyFinding.OffWindowFixations(recallA, tally(recallA), OffWindowPolicy.FailTrial),
        StudyFinding.NoFixationInWindow(recallB, tally(recallB))
      )
    )
    assert(report.ready)
    assertEquals(report.warnings.map(_.remedy).distinct, Vector(Remedy.ReviewAnalysisWindow))
    assert(report.findings.head.message.contains("fails at every scale"))
  }

  test("a window covering the whole frame reproduces the whole-frame study bit for bit") {
    val inScreen = StudyInput(
      Trials(trials.map(t => t.copy(value = path(t.key, (3, 3), (11, 7), (17.5, 1.5)))))
    )
    val whole   = get(Subframe.whole(screen, FrameId("everything")))
    val onWhole = get(Grid.over(whole.frame, 5, 3))
    val scales  = Vector(
      StudyScale.Native(StudyEstimate.Binned()),
      StudyScale.Native(StudyEstimate.Gaussian(get(Sigma.px(1.5)), EdgePolicy.Renormalise))
    )
    val a = get(
      get(plan(StudyGeometry.WholeFrame(get(Grid.over(screen, 5, 3))), inScreen, scales))
        .run(inScreen)
    )
    val b = get(
      get(
        plan(
          get(StudyGeometry.windowed(whole, onWhole, OffWindowPolicy.FailTrial)),
          inScreen,
          scales
        )
      ).run(inScreen)
    )
    a.scales.zip(b.scales).foreach { (x, y) =>
      assertEquals(
        x.estimation
          .map((k, m) => k -> m.map(_.values.toVector.map(java.lang.Double.doubleToLongBits))),
        y.estimation
          .map((k, m) => k -> m.map(_.values.toVector.map(java.lang.Double.doubleToLongBits)))
      )
      assertEquals(
        x.contrast.map(_.rows.map(_.difference)),
        y.contrast.map(_.rows.map(_.difference))
      )
    }
  }

  test("a scale in degrees is its declared pixel equivalent, and the description says both") {
    val ppd     = get(LinearAngularScale.of(screen, 2.0))
    val degrees = StudyEstimate.Gaussian[Deg](get(Sigma.deg(0.75)), EdgePolicy.Truncate)
    val pixels  = StudyEstimate.Gaussian[Px](get(Sigma.px(1.5)), EdgePolicy.Truncate)
    val angular = get(
      plan(
        windowed(OffWindowPolicy.Exclude),
        scales = Vector(StudyScale.Angular(degrees)),
        angular = Some(ppd)
      )
    )
    val native =
      get(plan(windowed(OffWindowPolicy.Exclude), scales = Vector(StudyScale.Native(pixels))))
    assertEquals(angular.estimates, Vector(pixels))
    val description = angular.description.toMap
    assertEquals(
      description.get("angularScale"),
      Some(Vector(Provenance.Param.Text("screen"), Provenance.Param.Num(2.0)))
    )
    assertEquals(description.get("scale.0").map(_.head), Some(Provenance.Param.Text("degrees")))
    assert(!native.description.toMap.contains("angularScale"))
    assertEquals(
      get(angular.run(input)).scales.head.contrast.map(_.rows.map(_.difference)),
      get(native.run(input)).scales.head.contrast.map(_.rows.map(_.difference))
    )
    assert(!angular.isVersion1)
    assert(angular.diff(native).map(_.field).contains("scale.0"))
  }

  test("an angular scale needs units per degree on the admission frame") {
    val degrees =
      StudyScale.Angular[Px](StudyEstimate.Gaussian(get(Sigma.deg(1)), EdgePolicy.Truncate))
    assertEquals(
      plan(windowed(OffWindowPolicy.Exclude), scales = Vector(degrees)).left.toOption,
      Some(PlanError.MissingAngularScale(0))
    )
    val onWindow = get(LinearAngularScale.of(window.frame, 2.0))
    assertEquals(
      plan(
        windowed(OffWindowPolicy.Exclude),
        scales = Vector(degrees),
        angular = Some(onWindow)
      ).left.toOption,
      Some(PlanError.Geometry(GeometryError.FrameMismatch(window.frame.id, screen.id)))
    )
  }

  test("a trial in another frame is still a frame failure, never an off-window one") {
    val other = get(Frame.screen("elsewhere", 20, 10))
    val clock = ClockId("elsewhere")
    val moved = get(
      Scanpath.of(
        other,
        clock,
        IArray(
          get(
            Event.Fixation.withoutDispersion(
              get(Interval.of(clock, Instant.micros(0), Instant.micros(10))),
              Pt[Px](6, 3),
              1
            )
          )
        )
      )
    )
    val source = StudyInput(Trials(trials.updated(1, Trial(recallB, (), moved))))
    val result = get(get(plan(windowed(OffWindowPolicy.Exclude), source)).run(source))
    assertEquals(
      result.scales.head.estimation.collect { case (k, Left(f)) => k -> f },
      Vector(
        recallB -> StudyFailure.Frame(recallB, GeometryError.FrameMismatch(screen.id, other.id))
      )
    )
    assertEquals(
      get(get(plan(windowed(OffWindowPolicy.Exclude), source)).prepare(source)).windowTallies
        .map(_._1),
      Vector(recallA, encodeA, encodeB)
    )
  }

  test("a temporal repetition keeps the base plan's window") {
    val base    = get(plan(windowed(OffWindowPolicy.FailTrial)))
    val swapped = get(base.withPhases("encode", "recall", Weight.Uniform))
    assertEquals(swapped.geometry, base.geometry)
    assertEquals(swapped.focalPhase, "encode")
  }

  test("inspection of a completed result carries the same tallies") {
    val p         = get(plan(windowed(OffWindowPolicy.Exclude)))
    val result    = get(p.run(input))
    val inspected = get(ResultInspection.study(p, result, input, None))
    assertEquals(inspected.windowTallies, p.windowTallies(input))
    assertEquals(inspected.windowTally(recallA), Some(tally(recallA)))
  }
