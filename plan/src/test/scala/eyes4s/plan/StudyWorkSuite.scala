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
import eyes4s.kernel.Unit2D.Px
import eyes4s.surface.EdgePolicy

class StudyWorkSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(e => fail(s"$e"), identity)
  private val frame                         = get(Frame.screen("study-work", 2, 2))
  private val grid                          = get(Grid.over(frame, 2, 2))
  private val a                             = StudyKey("p1", "a", "recall")
  private val b                             = StudyKey("p1", "b", "recall")
  private val ar                            = StudyKey("p1", "a", "encode")
  private val br                            = StudyKey("p1", "b", "encode")
  private def trial(key: StudyKey, x: Double, f: Frame[Px] = frame) =
    val clock = ClockId(key.participant + "/" + key.stimulus + "/" + key.phase)
    val fix   = get(
      Event.Fixation.withoutDispersion(
        get(Interval.of(clock, Instant.micros(0), Instant.micros(1000))),
        Pt[Px](x, 0.5),
        1
      )
    )
    Trial(key, (), get(Scanpath.of(f, clock, IArray(fix))))
  private val input = StudyInput(
    Trials(
      Vector(
        trial(a, 0.5),
        trial(b, 1.5),
        trial(ar, 0.5),
        trial(br, 1.5),
        trial(StudyKey("p2", "c", "excluded"), 0.5)
      )
    )
  )

  private def plan(
      source: StudyInput[StudyKey, Px] = input,
      scales: Vector[StudyEstimate[Px]] = Vector(StudyEstimate.Binned()),
      method: StudyMethod[Unit, Px, Similarity, SignedDifference] =
        StudyMethod.cosine[Px](DefinitionId.cosine)
  ) =
    get(
      StudyPlan.of(
        source.reference,
        StudyKey.layout(DefinitionId.studyLayout),
        grid,
        "recall",
        "encode",
        Weight.Duration,
        scales,
        FailurePolicy.RequireAll,
        method,
        ()
      )
    )

  test(
    "preparation binds scientific identity, preserves source order, and performs no comparison"
  ) {
    var comparisons = 0
    val method      = new StudyMethod[Unit, Px, Similarity, SignedDifference](
      DefinitionId.cosine,
      "observed",
      _ => Vector.empty,
      _ =>
        comparisons += 1
        Distribution.cosine[Px]
    )
    val p    = plan(method = method)
    val work = get(p.prepare(input))
    assertEquals(comparisons, 0)
    assertEquals(work.description, p.description)
    assertEquals(work.methodId, p.method.id)
    assertEquals(work.layoutId, p.layout.id)
    assertEquals(work.inputReference, input.reference)
    assertEquals(work.focalIndices, Vector(0, 1))
    assertEquals(work.referenceIndices, Vector(2, 3))
    assertEquals(work.excludedPhases, Vector(input.trials.rows.last.key))
    assertEquals(work.candidateVisitsAcrossScales, 8L)
    val result = get(work.run)
    assertEquals(comparisons, 1)
    val contrast = get(result.scales.head.contrast)
    assertEquals(
      contrast.rows.map(r => r.key -> get(r.difference).value),
      Vector(a -> 1.0, b -> 1.0)
    )
    assertEquals(contrast.rows.map(_.control.map(_.contributing)), Vector(Some(1), Some(1)))
  }

  test("source permutation changes identity and traversal order without relabelling keys") {
    val reversed = StudyInput(Trials(input.trials.rows.reverse))
    assertNotEquals(reversed.reference, input.reference)
    assert(plan().prepare(reversed).isLeft)
    val work = get(plan(reversed).prepare(reversed))
    assertEquals(work.focalIndices.map(i => reversed.trials.rows(i).key), Vector(b, a))
    val result = get(get(work.run).scales.head.contrast)
    assertEquals(result.matched.entries.map(_.key), Vector(b, a))
    assertEquals(
      result.rows.map(_.key),
      Vector(a, b)
    ) // Contrast retains its canonical key ordering.
  }

  test("frame failures remain trial outcomes at every scale and excluded phase is retained") {
    val other  = get(Frame.screen("different-address", 2, 2))
    val source = StudyInput(
      Trials(Vector(trial(a, 0.5, other), trial(b, 1.5), trial(ar, 0.5), trial(br, 1.5)))
    )
    val p = plan(
      source,
      Vector(
        StudyEstimate.Binned(),
        StudyEstimate.Gaussian(get(Sigma.px(0.5)), EdgePolicy.Truncate)
      )
    )
    val work = get(p.prepare(source))
    assert(work.frameChecks(0).isLeft)
    assert(work.frameChecks.drop(1).forall(_.isRight))
    val result = get(work.run)
    assertEquals(result.scales.size, 2)
    result.scales.foreach { scale =>
      assertEquals(scale.estimation.map(_._1), Vector(a, b, ar, br))
      assert(scale.estimation.head._2.isLeft)
      assertEquals(scale.excludedPhases, Vector.empty)
      val rows = get(scale.contrast).rows
      assert(rows.head.difference.isLeft)
      assert(rows(1).difference.isRight)
    }
  }

  test("candidate visits include both designs and scales and are refused before preparation") {
    val budget = get(PairScheduleBudget.of(5, 7L, 10))
    assertEquals(
      plan().prepare(input, budget).left.toOption,
      Some(PlanError.StudyWorkBudget(2, 2, 1, 7L))
    )
    val smallRows = get(PairScheduleBudget.of(4, 100, 10))
    assertEquals(
      plan().prepare(input, smallRows).left.toOption,
      Some(PlanError.Schedule(PairScheduleError.SourceBudget(5, 0, 4)))
    )
    val scales = Vector(
      StudyEstimate.Binned(),
      StudyEstimate.Gaussian(get(Sigma.px(1.0)), EdgePolicy.Truncate)
    )
    assertEquals(get(plan(scales = scales).prepare(input)).candidateVisitsAcrossScales, 16L)
  }

  test("single-trial estimates equal every runner density at every configured scale") {
    val scales = Vector(
      StudyEstimate.Binned[Px](),
      StudyEstimate.Gaussian(get(Sigma.px(0.5)), EdgePolicy.Truncate),
      StudyEstimate.Anisotropic(get(Sigma.px(0.3)), get(Sigma.px(0.7)), EdgePolicy.Truncate)
    )
    val work   = get(plan(scales = scales).prepare(input))
    val result = get(work.run)
    result.scales.foreach { scale =>
      scale.estimation.foreach { case (key, mass) =>
        assertEquals(
          work.estimate(key, scale.estimate),
          mass.flatMap(m => DensityView.of(m).left.map(StudyFailure.Frame(key, _)))
        )
      }
    }
    val chosen = get(work.estimate(a, StudyEstimate.Binned()))
    assertEquals(chosen.cells.toVector, Vector(1.0, 0.0, 0.0, 0.0))
  }

  test(
    "single-trial estimates refuse unknown and repeated full keys without selecting an occurrence"
  ) {
    val unknown = StudyKey("missing", "a", "recall")
    assertEquals(
      get(plan().prepare(input)).estimate(unknown, StudyEstimate.Binned()),
      Left(StudyFailure.TrialCardinality(unknown, 0))
    )
    val repeated = StudyInput(Trials(Vector(trial(a, 0.5), trial(a, 1.5), trial(ar, 0.5))))
    assertEquals(
      get(plan(repeated).prepare(repeated)).estimate(a, StudyEstimate.Binned()),
      Left(StudyFailure.TrialCardinality(a, 2))
    )
  }

  test(
    "single-trial estimates preserve frame failures, initial policy and window rendering geometry"
  ) {
    val other      = get(Frame.screen("estimate-other", 2, 2))
    val mismatched = StudyInput(Trials(Vector(trial(a, 0.5, other), trial(ar, 0.5))))
    val work       = get(plan(mismatched).prepare(mismatched))
    assertEquals(
      work.estimate(a, StudyEstimate.Binned()).left.toOption,
      get(work.run).scales.head.estimation.head._2.left.toOption
    )
    val window = get(
      Subframe.of(frame, FrameId("estimate-window"), get(Bounds.of[Px](0.5, 0.0, 1.5, 2.0)))
    )
    val windowGrid                                     = get(Grid.over(window.frame, 2, 2))
    val angular                                        = get(LinearAngularScale.of(frame, 2.0))
    def windowPlan(initial: InitialFixationPolicy[Px]) = get(
      StudyPlan.configure(
        input.reference,
        StudyKey.layout(DefinitionId.studyLayout),
        get(StudyGeometry.windowed(window, windowGrid, OffWindowPolicy.Exclude)),
        "recall",
        "encode",
        Weight.Duration,
        Vector(StudyScale.Native(StudyEstimate.Binned[Px]())),
        Some(angular),
        FailurePolicy.RequireAll,
        StudyMethod.cosine[Px](DefinitionId.cosine),
        (),
        initialFixations = initial
      )
    )
    val kept = get(windowPlan(InitialFixationPolicy.keepAll).prepare(input))
    val view = get(kept.estimate(a, StudyEstimate.Binned()))
    assertEquals(view.geometry.admissionFrame, frame)
    assertEquals(view.geometry.origin, Pt[Px](0.5, 0.0))
    assertEquals(view.geometry.cellDegrees, Some(get(Extent.of[Unit2D.Deg](0.25, 0.5))))
    val dropped = get(windowPlan(InitialFixationPolicy.dropFirst).prepare(input))
    assertEquals(
      dropped.estimate(a, StudyEstimate.Binned()).left.toOption,
      get(dropped.run).scales.head.estimation.head._2.left.toOption
    )
    assert(dropped.estimate(a, StudyEstimate.Binned()).isLeft)
  }

  test("a changed custom parameter description invalidates already prepared work") {
    var versionedValue = 1.0
    val method         = new StudyMethod[Unit, Px, Similarity, SignedDifference](
      DefinitionId.cosine,
      "tracked",
      _ => Vector("value" -> Provenance.Param.Num(versionedValue)),
      _ => Distribution.cosine[Px]
    )
    val p    = plan(method = method)
    val work = get(p.prepare(input))
    versionedValue = 2.0
    assertEquals(
      work.estimate(a, StudyEstimate.Binned()),
      Left(
        StudyFailure.PreparedPlan(a, PlanError.ChangedPreparedPlan(p.method.id, p.layout.id))
      )
    )
    assertEquals(
      work.run.left.toOption,
      Some(PlanError.ChangedPreparedPlan(p.method.id, p.layout.id))
    )
  }
