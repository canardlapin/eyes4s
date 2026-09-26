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

/** A study under an initial-fixation policy: which fixations are dropped,
  * how they are counted, that focal and reference trials are treated alike,
  * and what the plan refuses.
  */
class InitialFixationSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)
  private val screen                        = get(Frame.screen("screen", 20, 10))
  private val grid                          = get(Grid.over(screen, 10, 5))
  private val degrees                       = get(LinearAngularScale.of(screen, 2.0))
  private val cross                         = Pt[Px](10, 5)

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

  // The cross is (10, 5) and 1.5 degrees are 3 pixels. recallA starts on the
  // cross, then exactly 3 px away (within the closed disc), leaves, and
  // returns; encodeA starts 3.5 px away; encodeB never leaves the disc.
  private val trials = Vector(
    Trial(recallA, (), path(recallA, (10, 5), (13, 5), (2, 2), (10, 6))),
    Trial(recallB, (), path(recallB, (4, 4), (16, 6))),
    Trial(encodeA, (), path(encodeA, (13.5, 5), (10, 5), (3, 8))),
    Trial(encodeB, (), path(encodeB, (10, 5), (11, 6)))
  )
  private val input = StudyInput(Trials(trials))

  private def plan(
      policy: InitialFixationPolicy[Px],
      focal: String = "recall",
      reference: String = "encode",
      angular: Option[LinearAngularScale[Px]] = Some(degrees)
  ) = StudyPlan.configure(
    input.reference,
    StudyKey.layout(DefinitionId.studyLayout),
    StudyGeometry.WholeFrame(grid),
    focal,
    reference,
    Weight.Duration,
    Vector(StudyScale.Native(StudyEstimate.Binned())),
    angular,
    FailurePolicy.RequireAll,
    StudyMethod.cosine[Px](DefinitionId.cosine),
    (),
    StudyPairing.default,
    policy
  )

  private val nearCross =
    get(InitialFixationPolicy.dropLeadingInClosedDisc(cross, 1.5))

  private def dropped(p: StudyPlan[StudyKey, Px, Unit, Similarity, SignedDifference]) =
    p.initialFixationTallies(input).map((k, t) => k -> get(t).dropped)

  test("keep all drops nothing and describes nothing; drop first drops one per trial") {
    val keep = get(plan(InitialFixationPolicy.keepAll))
    assertEquals(dropped(keep).map(_._2), Vector(0, 0, 0, 0))
    assert(!keep.description.exists(_._1 == "initialFixations"))
    val first = get(plan(InitialFixationPolicy.dropFirst))
    assertEquals(dropped(first).map(_._2), Vector(1, 1, 1, 1))
    assertEquals(
      first.description.last,
      "initialFixations" -> Vector(Provenance.Param.Text("dropFirst"))
    )
  }

  test("the leading run within the closed disc is dropped; a later return is kept") {
    val p = get(plan(nearCross))
    assertEquals(
      dropped(p),
      Vector(recallA -> 2, recallB -> 0, encodeA -> 0, encodeB -> 2)
    )
    val a = get(p.initialFixationTallies(input).head._2)
    assertEquals((a.dropped, a.kept, a.total), (2, 2, 4))
    assertEquals(a.droppedDuration, Span.micros(300))
    assertEquals(a.totalDuration, Span.micros(1000))
    assertEquals(a.keptDuration, Span.micros(700))
    assertEquals(
      p.description.last,
      "initialFixations" -> Vector(
        Provenance.Param.Text("dropLeadingInClosedDisc"),
        Provenance.Param.Num(10),
        Provenance.Param.Num(5),
        Provenance.Param.Num(1.5)
      )
    )
    assert(p.inspect.isRight, p.inspect.toString)
    assert(p.inspect.toOption.get.fields.exists(_.info.id == "initialFixations"))
    assertEquals(
      nearCross.methods,
      "Fixations at the start of every trial, query and reference alike, whose centres lay " +
        "within 1.5° of the fixation cross at (10, 5) were excluded, up to the first fixation " +
        "farther from it."
    )
  }

  test("a trial left without fixations fails at every scale; the others are mapped") {
    val p      = get(plan(nearCross))
    val result = get(p.run(input))
    val failed = result.scales.head.estimation.collect { case (k, Left(f)) => k -> f }
    assertEquals(
      failed,
      Vector(
        encodeB -> StudyFailure.InitialFixations(
          encodeB,
          InitialFixationError.NoFixationKept(2, 300L)
        )
      )
    )
    assertEquals(Diagnostic.of(failed.head._2).code.render, "study-failure.initial-fixations")
    val work = get(p.prepare(input))
    assertEquals(work.initialFixationTallies, p.initialFixationTallies(input))
    assertEquals(
      get(work.preview).initialFixationSummary,
      InitialFixationSummary(4, 11, 2, 1, 4, 0)
    )
    // Window tallies count only the kept fixations.
    assertEquals(
      p.windowTallies(input).map((k, t) => k -> get(t).total),
      Vector(recallA -> 2, recallB -> 2, encodeA -> 3, encodeB -> 0)
    )
  }

  test("each trial is mapped from its kept fixations, whether it is focal or reference") {
    def masses(p: StudyPlan[StudyKey, Px, Unit, Similarity, SignedDifference]) =
      get(p.run(input)).scales.head.estimation.collect { case (k, Right(m)) =>
        k -> m.values.toVector.map(java.lang.Double.doubleToLongBits)
      }.toMap
    val forward = masses(get(plan(nearCross)))
    val swapped = masses(get(plan(nearCross, focal = "encode", reference = "recall")))
    assertEquals(forward, swapped)
    // recallA's map is exactly the map of its last two fixations.
    val trimmed = StudyInput(
      Trials(trials.updated(0, Trial(recallA, (), path(recallA, (2, 2), (10, 6)))))
    )
    val kept = get(
      StudyPlan
        .configure(
          trimmed.reference,
          StudyKey.layout(DefinitionId.studyLayout),
          StudyGeometry.WholeFrame(grid),
          "recall",
          "encode",
          Weight.Uniform,
          Vector(StudyScale.Native(StudyEstimate.Binned())),
          None,
          FailurePolicy.RequireAll,
          StudyMethod.cosine[Px](DefinitionId.cosine),
          ()
        )
        .flatMap(_.run(trimmed))
    ).scales.head.estimation.collectFirst { case (`recallA`, Right(m)) => m.values.toVector }
    val uniform = get(
      get(plan(nearCross)).revise(
        Vector(StudyChange.Weighting(Weight.Duration, Weight.Uniform))
      )
    )
    val ours = get(uniform.run(input)).scales.head.estimation.collectFirst {
      case (`recallA`, Right(m)) => m.values.toVector
    }
    assertEquals(ours, kept)
  }

  test("prepared work, preflight and inspection report the policy's tallies") {
    val p    = get(plan(nearCross))
    val work = get(p.prepare(input))
    assertEquals(work.initialFixations, nearCross)
    assertEquals(work.initialFixationSummary, InitialFixationSummary(4, 11, 2, 1, 4, 0))
    // encodeB is emptied by the policy, not by the window: preflight does not
    // blame the window for it.
    assert(
      !p.preflight(Some(input)).findings.exists {
        case StudyFinding.NoFixationInWindow(key, _) => key == encodeB
        case _                                       => false
      }
    )
    val result     = get(p.run(input))
    val inspection =
      ResultInspection.study(p, result, input, None).fold(e => fail(s"$e"), identity)
    assertEquals(inspection.initialFixationTally(recallA).map(_.dropped), Some(2))
    assertEquals(inspection.initialFixationTally(StudyKey("p9", "z", "recall")), None)
  }

  test("every change states the value it starts from") {
    val base   = get(plan(InitialFixationPolicy.keepAll))
    val image  = get(Subframe.of(screen, FrameId("image"), get(Bounds.of[Px](5, 2, 15, 8))))
    val onIt   = get(Grid.over(image.frame, 5, 3))
    val window = get(
      base.revise(
        Vector(
          StudyChange.Grid(base.grid, onIt),
          StudyChange.Window(None, Some(image)),
          StudyChange.OffWindow(None, Some(OffWindowPolicy.FailTrial)),
          StudyChange.Controls(
            ControlReferences.SameSelection,
            ControlReferences.AllOccurrences
          )
        )
      )
    )
    assertEquals(
      base.structuralDiff(window).map(_.stated),
      Vector(s"10×5 (${grid.id.name})", "the whole frame", "none", "SameSelection")
    )
    assertEquals(
      window.structuralDiff(base).map(_.stated),
      Vector(
        s"5×3 (${onIt.id.name})",
        "image [5, 15) × [2, 8) px",
        "FailTrial",
        "AllOccurrences"
      )
    )
  }

  test("a change of layout or method is typed, inverted and rendered by identity") {
    val base         = get(plan(InitialFixationPolicy.keepAll))
    val otherLayout  = StudyKey.layout(get(DefinitionId.of("eyes4s.other-layout", 1)))
    val otherCosine  = StudyMethod.cosine[Px](get(DefinitionId.of("eyes4s.cosine-variant", 2)))
    val relaid       = get(base.revise(Vector(StudyChange.Layout(base.layout, otherLayout))))
    val remethodised = get(
      base.revise(Vector(StudyChange.Method(base.method, (), otherCosine, ())))
    )
    val layout = base.structuralDiff(relaid)
    assertEquals(layout.map(_.field), Vector(StudyField.Layout))
    assertEquals(
      layout.head.render,
      "layout eyes4s.participant-stimulus-phase@1 → eyes4s.other-layout@1"
    )
    assertEquals(relaid.structuralDiff(base), layout.map(_.inverse))
    val method = base.structuralDiff(remethodised)
    assertEquals(method.map(_.field), Vector(StudyField.Method))
    assertEquals(method.head.render, "method eyes4s.cosine@1 → eyes4s.cosine-variant@2")
    assertEquals(remethodised.structuralDiff(base), method.map(_.inverse))
    assertEquals(
      relaid.revise(layout),
      Left(
        StudyRevisionError.Stale(
          StudyField.Layout,
          "eyes4s.participant-stimulus-phase@1",
          "eyes4s.other-layout@1"
        )
      )
    )
    assertEquals(
      remethodised.revise(method).left.map(_.message),
      Left(
        "The change of method starts from eyes4s.cosine@1 none, but the plan has " +
          "eyes4s.cosine-variant@2 none."
      )
    )
  }

  test("the plan refuses a cross off the frame, a non-positive radius and a missing scale") {
    assertEquals(
      InitialFixationPolicy.dropLeadingInClosedDisc(cross, 0.0),
      Left(InitialFixationError.NonPositiveRadius(0.0))
    )
    InitialFixationPolicy.dropLeadingInClosedDisc(Pt[Px](Double.NaN, 1), 1.0) match
      case Left(InitialFixationError.NonFiniteCross(x, y)) => assert(x.isNaN && y == 1.0)
      case other                                           => fail(s"accepted $other")
    val off = get(InitialFixationPolicy.dropLeadingInClosedDisc(Pt[Px](20, 5), 1.0))
    assertEquals(
      plan(off).left.toOption,
      Some(PlanError.InitialFixations(InitialFixationError.CrossOffFrame(20, 5, screen.id)))
    )
    assertEquals(
      plan(nearCross, angular = None).left.toOption,
      Some(PlanError.InitialFixations(InitialFixationError.MissingAngularScale(1.5)))
    )
    assertEquals(
      Diagnostic
        .of(PlanError.InitialFixations(InitialFixationError.MissingAngularScale(1.5)))
        .causes
        .map(_.code.render),
      Vector("initial-fixation.missing-angular-scale")
    )
    assertEquals(
      InitialFixationTally.of(3, 2, Span.micros(10), Span.micros(20)),
      Left(InitialFixationError.InvalidTally(3, 2, 10L, 20L))
    )
    // A tally partitions its trial: none dropped is no dropped time, all
    // dropped is all of it.
    assertEquals(
      InitialFixationTally.of(0, 2, Span.micros(5), Span.micros(20)),
      Left(InitialFixationError.InvalidTally(0, 2, 5L, 20L))
    )
    assertEquals(
      InitialFixationTally.of(2, 2, Span.micros(5), Span.micros(20)),
      Left(InitialFixationError.InvalidTally(2, 2, 5L, 20L))
    )
    assert(InitialFixationTally.of(1, 2, Span.micros(5), Span.micros(20)).isRight)
  }
