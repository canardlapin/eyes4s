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

import eyes4s.core.*
import eyes4s.design.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px

import scala.annotation.tailrec

/** Items studied two or three times, recalled at chosen occurrences: every
  * matched-reference rule, the same selection applied to the control pool,
  * all-occurrence controls, the RequireOne blocker, refused unmatched trials
  * and conflicting match items.
  */
class OccurrencePairingSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)
  private val frame                         = get(Frame.screen("occurrence", 4, 4))
  private val grid                          = get(Grid.over(frame, 2, 2))
  private val layout                        = TrialKey.layout(TrialKeyDefinitions.trialLayout)

  private def key(phase: String, trial: String, occurrence: Int, item: String) =
    get(TrialKey.of("p1", phase, trial, get(TrialOccurrence.of(occurrence)), item))
  private def enc(trial: String, occurrence: Int, item: String) =
    key("encoding", trial, occurrence, item)
  private def ret(trial: String, occurrence: Int, item: String) =
    key("retrieval", trial, occurrence, item)

  // Item a studied three times, b twice, c once; d never.
  private val a1   = enc("e1", 1, "a")
  private val b1   = enc("e2", 1, "b")
  private val a2   = enc("e3", 2, "a")
  private val c1   = enc("e4", 1, "c")
  private val b2   = enc("e5", 2, "b")
  private val a3   = enc("e6", 3, "a")
  private val r1   = ret("r1", 1, "a")
  private val r2   = ret("r2", 2, "a")
  private val r3   = ret("r3", 2, "b")
  private val r4   = ret("r4", 1, "c")
  private val r5   = ret("r5", 1, "d")
  private val keys = Vector(a1, b1, a2, c1, b2, a3, r1, r2, r3, r4, r5)

  private def path(k: TrialKey, x: Double): Scanpath[Px] =
    val clock = ClockId(s"${k.phase}/${k.trial}")
    get(
      Scanpath.of(
        frame,
        clock,
        IArray(
          get(
            Event.Fixation.withoutDispersion(
              get(Interval.of(clock, Instant.micros(0), Instant.micros(100))),
              Pt[Px](x, 1.5),
              1
            )
          )
        )
      )
    )
  private def inputOf(ks: Vector[TrialKey]) =
    StudyInput(Trials(ks.zipWithIndex.map((k, i) => Trial(k, (), path(k, 0.5 + (i % 4))))))
  private val input = inputOf(keys)

  private def plan(pairing: StudyPairing, source: StudyInput[TrialKey, Px] = input) =
    StudyPlan.configure(
      source.reference,
      layout,
      StudyGeometry.WholeFrame(grid),
      "retrieval",
      "encoding",
      Weight.Duration,
      Vector(StudyScale.Native(StudyEstimate.Binned())),
      None,
      FailurePolicy.RequireAll,
      StudyMethod.cosine[Px](DefinitionId.cosine),
      (),
      pairing
    )
  private def pairing(
      matched: MatchedReferences,
      controls: ControlReferences = ControlReferences.SameSelection,
      unmatched: UnmatchedFocalPolicy = UnmatchedFocalPolicy.ReportNoMatch
  )                      = StudyPairing(matched, controls, unmatched)
  private def at(n: Int) =
    MatchedReferences.Select(OccurrenceChoice.At(get(TrialOccurrence.of(n))))

  private def pairs(schedule: DirectedPairSchedule[TrialKey, TrialKey]) =
    @tailrec
    def loop(
        cursor: PairCursor[TrialKey, TrialKey],
        acc: Vector[(TrialKey, TrialKey)]
    ): Vector[(TrialKey, TrialKey)] =
      get(cursor.advance(PairQuantum.default)) match
        case PairPage.More(ps, _, next) => loop(next, acc ++ ps.map(p => p.left -> p.right))
        case PairPage.Done(ps, _, _)    => acc ++ ps.map(p => p.left -> p.right)
    loop(schedule.start, Vector.empty)

  private def matchedOf(p: StudyPairing) =
    val work = get(get(plan(p)).prepare(input))
    pairs(work.matched).groupMap(_._1)(_._2)
  private def controlsOf(p: StudyPairing, focal: TrialKey) =
    val work = get(get(plan(p)).prepare(input))
    pairs(work.controls).collect { case (`focal`, r) => r }

  test(
    "new plans require one matched reference and block, naming the trials, when there are more"
  ) {
    val p = get(plan(StudyPairing.default))
    assertEquals(p.pairing, StudyPairing.default)
    val report = p.preflight(Some(input))
    val policy = MatchedReferences.RequireOne
    assertEquals(
      report.blockers,
      Vector(
        StudyFinding.MatchedCardinality(r1, Vector(a1, a2, a3), policy),
        StudyFinding.MatchedCardinality(r2, Vector(a1, a2, a3), policy),
        StudyFinding.MatchedCardinality(r3, Vector(b1, b2), policy),
        StudyFinding.AmbiguousReferences(Vector(a1, a2, a3), policy),
        StudyFinding.AmbiguousReferences(Vector(b1, b2), policy)
      )
    )
    assert(
      report.warnings.contains(
        StudyFinding.UnmatchedFocal[TrialKey, Px](r5, UnmatchedKind.Undetermined)
      )
    )
    assertEquals(report.blockers.head.remedy, Remedy.ChooseMatchedReference)
    val work    = get(p.prepare(input))
    val refusal = work.run.left.toOption
    assert(
      refusal.exists {
        case PlanError.MatchedCardinality(MatchedReferences.RequireOne, focal, _) =>
          focal == Vector(r1, r2, r3).map(k => KeyDigest[TrialKey].digest(k).render)
        case _ => false
      },
      s"$refusal"
    )
    assertEquals(get(work.preview).matchedCardinality, work.matchedCardinality)
    assertEquals(get(work.matchedCardinality).multiple.map(_._1), Vector(r1, r2, r3))
    assert(get(work.matchedCardinality).blocking)
    assert(
      !get(
        get(
          get(plan(pairing(MatchedReferences.SameOccurrence))).prepare(input)
        ).matchedCardinality
      ).blocking
    )
  }

  test("SameOccurrence pairs recall n with study n, and narrows the controls alike") {
    val same = pairing(MatchedReferences.SameOccurrence)
    assertEquals(
      matchedOf(same),
      Map(r1 -> Vector(a1), r2 -> Vector(a2), r3 -> Vector(b2), r4 -> Vector(c1))
    )
    assertEquals(controlsOf(same, r1), Vector(b1, c1))
    assertEquals(controlsOf(same, r2), Vector(b2))
    val p = get(plan(same))
    assertEquals(p.preflight(Some(input)).blockers, Vector.empty)
    assert(get(p.run(input)).scales.nonEmpty)
    assertEquals(
      controlsOf(
        pairing(MatchedReferences.SameOccurrence, ControlReferences.AllOccurrences),
        r1
      ),
      Vector(b1, c1, b2)
    )
  }

  test("Select keeps the first, last or given occurrence for matches and controls") {
    val first = pairing(MatchedReferences.Select(OccurrenceChoice.First))
    assertEquals(
      matchedOf(first),
      Map(r1 -> Vector(a1), r2 -> Vector(a1), r3 -> Vector(b1), r4 -> Vector(c1))
    )
    assertEquals(controlsOf(first, r4), Vector(a1, b1))
    val last = pairing(MatchedReferences.Select(OccurrenceChoice.Last))
    assertEquals(
      matchedOf(last),
      Map(r1 -> Vector(a3), r2 -> Vector(a3), r3 -> Vector(b2), r4 -> Vector(c1))
    )
    assertEquals(controlsOf(last, r4), Vector(b2, a3))
    val second = pairing(at(2))
    assertEquals(matchedOf(second), Map(r1 -> Vector(a2), r2 -> Vector(a2), r3 -> Vector(b2)))
    assertEquals(controlsOf(second, r1), Vector(b2))
    assertEquals(
      get(get(get(plan(second)).prepare(input)).matchedCardinality).unmatched,
      Vector(r4, r5)
    )
    assertEquals(
      controlsOf(pairing(at(2), ControlReferences.AllOccurrences), r1),
      Vector(b1, c1, b2)
    )
    Vector(first, last, second).foreach(p => assert(get(plan(p)).run(input).isRight, s"$p"))
  }

  test("MeanOfAll is the explicitly labelled version-1 average, reported as a warning") {
    val mean   = get(plan(pairing(MatchedReferences.MeanOfAll)))
    val report = mean.preflight(Some(input))
    assertEquals(report.blockers, Vector.empty)
    assert(
      report.warnings.contains(
        StudyFinding
          .MatchedCardinality[TrialKey, Px](r1, Vector(a1, a2, a3), MatchedReferences.MeanOfAll)
      )
    )
    assert(mean.run(input).isRight)
    val v1 = get(
      StudyPlan.of(
        input.reference,
        layout,
        grid,
        "retrieval",
        "encoding",
        Weight.Duration,
        Vector(StudyEstimate.Binned()),
        FailurePolicy.RequireAll,
        StudyMethod.cosine[Px](DefinitionId.cosine),
        ()
      )
    )
    assertEquals(v1.pairing, StudyPairing.version1)
    assert(v1.isVersion1)
    assertEquals(mean.description, v1.description)
    assert(get(plan(StudyPairing.default)).diff(v1).map(_.field).contains("pairing"))
  }

  test("Refuse blocks and refuses focal trials without a matched reference") {
    val refuse = get(
      plan(pairing(MatchedReferences.SameOccurrence, unmatched = UnmatchedFocalPolicy.Refuse))
    )
    assertEquals(
      refuse.preflight(Some(input)).blockers,
      Vector(StudyFinding.UnmatchedFocalRefused[TrialKey, Px](r5))
    )
    assertEquals(
      refuse.run(input).left.toOption,
      Some(PlanError.UnmatchedFocalRefused(Vector(KeyDigest[TrialKey].digest(r5).render)))
    )
  }

  test("one trial identity naming two items is a typed conflict") {
    val twin   = ret("r1", 1, "b")
    val source = inputOf(keys :+ twin)
    val p      = get(plan(pairing(MatchedReferences.SameOccurrence), source))
    assertEquals(
      p.preflight(Some(source)).blockers,
      Vector(StudyFinding.MatchItemConflict[TrialKey, Px](Vector(r1, twin)))
    )
    assert(p.run(source).left.toOption.exists {
      case PlanError.MatchItemConflict(groups) => groups.map(_.size) == Vector(2)
      case _                                   => false
    })
  }

  test("an occurrence rule needs a layout that declares occurrences") {
    val studyKeys = StudyInput(
      Trials(Vector(Trial(StudyKey("p", "a", "encoding"), (), path(a1, 0.5))))
    )
    assertEquals(
      StudyPlan
        .configure(
          studyKeys.reference,
          StudyKey.layout(DefinitionId.studyLayout),
          StudyGeometry.WholeFrame(grid),
          "retrieval",
          "encoding",
          Weight.Duration,
          Vector(StudyScale.Native(StudyEstimate.Binned())),
          None,
          FailurePolicy.RequireAll,
          StudyMethod.cosine[Px](DefinitionId.cosine),
          (),
          pairing(MatchedReferences.SameOccurrence)
        )
        .left
        .toOption,
      Some(
        PlanError.OccurrenceUnavailable(
          DefinitionId.studyLayout,
          MatchedReferences.SameOccurrence
        )
      )
    )
    assertEquals(TrialOccurrence.of(0), Left(PlanError.InvalidOccurrence(0)))
    assertEquals(
      TrialKey.of("p", "retrieval", "t", TrialOccurrence.first, " "),
      Left(PlanError.BlankKeyField("item"))
    )
  }

  test("an item studied twice but never recalled still blocks a one-reference control pool") {
    val source = inputOf(Vector(a1, a2, c1, r4))
    val p      = get(plan(StudyPairing.default, source))
    assertEquals(
      p.preflight(Some(source)).blockers,
      Vector(
        StudyFinding
          .AmbiguousReferences[TrialKey, Px](Vector(a1, a2), MatchedReferences.RequireOne)
      )
    )
    assert(p.run(source).isLeft)
    val all =
      get(plan(pairing(MatchedReferences.RequireOne, ControlReferences.AllOccurrences), source))
    assertEquals(all.preflight(Some(source)).blockers, Vector.empty)
    assert(all.run(source).isRight)
  }

  test("a refusal names focal trials and reference groups apart, and omits an empty half") {
    val source  = inputOf(Vector(a1, a2, c1, r4))
    val refused = get(plan(StudyPairing.default, source)).run(source).left.toOption
    val message = refused.map(_.message).getOrElse(fail("not refused"))
    assert(!message.contains("Vector()"), message)
    assert(!message.contains("focal"), message)
    val digest = KeyDigest[TrialKey]
    assert(
      message.contains(Vector(a1, a2).map(digest.digest(_).render).mkString(", ")),
      message
    )
  }

  test("a refusal is the same whatever the input order") {
    val forward  = inputOf(keys)
    val backward = inputOf(keys.reverse)
    assertEquals(
      get(plan(StudyPairing.default, forward)).run(forward).left.toOption,
      get(plan(StudyPairing.default, backward)).run(backward).left.toOption
    )
  }

  test("one trial label at two occurrences, or with two items, is one trial in conflict") {
    val d      = ret("r9", 1, "d")
    val e      = ret("r9", 2, "e")
    val source = inputOf(keys :+ d :+ e)
    val p      = get(plan(pairing(MatchedReferences.SameOccurrence), source))
    assert(p.preflight(Some(source)).blockers.exists {
      case StudyFinding.MatchItemConflict(ks) => ks.toSet == Set(d, e)
      case _                                  => false
    })
    assert(p.run(source).isLeft)
  }
