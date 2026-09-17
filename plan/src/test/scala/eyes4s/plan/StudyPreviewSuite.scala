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

import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

class StudyPreviewSuite extends munit.ScalaCheckSuite:
  private case class Key(person: String, stimulus: String, phase: String, occurrence: Int)
  private given KeyDigest[Key] = KeyDigest.derived[Key]
  private given Ordering[Key]  = Ordering.by(k => (k.person, k.stimulus, k.phase, k.occurrence))
  private def get[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)
  private val layout                            = new StudyLayout[Key](
    DefinitionId.studyLayout,
    Projection.named("person")(_.person),
    Projection.named("stimulus")(_.stimulus),
    Projection.named("phase")(_.phase)
  )
  private val frame = get(Frame.screen("preview", 2, 2))
  private val grid  = get(Grid.over(frame, 2, 2))
  private val clock = ClockId("preview")
  private val path  = get(
    Scanpath.of(
      frame,
      clock,
      IArray(
        get(
          Event.Fixation.withoutDispersion(
            get(Interval.of(clock, Instant.epoch, Instant.micros(100))),
            Pt[Px](0.5, 0.5),
            1
          )
        )
      )
    )
  )
  private def input(keys: Vector[Key]) = StudyInput(Trials(keys.map(k => Trial(k, (), path))))
  private def plan(
      source: StudyInput[Key, Px],
      policy: FailurePolicy = FailurePolicy.RequireAll,
      method: StudyMethod[Unit, Px, Similarity, SignedDifference] =
        StudyMethod.cosine[Px](DefinitionId.cosine),
      keyLayout: StudyLayout[Key] = layout
  ) =
    get(
      StudyPlan.of(
        source.reference,
        keyLayout,
        grid,
        "recall",
        "encode",
        Weight.Duration,
        Vector(StudyEstimate.Binned()),
        policy,
        method,
        ()
      )
    )

  private def collect(
      schedule: DirectedPairSchedule[Key, Key],
      quantum: Int
  ): (Vector[(Key, Key)], PairingReport[Key, Key]) =
    val q = get(PairQuantum.of(quantum))
    @annotation.tailrec
    def loop(
        cursor: PairCursor[Key, Key],
        pairs: Vector[(Key, Key)]
    ): (Vector[(Key, Key)], PairingReport[Key, Key]) =
      get(cursor.advance(q)) match
        case PairPage.More(page, work, next) =>
          assert(work <= quantum)
          assert(page.size <= quantum)
          loop(next, pairs ++ page.map(p => p.left -> p.right))
        case PairPage.Done(page, work, report) =>
          assert(work <= quantum)
          (pairs ++ page.map(p => p.left -> p.right), report)
    loop(schedule.start, Vector.empty)

  // Deliberately no Relation, Pairing or schedule calls in this small exhaustive oracle.
  private def expected(keys: Vector[Key], matched: Boolean): Vector[(Key, Key)] =
    val unique = keys.filter(k => keys.count(_ == k) == 1)
    for
      l <- unique if l.phase == "recall"
      r <- unique if r.phase == "encode"
      if l.person == r.person && ((l.stimulus == r.stimulus) == matched)
    yield l -> r

  private val a1           = Key("p1", "a", "recall", 1)
  private val a2           = a1.copy(occurrence = 2)
  private val b            = Key("p1", "b", "recall", 1)
  private val aRef1        = a1.copy(phase = "encode")
  private val aRef2        = a2.copy(phase = "encode")
  private val bRef         = b.copy(phase = "encode")
  private val other        = a1.copy(person = "p2")
  private val otherRef     = other.copy(phase = "encode")
  private val missing      = a1.copy(person = "p3")
  private val missingRef   = aRef1.copy(person = "p4")
  private val duplicate    = a1.copy(stimulus = "duplicate")
  private val duplicateRef = duplicate.copy(phase = "encode")
  private val excluded     = a1.copy(phase = "practice")
  private val keys         = Vector(
    a1,
    a2,
    b,
    other,
    missing,
    aRef1,
    aRef2,
    bRef,
    otherRef,
    missingRef,
    duplicate,
    duplicate,
    duplicateRef,
    duplicateRef,
    excluded
  )

  test("manual truth table retains occurrences, participant scope, duplicates and exclusions") {
    val source  = input(keys)
    val p       = plan(source, get(FailurePolicy.successfulOnly(2)))
    val work    = get(p.prepare(source))
    val preview = get(work.preview)
    assert(preview.matched eq work.matched)
    assert(preview.controls eq work.controls)
    assertEquals(preview.inputReference, source.reference)
    assertEquals(preview.layoutId, layout.id)
    assertEquals(preview.methodId, p.method.id)
    assertEquals(preview.description, p.description)
    assertEquals(preview.failurePolicy, p.policy)
    assertEquals(preview.reductionOrientation, ReductionOrientation.ByLeft)
    assertEquals(preview.excludedPhases, Vector(excluded))
    assertEquals(preview.focalKeys, keys.filter(_.phase == "recall"))
    assertEquals(preview.referenceKeys, keys.filter(_.phase == "encode"))
    val (matched, report) = collect(preview.matched, 1)
    assertEquals(
      matched,
      Vector(a1 -> aRef1, a1 -> aRef2, a2 -> aRef1, a2 -> aRef2, b -> bRef, other -> otherRef)
    )
    assertEquals(report.eligiblePairCount, 6L)
    assertEquals(report.unmatchedLeft, Vector(missing))
    assertEquals(report.unmatchedRight, Vector(missingRef))
    assertEquals(
      report.ambiguous,
      Vector(
        PairingAmbiguity.DuplicateLeft[Key, Key](duplicate, Vector(5, 6)),
        PairingAmbiguity.DuplicateRight[Key, Key](duplicateRef, Vector(5, 6))
      )
    )
    val (controls, controlsReport) = collect(preview.controls, 2)
    assertEquals(controls, Vector(a1 -> bRef, a2 -> bRef, b -> aRef1, b -> aRef2))
    assertEquals(controlsReport.eligiblePairCount, 4L)
    assertEquals(controlsReport.unmatchedLeft, Vector(other, missing))
    assertEquals(controlsReport.unmatchedRight, Vector(otherRef, missingRef))
  }

  property("bounded pages agree with an independent exhaustive oracle") {
    forAll(Gen.choose(0, 20).flatMap(n => Gen.listOfN(n, Gen.oneOf(keys))), Gen.choose(1, 7)) {
      (rows, quantum) =>
        val source  = input(rows.toVector)
        val preview = get(get(plan(source).prepare(source)).preview)
        Vector(true -> preview.matched, false -> preview.controls).foreach {
          (matched, schedule) =>
            val (pairs, report) = collect(schedule, quantum)
            val oracle          = expected(rows.toVector, matched)
            assertEquals(pairs, oracle)
            assertEquals(report.eligiblePairCount, oracle.size.toLong)
            assertEquals(report.selectedPairCount, oracle.size)
            assertEquals(collect(schedule, 1000), (pairs, report))
        }
        true
    }
  }

  test("empty phases and absent controls return complete zero-count reports") {
    Vector(Vector.empty, Vector(a1), Vector(aRef1), Vector(a1, aRef1)).foreach { rows =>
      val source          = input(rows)
      val preview         = get(get(plan(source).prepare(source)).preview)
      val (pairs, report) = collect(preview.controls, 1)
      assertEquals(pairs, Vector.empty)
      assertEquals(report.eligiblePairCount, 0L)
      assertEquals(report.unmatchedLeft, rows.filter(_.phase == "recall"))
      assertEquals(report.unmatchedRight, rows.filter(_.phase == "encode"))
    }
  }

  test("inspection does no numerical work; failures change success counts, not eligibility") {
    var builds      = 0
    var comparisons = 0
    val failing     = new StudyMethod[Unit, Px, Similarity, SignedDifference](
      DefinitionId.cosine,
      "always fails",
      _ => Vector.empty,
      _ =>
        builds += 1
        new Compare[Mass[Px], Mass[Px], Similarity]:
          val info = Distribution.cosine[Px].info
          def compare(a: Mass[Px], b: Mass[Px]): Either[CompareError, Similarity] =
            comparisons += 1
            Left(CompareError.ZeroNorm("injected", a.values.sum, b.values.sum))
    )
    val source             = input(Vector(a1, b, aRef1, bRef))
    val p                  = plan(source, method = failing)
    val work               = get(p.prepare(source))
    val preview            = get(work.preview)
    val (_, matchedReport) = collect(preview.matched, 1)
    val (_, controlReport) = collect(preview.controls, 1)
    assertEquals(builds, 0)
    assertEquals(comparisons, 0)
    val failed = get(get(work.run).scales.head.contrast)
    val passed = get(get(plan(source).run(source)).scales.head.contrast)
    assertEquals(builds, 1)
    assertEquals(comparisons, 4)
    Vector(
      (failed.matched, passed.matched, matchedReport),
      (failed.control, passed.control, controlReport)
    )
      .foreach { (bad, good, report) =>
        // Reduced analyses retain existential source key types; equality still
        // compares the complete case-class report, including the typed keys.
        assert(bad.source.diagnostics == report)
        assert(good.source.diagnostics == report)
        assertEquals(bad.entries.map(_.successful), Vector(0, 0))
        assertEquals(bad.entries.map(_.contributing), Vector(0, 0))
        assertEquals(good.entries.map(_.successful), Vector(1, 1))
      }
  }

  test("input, layout and policy changes invalidate the preview stamp") {
    val source  = input(keys)
    val p       = plan(source)
    val preview = get(get(p.prepare(source)).preview)
    assertEquals(preview.checkCurrent(p, source), Right(()))
    val reordered = input(keys.reverse)
    assertEquals(
      preview.checkCurrent(plan(reordered), reordered),
      Left(PlanError.ArtifactMismatch(source.reference.digest, reordered.reference.digest))
    )
    assertEquals(
      preview.checkCurrent(plan(source, get(FailurePolicy.successfulOnly(1))), source),
      Left(PlanError.ChangedPreparedPlan(p.method.id, layout.id))
    )
    val renamedLayout = new StudyLayout[Key](
      get(DefinitionId.of("other-layout", 1)),
      layout.participant,
      layout.stimulus,
      layout.phase
    )
    assert(preview.checkCurrent(plan(source, keyLayout = renamedLayout), source).isLeft)
  }

  test("changed declared method parameters refuse new and cached previews") {
    var parameter = 1.0
    val method    = new StudyMethod[Unit, Px, Similarity, SignedDifference](
      DefinitionId.cosine,
      "changing",
      _ => Vector("parameter" -> Provenance.Param.Num(parameter)),
      _ => Distribution.cosine[Px]
    )
    val source  = input(keys)
    val p       = plan(source, method = method)
    val work    = get(p.prepare(source))
    val preview = get(work.preview)
    parameter = 2.0
    assertEquals(
      work.preview.left.toOption,
      Some(PlanError.ChangedPreparedPlan(method.id, layout.id))
    )
    assert(preview.checkCurrent(p, source).isLeft)
  }

  test("selected-pair budget remains an explicit error during preview paging") {
    val source  = input(Vector(a1, a2, aRef1))
    val preview =
      get(get(plan(source).prepare(source, get(PairScheduleBudget.of(3, 4, 1)))).preview)
    assert(preview.matched.start.advance(get(PairQuantum.of(100))).left.exists {
      case PairScheduleError.SelectedBudget(_, 2L, 1) => true
      case _                                          => false
    })
  }
