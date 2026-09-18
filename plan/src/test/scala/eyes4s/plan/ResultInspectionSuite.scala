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

import cats.syntax.all.*
import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.detect.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.{Deg, Px}

/** Two score components with different units and directions. */
final case class OverlapSpread(overlap: Double, spread: Double) derives CanEqual
final case class OverlapSpreadDifference(overlap: Double, spread: Double) derives CanEqual
object OverlapSpread:
  given ScoreMean[OverlapSpread] with
    def mean(values: Vector[OverlapSpread]): Either[ScoreMeanError, OverlapSpread] =
      for
        overlap <- ScoreMean[Double].mean(values.map(_.overlap))
        spread  <- ScoreMean[Double].mean(values.map(_.spread))
      yield OverlapSpread(overlap, spread)
  given Contrastable[OverlapSpread, OverlapSpreadDifference] with
    val components = Vector("overlap", "spread")
    def subtract(
        matched: OverlapSpread,
        control: OverlapSpread
    ): Either[DifferenceError, OverlapSpreadDifference] =
      for
        overlap <- SignedDifference.between(matched.overlap, control.overlap, "overlap")
        spread  <- SignedDifference.between(matched.spread, control.spread, "spread")
      yield OverlapSpreadDifference(overlap.value, spread.value)

/** Result inspection reads a completed study and its sources: typed
  * references, drill-down to ledger records, membership, paging, failures and
  * explicit missing locators. No scientific value is recomputed.
  */
class ResultInspectionSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(e => fail(s"$e"), identity)

  // ---------------------------------------------------------------- fixtures

  private val frame   = get(Frame.screen("inspection", 2, 2))
  private val shifted = get(Frame.screen("inspection-shifted", 2, 2))
  private val grid    = get(Grid.over(frame, 2, 2))
  private val a       = StudyKey("p1", "a", "recall")
  private val b       = StudyKey("p1", "b", "recall")
  private val ar      = StudyKey("p1", "a", "encode")
  private val br      = StudyKey("p1", "b", "encode")
  private val cr      = StudyKey("p1", "c", "encode")
  private val lone    = StudyKey("p2", "c", "recall")
  private val other   = StudyKey("p1", "a", "practice")
  private val dropped = StudyKey("p3", "a", "recall")

  private def clock(key: StudyKey): ClockId =
    ClockId(key.participant + "/" + key.stimulus + "/" + key.phase)
  private def interval(key: StudyKey, from: Long, until: Long) =
    get(Interval.of(clock(key), Instant.micros(from), Instant.micros(until)))
  private def trial(key: StudyKey, xs: Vector[Double], f: Frame[Px] = frame) =
    val fixes = xs.zipWithIndex.map { case (x, i) =>
      get(
        Event.Fixation.withoutDispersion(
          interval(key, i * 1000L, i * 1000L + 1000L),
          Pt[Px](x, 0.5),
          1
        )
      )
    }
    Trial(key, (), get(Scanpath.of(f, clock(key), IArray.from(fixes))))

  private def rows(failing: Boolean) = Vector(
    trial(a, Vector(0.5, 1.5)),
    trial(b, Vector(1.5, 1.5)),
    trial(ar, Vector(0.5, 0.5)),
    trial(br, Vector(1.5, 0.5), if failing then shifted else frame),
    trial(cr, Vector(0.5, 1.5)),
    trial(lone, Vector(0.5)),
    trial(other, Vector(0.5))
  )
  private val input   = StudyInput(Trials(rows(failing = false)))
  private val failing = StudyInput(Trials(rows(failing = true)))

  /** Records interleave trials and ordinals are not record-ordered, so a
    * fixation's record follows ordinal rank, never record or row position.
    */
  private val admittedRecords: Vector[(Int, StudyKey, Int)] = Vector(
    (2, ar, 1),
    (3, a, 5),
    (4, ar, 0),
    (5, b, 0),
    (6, a, 2),
    (7, br, 0),
    (9, br, 1),
    (10, b, 1),
    (12, cr, 0),
    (13, cr, 1),
    (14, lone, 0),
    (15, other, 0)
  )
  private val header = Vector("participant", "image", "phase", "fixation", "x", "onset")
  private def raw(key: StudyKey, ordinal: Int) =
    Vector(key.participant, key.stimulus, key.phase, ordinal.toString, "0.5", "0")
  private val rejectedTime =
    AdmissionReason.Time("0", "-1", "microseconds", "duration must be positive")
  private val ledgerRecords: Vector[SourceRecord[StudyKey]] =
    (admittedRecords.map { case (record, key, ordinal) =>
      SourceRecord(record, Disposition.Admitted(key, ordinal))
    } ++ Vector(
      SourceRecord(8, Disposition.Rejected(raw(dropped, 0), Some(dropped), rejectedTime)),
      SourceRecord(
        11,
        Disposition.Rejected(
          raw(dropped, 1),
          Some(dropped),
          AdmissionReason.Quarantined(Vector(8, 11), QuarantineCause.RejectedRecords)
        )
      )
    )).sortBy(_.record)
  private val source = SourceRef.of(
    "inspection.csv",
    header,
    ledgerRecords.map(r =>
      r.disposition match
        case Disposition.Admitted(key, ordinal) => raw(key, ordinal)
        case Disposition.Rejected(fields, _, _) => fields
    )
  )
  private val ledger = get(
    AdmissionLedger.decide(source, header, ledgerRecords, AdmissionDecision.ReviewExclusions)
  )

  private val cosine = StudyMethod.cosine[Px](DefinitionId.cosine)
  private def plan[P, S, D](
      on: StudyInput[StudyKey, Px],
      policy: FailurePolicy = FailurePolicy.RequireAll,
      method: StudyMethod[P, Px, S, D] = cosine,
      parameters: P = (),
      layout: StudyLayout[StudyKey] = StudyKey.layout(DefinitionId.studyLayout),
      scales: Vector[StudyEstimate[Px]] = Vector(StudyEstimate.Binned())
  ) = get(
    StudyPlan.of(
      on.reference,
      layout,
      grid,
      "recall",
      "encode",
      Weight.Duration,
      scales,
      policy,
      method,
      parameters
    )
  )
  private val successfulOnly = get(FailurePolicy.successfulOnly(1))

  private def inspect[P, S, D](
      p: StudyPlan[StudyKey, Px, P, S, D],
      on: StudyInput[StudyKey, Px],
      withLedger: Boolean = true
  ): (StudyResult[StudyKey, Px, S, D], StudyInspection[StudyKey, Px, S, D]) =
    val result = get(p.run(on))
    result -> get(ResultInspection.study(p, result, on, Option.when(withLedger)(ledger)))

  /** Every entry of a listing, reached only through reference-keyed pages. */
  private def whole[K, A](listing: Listing[K, A], size: Int): Vector[A] =
    val pageSize = get(PageSize.of(size))
    Iterator
      .iterate(Option(listing.first(pageSize)))(
        _.flatMap(_.next).map(ref => get(listing.page(ref, pageSize)))
      )
      .takeWhile(_.isDefined)
      .flatten
      .flatMap(_.entries)
      .toVector

  private def all[A](listing: Listing[StudyKey, A]): Vector[A] =
    whole(listing, PageSize.maximum)

  /** Every source record of a trial, in record order. */
  private def records(key: StudyKey, inspection: StudyInspection[StudyKey, Px, ?, ?]) =
    get(inspection.trial(key)).records

  private def status(member: Member[StudyKey]): String = member.status match
    case Membership.Contributing  => "contributing"
    case Membership.FailedPair(_) => "failed"
    case Membership.Withheld(_)   => "withheld"

  // ---------------------------------------------------------------- tests

  test("a projected pair recovers exactly its focal, reference and source-ledger entries") {
    val (result, inspection) = inspect(plan(input), input)
    val ref                  = ResultRef.PairRow(0, StudyDesign.Matched, a, ar)
    val pair                 = get(inspection.pair(ref))
    assertEquals((pair.focal, pair.reference, pair.design), (a, ar, StudyDesign.Matched))
    val stored = result.scales.head.analyses.matchedSource.rows
      .find(row => row.left == a && row.right == ar)
      .map(_.result)
    val view  = get(pair.outcome.left.map(_.message))
    val score = view.value
    assertEquals(stored, Some(Right(score)))
    assertEquals(
      view.components,
      Vector(
        ComponentValue(
          "value",
          ParameterUnits.Dimensionless,
          MeasureScale.Bounded(0, 1),
          ScoreDirection.HigherIsCloser,
          score.value
        )
      )
    )
    assertEquals(
      get(inspection.trial(a)).admitted,
      Vector(FixationSource(a, 0, 6, 2), FixationSource(a, 1, 3, 5))
    )
    assertEquals(
      get(inspection.trial(ar)).admitted,
      Vector(FixationSource(ar, 0, 4, 0), FixationSource(ar, 1, 2, 1))
    )
    assertEquals(inspection.sources.fixation(a, 1), Right(FixationSource(a, 1, 3, 5)))
    assertEquals(inspection.sources.trialOf(3), Some(a))
    assertEquals(
      inspection.sources.record(4).map(_.disposition),
      Some(Disposition.Admitted(ar, 0))
    )
    assertEquals(
      inspection.sources.locate(ref.loci),
      Vector(3, 6, 2, 4).map(SourceLink.Record(source, _))
    )
  }

  test("a contrast row drills down to its reductions, pairs, trials and records") {
    val (_, inspection) = inspect(plan(input), input)
    val scale           = get(inspection.scale(0))
    val row             = get(inspection.contrastRow(ResultRef.ContrastRow(0, a)))
    assertEquals(row.matched, Some(ResultRef.Reduction(0, StudyDesign.Matched, a)))
    val control = get(inspection.reduction(get(row.control.toRight("control"))))
    assertEquals(control.key, a)
    assertEquals(
      control.members.map(_.pair),
      Vector(
        ResultRef.PairRow(0, StudyDesign.Control, a, br),
        ResultRef.PairRow(0, StudyDesign.Control, a, cr)
      )
    )
    assertEquals(control.contributors, Vector(br, cr))
    val drilled = control.members.flatMap { member =>
      val pair = get(inspection.pair(member.pair))
      records(pair.reference, inspection)
    }
    assertEquals(drilled, Vector(7, 9, 12, 13))
    assertEquals(scale.pairing(StudyDesign.Control).unmatchedLeft, Vector(lone))
    assertEquals(scale.excludedPhases, Vector(other))
  }

  test("aggregate membership keeps contributing keys and explicit exclusions distinct") {
    val (result, lenient) = inspect(plan(failing, successfulOnly), failing)
    val control = get(lenient.reduction(ResultRef.Reduction(0, StudyDesign.Control, a)))
    assertEquals(control.key, a)
    assertEquals(control.contributors, Vector(cr))
    assertEquals(control.exclusions.map(_.reference), Vector(br))
    assertEquals(control.members.map(status), Vector("failed", "contributing"))
    val excluded = control.exclusions.head.status match
      case Membership.FailedPair(d) => d
      case other                    => fail(s"expected a failed pair, got $other")
    assertEquals(excluded.code.render, "study-failure.frame")
    assertEquals(
      excluded.subject,
      Vector(
        Locus.Scale(0),
        Locus.Design(StudyDesign.Control),
        Locus.Pair(a, br),
        Locus.Trial(br)
      )
    )
    assertEquals(excluded.sources, Vector(7, 9).map(SourceLink.Record(source, _)))
    val stored = result.scales.head.analyses.control.entries.find(_.key == a).get
    assertEquals(
      (control.selected, control.successful, control.failed, control.contributing),
      (stored.selected, stored.successful, stored.failed, stored.contributing)
    )
    assertEquals(control.members.size, control.selected)
    assertEquals(control.contributors.size, control.contributing)

    val (_, strict) = inspect(plan(failing), failing)
    val refused     = get(strict.reduction(ResultRef.Reduction(0, StudyDesign.Control, a)))
    assertEquals(refused.outcome.left.map(_.code.render), Left("reduction.failed-scores"))
    assertEquals(refused.contributors, Vector.empty)
    assertEquals(
      refused.members.map(m => m.reference -> status(m)),
      Vector(br -> "failed", cr -> "withheld")
    )
    refused.members(1).status match
      case Membership.Withheld(why) => assertEquals(Left(why), refused.outcome)
      case other                    => fail(s"expected withheld, got $other")
    assertEquals(refused.contributing, 0)
  }

  test("repeated keys with identical display text keep distinct typed references") {
    def label(key: StudyKey) = s"${key.participant}/${key.stimulus}/${key.phase}"
    val slashA               = StudyKey("p1", "a/b", "recall")
    val slashB               = StudyKey("p1/a", "b", "recall")
    val slashAr              = StudyKey("p1", "a/b", "encode")
    val slashBr              = StudyKey("p1/a", "b", "encode")
    assertEquals(label(slashA), label(slashB))
    val collide = StudyInput(
      Trials(
        Vector(
          trial(slashA, Vector(0.5)),
          trial(slashB, Vector(1.5)),
          trial(slashAr, Vector(0.5)),
          trial(slashBr, Vector(0.5))
        )
      )
    )
    val collideLedger = get(
      AdmissionLedger.decide(
        SourceRef.of("collide.csv", header, Vector.fill(4)(Vector("x"))),
        header,
        Vector(slashA, slashB, slashAr, slashBr).zipWithIndex.map { case (key, i) =>
          SourceRecord(i + 2, Disposition.Admitted(key, 0))
        },
        AdmissionDecision.RequireComplete
      )
    )
    val p          = plan(collide)
    val result     = get(p.run(collide))
    val inspection = get(ResultInspection.study(p, result, collide, Some(collideLedger)))
    val first      = get(inspection.contrastRow(ResultRef.ContrastRow(0, slashA)))
    val second     = get(inspection.contrastRow(ResultRef.ContrastRow(0, slashB)))
    assertNotEquals(first.ref, second.ref)
    assertNotEquals(first.outcome, second.outcome)
    assertEquals(records(slashA, inspection), Vector(2))
    assertEquals(records(slashB, inspection), Vector(3))

    // A repeated full key is ambiguous: every occurrence stays visible and
    // is excluded from pairing, and no single source can be addressed.
    val repeated = StudyInput(
      Trials(rows(failing = false) :+ trial(a, Vector(1.5, 0.5)))
    )
    val repeatedPlan = plan(repeated)
    val repeatedRun  = get(repeatedPlan.run(repeated))
    val unledgered   = get(ResultInspection.study(repeatedPlan, repeatedRun, repeated, None))
    val estimation   = get(unledgered.estimation(ResultRef.Estimation(0, a)))
    assert(estimation.ambiguous)
    assertEquals(estimation.outcomes.size, 2)
    val reduced = get(unledgered.reduction(ResultRef.Reduction(0, StudyDesign.Matched, a)))
    assertEquals(reduced.outcome.left.map(_.code.render), Left("reduction.ambiguous-key"))
    assertEquals(reduced.members, Vector.empty)
    assertEquals(unledgered.sources.samples(a, 0), Left(MissingSource.AmbiguousTrial(a, 2)))
    val ambiguous = ResultInspection
      .study(repeatedPlan, repeatedRun, repeated, Some(ledger))
      .left
      .map(e => Diagnostic.of(e))
    assertEquals(ambiguous.left.map(_.code.render), Left("inspection.sources"))
    assertEquals(
      ambiguous.left.map(_.causes.map(_.code.render)),
      Left(Vector("admission.ambiguous-trial"))
    )
    // The repeated key, and one input position per occurrence, since the key
    // cannot tell the occurrences apart; linked to the key's own records.
    assertEquals(
      ambiguous.left.map(_.subject),
      Left(Vector(Locus.Trial(a), Locus.InputTrial(0), Locus.InputTrial(7)))
    )
    assertEquals(
      ambiguous.left.map(_.sources),
      Left(Vector(3, 6).map(SourceLink.Record(source, _)))
    )
  }

  test("a reordered input keeps every reference and every entry it addresses") {
    val reordered     = StudyInput(Trials(rows(failing = false).reverse))
    val (_, original) = inspect(plan(input), input)
    val (_, permuted) = inspect(plan(reordered), reordered)
    val originalScale = get(original.scale(0))
    val permutedScale = get(permuted.scale(0))
    def pairsOf(
        s: ScaleInspection[StudyKey, Px, Similarity, SignedDifference],
        d: StudyDesign
    ) =
      all(s.pairs(d))
    StudyDesign.values.foreach { design =>
      val before = pairsOf(originalScale, design)
      val after  = pairsOf(permutedScale, design)
      assertNotEquals(before.map(_.ref), after.map(_.ref))
      assertEquals(before.map(_.ref).toSet, after.map(_.ref).toSet)
      before.foreach(entry =>
        assertEquals(permutedScale.pairs(design).get(entry.ref), Some(entry))
      )
      // Members follow the stored pair order, which follows the source order;
      // the addressed members themselves are the same.
      def normal(entry: ReductionEntry[StudyKey, Similarity]) =
        entry.copy(members = entry.members.sortBy(_.reference))
      all(originalScale.reductions(design)).foreach(entry =>
        assertEquals(
          permutedScale.reductions(design).get(entry.ref).map(normal),
          Some(normal(entry))
        )
      )
    }
    all(originalScale.estimation).foreach(entry =>
      assertEquals(permutedScale.estimation.get(entry.ref), Some(entry))
    )
    records(a, permuted).foreach(r => assertEquals(permuted.sources.trialOf(r), Some(a)))
  }

  test("multi-component custom scores keep named components, units and directions") {
    val id         = get(DefinitionId.of("test.overlap-spread", 1))
    val comparison = new Compare[Mass[Px], Mass[Px], OverlapSpread]:
      val info = MeasureInfo(
        "overlap-spread",
        "Cosine overlap and summed absolute cell difference",
        MeasureScale.UnboundedSimilarity,
        None
      )
      def compare(x: Mass[Px], y: Mass[Px]): Either[CompareError, OverlapSpread] =
        Distribution.cosine[Px].compare(x, y).map { similarity =>
          OverlapSpread(
            similarity.value,
            x.values.indices.map(i => math.abs(x.values(i) - y.values(i))).sum
          )
        }
    def component(name: String) = name match
      case "overlap" =>
        ScoreComponent.of[OverlapSpread, OverlapSpreadDifference](
          "overlap",
          "Cosine overlap of the two maps",
          ParameterUnits.Dimensionless,
          MeasureScale.Bounded(0, 1),
          ScoreDirection.HigherIsCloser
        )(_.overlap, _.overlap)
      case _ =>
        ScoreComponent.of[OverlapSpread, OverlapSpreadDifference](
          "spread",
          "Summed absolute difference of cell masses",
          ParameterUnits.Cells,
          MeasureScale.DistanceLike,
          ScoreDirection.LowerIsCloser
        )(_.spread, _.spread)
    def method(order: Vector[String]) =
      new StudyMethod[Unit, Px, OverlapSpread, OverlapSpreadDifference](
        id,
        "overlap-spread",
        _ => Vector.empty,
        _ => comparison,
        Some(
          MethodDescriptor.of[Unit, OverlapSpread, OverlapSpreadDifference](
            id,
            ParameterSet.empty,
            _ => comparison.info,
            _ => order.traverse(component)
          )
        )
      )
    val described          = plan(input, method = method(Vector("overlap", "spread")))
    val (result, inspect2) = inspect(described, input)
    val stored             = get(result.scales.head.contrast).rows.find(_.key == a).get
    val row                = get(inspect2.contrastRow(ResultRef.ContrastRow(0, a)))
    val view               = get(row.outcome.left.map(_.message))
    assertEquals(Right(view.value), stored.difference.left.map(_.message))
    assertEquals(view.components.map(_.id), Vector("overlap", "spread"))
    assertEquals(
      view.components.map(_.units),
      Vector(ParameterUnits.Dimensionless, ParameterUnits.Cells)
    )
    assertEquals(
      view.components.map(_.direction),
      Vector(ScoreDirection.HigherIsCloser, ScoreDirection.LowerIsCloser)
    )
    assertEquals(view.components.map(_.value), Vector(view.value.overlap, view.value.spread))
    val matched = get(inspect2.reduction(get(row.matched.toRight("matched"))))
    val score   = get(matched.outcome.left.map(_.message))
    assertEquals(score.components.map(_.value), Vector(score.value.overlap, score.value.spread))

    val misordered = plan(input, method = method(Vector("spread", "overlap")))
    val refused    =
      ResultInspection.study(misordered, get(misordered.run(input)), input, Some(ledger))
    val diagnostic = Diagnostic.of(get(refused.swap.left.map(_ => "expected a refusal")))
    assertEquals(diagnostic.code.render, "inspection.components")
    assertEquals(diagnostic.causes.map(_.code.render), Vector("descriptor.component-mismatch"))
  }

  test("scale and trial failures keep stable typed references and source records") {
    val (_, inspection) = inspect(plan(failing), failing)
    val estimation      = get(inspection.estimation(ResultRef.Estimation(0, br)))
    val failure         = estimation.outcomes match
      case Vector(EstimationOutcome.Failed(d)) => d
      case other                               => fail(s"expected one failure, got $other")
    assertEquals(failure.code.render, "study-failure.frame")
    assertEquals(failure.subject, Vector(Locus.Scale(0), Locus.Trial(br)))
    assertEquals(failure.causes.map(_.code.render), Vector("geometry.frame-mismatch"))
    assertEquals(failure.sources, Vector(7, 9).map(SourceLink.Record(source, _)))

    val coarse = new StudyLayout[StudyKey](
      get(DefinitionId.of("test.participant-only", 1)),
      Projection.named("participant")(_.participant),
      Projection.named("stimulus")(_.stimulus),
      Projection.named("phase")(_.phase)
    )(using KeyDigest[StudyKey], Ordering.by[StudyKey, String](_.participant))
    val (_, refused) = inspect(plan(input, layout = coarse), input)
    get(refused.scale(0)).contrast match
      case ScaleContrast.Failed(d) =>
        assertEquals(d.code.render, "contrast.indistinguishable-ordering")
        assertEquals(d.subject, Vector(Locus.Scale(0), Locus.Trials(Vector(a, b))))
        assertEquals(
          d.sources,
          (records(a, refused) ++ records(b, refused)).map(SourceLink.Record(source, _))
        )
      case other => fail(s"expected a scale failure, got $other")
    assertEquals(
      refused.contrastRow(ResultRef.ContrastRow(0, a)),
      Left(InspectionError.NoContrast(0))
    )
  }

  test("window failures and window fixations link to their trials and records") {
    val epochs = input.trials.rows.collect {
      case row if row.key != lone =>
        row.key -> TrialEpoch(
          Instant.micros(0),
          get(ObservedCoverage.of(clock(row.key), Vector(interval(row.key, 0L, 3000L))))
        )
    }
    val temporalInput = get(TemporalStudyInput.of(input, epochs))
    val window = get(StudyWindow.of("early", get(Window.of(Span.micros(0), Span.micros(1500)))))
    val repetition   = get(RepetitionContrast.withinParticipant("recall", "recall", "encode"))
    val temporalPlan = get(
      TemporalStudyPlan.of(
        plan(input),
        temporalInput.reference,
        Vector(window),
        Vector(repetition),
        FixationBoundary.ClipDuration
      )
    )
    val result  = get(temporalPlan.run(temporalInput))
    val sources = get(StudySources.of(input, ledger))
    val view    =
      get(ResultInspection.temporal(result, sources, get(ScoreSchema.study(plan(input)))))
    assertEquals(view.cellNames, Vector("recall" -> "early"))
    val cell    = get(view.cell("recall", "early"))
    val missing =
      get(cell.occupancy.get(ResultRef.Occupancy("recall", "early", lone)).toRight("lone"))
    val diagnostic = get(missing.outcome.swap.left.map(_ => "expected a failure"))
    assertEquals(diagnostic.code.render, "temporal.missing-epoch")
    assertEquals(
      diagnostic.subject.take(3),
      Vector(Locus.Repetition("recall"), Locus.Window("early"), Locus.Trial(lone))
    )
    assertEquals(diagnostic.sources, Vector(SourceLink.Record(source, 14)))
    val occupied = get(
      get(
        cell.occupancy.get(ResultRef.Occupancy("recall", "early", a)).toRight("a")
      ).outcome.left.map(_.message)
    )
    assertEquals(occupied.fixations.map(_.index), Vector(0, 1))
    assertEquals(
      occupied.fixations.map(f => get(sources.fixation(a, f.index)).record),
      Vector(6, 3)
    )
    val bare = ResultRef.Estimation(0, lone)
    val here = ResultRef.InCell("recall", "early", bare)
    assertEquals(cell.study.estimation(bare), Left(InspectionError.UnknownReference(bare)))
    val elsewhere = ResultRef.InCell("recall", "late", bare)
    assertEquals(
      cell.study.estimation(elsewhere),
      Left(InspectionError.UnknownReference(elsewhere))
    )
    assertEquals(view.cellOf(here).map(_.window), Right("early"))
    assertEquals(view.cellOf(elsewhere), Left(InspectionError.UnknownCell("recall", "late")))
    val studied = get(cell.study.estimation(here))
    assertEquals(studied.ref, here)
    val row = get(
      cell.study.contrastRow(ResultRef.InCell("recall", "early", ResultRef.ContrastRow(0, a)))
    )
    assertEquals(
      row.matched,
      Some(ResultRef.InCell("recall", "early", ResultRef.Reduction(0, StudyDesign.Matched, a)))
    )
    assert(cell.study.reduction(get(row.matched.toRight("matched"))).isRight)
    studied.outcomes match
      case Vector(EstimationOutcome.Failed(d)) =>
        assertEquals(d.code.render, "study-failure.temporal")
        assertEquals(d.causes.map(_.code.render), Vector("temporal.missing-epoch"))
        assertEquals(
          d.subject.take(4),
          Vector(
            Locus.Repetition("recall"),
            Locus.Window("early"),
            Locus.Scale(0),
            Locus.Trial(lone)
          )
        )
        assertEquals(d.sources, Vector(SourceLink.Record(source, 14)))
      case other => fail(s"expected the missing epoch to fail estimation, got $other")
    assertEquals(
      view.cell("recall", "late"),
      Left(InspectionError.UnknownCell("recall", "late"))
    )
  }

  test("missing source locators are explicit, never guessed") {
    val (_, unledgered) = inspect(plan(input), input, withLedger = false)
    assertEquals(
      unledgered.sources.links(a),
      Vector(SourceLink.Missing(MissingSource.NoLedger))
    )
    assertEquals(unledgered.sources.fixation(a, 0), Left(MissingSource.NoLedger))
    assertEquals(unledgered.sources.samples(a, 0), Left(MissingSource.NotSourceSupported(a)))
    assertEquals(
      unledgered.sources.fixationLinks(a, 0),
      Vector(SourceLink.Fixation(a, 0), SourceLink.Missing(MissingSource.NoLedger))
    )
    val (_, ledgered) = inspect(plan(input), input)
    val unknown       = StudyKey("p9", "z", "recall")
    assertEquals(ledgered.sources.trial(unknown), Left(MissingSource.UnknownTrial(unknown)))
    assertEquals(
      ledgered.sources.links(unknown),
      Vector(SourceLink.Missing(MissingSource.UnknownTrial(unknown)))
    )
    assertEquals(
      ledgered.sources.fixation(a, 5),
      Left(MissingSource.FixationOutOfRange(a, 5, 2))
    )
    assertEquals(
      ledgered.sources.locate(Vector(Locus.Trial(a), Locus.Fixation(5))),
      Vector(SourceLink.Missing(MissingSource.FixationOutOfRange(a, 5, 2)))
    )
    assertEquals(
      unledgered.sources.fixationLinks(unknown, 0),
      Vector(SourceLink.Missing(MissingSource.UnknownTrial(unknown)))
    )
    assertEquals(
      unledgered.sources.fixationLinks(a, 2),
      Vector(SourceLink.Missing(MissingSource.FixationOutOfRange(a, 2, 2)))
    )
    // Errors that kept an input position or a key digest resolve to their trial.
    val digestOfA = KeyDigest[StudyKey].digest(a).render
    assertEquals(
      ledgered.sources.locate(Vector(Locus.TrialDigest(digestOfA))),
      ledgered.sources.links(a)
    )
    assertEquals(
      ledgered.sources.locate(Vector(Locus.TrialDigest("0000000000000000"))),
      Vector(SourceLink.Missing(MissingSource.UnknownDigest("0000000000000000")))
    )
    assertEquals(
      ledgered.sources.locate(Vector(Locus.InputTrial(0))),
      ledgered.sources.links(a)
    )
    assertEquals(
      ledgered.sources.locate(Vector(Locus.InputTrial(99))),
      Vector(SourceLink.Missing(MissingSource.UnknownInputTrial(99, 7)))
    )
    val repeated = StudySources.unledgered(
      StudyInput(Trials(rows(failing = false) :+ trial(a, Vector(1.5))))
    )
    assertEquals(
      repeated.fixationLinks(a, 0),
      Vector(SourceLink.Missing(MissingSource.AmbiguousTrial(a, 2)))
    )
    assertEquals(
      ledgered.sources.locate(Vector(Locus.Trial(a), Locus.Fixation(1))),
      Vector(SourceLink.Fixation(a, 1), SourceLink.Record(source, 3))
    )
    val (_, failingUnledgered) = inspect(plan(failing), failing, withLedger = false)
    assert(failingUnledgered.failures.nonEmpty)
    failingUnledgered.failures.foreach(d =>
      assert(d.sources.contains(SourceLink.Missing(MissingSource.NoLedger)), d.code.render)
    )
  }

  test("rejected and quarantined records are diagnostics naming record, key and reason") {
    val sources = get(StudySources.of(input, ledger))
    assertEquals(
      sources.rejections.map(d => (d.code.render, d.subject.take(2), d.sources)),
      Vector(
        (
          "admission-reason.time",
          Vector(Locus.Trial(dropped), Locus.Record(8)),
          Vector(SourceLink.Record(source, 8))
        ),
        (
          "admission-reason.quarantined",
          Vector(Locus.Trial(dropped), Locus.Record(11)),
          Vector(SourceLink.Record(source, 11))
        )
      )
    )
    val quarantined = get(sources.trial(dropped))
    assertEquals(quarantined.admitted, Vector.empty)
    assertEquals(quarantined.records, Vector(8, 11))
    assertEquals(
      sources.fixation(dropped, 0),
      Left(MissingSource.NotAdmitted(dropped, Vector(8, 11)))
    )
    assertEquals(
      sources.locate(Vector(Locus.Trial(dropped), Locus.Fixation(1))),
      Vector(SourceLink.Missing(MissingSource.NotAdmitted(dropped, Vector(8, 11))))
    )
    assertEquals(sources.links(dropped), Vector(8, 11).map(SourceLink.Record(source, _)))
  }

  test("numeric buffers are copies: mutating one never reaches the result") {
    val (result, inspection) = inspect(plan(input), input)
    val entry                = get(inspection.estimation(ResultRef.Estimation(0, a)))
    val density              = entry.outcomes match
      case Vector(EstimationOutcome.Estimated(d)) => d
      case other                                  => fail(s"expected a density, got $other")
    val stored = get(result.scales.head.estimation.find(_._1 == a).get._2.left.map(_.message))
    val before = stored.values.toVector
    assertEquals(density.cells.toVector, before)
    assertEquals(
      (density.frame, density.grid, density.nx, density.ny),
      (frame.id, grid.id, 2, 2)
    )
    assertEquals(density.provenance, stored.provenance)
    val cells = density.cells
    cells.asInstanceOf[Array[Double]](0) = 99.0
    assertEquals(stored.values.toVector, before)
    assertEquals(density.cells.toVector, before)
    assertEquals(get(inspection.estimation(ResultRef.Estimation(0, a))), entry)
  }

  test("pages at several sizes concatenate to the whole, keyed by reference") {
    val (result, inspection) = inspect(plan(failing, successfulOnly), failing)
    val scale                = get(inspection.scale(0))
    val stored               = result.scales.head
    val expectedPairs        = StudyDesign.values.toVector.map { design =>
      design -> stored.analyses
        .source(design)
        .rows
        .map(r => ResultRef.PairRow(0, design, r.left, r.right))
    }.toMap
    val expectedReductions = StudyDesign.values.toVector.map { design =>
      design -> stored.analyses
        .reduced(design)
        .entries
        .map(r => ResultRef.Reduction(0, design, r.key))
    }.toMap
    val contrastRows = scale.contrast match
      case ScaleContrast.Rows(rows) => rows
      case other                    => fail(s"expected rows, got $other")
    Vector(1, 2, 3, 7, PageSize.maximum).foreach { size =>
      assertEquals(
        whole(scale.estimation, size).map(_.ref),
        stored.estimation.map(_._1).distinct.map(ResultRef.Estimation(0, _))
      )
      StudyDesign.values.foreach { design =>
        assertEquals(whole(scale.pairs(design), size).map(_.ref), expectedPairs(design))
        assertEquals(
          whole(scale.reductions(design), size).map(_.ref),
          expectedReductions(design)
        )
        assertEquals(whole(scale.pairs(design), size), all(scale.pairs(design)))
      }
      assertEquals(
        whole(contrastRows, size).map(_.ref),
        get(stored.contrast).rows.map(r => ResultRef.ContrastRow(0, r.key))
      )
      assert(whole(scale.estimation, size).forall(e => scale.estimation.get(e.ref).contains(e)))
    }
    val size = get(PageSize.of(2))
    val page = scale.pairs(StudyDesign.Control).first(size)
    assertEquals(page.entries.size, 2)
    assertEquals(page.next, expectedPairs(StudyDesign.Control).lift(2))
    val stranger = ResultRef.PairRow(0, StudyDesign.Control, lone, ar)
    assertEquals(
      scale.pairs(StudyDesign.Control).page(stranger, size),
      Left(InspectionError.UnknownReference(stranger))
    )
    assertEquals(
      PageSize.of(0).left.map(Diagnostic.of(_).code.render),
      Left("inspection.invalid-page-size")
    )
    assert(PageSize.of(PageSize.maximum + 1).isLeft)
  }

  test("the projection agrees with the typed result, failures included") {
    val (result, inspection) = inspect(plan(failing, successfulOnly), failing)
    val scale                = get(inspection.scale(0))
    val stored               = result.scales.head
    StudyDesign.values.foreach { design =>
      assert(scale.pairing(design) == stored.analyses.source(design).diagnostics)
      assert(scale.report(design) == stored.analyses.reduced(design).diagnostics)
      all(scale.pairs(design)).zip(stored.analyses.source(design).rows).foreach {
        (entry, row) =>
          assertEquals((entry.focal, entry.reference), (row.left, row.right))
          assertEquals(entry.outcome.toOption.map(_.value), row.result.toOption)
          (entry.outcome, row.result) match
            case (Left(d), Left(failure)) =>
              val inner = Diagnostic.of(failure)
              assertEquals((d.code, d.operands), (inner.code, inner.operands))
              assertEquals(
                d.subject,
                entry.ref.loci ++ inner.subject.filterNot(entry.ref.loci.contains)
              )
            case (Right(_), Right(_)) => ()
            case other                => fail(s"outcome shapes differ: $other")
      }
      all(scale.reductions(design)).zip(stored.analyses.reduced(design).entries).foreach {
        (entry, row) =>
          assertEquals(
            (entry.key, entry.selected, entry.successful, entry.failed, entry.contributing),
            (row.key, row.selected, row.successful, row.failed, row.contributing)
          )
          assertEquals(entry.outcome.toOption.map(_.value), row.result.toOption)
          assertEquals(
            entry.outcome.left.toOption.map(_.code),
            row.result.left.toOption.map(Diagnostic.of(_).code)
          )
          assertEquals(entry.contributors.size, entry.contributing)
      }
    }
    val expected =
      stored.estimation.count(_._2.isLeft) +
        StudyDesign.values.toVector
          .map(d =>
            stored.analyses.source(d).rows.count(_.result.isLeft) +
              stored.analyses.reduced(d).entries.count(_.result.isLeft)
          )
          .sum +
        get(stored.contrast).rows.count(_.difference.isLeft)
    assertEquals(inspection.failures.size, expected)
    inspection.failures.foreach { d =>
      assert(d.keys.nonEmpty, d.code.render)
      assert(
        d.sources.exists {
          case SourceLink.Record(`source`, _) => true
          case _                              => false
        },
        d.code.render
      )
    }
  }

  test("inspection refuses sources of other data and unknown addresses") {
    val p      = plan(input)
    val result = get(p.run(input))
    assertEquals(
      ResultInspection
        .study(result, StudySources.unledgered(failing), ScoreSchema.undescribed)
        .left
        .map(Diagnostic.of(_).code.render),
      Left("inspection.input-mismatch")
    )
    val otherLedger = get(
      AdmissionLedger.decide(
        source,
        header,
        ledgerRecords.filterNot(_.record == 15),
        AdmissionDecision.ReviewExclusions
      )
    )
    val refused = ResultInspection.study(p, result, input, Some(otherLedger))
    assertEquals(refused.left.map(Diagnostic.of(_).code.render), Left("inspection.sources"))
    // The ledger of another CSV has no records for one input trial: the
    // refusal names that trial by key and says no record supplied it.
    assertEquals(refused.left.map(Diagnostic.of(_).subject), Left(Vector(Locus.Trial(other))))
    assertEquals(refused.left.map(Diagnostic.of(_).keys), Left(Vector(other)))
    assertEquals(
      refused.left.map(Diagnostic.of(_).sources),
      Left(Vector(SourceLink.Missing(MissingSource.UnknownTrial(other))))
    )
    assertEquals(
      StudySources.of(input, otherLedger).left.map(_.error),
      Left(AdmissionError.UnadmittedTrial(6))
    )
    // An input one trial short of the ledger: the orphan records name their trial.
    val short    = StudyInput(Trials(rows(failing = false).filterNot(_.key == lone)))
    val orphaned = get(StudySources.of(short, ledger).swap.left.map(_ => "expected a refusal"))
    val orphan   = Diagnostics.ledgerRefusal(orphaned)
    assertEquals(orphan.code.render, "admission.unknown-trial")
    assertEquals(orphan.subject, Vector(Locus.Trial(lone), Locus.Records(Vector(14))))
    assertEquals(orphan.sources, Vector(SourceLink.Record(source, 14)))
    val stale = ResultInspection.study(plan(input, successfulOnly), result, input, Some(ledger))
    assertEquals(stale.left.map(Diagnostic.of(_).code.render), Left("inspection.plan-mismatch"))
    assertEquals(
      stale.left.map {
        case InspectionError.PlanMismatch(changes) => changes.map(_.field)
        case other                                 => Vector(other.toString)
      },
      Left(Vector("failurePolicy"))
    )
    val wrong = get(
      ScoreComponent.of[Similarity, SignedDifference](
        "overlap",
        "A component the method does not have",
        ParameterUnits.Dimensionless,
        MeasureScale.Bounded(0, 1),
        ScoreDirection.HigherIsCloser
      )(_.value, _.value)
    )
    assertEquals(
      ResultInspection
        .study(result, get(StudySources.of(input, ledger)), ScoreSchema.of(Vector(wrong)))
        .left
        .map(Diagnostic.of(_).causes.map(_.code.render)),
      Left(Vector("descriptor.component-mismatch"))
    )
    val inspection = get(ResultInspection.study(p, result, input, Some(ledger)))
    assertEquals(inspection.scale(3), Left(InspectionError.UnknownScale(3, 1)))
    val wrongKind = ResultRef.ContrastRow(0, a)
    assertEquals(inspection.pair(wrongKind), Left(InspectionError.UnknownReference(wrongKind)))
    assertEquals(
      Diagnostic.of(InspectionError.UnknownReference(wrongKind)).subject,
      Vector(Locus.Scale(0), Locus.Trial(a))
    )
  }

  test("recording events are addressed by the source sample ranges that support them") {
    val display                = get(Frame.screen("inspection-display", 1000, 1000))
    val tracker                = ClockId("inspection-tracker")
    def gaze(i: Int): Gaze[Px] =
      if i < 15 then Gaze.Tracked(Pt[Px](400.0, 500.0), None)
      else if i == 15 then Gaze.Tracked(Pt[Px](480.0, 500.0), None)
      else if i == 16 then Gaze.Tracked(Pt[Px](560.0, 500.0), None)
      else Gaze.Tracked(Pt[Px](600.0, 500.0), None)
    val recording = get(
      Recording.of(
        display,
        tracker,
        Rate.Fixed(get(Hz(100.0))),
        Eye.Left,
        None,
        IArray.from((0 until 40).map(i => Sample(Instant.millis(i.toLong * 10L), gaze(i))))
      )
    )
    val recordingPlan = get(
      RecordingPlan.of(
        ArtifactRef.of(recording.contentHash),
        RecordingRef("inspection-recording"),
        display,
        tracker,
        ClockId("inspection-analysis"),
        FrameId("inspection-angular"),
        Some(get(Viewing.millimetres(600.0, 500.0, 500.0))),
        SyncFitMode.OffsetOnly,
        Vector(
          get(SyncMark.of("start", Instant.millis(0), Instant.millis(0))),
          get(SyncMark.of("end", Instant.millis(390), Instant.millis(390)))
        ),
        None,
        get(InterpolationGap.of(Span.micros(30000))),
        Vector(
          get(
            RecordingArea
              .of("centre", "Central area", get(Bounds.of[Px](300.0, 300.0, 700.0, 700.0)))
          )
        ),
        RecordingMethod.ivt(get(DefinitionId.of("eyes4s.ivt", 1))),
        IvtParameters(
          get(IvtThreshold.of(get(Velocity.perSecond[Deg](30)))),
          get(MinimumEventDuration.of(Span.micros(20000)))
        )
      )
    )
    val analysis = get(recordingPlan.run(recording))
    val view     = get(ResultInspection.recording(recordingPlan, analysis))
    val series   = analysis.detection.eventSeries
    // Links name the input recording the plan was run on, by its own digest.
    val input = ArtifactRef.of[Recording[Px]](recording.contentHash)
    Vector(1, 2, PageSize.maximum).foreach { size =>
      val events = whole(view.events, size)
      assertEquals(events.map(_.ref), series.support.map(r => ResultRef.Event(r.from, r.until)))
      assertEquals(events.map(_.span), series.events.map(_.span))
      assertEquals(
        events.map(_.source),
        series.support.map(r =>
          SourceLink.Samples(RecordingRef("inspection-recording"), input, r.from, r.until)
        )
      )
    }
    assertEquals(
      whole(view.events, 2).head.ref.loci,
      Vector(Locus.Samples(series.support.head.from, series.support.head.until))
    )
    val otherPlan = get(
      RecordingPlan.of(
        recordingPlan.input,
        recordingPlan.source,
        recordingPlan.display,
        recordingPlan.trackerClock,
        recordingPlan.analysisClock,
        recordingPlan.angularFrameId,
        recordingPlan.viewing,
        recordingPlan.synchronizationModel,
        recordingPlan.marks,
        recordingPlan.residualLimit,
        get(InterpolationGap.of(Span.micros(40000))),
        recordingPlan.areas,
        recordingPlan.method,
        recordingPlan.parameters
      )
    )
    assertEquals(
      ResultInspection.recording(otherPlan, analysis).left.map(Diagnostic.of(_).code.render),
      Left("inspection.plan-mismatch")
    )
    assert(whole(view.events, 3).map(_.kind).contains(EventKind.Fixation))
    assertEquals(view.description, analysis.description)
    assertEquals(series.recording.size, recording.size)
  }
