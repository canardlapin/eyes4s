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

package eyes4s.io

import eyes4s.codec.*
import eyes4s.compare.*
import eyes4s.core.{FixationBoundary, ObservedCoverage, Weight}
import eyes4s.design.*
import eyes4s.examples.MatchedControlFixtures
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

/** A two-component score with different units and directions, for export
  * conformance: cosine overlap (higher is closer) and summed absolute cell
  * difference (lower is closer). Differences keep the same two components.
  */
final case class OverlapSpreadScore(overlap: Double, spread: Double) derives CanEqual
object OverlapSpreadScore:
  given ScoreMean[OverlapSpreadScore] with
    def mean(values: Vector[OverlapSpreadScore]): Either[ScoreMeanError, OverlapSpreadScore] =
      for
        overlap <- ScoreMean[Double].mean(values.map(_.overlap))
        spread  <- ScoreMean[Double].mean(values.map(_.spread))
      yield OverlapSpreadScore(overlap, spread)
  given Contrastable[OverlapSpreadScore, OverlapSpreadScore] with
    val components = Vector("overlap", "spread")
    def subtract(
        matched: OverlapSpreadScore,
        control: OverlapSpreadScore
    ): Either[DifferenceError, OverlapSpreadScore] =
      for
        overlap <- SignedDifference.between(matched.overlap, control.overlap, "overlap")
        spread  <- SignedDifference.between(matched.spread, control.spread, "spread")
      yield OverlapSpreadScore(overlap.value, spread.value)

  private val comparison = new Compare[Mass[Px], Mass[Px], OverlapSpreadScore]:
    val info = MeasureInfo(
      "overlap-spread",
      "Cosine overlap and summed absolute cell difference",
      MeasureScale.UnboundedSimilarity,
      None
    )
    def compare(x: Mass[Px], y: Mass[Px]): Either[CompareError, OverlapSpreadScore] =
      Distribution.cosine[Px].compare(x, y).map { similarity =>
        OverlapSpreadScore(
          similarity.value,
          x.values.indices.map(i => math.abs(x.values(i) - y.values(i))).sum
        )
      }

  def method(id: DefinitionId): StudyMethod[Unit, Px, OverlapSpreadScore, OverlapSpreadScore] =
    def component(
        name: String,
        units: ParameterUnits,
        range: MeasureScale,
        direction: ScoreDirection
    )(read: OverlapSpreadScore => Double) =
      ScoreComponent.of[OverlapSpreadScore, OverlapSpreadScore](
        name,
        s"The $name component",
        units,
        range,
        direction
      )(read, read)
    new StudyMethod[Unit, Px, OverlapSpreadScore, OverlapSpreadScore](
      id,
      "overlap-spread",
      _ => Vector.empty,
      _ => comparison,
      Some(
        MethodDescriptor.of[Unit, OverlapSpreadScore, OverlapSpreadScore](
          id,
          ParameterSet.empty,
          _ => comparison.info,
          _ =>
            for
              overlap <- component(
                "overlap",
                ParameterUnits.Dimensionless,
                MeasureScale.Bounded(0, 1),
                ScoreDirection.HigherIsCloser
              )(_.overlap)
              spread <- component(
                "spread",
                ParameterUnits.Cells,
                MeasureScale.DistanceLike,
                ScoreDirection.LowerIsCloser
              )(_.spread)
            yield Vector(overlap, spread)
        )
      )
    )

/** From a fixation CSV through the importer's ledger to result inspection:
  * a result row reaches the logical CSV record that supplied it, rejected rows
  * are coded diagnostics, and the contrast export carries the same named
  * result semantics as the projection.
  */
class InspectionSourceSuite extends munit.FunSuite:
  private def get[E, A](e: Either[E, A]): A = e.fold(error => fail(s"$error"), identity)
  private val frame                         = get(Frame.screen("matched-control-display", 2, 2))
  private val columns                       = get(
    FixationColumns.of("fixation", "x_px", "y_px", "onset_us", "duration_us", "sample_count")
  )
  private val keys = get(FixationKeyReader.study("participant", "image", "phase"))

  /** A note column whose quoted value spans lines, so logical record numbers
    * and physical line numbers disagree after the first note.
    */
  private val header = StudyInputFixtures.header :+ "note"
  private val rows   = StudyInputFixtures.records.zipWithIndex.map { case (fields, i) =>
    fields :+ (if i % 5 == 0 then "reviewed,\nsee notebook" else "")
  }
  private def csv(records: Vector[Vector[String]] = rows): String =
    Rfc4180.encode(header +: records)
  private def decoded(text: String): Vector[Vector[String]] = get(Rfc4180.decode(text))

  private val plan = get(
    StudyPlan.cosine(
      StudyInputFixtures.matchedControl.reference,
      get(Grid.over(frame, 2, 2)),
      "recall",
      "encode",
      Weight.Duration,
      Vector(StudyEstimate.Binned()),
      FailurePolicy.RequireAll
    )
  )

  test("a result row drills down to the logical CSV record that supplied it") {
    val text     = csv()
    val imported = get(FixationCsv.read(text, columns, keys, frame, TimestampUnit.Microseconds))
    val ledger   =
      get(
        FixationEvidence.ledger(
          "matched-control.csv",
          imported,
          AdmissionDecision.RequireComplete
        )
      )
    val input = get(imported.requireComplete)
    assertEquals(input.reference, plan.input)
    val result     = get(plan.run(input))
    val inspection = get(ResultInspection.study(plan, result, input, Some(ledger)))

    val focal     = StudyKey("s2", "b", "recall")
    val row       = get(inspection.contrastRow(ResultRef.ContrastRow(0, focal)))
    val matched   = get(inspection.reduction(get(row.matched.toRight("matched"))))
    val member    = matched.members.find(_.status == Membership.Contributing).get
    val reference = get(inspection.pair(member.pair)).reference
    assertEquals(reference, StudyKey("s2", "b", "encode"))

    val located = get(inspection.sources.fixation(reference, 3))
    val record  = decoded(text)(located.record - 1)
    assertEquals(record.take(4), Vector("s2", "b", "encode", located.ordinal.toString))
    val fixation = input.trials.rows.find(_.key == reference).get.value.fixations(3)
    assertEquals(record(4).toDouble, fixation.centre.x)
    assertEquals(record(5).toDouble, fixation.centre.y)
    assertEquals(record(6).toLong, fixation.span.onset.toMicros)
    // Physical lines drift from logical records once a note spans two lines.
    assertNotEquals(
      text.linesIterator.toVector(located.record - 1).split(',').take(4).toVector,
      record.take(4)
    )
    assertEquals(inspection.sources.source.map(_.label), Some("matched-control.csv"))
    assertEquals(
      ledger.records.map(_.record),
      (2 to MatchedControlFixtures.fixations.size + 1).toVector
    )
  }

  test("every rejected CSV row is a diagnostic naming its logical record and typed reason") {
    val bad      = rows.updated(1, rows(1).updated(7, "-1"))
    val text     = csv(bad)
    val imported = get(FixationCsv.read(text, columns, keys, frame, TimestampUnit.Microseconds))
    val ledger   =
      get(
        FixationEvidence.ledger(
          "matched-control.csv",
          imported,
          AdmissionDecision.ReviewExclusions
        )
      )
    val accepted = StudyInput(imported.accepted)
    val sources  = get(StudySources.of(accepted, ledger))
    val dropped  = StudyKey("s1", "a", "encode")
    val rejected = sources.rejections
    assertEquals(
      rejected.map(d => d.code.render -> d.sources),
      Vector(
        "admission-reason.quarantined" -> Vector(SourceLink.Record(ledger.source, 2)),
        "admission-reason.time"        -> Vector(SourceLink.Record(ledger.source, 3)),
        "admission-reason.quarantined" -> Vector(SourceLink.Record(ledger.source, 4)),
        "admission-reason.quarantined" -> Vector(SourceLink.Record(ledger.source, 5))
      )
    )
    rejected.foreach(d => assertEquals(d.keys, Vector(dropped)))
    assertEquals(decoded(text)(3 - 1)(7), "-1")
    assertEquals(
      rejected(1).operand("duration"),
      Some(Operand.Text("-1"))
    )
  }

  test("the contrast export and the projection share one set of named result semantics") {
    val ledger = get(
      FixationEvidence.ledger(
        "matched-control.csv",
        get(
          FixationCsv.read(
            csv(rows.updated(1, rows(1).updated(7, "-1"))),
            columns,
            keys,
            frame,
            TimestampUnit.Microseconds
          )
        ),
        AdmissionDecision.ReviewExclusions
      )
    )
    val accepted = StudyInput(
      Trials(
        StudyInputFixtures.matchedControl.trials.rows
          .filterNot(_.key == StudyKey("s1", "a", "encode"))
      )
    )
    val cases: Vector[
      (
          StudyPlan[StudyKey, Px, Unit, Similarity, SignedDifference],
          StudyInput[StudyKey, Px],
          Option[AdmissionLedger[StudyKey]]
      )
    ] = Vector(
      (plan, StudyInputFixtures.matchedControl, None),
      (
        get(
          StudyPlan.cosine(
            accepted.reference,
            plan.grid,
            plan.focalPhase,
            plan.referencePhase,
            plan.weight,
            plan.estimates,
            plan.policy
          )
        ),
        accepted,
        Some(ledger)
      )
    )
    cases.foreach { (study, input, evidence) =>
      val result     = get(study.run(input))
      val inspection = get(ResultInspection.study(study, result, input, evidence))
      val document   =
        get(
          ContrastCsv.document(study, result, StudyCodecs.cosine[Px], ScoreColumns.similarity)
        )
      val named = document.rows
        .map(r => document.header.zip(r).toMap)
        .filter(_("scope") == "contrast")
      val rows = inspection.scales.head.contrast match
        case ScaleContrast.Rows(listing) =>
          Iterator
            .iterate(Option(listing.first(get(PageSize.of(2)))))(
              _.flatMap(_.next).map(r => get(listing.page(r, get(PageSize.of(2)))))
            )
            .takeWhile(_.isDefined)
            .flatten
            .flatMap(_.entries)
            .toVector
        case ScaleContrast.Failed(d) => fail(d.message)
      assertEquals(named.size, rows.size)
      named.zip(rows).foreach { (exported, entry) =>
        assertEquals(
          StudyKey(exported("participant"), exported("stimulus"), exported("phase")),
          entry.key
        )
        def operand(ref: Option[ResultRef[StudyKey]], prefix: String): Unit =
          ref match
            case None =>
              Vector("selected", "successful", "failed", "contributing")
                .foreach(c => assertEquals(exported(s"${prefix}_$c"), ""))
              assertEquals(exported(prefix), "")
            case Some(reference) =>
              val reduction = get(inspection.reduction(reference))
              assertEquals(
                Vector("selected", "successful", "failed", "contributing")
                  .map(c => exported(s"${prefix}_$c")),
                Vector(
                  reduction.selected,
                  reduction.successful,
                  reduction.failed,
                  reduction.contributing
                )
                  .map(_.toString)
              )
              // Missing is an empty field; a zero count is "0".
              assertEquals(
                exported(prefix),
                reduction.outcome.fold(_ => "", _.components.head.value.toString)
              )
        operand(entry.matched, "matched")
        operand(entry.control, "control")
        assertEquals(exported("component"), "value")
        assertEquals(
          exported("difference"),
          entry.outcome.fold(_ => "", _.components.head.value.toString)
        )
        assertEquals(exported("status"), if entry.outcome.isRight then "ok" else "failed")
        assertEquals(exported("reason"), entry.outcome.fold(_.message, _ => ""))
      }
    }
  }

  test(
    "the temporal export and the temporal projection agree cell by cell and link to records"
  ) {
    val temporalFrame = get(Frame.screen("temporal", 2, 2))
    val text          = TemporalFixtures.csv
    val imported      =
      get(FixationCsv.read(text, columns, keys, temporalFrame, TimestampUnit.Microseconds))
    val ledger =
      get(FixationEvidence.ledger("temporal.csv", imported, AdmissionDecision.RequireComplete))
    val input  = get(imported.requireComplete)
    val epochs = input.trials.rows.map { trial =>
      val k     = trial.key
      val spans = TemporalFixtures.coverage(s"${k.participant}/${k.stimulus}/${k.phase}").map {
        case (a, b) => get(Interval.of(trial.value.clock, Instant.micros(a), Instant.micros(b)))
      }
      k -> TrialEpoch(Instant.micros(0), get(ObservedCoverage.of(trial.value.clock, spans)))
    }
    val temporal = get(TemporalStudyInput.of(input, epochs))
    val base     = get(
      StudyPlan.cosine(
        input.reference,
        get(Grid.over(temporalFrame, 2, 2)),
        "recall",
        "encode",
        Weight.Duration,
        Vector(StudyEstimate.Binned()),
        FailurePolicy.RequireAll
      )
    )
    val windows = Vector(("early", 0L, 300000L), ("late", 600000L, 950000L)).map {
      case (name, from, until) =>
        get(StudyWindow.of(name, get(Window.of(Span.micros(from), Span.micros(until)))))
    }
    val plan = get(
      TemporalStudyPlan.of(
        base,
        temporal.reference,
        windows,
        Vector(get(RepetitionContrast.withinParticipant("retest-recall", "retest", "recall"))),
        FixationBoundary.ClipDuration
      )
    )
    val result      = get(plan.run(temporal))
    val persistence =
      new TemporalStudyCodec(
        get(DefinitionId.of("eyes4s.temporal-study", 1)),
        StudyCodecs.cosine[Px]
      )
    val tables =
      get(TemporalContrastCsv.document(plan, result, persistence, ScoreColumns.similarity))
    val sources = get(StudySources.of(input, ledger))
    val view    = get(ResultInspection.temporal(result, sources, get(ScoreSchema.study(base))))
    val records = decoded(text)

    val coverage = tables.coverage.rows.map(r => tables.coverage.header.zip(r).toMap)
    assertEquals(
      coverage.size,
      view.cellNames.map((r, w) => get(view.cell(r, w)).occupancy.size).sum
    )
    var linked = 0
    coverage.foreach { row =>
      val key   = get(persistence.study.keys.parse(row("key_json")))
      val ref   = ResultRef.Occupancy(row("repetition"), row("window"), key)
      val cell  = get(view.cell(row("repetition"), row("window")))
      val entry = get(cell.occupancy.get(ref).toRight(s"no occupancy for $ref"))
      entry.outcome match
        case Right(occupied) =>
          assertEquals(row("status"), "ok")
          assertEquals(row("observed_us"), occupied.observedMicros.toString)
          assertEquals(row("missing_us"), occupied.missingMicros.toString)
          assertEquals(
            row("excluded_fixations"),
            occupied.fixations.filter(_.retainedMicros == 0).map(_.index).mkString(",")
          )
          occupied.fixations.foreach { time =>
            val source = get(sources.fixation(key, time.index))
            val fields = records(source.record - 1)
            assertEquals(
              fields.take(4),
              Vector(key.participant, key.stimulus, key.phase, source.ordinal.toString)
            )
            linked += 1
          }
        case Left(diagnostic) =>
          assertEquals(row("status"), "failed")
          assertEquals(row("reason"), diagnostic.message)
    }
    assert(linked > 0)

    val contrasts = tables.contrasts.rows
      .map(r => tables.contrasts.header.zip(r).toMap)
      .filter(_("scope") == "contrast")
    assert(contrasts.nonEmpty)
    contrasts.foreach { row =>
      val cell  = get(view.cell(row("repetition"), row("window")))
      val focal = StudyKey(row("participant"), row("stimulus"), row("phase"))
      val entry = get(
        cell.study.contrastRow(
          ResultRef.InCell(row("repetition"), row("window"), ResultRef.ContrastRow(0, focal))
        )
      )
      assertEquals(
        row("difference"),
        entry.outcome.fold(_ => "", _.components.head.value.toString)
      )
      assertEquals(row("status"), if entry.outcome.isRight then "ok" else "failed")
      assertEquals(row("reason"), entry.outcome.fold(_.message, _ => ""))
    }
  }

  test("a two-component export and its projection carry the same named components") {
    val id     = get(DefinitionId.of("test.overlap-spread", 1))
    val method = OverlapSpreadScore.method(id)
    val study  = get(
      StudyPlan.of(
        StudyInputFixtures.matchedControl.reference,
        StudyKey.layout(DefinitionId.studyLayout),
        get(Grid.over(frame, 2, 2)),
        "recall",
        "encode",
        Weight.Duration,
        Vector(StudyEstimate.Binned()),
        FailurePolicy.RequireAll,
        method,
        ()
      )
    )
    val input       = StudyInputFixtures.matchedControl
    val result      = get(study.run(input))
    val inspection  = get(ResultInspection.study(study, result, input, None))
    val persistence = new StudyCodec(
      get(DefinitionId.of("test.overlap-spread-study", 1)),
      StudyKey.layout(DefinitionId.studyLayout),
      StudyCodecs.key(DefinitionId.studyKey),
      method,
      VersionedCodec.unit(DefinitionId.unit)
    )
    val columns = get(
      ScoreColumns.of[OverlapSpreadScore, OverlapSpreadScore](Vector("overlap", "spread"))(
        s => Vector(s.overlap, s.spread),
        d => Vector(d.overlap, d.spread)
      )
    )
    val document = get(ContrastCsv.document(study, result, persistence, columns))
    val exported = document.rows
      .map(r => document.header.zip(r).toMap)
      .filter(_("scope") == "contrast")
    val rows = inspection.scales.head.contrast match
      case ScaleContrast.Rows(listing) =>
        Iterator
          .iterate(Option(listing.first(get(PageSize.of(4)))))(
            _.flatMap(_.next).map(r => get(listing.page(r, get(PageSize.of(4)))))
          )
          .takeWhile(_.isDefined)
          .flatten
          .flatMap(_.entries)
          .toVector
      case ScaleContrast.Failed(d) => fail(d.message)
    assertEquals(exported.size, rows.size * 2)
    rows.foreach { entry =>
      val view  = get(entry.outcome.left.map(_.message))
      val lines =
        exported.filter(r => StudyKey(r("participant"), r("stimulus"), r("phase")) == entry.key)
      assertEquals(lines.map(_("component")), view.components.map(_.id))
      lines.zip(view.components).foreach { (line, component) =>
        assertEquals(line("difference"), component.value.toString)
      }
      val matched = get(inspection.reduction(get(entry.matched.toRight("matched"))))
      val score   = get(matched.outcome.left.map(_.message))
      lines.zip(score.components).foreach { (line, component) =>
        assertEquals(line("matched"), component.value.toString)
      }
      assertEquals(
        view.components.map(_.direction),
        Vector(ScoreDirection.HigherIsCloser, ScoreDirection.LowerIsCloser)
      )
    }
  }
