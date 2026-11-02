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

package eyes4s.results

import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

/** Declared labels for a location trial. An observer slot is a supplied label,
  * not a person or participant identity.
  */
final case class LocationTrialMetadata(
    trial: LocationTrialId,
    imageIndex: Int,
    observerSlot: String,
    image: String,
    imageFamily: String,
    phase: String,
    category: String,
    rawSDT: String
) derives CanEqual

/** Refusals while making a descriptive location report. */
enum LocationReportError derives CanEqual:
  case InputHash(expected: ContentHash, found: ContentHash)
  case PlanHash(expected: ContentHash, found: ContentHash)
  case Grid(expected: GridId, found: GridId)
  case MapTrials(expected: Vector[LocationTrialId], found: Vector[LocationTrialId])
  case OutcomeQueries(expected: Vector[String], found: Vector[String])
  case MissingMetadata(trial: LocationTrialId)
  case DuplicateMetadata(trial: LocationTrialId)
  case SurplusMetadata(trial: LocationTrialId)
  case Table(underlying: ResultTableError)

  def message: String = this match
    case InputHash(expected, found) =>
      s"Location result input hash ${found.render} does not match plan input hash ${expected.render}."
    case PlanHash(expected, found) =>
      s"Location result plan hash ${found.render} does not match plan hash ${expected.render}."
    case Grid(expected, found) =>
      s"Location result grid '${found.name}' does not match plan grid '${expected.name}'."
    case MapTrials(expected, found) =>
      s"Location result maps name trials ${found.map(_.value)}, expected ${expected.map(_.value)} in that order."
    case OutcomeQueries(expected, found) =>
      s"Location result outcomes name queries $found, expected $expected in that order."
    case MissingMetadata(trial) =>
      s"Location metadata is missing required trial '${trial.value}'."
    case DuplicateMetadata(trial) =>
      s"Location metadata declares trial '${trial.value}' more than once."
    case SurplusMetadata(trial) => s"Location metadata declares unknown trial '${trial.value}'."
    case Table(error)           => s"Location report table was refused: ${error.message}"

/** A raw stored query triple. Failures remain their original run errors. */
final case class LocationTripleOutcome(
    query: String,
    focal: LocationTrialId,
    matched: LocationTrialId,
    control: LocationTrialId,
    matchedCosine: Either[LocationRunError, Double],
    controlCosine: Either[LocationRunError, Double],
    difference: Either[LocationRunError, Double]
) derives CanEqual

/** A trial histogram together with the pixel grid that orders its bins. */
final case class LocationHistogramOutcome(
    trial: LocationTrialId,
    grid: Grid[Px],
    bins: Either[LocationMapError, Vector[Double]]
) derives CanEqual

/** Equal-query descriptive accounting only; it is not a participant summary. */
final case class LocationDifferenceSummary(
    successfulQueries: Int,
    failedQueries: Int,
    meanDifference: Option[Double]
) derives CanEqual

/** Pure tables and raw outcomes for one verified location comparison result. */
final class LocationReport private (
    val triples: Vector[LocationTripleOutcome],
    val histograms: Vector[LocationHistogramOutcome],
    val summary: LocationDifferenceSummary,
    val tables: Vector[ResultTable]
)

object LocationReport:
  import ResultCell.{Integer as I, Missing as M, Number as N, Text as T}

  private val tripleColumns = Vector(
    text("query_id", "explicit comparison query identifier"),
    text("focal_trial_id", "focal trial identity"),
    text("matched_trial_id", "matched trial identity"),
    text("control_trial_id", "control trial identity"),
    decimal("matched_cosine", "cosine from focal to matched trial", nullable = true),
    nullableText("matched_failure", "retained matched comparison failure reason"),
    decimal("control_cosine", "cosine from focal to control trial", nullable = true),
    nullableText("control_failure", "retained control comparison failure reason"),
    decimal("difference", "matched cosine minus control cosine", nullable = true),
    nullableText("difference_failure", "retained difference failure reason")
  )
  private val histogramColumns = Vector(
    text("trial_id", "location trial identity"),
    ResultColumn(
      "bins_json",
      ResultColumnType.JsonUtf8,
      true,
      "count",
      "pixel-grid bins in grid index order"
    ),
    nullableText("map_failure", "retained histogram preparation failure reason"),
    nullableCount("image_index", "declared image index"),
    nullableText("observer_slot", "declared observer slot; not a person identity"),
    nullableText("image", "declared image label"),
    nullableText("image_family", "declared image family label"),
    nullableText("phase", "declared phase label"),
    nullableText("category", "declared category label"),
    nullableText("raw_sdt", "declared raw SDT label; no response semantics inferred")
  )
  private val summaryColumns = Vector(
    count("successful_query_count", "queries with a finite retained difference"),
    count("failed_query_count", "queries without a retained difference"),
    decimal(
      "mean_difference",
      "equal-query descriptive mean of successful differences",
      nullable = true
    )
  )

  def of(
      plan: LocationComparisonPlan,
      result: LocationComparisonResult,
      metadata: Option[Vector[LocationTrialMetadata]] = None
  ): Either[LocationReportError, LocationReport] =
    verify(plan, result).flatMap { _ =>
      metadataIndex(plan, metadata).flatMap { declared =>
        val triples = result.outcomes.map(outcome =>
          LocationTripleOutcome(
            outcome.query.id,
            outcome.query.focal,
            outcome.query.matched,
            outcome.query.control,
            outcome.matched.map(_.value),
            outcome.control.map(_.value),
            outcome.difference
          )
        )
        val histograms = result.maps.map(map =>
          LocationHistogramOutcome(map.trial, result.grid, map.result.map(_.bins))
        )
        val values  = triples.flatMap(_.difference.toOption)
        val summary =
          LocationDifferenceSummary(values.size, triples.size - values.size, stableMean(values))
        for
          tripleTable <- ResultTable
            .of(
              ResultFamily.LocationTriples,
              tripleColumns,
              tripleRows(triples),
              context(result)
            )
            .left
            .map(LocationReportError.Table.apply)
          histogramTable <- ResultTable
            .of(
              ResultFamily.LocationHistograms,
              histogramColumns,
              histogramRows(histograms, declared),
              context(result)
            )
            .left
            .map(LocationReportError.Table.apply)
          summaryTable <- ResultTable
            .of(
              ResultFamily.LocationSummary,
              summaryColumns,
              Vector(summaryRow(summary)),
              context(result)
            )
            .left
            .map(LocationReportError.Table.apply)
        yield new LocationReport(
          triples,
          histograms,
          summary,
          Vector(tripleTable, histogramTable, summaryTable)
        )
      }
    }

  private def verify(
      plan: LocationComparisonPlan,
      result: LocationComparisonResult
  ): Either[LocationReportError, Unit] =
    if result.inputHash != plan.inputHash then
      Left(LocationReportError.InputHash(plan.inputHash, result.inputHash))
    else if result.planHash != plan.planHash then
      Left(LocationReportError.PlanHash(plan.planHash, result.planHash))
    else if result.grid.id != plan.grid.id then
      Left(LocationReportError.Grid(plan.grid.id, result.grid.id))
    else
      val trials   = plan.input.trials.map(_.id)
      val maps     = result.maps.map(_.trial)
      val queries  = plan.queries.map(_.id)
      val outcomes = result.outcomes.map(_.query.id)
      if maps != trials then Left(LocationReportError.MapTrials(trials, maps))
      else if outcomes != queries then
        Left(LocationReportError.OutcomeQueries(queries, outcomes))
      else Right(())

  private def metadataIndex(
      plan: LocationComparisonPlan,
      metadata: Option[Vector[LocationTrialMetadata]]
  ): Either[LocationReportError, Map[LocationTrialId, LocationTrialMetadata]] = metadata match
    case None         => Right(Map.empty)
    case Some(values) =>
      val seen = scala.collection.mutable.HashSet.empty[LocationTrialId]
      values.find(m => !seen.add(m.trial)) match
        case Some(value) => Left(LocationReportError.DuplicateMetadata(value.trial))
        case None        =>
          val index    = values.map(m => m.trial -> m).toMap
          val expected = plan.input.trials.map(_.id).toSet
          values.iterator.map(_.trial).find(!expected.contains(_)) match
            case Some(trial) => Left(LocationReportError.SurplusMetadata(trial))
            case None        =>
              plan.input.trials.iterator.map(_.id).find(!index.contains(_)) match
                case Some(trial) => Left(LocationReportError.MissingMetadata(trial))
                case None        => Right(index)

  private def tripleRows(values: Vector[LocationTripleOutcome]): Vector[Vector[ResultCell]] =
    values.map { value =>
      Vector(
        T(value.query),
        T(value.focal.value),
        T(value.matched.value),
        T(value.control.value),
        value.matchedCosine.fold(_ => M, N.apply),
        value.matchedCosine.fold(e => T(e.message), _ => M),
        value.controlCosine.fold(_ => M, N.apply),
        value.controlCosine.fold(e => T(e.message), _ => M),
        value.difference.fold(_ => M, N.apply),
        value.difference.fold(e => T(e.message), _ => M)
      )
    }

  private def histogramRows(
      values: Vector[LocationHistogramOutcome],
      metadata: Map[LocationTrialId, LocationTrialMetadata]
  ): Vector[Vector[ResultCell]] = values.map { value =>
    val declared = metadata.get(value.trial)
    Vector(
      T(value.trial.value),
      value.bins.fold(
        _ => M,
        bins => T(TableJson.Arr(bins.map(n => TableJson.integer(n.toLong))).canonical)
      ),
      value.bins.fold(e => T(e.message), _ => M),
      declared.fold[ResultCell](M)(m => I(m.imageIndex.toLong)),
      declared.fold[ResultCell](M)(m => T(m.observerSlot)),
      declared.fold[ResultCell](M)(m => T(m.image)),
      declared.fold[ResultCell](M)(m => T(m.imageFamily)),
      declared.fold[ResultCell](M)(m => T(m.phase)),
      declared.fold[ResultCell](M)(m => T(m.category)),
      declared.fold[ResultCell](M)(m => T(m.rawSDT))
    )
  }

  private def summaryRow(value: LocationDifferenceSummary): Vector[ResultCell] = Vector(
    I(value.successfulQueries.toLong),
    I(value.failedQueries.toLong),
    value.meanDifference.fold[ResultCell](M)(N.apply)
  )

  private def context(result: LocationComparisonResult): TableJson = TableJson.obj(
    "input_hash" -> TableJson.text(result.inputHash.render),
    "plan_hash"  -> TableJson.text(result.planHash.render),
    "grid"       -> TableJson.obj(
      "id"      -> TableJson.text(result.grid.id.name),
      "columns" -> TableJson.integer(result.grid.nx.toLong),
      "rows"    -> TableJson.integer(result.grid.ny.toLong),
      "dx"      -> TableJson.Number.of(DecimalText.render(result.grid.cellWidth)).get,
      "dy"      -> TableJson.Number.of(DecimalText.render(result.grid.cellHeight)).get,
      "unit"    -> TableJson.text("px"),
      "frame"   -> TableJson.obj(
        "id"    -> TableJson.text(result.grid.frame.id.name),
        "x_min" -> TableJson.Number.of(DecimalText.render(result.grid.frame.bounds.xMin)).get,
        "x_max" -> TableJson.Number.of(DecimalText.render(result.grid.frame.bounds.xMax)).get,
        "y_min" -> TableJson.Number.of(DecimalText.render(result.grid.frame.bounds.yMin)).get,
        "y_max" -> TableJson.Number.of(DecimalText.render(result.grid.frame.bounds.yMax)).get,
        "axis"  -> TableJson.text(result.grid.frame.yAxis.toString),
        "unit"  -> TableJson.text("px")
      ),
      "index_order" -> TableJson.text("iy * nx + ix; x varies fastest")
    ),
    "observation_measure" -> TableJson.text("uniform observation counts"),
    "similarity"          -> TableJson.text("cosine"),
    "score_unit"          -> TableJson.text("dimensionless"),
    "summary"             -> TableJson.text(
      "equal-query descriptive mean of successful matched-minus-control differences"
    ),
    "metadata"   -> TableJson.text("observer slots are declared labels, not persons"),
    "provenance" -> TableJson.text(
      "retained computed or restored location result; no rerun or response-semantics inference"
    )
  )

  private def text(name: String, meaning: String): ResultColumn =
    ResultColumn(name, ResultColumnType.Utf8, false, "text", meaning)
  private def nullableText(name: String, meaning: String): ResultColumn =
    ResultColumn(name, ResultColumnType.Utf8, true, "text", meaning)
  private def decimal(name: String, meaning: String, nullable: Boolean): ResultColumn =
    ResultColumn(name, ResultColumnType.Float64, nullable, "dimensionless", meaning)
  private def count(name: String, meaning: String): ResultColumn =
    ResultColumn(name, ResultColumnType.Int64, false, "count", meaning)
  private def nullableCount(name: String, meaning: String): ResultColumn =
    ResultColumn(name, ResultColumnType.Int64, true, "count", meaning)

  /** Neumaier compensation avoids discarding small retained differences. */
  private def stableMean(values: Vector[Double]): Option[Double] =
    if values.isEmpty then None
    else
      val (sum, correction) = values.foldLeft((0.0, 0.0)) { case ((sum, correction), value) =>
        val next  = sum + value
        val error =
          if math.abs(sum) >= math.abs(value) then (sum - next) + value
          else (value - next) + sum
        (next, correction + error)
      }
      Some((sum + correction) / values.size.toDouble)
