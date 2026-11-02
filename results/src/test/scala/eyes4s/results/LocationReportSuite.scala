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

class LocationReportSuite extends munit.FunSuite:
  private val HandArithmeticTolerance           = 1e-12
  private def get[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)
  private val frame   = get(Frame.screen("location-report", 6, 4))
  private val focal   = get(LocationTrialId.of("focal"))
  private val matched = get(LocationTrialId.of("matched"))
  private val control = get(LocationTrialId.of("control"))
  private val source  = get(LocationSourceId.of("locations.tsv"))

  private def row(ordinal: Long, x: Double, y: Double) =
    LocationObservation(source, ordinal, Pt[Px](x, y))
  private def trial(id: LocationTrialId, rows: LocationObservation*) = get(
    LocationTrial.of(id, rows.toVector)
  )
  private def plan(
      queries: Vector[LocationComparisonQuery] = Vector(
        get(LocationComparisonQuery.of("q", focal, matched, control))
      )
  ) = get(
    LocationComparisonPlan.binned(
      get(
        LocationInput.of(
          frame,
          Vector(
            trial(focal, row(1, 0.1, 0.1), row(2, 0.2, 0.2), row(3, 2.1, 0.1)),
            trial(matched, row(4, 0.1, 0.1), row(5, 2.1, 0.1)),
            trial(control, row(6, 5.9, 3.9))
          )
        )
      ),
      3,
      2,
      queries
    )
  )

  test("raw triples, pixel histograms, metadata and equal-query arithmetic are retained") {
    val current = plan(
      Vector(
        get(LocationComparisonQuery.of("q", focal, matched, control)),
        get(LocationComparisonQuery.of("q2", control, matched, focal))
      )
    )
    val report = get(
      LocationReport.of(
        current,
        current.run,
        Some(
          Vector(
            LocationTrialMetadata(
              focal,
              101,
              "slot-a",
              "focal.jpg",
              "family-a",
              "study",
              "target",
              "d'=raw"
            ),
            LocationTrialMetadata(
              matched,
              102,
              "slot-b",
              "matched.jpg",
              "family-a",
              "study",
              "match",
              "d'=raw"
            ),
            LocationTrialMetadata(
              control,
              103,
              "slot-c",
              "control.jpg",
              "family-b",
              "study",
              "control",
              "d'=raw"
            )
          )
        )
      )
    )
    val expected = 3.0 / math.sqrt(10.0)

    assertEquals(report.triples.map(_.query), Vector("q", "q2"))
    assertEquals(report.triples.head.focal, focal)
    assertEquals(report.triples.head.matched, matched)
    assertEquals(report.triples.head.control, control)
    assertEqualsDouble(
      get(report.triples.head.matchedCosine),
      expected,
      HandArithmeticTolerance
    )
    assertEqualsDouble(get(report.triples.head.controlCosine), 0.0, HandArithmeticTolerance)
    assertEqualsDouble(get(report.triples.head.difference), expected, HandArithmeticTolerance)
    assertEquals(report.histograms.map(_.trial), Vector(focal, matched, control))
    assertEquals(get(report.histograms.head.bins), Vector(2.0, 1.0, 0.0, 0.0, 0.0, 0.0))
    assertEquals(report.histograms.head.grid.id, current.grid.id)
    assertEquals(report.summary.successfulQueries, 2)
    assertEquals(report.summary.failedQueries, 0)
    assertEqualsDouble(
      report.summary.meanDifference.getOrElse(fail("missing mean")),
      expected / 2.0,
      HandArithmeticTolerance
    )
    assertEquals(
      report.tables.map(_.family),
      Vector(
        ResultFamily.LocationTriples,
        ResultFamily.LocationHistograms,
        ResultFamily.LocationSummary
      )
    )
    assertEquals(
      report.tables(1).column("image_index").get.map(_.text),
      Vector("101", "102", "103")
    )
    assertEquals(
      report.tables(1).column("observer_slot").get.map(_.text),
      Vector("slot-a", "slot-b", "slot-c")
    )
    assertEquals(
      report.tables(1).column("image").get.map(_.text),
      Vector("focal.jpg", "matched.jpg", "control.jpg")
    )
    assertEquals(
      report.tables(1).column("image_family").get.map(_.text),
      Vector("family-a", "family-a", "family-b")
    )
    val binsJson = report.tables(1).column("bins_json").get.head match
      case ResultCell.Text(value) => get(TableJson.parse(value))
      case other                  => fail(s"expected JSON histogram, got $other")
    assertEquals(
      binsJson,
      TableJson.Arr(
        Vector(
          TableJson.integer(2),
          TableJson.integer(1),
          TableJson.integer(0),
          TableJson.integer(0),
          TableJson.integer(0),
          TableJson.integer(0)
        )
      )
    )
    assertEquals(
      report.tables.head.columns.find(_.name == "matched_cosine").map(_.unit),
      Some("dimensionless")
    )
    assert(report.tables(1).context.canonical.contains("iy * nx + ix"))
    assert(report.tables(1).context.canonical.contains("\"dx\":2"))
    assert(report.tables(1).context.canonical.contains("\"axis\":\"Down\""))
    assert(report.tables(1).context.canonical.contains("uniform observation counts"))
    assert(report.tables(1).context.canonical.contains("retained computed or restored"))
  }

  test("metadata is all-or-nothing and result identity is verified") {
    val current = plan()
    val result  = current.run
    assertEquals(
      LocationReport.of(
        current,
        result,
        Some(Vector(LocationTrialMetadata(focal, 1, "a", "image", "family", "p", "c", "raw")))
      ),
      Left(LocationReportError.MissingMetadata(matched))
    )
    val duplicate = Vector(
      LocationTrialMetadata(focal, 1, "a", "image", "family", "p", "c", "raw"),
      LocationTrialMetadata(focal, 2, "b", "image", "family", "p", "c", "raw"),
      LocationTrialMetadata(matched, 3, "c", "image", "family", "p", "c", "raw"),
      LocationTrialMetadata(control, 4, "d", "image", "family", "p", "c", "raw")
    )
    assertEquals(
      LocationReport.of(current, result, Some(duplicate)),
      Left(LocationReportError.DuplicateMetadata(focal))
    )
    val surplus         = get(LocationTrialId.of("surplus"))
    val exactButSurplus = Vector(
      LocationTrialMetadata(focal, 1, "a", "image", "family", "p", "c", "raw"),
      LocationTrialMetadata(matched, 2, "b", "image", "family", "p", "c", "raw"),
      LocationTrialMetadata(control, 3, "c", "image", "family", "p", "c", "raw"),
      LocationTrialMetadata(surplus, 4, "d", "image", "family", "p", "c", "raw")
    )
    assertEquals(
      LocationReport.of(current, result, Some(exactButSurplus)),
      Left(LocationReportError.SurplusMetadata(surplus))
    )
    val other = get(LocationComparisonPlan.binned(current.input, 2, 2, current.queries))
    assertEquals(
      LocationReport.of(other, result),
      Left(LocationReportError.PlanHash(other.planHash, result.planHash))
    )
  }

  test("failed map and query outcomes remain in order with their reasons") {
    val current = plan()
    val run     = current.run
    val failure = LocationMapError.Surface(matched, SurfaceError.DegenerateTotal(0))
    val maps    = run.maps
      .map(map => map.trial -> map.result.map(_.bins))
      .updated(1, matched -> Left(failure))
    val outcomes = Vector(
      LocationStoredOutcome(
        "q",
        Left(LocationRunError.Map("q", "matched", matched, failure)),
        run.outcomes.head.control.map(_.value)
      )
    )
    val restored = get(
      LocationComparisonResult.restore(
        current,
        current.inputHash,
        current.planHash,
        maps,
        outcomes
      )
    )
    val report = get(LocationReport.of(current, restored))

    assertEquals(report.histograms.map(_.trial), Vector(focal, matched, control))
    assertEquals(report.histograms(1).bins, Left(failure))
    assertEquals(
      report.triples.head.matchedCosine,
      Left(LocationRunError.Map("q", "matched", matched, failure))
    )
    assertEquals(
      report.triples.head.difference,
      Left(LocationRunError.Map("q", "matched", matched, failure))
    )
    assertEquals(report.summary, LocationDifferenceSummary(0, 1, None))
    assertEquals(
      report.tables.head.column("matched_failure").get.head.text,
      LocationRunError.Map("q", "matched", matched, failure).message
    )
    assertEquals(report.tables(1).column("map_failure").get(1).text, failure.message)
  }

  test("Neumaier mean retains restored cosine cancellation residuals") {
    val queries = Vector(
      get(LocationComparisonQuery.of("q1", focal, matched, control)),
      get(LocationComparisonQuery.of("q2", focal, matched, control)),
      get(LocationComparisonQuery.of("q3", focal, matched, control))
    )
    val current  = plan(queries)
    val maps     = current.run.maps.map(map => map.trial -> map.result.map(_.bins))
    val residual = 1e-17
    val restored = get(
      LocationComparisonResult.restore(
        current,
        current.inputHash,
        current.planHash,
        maps,
        Vector(
          LocationStoredOutcome("q1", Right(1.0), Right(0.0)),
          LocationStoredOutcome("q2", Right(residual), Right(0.0)),
          LocationStoredOutcome("q3", Right(0.0), Right(1.0))
        )
      )
    )
    val mean = get(LocationReport.of(current, restored)).summary.meanDifference
      .getOrElse(fail("missing mean"))
    val expected = residual / 3.0

    assertEquals(mean, expected)
    assert(mean > 0.0)
    assert(math.abs(mean - expected) / expected <= 1e-15)
  }
