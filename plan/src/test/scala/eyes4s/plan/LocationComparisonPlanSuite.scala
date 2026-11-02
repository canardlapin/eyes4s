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

import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.compare.Similarity.*

class LocationComparisonPlanSuite extends munit.FunSuite:
  private val HandCalculatedTolerance           = 1e-12
  private def get[E, A](value: Either[E, A]): A =
    value.fold(error => fail(error.toString), identity)
  private val frame   = get(Frame.screen("location-comparison", 6, 4))
  private val focal   = get(LocationTrialId.of("focal"))
  private val matched = get(LocationTrialId.of("matched"))
  private val control = get(LocationTrialId.of("control"))
  private val source  = get(LocationSourceId.of("locations.tsv"))

  private def row(ordinal: Long, x: Double, y: Double) =
    LocationObservation(source, ordinal, Pt[Px](x, y))
  private def trial(id: LocationTrialId, rows: LocationObservation*) =
    get(LocationTrial.of(id, rows.toVector))
  private def query(id: String = "q") =
    get(LocationComparisonQuery.of(id, focal, matched, control))

  private def input(focalRows: Vector[LocationObservation]) =
    get(
      LocationInput.of(
        frame,
        Vector(
          get(LocationTrial.of(focal, focalRows)),
          trial(matched, row(4, 0.1, 0.1), row(5, 2.1, 0.1)),
          trial(control, row(6, 5.9, 3.9))
        )
      )
    )

  test("an asymmetric grid retains exact row-major bins and independent cosine scores") {
    val locations = input(Vector(row(1, 0.1, 0.1), row(2, 0.2, 0.2), row(3, 2.1, 0.1)))
    val result    = get(LocationComparisonPlan.binned(locations, 3, 2, Vector(query()))).run
    val map       =
      get(result.maps.find(_.trial == focal).map(_.result).getOrElse(fail("missing focal map")))
    val outcome = result.outcomes.head

    assertEquals(map.bins, Vector(2.0, 1.0, 0.0, 0.0, 0.0, 0.0))
    assertEquals(result.grid.nx, 3)
    assertEquals(result.grid.ny, 2)
    assertEqualsDouble(
      get(outcome.matched).value,
      3.0 / math.sqrt(10.0),
      HandCalculatedTolerance
    )
    assertEqualsDouble(get(outcome.control).value, 0.0, HandCalculatedTolerance)
    assertEqualsDouble(get(outcome.difference), 3.0 / math.sqrt(10.0), HandCalculatedTolerance)
  }

  test(
    "execution advances one map or query triple, resumes immutably, and drains to the hand oracle"
  ) {
    val locations = input(Vector(row(1, 0.1, 0.1), row(2, 0.2, 0.2), row(3, 2.1, 0.1)))
    val q2        = get(LocationComparisonQuery.of("q2", matched, focal, control))
    val plan      = get(LocationComparisonPlan.binned(locations, 3, 2, Vector(query(), q2)))
    val initial   = plan.execution

    assertEquals(
      (
        initial.completedMaps,
        initial.totalMaps,
        initial.completedQueries,
        initial.totalQueries
      ),
      (0, 3, 0, 2)
    )
    assertEquals(initial.result, None)

    val oneMap = initial.step
    assertEquals((oneMap.completedMaps, oneMap.completedQueries), (1, 0))
    assertEquals(
      (initial.completedMaps, initial.completedQueries),
      (0, 0),
      "previous cursor changed"
    )

    val paused = oneMap.step.step
    assertEquals((paused.completedMaps, paused.completedQueries), (3, 0))
    assertEquals(paused.result, None)

    val oneQuery = paused.step
    assertEquals((oneQuery.completedMaps, oneQuery.completedQueries), (3, 1))
    assertEquals(paused.completedQueries, 0, "paused cursor changed")
    assertEquals(oneQuery.result, None)

    val finished = oneQuery.step
    val result   = finished.result.getOrElse(fail("no completed result"))
    val oracle   = plan.run
    assert(finished.complete)
    assert(finished.isComplete)
    assert(finished.step eq finished, "a completed cursor must be idempotent")
    assertEquals(
      (
        finished.completedMaps,
        finished.completedQueries,
        finished.totalMaps,
        finished.totalQueries
      ),
      (3, 2, 3, 2)
    )
    assertEquals(result.maps.map(_.trial), Vector(focal, matched, control))
    assertEquals(result.outcomes.map(_.query.id), Vector("q", "q2"))
    assertEquals(
      result.maps.map(_.result.map(_.bins)),
      oracle.maps.map(_.result.map(_.bins)),
      "cursor bins differ from the eager hand oracle"
    )
    assertEquals(result.outcomes.map(_.matched), oracle.outcomes.map(_.matched))
    assertEquals(result.outcomes.map(_.control), oracle.outcomes.map(_.control))
    assertEquals(result.outcomes.map(_.difference), oracle.outcomes.map(_.difference))
  }

  test("source-row permutation preserves bins and explicit-triple scores") {
    val original = input(Vector(row(1, 0.1, 0.1), row(2, 0.2, 0.2), row(3, 2.1, 0.1)))
    val permuted = input(Vector(row(3, 2.1, 0.1), row(1, 0.1, 0.1), row(2, 0.2, 0.2)))
    val a = get(LocationComparisonPlan.binned(original, 3, 2, Vector(query())).map(_.run))
    val b = get(LocationComparisonPlan.binned(permuted, 3, 2, Vector(query())).map(_.run))

    assertEquals(get(a.maps.head.result).bins, get(b.maps.head.result).bins)
    assertEqualsDouble(
      get(a.outcomes.head.matched).value,
      get(b.outcomes.head.matched).value,
      HandCalculatedTolerance
    )
    assertEqualsDouble(
      get(a.outcomes.head.control).value,
      get(b.outcomes.head.control).value,
      HandCalculatedTolerance
    )
  }

  test("queries and plans reject missing references, self comparisons, and empty query sets") {
    val locations = input(Vector(row(1, 0.1, 0.1), row(2, 0.2, 0.2), row(3, 2.1, 0.1)))
    val missing   = get(LocationTrialId.of("missing"))

    assertEquals(
      LocationComparisonQuery.of("", focal, matched, control),
      Left(LocationQueryError.EmptyQueryId(""))
    )
    assertEquals(
      LocationComparisonQuery.of("  ", focal, matched, control),
      Left(LocationQueryError.EmptyQueryId("  "))
    )
    assertEquals(
      LocationComparisonQuery.of("self", focal, focal, control),
      Left(LocationQueryError.SameTrial("self", "focal", focal, "matched", focal))
    )
    assertEquals(
      LocationComparisonQuery.of("self", focal, matched, focal),
      Left(LocationQueryError.SameTrial("self", "focal", focal, "control", focal))
    )
    assertEquals(
      LocationComparisonQuery.of("self", focal, matched, matched),
      Left(LocationQueryError.SameTrial("self", "matched", matched, "control", matched))
    )
    assertEquals(
      LocationComparisonPlan.binned(locations, 3, 2, Vector.empty),
      Left(LocationPlanError.EmptyQueries)
    )
    assertEquals(
      LocationComparisonPlan.binned(locations, 3, 2, Vector(query(), query())),
      Left(LocationPlanError.DuplicateQuery("q"))
    )
    val missingQuery = get(LocationComparisonQuery.of("missing", focal, matched, missing))
    assertEquals(
      LocationComparisonPlan.binned(locations, 3, 2, Vector(missingQuery)),
      Left(LocationPlanError.MissingTrial("missing", "control", missing))
    )
  }

  test("cold reconstruction canonicalises stored order without binning or scoring") {
    val locations = input(Vector(row(1, 0.1, 0.1), row(2, 0.2, 0.2), row(3, 2.1, 0.1)))
    val plan      = get(LocationComparisonPlan.binned(locations, 3, 2, Vector(query())))
    val run       = plan.run
    val maps      = run.maps.map(map => map.trial -> map.result.map(_.bins)).reverse
    val outcomes  = run.outcomes.map { outcome =>
      LocationStoredOutcome(
        outcome.query.id,
        outcome.matched.map(_.value),
        outcome.control.map(_.value)
      )
    }.reverse
    val restored =
      get(LocationComparisonResult.restore(plan, plan.inputHash, plan.planHash, maps, outcomes))

    assertEquals(restored.maps.map(_.trial), plan.input.trials.map(_.id))
    assertEquals(restored.outcomes.map(_.query.id), plan.queries.map(_.id))
    assertEquals(
      restored.maps.map(map => get(map.result).bins),
      run.maps.map(map => get(map.result).bins)
    )
    assertEqualsDouble(
      get(restored.outcomes.head.matched).value,
      get(run.outcomes.head.matched).value,
      HandCalculatedTolerance
    )
    assertEqualsDouble(
      get(restored.outcomes.head.difference),
      get(run.outcomes.head.difference),
      HandCalculatedTolerance
    )
  }

  test("cold reconstruction rejects malformed bins, scores, and stored identities") {
    val locations = input(Vector(row(1, 0.1, 0.1), row(2, 0.2, 0.2), row(3, 2.1, 0.1)))
    val plan      = get(LocationComparisonPlan.binned(locations, 3, 2, Vector(query())))
    val run       = plan.run
    val maps      = run.maps.map(map => map.trial -> map.result.map(_.bins))
    val outcomes  = run.outcomes.map(outcome =>
      LocationStoredOutcome(
        outcome.query.id,
        outcome.matched.map(_.value),
        outcome.control.map(_.value)
      )
    )
    val focalBins = maps.head._2.toOption.get

    assertEquals(
      LocationComparisonResult.restore(
        plan,
        plan.inputHash,
        plan.planHash,
        maps.updated(0, focal -> Right(focalBins.drop(1))),
        outcomes
      ),
      Left(LocationRestoreError.BinLength(focal, 6, 5))
    )
    assertEquals(
      LocationComparisonResult.restore(
        plan,
        plan.inputHash,
        plan.planHash,
        maps.updated(0, focal -> Right(focalBins.updated(0, 1.5))),
        outcomes
      ),
      Left(LocationRestoreError.BinValue(focal, 0, 1.5))
    )
    assertEquals(
      LocationComparisonResult.restore(
        plan,
        plan.inputHash,
        plan.planHash,
        maps.updated(0, focal -> Right(focalBins.updated(0, 1.0))),
        outcomes
      ),
      Left(LocationRestoreError.BinTotal(focal, 3, 2.0))
    )
    val invalidScore = outcomes.updated(0, outcomes.head.copy(matched = Right(2.0)))
    assertEquals(
      LocationComparisonResult.restore(plan, plan.inputHash, plan.planHash, maps, invalidScore),
      Left(LocationRestoreError.InvalidScore("q", "matched", 2.0))
    )
    assertEquals(
      LocationComparisonResult
        .restore(plan, plan.inputHash, plan.planHash, maps.tail, outcomes),
      Left(LocationRestoreError.MapTrialSet(plan.input.trials.map(_.id), maps.tail.map(_._1)))
    )
    val unknownOutcome = outcomes.updated(0, outcomes.head.copy(query = "unknown"))
    assertEquals(
      LocationComparisonResult
        .restore(plan, plan.inputHash, plan.planHash, maps, unknownOutcome),
      Left(LocationRestoreError.OutcomeQuerySet(Vector("q"), Vector("unknown")))
    )
  }

  test("cold reconstruction applies the named cosine tolerance without clamping") {
    val identical = get(
      LocationInput.of(
        frame,
        Vector(
          trial(focal, row(1, 0.1, 0.1), row(2, 2.1, 0.1)),
          trial(matched, row(3, 0.1, 0.1), row(4, 2.1, 0.1)),
          trial(control, row(5, 5.9, 3.9))
        )
      )
    )
    val plan   = get(LocationComparisonPlan.binned(identical, 3, 2, Vector(query())))
    val run    = plan.run
    val maps   = run.maps.map(map => map.trial -> map.result.map(_.bins))
    val stored = run.outcomes.map(outcome =>
      LocationStoredOutcome(
        outcome.query.id,
        outcome.matched.map(_.value),
        outcome.control.map(_.value)
      )
    )
    assertEqualsDouble(get(run.outcomes.head.matched).value, 1.0, HandCalculatedTolerance)

    val nearUpper = 1.0 + LocationComparisonResult.CosineBoundTolerance / 2.0
    val accepted  = stored.updated(0, stored.head.copy(matched = Right(nearUpper)))
    assertEqualsDouble(
      get(
        get(
          LocationComparisonResult.restore(plan, plan.inputHash, plan.planHash, maps, accepted)
        ).outcomes.head.matched
      ).value,
      nearUpper,
      0.0
    )
    Vector(-0.01, 1.01).foreach { invalid =>
      val changed = stored.updated(0, stored.head.copy(matched = Right(invalid)))
      assertEquals(
        LocationComparisonResult.restore(plan, plan.inputHash, plan.planHash, maps, changed),
        Left(LocationRestoreError.InvalidScore("q", "matched", invalid))
      )
    }
    val nonFinite = stored.updated(0, stored.head.copy(matched = Right(Double.NaN)))
    LocationComparisonResult.restore(plan, plan.inputHash, plan.planHash, maps, nonFinite) match
      case Left(LocationRestoreError.InvalidScore("q", "matched", value)) => assert(value.isNaN)
      case other => fail(s"Expected non-finite-score refusal, got $other")
  }

  test(
    "cold reconstruction binds coordinates and explicit query operands through content hashes"
  ) {
    val original = input(Vector(row(1, 0.1, 0.1), row(2, 0.2, 0.2), row(3, 2.1, 0.1)))
    val plan     = get(LocationComparisonPlan.binned(original, 3, 2, Vector(query())))
    val run      = plan.run
    val maps     = run.maps.map(map => map.trial -> map.result.map(_.bins))
    val outcomes = run.outcomes.map(outcome =>
      LocationStoredOutcome(
        outcome.query.id,
        outcome.matched.map(_.value),
        outcome.control.map(_.value)
      )
    )
    val changedCoordinates = input(Vector(row(1, 1.1, 0.1), row(2, 0.2, 0.2), row(3, 2.1, 0.1)))
    val coordinatePlan     =
      get(LocationComparisonPlan.binned(changedCoordinates, 3, 2, Vector(query())))
    assertEquals(
      LocationComparisonResult
        .restore(coordinatePlan, plan.inputHash, plan.planHash, maps, outcomes),
      Left(LocationRestoreError.InputHash(coordinatePlan.inputHash, plan.inputHash))
    )
    val swapped     = get(LocationComparisonQuery.of("q", focal, control, matched))
    val swappedPlan = get(LocationComparisonPlan.binned(original, 3, 2, Vector(swapped)))
    assertEquals(
      LocationComparisonResult
        .restore(swappedPlan, plan.inputHash, plan.planHash, maps, outcomes),
      Left(LocationRestoreError.PlanHash(swappedPlan.planHash, plan.planHash))
    )
  }
