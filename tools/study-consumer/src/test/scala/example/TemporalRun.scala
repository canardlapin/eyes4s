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

package example

import cats.effect.{Deferred, IO, Ref}
import cats.syntax.all.*
import eyes4s.codec.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.fs2.*
import eyes4s.io.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.laws.TemporalResultEquivalence
import eyes4s.plan.*

/** One temporal route through the journey, as an application runs it: the
  * pinned temporal table imported through the fixation route's reader, a
  * measured epoch per trial (anchor 0 and the fixture's observed coverage on
  * the trial's own clock), the four windows and two repetitions of
  * `tools/r-parity/fixtures/temporal.json`, the binned and sigma 1 scales, and
  * the steps the suites and the fresh reader share.
  */
final class TemporalCase[K, P, S, D](val c: JourneyCase[K, P, S, D], schemaName: String):
  import JourneySetup.{comparison, gaussian, get, pairs, quanta, utf8}

  type Outcome = TemporalOutcome[K, Px, P, S, D]
  type Result  = TemporalStudyResult[K, Px, P, S, D]

  val journey = Journey(c)
  val route   = get(TemporalRoute.of(journey.route, schemaName))
  val runner  = TemporalExecution[IO]

  lazy val imported: FixationImport[K, Px] = journey.read(TemporalConsumerFixtures.csv)
  lazy val ledger: AdmissionLedger[K]      =
    get(FixationEvidence.ledger("temporal.csv", imported, AdmissionDecision.RequireComplete))
  lazy val base: StudyInput[K, Px] = get(imported.requireComplete)

  /** Every trial's epoch, except the trials in `without`. */
  def epochs(without: Set[String] = Set.empty): Vector[(K, TrialEpoch)] =
    base.trials.rows.filterNot(t => without.contains(c.label(t.key))).map { trial =>
      val clock = trial.value.clock
      val spans = TemporalConsumerFixtures.coverage(c.label(trial.key)).map { (from, until) =>
        get(Interval.of(clock, Instant.micros(from), Instant.micros(until)))
      }
      trial.key -> TrialEpoch(Instant.micros(0), get(ObservedCoverage.of(clock, spans)))
    }

  lazy val input: TemporalStudyInput[K, Px] = get(route.input(base, epochs()))

  val estimates: Vector[StudyEstimate[Px]] = Vector(StudyEstimate.Binned[Px](), gaussian(1.0))

  lazy val windows: Vector[StudyWindow] = TemporalConsumerFixtures.windows.map { (n, a, b) =>
    get(
      RecipeParameters.studyWindow.parse(
        n -> get(RecipeParameters.relativeWindow.parse((Span.micros(a), Span.micros(b))))
      )
    )
  }
  lazy val repetitions: Vector[RepetitionContrast] =
    TemporalConsumerFixtures.repetitions.map((n, f, r) =>
      get(RecipeParameters.repetition.parse((n, f, r)))
    )
  val boundary: FixationBoundary = get(
    RecipeParameters.boundary.parse(FixationBoundary.ClipDuration)
  )

  /** The temporal plan over `on`, with the base study's scales. */
  def plan(
      on: TemporalStudyInput[K, Px],
      scales: Vector[StudyEstimate[Px]] = estimates,
      cells: Vector[StudyWindow] = windows,
      repeats: Vector[RepetitionContrast] = repetitions,
      bound: FixationBoundary = boundary
  ): TemporalStudyPlan[K, Px, P, S, D] =
    get(route.plan(journey.plan(on.study, scales), on, cells, repeats, bound))

  lazy val study: TemporalStudyPlan[K, Px, P, S, D] = plan(input)

  lazy val work: PreparedTemporalStudy[K, Px, P, S, D] = prepare(study, input)

  /** Preflight, confirm and prepare `on` with the explicit pair budget. */
  def prepare(
      on: TemporalStudyPlan[K, Px, P, S, D],
      available: TemporalStudyInput[K, Px]
  ): PreparedTemporalStudy[K, Px, P, S, D] =
    get(on.preflight(Some(available), pairs).confirm(on, available))
    get(on.prepare(available, pairs))

  lazy val pure: Result = get(work.run)

  lazy val id: TemporalRunId = TemporalRunId.of(work, quanta)

  lazy val schema: ScoreSchema[S, D] = get(ScoreSchema.study(study.base))

  def bits(result: Result): Vector[Option[Long]] = get(TemporalJourney.fingerprint(result))

  /** Bit-for-bit structural identity of two results of this route. */
  def same(a: Result, b: Result): Boolean =
    bits(a) == bits(b) && TemporalResultEquivalence.same(a, b)(
      (x, y) => schema.score(x).components == schema.score(y).components,
      (x, y) => schema.difference(x).components == schema.difference(y).components
    )

  def completed(outcome: Outcome): Result = outcome match
    case RunOutcome.Completed(_, _, result) => result
    case other => throw new IllegalStateException(s"expected a completed run: $other")

  def events: IO[Vector[TemporalEvent[K, Px, P, S, D]]] =
    runner.events(work, comparison, quanta).compile.toVector

  /** Start a run, park it before step `completed + 1` and cancel it there. */
  def cancelAfter(completed: Int): IO[(Outcome, Vector[TemporalProgress])] =
    for
      started <- Ref[IO].of(0)
      reached <- Deferred[IO, Unit]
      parked  <- Deferred[IO, Unit]
      between = started.updateAndGet(_ + 1).flatMap { n =>
        if n == completed + 1 then reached.complete(()) >> parked.get else IO.unit
      }
      result <- runner.start(work, comparison, quanta, between).use { run =>
        reached.get >> run.cancel >> (run.outcome, run.progress.compile.toVector).tupled
      }
    yield result

  /** A completed streamed run and its saved manifest. */
  def saved: IO[(Result, SavedManifest)] =
    runner.start(work, comparison, quanta).use(_.outcome).map { outcome =>
      val result = completed(outcome)
      result -> get(TemporalJourney.save(route, input, ledger, study, result))
    }

  def storage(saved: SavedManifest): Map[String, IArray[Byte]] =
    SavedRun
      .files(saved)
      .map((name, bytes) => name -> IArray.tabulate(bytes.length)(bytes(_)))
      .toMap

  def reload(
      files: Map[String, IArray[Byte]]
  ): Either[JourneyError, TemporalJourney.Reloaded[K, P, S, D]] =
    val address = get(ByteDigest.parse(utf8(files(SavedRun.addressFile))))
    TemporalJourney.resolve(route, address, SavedRun.source(files.get))

  def archived(result: Result): ByteDigest =
    get(StoredArtifact.temporalResult("rerun", route.results, result)).entry.sha256

  /** Every contrast by repetition, oracle label, window and scale (`None` for
    * binned, `Some(sigma)` for Gaussian); `None` where the row failed.
    */
  def contrasts(result: Result): Map[(String, String, String, Option[Double]), Option[Double]] =
    result.cells.flatMap { cell =>
      cell.result.scales.flatMap { scale =>
        val sigma = scale.estimate match
          case StudyEstimate.Gaussian(s, _)       => Some(s.value)
          case StudyEstimate.Binned()             => None
          case StudyEstimate.Anisotropic(_, _, _) =>
            throw new AssertionError(
              "This pinned scalar-bandwidth oracle fixture has no anisotropic scale"
            )
        scale.contrast.toOption.toVector.flatMap(_.rows).map { row =>
          (cell.repetition.name, c.label(row.key), cell.window.name, sigma) ->
            row.difference.toOption.map(d => schema.difference(d).components.head.value)
        }
      }
    }.toMap

  /** Every trial's occupancy ledger by oracle label and window: each
    * fixation's retained microseconds, the observed and the missing time.
    */
  def ledgers(result: Result): Vector[((String, String), (Vector[Long], Long, Long))] =
    result.cells.flatMap { cell =>
      cell.occupancy.collect { case (key, Right(o)) =>
        (c.label(key), cell.window.name) ->
          (o.fixationTimes.map(_.retainedMicros), o.observedMicros, o.missingMicros)
      }
    }

/** The consumer's two temporal routes: the shipped cosine and its scaled cosine. */
object TemporalRun:
  val cosine: TemporalCase[StudyKey, Unit, eyes4s.compare.Similarity, SignedDifference] =
    TemporalCase(JourneyCases.cosine, "eyes4s.temporal-study")
  val scaled: TemporalCase[SubjectItemKey, Multiplier, ScaledScore, SignedDifference] =
    TemporalCase(JourneyCases.scaled, "my.lab.temporal-study")
