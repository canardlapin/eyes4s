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

import cats.effect.IO
import eyes4s.codec.*
import eyes4s.compare.ComparisonBudget
import eyes4s.design.*
import eyes4s.fs2.*
import eyes4s.io.ArtifactLoading
import eyes4s.kernel.Unit2D.Px
import eyes4s.plan.*

/** The steps of the temporal journey an application repeats after the
  * analysis has run once: save the base study input, its admission ledger,
  * the temporal input (base by reference), the plan and the result under one
  * manifest; reload them from bytes alone; rerun through the FS2 runner; and
  * fingerprint the result.
  */
object TemporalJourney:

  /** Manifest entry names; the application chooses them. */
  val baseEntry   = "base-input.json"
  val ledgerEntry = "ledger.json"
  val inputEntry  = "temporal-input.json"
  val planEntry   = "temporal-plan.json"
  val resultEntry = "temporal-result.json"

  /** A saved temporal run, typed again through the route's codecs. */
  final case class Reloaded[K, P, S, D](
      base: StudyInput[K, Px],
      ledger: AdmissionLedger[K],
      input: TemporalStudyInput[K, Px],
      plan: TemporalStudyPlan[K, Px, P, S, D],
      result: TemporalStudyResult[K, Px, P, S, D]
  )

  /** Save a completed temporal study with `LedgerOf`, `TemporalBase`,
    * `TemporalPlanInput` and `TemporalResultOf` relations.
    */
  def save[K, P, S, D](
      route: TemporalRoute[K, P, S, D],
      input: TemporalStudyInput[K, Px],
      ledger: AdmissionLedger[K],
      plan: TemporalStudyPlan[K, Px, P, S, D],
      result: TemporalStudyResult[K, Px, P, S, D]
  ): Either[CodecError, SavedManifest] = for
    b <- StoredArtifact.input(baseEntry, route.study.inputs, input.study)
    l <- StoredArtifact.ledger(ledgerEntry, route.study.inputs, ledger)
    t <- StoredArtifact.temporalInput(inputEntry, route.inputs, input)
    p <- StoredArtifact.temporalPlan(planEntry, route.persistence, plan)
    r <- StoredArtifact.temporalResult(resultEntry, route.results, result)
    s <- SavedManifest.of(
      Vector(b, l, t, p, r),
      Vector(
        ManifestRelation.LedgerOf(l.name, b.name),
        ManifestRelation.TemporalBase(t.name, b.name),
        ManifestRelation.TemporalPlanInput(p.name, t.name),
        ManifestRelation.TemporalResultOf(r.name, p.name, t.name)
      )
    )
  yield s

  /** Resolve and verify a stored run from bytes, then type it again. */
  def resolve[K, P, S, D](
      route: TemporalRoute[K, P, S, D],
      address: ByteDigest,
      source: ByteSource
  ): Either[JourneyError, Reloaded[K, P, S, D]] = for
    decoders <- route.decoders.left.map(JourneyError.Codec.apply)
    resolved <- ArtifactResolver
      .resolve(address, source, decoders)
      .left
      .map(JourneyError.Resolve.apply)
    run <- reload(route, resolved)
  yield run

  /** The same over storage read in `IO`, such as `ArtifactFiles.directory`. */
  def load[K, P, S, D](
      route: TemporalRoute[K, P, S, D],
      address: ByteDigest,
      read: ByteRequest => IO[ArtifactLoading.Read]
  ): IO[Either[JourneyError, Reloaded[K, P, S, D]]] =
    route.decoders match
      case Left(error)     => IO.pure(Left(JourneyError.Codec(error)))
      case Right(decoders) =>
        ArtifactLoading
          .resolve(address, read, decoders)
          .map(_.left.map(JourneyError.Resolve.apply).flatMap(reload(route, _)))

  /** Type a verified graph again through the route's codecs; `LoadedTemporal`
    * keeps its parameter and score types abstract.
    */
  def reload[K, P, S, D](
      route: TemporalRoute[K, P, S, D],
      resolved: ResolvedManifest[K, Px]
  ): Either[JourneyError, Reloaded[K, P, S, D]] =
    import SavedRun.single
    for
      base     <- single(ArtifactRole.StudyInput, resolved.inputs)
      ledger   <- single(ArtifactRole.AdmissionLedger, resolved.ledgers)
      input    <- single(ArtifactRole.TemporalInput, resolved.temporalInputs)
      loaded   <- single(ArtifactRole.TemporalPlan, resolved.temporalPlans)
      archived <- single(ArtifactRole.TemporalResult, resolved.temporalResults)
      plan     <- loaded.encode
        .flatMap(route.persistence.codec.decode)
        .left
        .map(JourneyError.Codec.apply)
      result <- archived.encode
        .flatMap(route.results.codec.decode)
        .left
        .map(JourneyError.Codec.apply)
    yield Reloaded(base, ledger, input, plan, result)

  /** Preflight the reloaded plan, prepare it with an explicit pair budget,
    * and run it on its own fiber.
    */
  def rerun[K, P, S, D](
      run: Reloaded[K, P, S, D],
      pairs: PairScheduleBudget,
      comparison: ComparisonBudget,
      quanta: WorkQuanta
  ): IO[Either[JourneyError, TemporalOutcome[K, Px, P, S, D]]] =
    val prepared = for
      _ <- run.plan
        .preflight(Some(run.input), pairs)
        .confirm(run.plan, run.input)
        .left
        .map(
          JourneyError.Preflight.apply
        )
      work <- run.plan.prepare(run.input, pairs).left.map(JourneyError.Temporal.apply)
    yield work
    prepared match
      case Left(error) => IO.pure(Left(error))
      case Right(work) =>
        TemporalExecution[IO].start(work, comparison, quanta).use(_.outcome).map(Right(_))

  /** The numbers of a temporal result in cell order: every occupancy's
    * interval, observed and missing time and every fixation's original and
    * retained microseconds (or a failure marker), then the cell's study
    * fingerprint (`FixationJourney.fingerprint`: masses, pair scores,
    * reductions and contrasts as raw IEEE 754 bits, `None` for a typed
    * failure).
    */
  def fingerprint[K, P, S, D](
      result: TemporalStudyResult[K, Px, P, S, D]
  ): Either[JourneyError, Vector[Option[Long]]] =
    result.cells.foldLeft[Either[JourneyError, Vector[Option[Long]]]](Right(Vector.empty)) {
      (acc, cell) =>
        for
          before <- acc
          study  <- FixationJourney.fingerprint(cell.study, cell.result)
        yield
          val occupancy = cell.occupancy.flatMap {
            case (_, Left(_))  => Vector(None)
            case (_, Right(o)) =>
              Vector(
                o.interval.onset.toMicros,
                o.interval.offset.toMicros,
                o.observedMicros,
                o.missingMicros
              ).map(Some(_)) ++ o.fixationTimes.flatMap(t =>
                Vector(
                  Some(t.index.toLong),
                  Some(t.originalMicros.toLong),
                  Some(t.retainedMicros)
                )
              )
          }
          before ++ occupancy ++ study
    }
