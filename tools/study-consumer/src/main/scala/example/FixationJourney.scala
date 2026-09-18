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

/** The steps of the fixation-only journey that an application repeats after
  * the analysis has run once: save a run, reload it from bytes alone, rerun it
  * with progress and cancellation, and fingerprint its numbers. Each step is
  * a published eyes4s call; this object only fixes the application's own
  * choices (entry names, file layout, the effect type) and keeps every
  * failure typed.
  */
object FixationJourney:

  /** Manifest entry names; the application chooses them. */
  val planEntry   = "plan.json"
  val inputEntry  = "input.json"
  val ledgerEntry = "ledger.json"
  val resultEntry = "result.json"

  /** The application's file layout, shared by every route; see [[SavedRun]]. */
  val manifestFile = SavedRun.manifestFile
  val addressFile  = SavedRun.addressFile

  /** A saved run, typed again through the route's own codecs. */
  final case class Reloaded[K, P, S, D](
      plan: StudyPlan[K, Px, P, S, D],
      input: StudyInput[K, Px],
      ledger: AdmissionLedger[K],
      result: StudyResult[K, Px, S, D]
  )

  /** Save the plan, its admitted input, the admission ledger and a completed
    * result under one manifest with typed relations between them.
    */
  def save[K, P, S, D](
      route: AnalysisRoute[K, P, S, D],
      plan: StudyPlan[K, Px, P, S, D],
      input: StudyInput[K, Px],
      ledger: AdmissionLedger[K],
      result: StudyResult[K, Px, S, D]
  ): Either[CodecError, SavedManifest] = for
    p <- StoredArtifact.plan(planEntry, route.persistence, plan)
    i <- StoredArtifact.input(inputEntry, route.inputs, input)
    l <- StoredArtifact.ledger(ledgerEntry, route.inputs, ledger)
    r <- StoredArtifact.result(resultEntry, route.results, result)
    s <- SavedManifest.of(
      Vector(p, i, l, r),
      Vector(
        ManifestRelation.PlanInput(p.name, i.name),
        ManifestRelation.LedgerOf(l.name, i.name),
        ManifestRelation.ResultOf(r.name, p.name, i.name)
      )
    )
  yield s

  /** Every file of a saved run under the application's layout. Entry names
    * must differ from the two layout files; `save` uses the names above.
    */
  def files(saved: SavedManifest): Vector[(String, IArray[Byte])] = SavedRun.files(saved)

  /** Serve a stored run from whatever holds its files, by the layout above. */
  def source(files: String => Option[IArray[Byte]]): ByteSource = SavedRun.source(files)

  /** Resolve and verify a stored run from bytes, then type it again. */
  def resolve[K, P, S, D](
      route: AnalysisRoute[K, P, S, D],
      address: ByteDigest,
      source: ByteSource
  ): Either[JourneyError, Reloaded[K, P, S, D]] = for
    decoders <- route.decoders.left.map(JourneyError.Codec.apply)
    resolved <- ArtifactResolver
      .resolve(address, source, decoders)
      .left
      .map(JourneyError.Resolve.apply)
    study <- reload(route, resolved)
  yield study

  /** The same over storage read in `IO`, such as `ArtifactFiles.directory`. */
  def load[K, P, S, D](
      route: AnalysisRoute[K, P, S, D],
      address: ByteDigest,
      read: ByteRequest => IO[ArtifactLoading.Read]
  ): IO[Either[JourneyError, Reloaded[K, P, S, D]]] =
    route.decoders match
      case Left(error)     => IO.pure(Left(JourneyError.Codec(error)))
      case Right(decoders) =>
        ArtifactLoading
          .resolve(address, read, decoders)
          .map(_.left.map(JourneyError.Resolve.apply).flatMap(reload(route, _)))

  /** Type a verified graph again through the route's codecs. The resolver
    * returns plans and results with their parameter and score types
    * abstract; the application knows its registration, so it re-reads the
    * verified values with the typed codec it registered.
    */
  def reload[K, P, S, D](
      route: AnalysisRoute[K, P, S, D],
      resolved: ResolvedManifest[K, Px]
  ): Either[JourneyError, Reloaded[K, P, S, D]] =
    import SavedRun.single
    for
      loaded   <- single(ArtifactRole.StudyPlan, resolved.plans)
      input    <- single(ArtifactRole.StudyInput, resolved.inputs)
      ledger   <- single(ArtifactRole.AdmissionLedger, resolved.ledgers)
      archived <- single(ArtifactRole.StudyResult, resolved.results)
      plan     <- loaded.encode
        .flatMap(route.persistence.codec.decode)
        .left
        .map(JourneyError.Codec.apply)
      result <- archived.encode
        .flatMap(route.results.codec.decode)
        .left
        .map(JourneyError.Codec.apply)
    yield Reloaded(plan, input, ledger, result)

  /** Preflight the reloaded plan against its reloaded input and run it on
    * its own fiber; the outcome is the run's single authoritative end.
    */
  def rerun[K, P, S, D](
      study: Reloaded[K, P, S, D],
      pairs: PairScheduleBudget,
      comparison: ComparisonBudget,
      quanta: WorkQuanta
  ): IO[Either[JourneyError, StudyOutcome[K, Px, S, D]]] =
    study.plan
      .preflight(Some(study.input), pairs)
      .prepare(study.plan, study.input, pairs) match
      case Left(error) => IO.pure(Left(JourneyError.Preflight(error)))
      case Right(work) =>
        StudyExecution[IO].start(work, comparison, quanta).use(_.outcome).map(Right(_))

  /** The numeric outcomes of a result in result order: every trial's cell
    * masses, every pair score, every reduced score and every contrast
    * difference, each value through the method's described components as its
    * raw IEEE 754 bits, and `None` in the place of each outcome that is a
    * typed failure. Equal fingerprints mean equal numbers, bit for bit
    * (`-0.0` and `+0.0` apart), and failures in the same places. Keys, the
    * failures' own operands, reports and provenance are not here: compare
    * those structurally (`StudyResultEquivalence`) or by the result's
    * canonical archive digest.
    */
  def fingerprint[K, P, S, D](
      plan: StudyPlan[K, Px, P, S, D],
      result: StudyResult[K, Px, S, D]
  ): Either[JourneyError, Vector[Option[Long]]] =
    ScoreSchema.study(plan).left.map(JourneyError.Descriptor.apply).map { schema =>
      def bits(values: Vector[Double]) =
        values.map(v => Some(java.lang.Double.doubleToRawLongBits(v)))
      def outcome[E, A](value: Either[E, A])(components: A => Vector[Double]) =
        value.fold(_ => Vector(None), a => bits(components(a)))
      def scores(value: S)      = schema.score(value).components.map(_.value)
      def differences(value: D) = schema.difference(value).components.map(_.value)
      result.scales.flatMap { scale =>
        scale.estimation.flatMap((_, mass) => outcome(mass)(_.values.toVector)) ++
          StudyDesign.values.toVector.flatMap { design =>
            scale.analyses.source(design).rows.flatMap(row => outcome(row.result)(scores)) ++
              scale.analyses.reduced(design).entries.flatMap(row => outcome(row.result)(scores))
          } ++
          scale.contrast.fold(
            _ => Vector(None),
            _.rows.flatMap(row => outcome(row.difference)(differences))
          )
      }
    }
