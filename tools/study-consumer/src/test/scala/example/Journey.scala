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
import eyes4s.compare.*
import eyes4s.core.*
import eyes4s.design.*
import eyes4s.fs2.*
import eyes4s.io.*
import eyes4s.kernel.*
import eyes4s.kernel.Unit2D.Px
import eyes4s.laws.StudyResultEquivalence
import eyes4s.plan.*
import eyes4s.surface.EdgePolicy
import example.FixationJourney.Reloaded

import java.nio.charset.StandardCharsets

/** One analysis route through the journey, with the mapping from its typed
  * keys to the oracle's `participant/image/phase` labels. `multiplier` is the
  * factor the route's method applies to cosine similarity.
  */
final class JourneyCase[K, P, S, D](
    val name: String,
    val route: () => AnalysisRoute[K, P, S, D],
    val key: (String, String, String) => K,
    val label: K => String,
    val multiplier: Double,
    val methodFields: Vector[String]
)

object JourneyCases:
  import JourneySetup.get

  private val items = Vector("a", "b", "c")

  val cosine: JourneyCase[StudyKey, Unit, Similarity, SignedDifference] = new JourneyCase(
    "cosine",
    () => get(AnalysisRoute.cosine("participant", "image", "phase")),
    StudyKey.apply,
    key => s"${key.participant}/${key.stimulus}/${key.phase}",
    1.0,
    Vector.empty
  )

  val scaled: JourneyCase[TrialKey, Multiplier, ScaledScore, SignedDifference] =
    new JourneyCase(
      "scaled",
      () =>
        get(
          AnalysisRoute.scaled(
            get(Multiplier.of(2.0)),
            "participant",
            "image",
            "phase",
            items.zipWithIndex.map((image, i) => image -> (i + 1)).toMap
          )
        ),
      (participant, image, phase) => TrialKey(participant, items.indexOf(image) + 1, phase),
      key => s"${key.subject}/${items(key.item - 1)}/${key.phase}",
      2.0,
      Vector("method.multiplier")
    )

/** The application's declared study: display geometry, table columns,
  * phases, scales built through the typed recipe descriptors, and explicit
  * budgets (the library defaults are effectively unbounded).
  */
object JourneySetup:
  def get[E, A](value: Either[E, A]): A =
    value.fold(error => throw new IllegalStateException(s"journey setup: $error"), identity)

  val frame: Frame[Px] = get(Frame.screen("display", 2, 2))
  val grid: Grid[Px]   = get(Grid.over(frame, 2, 2))
  val columns          = get(
    FixationColumns.of("fixation", "x_px", "y_px", "onset_us", "duration_us", "sample_count")
  )
  val phases = ("recall", "encode")

  val pairs: PairScheduleBudget    = get(PairScheduleBudget.of(64, 512, 64))
  val comparison: ComparisonBudget = get(ComparisonBudget.of(4))
  val quanta: WorkQuanta = WorkQuanta(get(PairQuantum.of(4)), get(ComparisonQuantum.of(2)))

  val sigmas                                     = Vector(0.5, 1.0, 2.0)
  def gaussian(sigma: Double): StudyEstimate[Px] =
    StudyEstimate.Gaussian(
      get(RecipeParameters.sigma[Px].parse(sigma)),
      get(RecipeParameters.edges.parse(EdgePolicy.Truncate))
    )
  val scales: Vector[StudyEstimate[Px]] = StudyEstimate.Binned[Px]() +: sigmas.map(gaussian)
  val weight: Weight                    = get(RecipeParameters.weight.parse(Weight.Duration))
  val policy: FailurePolicy             = get(RecipeParameters.failurePolicy.parse(None))

  def utf8(bytes: IArray[Byte]): String =
    new String(IArray.genericWrapArray(bytes).toArray, StandardCharsets.UTF_8)

  /** A fingerprint as receipt text: hexadecimal bits, `-` for a failure. */
  def render(bits: Vector[Option[Long]]): Vector[String] =
    bits.map(_.fold("-")(java.lang.Long.toHexString))

  /** A run id as receipt text, for comparison across processes. */
  def render(id: StudyRunId): String = id.toString

/** One route's journey, as an application runs it: the imported table, the
  * study, its prepared work and the steps the suites and the fresh reader
  * share. A refusal here is a broken fixture, reported with its typed error.
  */
final class Journey[K, P, S, D](val c: JourneyCase[K, P, S, D]):
  import JourneySetup.*

  type Outcome = StudyOutcome[K, Px, S, D]

  val route  = c.route()
  val runner = StudyExecution[IO]

  /** This route's key digest, for inputs the application assembles itself. */
  given KeyDigest[K] = route.layout.digest

  def read(table: String): FixationImport[K, Px] =
    get(FixationCsv.read(table, columns, route.reader, frame, TimestampUnit.Microseconds))

  lazy val imported: FixationImport[K, Px] = read(JourneyFixtures.table)
  lazy val ledger: AdmissionLedger[K]      =
    get(FixationEvidence.ledger("journey.csv", imported, AdmissionDecision.RequireComplete))
  lazy val input: StudyInput[K, Px] = get(imported.requireComplete)

  def plan(
      on: StudyInput[K, Px],
      estimates: Vector[StudyEstimate[Px]] = scales,
      weighting: Weight = weight
  ): StudyPlan[K, Px, P, S, D] =
    get(route.plan(on.reference, grid, phases, weighting, estimates, policy))

  lazy val study: StudyPlan[K, Px, P, S, D]    = plan(input)
  lazy val work: PreparedStudy[K, Px, P, S, D] =
    get(study.preflight(Some(input), pairs).prepare(study, input, pairs))

  lazy val schema: ScoreSchema[S, D] = get(ScoreSchema.study(study))

  def bits(result: StudyResult[K, Px, S, D]): Vector[Option[Long]] =
    get(FixationJourney.fingerprint(study, result))

  /** Bit-for-bit structural identity of two results of this route. */
  def same(a: StudyResult[K, Px, S, D], b: StudyResult[K, Px, S, D]): Boolean =
    bits(a) == bits(b) && StudyResultEquivalence.same(a, b)(
      (x, y) => schema.score(x).components == schema.score(y).components,
      (x, y) => schema.difference(x).components == schema.difference(y).components
    )

  def key(participant: String, image: String, phase: String): K =
    c.key(participant, image, phase)

  def trialLinks(
      source: SourceRef,
      participant: String,
      image: String,
      phase: String
  ): Vector[SourceLink[K]] =
    JourneyFixtures.records(participant, image, phase).map(SourceLink.Record(source, _))

  def completed(outcome: Outcome): StudyResult[K, Px, S, D] =
    outcome match
      case RunOutcome.Completed(_, _, result) => result
      case other => throw new IllegalStateException(s"expected a completed run: $other")

  /** The full event sequence of one pull-driven run. */
  def events: IO[Vector[StudyEvent[K, Px, S, D]]] =
    runner.events(work, comparison, quanta).compile.toVector

  /** Start a run, park it before step `completed + 1` and cancel it there. */
  def cancelAfter(completed: Int): IO[(Outcome, Vector[StudyProgress])] =
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

  /** Start a run, park it before step `k + 1`, read what a new progress
    * observer receives while the run is parked, then release the run and
    * await its outcome.
    */
  def observeParked(k: Int): IO[(Option[StudyProgress], Outcome)] =
    for
      started <- Ref[IO].of(0)
      reached <- Deferred[IO, Unit]
      release <- Deferred[IO, Unit]
      between = started.updateAndGet(_ + 1).flatMap { n =>
        if n == k + 1 then reached.complete(()) >> release.get else IO.unit
      }
      result <- runner.start(work, comparison, quanta, between).use { run =>
        for
          _       <- reached.get
          seen    <- run.progress.take(1).compile.last
          _       <- release.complete(())
          outcome <- run.outcome
        yield seen -> outcome
      }
    yield result

  /** A completed run and its saved manifest. */
  def saved: IO[(StudyResult[K, Px, S, D], SavedManifest)] =
    runner
      .start(work, comparison, quanta)
      .use(_.outcome)
      .map { outcome =>
        val result = completed(outcome)
        result -> get(FixationJourney.save(route, study, input, ledger, result))
      }

  /** The application's storage: a private copy of every file, by name. */
  def storage(saved: SavedManifest): Map[String, IArray[Byte]] =
    FixationJourney
      .files(saved)
      .map((name, bytes) => name -> IArray.tabulate(bytes.length)(bytes(_)))
      .toMap

  /** A reader that starts from the stored bytes and fresh registrations. */
  def reload(files: Map[String, IArray[Byte]]): Either[JourneyError, Reloaded[K, P, S, D]] =
    val address = get(ByteDigest.parse(utf8(files(FixationJourney.addressFile))))
    FixationJourney.resolve(c.route(), address, FixationJourney.source(files.get))

  /** Contrast values of one scale by oracle label. */
  def contrasts(result: StudyResult[K, Px, S, D], scale: Int): Vector[(String, Double)] =
    get(result.scales(scale).contrast).rows.map(row =>
      c.label(row.key) -> schema.difference(get(row.difference)).components.head.value
    )

  /** Reduced scores of one scale and design by oracle label. */
  def reductions(
      result: StudyResult[K, Px, S, D],
      scale: Int,
      design: StudyDesign
  ): Vector[(String, Double)] =
    result
      .scales(scale)
      .analyses
      .reduced(design)
      .entries
      .map(row => c.label(row.key) -> schema.score(get(row.result)).components.head.value)

  /** The records behind the first fixation-level drill-down of the journey:
    * the matched reference of `s1/a/recall` at the binned scale.
    */
  def drillDown(
      plan: StudyPlan[K, Px, P, S, D],
      result: StudyResult[K, Px, S, D],
      on: StudyInput[K, Px],
      admitted: AdmissionLedger[K]
  ): Either[DrillDownError[K], Vector[Int]] =
    val focal = key("s1", "a", "recall")
    val entry = for
      view    <- ResultInspection.study(plan, result, on, Some(admitted))
      row     <- view.contrastRow(ResultRef.ContrastRow(0, focal))
      matched <- row.matched.toRight(InspectionError.UnknownReference(row.ref))
      reduced <- view.reduction(matched)
      member  <- reduced.members.headOption.toRight(InspectionError.UnknownReference(matched))
      pair    <- view.pair(member.pair)
    yield view -> pair
    entry.left.map(DrillDownError.Inspection(_)).flatMap { (view, pair) =>
      (0 until 4).toVector
        .traverse(i => view.sources.fixation(pair.reference, i))
        .left
        .map(DrillDownError.Source(_))
        .map(_.map(_.record))
    }

/** Why a drill-down stopped: an unknown reference, or a missing source. */
enum DrillDownError[+K] derives CanEqual:
  case Inspection(error: InspectionError[K])
  case Source(missing: MissingSource[K])
