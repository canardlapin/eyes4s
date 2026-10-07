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

package eyes4s.studio.core.real

import cats.syntax.all.*
import eyes4s.compare.Similarity
import eyes4s.design.SignedDifference
import eyes4s.kernel.Unit2D
import eyes4s.plan.{
  ResultInspection,
  ResultRef,
  ScaleContrast,
  ScaleInspection,
  StudyDesign,
  StudyInspection,
  TrialKey as CoreKey
}
import eyes4s.studio.core.backend.*

/** A run's eyes4s result read through `ResultInspection` (S3.7 slice 5):
  * pair rows and inspected items, each value eyes4s's. Studio only orders
  * pages and converts keys.
  */
final class RealResults private (
    val run: RunId,
    val revision: AnalysisRevision,
    val dataset: DatasetRevision,
    val scales: Vector[String],
    sigmas: Vector[Double],
    origin: RealStudyBackend.RunOrigin,
    inspection: StudyInspection[CoreKey, Unit2D.Px, Similarity, SignedDifference],
    keys: Map[TrialKey, CoreKey],
    windowTallies: Map[CoreKey, eyes4s.plan.WindowTally],
    prepared: RealPrepared
):
  import RealResults.*

  private def scale(
      index: Int
  ): Either[BackendError, ScaleInspection[CoreKey, Unit2D.Px, Similarity, SignedDifference]] =
    inspection.scales.lift(index).toRight(BackendError.UnknownScale(run, index, scales))

  private def queryRefusal(trial: TrialKey, reason: String): BackendError =
    BackendError.Unavailable(
      DiagnosticLocus.Artifact(s"${run.label} query ${trial.label}: $reason")
    )

  /** Inventory order, with missing and unadmitted queries retained as rows.
    * References and control cardinality come from native inspected pair rows;
    * scores are the native contrast and its retained reduction operands.
    */
  private[real] lazy val queryRows: Either[BackendError, Vector[QueryRow]] =
    for
      inventory <- prepared.admitted.evidence.inventory.toRight(
        BackendError.Unavailable(DiagnosticLocus.Field(s"${run.label} trial inventory"))
      )
      reasons = eyes4s.plan.UnmatchedReasons.of(
        prepared.plan.layout,
        prepared.plan.pairing,
        prepared.plan.referencePhase,
        prepared.counts.cardinality.unmatched,
        inventory
      )
      rows <- prepared.admitted.ledger
        .filter(_.trial.phase == prepared.recipe.phases.focal)
        .traverse { entry =>
          val trial = entry.trial
          for
            response <- entry.response.toRight(
              queryRefusal(trial, "the inventory declares no response")
            )
            row <- entry.disposition match
              case TrialDisposition.Admitted =>
                for
                  core <- keys.get(trial).toRight(queryRefusal(trial, "no admitted key"))
                  row  <- reasons.reason(core) match
                    case Some(reason) =>
                      val d = diagnostic(
                        eyes4s.plan.Diagnostic.of(
                          eyes4s.plan.StudyFinding
                            .UnmatchedFocal[CoreKey, Unit2D.Px](core, reason)
                        )
                      )
                      Right(
                        QueryRow(
                          trial,
                          entry.item,
                          response,
                          None,
                          None,
                          QueryStatus.NoMatch(d)
                        )
                      )
                    case None =>
                      for
                        first <- scale(0)
                        matched = first
                          .pairsOfQuery(StudyDesign.Matched, core)
                          .collect { case ResultRef.PairRow(_, _, _, reference) =>
                            key(reference)
                          }
                          .distinct
                        reference <- matched match
                          case Vector(reference) => Right(reference)
                          case refs              =>
                            Left(
                              queryRefusal(
                                trial,
                                s"the query trail needs one matched reference, but the native result has ${refs.size}"
                              )
                            )
                        outcomes <- inspection.scales
                          .traverse(s => inspect(ResultAddress.ContrastRow(s.index, trial)))
                        successes = outcomes.collect { case Inspection.Contrast(_, m, b, d) =>
                          (m, b, d)
                        }
                        failures = outcomes.collect {
                          case Inspection.Unscored(_, QueryStatus.Failed(d)) => d
                        }
                        status <-
                          if successes.size == outcomes.size then
                            Right(
                              QueryStatus.Contributing(
                                successes.map(_._1),
                                successes.map(_._2),
                                successes.map(_._3)
                              )
                            )
                          else if failures.nonEmpty && failures.size == outcomes.size then
                            Right(QueryStatus.FailedAtScales(failures))
                          else
                            Left(
                              queryRefusal(
                                trial,
                                "the current query-row protocol cannot represent different scored/failed states by scale; inspect each scale separately"
                              )
                            )
                        controls = first
                          .reductions(StudyDesign.Control)
                          .get(ResultRef.Reduction(0, StudyDesign.Control, core))
                          .map(_.selected)
                      yield QueryRow(
                        trial,
                        entry.item,
                        response,
                        Some(reference),
                        controls,
                        status
                      )
                yield row
              case disposition =>
                Right(
                  QueryRow(
                    trial,
                    entry.item,
                    response,
                    None,
                    None,
                    QueryStatus.NotAdmitted(disposition)
                  )
                )
          yield row
        }
    yield rows

  def queries(page: PageRequest): Either[BackendError, QueryPage] =
    queryRows.map { all =>
      val rows = all.slice(page.offset, page.offset + page.size)
      QueryPage(run, PageInfo.of(page, all.size, rows.size), rows)
    }

  /** Counts of the native query listing, with no reporting or scale-bound means. */
  def summary: Either[BackendError, ResultSummary] = queryRows.map { rows =>
    def counts(source: Vector[QueryRow]): QueryContrasts =
      QueryContrasts(
        source.size,
        source.count(_.status.isInstanceOf[QueryStatus.NotAdmitted]),
        source.count(_.status.isInstanceOf[QueryStatus.NoMatch]),
        source.count(_.status.isFailed),
        source.count(_.status.isInstanceOf[QueryStatus.Contributing])
      )
    val participants =
      rows.groupBy(_.query.participant).toVector.sortBy(_._1).map { (participant, entries) =>
        val c = counts(entries)
        ParticipantCounts(
          participant,
          c.requested,
          c.contributing,
          c.failed,
          c.noMatch,
          c.queryNotAdmitted
        )
      }
    ResultSummary(
      run,
      revision,
      dataset,
      prepared.recipe.scales.values,
      prepared.counts.pairRowsPerScale,
      prepared.counts.totalPairs,
      prepared.counts.eligibleQueries.toInt,
      counts(rows),
      participants
    )
  }

  /** Every pair row at `index`, query by query in focal order, each query's
    * matched pairs before its controls (protocol 1.9).
    */
  private val pairOrder: Map[Int, Vector[ResultRef[CoreKey]]] =
    inspection.scales.map { s =>
      val matched                      = s.pairs(StudyDesign.Matched).references
      val control                      = s.pairs(StudyDesign.Control).references
      def focal(r: ResultRef[CoreKey]) = r match
        case ResultRef.PairRow(_, _, f, _) => Some(f)
        case _                             => None
      val byFocal = (matched ++ control).groupBy(focal)
      val order   = (matched ++ control).flatMap(focal).distinct
      s.index -> order.flatMap(f => byFocal.getOrElse(Some(f), Vector.empty))
    }.toMap

  def pairRows(index: Int, page: PageRequest): Either[BackendError, PairRowPage] =
    scale(index).map { s =>
      val all  = pairOrder.getOrElse(index, Vector.empty)
      val rows = all.slice(page.offset, page.offset + page.size).flatMap {
        case ref @ ResultRef.PairRow(_, design, _, _) =>
          s.pairs(design).get(ref).map(entry(_, windowTallies))
        case _ => None
      }
      PairRowPage(run, index, PageInfo.of(page, all.size, rows.size), rows)
    }

  /** Serve the retained estimate through the library's checked rendering geometry. */
  def mapGrid(index: Int, trial: TrialKey): Either[BackendError, DensityGrid] =
    val address               = ResultAddress.Estimation(index, trial)
    def none(message: String) = BackendError.NoDensity(
      run,
      address,
      BackendError.UnknownReference(run, address).diagnostic.copy(message = message)
    )
    for
      s     <- scale(index)
      key   <- keys.get(trial).toRight(BackendError.UnknownReference(run, address))
      entry <- s.estimation
        .get(ResultRef.Estimation(index, key))
        .toRight(BackendError.UnknownReference(run, address))
      density <- entry.outcomes match
        case Vector(eyes4s.plan.EstimationOutcome.Estimated(value)) => Right(value)
        case Vector(eyes4s.plan.EstimationOutcome.Failed(error))    =>
          Left(BackendError.NoDensity(run, address, diagnostic(error)))
        case found =>
          Left(
            none(
              s"${address.render} has ${found.size} outcomes; no unique density can be served."
            )
          )
      levels <- density.levels(Vector(0.5, 0.9)).leftMap(e => none(e.message))
      geometry = density.geometry
      region <- DensityGrid
        .region(
          run,
          index,
          trial,
          geometry.origin.x,
          geometry.origin.y,
          geometry.origin.x + geometry.bounds.width,
          geometry.origin.y + geometry.bounds.height
        )
        .leftMap(e => none(e.message))
      sigma <- sigmas.lift(index).toRight(BackendError.UnknownScale(run, index, scales))
      order = geometry.yAxis match
        case eyes4s.kernel.YAxis.Down => RowOrder.TopFirst
        case eyes4s.kernel.YAxis.Up   => RowOrder.BottomFirst
      grid <- DensityGrid
        .of(
          run,
          index,
          trial,
          sigma,
          region,
          density.nx,
          density.ny,
          order,
          geometry.cellDegrees.map(d => CellDegrees(d.width, d.height)),
          density.cells.toVector,
          levels.map(l => DensityLevel(l.coverage, l.threshold))
        )
        .leftMap(e => none(e.message))
    yield grid

  /** The item an address names, as eyes4s holds it. */
  def inspect(address: ResultAddress): Either[BackendError, Inspection] =
    scale(address.scale).flatMap { _ =>
      address match
        case ResultAddress.ContrastRow(_, trial) =>
          unscoredQuery(trial).flatMap {
            case Some(status) => Right(Inspection.Unscored(address, status))
            case None         => inspectNative(address)
          }
        case _ => inspectNative(address)
    }

  /** Inventory admission and native pairing refusals remain inspectable even
    * when no admitted contrast row exists. This does not read queryRows,
    * whose lazy construction itself calls inspect for scored/failed scales.
    */
  private def unscoredQuery(trial: TrialKey): Either[BackendError, Option[QueryStatus]] =
    prepared.admitted.ledger.find(_.trial == trial) match
      case Some(entry) if entry.disposition != TrialDisposition.Admitted =>
        Right(Some(QueryStatus.NotAdmitted(entry.disposition)))
      case _ =>
        keys.get(trial) match
          case Some(key) if prepared.counts.cardinality.unmatched.contains(key) =>
            for
              inventory <- prepared.admitted.evidence.inventory
                .toRight(queryRefusal(trial, "no retained inventory"))
              reason <- eyes4s.plan.UnmatchedReasons
                .of(
                  prepared.plan.layout,
                  prepared.plan.pairing,
                  prepared.plan.referencePhase,
                  prepared.counts.cardinality.unmatched,
                  inventory
                )
                .reason(key)
                .toRight(queryRefusal(trial, "no retained unmatched reason"))
            yield Some(
              QueryStatus.NoMatch(
                diagnostic(
                  eyes4s.plan.Diagnostic.of(
                    eyes4s.plan.StudyFinding.UnmatchedFocal[CoreKey, Unit2D.Px](key, reason)
                  )
                )
              )
            )
          case _ => Right(None)

  private def inspectNative(address: ResultAddress): Either[BackendError, Inspection] =
    def unknown           = BackendError.UnknownReference(run, address)
    def core(k: TrialKey) = keys.get(k).toRight(unknown)
    scale(address.scale).flatMap { s =>
      address match
        case ResultAddress.Estimation(_, _) =>
          // Density grids are served by their own request (protocol MapGridOf).
          Left(BackendError.Unavailable(DiagnosticLocus.Address(address)))
        case ResultAddress.PairRow(i, design, focal, reference) =>
          for
            f <- core(focal)
            r <- core(reference)
            p <- s
              .pairs(studyDesign(design))
              .get(ResultRef.PairRow(i, studyDesign(design), f, r))
              .toRight(unknown)
          yield p.outcome.fold(
            d => Inspection.Unscored(address, QueryStatus.Failed(diagnostic(d))),
            v => Inspection.Pair(address, r.item, v.value.value)
          )
        case ResultAddress.Reduction(i, design, key) =>
          for
            k <- core(key)
            e <- s
              .reductions(studyDesign(design))
              .get(ResultRef.Reduction(i, studyDesign(design), k))
              .toRight(unknown)
          yield e.outcome.fold(
            d => Inspection.Unscored(address, QueryStatus.Failed(diagnostic(d))),
            v => Inspection.Reduction(address, v.value.value, e.selected)
          )
        case ResultAddress.ContrastRow(i, key) =>
          for
            k    <- core(key)
            rows <- s.contrast match
              case ScaleContrast.Rows(rows) => Right(rows)
              // The scale's contrast was refused whole: eyes4s's diagnostic.
              case ScaleContrast.Failed(d) =>
                val diag = diagnostic(d)
                Left(
                  BackendError.Unavailable(
                    DiagnosticLocus.Artifact(
                      s"${address.render}: ${diag.code}: ${diag.message}"
                    )
                  )
                )
            row <- rows.get(ResultRef.ContrastRow(i, k)).toRight(unknown)
          yield
            val operands = for
              m <- row.matched
                .flatMap(s.reductions(StudyDesign.Matched).get)
                .flatMap(_.outcome.toOption)
              b <- row.control
                .flatMap(s.reductions(StudyDesign.Control).get)
                .flatMap(_.outcome.toOption)
            yield (m.value.value, b.value.value)
            (row.outcome, operands) match
              case (Right(d), Some((m, b))) => Inspection.Contrast(address, m, b, d.value.value)
              case (Left(d), _)             =>
                Inspection.Unscored(address, QueryStatus.Failed(diagnostic(d)))
              case (Right(_), None) =>
                // eyes4s scored a difference whose operands it does not hold: a defect.
                Inspection.Unscored(
                  address,
                  QueryStatus.Failed(
                    RealExecution.defect(
                      run,
                      RealExecution.Defect(s"${address.render}: no operands")
                    )
                  )
                )
    }

  /** The trail from the run to the item an address names, once eyes4s
    * holds that item: the run, its revision and dataset, the scale, the
    * design, and the trials with their items.
    */
  def provenance(address: ResultAddress): Either[BackendError, Provenance] =
    def unknown              = BackendError.UnknownReference(run, address)
    def core(k: TrialKey)    = keys.get(k).toRight(unknown)
    def held(found: Boolean) = Either.cond(found, (), unknown)
    def trial(k: CoreKey)    = ProvenanceStep.Trial(key(k), k.item)
    scale(address.scale).flatMap { s =>
      val tail = address match
        case ResultAddress.Estimation(i, k) =>
          core(k).flatMap(c =>
            held(s.estimation.contains(ResultRef.Estimation(i, c))).as(Vector(trial(c)))
          )
        case ResultAddress.ContrastRow(i, k) =>
          core(k).flatMap(c =>
            held(s.contrast match
              case ScaleContrast.Rows(rows) => rows.contains(ResultRef.ContrastRow(i, c))
              case ScaleContrast.Failed(_)  => false).as(Vector(trial(c)))
          )
        case ResultAddress.Reduction(i, design, k) =>
          val d = studyDesign(design)
          core(k).flatMap(c =>
            held(s.reductions(d).contains(ResultRef.Reduction(i, d, c)))
              .as(Vector(ProvenanceStep.Design(design), trial(c)))
          )
        case ResultAddress.PairRow(i, design, focal, reference) =>
          val d = studyDesign(design)
          (core(focal), core(reference)).tupled.flatMap((f, r) =>
            held(s.pairs(d).contains(ResultRef.PairRow(i, d, f, r)))
              .as(Vector(ProvenanceStep.Design(design), trial(f), trial(r)))
          )
      val recomputed = origin match
        case RealStudyBackend.RunOrigin.Computed            => Vector.empty
        case RealStudyBackend.RunOrigin.Recomputed(version) =>
          Vector(ProvenanceStep.Recomputed(version))
        case RealStudyBackend.RunOrigin.Restored(manifest) =>
          Vector(ProvenanceStep.Restored(manifest))
      tail.map(t =>
        Provenance(
          address,
          Vector(
            ProvenanceStep.Run(run)
          ) ++ recomputed ++ Vector(
            ProvenanceStep.Analysis(revision),
            ProvenanceStep.Dataset(dataset),
            ProvenanceStep.Scale(address.scale, scales(address.scale))
          ) ++ t
        )
      )
    }

object RealResults:
  /** A studio key of an eyes4s trial key (the item is carried separately). */
  def key(k: CoreKey): TrialKey =
    TrialKey(k.participant, Phase(k.phase), k.trial, k.occurrence.value)

  def studyDesign(design: PairDesign): StudyDesign = design match
    case PairDesign.Matched => StudyDesign.Matched
    case PairDesign.Control => StudyDesign.Control

  def diagnostic(d: eyes4s.plan.Diagnostic[CoreKey]): StudioDiagnostic =
    StudioDiagnostic.of(d, key)

  private def entry(
      p: eyes4s.plan.PairEntry[CoreKey, Similarity],
      tallies: Map[CoreKey, eyes4s.plan.WindowTally]
  ): PairRowEntry =
    PairRowEntry(
      key(p.focal),
      PairDesign.of(p.design),
      key(p.reference),
      p.reference.item,
      p.outcome.fold(
        d => PairScoreState.Failed(diagnostic(d)),
        v => PairScoreState.Scored(v.value.value)
      ),
      tallies
        .get(p.focal)
        .map(t => TrialTally(eyes4s.studio.core.selection.StudioRef.Trial(key(p.focal)), t)),
      tallies
        .get(p.reference)
        .map(t => TrialTally(eyes4s.studio.core.selection.StudioRef.Trial(key(p.reference)), t))
    )

  /** Inspect `held` through eyes4s, over the plan, input and admission ledger
    * that produced it.
    */
  def of(run: RunId, held: RealStudyBackend.RealRun): Either[BackendError, RealResults] =
    val p = held.prepared
    for
      inspection <- ResultInspection
        .study(p.plan, held.result, p.admitted.input, Some(p.admitted.evidence))
        .leftMap(e =>
          BackendError.Unavailable(DiagnosticLocus.Artifact(s"${run.label}: ${e.message}"))
        )
      tallies <- p.plan.windowTallies(p.admitted.input).traverse { (core, tally) =>
        tally.bimap(
          e =>
            BackendError
              .TrialViewRefused(TrialViewError.Study(key(core), "window tally", e.message)),
          value => core -> value
        )
      }
    yield new RealResults(
      run,
      p.revision,
      p.dataset,
      p.summary.scales,
      p.recipe.scales.values.map(_.degrees),
      held.origin,
      inspection,
      p.admitted.input.trials.rows.map(r => key(r.key) -> r.key).toMap,
      tallies.toMap,
      p
    )
