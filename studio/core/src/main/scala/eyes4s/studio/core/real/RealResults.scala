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
    keys: Map[TrialKey, CoreKey]
):
  import RealResults.*

  private def scale(
      index: Int
  ): Either[BackendError, ScaleInspection[CoreKey, Unit2D.Px, Similarity, SignedDifference]] =
    inspection.scales.lift(index).toRight(BackendError.UnknownScale(run, index, scales))

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
        case ref @ ResultRef.PairRow(_, design, _, _) => s.pairs(design).get(ref).map(entry)
        case _                                        => None
      }
      PairRowPage(run, index, PageInfo.of(page, all.size, rows.size), rows)
    }

  /** Serve the retained estimate through the library's checked rendering geometry. */
  def mapGrid(index: Int, trial: TrialKey): Either[BackendError, DensityGrid] =
    val address = ResultAddress.Estimation(index, trial)
    def none(message: String) = BackendError.NoDensity(run, address,
      BackendError.UnknownReference(run, address).diagnostic.copy(message = message))
    for
      s <- scale(index)
      key <- keys.get(trial).toRight(BackendError.UnknownReference(run, address))
      entry <- s.estimation.get(ResultRef.Estimation(index, key)).toRight(BackendError.UnknownReference(run, address))
      density <- entry.outcomes match
        case Vector(eyes4s.plan.EstimationOutcome.Estimated(value)) => Right(value)
        case Vector(eyes4s.plan.EstimationOutcome.Failed(error)) => Left(BackendError.NoDensity(run, address, diagnostic(error)))
        case found => Left(none(s"${address.render} has ${found.size} outcomes; no unique density can be served."))
      levels <- density.levels(Vector(0.5, 0.9)).leftMap(e => none(e.message))
      geometry = density.geometry
      region <- DensityGrid.region(run, index, trial, geometry.origin.x, geometry.origin.y,
        geometry.origin.x + geometry.bounds.width, geometry.origin.y + geometry.bounds.height).leftMap(e => none(e.message))
      sigma <- sigmas.lift(index).toRight(BackendError.UnknownScale(run, index, scales))
      order = geometry.yAxis match
        case eyes4s.kernel.YAxis.Down => RowOrder.TopFirst
        case eyes4s.kernel.YAxis.Up => RowOrder.BottomFirst
      grid <- DensityGrid.of(run, index, trial, sigma, region, density.nx, density.ny, order,
        geometry.cellDegrees.map(d => CellDegrees(d.width, d.height)), density.cells.toVector,
        levels.map(l => DensityLevel(l.coverage, l.threshold))).leftMap(e => none(e.message))
    yield grid

  /** The item an address names, as eyes4s holds it. */
  def inspect(address: ResultAddress): Either[BackendError, Inspection] =
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

  private def entry(p: eyes4s.plan.PairEntry[CoreKey, Similarity]): PairRowEntry =
    PairRowEntry(
      key(p.focal),
      PairDesign.of(p.design),
      key(p.reference),
      p.reference.item,
      p.outcome.fold(
        d => PairScoreState.Failed(diagnostic(d)),
        v => PairScoreState.Scored(v.value.value)
      )
    )

  /** Inspect `held` through eyes4s, over the plan, input and admission ledger
    * that produced it.
    */
  def of(run: RunId, held: RealStudyBackend.RealRun): Either[BackendError, RealResults] =
    val p = held.prepared
    ResultInspection
      .study(p.plan, held.result, p.admitted.input, Some(p.admitted.evidence))
      .leftMap(e =>
        BackendError.Unavailable(DiagnosticLocus.Artifact(s"${run.label}: ${e.message}"))
      )
      .map(inspection =>
        new RealResults(
          run,
          p.revision,
          p.dataset,
          p.summary.scales,
          p.recipe.scales.values.map(_.degrees),
          held.origin,
          inspection,
          p.admitted.input.trials.rows.map(r => key(r.key) -> r.key).toMap
        )
      )
