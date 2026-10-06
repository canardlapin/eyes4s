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
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.*

/** The authoritative current document plus definitions already admitted or
  * saved under nominal ids. Old held jobs/results retain their own snapshots.
  */
private[real] final class RealRegistry private (
    val document: StudioDocument,
    protectedDatasets: Map[DatasetRevision, DatasetRevisionSpec],
    protectedRevisions: Map[AnalysisRevision, AnalysisRevisionSpec],
    protectedRuns: Map[RunId, RunRef],
    val reservations: Set[RunId]
):
  val datasets: Map[DatasetRevision, DatasetRevisionSpec] =
    document.datasets.map(d => d.id -> d).toMap
  val revisions: Map[AnalysisRevision, (DatasetRevision, Recipe)] =
    val saved = document.analyses.map(a => a.id -> (a.dataset, a.recipe))
    val draft = document.draft.flatMap(d =>
      document
        .analysis(d.base)
        .map(base => d.id -> (d.dataset.getOrElse(base.dataset), d.recipe(base.recipe)))
    )
    (saved ++ draft).toMap
  val planBindings: Map[AnalysisRevision, CoreBinding[StudyPlanArtifact]] =
    document.analyses.map(a => a.id -> a.plan).toMap
  val documentRuns: Map[RunId, RunRef]                = document.runs.map(r => r.id -> r).toMap
  val reportingSpecs: Map[ReportingId, ReportingSpec] =
    document.reporting.map(s => s.id -> s).toMap
  val knownDatasets: Vector[DatasetRevision]   = datasets.keys.toVector.sortBy(_.number)
  val knownRevisions: Vector[AnalysisRevision] = revisions.keys.toVector.sortBy(_.number)

  private def refused(locus: DiagnosticLocus, reason: String) =
    BackendError.RegistryRefused(locus, reason)
  private def binding[A](old: CoreBinding[A], next: CoreBinding[A]): Boolean =
    old == next || (old.isInstanceOf[CoreBinding.Unbound[?]] && next
      .isInstanceOf[CoreBinding.Bound[?]])

  /** Only a newly declared Running record can reserve a new execution id;
    * records reopened from another session never become reservations.
    */
  def reserved(
      revision: AnalysisRevision,
      dataset: DatasetRevision,
      claimed: Set[RunId]
  ): Either[BackendError, Option[RunId]] =
    val matches = reservations.toVector
      .flatMap(documentRuns.get)
      .filter(r =>
        r.state == RunLifecycle.Running && r.analysis == revision && r.dataset == dataset && !claimed(
          r.id
        )
      )
      .sortBy(_.id.number)
    if matches.size <= 1 then Right(matches.headOption.map(_.id))
    else
      Left(
        refused(
          DiagnosticLocus.Revision(revision),
          s"${revision.label} on ${dataset.label} has ambiguous requested runs ${matches.map(_.id.label).mkString(", ")}."
        )
      )

  def synchronize(next: StudioDocument): Either[BackendError, RealRegistry.Change] =
    for
      _ <- next.datasets.traverse_ { candidate =>
        protectedDatasets.get(candidate.id).traverse_ { saved =>
          for
            before <- DatasetRevisionSpec
              .contentDigest(saved)
              .leftMap(e => refused(DiagnosticLocus.Dataset(saved.id), e.message))
            after <- DatasetRevisionSpec
              .contentDigest(candidate)
              .leftMap(e => refused(DiagnosticLocus.Dataset(candidate.id), e.message))
            _ <- Either.cond(
              before == after && saved.decision.admittedUnder == candidate.decision.admittedUnder,
              (),
              refused(
                DiagnosticLocus.Dataset(candidate.id),
                s"Admitted ${candidate.id.label} cannot change its content/policy: recorded ${before.display}, requested ${after.display}."
              )
            )
          yield ()
        }
      }
      _ <- next.analyses.traverse_ { candidate =>
        protectedRevisions.get(candidate.id).traverse_ { saved =>
          Either.cond(
            saved.dataset == candidate.dataset && saved.recipe == candidate.recipe && binding(
              saved.plan,
              candidate.plan
            ),
            (),
            refused(
              DiagnosticLocus.Revision(candidate.id),
              s"Saved ${candidate.id.label} cannot change its dataset/recipe/plan binding: recorded ${saved.dataset.label} (${saved.plan.render}), requested ${candidate.dataset.label} (${candidate.plan.render})."
            )
          )
        }
      }
      _ <- next.runs.traverse_ { candidate =>
        protectedRuns.get(candidate.id).traverse_ { saved =>
          Either.cond(
            saved.analysis == candidate.analysis && saved.dataset == candidate.dataset && binding(
              saved.archive,
              candidate.archive
            ),
            (),
            refused(
              DiagnosticLocus.Run(candidate.id),
              s"${candidate.id.label} cannot change its recorded analysis/dataset/archive: ${saved.analysis.label}/${saved.dataset.label} (${saved.archive.render}) -> ${candidate.analysis.label}/${candidate.dataset.label} (${candidate.archive.render})."
            )
          )
        }
      }
      made = new RealRegistry(
        next,
        protectedDatasets ++ next.datasets.filter(_.decision.isAdmitted).map(d => d.id -> d),
        protectedRevisions ++ next.analyses.map(a => a.id -> a),
        protectedRuns ++ next.runs.map(r => r.id -> r),
        reservations.intersect(next.runs.map(_.id).toSet) ++ next.runs
          .filter(r => !protectedRuns.contains(r.id) && r.state == RunLifecycle.Running)
          .map(_.id)
      )
      changedDatasets = (datasets.keySet ++ made.datasets.keySet).filter(id =>
        datasets.get(id) != made.datasets.get(id)
      )
      changedRevisions = (revisions.keySet ++ made.revisions.keySet).filter(id =>
        revisions.get(id) != made.revisions
          .get(id) || made.revisions.get(id).exists((dataset, _) => changedDatasets(dataset)) ||
          planBindings.get(id) != made.planBindings.get(id)
      )
    yield RealRegistry.Change(made, changedDatasets, changedRevisions)

private[real] object RealRegistry:
  final case class Change(
      next: RealRegistry,
      datasets: Set[DatasetRevision],
      revisions: Set[AnalysisRevision]
  )
  def of(document: StudioDocument): RealRegistry = new RealRegistry(
    document,
    document.datasets.filter(_.decision.isAdmitted).map(d => d.id -> d).toMap,
    document.analyses.map(a => a.id -> a).toMap,
    document.runs.map(r => r.id -> r).toMap,
    Set.empty
  )
