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

import cats.effect.kernel.{Concurrent, Ref}
import cats.syntax.all.*
import eyes4s.codec.CanonicalDigest
import eyes4s.design.WorkQuanta
import eyes4s.plan.{
  CountCursor,
  Diagnostic,
  StudyCounts,
  StudyFinding,
  TrialKey as CoreKey,
  UnmatchedReasons,
  WorkStep
}
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.{CoreBinding, StudyPlanArtifact}
import eyes4s.studio.core.execution.{RunStamp, StudyInputArtifact}
import eyes4s.studio.core.preview.*
import fs2.Stream

/** Backend-owned count cursors and exact receipts, never reconstructed from client counts. */
final class RealPreview[F[_]] private (state: Ref[F, RealPreview.State])(using
    F: Concurrent[F]
):
  import RealPreview.*

  def begin(
      configured: RealConfigured,
      budget: PreviewBudget
  ): Stream[F, Either[BackendError, PreviewEvent]] =
    Stream.eval(F.pure(initial(configured))).flatMap {
      case Left(error)                        => Stream.emit(Left(error))
      case Right((stamp, candidates, cursor)) =>
        Stream
          .eval(state.modify { current =>
            val id       = PreviewId(current.nextId)
            val retained = Retained(configured, stamp, candidates, Status.Counting(cursor))
            (
              current.copy(
                nextId = current.nextId + 1,
                previews = current.previews.updated(id, retained),
                owners = current.owners.updated(configured.revision, Owner.Cursor(id)),
                ready = current.ready - configured.revision,
                rows = current.rows - configured.revision
              ),
              id
            )
          })
          .flatMap { id =>
            Stream.emit(Right(PreviewEvent.Initial(id, stamp, candidates))) ++ pages(id, budget)
          }
    }

  def continue(
      id: PreviewId,
      budget: PreviewBudget
  ): Stream[F, Either[BackendError, PreviewEvent]] =
    pages(id, budget)

  private def pages(
      id: PreviewId,
      budget: PreviewBudget
  ): Stream[F, Either[BackendError, PreviewEvent]] =
    Stream
      .range(0, budget.pages)
      .evalMap(_ =>
        state.modify { current =>
          current.previews.get(id) match
            case None           => (current, Vector(Left(unknown(id, current))))
            case Some(retained) =>
              val (next, events) = advance(id, retained)
              val available      = next.status match
                case Status.Ready(_, prepared)
                    if current.owners.get(prepared.revision).contains(Owner.Cursor(id)) =>
                  current.ready.updated(prepared.revision, prepared)
                case _ => current.ready
              (
                current.copy(previews = current.previews.updated(id, next), ready = available),
                events
              )
        }
      )
      .flatMap(Stream.emits)
      .takeThrough {
        case Right(PreviewEvent.Ready(_)) => false
        case Left(_)                      => false
        case _                            => true
      }

  /** Compatibility for an explicitly requested synchronous preview's completed counts. */
  def remember(prepared: RealPrepared): F[Unit] =
    state.update(current =>
      current.copy(
        ready = current.ready.updated(prepared.revision, prepared),
        owners = current.owners.updated(prepared.revision, Owner.Synchronous(prepared)),
        rows = current.rows - prepared.revision
      )
    )

  /** Current draft views expire; older cursor/receipt snapshots remain immutable. */
  private[real] def invalidate(revisions: Set[AnalysisRevision]): F[Unit] =
    state.update(current =>
      current.copy(
        ready = current.ready -- revisions,
        rows = current.rows -- revisions,
        owners = current.owners -- revisions
      )
    )

  /** Page only completed native metadata; asking for rows never starts counting. */
  def rows(
      revision: AnalysisRevision,
      request: PageRequest
  ): F[Either[BackendError, PreviewPage]] =
    state.get.flatMap { current =>
      current.ready.get(revision) match
        case None =>
          F.pure(
            Left(
              BackendError.Unavailable(
                DiagnosticLocus.Artifact(
                  s"${revision.label} preview rows require a completed preview."
                )
              )
            )
          )
        case Some(prepared) =>
          current.rows.get(revision) match
            case Some(rows) => F.pure(Right(page(revision, rows, request)))
            case None       =>
              val owner = current.owners.get(revision)
              F.pure(allRows(prepared)).flatMap {
                case Left(error) => F.pure(Left(error))
                case Right(rows) =>
                  state.modify { live =>
                    if live.owners.get(revision) == owner && live.ready
                        .get(revision)
                        .exists(_ eq prepared)
                    then
                      (
                        live.copy(rows = live.rows.updated(revision, rows)),
                        Right(page(revision, rows, request))
                      )
                    else
                      (
                        live,
                        Left(
                          BackendError.Unavailable(
                            DiagnosticLocus.Artifact(
                              s"${revision.label} preview rows changed while the page was being read."
                            )
                          )
                        )
                      )
                  }
              }
    }

  /** Validate the whole receipt before checking the current native identity. */
  def accept(ready: PreviewReady)(
      currentStamp: AnalysisRevision => F[Either[BackendError, RunStamp]]
  ): F[Either[BackendError, RealPrepared]] =
    state.get.flatMap { current =>
      val retained =
        current.previews.get(ready.id).toRight(unknown(ready.id, current)).flatMap { retained =>
          retained.status match
            case Status.Refused(error)   => Left(error)
            case Status.Counting(cursor) =>
              Left(
                BackendError
                  .PreviewWorkNotReady(ready.id, PairDesign.of(cursor.stage), cursor.visited)
              )
            case Status.Ready(expected, _) if ready != expected =>
              Left(BackendError.TamperedPreview(ready, expected))
            case Status.Ready(expected, prepared) => Right(expected -> prepared)
        }
      retained match
        case Left(error)                 => F.pure(Left(error))
        case Right((expected, prepared)) =>
          currentStamp(expected.stamp.revision).map(_.flatMap { identity =>
            if expected.stamp != identity then
              Left(BackendError.StalePreview(ready.id, expected.stamp, identity))
            else
              prepared.pairingRefusal
                .fold[Either[BackendError, RealPrepared]](Right(prepared)) { error =>
                  Left(RealPrepared.refused(prepared.revision, "preview submission")(error))
                }
          })
    }

object RealPreview:
  private enum Owner:
    case Cursor(id: PreviewId)
    case Synchronous(prepared: RealPrepared)
  private enum Status:
    case Counting(cursor: CountCursor[CoreKey])
    case Ready(receipt: PreviewReady, prepared: RealPrepared)
    case Refused(error: BackendError)

  private final case class Retained(
      configured: RealConfigured,
      stamp: RunStamp,
      candidates: PreviewCandidates,
      status: Status
  )
  private final case class State(
      nextId: Long,
      previews: Map[PreviewId, Retained],
      ready: Map[AnalysisRevision, RealPrepared],
      rows: Map[AnalysisRevision, Vector[PreviewRow]],
      owners: Map[AnalysisRevision, Owner]
  )

  def create[F[_]: Concurrent]: F[RealPreview[F]] =
    Ref
      .of[F, State](State(1L, Map.empty, Map.empty, Map.empty, Map.empty))
      .map(new RealPreview(_))

  private def unknown(id: PreviewId, state: State): BackendError =
    BackendError.UnknownPreview(id, state.previews.keys.toVector.sortBy(_.value))

  private def refused(configured: RealConfigured, step: String)(message: String): BackendError =
    BackendError.Unavailable(
      DiagnosticLocus.Artifact(s"${configured.revision.label} $step: $message")
    )

  /** Canonical library codec digests bind the prepared plan and admitted input. */
  def stamp(configured: RealConfigured): Either[BackendError, RunStamp] =
    for
      plan <- configured.plans.codec
        .digest(configured.plan)
        .leftMap(e => refused(configured, "plan digest")(e.message))
      input <- configured.inputs.input
        .digest(configured.admitted.input)
        .leftMap(e => refused(configured, "input digest")(e.message))
      planBinding <- CanonicalDigest
        .parse[StudyPlanArtifact](plan.sha256.hex)
        .leftMap(e => refused(configured, "plan binding")(e.message))
      inputBinding <- CanonicalDigest
        .parse[StudyInputArtifact](input.sha256.hex)
        .leftMap(e => refused(configured, "input binding")(e.message))
    yield RunStamp(
      configured.revision,
      configured.dataset,
      CoreBinding.Bound(planBinding),
      CoreBinding.Bound(inputBinding)
    )

  private def reasons(
      configured: RealConfigured,
      keys: Vector[CoreKey]
  ): Either[BackendError, UnmatchedReasons[CoreKey]] =
    configured.admitted.evidence.inventory
      .toRight(refused(configured, "inventory")("No admitted trial inventory is retained."))
      .map { inventory =>
        UnmatchedReasons.of(
          configured.plan.layout,
          configured.plan.pairing,
          configured.plan.referencePhase,
          keys,
          inventory
        )
      }

  private def initial(
      configured: RealConfigured
  ): Either[BackendError, (RunStamp, PreviewCandidates, CountCursor[CoreKey])] =
    val requested =
      configured.admitted.ledger.filter(_.trial.phase == configured.recipe.phases.focal)
    for
      identity   <- stamp(configured)
      candidates <- PreviewCandidates
        .of(
          configured.preview.focalKeys.size,
          configured.preview.referenceKeys.size,
          requested.map(_.trial.participant).distinct.size,
          configured.preview.matched.candidatePairCount,
          requested.size,
          requested.count(_.disposition != TrialDisposition.Admitted),
          None
        )
        .leftMap(e => refused(configured, "preview candidates")(e.message))
      cursor <- configured.work.countWork.leftMap(
        RealPrepared.refused(configured.revision, "count cursor")
      )
    yield (identity, candidates, cursor)

  /** The pairing findings eyes4s's study preflight reports, keyed by trial
    * (`Preflight`): item conflicts, queries with more than one matched
    * reference and reference groups the pairing rule cannot reduce. The
    * plan's own refusal names the same trials only by key digest, which
    * Studio cannot open, so these keyed findings stand in for it.
    */
  private def cardinality(
      counts: StudyCounts[CoreKey]
  ): Vector[StudyFinding[CoreKey, eyes4s.kernel.Unit2D.Px]] =
    val c      = counts.cardinality
    val policy = c.pairing.matched
    c.itemConflicts.map(StudyFinding.MatchItemConflict(_)) ++
      c.multiple.map((key, references) =>
        StudyFinding.MatchedCardinality(key, references, policy)
      ) ++
      c.blockingReferences.map(StudyFinding.AmbiguousReferences(_, policy))

  private def complete(
      id: PreviewId,
      retained: Retained,
      counts: StudyCounts[CoreKey]
  ): Either[BackendError, (PreviewReady, RealPrepared)] =
    val configured = retained.configured
    for
      prepared  <- RealPrepared.fromCounts(configured, counts)
      unmatched <- reasons(configured, counts.cardinality.unmatched)
      exact     <- PreviewCounts
        .of(
          counts.pairRowsPerScale,
          counts.totalPairs,
          counts.eligibleQueries.toInt,
          unmatched.reasons.size,
          counts.cardinality.multiple.size + counts.cardinality.ambiguousReferences.size
        )
        .leftMap(e => refused(configured, "preview counts")(e.message))
      diagnostics = unmatched.reasons.map { (key, reason) =>
        RealResults.diagnostic(
          Diagnostic.of(
            StudyFinding.UnmatchedFocal[CoreKey, eyes4s.kernel.Unit2D.Px](key, reason)
          )
        )
      } ++ cardinality(counts).map(finding => RealResults.diagnostic(Diagnostic.of(finding))) ++
        prepared.pairingRefusal.toVector.collect {
          case error: eyes4s.plan.PlanError.UnmatchedFocalRefused =>
            RealResults.diagnostic(Diagnostic.of(error))
        }
      receipt <- PreviewReady
        .of(
          id,
          retained.stamp,
          retained.candidates,
          exact,
          diagnostics,
          Some(configured.recipe)
        )
        .leftMap(e => refused(configured, "preview receipt")(e.message))
    yield (receipt, prepared)

  private def advance(
      id: PreviewId,
      retained: Retained
  ): (Retained, Vector[Either[BackendError, PreviewEvent]]) =
    retained.status match
      case Status.Ready(receipt, _) => (retained, Vector(Right(PreviewEvent.Ready(receipt))))
      case Status.Refused(error)    => (retained, Vector(Left(error)))
      case Status.Counting(cursor)  =>
        cursor
          .advance(WorkQuanta.default)
          .leftMap(RealPrepared.refused(retained.configured.revision, "count page")) match
          case Left(error) =>
            (retained.copy(status = Status.Refused(error)), Vector(Left(error)))
          case Right(WorkStep.More(design, units, next)) =>
            val event =
              PreviewEvent.CountingWork(id, PairDesign.of(design), units, next.visited)
            (retained.copy(status = Status.Counting(next)), Vector(Right(event)))
          case Right(WorkStep.Done(units, counts)) =>
            // Done only comes from the control PairCursor page; cardinality
            // diagnostics return More without adding to cursor.visited.
            val event = PreviewEvent.CountingWork(
              id,
              PairDesign.of(cursor.stage),
              units,
              cursor.visited + units
            )
            complete(id, retained, counts) match
              case Left(error) =>
                (
                  retained.copy(status = Status.Refused(error)),
                  Vector(Right(event), Left(error))
                )
              case Right((receipt, prepared)) =>
                (
                  retained.copy(status = Status.Ready(receipt, prepared)),
                  Vector(Right(event), Right(PreviewEvent.Ready(receipt)))
                )

  /** Each requested inventory query remains present, including admission refusals. */
  private def page(
      revision: AnalysisRevision,
      rows: Vector[PreviewRow],
      request: PageRequest
  ): PreviewPage =
    val selected = rows.slice(request.offset, request.offset + request.size)
    PreviewPage(revision, PageInfo.of(request, rows.size, selected.size), selected)

  private def allRows(prepared: RealPrepared): Either[BackendError, Vector[PreviewRow]] =
    val requested =
      prepared.admitted.ledger.filter(_.trial.phase == prepared.recipe.phases.focal)
    val keys = prepared.preview.focalKeys.map(key => RealResults.key(key) -> key).toMap
    for
      inventory <- prepared.admitted.evidence.inventory.toRight(
        BackendError.Unavailable(
          DiagnosticLocus.Artifact(
            s"${prepared.revision.label} has no retained trial inventory."
          )
        )
      )
      unmatched = UnmatchedReasons.of(
        prepared.plan.layout,
        prepared.plan.pairing,
        prepared.plan.referencePhase,
        prepared.counts.cardinality.unmatched,
        inventory
      )
      rows <- requested.traverse { entry =>
        val membership: Either[BackendError, (Option[TrialKey], Option[Int], Eligibility)] =
          entry.disposition match
            case TrialDisposition.Admitted =>
              for
                key <- keys
                  .get(entry.trial)
                  .toRight(
                    BackendError.Unavailable(
                      DiagnosticLocus.Artifact(
                        s"${entry.trial.label} is absent from the retained focal keys."
                      )
                    )
                  )
                counts <- prepared.counts.queryPairs
                  .get(key)
                  .toRight(
                    BackendError.Unavailable(
                      DiagnosticLocus
                        .Artifact(s"${entry.trial.label} has no retained native pair counts.")
                    )
                  )
                row <-
                  if counts.matchedPairs > 1 then
                    Left(
                      BackendError.Unavailable(
                        DiagnosticLocus.Artifact(
                          s"${entry.trial.label} has ${counts.matchedPairs} matched references; a single-reference preview row cannot represent them."
                        )
                      )
                    )
                  else
                    counts.singleMatchedReference match
                      case Some(reference) =>
                        Right(
                          (
                            Some(RealResults.key(reference)),
                            Some(counts.controlPairs),
                            Eligibility.Eligible
                          )
                        )
                      case None =>
                        unmatched
                          .reason(key)
                          .toRight(
                            BackendError.Unavailable(
                              DiagnosticLocus.Artifact(
                                s"${entry.trial.label} has no retained unmatched reason."
                              )
                            )
                          )
                          .map(reason =>
                            (
                              None,
                              None,
                              Eligibility.NoMatch(
                                RealResults.diagnostic(
                                  Diagnostic.of(
                                    StudyFinding
                                      .UnmatchedFocal[CoreKey, eyes4s.kernel.Unit2D.Px](
                                        key,
                                        reason
                                      )
                                  )
                                )
                              )
                            )
                          )
              yield row
            case disposition => Right((None, None, Eligibility.QueryNotAdmitted(disposition)))
        for
          (reference, controls, eligibility) <- membership
          response                           <- entry.response.toRight(
            BackendError.Unavailable(
              DiagnosticLocus.Artifact(s"${entry.trial.label} response is not declared.")
            )
          )
        yield PreviewRow(entry.trial, entry.item, response, reference, controls, eligibility)
      }
    yield rows
