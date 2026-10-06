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

package eyes4s.studio.core.headless

import cats.effect.kernel.{Concurrent, Resource}
import cats.syntax.all.*
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.{ReportingId, ReportingSpec}
import eyes4s.studio.core.navigation.{
  NavigationError,
  Page,
  PairMaps,
  ReportRef,
  StudyNavigator,
  UsedBy,
  UsedByRole
}
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}
import fs2.concurrent.SignallingRef
import fs2.Stream

/** Local host reads may wait for actual recomputation. The backend and IPC
  * remain nonblocking, so job cancellation requests can always be served.
  */
final class NativeReads[F[_]] private (
    backend: StudyBackend[F],
    rawNavigator: StudyNavigator[F],
    closed: SignallingRef[F, Boolean],
    synchronize: () => F[Either[BackendError, Unit]],
    awaitRestore: Option[RunId => F[Either[BackendError, Unit]]]
)(using F: Concurrent[F]):

  private def waitFor(run: RunId, job: JobId, own: Boolean): F[Either[BackendError, Unit]] =
    backend.subscribe(job).flatMap {
      case Left(error)   => F.pure(Left(error))
      case Right(events) =>
        events
          .collect { case JobEvent.Finished(outcome) => outcome }
          .take(1)
          .interruptWhen(closed)
          .compile
          .last
          .flatMap {
            case None =>
              closed.get.map {
                case true  => Left(BackendError.ResultReadClosed(run, Some(job)))
                case false =>
                  Left(
                    BackendError.Unavailable(
                      DiagnosticLocus.Artifact(
                        s"Job ${job.number} ended its result subscription for ${run.label} without a terminal outcome."
                      )
                    )
                  )
              }
            case Some(outcome) if outcome.job != job || (own && outcome.run != run) =>
              F.pure(
                Left(
                  BackendError.Unavailable(
                    DiagnosticLocus.Artifact(
                      s"Expected ${run.label}/job ${job.number}, received ${outcome.run.label}/job ${outcome.job.number}."
                    )
                  )
                )
              )
            case Some(JobOutcome.Completed(_, _, _))        => F.pure(Right(()))
            case Some(JobOutcome.Cancelled(_, _, _)) if own =>
              F.pure(Left(BackendError.ResultRecomputationCancelled(run, job)))
            case Some(JobOutcome.Failed(_, _, diagnostics, _)) if own =>
              F.pure(Left(BackendError.ResultRecomputationFailed(run, job, diagnostics)))
            case Some(_) =>
              F.pure(Right(())) // An unrelated failed/cancelled job releases the slot.
          }
    }

  /** Retry only the backend's typed pending/deferred result states. */
  def read[A](run: RunId)(request: => F[Either[BackendError, A]]): F[Either[BackendError, A]] =
    closed.get.flatMap {
      case true  => F.pure(Left(BackendError.ResultReadClosed(run, None)))
      case false =>
        request.flatMap {
          case Left(BackendError.ResultRestoring(`run`)) =>
            waitForRestore(run).flatMap {
              case Left(error) => F.pure(Left(error))
              case Right(_)    => read(run)(request)
            }
          case Left(BackendError.ResultPending(`run`, job)) =>
            waitFor(run, job, own = true).flatMap {
              case Left(error) => F.pure(Left(error))
              case Right(_)    => read(run)(request)
            }
          case Left(BackendError.ResultDeferred(`run`, job)) =>
            waitFor(run, job, own = false).flatMap {
              case Left(error) => F.pure(Left(error))
              case Right(_)    => read(run)(request)
            }
          case result => F.pure(result)
        }
    }

  private def waitForRestore(run: RunId): F[Either[BackendError, Unit]] =
    awaitRestore match
      case None        => F.pure(Left(BackendError.ResultRestoring(run)))
      case Some(await) =>
        Stream
          .eval(await(run))
          .interruptWhen(closed)
          .compile
          .last
          .map(
            _.getOrElse(Left(BackendError.ResultReadClosed(run, None)))
          )

  private def navigation[A](
      request: => F[Either[NavigationError, A]]
  ): F[Either[NavigationError, A]] =
    closed.get.flatMap {
      case true =>
        F.pure(
          Left(
            NavigationError.Backend(
              BackendError.Unavailable(
                DiagnosticLocus.Artifact("The local navigation read is closed.")
              )
            )
          )
        )
      case false =>
        synchronize().flatMap {
          case Left(error) => F.pure(Left(NavigationError.Backend(error)))
          case Right(_)    => navigationStep(request)
        }
    }

  private def navigationStep[A](
      request: => F[Either[NavigationError, A]]
  ): F[Either[NavigationError, A]] =
    request.flatMap {
      case Left(NavigationError.Backend(BackendError.ResultRestoring(run))) =>
        waitForRestore(run).flatMap {
          case Left(error) => F.pure(Left(NavigationError.Backend(error)))
          case Right(_)    => navigation(request)
        }
      case Left(NavigationError.Backend(BackendError.ResultPending(run, job))) =>
        waitFor(run, job, own = true).flatMap {
          case Left(error) => F.pure(Left(NavigationError.Backend(error)))
          case Right(_)    =>
            closed.get.flatMap {
              case true =>
                F.pure(
                  Left(NavigationError.Backend(BackendError.ResultReadClosed(run, Some(job))))
                )
              case false => navigation(request)
            }
        }
      case Left(NavigationError.Backend(BackendError.ResultDeferred(run, job))) =>
        waitFor(run, job, own = false).flatMap {
          case Left(error) => F.pure(Left(NavigationError.Backend(error)))
          case Right(_)    =>
            closed.get.flatMap {
              case true =>
                F.pure(
                  Left(NavigationError.Backend(BackendError.ResultReadClosed(run, Some(job))))
                )
              case false => navigation(request)
            }
        }
      case result => F.pure(result)
    }

  def result(run: RunId)                     = read(run)(backend.result(run))
  def queries(run: RunId, page: PageRequest) = read(run)(backend.queries(run, page))
  def report(run: RunId, spec: ReportingSpec, scale: Int) =
    read(run)(backend.report(run, spec, scale))
  def inspect(run: RunId, address: ResultAddress)    = read(run)(backend.inspect(run, address))
  def provenance(run: RunId, address: ResultAddress) =
    read(run)(backend.provenance(run, address))
  def mapGrid(run: RunId, scale: Int, trial: TrialKey) =
    read(run)(backend.mapGrid(run, scale, trial))
  def pairRows(run: RunId, scale: Int, page: PageRequest) =
    read(run)(backend.pairRows(run, scale, page))

  val navigator: StudyNavigator[F] = new StudyNavigator[F]:
    def cells(
        run: RunId,
        reporting: ReportingId,
        scale: ScaleIndex,
        page: PageRequest
    ): F[Either[NavigationError, Page[ReportRef.Cell]]] =
      navigation(rawNavigator.cells(run, reporting, scale, page))
    def participants(
        cell: ReportRef.Cell,
        page: PageRequest
    ): F[Either[NavigationError, Page[ReportRef.Participant]]] =
      navigation(rawNavigator.participants(cell, page))
    def queries(
        participant: ReportRef.Participant,
        page: PageRequest
    ): F[Either[NavigationError, Page[StudioRef]]] =
      navigation(rawNavigator.queries(participant, page))
    def pairs(
        contrast: StudioRef,
        design: PairDesign,
        page: PageRequest
    ): F[Either[NavigationError, Page[StudioRef]]] =
      navigation(rawNavigator.pairs(contrast, design, page))
    def maps(pair: StudioRef): F[Either[NavigationError, PairMaps]] = navigation(
      rawNavigator.maps(pair)
    )
    def fixations(
        map: StudioRef,
        page: PageRequest
    ): F[Either[NavigationError, Page[StudioRef]]] = navigation(
      rawNavigator.fixations(map, page)
    )
    def record(fixation: StudioRef): F[Either[NavigationError, StudioRef]] = navigation(
      rawNavigator.record(fixation)
    )
    def usedByCounts(map: StudioRef): F[Either[NavigationError, UsedBy]] = navigation(
      rawNavigator.usedByCounts(map)
    )
    def usedBy(
        map: StudioRef,
        role: UsedByRole,
        page: PageRequest
    ): F[Either[NavigationError, Page[StudioRef]]] =
      navigation(rawNavigator.usedBy(map, role, page))

object NativeReads:
  def resource[F[_]: Concurrent](
      backend: StudyBackend[F],
      navigator: StudyNavigator[F]
  ): Resource[F, NativeReads[F]] =
    resource(backend, navigator, () => Concurrent[F].pure(Right(())))

  def resource[F[_]: Concurrent](
      backend: StudyBackend[F],
      navigator: StudyNavigator[F],
      synchronize: () => F[Either[BackendError, Unit]],
      awaitRestore: Option[RunId => F[Either[BackendError, Unit]]] = None
  ): Resource[F, NativeReads[F]] =
    Resource
      .make(SignallingRef[F].of(false))(_.set(true))
      .map(closed => new NativeReads(backend, navigator, closed, synchronize, awaitRestore))
