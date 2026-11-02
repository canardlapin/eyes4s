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

package eyes4s.studio.desktop.project

import cats.data.EitherT
import cats.effect.{IO, Resource}
import cats.effect.unsafe.IORuntime
import cats.syntax.all.*
import eyes4s.studio.core.artifacts.NativeArtifactBudget
import eyes4s.studio.core.backend.{BackendError, JobId, JobOutcome}
import eyes4s.studio.core.command.{Command, JournalEntry}
import eyes4s.studio.core.document.{CoreBinding, PresentationState, RunRef, StudioDocument}
import eyes4s.studio.core.real.RealStudyBackend
import eyes4s.studio.core.session.ProjectSession
import eyes4s.studio.desktop.runtime.{DatasetSourceHosts, SessionPort}

/** Prepares the copied bundled example's shown result before its window owns
  * the target session. The temporary port owns only its serial transport;
  * the target session is retained only after successful preparation.
  */
object ExamplePreparation:

  private def message(error: Throwable): String =
    Option(error.getMessage).filter(_.nonEmpty).getOrElse(error.getClass.getName)

  private def temporary(document: StudioDocument, run: RunRef): Either[String, StudioDocument] =
    StudioDocument
      .of(
        document.datasets,
        document.analyses,
        document.draft,
        document.runs.filterNot(_.id == run.id),
        document.reporting,
        document.figures.filterNot(_.run == run.id),
        PresentationState.default,
        Vector.empty,
        document.analysisFamilies
      )
      .left
      .map(_.toString)

  private def terminal(backend: RealStudyBackend[IO], job: JobId): IO[Either[String, Unit]] =
    backend.subscribe(job).flatMap {
      case Left(error)   => IO.pure(Left(error.message))
      case Right(events) =>
        events.compile.drain *> backend.outcome(job).map {
          case Right(Some(_: JobOutcome.Completed))                 => Right(())
          case Right(Some(JobOutcome.Failed(_, _, diagnostics, _))) =>
            Left(s"Example analysis failed: ${diagnostics.map(_.message).mkString("; ")}")
          case Right(_) =>
            Left(s"Example analysis job ${job.number} did not complete successfully.")
          case Left(error) => Left(error.message)
        }
    }

  private def compute(
      session: ProjectSession[IO],
      port: SessionPort,
      document: StudioDocument,
      run: RunRef
  ): IO[Either[String, Unit]] =
    temporary(document, run).fold(
      reason => IO.pure(Left(reason)),
      temporaryDocument =>
        RealStudyBackend
          .resource[IO](temporaryDocument, DatasetSourceHosts.stored(port))
          .use { backend =>
            (for
              status <- EitherT(backend.submit(run.analysis).map(_.leftMap(_.message)))
              _      <- EitherT.cond[IO](
                status.run == run.id,
                (),
                s"example analysis made ${status.run.label}, expected ${run.id.label}"
              )
              _       <- EitherT(terminal(backend, status.job))
              archive <- EitherT(
                backend
                  .nativeArtifacts(run.id, NativeArtifactBudget.Default)
                  .map(_.leftMap(_.message))
              )
              facts <- EitherT(session.storeNativeArtifacts(archive).map(_.leftMap(_.message)))
              _     <- EitherT(
                session
                  .perform(JournalEntry.Apply(Command.BindCompletedArtifacts(facts)))
                  .map(_.leftMap(_.message))
              ).void
              _ <- EitherT(session.save.map(_.leftMap(_.message))).void
            yield ()).value
          }
    )

  private def verify(
      port: SessionPort,
      document: StudioDocument,
      run: RunRef
  ): IO[Either[String, Unit]] =
    RealStudyBackend
      .resource[IO](document, DatasetSourceHosts.stored(port), Some(port.nativeArtifactSource))
      .use { backend =>
        backend.result(run.id).flatMap {
          case Right(_)                              => IO.pure(Right(()))
          case Left(BackendError.ResultRestoring(_)) =>
            backend.awaitRestore(run.id).flatMap {
              case Left(error) => IO.pure(Left(error.message))
              case Right(_)    => backend.result(run.id).map(_.leftMap(_.message).void)
            }
          case Left(error) => IO.pure(Left(error.message))
        }
      }

  def prepare(session: ProjectSession[IO])(using IORuntime): IO[Either[String, Unit]] =
    val port = Resource.make(IO.blocking(SessionPort.start(session)))(value =>
      IO.blocking(value.close())
    )
    val action = port.use { temporaryPort =>
      session.document.flatMap { document =>
        document.presentation.shownRun.flatMap(document.run) match
          case None      => IO.pure(Left("The copied example has no shown run."))
          case Some(run) =>
            run.archive match
              case CoreBinding.Unbound() => compute(session, temporaryPort, document, run)
              case CoreBinding.Bound(_)  => verify(temporaryPort, document, run)
      }
    }
    action.attempt
      .flatMap {
        case Right(Right(done)) => IO.pure(Right(done))
        case Right(Left(error)) => session.close.void.as(Left(error))
        case Left(error)        => session.close.void.as(Left(message(error)))
      }
      .onCancel(session.close.void)
