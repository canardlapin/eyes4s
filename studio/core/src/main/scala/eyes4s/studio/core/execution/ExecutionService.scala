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

package eyes4s.studio.core.execution

import cats.effect.std.{Mutex, Queue, Supervisor}
import cats.effect.{Concurrent, Ref, Resource}
import cats.syntax.all.*
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.preview.PreviewReady
import fs2.Stream
import fs2.concurrent.Topic

/** Runs of analysis revisions on a [[StudyBackend]] (ticket S3.1): submit,
  * progress, cancel, stale-completion rejection, and the "Run N ready — Show"
  * notice.
  *
  * Every job is watched on its own fiber, never on the UI thread. Its
  * backend events are folded through [[ExecutionTracker]], whose rules this
  * service does not restate: progress is monotone per run, a completion
  * whose stamp is no longer the requested one is `Superseded`, and a
  * `Succeeded` job publishes a [[RunReady]] notice. Nothing here changes the
  * shown run; promotion is the user's Show ([[RunIntent.Show]]).
  *
  * An intent is recorded, under the service's lock, before the backend is
  * asked: a submission whose intent was replaced while the backend answered
  * is `Superseded` from the start and its backend job is cancelled.
  *
  * Events are applied under the lock and queued in order; one publisher
  * fibre delivers them. A subscriber that falls `subscriberBuffer` events
  * behind stalls delivery to every subscriber until it reads again (events
  * wait in memory meanwhile, none is dropped), but never stalls a state
  * change: submit, cancel and job watching go on.
  *
  * Refusals are values. An effect fails only on a defect of the backend
  * itself.
  */
trait ExecutionService[F[_]]:

  /** Submit a run of `stamp.revision`; `stamp` becomes the requested stamp
    * before the backend is asked.
    */
  def submit(stamp: RunStamp): F[Either[ExecutionError, ExecutionJob]]

  /** Submit the exact prepared study named by a ready backend preview. */
  def submitPreview(ready: PreviewReady): F[Either[ExecutionError, ExecutionJob]]

  /** Track a job the backend already runs (a reopened project's
    * `JobHandle`); `stamp` becomes the requested stamp.
    */
  def adopt(job: JobId, stamp: RunStamp): F[Either[ExecutionError, ExecutionJob]]

  /** The document now wants results for `stamp`; jobs of any other stamp
    * that complete from now on are `Superseded`.
    */
  def require(stamp: RunStamp): F[Unit]

  /** Request cancellation. A live job is `Cancelling` at once and `Cancelled`
    * when the backend settles it; a settled job is returned as it is.
    */
  def cancel(job: JobId): F[Either[ExecutionError, ExecutionJob]]

  def jobs: F[Vector[ExecutionJob]]

  def job(id: JobId): F[Either[ExecutionError, ExecutionJob]]

  /** The stamp completions are judged against. */
  def requested: F[Option[RunStamp]]

  /** Every event from acquisition on, in the order the service applied them.
    * The subscription is registered when the resource is acquired, so no
    * event published afterwards is missed.
    */
  def subscribe: Resource[F, Stream[F, ExecutionEvent]]

object ExecutionService:

  /** Events a subscriber may fall behind by before delivery waits for it. */
  val SubscriberBuffer: Int = 4096

  /** The service over `backend`. Releasing it stops watching every job; it
    * does not cancel backend jobs.
    */
  def resource[F[_]](backend: StudyBackend[F], subscriberBuffer: Int = SubscriberBuffer)(using
      F: Concurrent[F]
  ): Resource[F, ExecutionService[F]] =
    for
      supervisor <- Supervisor[F](await = false)
      topic      <- Resource.make(Topic[F, ExecutionEvent])(_.close.void)
      outbox     <- Resource.eval(Queue.unbounded[F, ExecutionEvent])
      _          <- F.background(
        Stream.fromQueueUnterminated(outbox).through(topic.publish).compile.drain
      )
      state <- Resource.eval(Ref.of[F, ExecutionTracker](ExecutionTracker.empty))
      mutex <- Resource.eval(Mutex[F])
    yield new Live(backend, supervisor, topic, outbox, state, mutex, subscriberBuffer max 1)

  private final class Live[F[_]](
      backend: StudyBackend[F],
      supervisor: Supervisor[F],
      topic: Topic[F, ExecutionEvent],
      outbox: Queue[F, ExecutionEvent],
      state: Ref[F, ExecutionTracker],
      mutex: Mutex[F],
      subscriberBuffer: Int
  )(using F: Concurrent[F])
      extends ExecutionService[F]:

    /** Apply one transition and queue its events, both under the mutex, so
      * subscribers see events in the order the state changed. Queueing never
      * waits; delivery happens on the publisher fibre.
      */
    private def step[A](
        transition: ExecutionTracker => Either[ExecutionError, ExecutionTracker.Step[A]]
    ): F[Either[ExecutionError, A]] =
      mutex.lock.surround {
        state.get.flatMap { t =>
          transition(t) match
            case Left(e)                  => F.pure(Left(e))
            case Right((next, events, a)) =>
              state.set(next) >> events.traverse_(outbox.offer) >> F.pure(Right(a))
        }
      }

    /** Record the intent to run `stamp`; its generation. */
    private def intend(stamp: RunStamp): F[Long] =
      mutex.lock.surround(state.modify(_.intend(stamp)))

    def submit(stamp: RunStamp): F[Either[ExecutionError, ExecutionJob]] =
      intend(stamp).flatMap { generation =>
        backend.submit(stamp.revision).flatMap {
          case Left(e)       => F.pure(Left(ExecutionError.Backend(e)))
          case Right(status) => start(status, stamp, generation)
        }
      }

    def submitPreview(ready: PreviewReady): F[Either[ExecutionError, ExecutionJob]] =
      intend(ready.stamp).flatMap { generation =>
        backend.submitPreview(ready).flatMap {
          case Left(e)       => F.pure(Left(ExecutionError.Backend(e)))
          case Right(status) => start(status, ready.stamp, generation)
        }
      }

    def adopt(job: JobId, stamp: RunStamp): F[Either[ExecutionError, ExecutionJob]] =
      state.get.flatMap { t =>
        if t.jobs.exists(_.id == job) then F.pure(Left(ExecutionError.AlreadyTracked(job)))
        else
          backend.job(job).flatMap {
            case Left(e)       => F.pure(Left(ExecutionError.Backend(e)))
            case Right(status) => intend(stamp).flatMap(start(status, stamp, _))
          }
      }

    private def start(
        status: JobStatus,
        stamp: RunStamp,
        generation: Long
    ): F[Either[ExecutionError, ExecutionJob]] =
      if status.revision != stamp.revision || status.dataset != stamp.dataset then
        backend
          .cancel(status.job)
          .as(
            Left(
              ExecutionError.StampMismatch(stamp, status.job, status.revision, status.dataset)
            )
          )
      else
        step(_.track(status, stamp, generation)).flatTap {
          case Right(j) if !j.phase.isTerminal => supervisor.supervise(watch(j.id)).void
          case Right(ExecutionJob(id, _, _, JobPhase.Superseded(_, _))) =>
            backend.cancel(id).void
          case _ => F.unit
        }

    /** Fold the job's backend events until it settles. If the stream ends or
      * breaks first, the backend's outcome settles it, or else it is lost.
      */
    private def watch(id: JobId): F[Unit] =
      def lose(reason: String) =
        step(_.lose(id, ExecutionDiagnostics.lost(id, reason))).void
      val follow = backend.subscribe(id).flatMap {
        case Left(e)       => lose(e.message)
        case Right(events) =>
          events.evalMap(e => step(_.observe(id, e))).compile.drain
      }
      follow.attempt.flatMap { ended =>
        state.get.map(_.job(id).exists(_.phase.isTerminal)).flatMap {
          case true  => F.unit
          case false =>
            backend.outcome(id).flatMap {
              case Right(Some(o)) => step(_.observe(id, JobEvent.Finished(o))).void
              case Right(None)    =>
                lose(
                  ended
                    .fold(e => s"the stream failed (${e.getMessage})", _ => "the stream ended")
                )
              case Left(e) => lose(e.message)
            }
        }
      }

    def require(stamp: RunStamp): F[Unit] =
      mutex.lock.surround(state.update(_.require(stamp)))

    def cancel(id: JobId): F[Either[ExecutionError, ExecutionJob]] =
      step(_.cancelling(id)).flatMap {
        case Right(j) if !j.phase.isTerminal =>
          backend.cancel(id).flatMap {
            case Left(e)  => F.pure(Left(ExecutionError.Backend(e)))
            case Right(_) => job(id)
          }
        case other => F.pure(other)
      }

    def jobs: F[Vector[ExecutionJob]] = state.get.map(_.jobs)

    def job(id: JobId): F[Either[ExecutionError, ExecutionJob]] = state.get.map(_.job(id))

    def requested: F[Option[RunStamp]] = state.get.map(_.requestedStamp)

    def subscribe: Resource[F, Stream[F, ExecutionEvent]] =
      topic.subscribeAwait(subscriberBuffer)
