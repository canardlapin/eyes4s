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

import cats.Functor
import cats.syntax.functor.*
import eyes4s.studio.core.backend.{JobId, RunId}
import io.circe.Codec

/** The user's run actions. The S2.2 reducer handles them: `Run` and `Cancel`
  * become [[ExecutionEffect]]s, `Show` and `Dismiss` act on the [[RunShelf]].
  */
enum RunIntent derives CanEqual, Codec.AsObject:
  /** Save & run: submit a run of the stamp's revision. */
  case Run(stamp: RunStamp)
  case Cancel(job: JobId)

  /** Promote a ready run to the shown run: the only way it changes. */
  case Show(run: RunId)

  /** Put a ready notice away without showing its run. */
  case Dismiss(run: RunId)

/** Work a pure update asks the runtime to do, as data. */
enum ExecutionEffect derives CanEqual, Codec.AsObject:
  case Submit(stamp: RunStamp)
  case Cancel(job: JobId)

  /** The document's requested stamp changed without a submission. */
  case Require(stamp: RunStamp)

object ExecutionEffect:
  /** Perform one effect on `service`. */
  def perform[F[_]: Functor](service: ExecutionService[F])(
      effect: ExecutionEffect
  ): F[Either[ExecutionError, Unit]] = effect match
    case Submit(stamp)  => service.submit(stamp).map(_.void)
    case Cancel(job)    => service.cancel(job).map(_.void)
    case Require(stamp) => service.require(stamp).map(Right(_))

/** The shown run and the ready notice beside it ("Run 8 ready — Show"), as a
  * pure value. A completion only ever fills `pending`; `shown` changes only
  * through [[show]], and only to the pending run (S8.8, E2E-08). A notice
  * whose stamp is no longer requested is withdrawn, so a run the user did
  * not ask for is never offered.
  */
final case class RunShelf private (shown: Option[RunId], pending: Option[RunReady])
    derives CanEqual:

  def receive(event: ExecutionEvent): RunShelf = event match
    case ExecutionEvent.Ready(notice) => copy(pending = Some(notice))
    case ExecutionEvent.Changed(_)    => this

  /** The requested stamp is now `stamp`. */
  def require(stamp: RunStamp): RunShelf = copy(pending = pending.filter(_.stamp == stamp))

  def show(run: RunId): Either[ExecutionError, RunShelf] = pending match
    case Some(notice) if notice.run == run => Right(RunShelf(Some(run), None))
    case _ => Left(ExecutionError.NotReady(run, pending.map(_.run)))

  def dismiss(run: RunId): RunShelf = copy(pending = pending.filterNot(_.run == run))

object RunShelf:
  /** The shelf of a document showing `shown`, with nothing ready. */
  def of(shown: Option[RunId]): RunShelf = RunShelf(shown, None)
