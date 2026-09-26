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

package eyes4s.studio.core.selection

import cats.effect.Concurrent
import cats.syntax.all.*
import fs2.Stream
import fs2.concurrent.SignallingRef

/** The shared selection bus (ticket S3.3): [[SelectionState]] behind a
  * signalling reference. Every view submits stamped inputs and subscribes to
  * `changes`; a refused input changes nothing and is returned as a value.
  */
final class SelectionBus[F[_]] private (ref: SignallingRef[F, SelectionState]):

  def current: F[SelectionState] = ref.get

  def submit(input: SelectionInput): F[Either[SelectionError, SelectionState]] =
    ref.modify { state =>
      state.submit(input) match
        case Right(next) => (next, Right(next))
        case Left(error) => (state, Left(error))
    }

  /** Move to a new context, keeping only the refs `keep` accepts. */
  def rebase(keep: StudioRef => Boolean): F[SelectionState] =
    ref.modify { state =>
      val next = state.rebase(keep)
      (next, next)
    }

  /** The current state, then every change of the selected refs or context
    * (sequence bookkeeping alone is not a change). A slow subscriber sees
    * the latest state.
    */
  def changes: Stream[F, SelectionState] =
    ref.discrete.changesBy(s => (s.context.value, s.revision))

object SelectionBus:
  def apply[F[_]: Concurrent](
      initial: SelectionState = SelectionState.empty
  ): F[SelectionBus[F]] =
    SignallingRef.of[F, SelectionState](initial).map(new SelectionBus(_))
