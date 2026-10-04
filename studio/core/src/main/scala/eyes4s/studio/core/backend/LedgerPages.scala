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

package eyes4s.studio.core.backend

import cats.Monad
import cats.syntax.all.*

/** Why a dataset's whole admission ledger could not be read. */
enum LedgerReadError derives CanEqual:
  /** The backend refused a page. */
  case Refused(error: BackendError)

  /** The page at `offset` cannot be asked for. */
  case Unpageable(offset: Int, error: PageError)

  /** The page of `dataset`'s ledger at `offset` names a next page at `next`,
    * which does not advance: reading on would never end.
    */
  case Stalled(dataset: DatasetRevision, offset: Int, next: Int)

  def message: String = this match
    case Refused(error)            => error.message
    case Unpageable(offset, error) =>
      s"The ledger page at offset $offset cannot be asked for: ${error.message}"
    case Stalled(dataset, offset, next) =>
      s"The ledger of ${dataset.label} does not advance: the page at offset $offset names " +
        s"the next page at offset $next."

/** A dataset's whole admission ledger, read page by page (ticket S5.6): the
  * admission ledger lists the trials behind each of its counts.
  */
object LedgerPages:

  /** Every entry of the ledger `read` pages through, in inventory order: the
    * largest pages eyes4s serves, until a page has no next. The first
    * refusal is the answer, and so is a page that cannot be asked for or a
    * next page that does not advance: a partial ledger is never the answer.
    */
  def all[F[_]: Monad](
      read: PageRequest => F[Either[BackendError, LedgerPage]]
  ): F[Either[LedgerReadError, Vector[LedgerEntry]]] =
    Monad[F].tailRecM((0, Vector.empty[LedgerEntry])) { (offset, acc) =>
      PageRequest.of(offset, PageRequest.MaximumSize) match
        case Left(e)     => Monad[F].pure(Right(Left(LedgerReadError.Unpageable(offset, e))))
        case Right(page) =>
          read(page).map {
            case Left(e)  => Right(Left(LedgerReadError.Refused(e)))
            case Right(p) =>
              val entries = acc ++ p.entries
              p.page.next match
                case Some(next) if next > offset => Left((next, entries))
                case Some(next)                  =>
                  Right(Left(LedgerReadError.Stalled(p.dataset, offset, next)))
                case None => Right(Right(entries))
          }
    }
