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

/** A dataset's whole admission ledger, read page by page (ticket S5.6): the
  * admission ledger lists the trials behind each of its counts.
  */
object LedgerPages:

  /** Every entry of the ledger `read` pages through, in inventory order: the
    * largest pages eyes4s serves, until a page has no next. The first
    * refusal is the answer.
    */
  def all[F[_]: Monad](
      read: PageRequest => F[Either[BackendError, LedgerPage]]
  ): F[Either[BackendError, Vector[LedgerEntry]]] =
    Monad[F].tailRecM((0, Vector.empty[LedgerEntry])) { (offset, acc) =>
      PageRequest.of(offset, PageRequest.MaximumSize) match
        // An offset past Int is not a page eyes4s could serve; it ends the read.
        case Left(_)     => Monad[F].pure(Right(Right(acc)))
        case Right(page) =>
          read(page).map {
            case Left(e)  => Right(Left(e))
            case Right(p) =>
              val entries = acc ++ p.entries
              p.page.next.filter(_ > offset) match
                case Some(next) => Left((next, entries))
                case None       => Right(Right(entries))
          }
    }
