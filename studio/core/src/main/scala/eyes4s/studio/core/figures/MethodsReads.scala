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

package eyes4s.studio.core.figures

import cats.Monad
import cats.data.EitherT
import cats.syntax.all.*
import eyes4s.studio.core.backend.*

/** How many queries of a run had each count of something, ascending by the
  * count: `(count, queries)`.
  */
final case class Tally private (entries: Vector[(Int, Int)]) derives CanEqual:
  def queries: Int = entries.map(_._2).sum

  /** The count most queries share (the smaller on a tie). */
  def mode: Option[Int] = entries.maxByOption((count, queries) => (queries, -count)).map(_._1)

object Tally:
  def of(counts: Vector[Int]): Tally =
    Tally(counts.groupMapReduce(identity)(_ => 1)(_ + _).toVector.sortBy(_._1))

/** The run and dataset facts a methods section cites beyond the result
  * summary (ticket S9.4): the dataset's admission, how many controls each
  * eligible query was compared with, and why queries failed, by diagnostic
  * code. Read from the backend; nothing is computed but the tallies.
  */
final case class MethodsFacts(
    run: RunId,
    admission: AdmissionSummary,
    queries: Int,
    controls: Tally,
    failures: Vector[(String, Int)]
) derives CanEqual

/** Why the methods facts could not be read. Every case names its operands. */
enum MethodsReadError derives CanEqual:
  case Admission(dataset: DatasetRevision, error: BackendError)
  case Queries(run: RunId, offset: Int, error: BackendError)
  case Paging(run: RunId, offset: Int, error: PageError)
  case Pairs(run: RunId, scale: Int, offset: Int, error: BackendError)

  def message: String = this match
    case Admission(dataset, error) =>
      s"The admission of dataset ${dataset.label}: ${error.message}"
    case Queries(run, offset, error) =>
      s"The queries of ${run.label} at offset $offset: ${error.message}"
    case Pairs(run, scale, offset, error) =>
      s"The pair rows of ${run.label} at scale $scale, offset $offset: ${error.message}"
    case Paging(run, offset, error) =>
      s"A page of ${run.label} at offset $offset: ${error.message}"

object MethodsReads:

  /** The facts of `run` on `dataset`: its admission summary and every query
    * row, page by page.
    */
  def read[F[_]: Monad](
      admission: DatasetRevision => F[Either[BackendError, AdmissionSummary]],
      queries: (RunId, PageRequest) => F[Either[BackendError, QueryPage]],
      run: RunId,
      dataset: DatasetRevision
  ): F[Either[MethodsReadError, MethodsFacts]] =
    (for
      summary <- EitherT(admission(dataset)).leftMap(MethodsReadError.Admission(dataset, _))
      all     <- EitherT(queryRows(queries, run))
    yield of(run, summary, all)).value

  /** Every query row of `run`, page by page, in the backend's order. */
  def queryRows[F[_]: Monad](
      queries: (RunId, PageRequest) => F[Either[BackendError, QueryPage]],
      run: RunId
  ): F[Either[MethodsReadError, Vector[QueryRow]]] =
    def rows(
        offset: Int,
        got: Vector[QueryRow]
    ): EitherT[F, MethodsReadError, Vector[QueryRow]] =
      for
        request <- EitherT.fromEither[F](
          PageRequest
            .of(offset, PageRequest.MaximumSize)
            .leftMap(MethodsReadError.Paging(run, offset, _))
        )
        page <- EitherT(queries(run, request)).leftMap(MethodsReadError.Queries(run, offset, _))
        all  <- page.page.next
          .filter(_ > offset)
          .fold(EitherT.rightT[F, MethodsReadError](got ++ page.rows))(
            rows(_, got ++ page.rows)
          )
      yield all
    rows(0, Vector.empty).value

  /** The facts from rows already read. Controls are tallied over the queries
    * the run scored or tried to (contributing and failed).
    */
  def of(run: RunId, admission: AdmissionSummary, rows: Vector[QueryRow]): MethodsFacts =
    val compared = rows.filter(r =>
      r.status match
        case QueryStatus.Contributing(_, _, _) | QueryStatus.Failed(_) => true
        case _                                                         => false
    )
    val failures = rows
      .collect { case QueryRow(_, _, _, _, _, QueryStatus.Failed(d)) => d.code }
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .toVector
      .sortBy((code, n) => (-n, code))
    MethodsFacts(run, admission, rows.size, Tally.of(compared.flatMap(_.controls)), failures)

  /** Every pair row of `run` at each of `scales` scale indices, a page per
    * scale in scale order (protocol 1.9).
    */
  def pairRows[F[_]: Monad](
      pairs: (RunId, Int, PageRequest) => F[Either[BackendError, PairRowPage]],
      run: RunId,
      scales: Int
  ): F[Either[MethodsReadError, Vector[PairRowPage]]] =
    def at(
        scale: Int,
        offset: Int,
        got: Vector[PairRowEntry]
    ): EitherT[F, MethodsReadError, PairRowPage] =
      for
        request <- EitherT.fromEither[F](
          PageRequest
            .of(offset, PageRequest.MaximumSize)
            .leftMap(MethodsReadError.Paging(run, offset, _))
        )
        page <- EitherT(pairs(run, scale, request))
          .leftMap(MethodsReadError.Pairs(run, scale, offset, _))
        all <- page.page.next
          .filter(_ > offset)
          .fold(EitherT.rightT[F, MethodsReadError](page.copy(rows = got ++ page.rows)))(
            at(scale, _, got ++ page.rows)
          )
      yield all.copy(page = PageInfo(0, all.page.total, None))
    (0 until scales).toVector.traverse(at(_, 0, Vector.empty)).value
