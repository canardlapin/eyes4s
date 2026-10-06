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

  /** The backend answered a request for `run` at `scale` with another page. */
  case OtherPairPage(run: RunId, scale: Int, foundRun: RunId, foundScale: Int)

  /** A page's `next` did not advance past its offset. */
  case PairsStalled(run: RunId, scale: Int, offset: Int, next: Int)

  /** The pages held `rows` rows of the `total` they announced. */
  case PairsShort(run: RunId, scale: Int, rows: Int, total: Int)

  def message: String = this match
    case Admission(dataset, error) =>
      s"The admission of dataset ${dataset.label}: ${error.message}"
    case Queries(run, offset, error) =>
      s"The queries of ${run.label} at offset $offset: ${error.message}"
    case Pairs(run, scale, offset, error) =>
      s"The pair rows of ${run.label} at scale $scale, offset $offset: ${error.message}"
    case OtherPairPage(run, scale, foundRun, foundScale) =>
      s"The pair rows of ${run.label} at scale $scale were answered with ${foundRun.label} " +
        s"at scale $foundScale."
    case PairsStalled(run, scale, offset, next) =>
      s"The pair rows of ${run.label} at scale $scale stop advancing: the page at $offset " +
        s"names $next as next."
    case PairsShort(run, scale, rows, total) =>
      s"The pair rows of ${run.label} at scale $scale hold $rows of the $total announced."
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
        case QueryStatus.Contributing(_, _, _) => true
        case failure if failure.isFailed       => true
        case _                                 => false
    )
    // Frequency is per query and code; retain every distinct scale's code
    // without counting the same query repeatedly for one shared code.
    val failures = rows
      .flatMap(row =>
        row.status match
          case QueryStatus.Failed(d)                   => Vector(d.code)
          case QueryStatus.FailedAtScales(diagnostics) => diagnostics.map(_.code).distinct
          case _                                       => Vector.empty
      )
      .groupMapReduce(identity)(_ => 1)(_ + _)
      .toVector
      .sortBy((code, n) => (-n, code))
    MethodsFacts(run, admission, rows.size, Tally.of(compared.flatMap(_.controls)), failures)

  /** Every page of `run`'s pair rows at each of `scales` scale indices, in
    * scale and page order, as the backend served them (protocol 1.9). A read
    * that would drop rows is refused: a page of another run or scale, a
    * `next` that does not advance, or rows that do not add up to the page's
    * `total`.
    */
  def pairRows[F[_]: Monad](
      pairs: (RunId, Int, PageRequest) => F[Either[BackendError, PairRowPage]],
      run: RunId,
      scales: Int
  ): F[Either[MethodsReadError, Vector[PairRowPage]]] =
    (0 until scales).toVector.flatTraverse(s => EitherT(pairRowsAt(pairs, run, s))).value

  /** Every page of `run`'s pair rows at scale index `scale`, refused as
    * [[pairRows]] refuses (Compare's pairs table, S8.5).
    */
  def pairRowsAt[F[_]: Monad](
      pairs: (RunId, Int, PageRequest) => F[Either[BackendError, PairRowPage]],
      run: RunId,
      scale: Int
  ): F[Either[MethodsReadError, Vector[PairRowPage]]] =
    def at(
        offset: Int,
        got: Vector[PairRowPage]
    ): EitherT[F, MethodsReadError, Vector[PairRowPage]] =
      for
        request <- EitherT.fromEither[F](
          PageRequest
            .of(offset, PageRequest.MaximumSize)
            .leftMap(MethodsReadError.Paging(run, offset, _))
        )
        page <- EitherT(pairs(run, scale, request))
          .leftMap(MethodsReadError.Pairs(run, scale, offset, _))
        _ <- EitherT.cond[F](
          page.run == run && page.scale == scale,
          (),
          MethodsReadError.OtherPairPage(run, scale, page.run, page.scale)
        )
        all = got :+ page
        rest <- page.page.next match
          case None                        => EitherT.rightT[F, MethodsReadError](all)
          case Some(next) if next > offset => at(next, all)
          case Some(next)                  =>
            EitherT.leftT[F, Vector[PairRowPage]](
              MethodsReadError.PairsStalled(run, scale, offset, next)
            )
        rows = rest.map(_.rows.size).sum
        _ <- EitherT.cond[F](
          got.nonEmpty || rows == page.page.total,
          (),
          MethodsReadError.PairsShort(run, scale, rows, page.page.total)
        )
      yield rest
    at(0, Vector.empty).value
