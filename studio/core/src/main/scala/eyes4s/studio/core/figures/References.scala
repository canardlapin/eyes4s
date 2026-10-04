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
import eyes4s.studio.core.navigation.{NavigationError, Page}
import eyes4s.studio.core.selection.{ScaleIndex, StudioRef}

/** One pair score of a query, as eyes4s inspected it. */
final case class PairScore(reference: TrialKey, item: String, score: Double) derives CanEqual

/** What eyes4s answered for one query at one scale (the Figures board's
  * panel C): its contrast (M, B and D), its matched pair, and every control
  * pair eyes4s scored. B is the control mean over `controlMembers` pairs;
  * `controls` may hold fewer when the backend does not score them all.
  */
final case class ReferenceScores(
    m: Double,
    b: Double,
    d: Double,
    controlMembers: Int,
    matched: PairScore,
    controls: Vector[PairScore]
) derives CanEqual

/** Why a query's reference scores could not be read. Every case names its
  * operands.
  */
enum ReferenceReadError derives CanEqual:
  case Backend(address: ResultAddress, error: BackendError)
  case Navigation(query: TrialKey, design: PairDesign, error: NavigationError)
  case Paging(query: TrialKey, offset: Int, error: PageError)

  /** The backend answered `address` with another kind of inspection. */
  case Unexpected(address: ResultAddress, found: Inspection)

  /** The query has no matched pair (or more than one). */
  case Matched(query: TrialKey, references: Int)

  def message: String = this match
    case Backend(address, error)          => s"${address.render}: ${error.message}"
    case Navigation(query, design, error) =>
      s"The ${design.render} pairs of ${query.label}: ${error.message}"
    case Paging(query, offset, error) =>
      s"The pairs of ${query.label} at offset $offset: ${error.message}"
    case Unexpected(address, found) =>
      s"${address.render} was answered with ${found.productPrefix.toLowerCase}."
    case Matched(query, n) => s"${query.label} has $n matched pairs, not one."

/** Reads a query's [[ReferenceScores]] through the backend's inspection and
  * the navigator's pair listing; nothing is computed.
  */
object ReferenceReads:

  /** The scores of `query` at `scale` of `run`. A control pair the backend
    * does not score (`BackendError.Unavailable`) is left out; any other
    * refusal is the answer.
    */
  def read[F[_]: Monad](
      inspect: ResultAddress => F[Either[BackendError, Inspection]],
      pairs: (StudioRef, PairDesign, PageRequest) => F[
        Either[NavigationError, Page[StudioRef]]
      ],
      run: RunId,
      scale: ScaleIndex,
      query: TrialKey
  ): F[Either[ReferenceReadError, ReferenceScores]] =
    val contrast = StudioRef.QueryContrast(run, scale, query)
    def ask(address: ResultAddress): EitherT[F, ReferenceReadError, Inspection] =
      EitherT(inspect(address)).leftMap(ReferenceReadError.Backend(address, _))
    def references(design: PairDesign): EitherT[F, ReferenceReadError, Vector[TrialKey]] =
      def from(
          offset: Int,
          got: Vector[TrialKey]
      ): EitherT[F, ReferenceReadError, Vector[TrialKey]] =
        for
          request <- EitherT.fromEither[F](
            PageRequest
              .of(offset, PageRequest.MaximumSize)
              .leftMap(ReferenceReadError.Paging(query, offset, _))
          )
          page <- EitherT(pairs(contrast, design, request))
            .leftMap(ReferenceReadError.Navigation(query, design, _))
          keys = page.entries.collect { case StudioRef.Pair(_, _, _, _, reference) =>
            reference
          }
          all <- page.next
            .filter(_ > offset)
            .fold(EitherT.rightT[F, ReferenceReadError](got ++ keys))(
              from(_, got ++ keys)
            )
        yield all
      from(0, Vector.empty)
    def pair(design: PairDesign, reference: TrialKey) =
      val address = ResultAddress.PairRow(scale.value, design, query, reference)
      ask(address)
        .subflatMap {
          case Inspection.Pair(_, item, score) => Right(Some(PairScore(reference, item, score)))
          case other => Left(ReferenceReadError.Unexpected(address, other))
        }
        .recover {
          // A control pair the backend does not score is left out of the
          // controls; the matched pair is required.
          case ReferenceReadError.Backend(_, BackendError.Unavailable(_))
              if design == PairDesign.Control =>
            None
        }
    (for
      mbd <- ask(ResultAddress.ContrastRow(scale.value, query)).subflatMap {
        case Inspection.Contrast(_, m, b, d) => Right((m, b, d))
        case other                           =>
          Left(
            ReferenceReadError.Unexpected(ResultAddress.ContrastRow(scale.value, query), other)
          )
      }
      members <- ask(ResultAddress.Reduction(scale.value, PairDesign.Control, query))
        .subflatMap {
          case Inspection.Reduction(_, _, n) => Right(n)
          case other                         =>
            Left(
              ReferenceReadError.Unexpected(
                ResultAddress.Reduction(scale.value, PairDesign.Control, query),
                other
              )
            )
        }
      matchedKeys <- references(PairDesign.Matched)
      matchedKey  <- EitherT.fromEither[F](matchedKeys match
        case Vector(one) => Right(one)
        case other       => Left(ReferenceReadError.Matched(query, other.size)))
      matched  <- pair(PairDesign.Matched, matchedKey)
      controls <- references(PairDesign.Control).flatMap(
        _.traverse(pair(PairDesign.Control, _))
      )
      (m, b, d) = mbd
      scores <- EitherT.fromOption[F](
        matched.map(ReferenceScores(m, b, d, members, _, controls.flatten)),
        ReferenceReadError.Matched(query, 0)
      )
    yield scores).value
