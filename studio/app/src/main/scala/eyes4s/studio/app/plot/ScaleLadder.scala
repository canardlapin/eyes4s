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

package eyes4s.studio.app.plot

import cats.Monad
import cats.data.EitherT
import cats.syntax.all.*
import eyes4s.studio.app.text.{LadderText, LadderTextId}
import eyes4s.studio.core.backend.{
  BackendError,
  Inspection,
  PageError,
  PageRequest,
  PairDesign,
  QueryStatus,
  ResultAddress,
  RunId,
  TrialKey
}
import eyes4s.studio.core.navigation.{NavigationError, StudyNavigator}
import eyes4s.studio.core.selection.{RefError, ScaleIndex, StudioRef}

/** Why a query's scale ladder could not be read. Every case names the query
  * and the scale, address or ref that failed.
  */
enum LadderError derives CanEqual:

  /** The backend refused to inspect `address`. */
  case Backend(query: TrialKey, address: ResultAddress, error: BackendError)

  /** The navigator refused the pairs of `contrast`. */
  case Navigation(query: TrialKey, contrast: StudioRef, error: NavigationError)

  /** The query has no score at `scale`; `status` says why. */
  case Unscored(query: TrialKey, scale: String, status: QueryStatus)

  /** Inspecting `address` answered with another kind of item. */
  case Unexpected(query: TrialKey, address: ResultAddress, inspection: Inspection)

  /** The navigator listed `ref` among the pairs of `contrast`, which is not
    * one of its pairs.
    */
  case NotAPair(query: TrialKey, contrast: StudioRef, ref: StudioRef)

  /** The query has `count` matched pairs at `scale`; M is one pair's score. */
  case MatchedCount(query: TrialKey, scale: String, count: Int)

  /** The scale's index or a page of pairs could not be formed. */
  case Scale(query: TrialKey, scale: String, error: RefError)
  case Paging(query: TrialKey, contrast: StudioRef, error: PageError)

  def message: String = this match
    case Backend(q, a, e) =>
      s"Scale ladder of ${q.label}: inspecting the ${a.render} failed: ${e.message}"
    case Navigation(q, c, e) => s"Scale ladder of ${q.label}: pairs of $c: ${e.message}"
    case Unscored(q, s, st)  =>
      s"Scale ladder of ${q.label}: no score at $s (${st.productPrefix})."
    case Unexpected(q, a, i) =>
      s"Scale ladder of ${q.label}: the ${a.render} inspected as ${i.productPrefix}."
    case NotAPair(q, c, r) => s"Scale ladder of ${q.label}: $r is listed as a pair of $c."
    case MatchedCount(q, s, n) =>
      s"Scale ladder of ${q.label}: $n matched pairs at $s; M needs exactly one."
    case Scale(q, s, e)     => s"Scale ladder of ${q.label}: scale $s: ${e.message}"
    case Paging(q, c, e)    => s"Scale ladder of ${q.label}: pairs of $c: ${e.message}"

/** One control pair of a query at one scale: the control's trial and, when
  * the backend serves its score, the control's item and cosine.
  */
final case class LadderControl(
    ref: StudioRef,
    reference: TrialKey,
    item: Option[String],
    cosine: Option[Double]
) derives CanEqual

/** One scale of a query's ladder, every value as the backend served it: M,
  * the score of the matched pair `matched`; B, the control reduction `mean`
  * over `members` controls; D, the query's contrast row `contrast`; and the
  * query's control pairs.
  */
final case class LadderScale(
    scale: ScaleIndex,
    label: String,
    matched: StudioRef,
    matchedTrial: TrialKey,
    matchedItem: String,
    m: Double,
    mean: StudioRef,
    b: Double,
    members: Int,
    contrast: StudioRef,
    d: Double,
    controls: Vector[LadderControl]
) derives CanEqual

/** A query's scale ladder in one run (ticket S4.5b): its M, B, D and control
  * scores at every scale of the run.
  */
final case class ScaleLadder(run: RunId, query: TrialKey, scales: Vector[LadderScale])
    derives CanEqual

/** The columns of a scale ladder's value source. */
final case class LadderColumns(
    scale: ColumnId,
    role: ColumnId,
    reference: ColumnId,
    item: ColumnId,
    cosine: ColumnId,
    d: ColumnId
) derives CanEqual

object LadderColumns:
  val standard: Either[PlotSourceError, LadderColumns] =
    for
      scale     <- ColumnId.of("scale")
      role      <- ColumnId.of("role")
      reference <- ColumnId.of("reference")
      item      <- ColumnId.of("item")
      cosine    <- ColumnId.of("cosine")
      d         <- ColumnId.of("d")
    yield LadderColumns(scale, role, reference, item, cosine, d)

object ScaleLadder:

  /** How many pairs the loader asks the navigator for at a time. */
  val PageSize: Int = 256

  /** The ladder of `query` in `run` at each of `scales` (the run's scale
    * labels, in index order), read from the backend: the contrast row gives
    * D, the matched pair M, the control reduction B, and every control pair
    * its cosine. A control whose score the backend holds no data for
    * (`BackendError.Unavailable`) keeps its row with no cosine; any other
    * refusal fails the ladder. Nothing is computed here.
    */
  def load[F[_]: Monad](
      inspect: (RunId, ResultAddress) => F[Either[BackendError, Inspection]],
      navigator: StudyNavigator[F]
  )(run: RunId, query: TrialKey, scales: Vector[String]): F[Either[LadderError, ScaleLadder]] =
    type Step[A] = EitherT[F, LadderError, A]

    def lift[A](either: Either[LadderError, A]): Step[A] = EitherT.fromEither[F](either)

    def inspected(address: ResultAddress): Step[Inspection] =
      EitherT(inspect(run, address)).leftMap(LadderError.Backend(query, address, _))

    def scored[A](label: String, address: ResultAddress)(
        read: PartialFunction[Inspection, A]
    ): Step[A] =
      inspected(address).subflatMap {
        case i if read.isDefinedAt(i)       => Right(read(i))
        case Inspection.Unscored(_, status) => Left(LadderError.Unscored(query, label, status))
        case other                          => Left(LadderError.Unexpected(query, address, other))
      }

    def pairs(contrast: StudioRef, design: PairDesign): Step[Vector[StudioRef]] =
      EitherT(Monad[F].tailRecM((Vector.empty[StudioRef], 0)) { (got, offset) =>
        PageRequest.of(offset, PageSize) match
          case Left(e)        => Monad[F].pure(Right(Left(LadderError.Paging(query, contrast, e))))
          case Right(request) =>
            navigator.pairs(contrast, design, request).map {
              case Left(e) => Right(Left(LadderError.Navigation(query, contrast, e)))
              case Right(page) =>
                page.next match
                  case Some(next) if next > offset => Left((got ++ page.entries, next))
                  case _                           => Right(Right(got ++ page.entries))
            }
      })

    def pairAt(contrast: StudioRef, ref: StudioRef): Step[(TrialKey, ResultAddress)] =
      lift(
        (ref, ref.resultAddress) match
          case (StudioRef.Pair(_, _, _, _, reference), Some(address)) =>
            Right((reference, address))
          case _ => Left(LadderError.NotAPair(query, contrast, ref))
      )

    def control(label: String, contrast: StudioRef, ref: StudioRef): Step[LadderControl] =
      pairAt(contrast, ref).flatMap { (reference, address) =>
        EitherT(inspect(run, address)).transform {
          case Right(Inspection.Pair(_, item, score)) =>
            Right(LadderControl(ref, reference, Some(item), Some(score)))
          case Right(Inspection.Unscored(_, status)) =>
            Left(LadderError.Unscored(query, label, status))
          case Right(other)                        => Left(LadderError.Unexpected(query, address, other))
          case Left(BackendError.Unavailable(_)) => Right(LadderControl(ref, reference, None, None))
          case Left(e)                           => Left(LadderError.Backend(query, address, e))
        }
      }

    def scaleAt(label: String, index: Int): Step[LadderScale] =
      for
        scale <- lift(ScaleIndex.of(index).leftMap(LadderError.Scale(query, label, _)))
        contrast = StudioRef.QueryContrast(run, scale, query)
        d <- scored(label, ResultAddress.ContrastRow(index, query)) {
          case Inspection.Contrast(_, _, _, d) => d
        }
        matchedRefs <- pairs(contrast, PairDesign.Matched)
        matched     <- lift(matchedRefs match
          case Vector(one) => Right(one)
          case other       => Left(LadderError.MatchedCount(query, label, other.size)))
        (matchedTrial, matchedAddress) <- pairAt(contrast, matched)
        scoredMatch                    <- scored(label, matchedAddress) {
          case Inspection.Pair(_, item, score) => (item, score)
        }
        meanAddress = ResultAddress.Reduction(index, PairDesign.Control, query)
        meanRef <- lift(
          StudioRef.fromAddress(run, meanAddress).leftMap(LadderError.Scale(query, label, _))
        )
        mean <- scored(label, meanAddress) { case Inspection.Reduction(_, value, members) =>
          (value, members)
        }
        controlRefs <- pairs(contrast, PairDesign.Control)
        controls    <- controlRefs.traverse(control(label, contrast, _))
      yield LadderScale(
        scale,
        label,
        matched,
        matchedTrial,
        scoredMatch._1,
        scoredMatch._2,
        meanRef,
        mean._1,
        mean._2,
        contrast,
        d,
        controls
      )

    scales.zipWithIndex
      .traverse((label, index) => scaleAt(label, index))
      .map(ScaleLadder(run, query, _))
      .value

  /** The ladder as a value source: at each scale, in the run's scale order,
    * M (the matched pair), B (the control reduction), D (the contrast row)
    * and then every control, each a row with its own ref. A cosine the
    * backend did not serve is missing, never zero; D has no cosine, and only
    * D has a D.
    */
  def source(ladder: ScaleLadder, columns: LadderColumns): Either[PlotSourceError, PlotSource] =
    def text(s: String)          = PlotValue.Text(s)
    def number(v: Double)        = PlotValue.Number(v)
    def optional[A](o: Option[A])(f: A => PlotValue) = o.fold(PlotValue.Missing)(f)
    val none                     = PlotValue.Missing
    val rows                     = ladder.scales.flatMap { s =>
      val scale = text(s.label)
      Vector(
        PlotRow(
          s.matched,
          Vector(
            scale,
            text(LadderText(LadderTextId.Matched)),
            text(s.matchedTrial.trial),
            text(s.matchedItem),
            number(s.m),
            none
          )
        ),
        PlotRow(
          s.mean,
          Vector(
            scale,
            text(LadderText(LadderTextId.ControlMean)),
            text(LadderText(LadderTextId.MeanOf, s.members.toString)),
            none,
            number(s.b),
            none
          )
        ),
        PlotRow(
          s.contrast,
          Vector(
            scale,
            text(LadderText(LadderTextId.Contrast)),
            text(LadderText(LadderTextId.Difference)),
            none,
            none,
            number(s.d)
          )
        )
      ) ++ s.controls.map(c =>
        PlotRow(
          c.ref,
          Vector(
            scale,
            text(LadderText(LadderTextId.Control)),
            text(c.reference.trial),
            optional(c.item)(text),
            optional(c.cosine)(number),
            none
          )
        )
      )
    }
    PlotSource(
      LadderText(LadderTextId.Caption, ladder.query.label),
      Vector(
        PlotColumn(columns.scale, LadderText(LadderTextId.ScaleHeader), ColumnFormat.Label),
        PlotColumn(columns.role, LadderText(LadderTextId.RoleHeader), ColumnFormat.Label),
        PlotColumn(
          columns.reference,
          LadderText(LadderTextId.ReferenceHeader),
          ColumnFormat.Label
        ),
        PlotColumn(columns.item, LadderText(LadderTextId.ItemHeader), ColumnFormat.Label),
        PlotColumn(columns.cosine, LadderText(LadderTextId.CosineHeader), ColumnFormat.Decimal(2)),
        PlotColumn(columns.d, LadderText(LadderTextId.DHeader), ColumnFormat.Signed(2))
      ),
      rows
    )
