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

package eyes4s.studio.core.preview

import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.{CoreBinding, Recipe, StudyPlanArtifact}
import eyes4s.studio.core.execution.{RunStamp, StudyInputArtifact}
import io.circe.{Codec, Decoder, Encoder}
import ProtocolCodecs.portableLong

/** An opaque backend resource name. It is deliberately not a digest: only the
  * backend can resolve it to the prepared study it retained.
  */
final case class PreviewId(value: Long) derives CanEqual
object PreviewId:
  given Codec[PreviewId] =
    Codec.from(portableLong.map(PreviewId(_)), portableLong.contramap(_.value))

/** A positive number of bounded cursor pages permitted by one request.
  * The fake's fixture page completes one participant; the real backend
  * advances one library counting step with its fixed work quantum.
  */
sealed abstract case class PreviewBudget private (pages: Int) derives CanEqual
object PreviewBudget:
  private def apply(pages: Int): PreviewBudget =
    new PreviewBudget(pages) {}

  val MaximumPages: Int                                         = 4096
  def of(pages: Int): Either[PreviewBudgetError, PreviewBudget] =
    Either.cond(
      pages > 0 && pages <= MaximumPages,
      PreviewBudget(pages),
      PreviewBudgetError.OutOfRange(pages, MaximumPages)
    )
  given Encoder[PreviewBudget] = Encoder.encodeInt.contramap(_.pages)
  given Decoder[PreviewBudget] = Decoder.decodeInt.emap(of(_).left.map(_.message))

enum PreviewBudgetError derives CanEqual:
  case OutOfRange(pages: Int, maximum: Int)
  def message: String = this match
    case OutOfRange(p, m) => s"Preview budget $p is outside 1 to $m bounded pages."

/** Why a preview count was refused. Every case names the field and the value
  * it refused.
  */
enum PreviewError derives CanEqual:
  case Negative(field: String, value: Long)
  case BeyondTotal(field: String, done: Long, total: Long)

  /** The focal trials by status do not add up to those requested. */
  case QueryPartition(
      requested: Int,
      eligible: Int,
      unmatched: Int,
      notAdmitted: Int,
      byDesign: Option[Int]
  )

  def message: String = this match
    case Negative(field, value)          => s"Preview $field is negative: $value."
    case BeyondTotal(field, done, total) =>
      s"Preview $field reports $done, beyond its total $total."
    case QueryPartition(requested, eligible, unmatched, notAdmitted, byDesign) =>
      val parts =
        Vector(s"$eligible eligible", s"$unmatched unmatched", s"$notAdmitted not admitted") ++
          byDesign.map(n => s"$n by design")
      s"Preview requests $requested queries, but ${parts.mkString(" + ")} is " +
        s"${eligible.toLong + unmatched + notAdmitted + byDesign.getOrElse(0)}."

private object PreviewCount:
  def nonNegative(field: String, value: Long): Either[PreviewError, Unit] =
    Either.cond(value >= 0, (), PreviewError.Negative(field, value))

  /** A decoder that refuses what a smart constructor refuses. */
  def decoder[A](decode: Decoder[Either[PreviewError, A]]): Decoder[A] =
    decode.emap(_.left.map(_.message))

/** A number of participants, never negative. */
sealed abstract case class ParticipantCount private[preview] (value: Int) derives CanEqual
object ParticipantCount:
  private[preview] def apply(value: Int): ParticipantCount =
    new ParticipantCount(value) {}

  def of(value: Int): Either[PreviewError, ParticipantCount] =
    PreviewCount.nonNegative("participant count", value).map(_ => ParticipantCount(value))
  given Encoder[ParticipantCount] = Encoder.encodeInt.contramap(_.value)
  given Decoder[ParticipantCount] = PreviewCount.decoder(Decoder.decodeInt.map(of))

/** A number of focal trials (queries), never negative. */
sealed abstract case class QueryCount private[preview] (value: Int) derives CanEqual
object QueryCount:
  private[preview] def apply(value: Int): QueryCount =
    new QueryCount(value) {}

  def of(value: Int): Either[PreviewError, QueryCount] =
    PreviewCount.nonNegative("query count", value).map(_ => QueryCount(value))
  given Encoder[QueryCount] = Encoder.encodeInt.contramap(_.value)
  given Decoder[QueryCount] = PreviewCount.decoder(Decoder.decodeInt.map(of))

/** The immutable candidate metadata known before eligibility has been counted.
  * No count is negative.
  *
  * The query counts come from the trial inventory and the recipe, before any
  * pair is counted: `requestedQueries` focal trials the design asks for, of
  * which `queriesNotAdmitted` the admission refused, and `byDesignQueries`
  * focal trials the recipe states have no reference by design (a recognition
  * lure), or `None` when the recipe has no such category.
  */
sealed abstract case class PreviewCandidates private (
    focalTrials: Int,
    referenceTrials: Int,
    participants: Int,
    candidatePairsPerScale: Long,
    requestedQueries: QueryCount,
    queriesNotAdmitted: QueryCount,
    byDesignQueries: Option[QueryCount]
) derives CanEqual

object PreviewCandidates:
  private def apply(
      focalTrials: Int,
      referenceTrials: Int,
      participants: Int,
      candidatePairsPerScale: Long,
      requestedQueries: QueryCount,
      queriesNotAdmitted: QueryCount,
      byDesignQueries: Option[QueryCount]
  ): PreviewCandidates =
    new PreviewCandidates(
      focalTrials,
      referenceTrials,
      participants,
      candidatePairsPerScale,
      requestedQueries,
      queriesNotAdmitted,
      byDesignQueries
    ) {}

  def of(
      focalTrials: Int,
      referenceTrials: Int,
      participants: Int,
      candidatePairsPerScale: Long,
      requestedQueries: Int,
      queriesNotAdmitted: Int,
      byDesignQueries: Option[Int]
  ): Either[PreviewError, PreviewCandidates] =
    for
      _ <- PreviewCount.nonNegative("focalTrials", focalTrials)
      _ <- PreviewCount.nonNegative("referenceTrials", referenceTrials)
      _ <- PreviewCount.nonNegative("participants", participants)
      _ <- PreviewCount.nonNegative("candidatePairsPerScale", candidatePairsPerScale)
      _ <- PreviewCount.nonNegative("requestedQueries", requestedQueries)
      _ <- PreviewCount.nonNegative("queriesNotAdmitted", queriesNotAdmitted)
      _ <- Either.cond(
        queriesNotAdmitted <= requestedQueries,
        (),
        PreviewError.BeyondTotal("queriesNotAdmitted", queriesNotAdmitted, requestedQueries)
      )
      _ <- byDesignQueries.fold(Right(()))(PreviewCount.nonNegative("byDesignQueries", _))
    yield PreviewCandidates(
      focalTrials,
      referenceTrials,
      participants,
      candidatePairsPerScale,
      QueryCount(requestedQueries),
      QueryCount(queriesNotAdmitted),
      byDesignQueries.map(QueryCount(_))
    )

  given Encoder.AsObject[PreviewCandidates] =
    Encoder.forProduct7(
      "focalTrials",
      "referenceTrials",
      "participants",
      "candidatePairsPerScale",
      "requestedQueries",
      "queriesNotAdmitted",
      "byDesignQueries"
    )(c =>
      (
        c.focalTrials,
        c.referenceTrials,
        c.participants,
        c.candidatePairsPerScale,
        c.requestedQueries.value,
        c.queriesNotAdmitted.value,
        c.byDesignQueries.map(_.value)
      )
    )

  given Decoder[PreviewCandidates] = PreviewCount.decoder(
    Decoder.forProduct7[
      Either[PreviewError, PreviewCandidates],
      Int,
      Int,
      Int,
      Long,
      Int,
      Int,
      Option[Int]
    ](
      "focalTrials",
      "referenceTrials",
      "participants",
      "candidatePairsPerScale",
      "requestedQueries",
      "queriesNotAdmitted",
      "byDesignQueries"
    )(of)
  )

/** A count supplied only after the backend has completed a participant page:
  * neither count is negative and no more participants are complete than exist.
  */
sealed abstract case class PreviewProgress private (
    completedParticipants: Int,
    totalParticipants: Int
) derives CanEqual:

  /** Every participant has been counted. */
  def isComplete: Boolean = completedParticipants == totalParticipants

  /** One more participant counted, or `None` when every one already is. */
  def advance: Option[PreviewProgress] =
    Option.unless(isComplete)(PreviewProgress(completedParticipants + 1, totalParticipants))

  def completed: ParticipantCount = ParticipantCount(completedParticipants)
  def total: ParticipantCount     = ParticipantCount(totalParticipants)

object PreviewProgress:
  private def apply(completedParticipants: Int, totalParticipants: Int): PreviewProgress =
    new PreviewProgress(completedParticipants, totalParticipants) {}

  /** No participant of `candidates` counted yet. */
  def start(candidates: PreviewCandidates): PreviewProgress =
    PreviewProgress(0, candidates.participants)

  def of(
      completedParticipants: Int,
      totalParticipants: Int
  ): Either[PreviewError, PreviewProgress] =
    for
      _ <- PreviewCount.nonNegative("completedParticipants", completedParticipants)
      _ <- PreviewCount.nonNegative("totalParticipants", totalParticipants)
      _ <- Either.cond(
        completedParticipants <= totalParticipants,
        (),
        PreviewError
          .BeyondTotal("completedParticipants", completedParticipants, totalParticipants)
      )
    yield PreviewProgress(completedParticipants, totalParticipants)

  given Encoder.AsObject[PreviewProgress] =
    Encoder.forProduct2("completedParticipants", "totalParticipants")(p =>
      (p.completedParticipants, p.totalParticipants)
    )

  given Decoder[PreviewProgress] = PreviewCount.decoder(
    Decoder.forProduct2[Either[PreviewError, PreviewProgress], Int, Int](
      "completedParticipants",
      "totalParticipants"
    )(of)
  )

/** Exact result of a retained preview. These are counts, not a scientific
  * result, and none is negative. `eligibleQueries` focal trials enter the
  * comparisons; `unmatchedQueries` were admitted without a usable declared
  * reference. By-design queries are counted separately in the candidates.
  */
sealed abstract case class PreviewCounts private (
    eligiblePairsPerScale: Long,
    eligiblePairs: Long,
    eligibleQueries: QueryCount,
    unmatchedQueries: QueryCount,
    ambiguousMatches: Int,
    knownFailingFocalQueries: Option[QueryCount]
) derives CanEqual:
  /** Pair-eligible queries without a known focal preparation failure.
    * This is not a count of successful, scored contrasts.
    */
  def withoutKnownFocalFailure: Option[Int] =
    knownFailingFocalQueries.map(n => eligibleQueries.value - n.value)

object PreviewCounts:
  private def apply(
      eligiblePairsPerScale: Long,
      eligiblePairs: Long,
      eligibleQueries: QueryCount,
      unmatchedQueries: QueryCount,
      ambiguousMatches: Int,
      knownFailingFocalQueries: Option[QueryCount]
  ): PreviewCounts =
    new PreviewCounts(
      eligiblePairsPerScale,
      eligiblePairs,
      eligibleQueries,
      unmatchedQueries,
      ambiguousMatches,
      knownFailingFocalQueries
    ) {}

  def of(
      eligiblePairsPerScale: Long,
      eligiblePairs: Long,
      eligibleQueries: Int,
      unmatchedQueries: Int,
      ambiguousMatches: Int,
      knownFailingFocalQueries: Option[Int] = None
  ): Either[PreviewError, PreviewCounts] =
    for
      _ <- PreviewCount.nonNegative("eligiblePairsPerScale", eligiblePairsPerScale)
      _ <- PreviewCount.nonNegative("eligiblePairs", eligiblePairs)
      _ <- PreviewCount.nonNegative("eligibleQueries", eligibleQueries)
      _ <- PreviewCount.nonNegative("unmatchedQueries", unmatchedQueries)
      _ <- PreviewCount.nonNegative("ambiguousMatches", ambiguousMatches)
      _ <- knownFailingFocalQueries.fold[Either[PreviewError, Unit]](Right(())) { n =>
        PreviewCount
          .nonNegative("knownFailingFocalQueries", n)
          .flatMap(_ =>
            Either.cond(
              n <= eligibleQueries,
              (),
              PreviewError.BeyondTotal("knownFailingFocalQueries", n, eligibleQueries)
            )
          )
      }
    yield PreviewCounts(
      eligiblePairsPerScale,
      eligiblePairs,
      QueryCount(eligibleQueries),
      QueryCount(unmatchedQueries),
      ambiguousMatches,
      knownFailingFocalQueries.map(QueryCount(_))
    )

  given Encoder.AsObject[PreviewCounts] =
    Encoder.forProduct6(
      "eligiblePairsPerScale",
      "eligiblePairs",
      "eligibleQueries",
      "unmatchedQueries",
      "ambiguousMatches",
      "knownFailingFocalQueries"
    )(c =>
      (
        c.eligiblePairsPerScale,
        c.eligiblePairs,
        c.eligibleQueries.value,
        c.unmatchedQueries.value,
        c.ambiguousMatches,
        c.knownFailingFocalQueries.map(_.value)
      )
    )

  given Decoder[PreviewCounts] = PreviewCount.decoder(
    Decoder
      .forProduct6[Either[PreviewError, PreviewCounts], Long, Long, Int, Int, Int, Option[Int]](
        "eligiblePairsPerScale",
        "eligiblePairs",
        "eligibleQueries",
        "unmatchedQueries",
        "ambiguousMatches",
        "knownFailingFocalQueries"
      )(of)
  )

/** Receipt of a ready backend-owned preview. A caller may carry this receipt,
  * but cannot manufacture a snapshot from it.
  *
  * Native receipts carry the exact recipe used to prepare the bound artifacts,
  * so a client can detect edits under the same draft revision id. Fixture
  * receipts have no native recipe snapshot and retain their unbound stamps.
  *
  * Its query counts partition the requested focal trials: requested =
  * eligible + unmatched + not admitted (+ by design, when the recipe has that
  * category), so no count exceeds the requested ones.
  */
sealed abstract case class PreviewReady private (
    id: PreviewId,
    stamp: RunStamp,
    candidates: PreviewCandidates,
    counts: PreviewCounts,
    diagnostics: Vector[StudioDiagnostic],
    recipe: Option[Recipe]
) derives CanEqual

object PreviewReady:
  private def apply(
      id: PreviewId,
      stamp: RunStamp,
      candidates: PreviewCandidates,
      counts: PreviewCounts,
      diagnostics: Vector[StudioDiagnostic],
      recipe: Option[Recipe]
  ): PreviewReady =
    new PreviewReady(id, stamp, candidates, counts, diagnostics, recipe) {}

  def of(
      id: PreviewId,
      stamp: RunStamp,
      candidates: PreviewCandidates,
      counts: PreviewCounts,
      diagnostics: Vector[StudioDiagnostic],
      recipe: Option[Recipe] = None
  ): Either[PreviewError, PreviewReady] =
    partition(candidates, counts).map(_ =>
      PreviewReady(id, stamp, candidates, counts, diagnostics, recipe)
    )

  /** Whether `counts` partition the queries `candidates` requests. */
  def partition(
      candidates: PreviewCandidates,
      counts: PreviewCounts
  ): Either[PreviewError, Unit] =
    val requested   = candidates.requestedQueries.value
    val eligible    = counts.eligibleQueries.value
    val unmatched   = counts.unmatchedQueries.value
    val notAdmitted = candidates.queriesNotAdmitted.value
    val byDesign    = candidates.byDesignQueries.map(_.value)
    val total       = eligible.toLong + unmatched + notAdmitted + byDesign.getOrElse(0)
    Either.cond(
      total == requested,
      (),
      PreviewError.QueryPartition(requested, eligible, unmatched, notAdmitted, byDesign)
    )

  given Encoder.AsObject[PreviewReady] =
    Encoder.forProduct6("id", "stamp", "candidates", "counts", "diagnostics", "recipe")(r =>
      (r.id, r.stamp, r.candidates, r.counts, r.diagnostics, r.recipe)
    )

  given Decoder[PreviewReady] = PreviewCount.decoder(
    Decoder.forProduct6[
      Either[PreviewError, PreviewReady],
      PreviewId,
      RunStamp,
      PreviewCandidates,
      PreviewCounts,
      Vector[StudioDiagnostic],
      Option[Recipe]
    ]("id", "stamp", "candidates", "counts", "diagnostics", "recipe")(of)
  )

/** Frames of a bounded preview request. Creation starts with Initial;
  * continuations emit progress or the ready receipt. Each request contains at
  * most its budget of Counting or CountingWork frames. Ready appears when the
  * retained count traversal completes; a completed continuation returns it again.
  */
enum PreviewEvent derives CanEqual, Codec.AsObject:
  case Initial(id: PreviewId, stamp: RunStamp, candidates: PreviewCandidates)
  case Counting(id: PreviewId, progress: PreviewProgress)

  /** One native cursor page. Schedule work includes pair enumeration and
    * schedule diagnostics; workUnits also counts cardinality diagnostics.
    */
  case CountingWork(
      id: PreviewId,
      design: PairDesign,
      workUnits: Int,
      visitedScheduleWork: Long
  )
  case Ready(ready: PreviewReady)

/** The retained stamp fake backends can honestly serve; fixture values stay
  * visibly unbound while the real backend uses native canonical codec digests.
  */
private[core] object PreviewStamp:
  def fake(revision: AnalysisRevision, dataset: DatasetRevision): RunStamp =
    RunStamp(
      revision,
      dataset,
      CoreBinding.unbound[StudyPlanArtifact],
      CoreBinding.unbound[StudyInputArtifact]
    )
