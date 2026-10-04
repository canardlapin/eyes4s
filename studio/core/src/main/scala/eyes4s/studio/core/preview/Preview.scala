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
import eyes4s.studio.core.document.{CoreBinding, StudyPlanArtifact}
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

/** A positive number of participant pages a caller permits one request to do.
  * The fake advances exactly one participant for each page.
  */
final case class PreviewBudget private (participants: Int) derives CanEqual
object PreviewBudget:
  /** A single request stays bounded independently of a particular study. */
  val MaximumParticipants: Int                                         = 4096
  def of(participants: Int): Either[PreviewBudgetError, PreviewBudget] =
    Either.cond(
      participants > 0 && participants <= MaximumParticipants,
      new PreviewBudget(participants),
      PreviewBudgetError.OutOfRange(participants, MaximumParticipants)
    )
  given Encoder[PreviewBudget] = Encoder.encodeInt.contramap(_.participants)
  given Decoder[PreviewBudget] = Decoder.decodeInt.emap(of(_).left.map(_.message))

enum PreviewBudgetError derives CanEqual:
  case OutOfRange(participants: Int, maximum: Int)
  def message: String = this match
    case OutOfRange(p, m) => s"Preview budget $p is outside 1 to $m participants."

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
final case class ParticipantCount private[preview] (value: Int) derives CanEqual
object ParticipantCount:
  def of(value: Int): Either[PreviewError, ParticipantCount] =
    PreviewCount.nonNegative("participant count", value).map(_ => new ParticipantCount(value))
  given Encoder[ParticipantCount] = Encoder.encodeInt.contramap(_.value)
  given Decoder[ParticipantCount] = PreviewCount.decoder(Decoder.decodeInt.map(of))

/** A number of focal trials (queries), never negative. */
final case class QueryCount private[preview] (value: Int) derives CanEqual
object QueryCount:
  def of(value: Int): Either[PreviewError, QueryCount] =
    PreviewCount.nonNegative("query count", value).map(_ => new QueryCount(value))
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
final case class PreviewCandidates private (
    focalTrials: Int,
    referenceTrials: Int,
    participants: Int,
    candidatePairsPerScale: Long,
    requestedQueries: QueryCount,
    queriesNotAdmitted: QueryCount,
    byDesignQueries: Option[QueryCount]
) derives CanEqual

object PreviewCandidates:
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
    yield new PreviewCandidates(
      focalTrials,
      referenceTrials,
      participants,
      candidatePairsPerScale,
      new QueryCount(requestedQueries),
      new QueryCount(queriesNotAdmitted),
      byDesignQueries.map(new QueryCount(_))
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
final case class PreviewProgress private (completedParticipants: Int, totalParticipants: Int)
    derives CanEqual:

  /** Every participant has been counted. */
  def isComplete: Boolean = completedParticipants == totalParticipants

  /** One more participant counted, or `None` when every one already is. */
  def advance: Option[PreviewProgress] =
    Option.unless(isComplete)(new PreviewProgress(completedParticipants + 1, totalParticipants))

  def completed: ParticipantCount = new ParticipantCount(completedParticipants)
  def total: ParticipantCount     = new ParticipantCount(totalParticipants)

object PreviewProgress:
  /** No participant of `candidates` counted yet. */
  def start(candidates: PreviewCandidates): PreviewProgress =
    new PreviewProgress(0, candidates.participants)

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
    yield new PreviewProgress(completedParticipants, totalParticipants)

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
  * comparisons; `unmatchedQueries` were admitted without a matched reference.
  */
final case class PreviewCounts private (
    eligiblePairsPerScale: Long,
    eligiblePairs: Long,
    eligibleQueries: QueryCount,
    unmatchedQueries: QueryCount,
    ambiguousMatches: Int
) derives CanEqual

object PreviewCounts:
  def of(
      eligiblePairsPerScale: Long,
      eligiblePairs: Long,
      eligibleQueries: Int,
      unmatchedQueries: Int,
      ambiguousMatches: Int
  ): Either[PreviewError, PreviewCounts] =
    for
      _ <- PreviewCount.nonNegative("eligiblePairsPerScale", eligiblePairsPerScale)
      _ <- PreviewCount.nonNegative("eligiblePairs", eligiblePairs)
      _ <- PreviewCount.nonNegative("eligibleQueries", eligibleQueries)
      _ <- PreviewCount.nonNegative("unmatchedQueries", unmatchedQueries)
      _ <- PreviewCount.nonNegative("ambiguousMatches", ambiguousMatches)
    yield new PreviewCounts(
      eligiblePairsPerScale,
      eligiblePairs,
      new QueryCount(eligibleQueries),
      new QueryCount(unmatchedQueries),
      ambiguousMatches
    )

  given Encoder.AsObject[PreviewCounts] =
    Encoder.forProduct5(
      "eligiblePairsPerScale",
      "eligiblePairs",
      "eligibleQueries",
      "unmatchedQueries",
      "ambiguousMatches"
    )(c =>
      (
        c.eligiblePairsPerScale,
        c.eligiblePairs,
        c.eligibleQueries.value,
        c.unmatchedQueries.value,
        c.ambiguousMatches
      )
    )

  given Decoder[PreviewCounts] = PreviewCount.decoder(
    Decoder.forProduct5[Either[PreviewError, PreviewCounts], Long, Long, Int, Int, Int](
      "eligiblePairsPerScale",
      "eligiblePairs",
      "eligibleQueries",
      "unmatchedQueries",
      "ambiguousMatches"
    )(of)
  )

/** Receipt of a ready backend-owned preview. A caller may carry this receipt,
  * but cannot manufacture a snapshot from it.
  *
  * Its query counts partition the requested focal trials: requested =
  * eligible + unmatched + not admitted (+ by design, when the recipe has that
  * category), so no count exceeds the requested ones.
  */
final case class PreviewReady private (
    id: PreviewId,
    stamp: RunStamp,
    candidates: PreviewCandidates,
    counts: PreviewCounts,
    diagnostics: Vector[StudioDiagnostic]
) derives CanEqual

object PreviewReady:
  def of(
      id: PreviewId,
      stamp: RunStamp,
      candidates: PreviewCandidates,
      counts: PreviewCounts,
      diagnostics: Vector[StudioDiagnostic]
  ): Either[PreviewError, PreviewReady] =
    partition(candidates, counts).map(_ =>
      new PreviewReady(id, stamp, candidates, counts, diagnostics)
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
    Encoder.forProduct5("id", "stamp", "candidates", "counts", "diagnostics")(r =>
      (r.id, r.stamp, r.candidates, r.counts, r.diagnostics)
    )

  given Decoder[PreviewReady] = PreviewCount.decoder(
    Decoder.forProduct5[
      Either[PreviewError, PreviewReady],
      PreviewId,
      RunStamp,
      PreviewCandidates,
      PreviewCounts,
      Vector[StudioDiagnostic]
    ]("id", "stamp", "candidates", "counts", "diagnostics")(of)
  )

/** Frames of a bounded preview page. Creation starts with Initial; continuations
  * emit only progress or the ready receipt. Each page contains at most its
  * requested number of Counting frames; Ready appears after all participants
  * have been counted, and a completed continuation returns Ready again.
  */
enum PreviewEvent derives CanEqual, Codec.AsObject:
  case Initial(id: PreviewId, stamp: RunStamp, candidates: PreviewCandidates)
  case Counting(id: PreviewId, progress: PreviewProgress)
  case Ready(ready: PreviewReady)

/** The retained stamp fake backends can honestly serve. Real bindings arrive
  * with S3.7; fake values stay visibly unbound.
  */
private[core] object PreviewStamp:
  def fake(revision: AnalysisRevision, dataset: DatasetRevision): RunStamp =
    RunStamp(
      revision,
      dataset,
      CoreBinding.unbound[StudyPlanArtifact],
      CoreBinding.unbound[StudyInputArtifact]
    )
