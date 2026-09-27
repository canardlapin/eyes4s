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

/** The immutable candidate metadata known before eligibility has been counted. */
final case class PreviewCandidates(
    focalTrials: Int,
    referenceTrials: Int,
    participants: Int,
    candidatePairsPerScale: Long
) derives CanEqual,
      Codec.AsObject

/** A count supplied only after the backend has completed a participant page. */
final case class PreviewProgress(completedParticipants: Int, totalParticipants: Int)
    derives CanEqual,
      Codec.AsObject

/** Exact result of a retained preview. These are counts, not a scientific result. */
final case class PreviewCounts(
    eligiblePairsPerScale: Long,
    eligiblePairs: Long,
    unmatchedQueries: Int,
    ambiguousMatches: Int
) derives CanEqual,
      Codec.AsObject

/** Receipt of a ready backend-owned preview. A caller may carry this receipt,
  * but cannot manufacture a snapshot from it.
  */
final case class PreviewReady(
    id: PreviewId,
    stamp: RunStamp,
    candidates: PreviewCandidates,
    counts: PreviewCounts,
    diagnostics: Vector[StudioDiagnostic]
) derives CanEqual,
      Codec.AsObject

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
