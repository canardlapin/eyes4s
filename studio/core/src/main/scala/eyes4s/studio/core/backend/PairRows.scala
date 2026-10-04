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

import io.circe.Codec

/** A pair row's score as the run holds it (protocol 1.9). */
enum PairScoreState derives CanEqual, Codec.AsObject:
  /** eyes4s compared the pair: its score. */
  case Scored(score: Double)

  /** The pair's comparison failed; eyes4s's diagnostic says why. */
  case Failed(diagnostic: StudioDiagnostic)

  /** The run's archive holds the pair but the backend does not serve its
    * score (the fake backend scores only the pairs its fixture holds).
    */
  case NotServed

/** One directed pair of a run at one scale (eyes4s `PairScores`): the query,
  * which reference design it belongs to, the reference trial and its item,
  * and its score.
  */
final case class PairRowEntry(
    query: TrialKey,
    design: PairDesign,
    reference: TrialKey,
    referenceItem: String,
    score: PairScoreState
) derives CanEqual,
      Codec.AsObject

/** A page of a run's pair rows at scale index `scale` (protocol 1.9), in
  * focal (source) order, each query's matched pair before its controls.
  */
final case class PairRowPage(run: RunId, scale: Int, page: PageInfo, rows: Vector[PairRowEntry])
    derives CanEqual,
      Codec.AsObject
