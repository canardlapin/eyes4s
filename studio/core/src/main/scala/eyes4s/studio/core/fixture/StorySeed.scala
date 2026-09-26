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

package eyes4s.studio.core.fixture

import cats.data.EitherT
import cats.effect.Concurrent
import cats.syntax.all.*
import eyes4s.studio.core.assets.AssetRegistry
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.StudioDocument
import eyes4s.studio.core.freshness.{DraftCheck, Freshness, SessionFacts}

/** Why a story seed could not be set up. Every case names its moment. */
enum SeedError derives CanEqual:
  case Document(moment: StoryMoment, reason: String)
  case Backend(moment: StoryMoment, error: BackendError)

  /** The document's runs and the fake's differ (id, revision, dataset). */
  case Runs(
      moment: StoryMoment,
      document: Vector[(RunId, AnalysisRevision, DatasetRevision)],
      backend: Vector[(RunId, AnalysisRevision, DatasetRevision)]
  )

  /** The document's job handles and the fake's unfinished jobs differ. */
  case Jobs(
      moment: StoryMoment,
      document: Vector[(RunId, JobId)],
      backend: Vector[(RunId, JobId)]
  )
  case Assets(moment: StoryMoment, reason: String)

  def message: String = this match
    case Document(m, reason) => s"Story moment $m: the document is refused: $reason"
    case Backend(m, error)   => s"Story moment $m: the fake backend refused: ${error.message}"
    case Runs(m, d, b)       =>
      s"Story moment $m: the document's runs ${render(d)} are not the fake's ${render(b)}."
    case Jobs(m, d, b) =>
      def jobs(v: Vector[(RunId, JobId)]) =
        v.map((r, j) => s"run ${r.number} → job ${j.number}").mkString("[", ", ", "]")
      s"Story moment $m: the document's jobs ${jobs(d)} are not the fake's ${jobs(b)}."
    case Assets(m, reason) => s"Story moment $m: the asset registry is refused: $reason"

  private def render(v: Vector[(RunId, AnalysisRevision, DatasetRevision)]): String =
    v.map((r, a, d) => s"run ${r.number} (${a.label}, ${d.label})").mkString("[", ", ", "]")

/** One story moment of FIXTURE.md set up as a board shows it (ticket S0.8):
  * the document, the fake backend at that moment, the session facts that
  * backend reports, and the latest dataset revision's asset registry. The
  * freshness grammar (S2.7) reads the document and the session.
  */
final case class StorySeed[F[_]](
    moment: StoryMoment,
    document: StudioDocument,
    backend: FakeStudyBackend[F],
    session: SessionFacts,
    assets: AssetRegistry
):
  def freshness: Freshness = Freshness.of(document, session)

/** The story seeds: t1 (Data · verify), t2 (Explore, Analysis, Compare ·
  * query, Figures) and t3 (Compare · summary, run 8 held mid-run on the fake
  * at 21,400 / 44,845 pairs).
  */
object StorySeed:

  /** The document of `moment` ([[StoryMoments]]). */
  def document(moment: StoryMoment): Either[String, StudioDocument] = moment match
    case StoryMoment.T1 => StoryMoments.t1
    case StoryMoment.T2 => StoryMoments.t2
    case StoryMoment.T3 => StoryMoments.t3

  /** Set up `moment`: build its document and a fresh fake backend, check that
    * they agree on every run and job, and read the session from the fake:
    * the progress of its running jobs and, while the document has a draft,
    * the draft's check (the fake's preview of it, which reports no
    * diagnostic).
    */
  def load[F[_]: Concurrent](moment: StoryMoment): F[Either[SeedError, StorySeed[F]]] =
    (for
      doc    <- EitherT.fromEither[F](document(moment).leftMap(SeedError.Document(moment, _)))
      assets <- EitherT.fromEither[F](
        doc.datasets.lastOption
          .toRight("the document has no dataset revision")
          .flatMap(GoldenAssets.registry)
          .leftMap(SeedError.Assets(moment, _))
      )
      backend <- EitherT.liftF(FakeStudyBackend.create[F](moment))
      runs    <- EitherT.liftF(backend.runs)
      _       <- EitherT.fromEither[F] {
        val d = doc.runs.map(r => (r.id, r.analysis, r.dataset))
        val b = runs.map(r => (r.run, r.revision, r.dataset))
        Either.cond(d == b, (), SeedError.Runs(moment, d, b))
      }
      jobs <- EitherT.liftF(backend.jobs)
      active = jobs.filter(_.state match
        case JobState.Finished(_) => false
        case _                    => true)
      _ <- EitherT.fromEither[F] {
        val d = doc.jobs.map(h => (h.run, h.job))
        val b = active.map(j => (j.run, j.job))
        Either.cond(d == b, (), SeedError.Jobs(moment, d, b))
      }
      check <- doc.draft.fold(EitherT.rightT[F, SeedError](DraftCheck.Unchecked)) { d =>
        EitherT(backend.preview(d.id))
          .leftMap(SeedError.Backend(moment, _))
          .as(DraftCheck.Checked(d, Vector.empty))
      }
      progress = active.flatMap(_.state match
        case JobState.Running(p) => Vector(p)
        case _                   => Vector.empty)
    yield StorySeed(
      moment,
      doc,
      backend,
      SessionFacts(progress, Vector.empty, check),
      assets
    )).value
