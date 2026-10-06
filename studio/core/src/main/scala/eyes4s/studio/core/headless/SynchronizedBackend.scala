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

package eyes4s.studio.core.headless

import cats.Monad
import cats.syntax.all.*
import eyes4s.codec.CanonicalDigest
import eyes4s.studio.core.backend.*
import eyes4s.studio.core.document.{DatasetRevisionSpec, ReportingSpec}
import eyes4s.studio.core.preview.*
import fs2.Stream

/** Register the host's authoritative document before nonblocking study requests.
  * Job observation and cancellation remain independent of document synchronization.
  */
final class SynchronizedBackend[F[_]](
    underlying: StudyBackend[F],
    synchronize: () => F[Either[BackendError, Unit]]
)(using F: Monad[F])
    extends StudyBackend[F]:
  private def before[A](request: => F[Either[BackendError, A]]): F[Either[BackendError, A]] =
    synchronize().flatMap {
      case Left(error) => F.pure(Left(error))
      case Right(_)    => request
    }
  private def streaming(
      request: => Stream[F, Either[BackendError, PreviewEvent]]
  ): Stream[F, Either[BackendError, PreviewEvent]] =
    Stream.eval(synchronize()).flatMap {
      case Left(error) => Stream.emit(Left(error))
      case Right(_)    => request
    }

  def admission(dataset: DatasetRevision) = before(underlying.admission(dataset))
  def verify(dataset: DatasetRevision, content: CanonicalDigest[DatasetRevisionSpec]) = before(
    underlying.verify(dataset, content)
  )
  def placement(spec: DatasetRevisionSpec)                = before(underlying.placement(spec))
  def ledger(dataset: DatasetRevision, page: PageRequest) = before(
    underlying.ledger(dataset, page)
  )
  def preview(revision: AnalysisRevision) = before(underlying.preview(revision))
  def previewRows(revision: AnalysisRevision, page: PageRequest) = before(
    underlying.previewRows(revision, page)
  )
  def previewCounting(revision: AnalysisRevision, budget: PreviewBudget) = streaming(
    underlying.previewCounting(revision, budget)
  )
  def continuePreview(id: PreviewId, budget: PreviewBudget) = streaming(
    underlying.continuePreview(id, budget)
  )
  def submitPreview(ready: PreviewReady)             = before(underlying.submitPreview(ready))
  def submit(revision: AnalysisRevision)             = before(underlying.submit(revision))
  def runs                                           = underlying.runs
  def jobs                                           = underlying.jobs
  def job(id: JobId)                                 = underlying.job(id)
  def subscribe(id: JobId)                           = underlying.subscribe(id)
  def cancel(id: JobId)                              = underlying.cancel(id)
  def outcome(id: JobId)                             = underlying.outcome(id)
  def result(run: RunId)                             = before(underlying.result(run))
  def queries(run: RunId, page: PageRequest)         = before(underlying.queries(run, page))
  def inspect(run: RunId, address: ResultAddress)    = before(underlying.inspect(run, address))
  def provenance(run: RunId, address: ResultAddress) = before(
    underlying.provenance(run, address)
  )
  def pairRows(run: RunId, scale: Int, page: PageRequest) = before(
    underlying.pairRows(run, scale, page)
  )
  def mapGrid(run: RunId, scale: Int, trial: TrialKey) = before(
    underlying.mapGrid(run, scale, trial)
  )
  def report(run: RunId, reporting: ReportingSpec, scale: Int) = before(
    underlying.report(run, reporting, scale)
  )
  def trialFixations(revision: AnalysisRevision, trial: TrialKey) = before(
    underlying.trialFixations(revision, trial)
  )
  def trialPreview(revision: AnalysisRevision, trial: TrialKey) = before(
    underlying.trialPreview(revision, trial)
  )
  def sourceRecords(revision: AnalysisRevision, from: Int, count: Int) = before(
    underlying.sourceRecords(revision, from, count)
  )
