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

package eyes4s.studio.desktop.runtime

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.all.*
import eyes4s.studio.app.AppModel
import eyes4s.studio.core.document.StudioDocument
import eyes4s.studio.core.execution.{
  ExecutionError,
  ExecutionEvent,
  ExecutionJob,
  ExecutionService
}
import eyes4s.studio.core.fixture.{FakeStudyBackend, StoryMoment}

/** The studio-core services one window runs on (tickets S1.4, S1.5a): the
  * study backend and the execution service over it. Until S3.7 the backend
  * is the [[FakeStudyBackend]] at a story moment.
  *
  * Events of the execution service are handed to `deliver` on the service's
  * own fibre, in publication order; the caller moves them to the UI thread.
  */
final class StudioSession private (
    val backend: FakeStudyBackend[IO],
    val service: ExecutionService[IO],
    release: IO[Unit]
)(using runtime: IORuntime):

  /** Run `io` on the service's runtime and hand its result to `done`, on
    * the runtime's thread.
    */
  def run[A](io: IO[A])(done: Either[Throwable, A] => Unit): Unit =
    io.unsafeRunAsync(done)

  /** Run `io` and wait for it (boot and tests only; never on the UI thread
    * while a job event is expected).
    */
  def await[A](io: IO[A]): A = io.unsafeRunSync()

  /** Track each job the document's running runs name (a reopened project,
    * t3's run 8), then return the service's jobs.
    */
  def adopt(document: StudioDocument): Vector[Either[ExecutionError, ExecutionJob]] =
    await(
      document.running
        .flatMap(r =>
          document.job(r.id).map(j => (j, AppModel.stampOf(document, r.analysis, r.dataset)))
        )
        .traverse((job, stamp) => service.adopt(job, stamp))
    )

  def jobs: Vector[ExecutionJob] = await(service.jobs)

  /** Stop watching jobs and stop delivering events. Backend jobs keep running. */
  def close(): Unit = await(release)

object StudioSession:

  /** Start the fake backend at `moment`, the execution service over it, and
    * the subscription that delivers its events.
    */
  def start(moment: StoryMoment, deliver: ExecutionEvent => Unit)(using
      runtime: IORuntime
  ): StudioSession =
    val resources =
      for
        backend <- cats.effect.Resource.eval(FakeStudyBackend.create[IO](moment))
        service <- ExecutionService.resource[IO](backend)
        events  <- service.subscribe
        _       <- events.evalMap(e => IO(deliver(e))).compile.drain.background
      yield (backend, service)
    val ((backend, service), release) = resources.allocated.unsafeRunSync()
    new StudioSession(backend, service, release)
