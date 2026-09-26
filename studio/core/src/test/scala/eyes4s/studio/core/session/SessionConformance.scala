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

package eyes4s.studio.core.session

import cats.effect.{IO, Resource}
import cats.syntax.all.*
import eyes4s.studio.core.bundle.*
import eyes4s.studio.core.command.JournalEntry
import eyes4s.studio.core.document.StudioDocument
import munit.CatsEffectSuite

/** What the session suites share. Each implementation of [[ProjectStore]]
  * supplies one new, empty bundle per test; `views` opens a new store on
  * it, as another process would. The in-memory store gives the same store
  * each time; the file store a new instance on the same directory.
  */
abstract class SessionConformance extends CatsEffectSuite:

  def bundle: Resource[IO, IO[ProjectStore[IO]]]

  protected def owner(name: String): LockOwner =
    LockOwner.of(name).fold(e => fail(e.message), identity)

  protected val alice: LockOwner = owner("Eyes Studio · alice")
  protected val bob: LockOwner   = owner("Eyes Studio · bob")

  protected def ok[E, A](io: IO[Either[E, A]]): IO[A] =
    io.flatMap(
      _.fold(
        e =>
          IO.raiseError(new AssertionError(e match
            case s: SessionError => s.message
            case other           => other.toString)),
        IO.pure
      )
    )

  /** A new project on `store` holding `document`, open for writing. */
  protected def created(
      store: ProjectStore[IO],
      document: StudioDocument,
      options: SessionOptions = SessionOptions.default
  ): IO[ProjectSession[IO]] =
    ok(
      ProjectSession.create(
        store,
        alice,
        document,
        SharingOptions.complete,
        BundleSamples.inputsFor(document),
        options
      )
    )

  /** Perform every entry, failing the test on a refusal. */
  protected def performAll(
      session: ProjectSession[IO],
      entries: Vector[JournalEntry]
  ): IO[Vector[Applied]] =
    entries.traverse(e => ok(session.perform(e)))

  /** Kill the process behind `faulty`: its lock is released as its exit
    * would release it.
    */
  protected def die(faulty: FaultyStore): IO[Unit] = faulty.bury

  /** Open the bundle again, as a new process: a new view, a new session. */
  protected def reopen(
      views: IO[ProjectStore[IO]],
      as: LockOwner = bob
  ): IO[ProjectSession.Opened[IO]] =
    views.flatMap(store => ok(ProjectSession.open(store, as)))

  protected def withoutJobs(d: StudioDocument): StudioDocument =
    d.withJobs(Vector.empty).fold(e => fail(e.toString), identity)
